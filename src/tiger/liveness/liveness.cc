#include "tiger/liveness/liveness.h"
#include <set>

extern frame::RegManager *reg_manager;
#define LDBG(format) {std::cout<<format<<std::endl;}
namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!res->Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

std::set<temp::Temp *> MakeSet(const std::list<temp::Temp *> &origin) {
  std::set<temp::Temp *> res;
  for (auto &it : origin)
    res.insert(it);
  return res;
};
temp::TempList *ToTempList(const std::set<temp::Temp *> &origin) {
  temp::TempList *res = new temp::TempList();
  for (auto &it : origin)
    res->Append(it);
  return res;
};
void LiveGraphFactory::LiveMap() {
  for(fg::FNodePtr nodePtr:flowgraph_->Nodes()->GetList()){
    in_->Enter(nodePtr, new temp::TempList());
    out_->Enter(nodePtr,new temp::TempList);
  }
  bool solved = false;
  while (!solved){
    solved = true;
    for(fg::FNodePtr node:flowgraph_->Nodes()->GetList()){
      assem::Instr *instr = node->NodeInfo();
      temp::TempList *use = instr->Use();
      temp::TempList *def = instr->Def();
      // out[n] <- U(s ∈ succ[n]) in[s]
      std::set<temp::Temp *> out_set;
      for (fg::FNodePtr succ : node->Succ()->GetList()) {
        out_set.merge(MakeSet(in_->Look(succ)->GetList()));
      }
      //in'[n] <- in[n] ; out'[n] <- out[n]
      std::set<temp::Temp *> out_set_pre = MakeSet(out_->Look(node)->GetList());
      std::set<temp::Temp *> in_set_pre = MakeSet(in_->Look(node)->GetList());

      //in[n] <- use[n] U (out[n] – def[n])
      std::set<temp::Temp *> in_set = MakeSet(use->GetList());
      std::set<temp::Temp *> def_set = MakeSet(def->GetList());
      std::list<temp::Temp *> diff;

      std::set_difference(out_set_pre.begin(), out_set_pre.end(),
                          def_set.begin(), def_set.end(),
                          back_inserter(diff));
      in_set.merge(MakeSet(diff));

      if (!(in_set == in_set_pre) || !(out_set == out_set_pre)) {
        solved = false;
        temp::TempList *in = ToTempList(in_set);
        temp::TempList *out = ToTempList(out_set);
        in_->Set(node, in);
        out_->Set(node, out);
      }
    }
  }
  LDBG("finish making map");
}

void LiveGraphFactory::InterfGraph() {
  LDBG("begin interface graph")

  //Build pre_colored Interface Graph
  auto precolored_temps = reg_manager->Registers()->GetList();
  // create nodes for all precolored regs
  for(auto precolored_temp : precolored_temps){
    INodePtr nodePtr = live_graph_.interf_graph->NewNode(precolored_temp);
    temp_node_map_->Enter(precolored_temp,nodePtr);
  }
  for (temp::Temp *temp1 : precolored_temps)
    for(temp::Temp *temp2 : precolored_temps){
      INodePtr node1 = temp_node_map_->Look(temp1);
      INodePtr node2 = temp_node_map_->Look(temp2);
      live_graph_.interf_graph->AddEdge(node1,node2);
    }

  for (auto node : flowgraph_->Nodes()->GetList()){
    assem::Instr *instr = node->NodeInfo();

    // check regs in Use
    for (auto use : instr->Use()->GetList()){
      if(temp_node_map_->Look(use))
        continue;
      INodePtr new_node = live_graph_.interf_graph->NewNode(use);
      temp_node_map_->Enter(use,new_node);
    }

    // check regs in Def
    for (auto def : instr->Def()->GetList()) {
      if (temp_node_map_->Look(def))
        continue;
      INodePtr new_node = live_graph_.interf_graph->NewNode(def);
      temp_node_map_->Enter(def, new_node);
    }
  }

  // add edges
  for(auto node : flowgraph_->Nodes()->GetList()){
    assem::Instr *instr = node->NodeInfo();
    if(typeid(*instr)== typeid(assem::MoveInstr)){
      for(auto def : instr->Def()->GetList()){
        INodePtr nodePtr = temp_node_map_->Look(def);
        auto out_set = MakeSet(out_->Look(node)->GetList());
        auto use_set = MakeSet(out_->Look(node)->GetList());
        std::list<temp::Temp *> diff_list;
        std::set_difference(out_set.begin(), out_set.end(),
                            use_set.begin(), use_set.end(),
                            back_inserter(diff_list));
        for(auto diff : diff_list){
          INodePtr d_node = temp_node_map_->Look(diff);
          live_graph_.interf_graph->AddEdge(nodePtr,d_node);
          live_graph_.interf_graph->AddEdge(d_node,nodePtr);
        }
        // for move instr, add in MoveList
        for (temp::Temp *use : instr->Use()->GetList()) {
          INodePtr use_node = temp_node_map_->Look(use);
          live_graph_.moves->Append(use_node, nodePtr);
        }
      }
    }

    else {
      for (auto def : instr->Def()->GetList()) {
        INodePtr def_node = temp_node_map_->Look(def);
        for (auto out : out_->Look(node)->GetList()) {
          INodePtr out_node = temp_node_map_->Look(out);
          live_graph_.interf_graph->AddEdge(def_node, out_node);
          live_graph_.interf_graph->AddEdge(out_node, def_node);
        }
      }
  }
}
}


void LiveGraphFactory::Liveness() {
  LiveMap();
  InterfGraph();
}

} // namespace live
