#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#define ALLOC_LOG(fmt, args...)                                                 \
  do {                                                                         \
    printf("[REG_ALLOC_LOG][%s:%d:%s] " fmt "\n", __FILE__, __LINE__,          \
           __FUNCTION__, ##args);                                              \
    fflush(stdout);                                                            \
  } while (0);

extern frame::RegManager *reg_manager;

namespace ra {
/* TODO: Put your lab6 code here */
RegAllocator::RegAllocator(frame::Frame *frame,
                           std::unique_ptr<cg::AssemInstr> assem_instr):frame_(frame),instrList_(assem_instr->GetInstrList()){

    ALLOC_LOG("start Init");
    precolored = new live::INodeList();
    simplify_worklist = new live::INodeList();
    freeze_worklist = new live::INodeList();
    spill_worklist = new live::INodeList();
    spilled_nodes = new live::INodeList();
    initial = new live::INodeList();
    coalesced_nodes = new live::INodeList();
    select_stack = new live::INodeList();
    colored_nodes = new live::INodeList();
    no_spill_temps = new live::INodeList();
    worklist_moves = new live::MoveList();
    active_moves = new live::MoveList();
    coalesced_moves = new live::MoveList();
    constrained_moves = new live::MoveList();
    frozen_moves = new live::MoveList();
    move_list = new tab::Table<live::INode, live::MoveList>();
    degree = new tab::Table<live::INode, int>();
    alias = new tab::Table<live::INode, live::INode>();

  K = reg_manager->Registers()->GetList().size();
}
void RegAllocator::Build() {

  {
    //init
    ALLOC_LOG("start ClearAndInit");
    precolored->Clear();
    simplify_worklist->Clear();
    freeze_worklist->Clear();
    spill_worklist->Clear();
    spilled_nodes->Clear();
    initial->Clear();
    coalesced_nodes->Clear();
    select_stack->Clear();
    colored_nodes->Clear();
    //  no_spill_temps->Clear();

    //  delete worklist_moves;
    //  worklist_moves = new live::MoveList();
    delete active_moves;
    active_moves = new live::MoveList();
    delete coalesced_moves;
    coalesced_moves = new live::MoveList();
    delete constrained_moves;
    constrained_moves = new live::MoveList();
    delete frozen_moves;
    frozen_moves = new live::MoveList();

    delete move_list;
    delete degree;
    delete alias;
    move_list = new tab::Table<live::INode, live::MoveList>();
    degree = new tab::Table<live::INode, int>();
    alias = new tab::Table<live::INode, live::INode>();
  }
  temp::Map *temp_map = reg_manager->temp_map_;
  for (live::INodePtr node : interf_graph->Nodes()->GetList()){
    degree->Enter(node,new int (node->OutDegree()));
    live::MoveList *related_moves = new live::MoveList();
    for (auto move : worklist_moves->GetList()) {
      if (move.first == node || move.second == node) {
        // moveList[n] ← moveList[n] ∪ {I}
        related_moves->Append(move.first, move.second);
      }
    }
    move_list->Enter(node, related_moves);
    alias->Enter(node,node);
    // add temp into precolored or initial
    if (temp_map->Look(node->NodeInfo())) {
      // real register
      std::cout<<"precolored append temp %d"<<node->NodeInfo()->Int()<<std::endl;
      precolored->Append(node);
    } else {
      initial->Append(node);
    }
  }

}
void RegAllocator::RegAlloc(){
  ALLOC_LOG("start RegAlloc");
  LivenessAnalysis();
  Build();
  MakeWorklist();
  do{
    if(!simplify_worklist->GetList().empty())
      Simplify();
    else if (!worklist_moves->GetList().empty())
      Coalesce();
    else if (!freeze_worklist->GetList().empty())
      Freeze();
    else if (!spill_worklist->GetList().empty())
      SelectSpill();
  }  while (!(simplify_worklist->GetList().empty() &&
             worklist_moves->GetList().empty() &&
             freeze_worklist->GetList().empty() &&
             spill_worklist->GetList().empty()));
  auto color_assign_result = AssignColors();
  if (!spilled_nodes->GetList().empty()){
    RewriteProgram();
    RegAlloc();
  }
  else {
    result_ = std::make_unique<Result>(
        color_assign_result.coloring,
        RemoveRedundantMove(color_assign_result.coloring));
  }
}
void RegAllocator::MakeWorklist() {
  ALLOC_LOG("start");

  for (auto n : initial->GetList()) {
    if (*(degree->Look(n)) >= K) {
      ALLOC_LOG("Add temp %d to spill_worklist, degree %d",
                    n->NodeInfo()->Int(), *(degree->Look(n)));
        spill_worklist->AddNode(n);
    }
    else if (MoveRelated(n)) {
        ALLOC_LOG("Add temp %d to freeze_worklist, degree %d",
                      n->NodeInfo()->Int(), *(degree->Look(n)));
        freeze_worklist->AddNode(n);
    } else {
        ALLOC_LOG("Add temp %d to simplify_worklist, degree %d",
                  n->NodeInfo()->Int(), *(degree->Look(n)));
        simplify_worklist->AddNode(n);
    }
  }
  initial->Clear();
  ALLOC_LOG("finish");
}
void RegAllocator::LivenessAnalysis() {
  ALLOC_LOG("start");
  // construct and assem flow graph
  fg::FlowGraphFactory flow_graph_factory(instrList_);
  flow_graph_factory.AssemFlowGraph();
  ALLOC_LOG("finish construct and assem flow graph")
  // construct and assem live graph
  live::LiveGraphFactory live_graph_factory(flow_graph_factory.GetFlowGraph());
  live_graph_factory.Liveness();
  live::LiveGraph live_graph = live_graph_factory.GetLiveGraph();
  interf_graph = live_graph.interf_graph;
  moves = live_graph.moves;
  tmp_node_map = live_graph_factory.GetTempNodeMap();
  ALLOC_LOG("finish construct and assem live graph")
#ifdef DBG_GRAPH
  interf_graph->Show(stdout, interf_graph->Nodes(),
                     [&](temp::Temp *t) {  fprintf(stdout, "[t%d]", t->Int()); });
#endif
  worklist_moves = moves;
}
bool RegAllocator::IsRedundant(assem::Instr *instr, temp::Map *coloring) {
  // check if color of src and dst are the same
  if (typeid(*instr) != typeid(assem::MoveInstr))
    return false;
  auto move_instr = dynamic_cast<assem::MoveInstr *>(instr);
  auto src = move_instr->src_;
  auto dst = move_instr->dst_;
  if (!src || !dst)
    return false;
  if (src->GetList().size() != 1 || dst->GetList().size() != 1)
    return false;
  auto src_color = coloring->Look(src->GetList().front());
  auto dst_color = coloring->Look(dst->GetList().front());
  return (src_color == dst_color);
}
assem::InstrList *RegAllocator::RemoveRedundantMove(temp::Map *coloring) {
  assem::InstrList *new_instr_list = new assem::InstrList();
  for (auto instr : instrList_->GetList()) {
    if (!IsRedundant(instr, coloring)) {
      new_instr_list->Append(instr);
    }
  }
  return new_instr_list;
}
void RegAllocator::AddEdge(live::INodePtr u, live::INodePtr v) {
  if (!u->Adj(v) && u != v) {
    if (!precolored->Contain(u)) {
      interf_graph->AddEdge(u, v);
      (*(degree->Look(u)))++;
    }
    if (!precolored->Contain(v)) {
      interf_graph->AddEdge(v, u);
      (*(degree->Look(v)))++;
    }
  }
}
void RegAllocator::EnableMoves(live::INodeListPtr nodes) {
  for (auto n : nodes->GetList()) {
    for (auto m : NodeMoves(n)->GetList()) {
      if (active_moves->Contain(m.first, m.second)) {
        active_moves->Delete(m.first, m.second);
        worklist_moves->AddEdge(m.first, m.second);
      }
    }
  }
}
void RegAllocator::Coalesce() {
  ALLOC_LOG("start");
  if (worklist_moves->GetList().empty()) {
    return;
  }
  auto m = worklist_moves->GetList().front();
  auto x = GetAlias(m.first);
  auto y = GetAlias(m.second);
  live::INodePtr u, v;
  if (precolored->Contain(y)) {
    u = y;
    v = x;
  } else {
    u = x;
    v = y;
  }
  worklist_moves->Delete(m.first, m.second);
  if (u == v) {
    coalesced_moves->AddEdge(x, y);
    std::cout<<("AddWorklist temp %d", u->NodeInfo()->Int());
    AddWorkList(u);
  } else if (precolored->Contain(v) || u->Adj(v)) {
    constrained_moves->AddEdge(x, y);
    std::cout<<("AddWorklist temp %d", u->NodeInfo()->Int());
    AddWorkList(u);
    std::cout<<("AddWorklist temp %d", v->NodeInfo()->Int());
    AddWorkList(v);
  } else {
    bool then_clause = false;
    if (precolored->Contain(u)) {
      auto adj_v = Adjacent(v);
      then_clause = true;
      if (adj_v) {
        for (auto t : adj_v->GetList()) {
          then_clause = then_clause && Finish(t, u);
        }
      }
    } else {
      then_clause = Conservative(Adjacent(u)->Union(Adjacent(v)));
    }

    if (then_clause) {
      coalesced_moves->AddEdge(x, y);
      Combine(u, v);
      std::cout<<("AddWorklist temp %d", u->NodeInfo()->Int());
      AddWorkList(u);
    } else {
      active_moves->AddEdge(x, y);
    }
  }
}
live::INodeListPtr RegAllocator::Adjacent(live::INodePtr n) {
  // adjList[n] \ (selectStack ∪ coalescedNodes)
  auto adj_list = n->Succ();
  return adj_list->Diff(select_stack->Union(coalesced_nodes));
}
bool RegAllocator::Finish(live::INodePtr t, live::INodePtr r) {
  return *(degree->Look(t)) < K
         || precolored->Contain(t)
         || t->Adj(r);
}
void RegAllocator::Combine(live::INodePtr u, live::INodePtr v) {
  if (freeze_worklist->Contain(v)) {
    freeze_worklist->DeleteNode(v);
  } else {
    std::cout<<("Delete temp %d from spill_worklist", v->NodeInfo()->Int());
    spill_worklist->DeleteNode(v);
  }
  coalesced_nodes->AddNode(v);
  alias->Set(v, u);
  move_list->Set(u, move_list->Look(u)->Union(move_list->Look(v)));
  auto v_list = new live::INodeList();
  v_list->Append(v);
  EnableMoves(v_list);
  for (auto t : Adjacent(v)->GetList()) {
    AddEdge(t, u);
    DecrementDegree(t);
  }
  if (*(degree->Look(u)) >= K && freeze_worklist->Contain(u)) {
    freeze_worklist->DeleteNode(u);
    ALLOC_LOG("Add temp %d to spill_worklist", u->NodeInfo()->Int());
    spill_worklist->AddNode(u);
  }
}
void RegAllocator::FreezeMoves(live::INodePtr u) {
  for (auto m : NodeMoves(u)->GetList()) {
    auto x = m.first;
    auto y = m.second;
    live::INodePtr u, v;
    if (GetAlias(y) == GetAlias(u)) {
      v = GetAlias(x);
    } else {
      v = GetAlias(y);
    }
    active_moves->Delete(x, y);
    frozen_moves->AddEdge(x, y);
    if (NodeMoves(v)->GetList().empty() && *(degree->Look(v)) < K) {
      freeze_worklist->DeleteNode(v);
      std::cout<<("Add temp %d to simplify_worklist", v->NodeInfo()->Int());
      simplify_worklist->AddNode(v);
    }
  }
}
void RegAllocator::RewriteProgram() {
  ALLOC_LOG("start");
  live::INodeListPtr new_temps = new live::INodeList();
  no_spill_temps->Clear();
  for (live::INodePtr v : spilled_nodes->GetList()) {
    // Allocate memory locations for each v∈spilledNodes
    frame::InFrameAccess *access =static_cast<frame::InFrameAccess *>(
        frame::Access::AllocLocal(
            frame_, true)
        );
    // Create a new temporary vi for each definition and each use
    temp::Temp *old_temp = v->NodeInfo();
    temp::Temp *vi = temp::TempFactory::NewTemp();

    auto new_instr_list = new assem::InstrList();
    // In the program (instructions), insert a store after each
    // definition of a vi , a fetch before each use of a vi
    auto instr_it = instrList_->GetList().begin();
    while (instr_it != instrList_->GetList().end()) {
      assert(!precolored->Contain(v));

      (*instr_it)->ReplaceTemp(old_temp, vi);

      // insert a fetch before each use of a vi
      if ((*instr_it)->Use()->Contain(vi)) {
        std::cout<<("insert a fetch before each use of a vi %d", vi->Int());
        std::string ins("movq (" + frame_->label->Name() + "_framesize" +
                        std::to_string(frame_->offset) + ")(`s0), `d0");
        new_instr_list->Append(new assem::OperInstr(
            ins, new temp::TempList(vi),
            new temp::TempList(reg_manager->StackPointer()), nullptr));
      }

      new_instr_list->Append(*instr_it);

      // insert a store after each definition of a vi
      if ((*instr_it)->Def()->Contain(vi)) {
        ALLOC_LOG("insert a store after each definition of a vi");
        std::string ins("movq `s0, (" + frame_->label->Name() + "_framesize" +
                        std::to_string(frame_->offset) + ")(`d0)");
        new_instr_list->Append(new assem::OperInstr(
            ins, new temp::TempList(reg_manager->StackPointer()),
            new temp::TempList(vi), nullptr));
      }

      ++instr_it;
    }

    instrList_ = new_instr_list;

    // Put All the vi into a set newTemps
    live::INodePtr new_node = interf_graph->NewNode(vi);
    new_temps->AddNode(new_node);
    no_spill_temps->Append(new_node);
  }

  spilled_nodes->Clear();
  initial->Clear();
  initial = colored_nodes->Union(coalesced_nodes->Union(new_temps));
  colored_nodes->Clear();
  coalesced_nodes->Clear();
}
void RegAllocator::SelectSpill() {
  ALLOC_LOG("start");
  if (spill_worklist->GetList().empty())
    return;
  ALLOC_LOG("Spill_worklist size %zu", spill_worklist->GetList().size());
  auto u = spill_worklist->GetList().front();
  // use heuristic algorithm
  for(auto t : spill_worklist->GetList()){
    if(t->Degree() > u->Degree()) u = t;
  }
  ALLOC_LOG("try SelectSpill temp %d", u->NodeInfo()->Int());
  ALLOC_LOG("Delete temp %d from spill_worklist", u->NodeInfo()->Int());
  spill_worklist->DeleteNode(u);

  ALLOC_LOG("Add temp %d to simplify_worklist", u->NodeInfo()->Int());
  simplify_worklist->AddNode(u);
  FreezeMoves(u);
  ALLOC_LOG("SelectSpill success temp %d", u->NodeInfo()->Int());
}
live::MoveList *RegAllocator::NodeMoves(live::INodePtr n) {
  // moveList[n] ∩ (activeMoves ∪ worklistMoves)
  return move_list->Look(n)->Intersect(active_moves->Union(worklist_moves));
}
col::Result RegAllocator::AssignColors() {
  ALLOC_LOG("start");
  col::Color color;
  std::cout<<("select_stack size %zu", select_stack->GetList().size());
  while (!select_stack->GetList().empty()) {
    // let n = pop(SelectStack)
    auto n = select_stack->GetList().front();
    select_stack->DeleteNode(n);

    // okColors ← [0, … , K-1]
    color.InitFinishColors();

    for (auto w : n->Succ()->GetList()) {
      //      if GetAlias[w]∈(ColoredNodes ∪ precolored) then
      //            okColors ← okColors \ {color[GetAlias(w)]}
      auto alias_w = GetAlias(w);
      if ((colored_nodes->Union(precolored))->Contain(alias_w)) {
        color.RemoveFinishColor(alias_w);
      }
    }

    if (color.FinishColorsEmpty()) {
      ALLOC_LOG("fail to color node, add to spilled_nodes");
      spilled_nodes->AddNode(n);
    } else {
      std::cout<<("color node, temp %d", n->NodeInfo()->Int());
      colored_nodes->AddNode(n);
      color.AssignColor(n);
    }
  }
  // assign same color for coalesced_nodes
  for (live::INodePtr n : coalesced_nodes->GetList()) {
    std::cout<<"assign same color for coalesced_nodes, src GetAlias(n) %d, "
                  "dst temp %d"<<
                  GetAlias(n)->NodeInfo()->Int()<< n->NodeInfo()->Int()<<std::endl;
    color.AssignSameColor(GetAlias(n), n);
    ALLOC_LOG("AssignSameColor success");
  }
  ALLOC_LOG("finish");
  return color.BuildAndGetResult();
}

bool RegAllocator::MoveRelated(live::INodePtr n) {
  // NodeMoves(n) ≠ {}
  return !NodeMoves(n)->GetList().empty();
}
void RegAllocator::AddWorkList(live::INodePtr u) {
  if (!precolored->Contain(u) && !MoveRelated(u) && (*(degree->Look(u)) < K)) {
    freeze_worklist->DeleteNode(u);
    std::cout<<"Add temp %d to simplify_worklist, degree %d",
                  u->NodeInfo()->Int(), *(degree->Look(u));
    simplify_worklist->AddNode(u);
  }
}
bool RegAllocator::Conservative(live::INodeListPtr nodes) {
  int k = 0;
  for (auto n : nodes->GetList()) {
    if (*(degree->Look(n)) >= K) {
      k = k + 1;
    }
  }
  return (k < K);
}
live::INodePtr RegAllocator::GetAlias(live::INodePtr n) {
  if (coalesced_nodes->Contain(n))
    return GetAlias(alias->Look(n));
  else
    return n;
}
void RegAllocator::Simplify() {
  ALLOC_LOG("start")
  if (simplify_worklist->GetList().empty())
    return;
  // let n ∈ simplifyWorkList

  auto n = simplify_worklist->GetList().front();
  // simplifyWorkList ← simplifyWorkList\{n}

  simplify_worklist->DeleteNode(n);

  // push(n, selectStack)
  std::cout<<("push temp %d on selectStack", n->NodeInfo()->Int());
  select_stack->Prepend(n);

  // forall m ∈ Adjacent(n)
  for (auto m : Adjacent(n)->GetList()) {
    // DecrementDegree(m)
    DecrementDegree(m);
  }

}
void RegAllocator::DecrementDegree(live::INodePtr m) {

  ALLOC_LOG("DecrementDegree temp %d, degree %d", m->NodeInfo()->Int(), *(degree->Look(m)))

  if (precolored->Contain(m)) {
    ALLOC_LOG("DecrementDegree temp %d is precolored", m->NodeInfo()->Int());
    return;
  }

  auto d = degree->Look(m);
  // degree->Set(m, new int((*d)-1));
  --(*d);

  ALLOC_LOG("DecrementDegree, after decrease temp %d, degree %d", m->NodeInfo()->Int(), *(degree->Look(m)));

  if (*d == K) {
    // EnableMoves(m ∪ Adjcent(m))
    live::INodeListPtr m_set = new live::INodeList();
    m_set->Append(m);
    EnableMoves(m_set->Union(Adjacent(m)));
    delete m_set;

    // spillWorkList ← spillWorkList \ {m}
    ALLOC_LOG("Delete temp %d from spill_worklist", m->NodeInfo()->Int());
    spill_worklist->DeleteNode(m);

    if (MoveRelated(m)) {
      freeze_worklist->AddNode(m);
    } else {
      ALLOC_LOG("Add temp %d to simplify_worklist", m->NodeInfo()->Int());
      simplify_worklist->AddNode(m);
    }
  }
}
void RegAllocator::Freeze() {
  ALLOC_LOG("start");
  if (!freeze_worklist->GetList().empty()){
  auto u = freeze_worklist->GetList().front();
  freeze_worklist->DeleteNode(u);
  ALLOC_LOG("Add temp %d to simplify_worklist", u->NodeInfo()->Int());
  simplify_worklist->AddNode(u);
  FreezeMoves(u);
  }
}

} // namespace ra