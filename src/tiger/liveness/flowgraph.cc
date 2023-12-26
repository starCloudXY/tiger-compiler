#include "tiger/liveness/flowgraph.h"
#include "tiger/translate/translate.h"
namespace fg {

void FlowGraphFactory::AssemFlowGraph() {
  /* TODO: Put your lab6 code here */
  assem::Instr *prev_instr = nullptr;
  FNodePtr prev_node = nullptr;
  for(assem::Instr *instr:instr_list_->GetList()){
    // create node for instr
    FNodePtr node = flowgraph_->NewNode(instr);
    if(prev_instr){
      if (typeid(*prev_instr) == typeid(assem::OperInstr)){
        if(!static_cast<assem::OperInstr*>(prev_instr)->jumps_){
          flowgraph_->AddEdge(prev_node, node);
        }
      }
      else{
        if (typeid(*prev_instr) == typeid(assem::LabelInstr)) {
          label_map_->Enter(( static_cast<assem::LabelInstr *>(prev_instr))->label_, node);
        }
        flowgraph_->AddEdge(prev_node, node);
      }
    }
    prev_instr = instr;
    prev_node = node;
  }
  for (FNodePtr nodePtr:flowgraph_->Nodes()->GetList()){
    assem::Instr *instr = nodePtr->NodeInfo();
    if(typeid(*instr) == typeid(assem::OperInstr)){
      assem::Targets *jumps = static_cast<assem::OperInstr *>(instr)->jumps_;
      if(jumps){
        for (temp::Label *jump_target : *(jumps->labels_)) {
          FNodePtr target = label_map_->Look(jump_target);
          flowgraph_->AddEdge(nodePtr, target);
        }
      }
    }
  }
}



} // namespace fg

namespace assem {

temp::TempList *LabelInstr::Def() const {
  return new temp::TempList();
}

temp::TempList *MoveInstr::Def() const {
  if (dst_)
    return dst_;
  else
    return new temp::TempList();
}

temp::TempList *OperInstr::Def() const {
  if (dst_)
    return dst_;
  else
    return new temp::TempList();
}

temp::TempList *LabelInstr::Use() const {
  return new temp::TempList();
}

temp::TempList *MoveInstr::Use() const {
  if (src_)
    return src_;
  else
    return new temp::TempList();
}

temp::TempList *OperInstr::Use() const {
  if (src_)
    return src_;
  else
    return new temp::TempList();
}
} // namespace assem
