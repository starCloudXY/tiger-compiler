#include "tiger/regalloc/color.h"

extern frame::RegManager *reg_manager;
#define COL_LOG(fmt, args...)                                            \
  do {                                                                         \
    printf("[COL_LOG][%s:%d:%s] " fmt "\n", __FILE__, __LINE__,          \
           __FUNCTION__, ##args);                                              \
    fflush(stdout);                                                            \
  } while (0);
namespace col {
/* TODO: Put your lab6 code here */

Color::Color() {
  coloring = temp::Map::Empty();
  auto regs = reg_manager->Registers()->GetList();
  for(auto reg:regs){
    auto reg_name = reg_manager->temp_map_->Look(reg);
    finishColors.insert(reg_name);
    coloring->Enter(reg, reg_name);
    COL_LOG("insert precolored temp %d, color %s", reg->Int(), reg_name->data());
  }
}
col::Result Color::BuildAndGetResult() {
  return {coloring, new live::INodeList()};
}
void Color::RemoveFinishColor(live::INodePtr n) {
  // finishColors ← finishColors \ {color[n]}
  finishColors.erase(coloring->Look(n->NodeInfo()));
}
bool Color::AssignColor(live::INodePtr n) {
  if(finishColors.empty()) return false;
  // let c ∈ finishColor
  auto c = finishColors.begin();
  // color[n] ← c
  coloring->Enter(n->NodeInfo(), *c);
  COL_LOG("AssignColor %s to temp %d",(*c)->data(),n->NodeInfo()->Int())
  return true;
}
void Color::AssignSameColor(live::INodePtr color_src,
                            live::INodePtr color_dst) {
  COL_LOG("start finding temp %d color", color_src->NodeInfo()->Int())
  auto c = coloring->Look(color_src->NodeInfo());
  // incase not finding color of src
  assert(c);
  coloring->Enter(color_dst->NodeInfo(), c);
}
void Color::InitFinishColors() {
  finishColors.clear();
  for (temp::Temp *temp : reg_manager->Registers()->GetList()) {
    std::string *color = reg_manager->temp_map_->Look(temp);
    finishColors.insert(color);
  }
}
bool Color::FinishColorsEmpty() { return finishColors.empty(); }

} // namespace col
