#include "tiger/frame/x64frame.h"

extern frame::RegManager *reg_manager;

namespace frame {
/* TODO: Put your lab5 code here */
tree::Exp *externalCall( std::string s, tree::ExpList *args) {
  return new tree::CallExp(new tree::NameExp(temp::LabelFactory::NamedLabel(s)), args);
}
class InFrameAccess : public Access {
public:
  int offset = 8;
  explicit InFrameAccess(int offset) : offset(offset) {
    assert(offset<0);
  }
  /* TODO: Put your lab5 code here */
  //save in memory
  tree::Exp *ToExp(tree::Exp *frame_ptr) const override{
    std::cout<<"to mem exp\n";
    return new tree::MemExp(
        new tree::BinopExp(
            tree::BinOp::PLUS_OP,
            frame_ptr,
            new tree::ConstExp(offset)));
  }
};


class InRegAccess : public Access {
public:
  // Temp is a data structure represents virtual registers
  temp::Temp *reg;
  explicit InRegAccess(temp::Temp *reg) : reg(reg) {}
  /* TODO: Put your lab5 code here */
  tree::Exp *ToExp(tree::Exp *frame_ptr) const override{
    std::cout<<"to reg exp dddd\n";
    return new tree::TempExp(reg);
  }
};

X64Frame::X64Frame(temp::Label *name, std::list<bool> *escapes) {
  //offset on the memory
  label = name;
  offset = -reg_manager->WordSize();
  formals_ = std::list<frame::Access*>();
  std::cout<<" Adding label : "<<name->Name()<<std::endl;
  if(escapes){
    for (bool escape : *escapes){
      std::cout<<"adding escape "<<escape<<std::endl;
      formals_.push_back(allocLocal(escape));
    }
  }
};
Access *X64Frame::allocLocal(bool escape) {
    if(escape){
    std::cout<<"allocate true escape in mem\n";
      auto newAccess = new InFrameAccess(offset);
      offset -=reg_manager->WordSize();
      return newAccess;
    }
    else{
      std::cout<<"allocate reg escape in \n";
      return new InRegAccess(temp::TempFactory::NewTemp());
    }

};
//tree::Stm *procEntryExit_fs(frame::Frame *frame, tree::Stm *stm) {
//    int num = 1;
//    tree::Stm *viewshift = new tree::ExpStm(new tree::ConstExp(0));
//    frame->formals_.reverse();
//    num = frame->formals_.size();
//    for (frame::Access *formal : frame->formals_) {
//  if (num <= 6) {
//      viewshift = new tree::SeqStm(
//          viewshift,
//          new tree::MoveStm(
//              formal->ToExp(new tree::TempExp(reg_manager->FramePointer())),
//              new tree::TempExp(reg_manager->ArgRegs()->
//                                NthTemp(num))));
//  } else {
//      viewshift = new tree::SeqStm(
//          viewshift,
//          new tree::MoveStm(
//              formal->ToExp(new tree::TempExp(reg_manager->FramePointer())),
//              tree::NewMemPlus_Const(
//                  new tree::TempExp(reg_manager->FramePointer()),
//                  -1 * (num - 6) * reg_manager->WordSize())));
//  }
//  num--;
//    }
//    return new tree::SeqStm(viewshift, stm);
//}

} // namespace frame