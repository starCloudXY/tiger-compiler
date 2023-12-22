#include "tiger/frame/x64frame.h"
#include "tiger/translate/translate.h"
#include <sstream>

extern frame::RegManager *reg_manager;

namespace frame {
/* TODO: Put your lab5 code here */
tree::Exp *externalCall( std::string s, tree::ExpList *args) {
  return new tree::CallExp(
      new tree::NameExp(
          temp::LabelFactory::NamedLabel(
              s)), args);
}



class InRegAccess : public Access {
public:
  // Temp is a data structure represents virtual registers
  explicit InRegAccess(temp::Temp *reg){
    this->reg = reg;
  };
  tree::Exp *ToExp(tree::Exp *frame_ptr) const override{
    DBG("------------------------in reg offset ");
    return new tree::TempExp(reg);
  }
};

X64Frame::X64Frame(temp::Label *name, std::list<bool> *escapes) {
  //offset on the memory
  label = name;
  offset = -1*reg_manager->WordSize();
  formals_ = std::list<frame::Access*>();
  if(escapes){
    for (bool escape : *escapes){
      formals_.push_back(allocLocal(escape));
    }
  }
};
Access *X64Frame::allocLocal(bool escape) {
  DBG("alloc local \n")
    if(escape){
      auto newAccess = new InFrameAccess(offset);
      offset -= reg_manager->WordSize();
      return newAccess;
    }
    else{
      return new InRegAccess(
          temp::TempFactory::NewTemp());
    }

};
/////?
tree::Stm *procEntryExit1(frame::Frame *frame, tree::Stm *stm) {
    DBG("\nprocEntryExit1 \n");
    int num = 0;
    tree::MoveStm *pStm = nullptr;
    tree::SeqStm *viewshift = nullptr;
    tree::SeqStm *tail = nullptr;
    auto arg_reg_num = reg_manager->ArgRegs()->GetList().size();
    auto arg_num = frame->formals_.size();
    DBG(arg_num);
    for (frame::Access *formal : frame->formals_) {
      auto dst = formal->ToExp(
          new tree::TempExp(
              reg_manager->FramePointer()
              )
          );
      assert(dst);
      if (num < arg_reg_num) {
      pStm = new tree::MoveStm(
              dst,
              new tree::TempExp(
                  reg_manager->ARG_nth(num)
              ));
      } else {
      pStm = new tree::MoveStm(
              dst,
              new tree::MemExp(
                  new tree::BinopExp(
                      tree::BinOp::PLUS_OP,
                  new tree::TempExp(reg_manager->FramePointer()),
                  new tree::ConstExp((arg_num-num) * reg_manager->WordSize())
                  )
              )
          );

  }
  if (!tail) {
      tail = viewshift =new tree::SeqStm(
          pStm,
          new tree::ExpStm(
              new tree::ConstExp(0)
              ));
  } else {
      tail->right_ = new tree::SeqStm(
          pStm,
          new tree::ExpStm(
              new tree::ConstExp(0)
                  ));
      tail = dynamic_cast<tree::SeqStm *>(tail->right_);
  }
  num++;
    }
    if (viewshift) {
    tail->right_ = stm;
    return viewshift;
    }
    return stm;
}
assem::InstrList *procEntryExit2(assem::InstrList *body) {
    temp::TempList *returnSink = reg_manager->ReturnSink();
    body->Append(new assem::OperInstr("", nullptr, returnSink, nullptr));
    return body;
}
assem::Proc *ProcEntryExit3(frame::Frame *frame, assem::InstrList * body) {
    std::stringstream prologue;
    const std::string name = temp::LabelFactory::LabelString(frame->label);
    const int rsp_offset = -1*frame->offset;
    prologue << ".set " << name << "_framesize, " << rsp_offset << std::endl;
    prologue << name << ":" << std::endl;
    if(frame->label->Name() == "tigermain") {
    prologue << "subq $8, %rsp" << std::endl;
    prologue << "movq %rbp, (%rsp)" << std::endl;
    }
    prologue << "subq $" << rsp_offset << ", %rsp" << std::endl;

    // epilog part
    std::stringstream epilogue;
    if(frame->label->Name() == "tigermain") {
    epilogue << "movq (%rsp), %rbp" << std::endl;
    epilogue << "addq $8, %rsp" << std::endl;
    }
    epilogue << "addq $" << rsp_offset << ", %rsp" << std::endl;
    epilogue << "retq" << std::endl << ".END" << std::endl;
    return new assem::Proc(prologue.str(), body, epilogue.str());
}
Access *Access::AllocLocal(frame::Frame *frame, bool escape) {
    if (escape) {
    //  Frame::AllocLocal(TRUE) Returns an InFrameAccess with an offset from the
    //  frame pointer
    return new frame::InFrameAccess(frame->Offset());
    } else {
    //  Frame::AllocLocal(FALSE) Returns a register InRegAccess(t481)
    return new frame::InRegAccess(temp::TempFactory::NewTemp());
    }
}
} // namespace frame
