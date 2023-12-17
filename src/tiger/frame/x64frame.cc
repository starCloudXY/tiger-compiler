#include "tiger/frame/x64frame.h"
#include "tiger/translate/translate.h"

extern frame::RegManager *reg_manager;

namespace frame {
/* TODO: Put your lab5 code here */
tree::Exp *externalCall( std::string s, tree::ExpList *args) {
  return new tree::CallExp(new tree::NameExp(temp::LabelFactory::NamedLabel(s)), args);
}
class InFrameAccess : public Access {
public:
  explicit InFrameAccess(int offset)  {
    offset_=offset;
    assert(offset<0);
  }
  /* TODO: Put your lab5 code here */
  //save in memory
  tree::Exp *ToExp(tree::Exp *frame_ptr) const override{
    DBG("------------------------in frame offset ");
    DBG(offset_);
    return new tree::MemExp(
        new tree::BinopExp(
            tree::BinOp::PLUS_OP,
            frame_ptr,
            new tree::ConstExp(
                offset_)));
  }
};


class InRegAccess : public Access {
public:
  // Temp is a data structure represents virtual registers
  explicit InRegAccess(temp::Temp *reg) {
      this->reg=reg;
  }
  /* TODO: Put your lab5 code here */
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
    static char instr[256];

    std::string prolog;
    int size = -frame->offset - 8;
    sprintf(instr, ".set %s_framesize, %d\n", frame->label->Name().c_str(), size);
    prolog = std::string(instr);
    sprintf(instr, "%s:\n", frame->label->Name().c_str());
    prolog.append(std::string(instr));
    sprintf(instr, "subq $%d, %%rsp\n", size);
    prolog.append(std::string(instr));

    sprintf(instr, "addq $%d, %%rsp\n", size);
    std::string epilog = std::string(instr);
    epilog.append(std::string("retq\n"));
    return new assem::Proc(prolog, body, epilog);
}

} // namespace frame
