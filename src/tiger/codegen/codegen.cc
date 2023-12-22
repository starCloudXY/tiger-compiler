#include "tiger/codegen/codegen.h"
#include "tiger/translate/translate.h"

#include <cassert>
#include <sstream>

extern frame::RegManager *reg_manager;

namespace {

constexpr int maxlen = 1024;


} // namespace

namespace cg {
enum CalleeType{
  rbx = 0,
  rbp,
  r12,
  r13,
  r14,
  r15
};
static temp::Temp *callee_reg[6];

void CodeGen::Codegen() {
  /* TODO: Put your lab5 code here */
  auto instr_list = new assem::InstrList();

  fs_ = ((frame::X64Frame*)frame_)->label->Name() + "_framesize";
  auto x64RM = dynamic_cast<frame::X64RegManager *>(reg_manager);
  instr_list->Append(new assem::OperInstr(
      "leaq " + fs_ + "(%rsp), `d0",
      new temp::TempList(
          reg_manager->ReturnValue()),
      nullptr,
      nullptr));
  {
    // save callee-saved registers
    callee_reg[CalleeType::rbx] = temp::TempFactory::NewTemp();
    callee_reg[CalleeType::rbp] = temp::TempFactory::NewTemp();
    callee_reg[CalleeType::r12] = temp::TempFactory::NewTemp();
    callee_reg[CalleeType::r13] = temp::TempFactory::NewTemp();
    callee_reg[CalleeType::r14] = temp::TempFactory::NewTemp();
    callee_reg[CalleeType::r15] = temp::TempFactory::NewTemp();
    instr_list->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(callee_reg[CalleeType::rbx]), new temp::TempList(x64RM->rbx)));
    instr_list->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(callee_reg[CalleeType::rbp]), new temp::TempList(x64RM->rbp)));
    instr_list->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(callee_reg[CalleeType::r12]), new temp::TempList(x64RM->r12)));
    instr_list->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(callee_reg[CalleeType::r13]), new temp::TempList(x64RM->r13)));
    instr_list->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(callee_reg[CalleeType::r14]), new temp::TempList(x64RM->r14)));
    instr_list->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(callee_reg[CalleeType::r15]), new temp::TempList(x64RM->r15)));

  }
  assem_instr_ = std::make_unique<cg::AssemInstr>(cg::AssemInstr(instr_list));

  instr_list->Append(
      new assem::OperInstr(
        "leaq " + fs_ + "(`s0), `d0",
        new temp::TempList(reg_manager->FramePointer()),
        new temp::TempList(reg_manager->StackPointer()), nullptr)
                     );

  for (tree::Stm *stm : traces_->GetStmList()->GetList()) {
    stm->Munch(*assem_instr_->GetInstrList(), fs_);
  }

  //restore callee registers
  {
    auto instr_p = &(*assem_instr_->GetInstrList());
    instr_p->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(x64RM->rbx), new temp::TempList(callee_reg[CalleeType::rbx])));
    instr_p->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(x64RM->rbp), new temp::TempList(callee_reg[CalleeType::rbp])));
    instr_p->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(x64RM->r12), new temp::TempList(callee_reg[CalleeType::r12])));
    instr_p->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(x64RM->r13), new temp::TempList(callee_reg[CalleeType::r13])));
    instr_p->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(x64RM->r14), new temp::TempList(callee_reg[CalleeType::r14])));
    instr_p->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(x64RM->r15), new temp::TempList(callee_reg[CalleeType::r15])));
  }
  frame::procEntryExit2(assem_instr_->GetInstrList());
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}
} // namespace cg

namespace tree {
/* TODO: Put your lab5 code here */

void SeqStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("SEQ MUNCH")
  left_->Munch(instr_list,fs);
  right_->Munch(instr_list,fs);
}

void LabelStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("LABEL MUNCH")
  instr_list.Append(
      new assem::LabelInstr(
          label_->Name(),
          label_
          )
      );
}

void JumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("JMP MUNCH")
  temp::Label *label = exp_->name_;
  auto targets = new assem::Targets(jumps_);
  instr_list.Append(
      new assem::OperInstr(
          "jmp "+label->Name(),
          nullptr,
          nullptr,
          targets
          )
      );
}

void CjumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("CJUMP MUNCH")
  temp::Temp *left = left_->Munch(
      instr_list,
      fs
      );
  temp::Temp *right = right_->Munch(
      instr_list,
      fs
      );
  std::string cjmp_string;
  instr_list.Append(
      new assem::OperInstr(
          "cmpq `s1,`s0",
          nullptr,
          new temp::TempList({left,right}),
          nullptr
          )
      );
  switch (op_) {
  case EQ_OP:
    cjmp_string = "je";
    break;
  case NE_OP:
    cjmp_string = "jne";
    break;
  case LT_OP:
    cjmp_string = "jl";
    break;
  case GT_OP:
    cjmp_string = "jg";
    break;
  case LE_OP:
    cjmp_string = "jle";
    break;
  case GE_OP:
    cjmp_string = "jge";
    break;
  case ULT_OP:
    cjmp_string = "jnb";
    break;
  case ULE_OP:
    cjmp_string = "jnbe";
    break;
  case UGE_OP:
    cjmp_string = "jnae";
    break;
  case REL_OPER_COUNT:
  default:
    assert(0);
  }
  instr_list.Append(
      new assem::OperInstr(
          cjmp_string+" `j0",
          nullptr,
          nullptr,
          new assem::Targets(
              new std::vector<temp::Label *>{
                  true_label_,
                  false_label_
              }
              )
          )
      );
}

void MoveStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("MOVE MUNCH")
  temp::Temp *src = src_->Munch(
      instr_list,
      fs
      );
  DBG("MOVE MUNCH sss")
  if(typeid(*dst_)== typeid(tree::MemExp)){
    temp::Temp *dst = ((MemExp *)dst_)->exp_->Munch(instr_list, fs);
    instr_list.Append(
        new assem::MoveInstr(
            "movq `s0, (`s1)",
            nullptr,
            new temp::TempList({src, dst})));
  }
  else{
    temp::Temp *dst = dst_->Munch(instr_list, fs);
    instr_list.Append(
        new assem::MoveInstr(
        "movq `s0, `d0",
            new temp::TempList(
                {dst}),
            new temp::TempList({src})));
  }

}

void ExpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("EXP MUNCH")
  exp_->Munch(instr_list, fs);
}

temp::Temp *BinopExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("BIN MUNCH")
  std::string op_instr;
  switch (op_) {
  case PLUS_OP:
    op_instr = "addq";
    break;
  case MINUS_OP:
    op_instr = "subq";
    break;
  case MUL_OP:
    op_instr = "imulq";
    break;
  case DIV_OP:
    op_instr = "idivq";
    break;
  case AND_OP:
    op_instr = "andq";
    break;
  case OR_OP:
    op_instr = "orq";
    break;
  case LSHIFT_OP:
    op_instr = "salq";
    break;
  case RSHIFT_OP:
    op_instr = "shrq";
    break;
  case ARSHIFT_OP:
    op_instr = "sarq";
    break;
  case XOR_OP:
    op_instr = "xorq";
    break;
  case BIN_OPER_COUNT:
  default:
    assert(0);
  }
  auto x64RM= dynamic_cast<frame::X64RegManager *>(reg_manager);
  temp::Temp *left = left_->Munch(instr_list, fs);
  temp::Temp *right = right_->Munch(instr_list, fs);
  temp::Temp *reg = temp::TempFactory::NewTemp();
  temp::TempList *result = new temp::TempList(reg);

  temp::Temp *rax = reg_manager->GetRegister(frame::Reg::rax);
  temp::Temp *rdx = reg_manager->GetRegister(frame::Reg::rdx);
  if (op_ == BinOp::MUL_OP) {
    // R[%rdx]:R[%rax] <- S * R[%rax]
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList(rax),
        new temp::TempList(left)));
    instr_list.Append(new assem::OperInstr(
        "imulq `s0",
        new temp::TempList({rax, rdx}),
        new temp::TempList(right), nullptr));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList(reg),
        new temp::TempList(rax)));
    return reg;
  }
  if (op_ == BinOp::DIV_OP) {
    // R[%rdx] <- R[%rdx]:R[%rax] mod S
    // R[%rax] <- R[%rdx]:R[%rax] / S
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList(rax),
        new temp::TempList(left)));
    instr_list.Append(
        new assem::OperInstr(
            "cqto",
            new temp::TempList(rdx),
            new temp::TempList(rax),
            nullptr));
    instr_list.Append(
        new assem::OperInstr(
            "idivq `s0",
            new temp::TempList({rax, rdx}),
            new temp::TempList(right),
            nullptr));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0",
        new temp::TempList(reg),
        new temp::TempList(rax)));
    return reg;
  }
  else {
    instr_list.Append(
        new assem::MoveInstr(
            "movq `s0, `d0",
            result,
            new temp::TempList(left)));
    instr_list.Append(new assem::OperInstr(
        op_instr + " `s0, `d0",
        result,
        new temp::TempList(
            right),
        nullptr));
    return reg;
  }

}

temp::Temp *MemExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("MEM MUNCH")
  temp::Temp *reg = temp::TempFactory::NewTemp();
  temp::Temp *exp = exp_->Munch(instr_list, fs);
  instr_list.Append(
      new assem::OperInstr(
          "movq (`s0), `d0",
          new temp::TempList(reg),
          new temp::TempList(exp),
          nullptr));
  return reg;
}

temp::Temp *TempExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("temp MUNCH")
  if (temp_ != reg_manager->FramePointer()) {
    return temp_;
  }
  temp::Temp *fp = temp::TempFactory::NewTemp();
  std::stringstream assem;
  assem << "leaq " << fs << "(`s0), `d0";
  instr_list.Append(new assem::OperInstr(
      assem.str(),
      new temp::TempList(fp),
      new temp::TempList(
          reg_manager->StackPointer()), nullptr));
  return fp;
}

temp::Temp *EseqExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("ESEQ MUNCH")
  stm_->Munch(instr_list, fs);
  return exp_->Munch(instr_list, fs);
}

temp::Temp *NameExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  DBG("NAME MUNCH")
  temp::Temp *dst = temp::TempFactory::NewTemp();
  instr_list.Append(
      new assem::OperInstr(
          "leaq " + name_->Name() + "(%rip), `d0",
          new temp::TempList(dst), nullptr, nullptr));
  return dst;
}

temp::Temp *ConstExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  temp::Temp *dst = temp::TempFactory::NewTemp();
  // Imm
  DBG("CONSt MUNCH")
  instr_list.Append(
      new assem::OperInstr(
          "movq $" +
              std::to_string(consti_) + ", `d0",
          new temp::TempList(dst), nullptr, nullptr));
  return dst;
}

temp::Temp *CallExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */


  temp::TempList *arg_regs = args_->MunchArgs(instr_list, fs);

  // CALL instruction will trash caller saved registers
  // specifies these registers and as “destinations” of the call
  instr_list.Append(new assem::OperInstr(
      "callq " + (dynamic_cast<NameExp *>(fun_))->name_->Name(),
      reg_manager->CallerSaves(), arg_regs, nullptr));
  // get return value
  temp::Temp *return_temp = temp::TempFactory::NewTemp();
  instr_list.Append(
      new assem::MoveInstr("movq `s0, `d0", new temp::TempList(return_temp),
                           new temp::TempList(reg_manager->ReturnValue())));
  // reset sp when existing arguments passed on the stack
  int arg_reg_num = reg_manager->ArgRegs()->GetList().size();
  int arg_stack_num = std::max(int(args_->GetList().size()) - arg_reg_num, 0);
  if (arg_stack_num > 0) {
    instr_list.Append(new assem::OperInstr(
        "addq $" + std::to_string(arg_stack_num * reg_manager->WordSize()) +
            ", `d0",
        new temp::TempList(reg_manager->StackPointer()), nullptr, nullptr));
  }
  return return_temp;
}

temp::TempList *ExpList::MunchArgs(assem::InstrList &instr_list, std::string_view fs) {
  /* TODO: Put your lab5 code here */
  temp::TempList *arg_regs = new temp::TempList();
  int arg_idx = 0;
  int arg_reg_num = reg_manager->ArgRegs()->GetList().size();
  for(Exp *exp:exp_list_){
    temp::Temp *src = exp->Munch(instr_list,fs);
    if(arg_idx<arg_reg_num){
      temp::Temp *reg = reg_manager->ArgRegs()->NthTemp(arg_idx);
      instr_list.Append(new assem::MoveInstr(
          "movq `s0, `d0", new temp::TempList(reg), new temp::TempList(src)));
      arg_regs->Append(reg);
    }
    else {
      // pass on stack
      // pushq (subq wordsize fo SP, then mov to SP)
      instr_list.Append(new assem::OperInstr(
          "subq $" + std::to_string(reg_manager->WordSize()) + ", `d0",
          new temp::TempList(reg_manager->StackPointer()), nullptr, nullptr));
      instr_list.Append(new assem::OperInstr(
          "movq `s0, (`d0)", new temp::TempList(reg_manager->StackPointer()),
          new temp::TempList(src), nullptr));
    }
    ++arg_idx;
  }
  return arg_regs;
}

} // namespace tree
