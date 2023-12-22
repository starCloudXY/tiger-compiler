//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"
#include <iostream>
extern frame::RegManager *reg_manager;
namespace frame {
enum Reg{
  rax, rdi, rsi, rdx, rcx, r8, r9,
  r10, r11, rbx, rbp, r12, r13, r14, r15, rsp
};
const std::string X64RegNames[16] = {"rax","rbx", "rcx", "rdx", "rsi", "rdi", "rbp",
                                     "rsp", "r8",  "r9",  "r10", "r11",
                                     "r12", "r13", "r14", "r15"};
class InRegAccess;
class X64RegManager : public RegManager {
  /* TODO: Put your lab5 code here */
public:
  void PrintInfo(){
    int cnt = 0;
    for(auto reg:registers->GetList()){
      std::cout<<X64RegNames[cnt]<<"   " <<reg->Int()<<std::endl;
      cnt++;
    }
  }
  X64RegManager(){
    rax = temp::TempFactory::NewTemp();
    rdi = temp::TempFactory::NewTemp();
    rsi = temp::TempFactory::NewTemp();
    rdx = temp::TempFactory::NewTemp();
    rcx = temp::TempFactory::NewTemp();
    rbx = temp::TempFactory::NewTemp();
    r8 = temp::TempFactory::NewTemp();
    r9 = temp::TempFactory::NewTemp();
    r10 = temp::TempFactory::NewTemp();
    r11 = temp::TempFactory::NewTemp();
    r12 = temp::TempFactory::NewTemp();
    r13 = temp::TempFactory::NewTemp();
    r14 = temp::TempFactory::NewTemp();
    r15 = temp::TempFactory::NewTemp();
    rbp = temp::TempFactory::NewTemp();
    rsp = temp::TempFactory::NewTemp();
    caller_save_reg = new temp::TempList({rax,rdi,rsi,rdx,rcx,r8,r9,r10,r11});
    callee_save_reg = new temp::TempList({rbx,rbp,r12,r13,r14,r15});

    regs_ =  {rax, rdi, rsi, rdx, rcx, r8, r9,
                                    r10, r11, rbx, rbp, r12, r13, r14, r15, rsp};
    registers = new temp::TempList({rax, rdi, rsi, rdx, rcx, r8, r9,
                                    r10, r11, rbx, rbp, r12, r13, r14, r15, rsp});
    arg_reg = new temp::TempList({rdi,rsi,rdx,rcx,r8,r9});
    ret_sink_reg = new temp::TempList({rsp,rax,rbx,rbp,r12,r13,r14,r15});
    temp_map_ = temp::Map::Empty();
    temp_map_->Enter(rax, new std::string("%rax"));
    temp_map_->Enter(rdi, new std::string("%rdi"));
    temp_map_->Enter(rsi, new std::string("%rsi"));
    temp_map_->Enter(rdx, new std::string("%rdx"));
    temp_map_->Enter(rcx, new std::string("%rcx"));
    temp_map_->Enter(r8, new std::string("%r8"));
    temp_map_->Enter(r9, new std::string("%r9"));
    temp_map_->Enter(r10, new std::string("%r10"));
    temp_map_->Enter(r11, new std::string("%r11"));
    temp_map_->Enter(rbx, new std::string("%rbx"));
    temp_map_->Enter(rbp, new std::string("%rbp"));
    temp_map_->Enter(r12, new std::string("%r12"));
    temp_map_->Enter(r13, new std::string("%r13"));
    temp_map_->Enter(r14, new std::string("%r14"));
    temp_map_->Enter(r15, new std::string("%r15"));
    temp_map_->Enter(rsp, new std::string("%rsp"));
    PrintInfo();
  }
public:

  // caller-saved registers
  temp::Temp *rax,*rdi,*rsi,*rdx,*rcx,*r8,*r9,*r10,*r11;
  // caller-saved registers
  temp::Temp *rbx,*rbp,*rsp,*r12,*r13,*r14,*r15;

  temp::TempList *registers,*arg_reg,*caller_save_reg,*callee_save_reg,*ret_sink_reg;
  int WordSize() override{
    return 8;
  }
  temp::TempList *Registers()override{
      return registers;
  };
  temp::TempList *ArgRegs()override{
      return arg_reg;
  };
  temp::TempList *CallerSaves()override{
      return caller_save_reg;
  };
 temp::TempList *CalleeSaves()override{
      return callee_save_reg;
  };

 temp::TempList *ReturnSink()override{
      return ret_sink_reg;
  };

  temp::Temp *FramePointer()override{
      assert(rbp);
      return rbp;
  };
  temp::Temp *StackPointer()override{
      return rsp;
  };
  temp::Temp *ReturnValue()override{
      return rax;
  };
  temp::Temp *ARG_nth(int num) override {

      switch (num) {
      case 0: return rdi;
      case 1: return rsi;
      case 2: return rdx;
      case 3: return rcx;
      case 4: return r8;
      case 5: return r9;
      default: return nullptr;
      }
  }
};
class X64Frame :public Frame{
  /* TODO: Put your lab5 code here */
public:
  X64Frame(temp::Label *name, std::list<bool> *escapes);
  Access *allocLocal(bool escape) override;
  int Offset() {
      // Keep away from the return address on the top of the frame
      offset -= reg_manager->WordSize();
      return offset;
  }
};
class InFrameAccess : public Access {
public:
  explicit InFrameAccess(int offset){
      offset_=offset;
  }
  /* TODO: Put your lab5 code here */
  //save in memory
  tree::Exp *ToExp(tree::Exp *frame_ptr) const override{
      return new tree::MemExp(
          new tree::BinopExp(
              tree::BinOp::PLUS_OP,
              new tree::ConstExp(
                  offset_),
              frame_ptr
              ));
  }
};
} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
