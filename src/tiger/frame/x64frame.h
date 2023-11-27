//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"
#include <iostream>

namespace frame {
class X64RegManager : public RegManager {
  /* TODO: Put your lab5 code here */
public:
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
    fp = temp::TempFactory::NewTemp();
    caller_save_reg = new temp::TempList({rax,rdi,rsi,rdx,rcx,r8,r9,r10,r11});
    callee_save_reg = new temp::TempList({rbx,rbp,r12,r13,r14,r15});
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
    temp_map_->Enter(rbp, new std::string("%rbp"));
    temp_map_->Enter(r12, new std::string("%r12"));
    temp_map_->Enter(r14, new std::string("%r14"));
    temp_map_->Enter(r15, new std::string("%r15"));
    temp_map_->Enter(rsp, new std::string("%rsp"));
  }
public:

  // caller-saved registers
  temp::Temp *rax,*rdi,*rsi,*rdx,*rcx,*r8,*r9,*r10,*r11,*fp;
  // caller-saved registers
  temp::Temp *rbx,*rbp,*rsp,*r12,*r13,*r14,*r15;
  temp::Map *temp_map_ = nullptr;

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
      return fp;
  };
  temp::Temp *StackPointer()override{
      return rsp;
  };
  temp::Temp *ReturnValue()override{
      std::cout<<"            Returning rax\n";
      return rax;
  };
  temp::Temp *ARG_nth(int num) override {
      switch (num) {
      case 1: return rdi;
      case 2: return rsi;
      case 3: return rdx;
      case 4: return rcx;
      case 5: return r8;
      case 6: return r9;
      default: return nullptr;
      }
  }
};
class X64Frame : public Frame {
  /* TODO: Put your lab5 code here */
public:
  temp::Label *label;
  std::list<frame::Access *> formals_;
  int offset;
  X64Frame(temp::Label *name, std::list<bool> *escapes);
  Access *allocLocal(bool escape) override;
};
} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
