#ifndef TIGER_TRANSLATE_TRANSLATE_H_
#define TIGER_TRANSLATE_TRANSLATE_H_
#define DBG(format, ...)  if(0){std::cout<<format<<std::endl;}
#include <list>
#include <memory>

#include "tiger/absyn/absyn.h"
#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/x64frame.h"
#include "tiger/semant/types.h"

namespace tr {

class Exp;
class ExpAndTy;
class Level;

class PatchList {
public:
  void DoPatch(temp::Label *label) {
    for(auto &patch : patch_list_) *patch = label;
  }

  static PatchList JoinPatch(const PatchList &first, const PatchList &second) {
    PatchList ret(first.GetList());
    for(auto &patch : second.patch_list_) {
      ret.patch_list_.push_back(patch);
    }
    return ret;
  }

  explicit PatchList(std::list<temp::Label **> patch_list) : patch_list_(patch_list) {}
  PatchList() = default;

  [[nodiscard]] const std::list<temp::Label **> &GetList() const {

    return patch_list_;
  }

private:
  std::list<temp::Label **> patch_list_;
};

class Access {
public:
  Level *level_;
  frame::Access *access_;

  Access(Level *level, frame::Access *access)
      : level_(level), access_(access) {}
  static Access *AllocLocal(Level *level, bool escape);

};

class Level {
public:
  frame::Frame *frame_;
  Level *parent_= nullptr;

  /* TODO: Put your lab5 code here */
  Level() = default;
  Level(frame::Frame *frame, Level *parent):parent_(parent){
    frame_=(frame::X64Frame*)frame;

  }
  Level(Level *parent, temp::Label *name,
        std::list<bool> *formals):parent_(parent){
    // add formal parameter for static link
    formals->push_front(true);
    // allocate new frame
    frame_ = new frame::X64Frame(name, formals);
  };
  std::list<Access *> *Formals(){
    DBG("Step in");
    std::list<frame::Access *> formal_list = frame_->formals_;
    DBG("std::list<frame::Access *> *formal_list get");
    std::list<tr::Access *> *formal_list_with_level =
        new std::list<tr::Access *>();
    DBG("std::list<tr::Access *> *formal_list_with_level newed");
    bool first = true;
    for (frame::Access *formal : formal_list) {
      if (first) {
        // skip the first para static link
        DBG("skip the first para static link");
        first = false;
        continue;
      }
      DBG("formal_list_with_level push back para");
      formal_list_with_level->push_back(new tr::Access(this, formal));
    }
    DBG("std::list<tr::Access *> *formal_list_with_level built");
    return formal_list_with_level;
  }
  static Level *NewLevel(Level *parent,temp::Label *name,std::list<bool> *formals){
      ////why?
    DBG("making new level\n");
    formals->push_front(true);

      return new Level(
          new frame::X64Frame(
              name,
              formals),
          parent);
  };

};

class ProgTr {
public:
  // TODO: Put your lab5 code here */
  ProgTr(std::unique_ptr<absyn::AbsynTree> absyn_tree,
         std::unique_ptr<err::ErrorMsg> errormsg)
      : absyn_tree_(std::move(absyn_tree)), errormsg_(std::move(errormsg)),
        tenv_(std::make_unique<env::TEnv>()),
        venv_(std::make_unique<env::VEnv>()) {

  }
  /**
   * Translate IR tree
   */
  void Translate();

  /**
   * Transfer the ownership of errormsg to outer scope
   * @return unique pointer to errormsg
   */
  std::unique_ptr<err::ErrorMsg> TransferErrormsg() {
    return std::move(errormsg_);
  }


private:
  std::unique_ptr<absyn::AbsynTree> absyn_tree_;
  std::unique_ptr<err::ErrorMsg> errormsg_;
  std::unique_ptr<Level> main_level_;
  std::unique_ptr<env::TEnv> tenv_;
  std::unique_ptr<env::VEnv> venv_;

  // Fill base symbol for var env and type env
  void FillBaseVEnv();
  void FillBaseTEnv();
};

} // namespace tr

#endif
