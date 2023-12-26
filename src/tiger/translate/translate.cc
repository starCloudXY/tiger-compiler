#include "tiger/translate/translate.h"

#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"

extern frame::Frags *frags;
extern frame::RegManager *reg_manager;

namespace tr {

Access *Access::AllocLocal(Level *level, bool escape) {
  /* TODO: Put your lab5 code here */
  return new Access(level, level->frame_->allocLocal(escape));
}

class Cx {
public:
  PatchList trues_;
  PatchList falses_;
  tree::Stm *stm_;

  Cx(PatchList trues, PatchList falses, tree::Stm *stm)
      : trues_(trues), falses_(falses), stm_(stm) {}
};

class Exp {
public:
  [[nodiscard]] virtual tree::Exp *UnEx() = 0;
  [[nodiscard]] virtual tree::Stm *UnNx() = 0;
  [[nodiscard]] virtual Cx UnCx(err::ErrorMsg *errormsg) = 0;
};

class ExpAndTy {
public:
  tr::Exp *exp_;
  type::Ty *ty_;

  ExpAndTy(tr::Exp *exp, type::Ty *ty) : exp_(exp), ty_(ty) {}
};

class ExExp : public Exp {
public:
  tree::Exp *exp_;

  explicit ExExp(tree::Exp *exp) : exp_(exp) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    /* TODO: Put your lab5 code here */
    DBG("ExEX")
    return exp_;
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    DBG("ExNX")
    /* TODO: Put your lab5 code here */
    return new tree::ExpStm(exp_);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    DBG("ExCX")
    /* TODO: Put your lab5 code here */
    auto stm = new tree::CjumpStm(tree::RelOp::NE_OP, exp_,
                                  new tree::ConstExp(0), nullptr, nullptr);
    PatchList trues = PatchList(std::list<temp::Label **>{&(stm->true_label_)});
    PatchList falses =
        PatchList(std::list<temp::Label **>{&(stm->false_label_)});
    return {trues, falses, stm};
  }
};

class NxExp : public Exp {
public:
  tree::Stm *stm_;

  explicit NxExp(tree::Stm *stm) : stm_(stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    /* TODO: Put your lab5 code here */
    DBG("NXEx")
    return new tree::EseqExp(stm_, new tree::ConstExp(0));
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    /* TODO: Put your lab5 code here */
    DBG("NxNX")
    return stm_;
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    /* TODO: Put your lab5 code here */
    assert(0);
  }
};

class CxExp : public Exp {
public:
  Cx cx_;

  CxExp(PatchList trues, PatchList falses, tree::Stm *stm)
      : cx_(trues, falses, stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    DBG("CxUX")
    /* TODO: Put your lab5 code here */
    temp::Temp *r = temp::TempFactory::NewTemp();
    temp::Label *t = temp::LabelFactory::NewLabel();
    temp::Label *f = temp::LabelFactory::NewLabel();
    cx_.trues_.DoPatch(t);
    cx_.falses_.DoPatch(f);
    return new tree::EseqExp(
        new tree::MoveStm(new tree::TempExp(r), new tree::ConstExp(1)),
        new tree::EseqExp(
            cx_.stm_,
            new tree::EseqExp(
                new tree::LabelStm(f),
                new tree::EseqExp(new tree::MoveStm(new tree::TempExp(r),
                                                    new tree::ConstExp(0)),
                                  new tree::EseqExp(new tree::LabelStm(t),
                                                    new tree::TempExp(r))))));
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    DBG("CxNX")
    /* TODO: Put your lab5 code here */
    //    temp::Label  *label = temp::LabelFactory::NewLabel();
    //    cx_.trues_.DoPatch(label);
    //    cx_.falses_.DoPatch(label);
    //    auto cjumpStm = dynamic_cast<tree::CjumpStm *>(cx_.stm_);
    //    cjumpStm->true_label_ = label;
    //    cjumpStm->false_label_ = label;
    //    return new tree::SeqStm(cx_.stm_,new tree::LabelStm(label));
    return cx_.stm_;
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    /* TODO: Put your lab5 code here */
    DBG("CxCX")

    return cx_;
  }
};
Level Outermost(temp::Label *label, std::list<bool> *formals) {
  DBG("make outermost\n");
  formals->push_front(true);
  frame::X64Frame *pFrame = new frame::X64Frame(label, formals);
  return Level(pFrame, nullptr);
  ;
}

void ProgTr::Translate() {

  FillBaseTEnv();
  FillBaseVEnv();
  /* TODO: Put your lab5 code here */
  temp::Label *main_label_ = temp::LabelFactory::NamedLabel("tigermain");
  main_level_ =
      std::make_unique<Level>(nullptr, main_label_, new std::list<bool>());

  tr::ExpAndTy *main =
      absyn_tree_->Translate(venv_.get(), tenv_.get(), main_level_.get(),
                             main_label_, errormsg_.get());
  frags->PushBack(new frame::ProcFrag(main->exp_->UnNx(), main_level_->frame_));
}
}
namespace absyn {
static type::TyList *make_formal_tylist(env::TEnvPtr tenv, FieldList *params, err::ErrorMsg *errormsg) {
  auto result = new type::TyList();
  if (params){
    for (Field * param : params->GetList()) {
      type::Ty *ty = nullptr;
      if (param->typ_) ty = tenv->Look(param->typ_);
      else errormsg->Error(param->pos_, "undefined type %s",
                        param->typ_->Name().c_str());
      result->Append(ty);
    }
  }
  return result;
}
std::list<bool> *make_formal_esclist(absyn::FieldList *params) {
  auto result = new std::list<bool>();
  if(params) {
    for (Field * param : params->GetList()) {
      result->push_back(param->escape_);
    }
  }
  return result;
}
tr::ExpAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate AbsynTree\n");
    return root_->Translate(venv,tenv,level,label,errormsg);
}
tree::Exp *StaticLink(tr::Level *target, tr::Level *level) {
    DBG("StaticLink\n");
    tree::Exp *staticlink = new tree::TempExp(reg_manager->FramePointer());
    while(level!= nullptr&&level != target){
    frame::Access *sl = level->frame_->formals_.front();
    if(sl != nullptr)staticlink = sl->ToExp(staticlink);
    level = level->parent_;
    }
    return staticlink;
}
tr::Exp *TranslateSimpleVar(tr::Access *access, tr::Level *level) {
    DBG("Start translate SIMPLE VAR\n");

    tree::Exp *real_fp = StaticLink(access->level_, level);
    return new tr::ExExp(
        access->access_->ToExp
        (real_fp));
}
tr::Exp *TranslateAssignExp(tr::Exp *var, tr::Exp *exp) {
    DBG("Start translate  ASSIGN VAR\n");
    return new tr::NxExp(
        new tree::MoveStm(
            var->UnEx(),
            exp->UnEx()
            )
        );
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate Simple Var\n");
    env::EnvEntry *entry = venv->Look(sym_);
    if(!entry){
      errormsg->Error(pos_,"no entry!!!!!");
    }
    if(entry&&(typeid(*entry) == typeid(env::VarEntry))){
    auto varEntry = dynamic_cast<env::VarEntry *>(entry);

//    tree::Exp *exp = var_entry->access_->ToExp(level);
//    tree::Exp *Access::ToExp(Level *currentLevel) {
//      // get the exp to access the variable, consider static links
//      // get framePtr of the frame where var in
//      tree::Exp *framePtr = currentLevel->StaticLink(level_);
//      return access_->ToExp(framePtr);
//    }
    auto exp = TranslateSimpleVar(varEntry->access_,level);
    if(!varEntry->ty_){
      errormsg->Error(pos_," no type!!!!!!");
      return new tr::ExpAndTy(exp,varEntry->ty_->ActualTy());
    }
    return new tr::ExpAndTy(exp,varEntry->ty_->ActualTy());
    }
    else{
    errormsg->Error(pos_, "undefined variable %s", sym_->Name().c_str());
    return nullptr;
    }
}

tr::ExpAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate FieldVar\n");
    tr::ExpAndTy  *check_var = var_->Translate(venv,tenv,level,label,errormsg);
    if(check_var == nullptr){
    errormsg->Error(pos_,"no var");
    }
    if(check_var->ty_== nullptr){
    return new tr::ExpAndTy(
        new tr::ExExp(
            new tree::ConstExp(0)
            ),
        nullptr);
    }

    type::RecordTy *actual_ty = dynamic_cast<type::RecordTy *>(check_var->ty_->ActualTy());

    if(!actual_ty){
    errormsg->Error(pos_, "not a record type");
    return new tr::ExpAndTy(nullptr, type::IntTy::Instance());
    }

    type::FieldList *fieldList = (dynamic_cast<type::RecordTy*>(actual_ty))->fields_;
    type::ArrayTy *array_field = (dynamic_cast<type::ArrayTy*>(actual_ty));
    int order = 0 ;
    for(type::Field *field:fieldList->GetList()){
      if(field->name_->Name()==sym_->Name()){
        tr::Exp *exp = new tr::ExExp(
          new tree::MemExp(
              new tree::BinopExp(
                  tree::BinOp::PLUS_OP,
                  check_var->exp_->UnEx(),
                  new tree::ConstExp(order*reg_manager->WordSize()))));
        return new tr::ExpAndTy(exp,field->ty_->ActualTy());
      }
      order++;
    }
    errormsg->Error(pos_, "field %s doesn't exist", sym_->Name().c_str());
    return new tr::ExpAndTy(nullptr, actual_ty);
}

tr::ExpAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level, temp::Label *label,
                                      err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate subscript var\n");
    tr::ExpAndTy *check_var = var_->Translate(venv,tenv,level,label,errormsg);

    if(!subscript_){
      errormsg->Error(pos_,"no subscript var");
    }
    tr::ExpAndTy *check_subscript = subscript_->Translate(venv,tenv,level,label,errormsg);
    type::Ty *var_type = check_var->ty_;
    if (typeid(*var_type->ActualTy()) == typeid(type::ArrayTy)) {
      // a[i] is MEM(+(MEM(e), *(i, CONST w))
      tr::Exp *exp = new tr::ExExp(
    new tree::MemExp(
        new tree::BinopExp(
            tree::BinOp::PLUS_OP,
            check_var->exp_->UnEx(),
            new tree::BinopExp(
                tree::BinOp::MUL_OP, check_subscript->exp_->UnEx(),
                new tree::ConstExp(
                    reg_manager->WordSize())))));
      // return type of the array
      return new tr::ExpAndTy(exp,
          (dynamic_cast<type::ArrayTy *>(var_type)->ty_->ActualTy()));
    }
    else {
      errormsg->Error(pos_, typeid(*var_type).name());
      return new tr::ExpAndTy(nullptr,type::VoidTy::Instance());
    }
}

tr::ExpAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate var exp\n");
    tr::ExpAndTy *result = var_->Translate(venv, tenv, level, label, errormsg);
    return result;
}

tr::ExpAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate NIL exp\n");
    return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)), type::NilTy::Instance());
}

tr::ExpAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate int exp \n");
    return new tr::ExpAndTy(
        new tr::ExExp(
            new tree::ConstExp(val_)
            ),
        type::IntTy::Instance());
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate STRING exp\n");
    temp::Label *string_label = temp::LabelFactory::NewLabel();
    auto *stringFrag = new frame::StringFrag(string_label,str_);
    frags->PushBack(stringFrag);
    return new tr::ExpAndTy(new tr::ExExp(new tree::NameExp(string_label)),type::StringTy::Instance());
}

tr::ExpAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate CallExp\n");
    /* TODO: Put your lab5 code here */
    // check if function is defined
    env::EnvEntry *entry = venv->Look(func_);
    if (entry && typeid(*entry) == typeid(env::FunEntry)) {
      env::FunEntry *func = dynamic_cast<env::FunEntry *>(entry);

      // check if formals num match args num
      if (func->formals_->GetList().size() != args_->GetList().size()) {
        if (args_->GetList().size() > func->formals_->GetList().size()) {
          errormsg->Error(pos_ - 1,
                          "too many params in function " + func_->Name());
          return 0;
        } else {
          errormsg->Error(pos_ - 1, "para type mismatch");
          return 0;
        }
      }

      tree::ExpList *args_list = new tree::ExpList();
      // pass static link as the first parameter

//      // If this is a user function
      if (func->level_) {

        // Pass static link as the first parameter
        args_list->Append(StaticLink(level,func->level_));
      }

      // check if types of actual and formal parameters match
      // need to use cbegin for const function
      auto formal_type = func->formals_->GetList().cbegin();
      for (Exp *arg : args_->GetList()) {
        tr::ExpAndTy *actual_exp_and_type =
            arg->Translate(venv, tenv, level, label, errormsg);
        type::Ty *actual_type = actual_exp_and_type->ty_;
        if (!actual_type->IsSameType(*formal_type)) {
          errormsg->Error(arg->pos_, "para type mismatch");
          return 0;
        }
        args_list->Append(actual_exp_and_type->exp_->UnEx());
        ++formal_type;
      }

      tree::CallExp *exp = new tree::CallExp(new tree::NameExp(func_), args_list);

      // return result type of function
      return new tr::ExpAndTy(new tr::ExExp(exp), func->result_);

    }

}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  DBG("Start translate OP exp\n");
  /* TODO: Put your lab5 code here */
  tr::ExpAndTy *leftExpAndTy = left_->Translate(venv,tenv,level,label,errormsg);
  tr::ExpAndTy *rightExpAndTy = right_->Translate(venv,tenv,level,label,errormsg);
  tree::Exp *leftExp = leftExpAndTy->exp_->UnEx();
  tree::Exp *rightExp = rightExpAndTy->exp_->UnEx();
  tr::Exp *exp = nullptr;
  switch (oper_) {
  case PLUS_OP:
  case MINUS_OP:
  case TIMES_OP:
  case DIVIDE_OP:
  {
      enum tree::BinOp op;
      op = tree::BinOp(oper_-PLUS_OP+tree::PLUS_OP);
      if (!leftExpAndTy->ty_->IsSameType(type::IntTy::Instance()))
        errormsg->Error(left_->pos_, "left integer required");
      if (!rightExpAndTy->ty_->IsSameType(type::IntTy::Instance()))
        errormsg->Error(right_->pos_, "right integer required");
      exp = new tr::ExExp(new tree::BinopExp(op,leftExpAndTy->exp_->UnEx(),rightExpAndTy->exp_->UnEx()));
      break;
  }
  case LT_OP:
  case LE_OP:
  case GT_OP:
  case GE_OP:
  case ABSYN_OPER_COUNT: {

      tree::CjumpStm *stm;
      enum tree::RelOp op;
      switch (oper_) {
      case absyn::LT_OP:
        op = tree::RelOp::LT_OP;
        break;
      case absyn::LE_OP:
        op = tree::RelOp::LE_OP;
        break;
      case absyn::GT_OP:
        op = tree::RelOp::GT_OP;
        break;
      case absyn::GE_OP:
        op = tree::RelOp::GE_OP;
        break;
      case absyn::ABSYN_OPER_COUNT:
        op = tree::RelOp::REL_OPER_COUNT;
        break;
      }
      if (!stm) {
        stm = new tree::CjumpStm(op, leftExp, rightExp, nullptr, nullptr);
      }
      stm = new tree::CjumpStm(op, leftExp, rightExp, nullptr, nullptr);

      tr::PatchList trues = tr::PatchList({&(stm->true_label_)});
      tr::PatchList falses = tr::PatchList({&(stm->false_label_)});
      exp = new tr::CxExp(trues, falses, stm);
      //// 12345654321
      break;
  }
  case EQ_OP:
  case NEQ_OP:
  {
    tree::CjumpStm *stm;
    switch (oper_) {
    case EQ_OP:
        if (dynamic_cast<type::StringTy *>(leftExpAndTy->ty_) != nullptr) {
          auto expList = new tree::ExpList({leftExp, rightExp});
          stm = new tree::CjumpStm(tree::RelOp::EQ_OP, frame::externalCall("string_equal", expList), new tree::ConstExp(1), nullptr, nullptr);
        }
        else{
          stm = new tree::CjumpStm(tree::RelOp::EQ_OP, leftExpAndTy->exp_->UnEx(),
                                   rightExpAndTy->exp_->UnEx(), nullptr, nullptr);
        }
        break;
    case NEQ_OP:
        if (dynamic_cast<type::StringTy *>(leftExpAndTy->ty_) != nullptr) {
          auto expList = new tree::ExpList({leftExp, rightExp});
          stm = new tree::CjumpStm(
              tree::RelOp::NE_OP,
              frame::externalCall("string_equal", expList), new tree::ConstExp(0), nullptr, nullptr);
        }
       else{
          stm = new tree::CjumpStm(tree::RelOp::NE_OP, leftExpAndTy->exp_->UnEx(), rightExpAndTy->exp_->UnEx(), nullptr, nullptr);
        }
          break;
    }

    tr::PatchList trues = tr::PatchList({&(stm->true_label_)});
    tr::PatchList falses = tr::PatchList({&(stm->false_label_)});
    exp = new tr::CxExp(trues, falses, stm);
    break;
  }
  case AND_OP:
  case OR_OP: {
    temp::Label *second_condition_label = temp::LabelFactory::NewLabel();
    tr::Cx left_cx = leftExpAndTy->exp_->UnCx(errormsg);
    tr::Cx right_cx = rightExpAndTy->exp_->UnCx(errormsg);

    switch (oper_) {
    case absyn::AND_OP: {
        left_cx.trues_.DoPatch(second_condition_label);
        tr::PatchList true_list = tr::PatchList(right_cx.trues_);
        tr::PatchList false_list =
            tr::PatchList::JoinPatch(left_cx.falses_, right_cx.falses_);
        tree::SeqStm *stm = new tree::SeqStm(
            left_cx.stm_,
            new tree::SeqStm(new tree::LabelStm(second_condition_label),
                             right_cx.stm_));
        exp = new tr::CxExp(true_list, false_list, stm);
        break;
    }
    case absyn::OR_OP: {
        left_cx.falses_.DoPatch(second_condition_label);
        tr::PatchList true_list =
            tr::PatchList::JoinPatch(left_cx.trues_, right_cx.trues_);
        tr::PatchList false_list = tr::PatchList(right_cx.falses_);
        tree::SeqStm *stm = new tree::SeqStm(
            left_cx.stm_,
            new tree::SeqStm(new tree::LabelStm(second_condition_label),
                             right_cx.stm_));
        exp= new tr::CxExp(true_list, false_list, stm);
        break;
    }
    }
  }
  }
  return new tr::ExpAndTy(exp,type::IntTy::Instance());
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate RECORD exp\n");
  tr::ExExp *exp = nullptr;
  type::Ty *ty = tenv->Look(typ_);

  if (!ty||!(typeid(*(ty->ActualTy())) == typeid(type::RecordTy))){
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return new tr::ExpAndTy(nullptr, type::IntTy::Instance());
  }
  auto args_list = new tree::ExpList();
  auto fieldList = new type::FieldList();
  auto records = dynamic_cast<type::RecordTy *>(ty->ActualTy())->fields_;
  temp::Temp *reg = temp::TempFactory::NewTemp();
  auto record = records->GetList().begin();
  int count = 0;
  const int offset = (records->GetList()).size()*reg_manager->WordSize();



  args_list->Append(
      new tree::ConstExp(offset));
  tree::MoveStm *left_move_stm = new tree::MoveStm(
      new tree::TempExp(reg), frame::externalCall("alloc_record", args_list));

  tree::SeqStm *initialization_list_head = nullptr;
  tree::SeqStm *left_init_seq_stm =
      new tree::SeqStm(left_move_stm, initialization_list_head);
  tree::SeqStm *last_seq_stm = left_init_seq_stm;

  for (EField *eField : fields_->GetList()) {

    tr::ExpAndTy *check_exp =
        eField->exp_->Translate(venv, tenv, level, label, errormsg);
    fieldList->Append(
        new type::Field(
          eField->name_,
          check_exp->ty_
            )
          );
    // check field name
    if (eField->name_->Name() != (*record)->name_->Name()) {
        errormsg->Error(pos_, "field %s doesn't exist",
                        eField->name_->Name().data());
    }
    // check field type
    if (!check_exp->ty_->IsSameType((*record)->ty_)) {
        errormsg->Error(pos_, "type of field %s doesn't match",
                        eField->name_->Name().data());
    }
    tree::BinopExp *address_exp =
        new tree::BinopExp(tree::BinOp::PLUS_OP,
                           new tree::TempExp(reg),
                           new tree::ConstExp(
                               count * reg_manager->WordSize())
        );
    tree::Stm *stm = new tree::MoveStm(
        new tree::MemExp(
            address_exp),
          check_exp->exp_->UnEx());
    tree::SeqStm *seq_stm = new tree::SeqStm(stm, nullptr);
    last_seq_stm->right_ = seq_stm;
    last_seq_stm = seq_stm;
    count++;
    record++;
  }


  last_seq_stm->right_ =  new tree::ExpStm(new tree::ConstExp(0));
  tree::EseqExp *eseq_exp =
      new tree::EseqExp(left_init_seq_stm, new tree::TempExp(reg));
  if (fields_->GetList().empty()) {
    return new tr::ExpAndTy(new tr::ExExp(eseq_exp), type::NilTy::Instance());
  }
  // The result of the whole expression is r
  return new tr::ExpAndTy(new tr::ExExp(eseq_exp), ty->ActualTy());
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate SeqExp\n");

  /* TODO: Put your lab5 code here */
  // get the last expression
  std::list<absyn::Exp *> exp_list = seq_->GetList();
  absyn::Exp *last_exp = exp_list.back();
  exp_list.pop_back();

  if (exp_list.empty()) {
    DBG("exp_list has only one element");
    // only one exp in seq_->GetList()
    tr::ExpAndTy *last_exp_and_ty =
        last_exp->Translate(venv, tenv, level, label, errormsg);
    return new tr::ExpAndTy(last_exp_and_ty->exp_, last_exp_and_ty->ty_);
  }

  tree::SeqStm *stm = nullptr;
  tree::SeqStm *current_stm = stm;
  bool first = true;
  for (absyn::Exp *exp : exp_list) {
    if (first) {
        first = false;
        tr::ExpAndTy *exp_and_ty =
            exp->Translate(venv, tenv, level, label, errormsg);
        current_stm = stm = new tree::SeqStm(exp_and_ty->exp_->UnNx(), nullptr);
    } else {
        tr::ExpAndTy *exp_and_ty =
            exp->Translate(venv, tenv, level, label, errormsg);
        current_stm->right_ = new tree::SeqStm(
            exp_and_ty->exp_->UnNx(),
            nullptr);
        current_stm =dynamic_cast<tree::SeqStm *>(current_stm->right_);
    }
  }

  tr::ExpAndTy *last_exp_and_ty =
      last_exp->Translate(venv, tenv, level, label, errormsg);

  tree::Exp *exp = nullptr;
  current_stm->right_ = new tree::ExpStm(new tree::ConstExp(0));
  exp = new tree::EseqExp(stm, last_exp_and_ty->exp_->UnEx());

  DBG("Finish translate SeqExp\n");

  // The type and return exp of SeqExp is the type and return exp of the last
  // expression
  return new tr::ExpAndTy(new tr::ExExp(exp), last_exp_and_ty->ty_);
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate ASSIGN exp\n");
  tr::ExpAndTy *check_var = var_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_exp  = exp_->Translate(venv, tenv, level, label, errormsg);
  tr::Exp *exp = new tr::NxExp(new tree::MoveStm(check_var->exp_->UnEx(), check_exp->exp_->UnEx()));
  return new tr::ExpAndTy(exp, type::VoidTy::Instance());
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate IF exp\n");
  tr::ExpAndTy *check_test = test_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_then = then_->Translate(venv, tenv, level, label, errormsg);
  type::Ty *then_ty = check_then->ty_;
  temp::Label *t = temp::LabelFactory::NewLabel();
  temp::Label *f = temp::LabelFactory::NewLabel();
  // Both braches should finish by jumping newly created “joint” label
  temp::Label *joint = temp::LabelFactory::NewLabel();

  // Allocate a temporary r
  temp::Temp *r = temp::TempFactory::NewTemp();

  // Treat e1 as a Cx (apply unCx to e1)
  tr::Cx test_cx = check_test->exp_->UnCx(errormsg);

  test_cx.trues_.DoPatch(t);
  test_cx.falses_.DoPatch(f);
  tr::Exp *exp = nullptr;
  if(elsee_){
    tr::ExpAndTy *check_else = elsee_->Translate(venv,tenv,level,label,errormsg);
    type::Ty *else_ty = check_else->ty_;
    if (!else_ty->IsSameType(then_ty)) {
        errormsg->Error(pos_, "then exp and else exp type mismatch");
        return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)),then_ty);
    }
  tree::SeqStm *true_stm = new tree::SeqStm(
      new tree::LabelStm(t),
      new tree::SeqStm(
          new tree::MoveStm(
              new tree::TempExp(r),
              check_then->exp_->UnEx()),
          new tree::JumpStm(
              new tree::NameExp(joint),
              new std::vector<temp::Label *>{joint}
              )
          )
      );
  DBG("||||||||||||||||||||||||||||||||||||||||||\n");
  tree::SeqStm *false_stm = new tree::SeqStm(
      new tree::LabelStm(f),
      new tree::SeqStm(
          new tree::MoveStm(new tree::TempExp(r),
                            check_else->exp_->UnEx()),
          new tree::JumpStm(new tree::NameExp(joint),
                            new std::vector<temp::Label *>{joint})));
  DBG("||||||||||||||||||||||||||||||||||||||||||\n");
  tree::EseqExp *exp = new tree::EseqExp(
      new tree::SeqStm(
          test_cx.stm_,
          new tree::SeqStm(
              true_stm,
              new tree::SeqStm(false_stm, new tree::LabelStm(joint)))),
      new tree::TempExp(r));
  return new tr::ExpAndTy(new tr::ExExp(exp), then_ty);
  }
  else{
        // no else
        // then must produce no value
        if (!then_ty->IsSameType(type::VoidTy::Instance())) {
          errormsg->Error(then_->pos_, "if-then exp's body must produce no value");
          return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)), then_ty);
        }

        // If e2 is “statements” (Nx), Translate the result as Nx
        // If not meet condition, go to false label
        tree::SeqStm *stm = new tree::SeqStm(
            test_cx.stm_,
            new tree::SeqStm(
                new tree::LabelStm(
                    t),
                new tree::SeqStm(
                    check_then->exp_->UnNx(),
                    new tree::LabelStm(f)
                    )
                )
            );
        return new tr::ExpAndTy(new tr::NxExp(stm), then_ty);
  }

}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate WHILE exp\n");
  tr::Exp *exp = nullptr;

  temp::Label *done_label = temp::LabelFactory::NewLabel();
  tr::ExpAndTy *check_test = test_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_body = body_->Translate(venv, tenv, level, done_label, errormsg);
  temp::Label *test_label = temp::LabelFactory::NewLabel();
  temp::Label *body_label = temp::LabelFactory::NewLabel();
  tr::Cx condition = check_test->exp_->UnCx(errormsg);
  condition.trues_.DoPatch(body_label);
  condition.falses_.DoPatch(done_label);
  //  test:
  //    if not(condition) goto done
  //  body:
  //    body
  //    goto test
  //  done:
auto *labelList = new std::vector<temp::Label *>();
labelList->push_back(test_label);
labelList->push_back(nullptr);
tree::Stm *stm = new tree::SeqStm(
    check_body->exp_->UnNx(),
    new tree::SeqStm(
        new tree::JumpStm(
            new tree::NameExp(test_label),
            new std::vector<temp::Label *>{test_label}
            ),
        new tree::LabelStm(done_label)
        )
    )
;
  exp = new tr::NxExp(
    new tree::SeqStm(
    new tree::LabelStm(test_label),
    new tree::SeqStm(
        condition.stm_,
        new tree::SeqStm(
            new tree::LabelStm(
                body_label
                ),
                stm)
        )
    )
    );
  return new tr::ExpAndTy(exp, type::VoidTy::Instance());
    ;}

tr::ExpAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    DBG("Start translate FOR exp\n");
  tr::Exp *exp = nullptr;
  type::Ty *ty = type::VoidTy::Instance();
  tr::ExpAndTy *check_lo = lo_->Translate(venv,tenv,level,label, errormsg);
  tr::ExpAndTy *check_hi = hi_->Translate(venv,tenv,level,label, errormsg);

  venv->BeginScope();
  venv->Enter(var_,
              new env::VarEntry(
                        tr::Access::AllocLocal(level, escape_),
                        check_lo->ty_,
                        true));
  tr::ExpAndTy *check_body = body_->Translate(venv, tenv, level, label, errormsg);
  // i = lo_
  // limit = hi_
  // while(i<hi_)
  // do { goto body ; i++ }
  // done:
  venv->EndScope();
  auto decList = new absyn::DecList();

  decList->Prepend(
      new absyn::VarDec(
          0,
          sym::Symbol::UniqueSymbol("__limit_var__"),
          sym::Symbol::UniqueSymbol("int"),
          hi_)
      );
  decList->Prepend(
      new absyn::VarDec(
          0,
          var_,
          sym::Symbol::UniqueSymbol("int"),
          lo_));
  auto expList = new absyn::ExpList();
  expList->Prepend(
      new absyn::AssignExp(
          0,
          new absyn::SimpleVar(0,var_),
          new absyn::OpExp(
              0,
              absyn::PLUS_OP,
              new absyn::VarExp(
                  0,
                  new absyn::SimpleVar(0,var_)
                  ),
              new absyn::IntExp(0, 1))));
  expList->Prepend(body_);
  absyn::Exp *forexp_to_letexp = new absyn::LetExp(
      0,
      decList,
      new absyn::WhileExp(
          0,
          new absyn::OpExp(
                 0,
                 absyn::Oper::LE_OP,
                 new absyn::VarExp(
                  0,
                  new absyn::SimpleVar(0, var_)
                  ),
                 new absyn::VarExp(0,
                                new absyn::SimpleVar(
                                          0,
                                          sym::Symbol::UniqueSymbol("__limit_var__")
                                          )
                                   )
                  ),
          new absyn::SeqExp(0, expList)));

return forexp_to_letexp->Translate(venv, tenv, level, label, errormsg);
}

tr::ExpAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
DBG("Start translate BREAK exp\n");
  tree::Stm *stm =
    new tree::JumpStm(new tree::NameExp(label), new std::vector<temp::Label *>({label}));
  return new tr::ExpAndTy(new tr::NxExp(stm), type::VoidTy::Instance());
}

tr::ExpAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate let exp\n");
  tr::Exp *exp = nullptr;

  tree::Exp *res = nullptr;

  venv->BeginScope();
  tenv->BeginScope();
  tree::Stm *stm = nullptr;

  int count = 0;
  for (Dec* dec : decs_->GetList()) {
  if (count == 0) {
        stm = dec->Translate(venv, tenv, level, label, errormsg)->UnNx();
  }
  else {
        stm = new tree::SeqStm(stm, dec->Translate(venv,tenv,level,label, errormsg)->UnNx());
  }
  count++;
  }
  tr::ExpAndTy *check_body = nullptr;
  if(!body_){
  check_body->ty_ = type::VoidTy::Instance();
  check_body->exp_ = new tr::ExExp(new tree::ConstExp(0));;
  }
  else
  check_body = body_->Translate(venv, tenv, level, label, errormsg);
  venv->EndScope();
  tenv->EndScope();
  if(stm)
    res = new tree::EseqExp(stm, check_body->exp_->UnEx());
  else
    res = check_body->exp_->UnEx();
  return new tr::ExpAndTy(
      new tr::ExExp(res),
      check_body->ty_->ActualTy());
}

tr::ExpAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  type::Ty *ty = tenv->Look(typ_)->ActualTy();
  tr::ExpAndTy *check_size = size_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_init = init_->Translate(venv, tenv, level, label, errormsg);
  type::Ty *size_ty = check_size->ty_;
  type::Ty *init_ty = check_init->ty_;
  type::Ty *array_ty = dynamic_cast<type::ArrayTy *>(ty)->ty_;
  auto *expList = new tree::ExpList();
  expList->Append(check_size->exp_->UnEx());
  expList->Append(check_init->exp_->UnEx());
  temp::Temp *r = temp::TempFactory::NewTemp();
  tree::EseqExp *exp = new tree::EseqExp(
      new tree::MoveStm(new tree::TempExp(r),
                        frame::externalCall("init_array", expList)),
      new tree::TempExp(r));
  return new tr::ExpAndTy(new tr::ExExp(exp), ty->ActualTy());
//  return new tr::ExpAndTy(exp, ty);
}

tr::ExpAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate VoidExp\n");
  return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)), type::VoidTy::Instance());
}

tr::Exp *FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate function dec\n");
  sym::Table<int> *check_table = new sym::Table<int>();
  for (FunDec *fundec : functions_->GetList()) {
  if(!fundec){
        errormsg->Error(pos_,"no fundec!!!!!!!!");
        return nullptr;
  }
  if(check_table->Look(fundec->name_)){
        errormsg->Error(pos_, "two functions have the same name");
        continue;
  }
  temp::Label *fun_label =
      temp::LabelFactory::NamedLabel(fundec->name_->Name());
  type::Ty *result_ty = fundec->result_ ?
                                        tenv->Look(fundec->result_)
                                        : type::VoidTy::Instance();
  check_table->Enter(fundec->name_,(int*)1);
  type::TyList *formal_types = make_formal_tylist(
      tenv, fundec->params_,errormsg);
  venv->Enter(
            fundec->name_,
            new env::FunEntry(
                level,
                temp::LabelFactory::NamedLabel(fundec->name_->Name()),
                formal_types,
                result_ty
            )
      );
  }
  DBG("Start second pass");
  for (FunDec *fundec : functions_->GetList()) {

  if(!venv->Look(fundec->name_)){
        errormsg->Error(pos_,fundec->name_->Name()+"undefined !!!!!!!");
        return nullptr;
  };
  DBG("Creating escape list ");
  auto formal_escapes = make_formal_esclist(fundec->params_);

  auto fun_entry =
      dynamic_cast<env::FunEntry *>(
          venv->Look(fundec->name_));
  if(!fun_entry){
        errormsg->Error(pos_,fundec->name_->Name()+"undefined !!!!!!!");
        return nullptr;
  }
  int count = 0;
  DBG("Creating new nesting level");
  tr::Level *function_level =
      tr::Level::NewLevel(
          level,
          fun_entry->label_,
          formal_escapes);
  venv->BeginScope();
  auto field = fundec->params_->GetList().begin();
  auto temp_frame =   function_level->Formals();
  auto access_= temp_frame->begin();
  type::Ty *result_ty = fundec->result_ ?
                                        tenv->Look(fundec->result_)
                                        : type::VoidTy::Instance();
//  auto formal_ty = fun_entry->formals_->GetList().begin();
  type::TyList *formals_ty =
      fundec->params_->MakeFormalTyList(tenv, errormsg);
  auto formal_ty = formals_ty->GetList().begin();
  DBG("Start build escape list");
  for (absyn::Field *param : fundec->params_->GetList()) {
        venv->Enter(
            param->name_,
            new env::VarEntry(
                *access_,
                *formal_ty));
        ++formal_ty;
        ++access_;
  }
  DBG("Start translating body");
    tr::ExpAndTy *body_exp_and_ty =
      fundec->body_->Translate(
          venv, tenv,
          function_level,
          fun_entry->label_,
          errormsg
          );
    DBG("finish translating body");
    if(!body_exp_and_ty){
        errormsg->Error(pos_," Entry undefined !!!!!!!");
        return nullptr;
    }
    venv->EndScope();
    if(!fun_entry->level_){
        errormsg->Error(pos_," Level undefined !!!!!!!");
        return nullptr;
    }
    DBG("take in frag \n");
    tree::Exp *exp = body_exp_and_ty->exp_->UnEx();
    frags->PushBack(
        new frame::ProcFrag(
            frame::procEntryExit1(
                function_level->frame_,
                new tree::MoveStm(
                    new tree::TempExp(reg_manager->ReturnValue()),
                    exp)
                ),
                (function_level->frame_))
                );
  }

  DBG("fundec finished");
  return new tr::ExExp(new tree::ConstExp(0));
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *label,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate var dec\n");
  tr::ExpAndTy *check_init =
      init_->Translate(venv,tenv,level,label, errormsg);

  type::Ty *var_type;
  if(typ_){
    // var x : type_id := exp
    var_type = tenv->Look(typ_);
    if (!var_type) {
        errormsg->Error(pos_, "undefined type %s", typ_->Name().data());
        return new tr::NxExp(new tree::ExpStm(new tree::ConstExp(0)));
    }
    if (!check_init->ty_->ActualTy()->IsSameType(var_type)) {
        errormsg->Error(init_->pos_, "type mismatch");
        return new tr::NxExp(new tree::ExpStm(new tree::ConstExp(0)));
    }
  }
  else{
    if ((check_init->ty_->ActualTy())==(type::NilTy::Instance())) {
        //////?
        errormsg->Error(pos_, "init should not be nil without type specified");
        return new tr::NxExp(
            new tree::ExpStm(new tree::ConstExp(0)));
    }
    var_type = check_init->ty_->ActualTy();
  }
  tr::Access *access = tr::Access::AllocLocal(level, escape_);
  venv->Enter(var_,
              new env::VarEntry(
                  access,
                  var_type));
  return new tr::NxExp(
      new tree::MoveStm(
          access->access_->ToExp(
              new tree::TempExp(
                  reg_manager->FramePointer())),
          check_init->exp_->UnEx()));
}

tr::Exp *TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, temp::Label *label,
                            err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate TypeDec\n");
  for (NameAndTy* current: types_->GetList()) {   // put all the name into the env first
  sym::Symbol *symbol = current->name_;
  tenv->Enter(symbol, new type::NameTy(symbol, nullptr));
  }
  for (NameAndTy *type : types_->GetList()) {
  // find name_ in tenv
  type::NameTy *name_ty =
      dynamic_cast<type::NameTy *>(tenv->Look(type->name_));
  // modify the ty_ field of the type::NameTy class in the tenv for which is
  name_ty->ty_ = type->ty_->Translate(tenv, errormsg);

  // doesn't get type
  if (!name_ty->ty_) {
        errormsg->Error(pos_, "undefined type %s", type->name_);
        break;
  }
  type::Ty *tmp = tenv->Look(type->name_), *next, *start = tmp;
  while (tmp) {
        // break if not a name type
        if (typeid(*tmp) != typeid(type::NameTy))
          break;
        next = (dynamic_cast<type::NameTy *>(tmp))->ty_;
        if (next == start) {
          errormsg->Error(pos_, "illegal type cycle");
          return new tr::ExExp(new tree::ConstExp(0));
        }
        tmp = next;
  }
  }
  return new tr::ExExp(new tree::ConstExp(0));
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  DBG("Start translate NAME exp\n");
  type::Ty *ty = tenv->Look(name_);
  if(!ty){
  errormsg->Error(pos_, "undefined type %s", name_->Name().c_str());
  return type::VoidTy::Instance();
  }
  return new type::NameTy(name_, ty);
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  type::FieldList *fields = record_->MakeFieldList(tenv,errormsg);
  return new type::RecordTy(fields);
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  type::Ty *ty = tenv->Look(array_);
  if(!ty){
  errormsg->Error(pos_, "undefined type %s", array_->Name().c_str());
  return new type::ArrayTy(nullptr);
  }
  return new type::ArrayTy(ty);
}

} // namespace absyn
