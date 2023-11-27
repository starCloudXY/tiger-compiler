#include "tiger/translate/translate.h"

#include <iostream>
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
  return new Access(level,level->frame_->allocLocal(escape));
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
    return exp_;
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    /* TODO: Put your lab5 code here */
    return new tree::ExpStm(exp_);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    /* TODO: Put your lab5 code here */
    auto stm = new tree::CjumpStm(tree::RelOp::NE_OP,exp_,new tree::ConstExp(0), nullptr, nullptr);
    PatchList trues,falses;
    trues.DoPatch(stm->true_label_);
    falses.DoPatch(stm->false_label_);
    return Cx(trues, falses, stm);
  }
};

class NxExp : public Exp {
public:
  tree::Stm *stm_;

  explicit NxExp(tree::Stm *stm) : stm_(stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    /* TODO: Put your lab5 code here */
    return new tree::EseqExp(stm_,new tree::ConstExp(0));
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    /* TODO: Put your lab5 code here */
    return stm_;
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    /* TODO: Put your lab5 code here */

  }
};

class CxExp : public Exp {
public:
  Cx cx_;

  CxExp(PatchList trues, PatchList falses, tree::Stm *stm)
      : cx_(trues, falses, stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    /* TODO: Put your lab5 code here */
    std::cout<<"CU return\n";
    temp::Temp *r = temp::TempFactory::NewTemp();
    temp::Label *t = temp::LabelFactory::NewLabel();
    temp::Label *f = temp::LabelFactory::NewLabel();
    cx_.trues_.DoPatch(t);
    cx_.falses_.DoPatch(f);
    return new tree::EseqExp(
        new tree::MoveStm(
            new tree::TempExp(r),new tree::ConstExp(1)),
        new tree::EseqExp(
            cx_.stm_,
            new tree::EseqExp(
                new tree::LabelStm(f),
                new tree::EseqExp(
                    new tree::MoveStm(new tree::TempExp(r),new tree::ConstExp(0)),
                    new tree::EseqExp(new tree::LabelStm(t),new tree::TempExp(r)))
                )
            )
        );
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    /* TODO: Put your lab5 code here */
    temp::Label  *label = temp::LabelFactory::NewLabel();
    cx_.trues_.DoPatch(label);
    cx_.falses_.DoPatch(label);
    auto cjumpStm = dynamic_cast<tree::CjumpStm *>(cx_.stm_);
    cjumpStm->true_label_ = label;
    cjumpStm->false_label_ = label;
    return new tree::SeqStm(cx_.stm_,new tree::LabelStm(label));
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    /* TODO: Put your lab5 code here */
    return cx_;
  }
};
Level Outermost() {
  return Level(new frame::X64Frame(temp::LabelFactory::NamedLabel("tigermain"), nullptr), nullptr);
}
void ProgTr::Translate() {
  /* TODO: Put your lab5 code here */
  temp::Label *main_label = temp::LabelFactory::NamedLabel("main");
  main_level_ = std::make_unique<tr::Level>(Outermost());
  Level *mainframe = main_level_.get();
  FillBaseTEnv();
  FillBaseVEnv();
  tr::ExpAndTy *main =absyn_tree_->Translate(venv_.get(),tenv_.get(),main_level_.get(),main_label,errormsg_.get());
  frags->PushBack(new frame::ProcFrag(main->exp_->UnNx(), main_level_->frame_));
}

} // namespace tr

namespace absyn {
static type::TyList *make_formal_tylist(env::TEnvPtr tenv, FieldList *params) {
  auto result = new type::TyList();
  if (params){
    for (Field * param : params->GetList()) {
      type::Ty *ty = nullptr;
      if (param->typ_) ty = tenv->Look(param->typ_);
      result->Append(ty);
      std::cout<<"=======> Add field : "<<param->name_->Name()<<"  with type "<< typeid(ty->ActualTy()).name()<<std::endl;
    }
  }
  return result;
}
std::list<bool> *make_formal_esclist(absyn::FieldList *params) {
  auto result = new std::list<bool>();
  std::cout<<"make formal esc list "<<std::endl;
  if(params) {
    for (Field * param : params->GetList()) {
      std::cout<<param->name_->Name()<<" "<<param->escape_<<std::endl;
      result->push_back(param->escape_);
    }
  }
  return result;
}
tr::ExpAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    return root_->Translate(venv,tenv,level,label,errormsg);
}
tree::Exp *StaticLink(tr::Level *target, tr::Level *level) {

    tree::Exp *staticlink = new tree::TempExp(reg_manager->FramePointer());
    while(level!= nullptr&&level != target){
    frame::Access *sl = level->frame_->formals_.back();
    if(sl != nullptr)staticlink = sl->ToExp(staticlink);
    level = level->parent_;
    }
    std::cout<<"finish~~~\n";
    return staticlink;
}
tr::Exp *TranslateSimpleVar(tr::Access *access, tr::Level *level) {
    tree::Exp *real_fp = StaticLink(access->level_, level);
    return new tr::ExExp(access->access_->ToExp(real_fp));
}
tr::Exp *TranslateAssignExp(tr::Exp *var, tr::Exp *exp) {
    return new tr::NxExp(new tree::MoveStm(var->UnEx(), exp->UnEx()));
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    env::EnvEntry *entry = venv->Look(sym_);
    if(!entry){
      errormsg->Error(pos_,"no entry!!!!!");
    }
    std::cout<<"        Translate simple Var : "<<sym_->Name()<<std::endl;
    if(entry&&(typeid(*entry) == typeid(env::VarEntry))){
      std::cout<<"         ===>Address of entry:  "<<&(entry);
    auto varEntry = dynamic_cast<env::VarEntry *>(entry);
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
    std::cout<<"check field entry : "<<sym_->Name()<<std::endl;
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

    std::cout<<"check var and field \n";
    type::RecordTy *actual_ty = dynamic_cast<type::RecordTy *>(check_var->ty_->ActualTy());

    if(!actual_ty){
    errormsg->Error(pos_, "not a record type");
    return new tr::ExpAndTy(nullptr, type::IntTy::Instance());
    }

    type::FieldList *fieldList = (dynamic_cast<type::RecordTy*>(actual_ty))->fields_;
    type::ArrayTy *array_field = (dynamic_cast<type::ArrayTy*>(actual_ty));
    if(array_field)std::cout<< "array type:"<<typeid(array_field->ActualTy()).name()<<std::endl;

    if(fieldList)
    std::cout<<fieldList->GetList().size()<<std::endl;

    int order = 0 ;
    for(type::Field *field:fieldList->GetList()){
    std::cout<<"field var check field "<<field->name_->Name()<<"\n";
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
    std::cout<<"~~~~~~ subscript check"<<std::endl;
    tr::ExpAndTy *check_var = var_->Translate(venv,tenv,level,label,errormsg);

    if(!subscript_){
      errormsg->Error(pos_,"no subscript var");
    }
    tr::ExpAndTy *check_subscript = subscript_->Translate(venv,tenv,level,label,errormsg);
    type::Ty *var_type = check_var->ty_;
    if (typeid(*var_type) == typeid(type::ArrayTy)) {
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
      errormsg->Error(pos_, "array type required");
      return new tr::ExpAndTy(nullptr,type::VoidTy::Instance());
    }
}

tr::ExpAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    std::cout<<"~~~~~~ var check"<<std::endl;
    tr::ExpAndTy *result = var_->Translate(venv, tenv, level, label, errormsg);
    std::cout<<"~~~~~~ var check finish"<<std::endl;
    return result;
}

tr::ExpAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    std::cout<<"~~~~~~ nil check"<<std::endl;
    return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)), type::NilTy::Instance());
}

tr::ExpAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    std::cout<<"translate int : "<<val_<<std::endl;
    return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(val_)), type::IntTy::Instance());
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    std::cout<<"check string ========== "<<str_<<std::endl;
    temp::Label *string_label = temp::LabelFactory::NewLabel();
    auto *stringFrag = new frame::StringFrag(string_label,str_);
    frags->PushBack(stringFrag);
    return new tr::ExpAndTy(new tr::ExExp(new tree::NameExp(string_label)),type::StringTy::Instance());
}

tr::ExpAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  tr::Exp *exp = nullptr;
  env::EnvEntry *entry = nullptr;
  type::Ty *type;
  std::cout<<"begin call function-----------------------------"<<std::endl;
  std::cout<<" "<<func_->Name()<<std::endl;
  if(func_) entry = venv->Look(func_);
  if(!entry){
      errormsg->Error(pos_,"undefined function"+func_->Name()+" %s.");
      return new tr::ExpAndTy(nullptr,type::VoidTy::Instance());
  }
  auto fun_entry = dynamic_cast<env::FunEntry *>(entry);
  auto *exp_list = new tree::ExpList();
  for(Exp *arg : args_->GetList()){
      tr::ExpAndTy *check_arg = arg->Translate(venv,tenv,level,label,errormsg);
      exp_list->Append(check_arg->exp_->UnEx());
  }
  if(!fun_entry->level_->parent_){
      exp = new tr::ExExp(frame::externalCall(func_->Name(), exp_list));
  } else{
      exp_list->Append(StaticLink(fun_entry->level_->parent_,level));
      exp = new tr::ExExp(new tree::CallExp(new tree::NameExp(func_),exp_list));
  }
  if(fun_entry->result_){
      type = fun_entry->result_->ActualTy();
  }
  else type = type::VoidTy::Instance();
  std::cout<<" ========================== call finish--------------------\n";
  return new tr::ExpAndTy(exp, type);
}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"begin operation check~·········"<<std::endl;
  tr::ExpAndTy *leftExpAndTy = left_->Translate(venv,tenv,level,label,errormsg);
  tr::ExpAndTy *rightExpAndTy = right_->Translate(venv,tenv,level,label,errormsg);
  tree::Exp *leftExp = leftExpAndTy->exp_->UnEx();
  tree::Exp *rightExp = rightExpAndTy->exp_->UnEx();
//      PLUS_OP,
//      MINUS_OP,
//      MUL_OP,
//      DIV_OP,
//      AND_OP,
//      OR_OP,
//      LSHIFT_OP,
//      RSHIFT_OP,
//      ARSHIFT_OP,
//      XOR_OP,
//      BIN_OPER_COUNT,
  std::string c = "+-*/&|!<《>》A";
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
      std::cout<<"finish making op exp  "<<c[op]<<"\n";
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
      stm = new tree::CjumpStm(op, leftExp, rightExp, nullptr, nullptr);
      tr::PatchList trues, falses;
      trues.DoPatch(stm->true_label_);
      falses.DoPatch(stm->false_label_);
      exp = new tr::CxExp(trues, falses, stm);
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
        else
          stm = new tree::CjumpStm(tree::RelOp::EQ_OP, leftExp, rightExp, nullptr, nullptr);
        std::cout<<"finish making op exp  =" <<"\n";
        break;
    case NEQ_OP:
        if (dynamic_cast<type::StringTy *>(leftExpAndTy->ty_) != nullptr) {
          auto expList = new tree::ExpList({leftExp, rightExp});
          stm = new tree::CjumpStm(tree::RelOp::NE_OP, frame::externalCall("string_equal", expList), new tree::ConstExp(0), nullptr, nullptr);
        }
          stm = new tree::CjumpStm(tree::RelOp::NE_OP, leftExp, rightExp, nullptr, nullptr);
          std::cout<<"finish making op exp  !=" <<"\n";
          break;
    }
    tr::PatchList trues, falses;
    trues.DoPatch(stm->true_label_);
    falses.DoPatch(stm->false_label_);
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
        std::cout<<"finish making op exp &\n";
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
        std::cout<<"finish making op exp |\n";
        break;
    }
    }
  }
  }
  std::cout<<"========================finish\n";
return new tr::ExpAndTy(exp,type::IntTy::Instance());
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  tr::ExExp *exp = nullptr;
  type::Ty *ty = tenv->Look(typ_);
  std::cout<<" look record for "<<typ_->Name()<<"    "<<typeid(ty->ActualTy()).name()<<std::endl;

  auto list = new tree::ExpList();
  if (!ty){
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return new tr::ExpAndTy(NULL, type::IntTy::Instance());
  }
  auto records = dynamic_cast<type::RecordTy *>(ty->ActualTy())->fields_;
  int count = 0;
  for (EField *eField : fields_->GetList()) {
    count++;
    tr::ExpAndTy *check_exp = eField->exp_->Translate(venv, tenv, level, label, errormsg);
    list->Append(check_exp->exp_->UnEx());
  }
  temp::Temp *reg = temp::TempFactory::NewTemp();
  auto expList = new tree::ExpList({new tree::ConstExp(count * reg_manager->WordSize())});
  tree::Stm *stm = new tree::MoveStm(new tree::TempExp(reg), frame::externalCall("alloc_record", expList));
  count = 0;
  tree::MemExp *t_exp = new tree::MemExp(
      new tree::BinopExp(
          tree::BinOp::PLUS_OP,
          (new tree::TempExp(reg)),
          new tree::ConstExp(count*reg_manager->WordSize()))
          );
  for (tree::Exp *exp1 : list->GetList()) {
    stm = new tree::SeqStm(
        stm, new tree::MoveStm(new tree::MemExp
                               (t_exp), exp1));
    count++;
  }

  exp = new tr::ExExp(new tree::EseqExp(stm, new tree::TempExp(reg)));
  return new tr::ExpAndTy(exp, ty->ActualTy());
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"Check seq_exp!!!!!!!!!\n";
  tr::Exp *exp = new tr::ExExp(new tree::ConstExp(0));
  if(!seq_){
    return new tr::ExpAndTy(nullptr, type::VoidTy::Instance());
  }
  if(seq_->GetList().size() == 0)
    return new tr::ExpAndTy(nullptr, type::VoidTy::Instance());

  auto *check_exp = new tr::ExpAndTy(nullptr, nullptr);
  for (absyn::Exp *exp_:seq_->GetList()) {
    check_exp = exp_->Translate(venv, tenv, level, label, errormsg);
    if(check_exp->exp_){
        exp = new tr::ExExp(new tree::EseqExp(exp->UnNx(), check_exp->exp_->UnEx()));
    }
    else exp = new tr::ExExp(new tree::EseqExp(exp->UnNx(), new tree::ConstExp(0)));
  }

  return new tr::ExpAndTy(exp, check_exp->ty_);
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"assign exp cheching : \n";
  tr::ExpAndTy *check_var = var_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_exp  = exp_->Translate(venv, tenv, level, label, errormsg);
  tr::Exp *exp = new tr::NxExp(new tree::MoveStm(check_var->exp_->UnEx(), check_exp->exp_->UnEx()));
  return new tr::ExpAndTy(exp, type::VoidTy::Instance());
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"===============> begin if exp check\n";
  tr::ExpAndTy *check_test = test_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_then = then_->Translate(venv, tenv, level, label, errormsg);

  tr::Exp *exp = nullptr;
  if(elsee_){
    tr::ExpAndTy *check_elsee = elsee_->Translate(venv, tenv, level, label, errormsg);
    tr::Cx test_c = check_test->exp_->UnCx(errormsg);
    temp::Temp *r = temp::TempFactory::NewTemp();
    temp::Label *true_label = temp::LabelFactory::NewLabel();
    temp::Label *false_label = temp::LabelFactory::NewLabel();
    temp::Label *meeting = temp::LabelFactory::NewLabel();
    test_c.trues_.DoPatch(true_label);
    test_c.falses_.DoPatch(false_label);
    auto cjumpStm = dynamic_cast<tree::CjumpStm *>(test_c.stm_);
    cjumpStm->true_label_ = true_label;
    cjumpStm->false_label_ = false_label;
    auto labelList = new std::vector<temp::Label *>();
    labelList->push_back(meeting);
    labelList->push_back(nullptr);
    exp = new tr::ExExp(new tree::EseqExp(test_c.stm_,
                                          new tree::EseqExp(new tree::LabelStm(true_label),
                                                          new tree::EseqExp(new tree::MoveStm(new tree::TempExp(r), check_then->exp_->UnEx()),
                                                                            new tree::EseqExp(new tree::JumpStm(new tree::NameExp(meeting), labelList),
                                                                                              new tree::EseqExp(new tree::LabelStm(false_label),
                                                                                                                new tree::EseqExp(new tree::MoveStm(new tree::TempExp(r), check_elsee->exp_->UnEx()),
                                                                                                                                  new tree::EseqExp(new tree::JumpStm(new tree::NameExp(meeting), labelList),
                                                                                                                                                    new tree::EseqExp(new tree::LabelStm(meeting), new tree::TempExp(r))))))))));}
  else {
  tr::Cx test_c = check_test->exp_->UnCx(errormsg);
  temp::Label *true_label = temp::LabelFactory::NewLabel();
  temp::Label *false_label = temp::LabelFactory::NewLabel();
  test_c.trues_.DoPatch(true_label);
  test_c.falses_.DoPatch(false_label);
  auto cjumpStm = dynamic_cast<tree::CjumpStm *>(test_c.stm_);
  cjumpStm->true_label_ = true_label;
  cjumpStm->false_label_ = false_label;
  exp = new tr::NxExp(new tree::SeqStm(
      test_c.stm_, new tree::SeqStm(
                       new tree::LabelStm(
                           true_label),
                       new tree::SeqStm(
                           check_then->exp_->UnNx(),
                           new tree::LabelStm(
                               false_label)))));
}
return new tr::ExpAndTy(exp, check_then->ty_);

}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"begin while exp\n";
  tr::Exp *exp = nullptr;

  temp::Label *done_label = temp::LabelFactory::NewLabel();
  tr::ExpAndTy *check_test = test_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_body = body_->Translate(venv, tenv, level, done_label, errormsg);

  temp::Label *test_label = temp::LabelFactory::NewLabel();
  temp::Label *body_label = temp::LabelFactory::NewLabel();
  tr::Cx condition = check_test->exp_->UnCx(errormsg);
  condition.trues_.DoPatch(body_label);
  condition.falses_.DoPatch(done_label);

auto *labelList = new std::vector<temp::Label *>();
labelList->push_back(test_label);
labelList->push_back(nullptr);

  exp = new tr::NxExp(
    new tree::SeqStm(
    new tree::LabelStm(test_label),
    new tree::SeqStm(
        condition.stm_,
        new tree::SeqStm(
            new tree::LabelStm(
                body_label
                ),
            new tree::SeqStm(
                check_body->exp_->UnNx(),
                new tree::SeqStm(
                    new tree::JumpStm(
                        new tree::NameExp(test_label),
                        new std::vector<temp::Label *>{test_label}
                        ),
                    new tree::LabelStm(done_label)
                    )
                )
            )
        )
    )
    );
  return new tr::ExpAndTy(exp, type::VoidTy::Instance());
    ;}

tr::ExpAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  tr::Exp *exp = nullptr;
  type::Ty *ty = type::VoidTy::Instance();
  tr::ExpAndTy *check_lo = lo_->Translate(venv,tenv,level,label, errormsg);
  tr::ExpAndTy *check_hi = hi_->Translate(venv,tenv,level,label, errormsg);

  venv->BeginScope();
  venv->Enter(var_, new env::VarEntry(tr::Access::AllocLocal(level, escape_), check_lo->ty_));
  tr::ExpAndTy *check_body = body_->Translate(venv, tenv, level, label, errormsg);
  venv->EndScope();
  auto decList = new absyn::DecList();
  decList->Prepend(new absyn::VarDec(0, var_, sym::Symbol::UniqueSymbol("int"), lo_));
  decList->Prepend(new absyn::VarDec(0, sym::Symbol::UniqueSymbol("__limit_var__"), sym::Symbol::UniqueSymbol("int"), hi_));

  auto expList = new absyn::ExpList();
  expList->Prepend(new absyn::AssignExp(0, new absyn::SimpleVar(0,var_), new absyn::OpExp(0, absyn::PLUS_OP, new absyn::VarExp(0, new absyn::SimpleVar(0,var_)), new absyn::IntExp(0, 1))));
  expList->Prepend(body_);
  absyn::Exp *forexp_to_letexp = new absyn::LetExp(0, decList,
                                                   new absyn::WhileExp(0, new absyn::OpExp(0, absyn::Oper::LE_OP, new absyn::VarExp(0, new absyn::SimpleVar(0, var_)), new absyn::VarExp(0, new absyn::SimpleVar(0, sym::Symbol::UniqueSymbol("__limit_var__")))),
                                                                       new absyn::SeqExp(0, expList)));

return forexp_to_letexp->Translate(venv, tenv, level, label, errormsg);
}

tr::ExpAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  tree::Stm *stm = new tree::JumpStm(new tree::NameExp(label), new std::vector<temp::Label *>({label}));
  return new tr::ExpAndTy(new tr::NxExp(stm), type::VoidTy::Instance());
}

tr::ExpAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  tr::Exp *exp = nullptr;

  static bool first = true;
  bool isMain = false;
  if(first){
  isMain = true;
  first = false;
  }

  tree::Exp *res = nullptr;

  venv->BeginScope();
  tenv->BeginScope();
  tree::Stm *stm = nullptr;

  int count = 0;
  for (Dec* dec : decs_->GetList()) {
  if (count == 0) {
        stm = dec->Translate(venv, tenv, level, label, errormsg)->UnNx();
        if(stm)std::cout<<"checking dec \n";
  }
  else {
        stm = new tree::SeqStm(stm, dec->Translate(venv,tenv,level,label, errormsg)->UnNx());
  }
  count++;
  }
  tr::ExpAndTy *check_body = body_->Translate(venv, tenv, level, label, errormsg);
  venv->EndScope();
  tenv->EndScope();
  if(stm)
  res = new tree::EseqExp(stm, check_body->exp_->UnEx());
  else
  res = check_body->exp_->UnEx();
  stm = new tree::ExpStm(res);
  if(isMain){
  frags->PushBack(new frame::ProcFrag(stm, level->frame_));
  isMain = false;
  }
  return new tr::ExpAndTy(new tr::ExExp(res),check_body->ty_->ActualTy());
}

tr::ExpAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"translate array: "<<typ_->Name()<<std::endl;
  type::Ty *ty = tenv->Look(typ_)->ActualTy();
  tr::ExpAndTy *check_size = size_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_init = init_->Translate(venv, tenv, level, label, errormsg);

  std::cout<<"   nnnnnnnnnnnnnnnnnnnn========= "<<typeid(check_size->ty_->ActualTy()).name()<<"  "<<typeid(check_init->ty_->ActualTy()).name();
  auto *expList = new tree::ExpList();
  expList->Append(check_size->exp_->UnEx());
  expList->Append(check_init->exp_->UnEx());
  tr::Exp *exp = new tr::ExExp(frame::externalCall("init_array", expList));
  std::cout<<"  "<< typeid(ty->ActualTy()).name()<<std::endl;
  return new tr::ExpAndTy(exp, ty);
}

tr::ExpAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  return new tr::ExpAndTy(nullptr, type::VoidTy::Instance());
}

tr::Exp *FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  sym::Table<int> *check_table = new sym::Table<int>();
  for (FunDec *fundec : functions_->GetList()) {
  std::cout<<" adding function:  "<<fundec->name_->Name()<<"\n";
  if(!fundec){
        errormsg->Error(pos_,"no fundec!!!!!!!!");
        return nullptr;
  }
  if(check_table->Look(fundec->name_)){
        errormsg->Error(pos_, "two functions have the same name");
        continue;
  }
  check_table->Enter(fundec->name_,(int*)1);
  type::TyList *formal_types = make_formal_tylist(tenv, fundec->params_);
  std::list<bool> *esc_list =make_formal_esclist(
      fundec->params_
      );
  std::cout<<"length "<<esc_list->size()<<std::endl;
  tr::Level *new_level = tr::Level::NewLevel(
      level,
      fundec->name_,
      esc_list
);
  if(!fundec->result_){
        venv->Enter(
            fundec->name_,
            new env::FunEntry(
                new_level,
                fundec->name_,
                formal_types,
                type::VoidTy::Instance())
            );
        continue;
  }
  else {
        type::Ty *result = tenv->Look(fundec->result_);
        venv->Enter(
            fundec->name_,
            new env::FunEntry(
                new_level,
                fundec->name_,
                formal_types,
                result));
  }
  }
  for (FunDec *fundec : functions_->GetList()) {
  std::cout<<"checking function:  "<<fundec->name_->Name()<<"\n";
  venv->BeginScope();
  if(!venv->Look(fundec->name_)){
        errormsg->Error(pos_,fundec->name_->Name()+"undefined !!!!!!!");
        return nullptr;
  };
  auto fun_entry = dynamic_cast<env::FunEntry *>(venv->Look(fundec->name_));
  if(!fun_entry){
        errormsg->Error(pos_,fundec->name_->Name()+"undefined !!!!!!!");
        return nullptr;
  }
  int count = 0;
  auto field = fundec->params_->GetList().begin();
  auto temp_frame = dynamic_cast<frame::X64Frame*>(fun_entry->level_->frame_)->formals_;
  auto access_= temp_frame.begin();
  auto formal_ty = fun_entry->formals_->GetList().begin();
  for (;field!=fundec->params_->GetList().end()&&access_!=temp_frame.end()&&formal_ty!=fun_entry->formals_->GetList().end();
       field++,access_++,formal_ty++) {
        std::cout<<"Enter venv : "<<(*field)->name_->Name()<<std::endl;
        if(!(*field)){
          std::cout<<"no field \n";
        }
        if(!(*formal_ty)){
          std::cout<<"no type \n";
        }
        if(!*access_){
          std::cout<<"no access \n";
        }
        venv->Enter(
            (*field)->name_,
            new env::VarEntry(
                new tr::Access(
                    fun_entry->level_,
                    *access_),
                *formal_ty));
        std::cout<<"    "<<(*access_)->reg<<"  "<<(*access_)<<" with type "<< typeid((*formal_ty)->ActualTy()).name()<<std::endl;
  }
  std::cout<<"end adding \n";
    tr::ExpAndTy *entry = fundec->body_->Translate(venv, tenv, fun_entry->level_, fun_entry->label_, errormsg);
    if(!entry){
        errormsg->Error(pos_," Entry undefined !!!!!!!");
        return nullptr;
    }
    venv->EndScope();
    std::cout<<"end scope \n";
    if(!fun_entry->level_){
        errormsg->Error(pos_," Level undefined !!!!!!!");
        return nullptr;
    }
    if(!entry->exp_){
        errormsg->Error(pos_,"No exp !!!!!!!");
        return nullptr;
    }
    tree::TempExp *return_r = new tree::TempExp(reg_manager->ReturnValue());
    tree::Exp *exp = entry->exp_->UnEx();
    frame::Frame *frame_f =fun_entry->level_->frame_;
    frags->PushBack(
        new frame::ProcFrag(
            frame::procEntryExit_fp(
                frame_f,
                new tree::MoveStm(
                    return_r,
                    exp)),
                frame_f)
                );
  }
  return new tr::ExExp(new tree::ConstExp(0));
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *label,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
 std::cout<<"\ntranslating  : ";

 std::cout<<var_->Name()<<std::endl;
  tr::ExpAndTy *check_init = init_->Translate(venv,tenv,level,label, errormsg);

  tr::Access *access;
  access = tr::Access::AllocLocal(level, true);
  venv->Enter(var_, new env::VarEntry(access, check_init->ty_));

  return TranslateAssignExp(TranslateSimpleVar(access, level), check_init->exp_);
}

tr::Exp *TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, temp::Label *label,
                            err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  for (NameAndTy* current: types_->GetList()) {   // put all the name into the env first
  sym::Symbol *symbol = current->name_;
  std::cout<< "     define symbol : " <<symbol->Name()<<" "<<typeid(current->ty_).name()<<std::endl;
  tenv->Enter(symbol, new type::NameTy(symbol, current->ty_->Translate(tenv, errormsg)));
  }
  return new tr::ExExp(new tree::ConstExp(0));
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
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
