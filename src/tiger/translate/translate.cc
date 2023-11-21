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
    trues.DoPatch(nullptr);
    falses.DoPatch(stm->false_label_);
    falses.DoPatch(nullptr);
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
  absyn_tree_->Translate(venv_.get(),tenv_.get(),main_level_.get(),main_label,errormsg_.get());
}

} // namespace tr

namespace absyn {
static type::TyList *make_formal_tylist(env::TEnvPtr tenv, FieldList *params) {
  std::cout<<"making list in  \n";
  auto result = new type::TyList();
  if (params){
    for (Field * param : params->GetList()) {
      type::Ty *ty = nullptr;
      if (param->typ_) ty = tenv->Look(param->typ_);
      result->Append(ty);
    }
  }
  std::cout<<"finish return list\n";
  return result;
}
std::list<bool> *make_formal_esclist(absyn::FieldList *params) {
  auto result = new std::list<bool>();
  if(params) {
    for (Field * param : params->GetList()) {
      result->push_back(param->escape_);
    }
  }
  std::cout<<"finish return formal list\n";
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
    std::cout<<"linking exp!!!!!!!!!!!!\n";
    if(level== nullptr){
    std::cout<<"ln+++++1!!!!!!!!!!!!\n";
    }
    if(target== nullptr){
    std::cout<<"tn+++++1!!!!!!!!!!!!\n";
    }
    if(level->frame_== nullptr){
    std::cout<<"lfream+++++1!!!!!!!!!!!!\n";
    }
    while(level!= nullptr&&level != target){
    std::cout<<"Level\n";

    frame::Access *sl = level->frame_->formals_.back();
    if(sl != nullptr)staticlink = sl->ToExp(staticlink);
    level = level->parent_;
    std::cout<<"get ~~~\n";

    }
    std::cout<<"finish~~~\n";
    return staticlink;
}
tr::Exp *TranslateSimpleVar(tr::Access *access, tr::Level *level) {
    std::cout<<"translating!!!!!!!!!!!!\n";
    tree::Exp *real_fp = StaticLink(access->level_, level);
    std::cout<<"translating finish!!!!!!!!!!!!\n";

    if(access == nullptr){
    std::cout<<"no accesss!\n";
    }
    std::cout<<"1\n";
    if(access->access_== nullptr){
    std::cout<<"no accesssdiajdasds!\n";
    }
    std::cout<<"2\n";
    if(!real_fp){
    std::cout<<"no real fps!\n";
    }
    else {
//    tree::Exp *result = access->access_->ToExp(real_fp);
    frame::Access *step_1 = access->access_;
    std::cout<<"aiuweqwuheiquwhe!\n";
    if(step_1== nullptr){
      std::cout<<"no accesssdiajdasds!\n";
    }
    tree::Exp *result = step_1->ToExp(real_fp);
    std::cout<<"rettttttttt!\n";
    return new tr::ExExp(result);
    }

}

tr::Exp *TranslateAssignExp(tr::Exp *var, tr::Exp *exp) {
    return new tr::NxExp(new tree::MoveStm(var->UnEx(), exp->UnEx()));
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    env::EnvEntry *entry = venv->Look(sym_);
    assert(entry);
    std::cout<<"~~~~~~ entry : "<<sym_->Name()<<std::endl;
    if(entry&&(typeid(*entry) == typeid(env::VarEntry))){
    auto varEntry = dynamic_cast<env::VarEntry *>(entry);
    std::cout<<"~~~~~~ entry finish : "<<sym_->Name()<<std::endl;
    auto exp = TranslateSimpleVar(varEntry->access_,level);
    std::cout<<"~~~~~~ Translate finish : "<<sym_->Name()<<std::endl;
    if(!varEntry->ty_){
      errormsg->Error(pos_,"aaaaaaaaaa no type!!!!!!");
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
    std::cout<<"~~~~~~ field entry : "<<sym_->Name()<<std::endl;
    tr::ExpAndTy  *check_var = var_->Translate(venv,tenv,level,label,errormsg);
    if(check_var->ty_== nullptr){
    return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)), nullptr);
    }
    type::Ty *actual_ty = check_var->ty_->ActualTy();
    type::FieldList *fieldList = ((type::RecordTy*)actual_ty)->fields_;
    int order = 0 ;
    for(type::Field *field:fieldList->GetList()){
      if(field->name_==sym_){
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
}

tr::ExpAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level, temp::Label *label,
                                      err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
    std::cout<<"~~~~~~ subscript check"<<std::endl;
    tr::ExpAndTy *check_var = var_->Translate(venv,tenv,level,label,errormsg);
    tr::ExpAndTy *check_subscript = subscript_->Translate(venv,tenv,level,label,errormsg);
    tr::Exp *exp = new tr::ExExp(
        new tree::MemExp(
            new tree::BinopExp(
                tree::BinOp::PLUS_OP, check_var->exp_->UnEx(),
                new tree::BinopExp(
                    tree::BinOp::MUL_OP, check_subscript->exp_->UnEx(),
                    new tree::ConstExp(
                        reg_manager->WordSize())))));
    return new tr::ExpAndTy(exp, check_subscript->ty_->ActualTy());
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
    return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(val_)), type::IntTy::Instance());
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
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
  std::cout<<"~~~~~~ begin checking fun!!!!!"<<std::endl;
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
  return new tr::ExpAndTy(exp, type);
}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  std::cout<<"~~~~~~~~~~~~~ op check~·········"<<std::endl;
  tr::ExpAndTy *leftExpAndTy = left_->Translate(venv,tenv,level,label,errormsg);
  std::cout<<"A\n";
  tr::ExpAndTy *rightExpAndTy = right_->Translate(venv,tenv,level,label,errormsg);
  std::cout<<"b\n";
  tree::Exp *leftExp = leftExpAndTy->exp_->UnEx();
  std::cout<<"c\n";
  tree::Exp *rightExp = rightExpAndTy->exp_->UnEx();
  tr::Exp *exp = nullptr;
  std::cout<<"finishing\n";
  switch (oper_) {
  case PLUS_OP:
  case MINUS_OP:
  case TIMES_OP:
  case DIVIDE_OP:
  {
      std::cout<<"============================\n";
      enum tree::BinOp op;
      std::cout<<"begin op  check"<<std::endl;
      op = tree::BinOp(oper_-PLUS_OP+tree::PLUS_OP);
      std::cout<<op<<std::endl;
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
  case GE_OP: {
      tree::CjumpStm *stm;
      enum tree::RelOp op;
      op = tree::RelOp(oper_ - LT_OP + tree::RelOp::LT_OP);
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
        break;
    case NEQ_OP:
          stm = new tree::CjumpStm(tree::RelOp::NE_OP, leftExp, rightExp, nullptr, nullptr);
        break;
    }
    tr::PatchList trues, falses;
    trues.DoPatch(stm->true_label_);
    falses.DoPatch(stm->false_label_);
    exp = new tr::CxExp(trues, falses, stm);
    break;
  }
  case AND_OP:{


    break;
  }
  case OR_OP:{
    break;
  }
  }
  return new tr::ExpAndTy(exp,type::IntTy::Instance());
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,      
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  tr::ExExp *exp = nullptr;
  type::Ty *ty = tenv->Look(typ_);
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
  tr::ExpAndTy *check_var = var_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_exp  = exp_->Translate(venv, tenv, level, label, errormsg);
  tr::Exp *exp = new tr::NxExp(new tree::MoveStm(check_var->exp_->UnEx(), check_exp->exp_->UnEx()));
  return new tr::ExpAndTy(exp, type::VoidTy::Instance());
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
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
tr::Exp *exp = nullptr;

temp::Label *done_label = temp::LabelFactory::NewLabel();
tr::ExpAndTy *check_test = test_->Translate(venv, tenv, level, label, errormsg);
tr::ExpAndTy *check_body = body_->Translate(venv, tenv, level, done_label, errormsg);

temp::Label *test_label = temp::LabelFactory::NewLabel();
temp::Label *body_label = temp::LabelFactory::NewLabel();
tr::Cx condition = check_test->exp_->UnCx(errormsg);
condition.trues_.DoPatch(body_label);
condition.falses_.DoPatch(done_label);

auto cjumpStm = dynamic_cast<tree::CjumpStm *>(condition.stm_);
cjumpStm->true_label_ = body_label;
cjumpStm->false_label_ = done_label;

auto *labelList = new std::vector<temp::Label *>();
labelList->push_back(test_label);
labelList->push_back(nullptr);
exp = new tr::NxExp(
    new tree::SeqStm(new tree::LabelStm(test_label),
                     new tree::SeqStm(condition.stm_,
                                      new tree::SeqStm(new tree::LabelStm(body_label),
                                                       new tree::SeqStm(check_body->exp_->UnNx(),
                                                                        new tree::SeqStm(new tree::JumpStm(new tree::NameExp(test_label), labelList),
                                                                                         new tree::LabelStm(done_label)))))));

return new tr::ExpAndTy(exp, type::VoidTy::Instance());
}

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
  std::cout<<"let checking1!!!!!!!!!\n";
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
        if(stm )std::cout<<"cheching dec \n";
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
  type::Ty *ty = tenv->Look(typ_)->ActualTy();
  tr::ExpAndTy *check_size = size_->Translate(venv, tenv, level, label, errormsg);
  tr::ExpAndTy *check_init = init_->Translate(venv, tenv, level, label, errormsg);
  auto *expList = new tree::ExpList();
  expList->Append(check_size->exp_->UnEx());
  expList->Append(check_init->exp_->UnEx());
  tr::Exp *exp = new tr::ExExp(frame::externalCall("init_array", expList));
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
  for (FunDec *fundec : functions_->GetList()) {
  std::cout<<"adding~~~~~~~~~~~  "<<fundec->name_->Name()<<"\n";
  if(!fundec){
        errormsg->Error(pos_,"no fundec!!!!!!!!");
        return nullptr;
  }
  type::TyList *formal_types = make_formal_tylist(tenv, fundec->params_);
  std::cout<<"sadsada~~~~~~~~  "<<"\n";
  tr::Level *new_level = tr::Level::NewLevel(
      level,
      fundec->name_,
      make_formal_esclist(fundec->params_));
  std::cout<<"sdfsdfsdf~~~~~~~  "<<"\n";
  if(!fundec->result_){
        venv->Enter(fundec->name_, new env::FunEntry(new_level, fundec->name_, formal_types, type::VoidTy::Instance()));
  }
  else {
        type::Ty *result = tenv->Look(fundec->result_);
        venv->Enter(fundec->name_, new env::FunEntry(new_level, fundec->name_, formal_types, result));
  }
  }
  for (FunDec *fundec : functions_->GetList()) {
  std::cout<<"checking~~~~~~~~~~~  "<<fundec->name_->Name()<<"\n";
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
  for (Field *field : fundec->params_->GetList()) {
        std::cout<<4<<std::endl;
        type::Ty *ty = nullptr;
        if (field->typ_) ty = tenv->Look(field->typ_);
        frame::Access *resultAccess;
        int count2 = 0;
        for (frame::Access *access : fun_entry->level_->frame_->formals_) {
          if (count2 == count) {
            resultAccess = access;
          }
          count2++;
        }
        venv->Enter(field->name_, new env::VarEntry(new tr::Access(fun_entry->level_, resultAccess), ty));
        count++;
  }
    tr::ExpAndTy *entry = fundec->body_->Translate(venv, tenv, fun_entry->level_, fun_entry->label_, errormsg);
    if(!entry){
        errormsg->Error(pos_," Entry undefined !!!!!!!");
        return nullptr;
    }
    venv->EndScope();
    std::cout<<6<<std::endl;
    if(!fun_entry->level_){
        errormsg->Error(pos_," Level undefined !!!!!!!");
        return nullptr;
    }
    if(!entry->exp_){
        errormsg->Error(pos_,"No exp !!!!!!!");
        return nullptr;
    }
    tree::TempExp *return_r = new tree::TempExp(reg_manager->ReturnValue());
    std::cout<<7<<std::endl;
    tree::Exp *exp = (entry->exp_)?entry->exp_->UnEx():nullptr;
    std::cout<<8<<std::endl;
    frame::Frame *frame_f =fun_entry->level_->frame_;
    std::cout<<9<<std::endl;
    frags->PushBack(
        new frame::ProcFrag(
        new tree::MoveStm(
            return_r,
                exp),
            frame_f));
  }
  std::cout<<10<<std::endl;
  return new tr::ExExp(new tree::ConstExp(0));
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *label,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
 std::cout<<"translating  ";

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
  //        errormsg->Error(pos_, "define symbol " + symbol->Name());
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
