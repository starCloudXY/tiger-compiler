#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include "tiger/translate/translate.h"

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  this->root_->SemAnalyze(venv,tenv,0,errormsg);

}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  env::EnvEntry *entry = venv->Look(sym_);
  if(entry && typeid(*entry)== typeid(env::VarEntry)){
    return (static_cast<env::VarEntry*>(entry))->ty_->ActualTy();
  }
  else{
    errormsg->Error(pos_,"undefined variable %s",sym_->Name().data());
  }
  return type::IntTy::Instance();
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *variable_type = var_->SemAnalyze(venv,tenv,labelcount,errormsg);
  if(variable_type== nullptr){
    errormsg->Error(pos_,"variable not defined");
    return type::NilTy::Instance();
  }
  else {
    if(typeid(*(variable_type->ActualTy()))!=typeid(type::RecordTy)){
      errormsg->Error(pos_,"variable not a record type");
      return type::NilTy::Instance();
    }
    else{
      type::RecordTy *real_type = (type::RecordTy*)variable_type;
      //find sym_ in field of var
      for(type::Field *field:real_type->fields_->GetList()){
        if(field->name_->Name()==sym_->Name()){
          ////返回的是sym_的ty还是所在var的ty>
          return field->ty_;
        }
      }
      errormsg->Error(pos_,"field "+sym_->Name()+" doesn't exist");
      return real_type;
    }
  }
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *variable_type = var_->SemAnalyze(venv,tenv,labelcount,errormsg);

  if(variable_type== nullptr){
    errormsg->Error(pos_,"variable not defined");
    return type::NilTy::Instance();
  }
  else {
    if(typeid(*(variable_type->ActualTy()))!=typeid(type::ArrayTy)){
      errormsg->Error(pos_,"array type required");
      return type::NilTy::Instance();
    }
    else{
      type::Ty *subscript_type = subscript_->SemAnalyze(venv,tenv,labelcount,errormsg);
      if(typeid(*(subscript_type->ActualTy()))!= typeid(type::IntTy)){
        errormsg->Error(pos_,"subscript is not an int type");
      }
      else{
        ////此处返回arrayTy还是包含元素的类型?
        return ((type::ArrayTy *)variable_type)->ty_->ActualTy();
      }
    }
  }
  return type::NilTy::Instance();
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
    return var_->SemAnalyze(venv,tenv,labelcount,errormsg);
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
    return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
    return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
    //func_函数名;args_参数
  env::FunEntry *funEntry = (env::FunEntry*) venv->Look(func_);
  DBG("sem analyze call function ");
  DBG(func_->Name()+"\n");
  if(funEntry== nullptr){
    errormsg->Error(pos_,"undefined function "+func_->Name());
  }
  auto formal_ = funEntry->formals_->GetList().begin();
  auto arg = args_->GetList().begin();
    //逐个比较传入参数
  for(;formal_!=funEntry->formals_->GetList().end()&&arg!=args_->GetList().end();formal_++,arg++){
    type::Ty *ty = (*arg)->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy();
    if(typeid(*(*formal_)->ActualTy())!=
        typeid(*(ty->ActualTy()))){
      errormsg->Error((*arg)->pos_,"para type mismatch");
    }
  }
  if(arg != args_->GetList().end()){
    errormsg->Error((*arg)->pos_,"too many params in function "+func_->Name());
    return type::NilTy::Instance();
  }
  if(formal_ != funEntry->formals_->GetList().end()){
    errormsg->Error((*arg)->pos_,"too little params in function"+func_->Name());
    return type::NilTy::Instance();
  }
  ////nullptr?
  return ((env::FunEntry*)funEntry)->result_->ActualTy();
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty *left_ty = left_->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy();
  type::Ty *right_ty = right_->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy();
  if(oper_==absyn::PLUS_OP||oper_ == absyn::TIMES_OP
      ||oper_==AND_OP||oper_==absyn::DIVIDE_OP){
    if(typeid(*left_ty)!= typeid(type::IntTy)){
      errormsg->Error(left_->pos_,"integer required");
    }
    if(typeid(*right_ty)!= typeid(type::IntTy)){
      errormsg->Error(right_->pos_,"integer required");
    }
    return type::IntTy::Instance();
  }
  else{
    if(!left_ty->IsSameType(right_ty)){
      errormsg->Error(pos_,"same type required");
      return type::IntTy::Instance();
    }
    return type::IntTy::Instance();
  }
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  type::Ty * ty = tenv->Look(typ_);
  if(!ty){
    errormsg->Error(pos_,"undefined type "+typ_->Name());
    return type::NilTy::Instance();
  }

  return ty;
}

type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  auto seq_list = seq_->GetList();
  if(seq_list.empty())return type::VoidTy::Instance();
  type::Ty *ty;
  for(auto iter = seq_list.begin();iter!=seq_list.end();iter++){
    ty = (*iter)->SemAnalyze(venv,tenv,labelcount,errormsg);
  }
  return ty;
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  type::Ty *var_ty = var_->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy();
  type::Ty *exp_ty = exp_->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy();
  if(typeid(*(exp_ty->ActualTy()))==typeid(type::NilTy)&&
      typeid(*(var_ty))== typeid(type::RecordTy)){
    return type::NilTy::Instance();
  }
  else if (var_ty && typeid(*(var_ty)->ActualTy()) == typeid(type::IntTy)) {
    absyn::SimpleVar *simpleVar = dynamic_cast<SimpleVar *>(var_);
    if (simpleVar != nullptr) {
      env::EnvEntry *envEntry;
      if (simpleVar->sym_ == nullptr) {
        envEntry = nullptr;
      }
      else {
        envEntry = venv->Look(simpleVar->sym_);
      }
      if (envEntry != nullptr) {
        bool readonly = envEntry->readonly_;
        if (readonly) {
          errormsg->Error(pos_, "loop variable can't be assigned");
        }
      }
    }
  }
  ///for loop variable
  if(typeid(*(var_ty->ActualTy()))!= typeid(*(exp_ty->ActualTy()))){
    errormsg->Error(pos_,"unmatched assign exp");
  }
  return type::VoidTy::Instance();
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  //if,then,else
  test_->SemAnalyze(venv,tenv,labelcount,errormsg);
  type::Ty* body_type = then_->SemAnalyze(venv,tenv,labelcount,errormsg);
  if(!elsee_){
    // no else
    // then must produce no value
    if(body_type&&!body_type->IsSameType(type::VoidTy::Instance())&&
        typeid(*(body_type->ActualTy()))!=typeid(type::NilTy))
      errormsg->Error(pos_,"if-then exp's body must produce no value");
    return type::VoidTy::Instance();
  }
  else{
      type::Ty *else_type = elsee_->SemAnalyze(venv,tenv,labelcount,errormsg);
      if(body_type->IsSameType(else_type)) {
      return  body_type;
      }
      else{
      errormsg->Error(pos_, "then exp and else exp type mismatch");
      return type::VoidTy::Instance();
      }
  }
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  venv->BeginScope();
  type::Ty *test_type = test_->SemAnalyze(venv,tenv,labelcount,errormsg);
  ////存在test type应该为IntTy
  if(test_type&&typeid(*(test_type->ActualTy()))!= typeid(type::IntTy)){
    errormsg->Error(pos_,"while test need to be IntTy");
  }
  type::Ty *result;
  if(!body_)
    result = type::NilTy::Instance();
  else
    result = body_->SemAnalyze(venv,tenv,labelcount+1,errormsg);
  if (typeid(*(result->ActualTy()))!= typeid(type::VoidTy))
    errormsg->Error(body_->pos_, "while body must produce no value");
  venv->EndScope();
  return result;
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  venv->BeginScope();
  tenv->BeginScope();
  type::Ty *lo_type = lo_->SemAnalyze(venv,tenv,labelcount,errormsg);
  venv->Enter(var_,new env::VarEntry(lo_type, true));
  type::Ty *hi_type = hi_->SemAnalyze(venv,tenv,labelcount,errormsg);
  if(typeid(*(hi_type->ActualTy()))!= typeid(type::IntTy)){
    errormsg->Error(hi_->pos_,"for exp's range type is not integer");
  }
  type::Ty *result;
  if(!body_)
    result = type::VoidTy::Instance();
  else
    result = body_->SemAnalyze(venv,tenv,labelcount+1,errormsg);
  venv->EndScope();
  tenv->EndScope();
  return result;
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  if(labelcount <= 0){
    errormsg->Error(pos_,"break is not inside any loop.");
  }
    return type::NilTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  venv->BeginScope();
  tenv->BeginScope();
  for(Dec *dec:decs_->GetList()){
    dec->SemAnalyze(venv,tenv,labelcount,errormsg);
  }
  type::Ty *result;
  if(!body_)
    result = type::VoidTy::Instance();
  else
    result = body_->SemAnalyze(venv,tenv,labelcount,errormsg);
  tenv->EndScope();
  venv->EndScope();
  return result;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  if(typ_ == nullptr){
    errormsg->Error(pos_,"array doesn't have a type");
    return nullptr;
  }
  else {
    type::Ty *array_type = tenv->Look(typ_)->ActualTy();
    type::Ty *init_element_type = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
    size_->SemAnalyze(venv,tenv,labelcount,errormsg);
    if(((type::ArrayTy*)array_type)->ty_&&!((type::ArrayTy*)array_type)->ty_->IsSameType(init_element_type)
        )
    {
     errormsg->Error(pos_,"type mismatch");
    }
    return array_type;
  }
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

  const std::list<FunDec *> * function_list = &functions_->GetList();
  for (auto iter = function_list->begin(); iter != function_list->end(); iter++) {
    auto next_iter = iter;
    if (next_iter != function_list->end()) next_iter++;
    for (; next_iter != function_list->end(); next_iter++)
      if ((*next_iter)->name_->Name() ==(*iter)->name_->Name())
        errormsg->Error((*iter)->pos_, "two functions have the same name");

    type::Ty* result;
    if ((*iter)->result_ != nullptr) {
      result = tenv->Look((*iter)->result_);
      if (result == nullptr)
        errormsg->Error(pos_, "FunctionDec undefined result");
    } else result = type::VoidTy::Instance();
    venv->Enter((*iter)->name_, new env::FunEntry((*iter)->params_->MakeFormalTyList(tenv,errormsg), result));
  };

  for (auto iter = function_list->begin(); iter != function_list->end(); iter++) {
    venv->BeginScope();
    const std::list<Field *>* field_list = &(*iter)->params_->GetList();
    for (auto field_iter = field_list->begin(); field_iter != field_list->end(); field_iter++) {
      type::Ty* ty = tenv->Look((*field_iter)->typ_);
      if (!ty) errormsg->Error((*field_iter)->pos_, "undefined type %s", (*field_iter)->typ_->Name().c_str());
      venv->Enter((*field_iter)->name_, new env::VarEntry(ty));
    };
    type::Ty* body_ty = (*iter)->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
    type::Ty* dec_ty = ((env::FunEntry*) venv->Look((*iter)->name_))->result_;
    if (!body_ty->IsSameType(dec_ty)) {
      if (typeid(*dec_ty) == typeid(type::VoidTy)) errormsg->Error((*iter)->body_->pos_, "procedure returns value");
      else errormsg->Error((*iter)->body_->pos_, "return type mismatch");

    };
    venv->EndScope();
  };
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
//  for var x : type_id := exp,
//                  it must check that type_id and type of exp are compatible
//                      if the type of exp is NilTy,
//                  type_id must be a RecordTy type
//                      var a : my_record := nil
  type::Ty *exp_ty = init_->SemAnalyze(venv, tenv, labelcount, errormsg);

  if (typ_ == nullptr) {
    if (typeid(*(exp_ty->ActualTy())) == typeid(type::NilTy))
      errormsg->Error(pos_, "init should not be nil without type specified");
    venv->Enter(var_, new env::VarEntry(exp_ty->ActualTy()));
  }
  else{
    type::Ty *type_ty = tenv->Look(typ_);
    if(exp_ty== nullptr){
      errormsg->Error(pos_, "init type is nullptr");
    }
    if(typeid(*(exp_ty->ActualTy()))== typeid(type::NilTy)){
      if(type_ty&&typeid(*(type_ty->ActualTy()))!= typeid(type::RecordTy))
        errormsg->Error(pos_, "init should not be nil without type specified");
    }
    if(type_ty->IsSameType(exp_ty))
    venv->Enter(var_,new env::VarEntry(type_ty));
    else errormsg->Error(pos_,"type mismatch");
  }

}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code her */
  const std::list<NameAndTy *> * type = &types_->GetList();
  //先存name
  for (auto iter = type->begin();iter != type->end(); iter++) {
    auto next_iter = iter;
    if (next_iter != type->end()) next_iter++;
    for (; next_iter != type->end(); next_iter++)
      if ((*next_iter)->name_ == (*iter)->name_)
        errormsg->Error(pos_, "two types have the same name");
    //type存成nullptr
    tenv->Enter((*iter)->name_, new type::NameTy((*iter)->name_, nullptr));
  };

  for (auto iter = type->begin(); iter != type->end(); iter++) {
    //再将相应的env存进去
//    type::Ty *ty = (*iter)->ty_->SemAnalyze(tenv, errormsg);

//    tenv->Set((*iter)->name_,ty);
    type::NameTy* name_ty = (type::NameTy*) tenv->Look((*iter)->name_);
    name_ty->ty_ = (*iter)->ty_->SemAnalyze(tenv, errormsg);
  };

  bool cycle = false;
  const std::list<NameAndTy *> * ty_list = &types_->GetList();
  for (auto iter = ty_list->begin(); iter != ty_list->end(); iter++) {
    type::Ty* ty = tenv->Look((*iter)->name_);
    if (typeid(*(ty))== typeid(type::NameTy)) {
      type::Ty* ty_ty = ((type::NameTy*) ty)->ty_;
      while (ty_ty != nullptr &&
             typeid(*(ty_ty))== typeid(type::NameTy)) {
        type::NameTy* name_ty = (type::NameTy*) ty_ty;
        if (name_ty->sym_->Name() == (*iter)->name_->Name()) {
          errormsg->Error(pos_, "illegal type cycle");
          cycle = true;
          break;
        };
        ty_ty = name_ty->ty_;
      };
    };
    if (cycle) break;
  };

  return;
}

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
    return  new type::NameTy(name_, tenv->Look(name_));
}

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */

    return new type::RecordTy(record_->MakeFieldList(tenv,errormsg));
}

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
    return new type::ArrayTy(tenv->Look(array_));
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());

}

} // namespace tr
