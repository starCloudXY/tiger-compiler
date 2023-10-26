#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include <iostream>

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
  } else{
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
    ////错了的话return啥比较好
    return type::NilTy::Instance();
  }
  else {
    ////究竟是比较Actual Type还是Type
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
    ////究竟是比较Actual Type还是Type
    if(typeid(*(variable_type->ActualTy()))!=typeid(type::ArrayTy)){
      errormsg->Error(pos_,"variable not a array type");
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
    //
    std::cout<<" Begin Call Check!!"<<std::endl;
  env::FunEntry *funEntry = (env::FunEntry*) venv->Look(func_);
  if(funEntry== nullptr){
    errormsg->Error(pos_,"function "+func_->Name()+" is undefined!");
  }
  auto formal_ = funEntry->formals_->GetList().begin();
  auto arg = args_->GetList().begin();
    //逐个比较传入参数
  for(;formal_!=funEntry->formals_->GetList().end()&&arg!=args_->GetList().end();formal_++,arg++){
    if(typeid((*arg)->SemAnalyze(venv,tenv,labelcount,errormsg)->ActualTy())!=
        typeid((*formal_)->ActualTy())){
      errormsg->Error((*arg)->pos_,"type mismatched");
    }
  }
  if(arg != args_->GetList().end()){
    errormsg->Error((*arg)->pos_,"too many params in function");
  }
  if(formal_ != funEntry->formals_->GetList().end()){
    errormsg->Error((*arg)->pos_,"too little params in function");
  }
  ////nullptr?
  return funEntry->result_->ActualTy();
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  std::cout<<" Begin Op Check!!"<<std::endl;
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
//  type::Ty * ty = tenv->Look(typ_);
//  std::cout<<"Record type : "<<typ_->Name()<<std::endl;
//  if(!ty){
//    errormsg->Error(pos_,"Function undeclared"+typ_->Name());
//    return type::NilTy::Instance();
//  }
//  if(!fields_){
//    errormsg->Error(pos_,"None field_");
//    return type::NilTy::Instance();
//  }
//  std::cout<<1<<std::endl;
//  auto field = fields_->GetList().begin();
//  std::cout<<2<<std::endl;
//  if(!((type::RecordTy *)ty)->fields_){
//    std::cout<<20<<std::endl;
//    errormsg->Error(pos_,"None type field_");
//    return type::NilTy::Instance();
//  }
//  auto record_list = ((type::RecordTy *)ty)->fields_->GetList();
//  std::cout<<3<<std::endl;
//  if(record_list.empty()){
//    std::cout<<"No record list. "<<std::endl;
//  }
//  auto record = record_list.begin();
//  std::cout<<"Get field "<<std::endl;
//  //The field names and types of the record expression must match
//  //    those of the named type, in the order given.
//  for(;field!=fields_->GetList().end()&&record!=record_list.end();field++,record++){
//    //check name
//    if((*field)->name_->Name()!= (*record)->name_->Name()){
//    errormsg->Error(pos_,"Record type unmatched");
//    }
//    else{
//      type::Ty *exp_type = (*field)->exp_->SemAnalyze(venv,tenv,labelcount,errormsg);
//      if(exp_type&&(*record)->ty_->ActualTy()&&typeid(exp_type->ActualTy())!=typeid((*record)->ty_->ActualTy())){
//        errormsg->Error(pos_, "field init expression is not matched to the type defined.");
//      };
//    }
//  }
//  return ty;
  type::Ty * record_type;
  type::Ty * type= nullptr;
  std::cout<<" Begin Record Check!!"<<std::endl;
  if (typ_ == nullptr) {
    record_type = nullptr;
  }
  else {
    record_type = tenv->Look(typ_);
  }
  type::RecordTy *real_record_type = dynamic_cast<type::RecordTy *> (record_type);
  if (real_record_type == nullptr) {
    errormsg->Error(pos_, "undefined type " + typ_->Name());
  }
  else {
    int total_error = 0;
    for (absyn::EField * eField: fields_->GetList()) {
      //                errormsg->Error(pos_, "in loop " + eField->name_->Name());
      std::cout<<eField->name_->Name()<<" Begin Check!!"<<std::endl;
      bool matched = false;
      type::Ty *field_type = nullptr;
      for (type::Field *field: real_record_type->fields_->GetList()) {
        if (eField->name_->Name() == field->name_->Name()) {
          matched = true;
          field_type = field->ty_;
          break;
        }
      }
      if (!matched) { // the field is not matched to the field listes
         errormsg->Error(pos_, "field is not in the record type.");
         break;
      }
      else { //if matched, then check the field type is matched to the field type

        type::Ty *exp_type = eField->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);

        if (field_type && exp_type && !field_type->IsSameType(exp_type)) {
          total_error++;
          errormsg->Error(
              pos_,
              "field init expression is not matched to the type defined.");
        }
      }
    }
    if (total_error == 0) {
      type = real_record_type;
      return type;
    }
  }
  type = real_record_type;
  return type;
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
  if(elsee_ == nullptr){
    if(body_type&&typeid(*(body_type->ActualTy()))!= typeid(type::VoidTy))
      errormsg->Error(pos_,"if then's body should have no value");
    return type::VoidTy::Instance();
  }
  else{
      if(typeid(*(body_type->ActualTy()))==
          typeid(*(elsee_->SemAnalyze(venv,tenv,labelcount,errormsg)))) {
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
    result = body_->SemAnalyze(venv,tenv,labelcount,errormsg);
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
    errormsg->Error(hi_->pos_,"range type is not int");
  }
  type::Ty *result;
  if(!body_)
    result = type::VoidTy::Instance();
  else
    result = body_->SemAnalyze(venv,tenv,labelcount,errormsg);
  venv->EndScope();
  tenv->EndScope();
  return result;
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  if(labelcount <= 0){
    errormsg->Error(pos_,"No loop to break.");
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
    type::Ty *array_type = tenv->Look(typ_);
    size_->SemAnalyze(venv,tenv,labelcount,errormsg);
    ////need check
    if(((type::ArrayTy*)array_type)->ty_&&
        typeid(init_->SemAnalyze(venv, tenv, labelcount, errormsg))!= typeid(((type::ArrayTy*)array_type)->ty_)){
      errormsg->Error(pos_,"type mismatched");
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
//  //找到第一个function
//  absyn::FunDec *funDec = functions_->GetList().front();
//  //参数
//  absyn::FieldList *params = funDec->params_;
//  //查找结果类型
//
//  type::Ty *result_ty = tenv->Look(funDec->result_);
//
//  type::TyList *formalTyList = params->MakeFormalTyList(tenv,errormsg);
//
//  venv->Enter(funDec->name_,new env::FunEntry(formalTyList,result_ty));
//  venv->BeginScope();
//  //formal_it:全局拥有的List;param_it:function拥有的变量类型list
//  auto formal_it = formalTyList->GetList().begin();
//  auto param_it = params->GetList().begin();
//  for(;param_it!=params->GetList().end();formal_it++,param_it++){
//    venv->Enter((*param_it)->name_,new env::VarEntry(*formal_it));
//  }
//  type::Ty *ty;
//  ty=funDec->body_->SemAnalyze(venv,tenv,labelcount,errormsg);
//  venv->EndScope();
  absyn::FunDec *last_function = nullptr;

  for (absyn::FunDec *function : functions_->GetList()) {
    if (last_function && function->name_->Name() == last_function->name_->Name()) {
      errormsg->Error(function->pos_, "two functions have the same name");
    }
    venv->Enter(function->name_, new env::FunEntry(nullptr, nullptr));
    last_function = function;
  }

  for (absyn::FunDec *function : functions_->GetList()) {

    type::Ty *result_ty;
    if (function->result_ == nullptr) result_ty = nullptr;
    else result_ty = tenv->Look(function->result_);
    type::TyList *formals = function->params_->MakeFormalTyList(tenv, errormsg);
    venv->Set(function->name_, new env::FunEntry(formals, result_ty));
    venv->BeginScope();
    auto formal_it = formals->GetList().begin();
    auto param_it = function->params_->GetList().begin();
    for (; param_it != function->params_->GetList().end(); formal_it++, param_it++) {
      venv->Enter((*param_it)->name_, new env::VarEntry(*formal_it));
    }
    type::Ty *ty = function->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
    //            errormsg->Error(pos_, "function body over");
    if (function->result_ == nullptr && ty && (typeid(*(ty->ActualTy())) != typeid(type::NilTy) && typeid(*(ty->ActualTy())) != typeid(type::VoidTy))) {
      errormsg->Error(pos_, "procedure returns value");
    }
    venv->EndScope();
  }
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
//  for var x : type_id := exp,
//                  it must check that type_id and type of exp are compatible
//                      if the type of exp is NilTy,
//                  type_id must be a RecordTy type
//                      var a : my_record := nil
  std::cout<<"VarDec check"<<std::endl;
  type::Ty *exp_ty = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typ_ == nullptr) {
    if (typeid(*(exp_ty->ActualTy())) == typeid(type::NilTy))
      errormsg->Error(pos_, "init should not be nil without type specified");
    venv->Enter(var_, new env::VarEntry(exp_ty->ActualTy()));
  }
  else{
    std::cout<<"with type VarDec check"<<std::endl;

    if(exp_ty== nullptr){
      errormsg->Error(pos_, "init type is nullptr");
      std::cout<<"exp_ty is nullptr."<<std::endl;
    }
    std::cout<<"with init_ty VarDec check"<<std::endl;
    if(typeid(*(exp_ty->ActualTy()))== typeid(type::NilTy)){
      std::cout<<"check Record Ty"<<std::endl;
      type::Ty *type_ty = tenv->Look(typ_);
      std::cout<<"Look tenv!"<<std::endl;
      if(type_ty&&typeid(*(type_ty->ActualTy()))!= typeid(type::RecordTy))
        errormsg->Error(pos_, "init should not be nil without type specified");
    }
    venv->Enter(var_,new env::VarEntry(exp_ty));
  }
  std::cout<<"Finish VarDec check"<<std::endl;

}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code her */
  auto type = types_->GetList();
  //先存name
  for (auto iter = type.begin();iter != type.end(); iter++) {
    auto next_iter = iter;
    if (next_iter != type.end()) next_iter++;
    for (; next_iter != type.end(); next_iter++)
      if ((*next_iter)->name_ == (*iter)->name_)
        errormsg->Error(pos_, "two types have the same name");
    //type存成nullptr
    tenv->Enter((*iter)->name_, new type::NameTy((*iter)->name_, nullptr));
  };

  for (auto iter = type.begin(); iter != type.end(); iter++) {
    //再将相应的env存进去
    type::Ty *ty = (*iter)->ty_->SemAnalyze(tenv, errormsg);
    tenv->Set((*iter)->name_,ty);
    std::cout<<(*iter)->name_->Name()<<" "<<typeid(*(ty->ActualTy())).name()<<std::endl;
  };
  bool cycle = false;
  for (auto iter = type.begin(); iter != type.end(); iter++) {
    if ((*iter)->name_ == nullptr) {
      std::cout << "Iter name is NULL!!!!" << std::endl;
      break;
    }
    type::Ty *ty = tenv->Look((*iter)->name_);
    if (ty == nullptr) {
      std::cout << "Type of " << (*iter)->name_->Name() << " is NULL!!!!"
                << std::endl;
      break;
    }
    std::cout<<"CHECK "<<(*iter)->name_->Name()<<"  "<<typeid(*ty).name()<<std::endl;

    if (typeid(*ty) == typeid(type::NameTy)) {
      type::Ty *ty_ty = ((type::NameTy *)ty)->ty_;
      while (ty_ty != nullptr &&
             typeid(*ty_ty) == typeid(type::NameTy)) {
        type::NameTy *name_ty = (type::NameTy *)ty_ty;
        std::cout << "Name1: " << name_ty->sym_->Name()
                  << " Name2: " << (*iter)->name_->Name() << std::endl;
        if (name_ty->sym_->Name() == (*iter)->name_->Name()) {
          errormsg->Error(pos_, "illegal type cycle");
          cycle = true;
          break;
        };
        ty_ty = name_ty->ty_;
      };
    };
    if (cycle)
      break;
  }
  std::cout << "Finish RETURN!!!!" << std::endl;
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
} // namespace sem
