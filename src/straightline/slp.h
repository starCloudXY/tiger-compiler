#ifndef STRAIGHTLINE_SLP_H_
#define STRAIGHTLINE_SLP_H_

#include <algorithm>
#include <cassert>
#include <string>
#include <list>

namespace A {

    class Stm;
    class Exp;
    class ExpList;

    enum BinOp { PLUS = 0, MINUS, TIMES, DIV };
    class Table {
    public:
        Table(std::string id, int value, const Table *tail)
                : id(std::move(id)), value(value), tail(tail) {}

        int Lookup(const std::string &key) const;
        Table *Update(const std::string &key, int val) const;

    private:
        std::string id;
        int value;
        const Table *tail;
    };


    struct IntAndTable {
        int i;
        Table *t;

        IntAndTable(int i, Table *t) : i(i), t(t) {}
    };


    class Stm {
    public:
        virtual int MaxArgs() const = 0;
        virtual Table *Interp(Table *) const = 0;
    };

    class CompoundStm : public Stm {
    public:
        CompoundStm(Stm *stm1, Stm *stm2) : stm1(stm1), stm2(stm2) {}
        int MaxArgs() const override;
        Table *Interp(Table *) const override;

    private:
        Stm *stm1, *stm2;
    };

    class AssignStm : public Stm {
    public:
        AssignStm(std::string id, Exp *exp) : id(std::move(id)), exp(exp) {}
        int MaxArgs() const override;
        Table *Interp(Table *) const override;

    private:
        std::string id;
        Exp *exp;
    };

    class PrintStm : public Stm {
    public:
        explicit PrintStm(ExpList *exps) : exps(exps) {}
        int MaxArgs() const override;
        Table *Interp(Table *) const override;

    private:
        ExpList *exps;
    };

    class Exp {
        // TODO: you'll have to add some definitions here (lab1).
        // Hints: You may add interfaces like `int MaxArgs()`,
        //        and ` IntAndTable *Interp(Table *)`
    public:
        virtual int MaxArgs() const = 0;
        virtual IntAndTable *Interp(Table *) const =0;
    };

    class IdExp : public Exp {
    public:
        explicit IdExp(std::string id) : id(std::move(id)) {}
        int MaxArgs() const override{
            return 0;
        };
        IntAndTable *Interp(Table *table) const override{
            return new  IntAndTable(table->Lookup(id), table);
        };

    private:
        std::string id;
    };

    class NumExp : public Exp {
    public:
        explicit NumExp(int num) : num(num) {}
        int MaxArgs() const override{
            return 0;
        } ;
        IntAndTable *Interp(Table * table) const override{
            return new IntAndTable(num,table);
        };
    private:
        int num;
    };

    class OpExp : public Exp {
    public:
        OpExp(Exp *left, BinOp oper, Exp *right)
                : left(left), oper(oper), right(right) {}
        int MaxArgs() const override{
            return std::max(left->MaxArgs(),right->MaxArgs());
        } ;
        IntAndTable *Interp(Table *table) const override{
            IntAndTable *left_table = left->Interp(table);
            IntAndTable *right_table = right->Interp(left_table->t);
            switch (oper) {
                case PLUS:
                    return new IntAndTable(left_table->i+right_table->i,right_table->t);
                case MINUS:
                    return new IntAndTable(left_table->i-right_table->i,right_table->t);
                case DIV:
                    return new IntAndTable(left_table->i/right_table->i,right_table->t);
                case TIMES:
                    return new IntAndTable(left_table->i*right_table->i,right_table->t);
                default:
                    return nullptr;
            }
        };

    private:
        Exp *left;
        BinOp oper;
        Exp *right;
    };

    class EseqExp : public Exp {
    public:
        EseqExp(Stm *stm, Exp *exp) : stm(stm), exp(exp) {}
        int MaxArgs() const override{
            return std::max(stm->MaxArgs(),exp->MaxArgs());
        } ;
        IntAndTable *Interp(Table *table) const override{
            Table * new_table = stm->Interp(table);
            return exp->Interp(new_table);
        };
    private:
        Stm *stm;
        Exp *exp;
    };

    class ExpList {
    public:
        // TODO: you'll have to add some definitions here (lab1).
        // Hints: You may add interfaces like `int MaxArgs()`, `int NumExps()`,
        //        and ` IntAndTable *Interp(Table *)`
    public:
        virtual int MaxArgs() const = 0;
        virtual IntAndTable *Interp(Table *) const = 0;
        virtual int NumExps() const = 0;
    };

    class PairExpList : public ExpList {
    public:
        PairExpList(Exp *exp, ExpList *tail) : exp(exp), tail(tail) {}
        // TODO: you'll have to add some definitions here (lab1).
        int MaxArgs() const override{
            return std::max(exp->MaxArgs(),tail->MaxArgs());
        }
        int NumExps() const override{
            return 1+tail->NumExps();
        }

        IntAndTable *Interp(Table *t) const {
            IntAndTable *first_int_and_table = exp->Interp(t);
            printf("%d ", first_int_and_table->i);
            IntAndTable *last_int_and_table = tail->Interp(first_int_and_table->t);
            return last_int_and_table;
        }
    private:
        Exp *exp;
        ExpList *tail;
    };
//printstm的最后一个参数
    class LastExpList : public ExpList {
    public:
        LastExpList(Exp *exp) : exp(exp) {}
        int MaxArgs() const override{
            return exp->MaxArgs();
        };
        IntAndTable *Interp(Table *table) const override{
            IntAndTable *intAndTable=exp->Interp(table);
            printf("%d\n",intAndTable->i);
            return intAndTable;
        };
        int NumExps() const override{
            return 1;
        } ;
    private:
        Exp *exp;
    };



}  // namespace A

#endif  // STRAIGHTLINE_SLP_H_
