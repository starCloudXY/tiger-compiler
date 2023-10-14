#include "straightline/slp.h"

#include <iostream>

namespace A {
    int A::CompoundStm::MaxArgs() const {
        // TODO: put your code here (lab1).
        return std::max(stm1->MaxArgs(),stm2->MaxArgs());
    }

    Table *A::CompoundStm::Interp(Table *t) const {
        // TODO: put your code here (lab1).
        Table *tmp = stm1->Interp(t);
        return stm2->Interp(tmp);
    }

    int A::AssignStm::MaxArgs() const {
        // TODO: put your code here (lab1).
        return exp->MaxArgs();
    }

    Table *A::AssignStm::Interp(Table *t) const {
        // TODO: put your code here (lab1).
        IntAndTable *intAndTable = exp->Interp(t);
        return t->Update(id,intAndTable->i);
    }

    int A::PrintStm::MaxArgs() const {
        // TODO: put your code here (lab1).
        return exps->NumExps();
    }

    Table *A::PrintStm::Interp(Table *t) const {
        // TODO: put your code here (lab1).
        return (exps->Interp(t))->t;
    }


    int Table::Lookup(const std::string &key) const {
        if (id == key) {
            return value;
        } else if (tail != nullptr) {
            return tail->Lookup(key);
        } else {
            assert(false);
        }
    }

    Table *Table::Update(const std::string &key, int val) const {
        return new Table(key, val, this);
    }
}  // namespace A
