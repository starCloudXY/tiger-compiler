#include "straightline/slp.h"

#include <iostream>

namespace A {
    int A::CompoundStm::MaxArgs() const {
        return std::max(stm1->MaxArgs(),stm2->MaxArgs());
    }

    Table *A::CompoundStm::Interp(Table *t) const {
        Table *tmp = stm1->Interp(t);
        return stm2->Interp(tmp);

    }

    int A::AssignStm::MaxArgs() const {
        return exp->MaxArgs();
    }

    Table *A::AssignStm::Interp(Table *t) const {
        IntAndTable *intAndTable = exp->Interp(t);
        return t->Update(id,intAndTable->i);
    }

    int A::PrintStm::MaxArgs() const {
        return exps->NumExps();
    }

    Table *A::PrintStm::Interp(Table *t) const {
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
