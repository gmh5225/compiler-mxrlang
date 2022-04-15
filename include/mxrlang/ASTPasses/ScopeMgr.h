#ifndef SCOPEMGR_H
#define SCOPEMGR_H

#include <memory>

#include "Environment.h"

namespace mxrlang {

// Manages the creation and destruction of environment scopes during
// an AST pass.
template <typename PassTy, typename T>
class ScopeMgr {
    // AST pass which needs environment management.
    PassTy& pass;
    // Environment being managed.
    std::shared_ptr<Environment<T>> env;

public:
    ScopeMgr(PassTy& pass) : pass(pass) {
        env = std::make_shared<Environment<T>>(pass.env);
        pass.env = env.get();
    }

    ~ScopeMgr() {
        pass.env = pass.env->getParent();
    }
};

} // namespace mxrlang

#endif // SCOPEMGR_H
