#ifndef ENVIRONMENT_H
#define ENVIRONMENT_H

#include "llvm/ADT/StringMap.h"

#include "Tree.h"

namespace mxrlang {

// Models the scope.
template <typename T> class Environment {
  llvm::StringMap<T *> values;

  // Parent scope in the scope chain.
  Environment *parent = nullptr;

public:
  Environment(Environment *parent) : parent(parent) {}

  // Insert a value in the current scope.
  bool insert(T *value, llvm::StringRef name) {
    auto v = values.find(name);
    if (v != values.end())
      return false;

    values[name] = value;
    return true;
  }

  // Find the value in the current scope. If it doesn't exist,
  // search the parent scopes.
  T *find(llvm::StringRef name) {
    auto v = values.find(name);
    if (v != values.end())
      return v->second;

    if (!parent)
      return nullptr;

    return parent->find(name);
  }

  Environment *getParent() { return parent; }
};

} // namespace mxrlang

#endif // ENVIRONMENT_H
