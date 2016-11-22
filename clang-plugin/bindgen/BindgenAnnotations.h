#pragma once

namespace clang {
class Decl;
};

namespace bindgen {

class BindgenAnnotations {
  // TODO;
public:
  static BindgenAnnotations parse(const clang::Decl&);
};

}  // namespace bindgen
