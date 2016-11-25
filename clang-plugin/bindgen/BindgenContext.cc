#include "BindgenContext.h"
#include "BindgenItem.h"
#include "BindgenType.h"

#include "clang/AST/AST.h"

#include <cassert>
#include <memory>

namespace bindgen {

enum class ReservedId : ItemId {
  Root = 0,
  Void,
  NullPtr,

  Bool,
  Char,
  UChar,
  Short,
  UShort,
  Int,
  UInt,
  Long,
  ULong,
  LongLong,
  ULongLong,
  I8,
  U8,
  I16,
  U16,
  I32,
  U32,
  I64,
  U64,
  I128,
  U128,

  Float,
  Float128,
  Double,
  LongDouble,
  Last,
};

inline static ItemId AsId(ReservedId id) {
  return static_cast<ItemId>(id);
}

const ItemId ROOT_MODULE = 0;

BindgenContext::BindgenContext(clang::ASTContext& ctx,
                               clang::CompilerInstance& ci)
  : m_compilerInstance(ci)
  , m_context(ctx)
  , m_lastItemId(AsId(ReservedId::Last)) {
  registerBuiltins();
}

void BindgenContext::addItem(std::unique_ptr<Item> item) {
  assert(item);
  assert(m_items.find(item->id()) == m_items.end());
  m_items.insert(std::make_pair(item->id(), std::move(item)));
}

void BindgenContext::registerBuiltins() {
#define BUILTIN_INTEGER(name_, kind_)                                          \
  m_items.insert(std::make_pair(                                               \
      AsId(ReservedId::kind_),                                                 \
      new IntegerType(AsId(ReservedId::kind_), AsId(ReservedId::Root), "",     \
                      #name_, IntKind::kind_)))

  BUILTIN_INTEGER(bool, Bool);
  BUILTIN_INTEGER(char, Char);
  BUILTIN_INTEGER(unsigned char, UChar);
  BUILTIN_INTEGER(short, Short);
  BUILTIN_INTEGER(unsigned short, UShort);
  BUILTIN_INTEGER(int, Int);
  BUILTIN_INTEGER(unsigned int, UInt);
  BUILTIN_INTEGER(long, Long);
  BUILTIN_INTEGER(unsigned long, ULong);
  BUILTIN_INTEGER(long long, LongLong);
  BUILTIN_INTEGER(unsigned long long, ULongLong);
  // TODO: Fill remaining reserved IDs!
}

ItemId BindgenContext::registerType(const clang::Type& type) {
  assert(m_registeredTypes.find(&type) == m_registeredTypes.end());
  auto id = nextItemId();
  m_registeredTypes.insert({&type, id});
  return id;
}

Item& BindgenContext::getItem(ItemId id) {
  auto it = m_items.find(id);
  assert(it != m_items.end());
  return *it->second;
}

bool BindgenContext::getRegisteredType(const clang::Type& ty,
                                       ItemId& out) const {
  auto it = m_registeredTypes.find(&ty);
  if (it == m_registeredTypes.end())
    return false;
  out = it->second;
  return true;
}

ItemId BindgenContext::maybeBuildWrapperForQualTy(ItemId wrapping,
                                                  clang::QualType ty) {
  Type* wrappingTy = getItem(wrapping).asType();
  assert(wrappingTy);

  if (ty.isConstQualified()) {
    ItemId id = nextItemId();
    addItem(std::make_unique<QualifiedType>(id, *wrappingTy, true));
    return id;
  }

  return wrapping;
}

bool BindgenContext::getBuiltinOrRegisteredTy(const clang::Type& type,
                                              ItemId& out) const {
  if (getRegisteredType(type, out))
    return true;

#define BUILTIN_CASE2(kind_, name_)                                            \
  case clang::BuiltinType::kind_:                                              \
    out = AsId(ReservedId::name_);                                             \
    return true
#define BUILTIN_CASE(kind_) BUILTIN_CASE2(kind_, kind_)

  switch (type.getTypeClass()) {
    case clang::Type::Builtin: {
      const clang::BuiltinType& builtin = llvm::cast<clang::BuiltinType>(type);
      switch (builtin.getKind()) {
        BUILTIN_CASE(Void);
        BUILTIN_CASE(NullPtr);

        BUILTIN_CASE(Bool);
        BUILTIN_CASE2(Char_U, Char);
        BUILTIN_CASE2(Char_S, Char);
        BUILTIN_CASE2(SChar, Char);
        BUILTIN_CASE(UChar);
        BUILTIN_CASE2(Char16, U16);
        BUILTIN_CASE2(Char32, U32);
        BUILTIN_CASE(UShort);
        BUILTIN_CASE(Int);
        BUILTIN_CASE(Long);
        BUILTIN_CASE(LongLong);
        BUILTIN_CASE(UInt);
        BUILTIN_CASE(ULong);
        BUILTIN_CASE(ULongLong);
        BUILTIN_CASE2(UInt128, U128);
        BUILTIN_CASE2(Int128, I128);

        BUILTIN_CASE2(WChar_S, U16);
        BUILTIN_CASE2(WChar_U, U16);

        BUILTIN_CASE(Float);
        BUILTIN_CASE(Float128);
        BUILTIN_CASE(Double);
        BUILTIN_CASE(LongDouble);

        default:
          llvm::errs() << "unhandled builtin " << builtin.getKind();
          return false;
      }
    }
    default:
      llvm::errs() << "unhandled type class " << type.getTypeClass();
      return false;
  }
}

}  // namespace bindgen
