// bindgen-flags: --with-derive-partialeq --impl-partialeq --opaque-type "Opaque"

struct Opaque {
    int i;
    int large[33];
};

class Base {
    int large[33];
};

class ShouldDerivePartialEq: Base {
};

// This class and all it's subclasses can't derive PartialEq because
// it contains opaque member.
class BaseThatCantDerivePartialEq {
    int large[33];
    Opaque a;
};

class ShouldNotDerivePartialEq: Base, BaseThatCantDerivePartialEq {
};
