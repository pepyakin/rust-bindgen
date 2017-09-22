// bindgen-flags: --with-derive-partialeq --impl-partialeq

struct Opaque {
    int i;
    int large[33];
};

class Base {
    int large[33];
};

class ShouldDerivePartialEq: Base {
};
