// bindgen-flags: --rust-target 1.0 --with-derive-partialeq --impl-partialeq

// This should derive PartialEq.
union ShouldDerivePartialEq {
    char a[150];
    int b;
};
