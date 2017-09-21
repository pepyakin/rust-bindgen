// bindgen-flags: --with-derive-partialeq --impl-partialeq

template <typename T>
struct foo {
    int large[33];
    T data;
};

struct IntStr {
    foo<int> a;
};
