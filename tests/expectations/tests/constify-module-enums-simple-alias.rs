/* automatically generated by rust-bindgen */


#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]


pub mod Foo {
    pub type Type = ::std::os::raw::c_int;
    pub const Variant1: Type = 0;
    pub const Variant2: Type = 1;
    pub const Variant3: Type = 2;
}
pub use self::Foo::Type as Foo_alias1;
pub use self::Foo_alias1 as Foo_alias2;
pub use self::Foo_alias2 as Foo_alias3;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Bar {
    pub baz1: Foo::Type,
    pub baz2: Foo_alias1,
    pub baz3: Foo_alias2,
    pub baz4: Foo_alias3,
    pub baz_ptr1: *mut Foo::Type,
    pub baz_ptr2: *mut Foo_alias1,
    pub baz_ptr3: *mut Foo_alias2,
    pub baz_ptr4: *mut Foo_alias3,
}
#[test]
fn bindgen_test_layout_Bar() {
    assert_eq!(
        ::std::mem::size_of::<Bar>(),
        48usize,
        concat!("Size of: ", stringify!(Bar))
    );
    assert_eq!(
        ::std::mem::align_of::<Bar>(),
        8usize,
        concat!("Alignment of ", stringify!(Bar))
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz1 as *const _ as usize },
        0usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz1)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz2 as *const _ as usize },
        4usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz2)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz3 as *const _ as usize },
        8usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz3)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz4 as *const _ as usize },
        12usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz4)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz_ptr1 as *const _ as usize },
        16usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz_ptr1)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz_ptr2 as *const _ as usize },
        24usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz_ptr2)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz_ptr3 as *const _ as usize },
        32usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz_ptr3)
        )
    );
    assert_eq!(
        unsafe { &(*(0 as *const Bar)).baz_ptr4 as *const _ as usize },
        40usize,
        concat!(
            "Alignment of field: ",
            stringify!(Bar),
            "::",
            stringify!(baz_ptr4)
        )
    );
}
impl Default for Bar {
    fn default() -> Self {
        unsafe { ::std::mem::zeroed() }
    }
}
