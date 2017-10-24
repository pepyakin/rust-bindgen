/* automatically generated by rust-bindgen */


#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]


#[allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]
pub mod root {
    #[allow(unused_imports)]
    use self::super::root;
    pub mod foo {
        #[allow(unused_imports)]
        use self::super::super::root;
        #[repr(C)]
        #[derive(Debug, Default, Copy, Clone)]
        pub struct Bar {
            pub foo: ::std::os::raw::c_int,
            pub baz: bool,
        }
        #[test]
        fn bindgen_test_layout_Bar() {
            assert_eq!(
                ::std::mem::size_of::<Bar>(),
                8usize,
                concat!("Size of: ", stringify!(Bar))
            );
            assert_eq!(
                ::std::mem::align_of::<Bar>(),
                4usize,
                concat!("Alignment of ", stringify!(Bar))
            );
            assert_eq!(
                unsafe { &(*(0 as *const Bar)).foo as *const _ as usize },
                0usize,
                concat!(
                    "Alignment of field: ",
                    stringify!(Bar),
                    "::",
                    stringify!(foo)
                )
            );
            assert_eq!(
                unsafe { &(*(0 as *const Bar)).baz as *const _ as usize },
                4usize,
                concat!(
                    "Alignment of field: ",
                    stringify!(Bar),
                    "::",
                    stringify!(baz)
                )
            );
        }
    }
    pub mod bar {
        #[allow(unused_imports)]
        use self::super::super::root;
        #[repr(C)]
        #[derive(Debug, Copy, Clone)]
        pub struct Foo {
            pub ptr: *mut root::foo::Bar,
        }
        #[test]
        fn bindgen_test_layout_Foo() {
            assert_eq!(
                ::std::mem::size_of::<Foo>(),
                8usize,
                concat!("Size of: ", stringify!(Foo))
            );
            assert_eq!(
                ::std::mem::align_of::<Foo>(),
                8usize,
                concat!("Alignment of ", stringify!(Foo))
            );
            assert_eq!(
                unsafe { &(*(0 as *const Foo)).ptr as *const _ as usize },
                0usize,
                concat!(
                    "Alignment of field: ",
                    stringify!(Foo),
                    "::",
                    stringify!(ptr)
                )
            );
        }
        impl Default for Foo {
            fn default() -> Self {
                unsafe { ::std::mem::zeroed() }
            }
        }
    }
}
