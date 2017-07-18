/* automatically generated by rust-bindgen */


#![allow(dead_code, non_snake_case, non_camel_case_types, non_upper_case_globals)]


pub type Array16 = u8;
pub type ArrayInt4 = [u32; 4usize];
#[repr(C)]
pub struct UsesArray {
    pub array_char_16: [u8; 16usize],
    pub array_bool_8: [u8; 8usize],
    pub array_int_4: ArrayInt4,
}
#[test]
fn bindgen_test_layout_UsesArray() {
    assert_eq!(::std::mem::size_of::<UsesArray>() , 40usize , concat ! (
               "Size of: " , stringify ! ( UsesArray ) ));
    assert_eq! (::std::mem::align_of::<UsesArray>() , 4usize , concat ! (
                "Alignment of " , stringify ! ( UsesArray ) ));
    assert_eq! (unsafe {
                & ( * ( 0 as * const UsesArray ) ) . array_char_16 as * const
                _ as usize } , 0usize , concat ! (
                "Alignment of field: " , stringify ! ( UsesArray ) , "::" ,
                stringify ! ( array_char_16 ) ));
    assert_eq! (unsafe {
                & ( * ( 0 as * const UsesArray ) ) . array_bool_8 as * const _
                as usize } , 16usize , concat ! (
                "Alignment of field: " , stringify ! ( UsesArray ) , "::" ,
                stringify ! ( array_bool_8 ) ));
    assert_eq! (unsafe {
                & ( * ( 0 as * const UsesArray ) ) . array_int_4 as * const _
                as usize } , 24usize , concat ! (
                "Alignment of field: " , stringify ! ( UsesArray ) , "::" ,
                stringify ! ( array_int_4 ) ));
}
impl Default for UsesArray {
    fn default() -> Self { unsafe { ::std::mem::zeroed() } }
}
#[test]
fn __bindgen_test_layout_Array_instantiation() {
    assert_eq!(::std::mem::size_of::<[u32; 4usize]>() , 16usize , concat ! (
               "Size of template specialization: " , stringify ! (
               [u32; 4usize] ) ));
    assert_eq!(::std::mem::align_of::<[u32; 4usize]>() , 4usize , concat ! (
               "Alignment of template specialization: " , stringify ! (
               [u32; 4usize] ) ));
}
#[test]
fn __bindgen_test_layout_Array_instantiation_1() {
    assert_eq!(::std::mem::size_of::<[u8; 16usize]>() , 16usize , concat ! (
               "Size of template specialization: " , stringify ! (
               [u8; 16usize] ) ));
    assert_eq!(::std::mem::align_of::<[u8; 16usize]>() , 1usize , concat ! (
               "Alignment of template specialization: " , stringify ! (
               [u8; 16usize] ) ));
}
#[test]
fn __bindgen_test_layout_Array_instantiation_2() {
    assert_eq!(::std::mem::size_of::<[u8; 8usize]>() , 8usize , concat ! (
               "Size of template specialization: " , stringify ! (
               [u8; 8usize] ) ));
    assert_eq!(::std::mem::align_of::<[u8; 8usize]>() , 1usize , concat ! (
               "Alignment of template specialization: " , stringify ! (
               [u8; 8usize] ) ));
}