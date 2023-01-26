use core::iter::Step;

pub trait Iter {
    type Item;

    extern "C" fn next(&mut self) -> Self::Item;
}

macro_rules! range_from_type {
    ($t:ty) => {
        /// An iterator over a given range of values
        #[no_mangle]
        #[repr(C)]
        pub struct Range$t($t, $t);

        impl Iter for Range {
            type Item = $t;

            #[no_mangle]
            extern "C" fn next(&mut self) -> Self::Item {
                self.0 += 1;
                self.0 - 1
            }
        }
    };
    ($($t:ty, )+) => {
        //
    };
}

range_from_type!(i32);
range_from_type!(i64);

// impl<T: Step + Copy> Iter for Range<T> {
//     type Item = T;

//     #[no_mangle]
//     extern "C" fn next(&mut self) -> Self::Item {
//         <T as Step>::forward(self.0, 1)
//     }
// }
