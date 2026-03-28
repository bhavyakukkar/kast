const C = @context newtype { .foo :: Int32 };

let mut s = "hello, world";
let &mut s = &mut s;