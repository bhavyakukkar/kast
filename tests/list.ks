let mut t = ArrayList.new();

&mut t |> ArrayList.push_back(8 :: UInt32);

ArrayList.length(&t) |> dbg.print;

&mut t |> ArrayList.pop_back |> dbg.print;

# going 2 panic now
&mut t |> ArrayList.pop_back;
