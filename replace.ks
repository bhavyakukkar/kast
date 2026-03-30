@syntax "index_array" 70 @wrap never = <- array "." "[" index:any "]";
@syntax "core:unquote" 1000 @wrap never = "$" _ ->;