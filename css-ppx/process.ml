open! Core_kernel

type result = {
    css_string : string;
    mapping: string String.Table.t
}

let transform s = 
    let hash = String.hash s in
    let mapping = String.Table.create () in 
    let css_string = Css_stuff.rewrite_identifiers s ~f:(function 
        | Class c ->
          let ret = sprintf "%s_class_%d" c hash in 
          let _ = String.Table.add mapping ~key:(sprintf "%s_class" c) ~data:ret in
          ret
        | Id id -> 
          let ret = sprintf "%s_id_%d" id hash in 
          let _ = String.Table.add mapping ~key:(sprintf "%s_id" id) ~data:ret in
          ret) in
    {css_string; mapping}
