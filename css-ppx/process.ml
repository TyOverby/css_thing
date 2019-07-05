open! Core_kernel

type result = {
    css_string : string;
    mapping: string String.Table.t
}

let transform s = 
    let hash = 
        Md5.digest_string s 
        |> Md5.to_hex 
        |> Fn.flip String.prefix 6 in 
    let mapping = String.Table.create () in 
    let css_string = Css_stuff.rewrite_identifiers s ~f:(function 
        | Class c ->
          let class_name = String.drop_prefix c 1  in 
          let ret = sprintf "%s_class_%s" c hash in 
          let _ = String.Table.add mapping ~key:(sprintf "%s_cl" class_name) ~data:ret in
          ret
        | Id id -> 
          let ret = sprintf "%s_id_%s" id hash in 
          let _ = String.Table.add mapping ~key:(sprintf "%s_id" id) ~data:ret in
          ret) in
    {css_string; mapping}
