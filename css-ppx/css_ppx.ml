open! Core_kernel
open! Ppxlib

let string_constant ~loc l = 
    {
        pexp_desc = Pexp_constant( Pconst_string(l, None)); 
        pexp_loc= loc;
        pexp_attributes=[];
    }
;;

let pat_constant ~loc txt = 
    { ppat_desc = Ppat_var ({ txt; loc });
      ppat_loc = loc;
      ppat_attributes = [];
    }


        

let f ~loc ~path:_ (expr: expression) = 
 match expr.pexp_desc with 
 | Pexp_constant (Pconst_string (l, _l2)) -> 
   let Process.{ css_string ; mapping } = Process.transform l in
   let css = [%stri let css_string = [%e string_constant ~loc css_string ]] in 

   let more_items = List.map (String.Table.to_alist mapping) ~f:(fun (k, v) -> 
       [%stri let [%p pat_constant ~loc k] = [%e string_constant ~loc v]]
   ) 
       in
   (*Pprintast.structure_item (Format.err_formatter) ret;
   ignore (failwith "fail");*)
   {pmod_desc= Pmod_structure (css :: more_items); pmod_loc=loc; pmod_attributes=[] }
 | _ -> Location.raise_errorf ~loc
      "%%css must take a single string as input"


let extension = 
  Extension.declare 
    "css"
    (Extension.Context.module_expr)
    Ast_pattern.(single_expr_payload __)
    f 

let () = Driver.register_transformation "css" ~extensions:[ extension ]
