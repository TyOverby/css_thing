open! Core_kernel
open! Ppxlib

let f ~loc ~path:_ (expr: expression) = 
 match expr.pexp_desc with 
 | Pexp_constant (Pconst_string (l, l2)) -> 
   let Process.{ css_string ; mapping } = Process.transform l in
   let reversed = {expr with pexp_desc = Pexp_constant( Pconst_string(String.rev l, l2))} in 
   let ret = [%str let y = 5;; let x = [%e reversed]] in 
   (*Pprintast.structure_item (Format.err_formatter) ret;
   ignore (failwith "fail");*)
   {pmod_desc= Pmod_structure ret; pmod_loc=loc; pmod_attributes=[] }
 | _ -> Location.raise_errorf ~loc
      "%%css must take a single string as input"


let extension = 
  Extension.declare 
    "css"
    (Extension.Context.module_expr)
    Ast_pattern.(single_expr_payload __)
    f 

let () = Driver.register_transformation "css" ~extensions:[ extension ]
