open! Core_kernel
open! Ppxlib

let f ~loc ~path:_ (expr: expression) = 
 match expr.pexp_desc with 
 | Pexp_constant (Pconst_string (l, l2)) -> 
   let reversed = {expr with pexp_desc = Pexp_constant( Pconst_string(String.rev l, l2))} in 
   [%expr [%e reversed]]
 | _ -> Location.raise_errorf ~loc
      "%%css must take a single string as input"


let extension = 
  Extension.declare 
    "css"
    (Extension.Context.expression)
    Ast_pattern.(map (single_expr_payload __) ~f:(fun f x -> f x))
    f 

let () = Driver.register_transformation "css" ~extensions:[ extension ]
