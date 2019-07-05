module Css : sig 
    val css_string: string
    val x_cl: string
    val y_id: string
end = [%css {| 
    .x {
        background-color: red;
        color: black;
    } 

    #y .x {

    }
|}];;

print_endline Css.css_string;;
print_endline Css.x_cl;;
print_endline Css.y_id;;
