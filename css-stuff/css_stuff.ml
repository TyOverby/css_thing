open! Core_kernel
open Css_lib.Types

type identifier_kind =
  | Class of string
  | Id of string
[@@deriving sexp]

let map_component_value ~f = function
  | Ident ident when String.is_prefix ident ~prefix:"." -> Ident (f (Class ident))
  | Hash hash -> Hash (f (Id hash))
  | other -> other
;;

let rec map_selector ~f = function
  | Simple list -> Simple (List.map list ~f:(map_component_value ~f))
  | Compound list -> Compound (List.map list ~f:(map_component_value ~f))
  | Complex list -> Complex (List.map list ~f:(map_selector ~f))
;;

let map_at_rule (at_rule : atrule) ~f =
  let prelude = at_rule.prelude in
  let prelude = List.map prelude ~f:(List.map ~f:(map_component_value ~f)) in
  { at_rule with prelude }
;;

let map_style_rule (rule : styleRule) ~f =
  let prelude = rule.prelude in
  let prelude = List.map prelude ~f:(map_selector ~f) in
  { rule with prelude }
;;

let map_rule ~f = function
  | StyleRule style_rule -> StyleRule (map_style_rule style_rule ~f)
  | AtRule at_rule -> AtRule (map_at_rule at_rule ~f)
  | other -> other
;;

let map_stylesheet stylesheet ~f = List.map stylesheet ~f:(map_rule ~f)

let extract_identifiers css_string =
  let stylesheet = Css_lib.Index.parse css_string in
  let idents = ref [] in
  let _ =
    map_stylesheet stylesheet ~f:(fun ident ->
        idents := ident :: !idents;
        match ident with
        | Class c -> c
        | Id h -> h)
  in
  List.rev !idents
;;

let rewrite_identifiers css_string ~f =
  let stylesheet = Css_lib.Index.parse css_string in
  let stylesheet = map_stylesheet stylesheet ~f in
  Css_lib.Index.print stylesheet
;;

let test s =
  let identifiers = extract_identifiers s in
  let rewritten =
    rewrite_identifiers s ~f:(function
        | Class s -> s ^ "-class-rewritten"
        | Id s -> s ^ "-id-rewritten")
  in
  print_s [%message (identifiers : identifier_kind list)];
  print_endline rewritten
;;

let%expect_test _ =
    test "a {} b {}";
  [%expect {|
      (identifiers ())
      a {

      }

      b {

      }|}]
;;

let%expect_test _ =
  test "a {}";
  [%expect {|
    (identifiers ())
    a {

    }|}]
;;

let%expect_test _ =
  test ".a {}";
  [%expect
    {|
      (identifiers ((Class .a)))
      .a-class-rewritten {

      }|}]
;;

let%expect_test _ =
  test ".a > .b {}";
  [%expect
        {|
    (identifiers ((Class .a) (Class .b)))
    .a-class-rewritten>.b-class-rewritten {

    }|}]
;;

let%expect_test _ =
  test ".a .b {}";
  [%expect
    {|
    (identifiers ((Class .a) (Class .b)))
    .a-class-rewritten .b-class-rewritten {

    }|}]
;;

let%expect_test _ =
  test ".a:hover {}";
  [%expect
    {|
    (identifiers ((Class .a)))
    .a-class-rewritten:hover {

    }|}]
;;

let%expect_test _ =
  test ".a .b > .c {}";
  [%expect
    {|
    (identifiers ((Class .a) (Class .b) (Class .c)))
    .a-class-rewritten .b-class-rewritten>.c-class-rewritten {

    }|}]
;;

let%expect_test _ =
  test "#a {}";
  [%expect {|
  (identifiers ((Id a)))
  #a-id-rewritten {

  }|}]
;;

let%expect_test _ =
  test "#a::first-letter {}";
  [%expect
    {|
    (identifiers ((Id a)))
    #a-id-rewritten::first-letter {

    }|}]
;;

let%expect_test _ =
  test {| #a[href^="http"] {} |};
  [%expect
    {|
    (identifiers ((Id a)))
    #a-id-rewritten[href^="http"] {

    }|}]
;;

let%expect_test _ =
  test "#a > #b {}";
  [%expect
    {|
    (identifiers ((Id a) (Id b)))
    #a-id-rewritten>#b-id-rewritten {

    }|}]
;;

let%expect_test _ =
  test "#a #b {}";
  [%expect
    {|
    (identifiers ((Id a) (Id b)))
    #a-id-rewritten #b-id-rewritten {

    }|}]
;;

let%expect_test _ =
  test "#a ~ #b {}";
  [%expect
    {|
    (identifiers ((Id a) (Id b)))
    #a-id-rewritten~#b-id-rewritten {

    }|}]
;;

let%expect_test "gmail" = 
    test {|
    .gb_Ue{background:rgba(60,64,67,0.90);-moz-border-radius:4px;border-radius:4px;color:#ffffff;font:500 12px 'Roboto',arial,sans-serif;letter-spacing:.8px;line-height:16px;margin-top:4px;min-height:14px;padding:4px 8px;position:absolute;z-index:1000;-moz-osx-font-smoothing:grayscale}.gb_b.gb_b{background-size:64px 64px}#gb2 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/3a1e625196.png')}.gb_c #gb2 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/3a1e625196.png')}#gb22 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/3daf4c1f88.png')}.gb_c #gb22 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/3daf4c1f88.png')}#gb45 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/f420d06f66.png')}.gb_c #gb45 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/f420d06f66.png')}#gb72 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/28a40ba7cc.png')}.gb_c #gb72 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/28a40ba7cc.png')}#gb117 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/142da27578.png')}.gb_c #gb117 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/142da27578.png')}#gb136 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/911e3628e6.png')}.gb_c #gb136 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/911e3628e6.png')}#gb166 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/41679a9ec5.png')}.gb_c #gb166 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/41679a9ec5.png')}#gb171 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/4244245d7e.png')}.gb_c #gb171 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/4244245d7e.png')}#gb177 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/4653513b7d.png')}.gb_c #gb177 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/4653513b7d.png')}#gb206 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/ad330d8459.png')}.gb_c #gb206 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/ad330d8459.png')}#gb207 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/2c21041e16.png')}.gb_c #gb207 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/2c21041e16.png')}#gb211 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/c03dda0b34.png')}.gb_c #gb211 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/c03dda0b34.png')}#gb217 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/71060be5b3.png')}.gb_c #gb217 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/71060be5b3.png')}#gb228 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/74aa55e0c2.png')}.gb_c #gb228 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/74aa55e0c2.png')}#gb249 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/afa40f6e42.png')}.gb_c #gb249 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/afa40f6e42.png')}#gb260 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/ea554714e7.png')}.gb_c #gb260 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/ea554714e7.png')}#gb261 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/0b26f6f8e4.png')}.gb_c #gb261 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/0b26f6f8e4.png')}#gb108 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/dfbeb24785.png')}.gb_c #gb108 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/dfbeb24785.png')}#gb60 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/85bb99a341.png')}.gb_c #gb60 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/85bb99a341.png')}#gb175 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/eacd033c28.png')}.gb_c #gb175 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/eacd033c28.png')}@media (min-resolution:1.25dppx),(-webkit-min-device-pixel-ratio:1.25),(min-device-pixel-ratio:1.25){#gb2 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/438087d3df.png')}.gb_c #gb2 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/438087d3df.png')}#gb22 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/cfa67efcd3.png')}.gb_c #gb22 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/cfa67efcd3.png')}#gb45 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/9c561d4392.png')}.gb_c #gb45 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/9c561d4392.png')}#gb72 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/cfa4e2be67.png')}.gb_c #gb72 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/cfa4e2be67.png')}#gb117 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/e3cbb9b858.png')}.gb_c #gb117 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/e3cbb9b858.png')}#gb136 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/17bdcddea9.png')}.gb_c #gb136 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/17bdcddea9.png')}#gb166 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/be3fe52205.png')}.gb_c #gb166 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/be3fe52205.png')}#gb171 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/1b217ae532.png')}.gb_c #gb171 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/1b217ae532.png')}#gb177 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/188f0d697b.png')}.gb_c #gb177 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/188f0d697b.png')}#gb206 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/20808fb750.png')}.gb_c #gb206 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/20808fb750.png')}#gb207 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/6d9eaee7f9.png')}.gb_c #gb207 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/6d9eaee7f9.png')}#gb211 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/2d7fffa981.png')}.gb_c #gb211 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/2d7fffa981.png')}#gb217 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/e2c0b463b4.png')}.gb_c #gb217 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/e2c0b463b4.png')}#gb228 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/fe8c881457.png')}.gb_c #gb228 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/fe8c881457.png')}#gb249 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/d54db42004.png')}.gb_c #gb249 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/d54db42004.png')}#gb260 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/99be7c5086.png')}.gb_c #gb260 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/99be7c5086.png')}#gb261 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/9001dae971.png')}.gb_c #gb261 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/9001dae971.png')}#gb108 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/ca7b209615.png')}.gb_c #gb108 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/ca7b209615.png')}#gb60 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/e000432278.png')}.gb_c #gb60 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/e000432278.png')}#gb175 .gb_b{background-image:url('//ssl.gstatic.com/gb/images/a/84d52a8885.png')}.gb_c #gb175 .gb_b::before{content:url('//ssl.gstatic.com/gb/images/a/84d52a8885.png')}}.gb_i{padding:1px;display:inline-block;vertical-align:top;color:black;z-index:999;height:98px;width:86px}.gb_i a{text-decoration:none}.gb_i[aria-grabbed=true]{visibility:hidden}.gb_i:hover{z-index:1001}.gb_i:hover a{border:1px solid #e5e5e5;-moz-border-radius:2px;border-radius:2px;margin:7px 1px}.gb_j.gb_i:hover a{background:#f6fafe!important;background-color:rgba(26,115,232,0.04);-moz-border-radius:8px;border-radius:8px;border-color:transparent;margin:none;overflow:hidden}.gb_j.gb_i:active a,.gb_j.gb_i:focus:active a{background:#e8f0fd!important;background-color:rgba(66,133,244,0.12);-moz-border-radius:8px;border-radius:8px}.gb_i.gb_k a{border:1px solid #e5e5e5;-moz-box-shadow:0 1px 2px rgba(0,0,0,0.1);box-shadow:0 1px 2px rgba(0,0,0,0.1);background:#fff;cursor:-moz-grabbing;cursor:-webkit-grabbing;margin:-1px;visibility:visible;z-index:1001}.gb_j.gb_i.gb_k a{background-color:#ffffff;-moz-border-radius:8px;border-radius:8px;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 1px 3px 1px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 1px 3px 1px rgba(60,64,67,0.15);opacity:.8}.gb_l{opacity:.5}.gb_i.gb_k a{color:rgba(0,0,0,0.87)!important;cursor:-moz-grabbing;cursor:-webkit-grabbing;font:13px/27px Roboto,RobotoDraft,Arial,sans-serif;text-decoration:none!important}.gb_d{color:rgba(0,0,0,0.87);display:inline-block;font-size:13px;margin:8px 2px;text-align:center;outline:none}.gb_d[draggable=false]{-moz-user-select:none}.gb_d .gb_m,.gb_d .gb_b{display:inline-block;vertical-align:top;height:64px;width:64px}.gb_d .gb_n{display:inline-block;height:64px;vertical-align:top;width:64px}.gb_o{display:block;line-height:20px;overflow:hidden;white-space:nowrap;width:84px;text-overflow:ellipsis}.gb_i:hover .gb_d{z-index:1}.gb_i:not(.gb_j):hover .gb_o{background:rgba(255,255,255,.9);white-space:normal;overflow-wrap:break-word;word-wrap:break-word}.gb_j.gb_i:hover .gb_o{white-space:normal;overflow-wrap:break-word;word-wrap:break-word}.gb_d .gb_m{background-image:url('https://ssl.gstatic.com/gb/images/p1_6234ef59.png');background-size:64px 2686px}.gb_c .gb_d .gb_m,.gb_c .gb_p.gb_m{background-image:none;overflow:hidden;position:relative}.gb_c .gb_d .gb_m::before,.gb_c .gb_p.gb_m::before{content:url('https://ssl.gstatic.com/gb/images/p1_6234ef59.png');position:absolute}.gb_c .gb_b{background-image:none!important;position:relative}.gb_c .gb_b::before{left:0;position:absolute;top:0}@media (min-resolution:1.25dppx),(-webkit-min-device-pixel-ratio:1.25),(min-device-pixel-ratio:1.25){.gb_d .gb_m{background-image:url('https://ssl.gstatic.com/gb/images/p2_547d5a41.png')}.gb_c .gb_d .gb_m::before{content:url('https://ssl.gstatic.com/gb/images/p2_547d5a41.png');-moz-transform:scale(.5);transform:scale(.5);-moz-transform-origin:0 0;transform-origin:0 0}.gb_c .gb_d .gb_b::before{-moz-transform:scale(.5);transform:scale(.5);-moz-transform-origin:0 0;transform-origin:0 0}}.gb_q .gb_d:focus,#gb#gb .gb_q a.gb_d:focus{text-decoration:underline}.gb_q .gb_j .gb_d:focus,#gb#gb .gb_q .gb_j a.gb_d:focus,.gb_q .gb_j .gb_d:hover:focus,#gb#gb .gb_q .gb_j a.gb_d:hover:focus{background:#ecf2fd!important;background-color:rgba(66,133,244,0.1);-moz-border-radius:8px;border-radius:8px;text-decoration:none}.gb_i[aria-grabbed=true].gb_r{visibility:visible}.gb_s,.gb_t{position:relative;top:27px;visibility:hidden}.gb_u,.gb_v{left:37px;visibility:hidden}.gb_s{float:left;width:0;height:0;border-top:5px solid transparent;border-bottom:5px solid transparent;border-right:5px solid #4273db}.gb_t{float:right;width:0;height:0;border-top:5px solid transparent;border-bottom:5px solid transparent;border-left:5px solid #4273db}.gb_u{position:absolute;top:0;width:0;height:0;border-left:5px solid transparent;border-right:5px solid transparent;border-bottom:5px solid #4273db}.gb_v{position:absolute;top:59px;width:0;height:0;border-left:5px solid transparent;border-right:5px solid transparent;border-top:5px solid #4273db}ul.gb_w li.gb_r:not(:first-child) .gb_s,ul.gb_w li.gb_r:not(:nth-child(-n+3)) .gb_u,ul.gb_w li.gb_r .gb_t,ul.gb_w li.gb_r .gb_v,ul.gb_x li.gb_r .gb_s,ul.gb_x li.gb_r .gb_u,ul.gb_x li.gb_r:not(:last-child) .gb_t,ul.gb_x li.gb_r:not(:nth-last-child(-n+3)) .gb_v{visibility:visible}.gb_B.gb_C{min-height:196px;overflow-y:auto;width:320px}.gb_B.gb_C.gb_j{-moz-border-radius:8px;border-radius:8px;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,.30),0 2px 6px 2px rgba(60,64,67,.15);box-shadow:0 1px 2px 0 rgba(60,64,67,.30),0 2px 6px 2px rgba(60,64,67,.15)}.gb_D{-moz-transition:height .2s ease-in-out;transition:height .2s ease-in-out}.gb_E{background:#fff;margin:0;min-height:100px;padding:28px;padding-right:27px;text-align:left;white-space:normal;width:265px}.gb_F{background:#f5f5f5;cursor:pointer;height:40px;overflow:hidden}.gb_H{position:relative}.gb_F{display:block;line-height:40px;text-align:center;width:320px}.gb_H{display:block;line-height:40px;text-align:center}.gb_H.gb_I{line-height:0}.gb_F,.gb_F:visited,.gb_F:active,.gb_H,.gb_H:visited{color:rgba(0,0,0,0.87);text-decoration:none}.gb_H:active{color:rgba(0,0,0,0.87)}#gb a.gb_F,#gb a.gb_F:visited,#gb a.gb_F:active,#gb a.gb_H,#gb a.gb_H:visited{color:rgba(0,0,0,0.87);text-decoration:none}#gb a.gb_H:active{color:rgba(0,0,0,0.87)}.gb_H,.gb_E,.gb_J,.gb_K{display:none}.gb_w,.gb_w+.gb_H,.gb_L .gb_H,.gb_L .gb_E{display:block}.gb_L .gb_J,.gb_L .gb_K{display:inline-block}.gb_H:hover,.gb_H:active,#gb a.gb_H:hover,#gb a.gb_H:active{text-decoration:underline}.gb_H{border-bottom:1px solid #ebebeb;left:28px;width:264px}.gb_j .gb_H{border-bottom:1px solid #e8eaed;left:0;width:320px}a.gb_J,a.gb_K{background-color:#ffffff;border:1px solid #dadce0;-moz-border-radius:4px;border-radius:4px;box-sizing:border-box;color:#1a73e8;font:500 14px/16px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;left:50%;margin:0 0 24px 0;max-width:264px;outline:none;overflow:hidden;padding:10px 24px;position:relative;text-align:center;text-decoration:none;text-overflow:ellipsis;transform:translateX(-50%);white-space:nowrap}.gb_K:hover,.gb_J:hover{background-color:#f8fbff;border-color:#cce0fc}.gb_K:focus,.gb_K:hover:focus,.gb_J:focus,.gb_J:hover:focus{background-color:#f4f8ff;border-color:#c9ddfc}.gb_K:active,.gb_K:active:focus,.gb_J:active,.gb_J:active:focus{background-color:#ecf3fe;border-color:transparent;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15)}.gb_L .gb_F{display:none}.gb_H:last-child{border-bottom-width:0}.gb_M .gb_d{display:initial}.gb_M.gb_N{height:100px;text-align:center}.gb_M.gb_N img{padding:34px 0;height:32px;width:32px}.gb_M .gb_m+img{border:0;margin:8px;height:48px;width:48px}.gb_M div.gb_O{background:#ffa;-moz-border-radius:5px;border-radius:5px;padding:5px;text-align:center}.gb_M.gb_P,.gb_M.gb_Q{padding-bottom:0}.gb_M.gb_R,.gb_M.gb_Q{padding-top:0}.gb_M.gb_Q a,.gb_M.gb_R a{top:0}.gb_S .gb_F{margin-top:0;position:static}.gb_T{display:inline-block}.gb_U{margin:-12px 28px 28px;position:relative;width:264px;-moz-border-radius:2px;border-radius:2px;-moz-box-shadow:0 1px 2px rgba(0,0,0,0.1),0 0 1px rgba(0,0,0,0.1);box-shadow:0 1px 2px rgba(0,0,0,0.1),0 0 1px rgba(0,0,0,0.1)}.gb_p{background-image:url('https://ssl.gstatic.com/gb/images/p1_6234ef59.png');background-size:64px 2686px;display:inline-block;margin:8px;vertical-align:middle;height:64px;width:64px}.gb_V{color:#262626;display:inline-block;font:13px/18px Arial,sans-serif;margin-right:80px;padding:10px 10px 10px 0;vertical-align:middle;white-space:normal}.gb_W{font:16px/24px Arial,sans-serif}.gb_X,#gb#gb .gb_X{color:#427fed;text-decoration:none}.gb_X:hover,#gb#gb .gb_X:hover{text-decoration:underline}.gb_Z .gb_E{position:relative}.gb_Z .gb_d{position:absolute;top:28px;left:28px}.gb_F.gb_0{display:none;height:0}.fFW7wc-OEVmcd{width:100%;height:100%;border:0;overflow:hidden}.fFW7wc.XKSfm-Sx9Kwc-xJ5Hnf{position:absolute;top:0;left:0;background-color:#fff}.fFW7wc.XKSfm-Sx9Kwc{position:absolute;top:0;left:0;background-color:#fff;border:1px solid #acacac;width:auto;padding:0;z-index:1001;overflow:auto;-moz-box-shadow:rgba(0,0,0,.2) 0 4px 16px;-moz-box-shadow:rgba(0,0,0,.2) 0 4px 16px;box-shadow:rgba(0,0,0,.2) 0 4px 16px;-moz-transition:top .5s ease-in-out;transition:top .5s ease-in-out}.fFW7wc-jJNx8e{position:absolute;z-index:1002}.fFW7wc.XKSfm-Sx9Kwc-bN97Pc{font-size:0;padding:0}.fFW7wc.XKSfm-Sx9Kwc-r4nke{height:0;margin:0}.fFW7wc.XKSfm-Sx9Kwc-r4nke-fmcmS,.fFW7wc.XKSfm-Sx9Kwc-c6xFrd{display:none}.gb_Da~.gb_Ua,.gb_Da~.gb_Va{left:auto;right:11.5px}.gb_Wa{outline:none;transform:translateZ(0)}.gb_j.gb_Wa{-moz-border-radius:8px;border-radius:8px}.gb_Wa.gb_oa:not(.gb_j){width:320px}.gb_j.gb_Wa{margin-left:12px}@media screen and (min-width:361px){.gb_j.gb_Wa{width:354px}}@media screen and (max-width:361px){.gb_j.gb_Wa{width:calc(100vw - 12px*2)}}.gb_j.gb_Wa.gb_Xa{max-height:-moz-calc(100vh - 62px - 30px);max-height:calc(100vh - 62px - 30px)}.gb_j.gb_Wa.gb_Za{max-height:-moz-calc(100vh - 62px - 15px - 30px);max-height:calc(100vh - 62px - 15px - 30px)}.gb_j.gb_Wa.gb_0a{background-color:#2d2e30}.gb_1a.gb_2a{color:#5f6368;font:400 12px/16px Roboto,RobotoDraft,Helvetica,Arial,sans-serif}.gb_3a.gb_1a.gb_2a{background-color:rgba(138,180,248,0.24);color:#e8eaed}.gb_4a,#gb a.gb_4a.gb_4a,.gb_5a a,#gb .gb_5a.gb_5a a{color:#36c;text-decoration:none}.gb_1a>.gb_4a,#gb .gb_1a>a.gb_4a.gb_4a{color:#0070ff;font:inherit;font-weight:500;outline:0}.gb_3a.gb_1a>.gb_4a,#gb .gb_3a.gb_1a>a.gb_4a.gb_4a{color:#8ab4f8}.gb_4a:active,#gb a.gb_4a.gb_4a:active,.gb_4a:hover,#gb a.gb_4a.gb_4a:hover,.gb_5a a:active,#gb .gb_5a a:active,.gb_5a a:hover,#gb .gb_5a a:hover,#gb .gb_1a>a.gb_4a.gb_4a:focus{text-decoration:underline}.gb_6a{margin:20px;white-space:nowrap}.gb_j>.gb_6a{margin:20px 33px}.gb_7a,.gb_8a{display:inline-block;vertical-align:top}.gb_7a.gb_9a,.gb_8a.gb_ab{vertical-align:middle}.gb_j .gb_7a,.gb_j .gb_8a{display:block;vertical-align:top;text-align:center}.gb_9a{cursor:default}.gb_Wa.gb_oa:not(.gb_j) .gb_8a{max-width:164px}.gb_bb.gb_7a{margin-right:20px;position:relative}.gb_j .gb_7a{margin-bottom:10px;position:relative;height:86px;width:86px}.gb_cb{-moz-border-radius:50%;border-radius:50%;overflow:hidden}.gb_Ca{background-size:96px 96px;border:none;vertical-align:top;height:96px;width:96px}.gb_db{margin-bottom:11px;margin-top:4px}@media screen and (min-width:361px){.gb_j .gb_7a,.gb_db{margin-left:101px}}@media screen and (max-width:361px){.gb_j .gb_7a,.gb_db{margin-left:calc((calc(100vw - 12px*2) - 33px*2 - 86px)/2)}}.gb_eb.gb_eb{fill:#1a73e8}.gb_0a .gb_eb{fill:#8ab4f8}.gb_j .gb_Ca{background-size:80px 80px;height:80px;width:80px}.gb_j .gb_fb.gb_Ca{background-size:76px 76px;position:relative;left:2px;top:2px;height:76px;width:76px}.gb_Qa{background:rgba(78,144,254,.7);bottom:0;color:#fff;font-size:9px;font-weight:bold;left:0;line-height:9px;position:absolute;padding:7px 0;text-align:center;width:96px}.gb_j .gb_gb{background:#fff;bottom:0;position:absolute;right:0;overflow:visible;height:32px;width:32px}.gb_j.gb_0a .gb_gb{background:#2d2e30}.gb_hb{bottom:0;-moz-box-shadow:0 1px 1px 0 rgba(65,69,73,0.3),0 1px 3px 1px rgba(65,69,73,0.15);box-shadow:0 1px 1px 0 rgba(65,69,73,0.3),0 1px 3px 1px rgba(65,69,73,0.15);margin:0 2.5px 3px;outline:0;position:absolute;right:0;height:26px;width:26px}.gb_hb:hover{background-color:#f8faff}.gb_hb:focus,.gb_hb:hover:focus{background-color:#f4f8ff}.gb_hb:active,.gb_hb:focus:active{background-color:#f4f8ff;-moz-box-shadow:0 1px 3px 0 rgba(60,64,67,0.3),0 4px 8px 3px rgba(60,64,67,0.15);box-shadow:0 1px 3px 0 rgba(60,64,67,0.3),0 4px 8px 3px rgba(60,64,67,0.15)}.gb_hb:hover>svg.gb_ib,.gb_hb:focus>svg.gb_ib,.gb_hb:active>svg.gb_ib{fill:#1a73e8}.gb_cb .gb_Qa{background:rgba(0,0,0,.54)}.gb_jb{font-weight:bold;margin:-4px 0 1px 0;text-overflow:ellipsis;overflow:hidden}.gb_j .gb_jb{color:#202124;font:500 16px/18px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;letter-spacing:.29px;margin:0 0 2px 0;text-align:center;text-overflow:ellipsis;overflow:hidden}.gb_j.gb_0a .gb_jb{color:#e8eaed}.gb_lb{color:#666;text-overflow:ellipsis;overflow:hidden}.gb_j .gb_lb{color:#5f6368;font:400 14px/19px Roboto,RobotoDraft,Helvetica,Arial,sans-serif;letter-spacing:normal;text-align:center;text-overflow:ellipsis;overflow:hidden}.gb_j.gb_0a .gb_lb{color:#e8eaed}.gb_ab>.gb_lb{color:#000;font-weight:bold;margin:-4px 0 1px 0;text-overflow:ellipsis;overflow:hidden}.gb_mb{color:#666;font-style:italic;font-weight:500;margin:4px 0;overflow:hidden}.gb_nb{color:#5f6368;font-family:Roboto,RobotoDraft,Helvetica,Arial,sans-serif;font-size:14px;line-height:19px;margin-top:4px;text-align:center}.gb_0a .gb_nb{color:#9aa0a6}.gb_ob{font-weight:500}.gb_pb.gb_pb{background-color:#ffffff;border:1px solid #dadce0;-moz-border-radius:100px;border-radius:100px;color:#3c4043;display:inline-block;font:500 14px/16px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;letter-spacing:.25px;margin:16px 0 0;max-width:254px;outline:0;padding:8px 16px;text-align:center;text-decoration:none;text-overflow:ellipsis;overflow:hidden}.gb_0a .gb_pb.gb_pb{background-color:#2d2e30;border:1px solid #5f6368;color:#e8eaed}.gb_pb:hover{background-color:#f7f8f8}.gb_pb:focus,.gb_pb:hover:focus{background-color:#f4f4f4}.gb_pb:active,.gb_pb:focus:active{background-color:#e8e8e9;border-color:transparent;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15)}.gb_5a{color:#ccc;margin:6px 0}.gb_Wa.gb_oa .gb_5a a{display:block;line-height:24px;margin:0}.gb_Wa.gb_oa .gb_5a a:first-child:last-child{line-height:normal}.gb_Wa:not(.gb_oa) .gb_5a a{margin:0 10px}.gb_Wa:not(.gb_oa) .gb_5a a:first-child{margin-left:0}.gb_Wa:not(.gb_oa) .gb_5a a:last-child{margin-right:0}.gb_qb{color:#5f6368;margin:14px 33px;text-align:center;white-space:normal}.gb_0a .gb_qb{color:#e8eaed}.gb_rb.gb_rb{-moz-border-radius:4px;border-radius:4px;color:#5f6368;display:inline-block;font:400 12px/16px Roboto,RobotoDraft,Helvetica,Arial,sans-serif;outline:0;padding:4px 8px;text-decoration:none;text-align:center;white-space:normal}.gb_0a .gb_rb.gb_rb{border:1px solid transparent;color:#e8eaed}.gb_rb:hover{background-color:#f7f8f8}.gb_rb:focus,.gb_rb:hover:focus{background-color:#f4f4f4}.gb_rb:active,.gb_rb:active:focus{background-color:#e8e8e9}.gb_8a .gb_4{background:#4d90fe;border-color:#3079ed;font-weight:bold;margin:10px 0 0 0;color:#fff}#gb .gb_8a a.gb_4.gb_4{color:#fff}.gb_8a .gb_4:hover{background:#357ae8;border-color:#2f5bb7}.gb_sb.gb_N{border-top:none}.gb_sb{background:#f5f5f5;border-top:1px solid #ccc;border-color:rgba(0,0,0,.2);padding:10px 0;width:100%;display:table}.gb_sb .gb_tb{margin:0 20px;white-space:nowrap}.gb_sb>div{display:table-cell;text-align:right}.gb_sb>div:first-child{text-align:left}.gb_sb .gb_ub{display:block;text-align:center}.gb_vb .gb_Ua{border-bottom-color:#fef9db}.gb_2a{background:#fef9db;font-size:11px;padding:10px 20px;white-space:normal}.gb_1a.gb_2a{background:#e8f0fe;-moz-border-radius:4px;border-radius:4px;margin:4px;padding:4px 29px;text-align:center}.gb_1a.gb_2a>#gbpbt>span{white-space:nowrap;font-weight:500}.gb_2a b,.gb_4a{white-space:nowrap}.gb_wb.gb_wb{background-color:#ffffff;color:#3c4043;display:table;font:500 14px/16px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;letter-spacing:.25px;outline:0;padding:14px 41px;text-align:center;text-decoration:none;width:100%}.gb_0a .gb_wb.gb_wb{background-color:#2d2e30;border:1px solid transparent;color:#e8eaed;width:270px}.gb_wb:hover{background-color:#f7f8f8}.gb_wb:focus,.gb_wb:hover:focus{background-color:#f4f4f4}.gb_wb:active,.gb_wb:focus:active{background-color:#e8e8e9}.gb_xb{border:none;display:table-cell;vertical-align:middle;height:20px;width:20px}.gb_hb>svg.gb_ib,.gb_xb>svg.gb_yb,.gb_zb>svg.gb_Ab{color:#5f6368;fill:currentColor}.gb_0a .gb_zb>svg.gb_Ab{fill:#9aa0a6}.gb_0a .gb_hb{border:1px solid transparent;-moz-box-shadow:0 1px 3px 0 rgba(0,0,0,0.3),0 4px 8px 3px rgba(0,0,0,0.15);box-shadow:0 1px 3px 0 rgba(0,0,0,0.3),0 4px 8px 3px rgba(0,0,0,0.15)}.gb_0a .gb_hb>svg.gb_ib,.gb_0a .gb_xb>svg.gb_yb{color:#e8eaed;fill:currentColor}.gb_0a .gb_hb:hover>svg.gb_ib,.gb_0a .gb_hb:focus>svg.gb_ib,.gb_0a .gb_hb:focus:hover>svg.gb_ib,.gb_0a .gb_hb:active>svg.gb_ib{fill:#8ab4f8}.gb_0a .gb_hb:hover{background-color:#353639;-moz-box-shadow:0 2px 3px 0 rgba(0,0,0,0.3),0 6px 10px 4px rgba(0,0,0,0.15);box-shadow:0 2px 3px 0 rgba(0,0,0,0.3),0 6px 10px 4px rgba(0,0,0,0.15)}.gb_0a .gb_hb:focus,.gb_0a .gb_hb:focus:hover{background-color:#353639;border:1px solid #5f6368;-moz-box-shadow:0 2px 3px 0 rgba(0,0,0,0.3),0 6px 10px 4px rgba(0,0,0,0.15);box-shadow:0 2px 3px 0 rgba(0,0,0,0.3),0 6px 10px 4px rgba(0,0,0,0.15)}.gb_0a .gb_hb:active{background-color:rgba(255,255,255,0.12);-moz-box-shadow:0 4px 4px 0 rgba(0,0,0,0.3),0 8px 12px 6px rgba(0,0,0,0.15);box-shadow:0 4px 4px 0 rgba(0,0,0,0.3),0 8px 12px 6px rgba(0,0,0,0.15)}.gb_Bb{display:table-cell;padding:0 74px 0 16px;text-align:left;vertical-align:middle;white-space:normal}.gb_Cb{border-bottom:1px solid #e8eaed;border-top:1px solid #e8eaed;padding:0 17px;text-align:center}.gb_0a .gb_Cb{border-bottom:1px solid #5f6368;border-top:1px solid #5f6368}.gb_Db.gb_Db,.gb_Eb.gb_Eb{background-color:#ffffff;border:1px solid #dadce0;-moz-border-radius:4px;border-radius:4px;display:inline-block;font:500 14px/16px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;letter-spacing:.15px;margin:16px;outline:0;padding:10px 24px;text-align:center;text-decoration:none;white-space:normal}.gb_Db.gb_Db{color:#3c4043}.gb_Eb.gb_Eb{color:#1a73e8}.gb_0a .gb_Eb.gb_Eb,.gb_0a .gb_Db.gb_Db{background-color:#2d2e30;border:1px solid #5f6368;color:#e8eaed}.gb_Db:hover{background-color:#f7f8f8}.gb_Db:focus,.gb_Db:hover:focus{background-color:#f4f4f4}.gb_Db:active,.gb_Db:active:focus{background-color:#e8e8e9;border-color:transparent;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15)}.gb_Eb:hover{background-color:#f8fbff;border-color:#cce0fc}.gb_Eb:focus,.gb_Eb:hover:focus{background-color:#f4f8ff;border-color:#c9ddfc}.gb_Eb:active,.gb_Eb:active:focus{background-color:#ecf3fe;border-color:transparent;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15)}.gb_Jb{border-top:1px solid #e8eaed}.gb_0a .gb_Jb{border-top:1px solid #5f6368}.gb_Mb{background:#f5f5f5;border-top:1px solid #ccc;border-top-color:rgba(0,0,0,.2);max-height:230px;overflow:auto}.gb_j.gb_Wa{overflow-y:auto;overflow-x:hidden}.gb_Mb.gb_oa{max-height:170px}.gb_Mb.gb_oa.gb_Nb{max-height:124px}.gb_Ob{border-top:1px solid #ccc;border-top-color:rgba(0,0,0,.2);display:block;outline-offset:-2px;padding:10px 20px;position:relative;white-space:nowrap}.gb_Pb>.gb_Ob{border:none;cursor:pointer;height:35px;outline:0;padding:12px 33px 13px}.gb_Qb .gb_Ob:focus .gb_Rb{outline:1px dotted #fff}.gb_Ob:hover{background:#eee}.gb_Pb>.gb_Ob:hover{background-color:#f7f8f8}.gb_Pb>.gb_Ob:focus,.gb_Pb>.gb_Ob:hover:focus{background-color:#f4f4f4}.gb_Pb>.gb_Ob:active,.gb_Pb>.gb_Ob:focus:active{background-color:#e8e8e9}.gb_0a .gb_Sb:hover,.gb_0a .gb_wb:hover,.gb_0a .gb_rb:hover,.gb_0a .gb_Pb>.gb_Ob:hover{background-color:rgba(255,255,255,0.04);border:1px solid transparent}.gb_0a .gb_Db:hover,.gb_0a .gb_pb:hover{background-color:rgba(232,234,237,0.04);border:1px solid #5f6368}.gb_0a .gb_Sb:focus,.gb_0a .gb_Sb:hover:focus,.gb_0a .gb_pb:focus,.gb_0a .gb_pb:hover:focus,.gb_0a .gb_wb:focus,.gb_0a .gb_wb:hover:focus,.gb_0a .gb_Db:focus,.gb_0a .gb_Db:hover:focus,.gb_0a .gb_Pb>.gb_Ob:focus,.gb_0a .gb_Pb>.gb_Ob:hover:focus{background-color:rgba(232,234,237,0.12);border:1px solid #e8eaed}.gb_0a .gb_rb:focus,.gb_0a .gb_rb:hover:focus{background-color:rgba(232,234,237,0.12)}.gb_0a .gb_Sb:active,.gb_0a .gb_Sb:focus:active,.gb_0a .gb_wb:active,.gb_0a .gb_wb:focus:active,.gb_0a .gb_rb:active,.gb_0a .gb_rb:active:focus,.gb_0a .gb_Pb>.gb_Ob:active,.gb_0a .gb_Pb>.gb_Ob:focus:active{background-color:rgba(232,234,237,0.1);border:1px solid transparent}.gb_0a .gb_Pb>.gb_Ob{border:1px solid transparent}.gb_0a .gb_Db:active,.gb_0a .gb_Db:active:focus,.gb_0a .gb_pb:active,.gb_0a .gb_pb:focus:active{background-color:rgba(232,234,237,0.1);border:1px solid #5f6368}.gb_Ob[selected="true"]{overflow:hidden}.gb_Ob[selected="true"]>.gb_Tb{background-color:rgba(117,117,117,.9)}.gb_Pb>.gb_Ob[selected="true"]{background-color:rgba(60,64,67,0.1)}.gb_0a .gb_Pb>.gb_Ob[selected="true"]{background-color:rgba(255,255,255,0.12)}.gb_Ob[selected="true"]>.gb_Ub{display:block;position:absolute;z-index:2}.gb_Ub::-moz-focus-inner{border:0}.gb_Ub{background-color:transparent;border:none;color:#fff;display:none;font-family:Roboto,Arial,sans-serif;font-weight:400;font-size:14px;min-height:36px;min-width:86px;text-align:center;top:16px;width:auto}.gb_Vb.gb_Ub{-moz-border-radius:4px;border-radius:4px;box-sizing:border-box;cursor:pointer;display:inline-block;font-family:Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;font-weight:500;letter-spacing:.25px;line-height:16px;margin-bottom:1px;min-width:86px;outline:none;padding:10px 24px;text-decoration:none}.gb_Vb.gb_Wb{background-color:#1a73e8;color:#fff;margin-left:0;margin-right:12px;margin-top:14px}.gb_0a .gb_Vb.gb_Wb{background-color:#8ab4f8;color:#2d2e30}.gb_Vb.gb_Xb{background-color:#ffffff;border:1px solid #dadce0;color:#3c4043;margin-left:0;margin-right:0;margin-top:11px}.gb_0a .gb_Vb.gb_Xb{background-color:rgba(218,220,224,0.01);border:1px solid #5f6368;color:#e8eaed}.gb_Vb.gb_Ub.gb_Xb:hover{background-color:#f7f8f8}.gb_Vb.gb_Ub.gb_Xb:focus,.gb_Vb.gb_Ub.gb_Xb:hover:focus{background-color:#f4f4f4}.gb_Vb.gb_Ub.gb_Xb:active{background-color:#f4f4f4;border:1px solid #5f6368;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 1px 3px 1px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 1px 3px 1px rgba(60,64,67,0.15)}.gb_Vb.gb_Ub.gb_Wb:hover{background-color:#2b7de9;border-color:transparent;-moz-box-shadow:0 1px 2px 0 rgba(66,133,244,0.3),0 1px 3px 1px rgba(66,133,244,0.15);box-shadow:0 1px 2px 0 rgba(66,133,244,0.3),0 1px 3px 1px rgba(66,133,244,0.15)}.gb_Vb.gb_Ub.gb_Wb:focus,.gb_Vb.gb_Ub.gb_Wb:hover:focus{background-color:#5094ed;border-color:transparent;-moz-box-shadow:0 1px 2px 0 rgba(66,133,244,0.3),0 1px 3px 1px rgba(66,133,244,0.15);box-shadow:0 1px 2px 0 rgba(66,133,244,0.3),0 1px 3px 1px rgba(66,133,244,0.15)}.gb_Vb.gb_Ub.gb_Wb:active{background-color:#63a0ef;-moz-box-shadow:0 1px 2px 0 rgba(66,133,244,0.3),0 1px 3px 1px rgba(66,133,244,0.15);box-shadow:0 1px 2px 0 rgba(66,133,244,0.3),0 1px 3px 1px rgba(66,133,244,0.15)}.gb_0a .gb_Vb.gb_Ub.gb_Xb:hover{background-color:rgba(232,234,237,0.04)}.gb_0a .gb_Vb.gb_Ub.gb_Xb:focus,.gb_0a .gb_Vb.gb_Ub.gb_Xb:hover:focus{background-color:rgba(232,234,237,0.12);border:1px solid #e8eaed}.gb_0a .gb_Vb.gb_Ub.gb_Xb:active,.gb_0a .gb_Vb.gb_Ub.gb_Xb:active:focus{background-color:rgba(232,234,237,0.1);border:1px solid #5f6368;-moz-box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15);box-shadow:0 1px 2px 0 rgba(60,64,67,0.3),0 2px 6px 2px rgba(60,64,67,0.15)}.gb_0a .gb_Vb.gb_Ub.gb_Wb:hover{background-color:#93b9f8;-moz-box-shadow:0 1px 2px 0 rgba(0,0,0,0.3),0 1px 3px 1px rgba(0,0,0,0.15);box-shadow:0 1px 2px 0 rgba(0,0,0,0.3),0 1px 3px 1px rgba(0,0,0,0.15)}.gb_0a .gb_Vb.gb_Ub.gb_Wb:focus,.gb_0a .gb_Vb.gb_Ub.gb_Wb:hover:focus{background-color:#a5c5f9}.gb_0a .gb_Vb.gb_Ub.gb_Wb:active{background-color:#8ab4f8;-moz-box-shadow:0 1px 2px 0 rgba(0,0,0,0.3),0 2px 6px 2px rgba(0,0,0,0.15);box-shadow:0 1px 2px 0 rgba(0,0,0,0.3),0 2px 6px 2px rgba(0,0,0,0.15)}.gb_Ob[selected="true"]>.gb_Ub:focus{background-color:rgba(0,0,0,.24);-moz-border-radius:2px;border-radius:2px;outline:0}.gb_Ob[selected="true"]>.gb_Ub:hover,.gb_Ob[selected="true"]>.gb_Ub:focus:hover{background-color:#565656;-moz-border-radius:2px;border-radius:2px}.gb_Ob[selected="true"]>.gb_Ub:active{-moz-border-radius:2px;border-radius:2px;background-color:#212121}.gb_Xb{left:0;margin-left:5%}.gb_Wb{margin-right:5%;right:0}.gb_Ob:first-child,.gb_Zb:first-child+.gb_Ob{border-top:0}.gb_0a.gb_j .gb_Ob:first-child,.gb_0a.gb_j .gb_Zb:first-child+.gb_Ob{border-top:1px solid transparent}.gb_0a.gb_j .gb_Ob:first-child:focus:hover,.gb_0a.gb_j .gb_Zb:first-child+.gb_Ob:focus:hover,.gb_0a.gb_j .gb_Ob:first-child:focus,.gb_0a.gb_j .gb_Zb:first-child+.gb_Ob:focus{border-top:1px solid #e8eaed}.gb_0a.gb_j .gb_Ob:first-child:active,.gb_0a.gb_j .gb_Zb:first-child+.gb_Ob:active,.gb_0a.gb_j .gb_Ob:first-child:active:focus,.gb_0a.gb_j .gb_Zb:first-child+.gb_Ob:active:focus{border-top:1px solid transparent}.gb_Zb{display:none}.gb_0b{cursor:default}.gb_0b:hover{background:transparent}.gb_Pb>.gb_0b{opacity:.38}.gb_Pb>.gb_Ob.gb_0b:hover,.gb_Pb>.gb_Ob.gb_0b:focus,.gb_Pb>.gb_Ob.gb_0b:active{background-color:#fff}.gb_1b{border:none;vertical-align:top;height:48px;width:48px}.gb_j .gb_1b{height:32px;width:32px}.gb_Rb{display:inline-block;margin:6px 0 0 10px}.gb_j .gb_Rb{margin:0 0 0 12px}@media screen and (min-width:361px){.gb_j .gb_Rb{width:244px}}@media screen and (max-width:361px){.gb_j .gb_Rb{width:calc(calc(100vw - 12px*2) - 12px - 32px - 33px*2)}}.gb_Wa.gb_oa .gb_Rb{max-width:222px}.gb_Mb>.gb_0b .gb_1b,.gb_Mb>.gb_0b .gb_Rb{opacity:.4}.gb_2b .gb_Rb{margin-top:7px}.gb_3b .gb_Rb{margin-top:9px}.gb_4b{color:#000;text-overflow:ellipsis;overflow:hidden}.gb_j .gb_4b{color:#3c4043;font:500 14px/18px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;letter-spacing:.25px}.gb_j.gb_0a .gb_4b{color:#e8eaed}.gb_Mb>.gb_0b .gb_4b{color:#666}.gb_5b .gb_4b{font-family:Roboto,RobotoDraft,Helvetica,Arial,sans-serif;font-size:12px;letter-spacing:normal;line-height:16px}.gb_6b{color:#666;text-overflow:ellipsis;overflow:hidden}.gb_2b .gb_6b{color:#000;margin-bottom:2px}.gb_j.gb_0a .gb_6b{color:#bdc1c6}.gb_j .gb_6b{color:#5f6368;display:inline-block;font:400 12px/16px Roboto,RobotoDraft,Helvetica,Arial,sans-serif}@media screen and (min-width:361px){.gb_j .gb_6b{max-width:244px}}@media screen and (max-width:361px){.gb_j .gb_6b{max-width:calc(calc(100vw - 12px*2) - 12px - 32px - 33px*2)}}@media screen and (min-width:361px){.gb_Rb>.gb_6b.gb_7b{max-width:224px}}@media screen and (max-width:361px){.gb_Rb>.gb_6b.gb_7b{max-width:calc(calc(calc(100vw - 12px*2) - 12px - 32px - 33px*2) - 20px)}}.gb_Pb>.gb_0b .gb_6b{color:#3c4043}.gb_5b .gb_6b{margin-top:2px}.gb_8b{color:#666;font-style:italic}.gb_j .gb_8b{color:#5d6369;display:block;float:right;font:italic 400 12px/14px Roboto,RobotoDraft,Helvetica,Arial,sans-serif;padding:3px 0 0 20px;text-align:right;visibility:visible}.gb_j.gb_0a .gb_8b{color:#9aa0a6}.gb_Tb{background-color:transparent;height:100%;left:0;position:absolute;text-align:center;top:0;width:100%;z-index:1}.gb_9b{background-color:transparent;display:none;left:0;overflow-wrap:break-word;position:relative;margin-left:44px;white-space:normal;width:100%;word-wrap:break-word;z-index:1}@media screen and (min-width:361px){.gb_9b{max-width:244px}}@media screen and (max-width:361px){.gb_9b{max-width:calc(calc(100vw - 12px*2) - 33px*2 - 44px)}}.gb_Ob[selected="true"]>.gb_9b{display:block}.gb_Pb>.gb_Ob[selected="true"]{height:auto;min-height:91px}.gb_Ub:hover{background-color:rgba(100,100,100,0.4)}.gb_ac{background:#f5f5f5;border-top:1px solid #ccc;border-top-color:rgba(0,0,0,.2)}.gb_bc{display:block;padding:10px 20px}.gb_Sb{outline:0;padding:14px 41px;width:280px}.gb_Sb:hover{background-color:#f7f8f8}.gb_Sb:focus,.gb_Sb:hover:focus{background-color:#f4f4f4}.gb_Sb:active,.gb_Sb:focus:active{background-color:#e8e8e9}.gb_cc{background-position:-244px 0;display:inline-block;margin:1px 0;vertical-align:middle;height:25px;width:25px}.gb_dc{display:inline-block;vertical-align:middle;height:20px;width:20px}.gb_c .gb_cc::before{left:-244px;top:0}.gb_ec{color:#427fed;display:inline-block;padding:0 25px 0 10px;vertical-align:middle;white-space:normal}.gb_fc{color:#3c4043;font:500 14px/18px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;padding:0 25px 0 16px;text-align:left}@media screen and (min-width:361px){.gb_fc{width:195px}}@media screen and (max-width:361px){.gb_fc{width:calc(calc(calc(100vw - 12px*2) - 12px - 32px - 33px*2) - 24px - 25px)}}.gb_0a .gb_fc{color:#e8eaed}.gb_gc{vertical-align:middle}.gb_hc{transform:rotate(180deg)}.gb_ac:hover .gb_ec{text-decoration:underline}.gb_sb .gb_tb:hover{-moz-box-shadow:0 1px 1px rgba(0,0,0,0.1);box-shadow:0 1px 1px rgba(0,0,0,0.1);border-color:#c6c6c6;color:#222;background-color:#fff;background-image:-moz-linear-gradient(top,#fff,#f8f8f8);background-image:-moz-linear-gradient(top,#fff,#f8f8f8);background-image:linear-gradient(top,#fff,#f8f8f8);filter:progid:DXImageTransform.Microsoft.gradient(startColorStr='#ffffff',EndColorStr='#f8f8f8')}.gb_ic{height:108px;position:absolute;right:-6px;top:-6px;width:108px}.gb_jc{height:88px;position:absolute;right:2px;top:-4px;width:88px}@-moz-keyframes progressmove{0%{margin-left:-100%}to{margin-left:100%}}@keyframes progressmove{0%{margin-left:-100%}to{margin-left:100%}}.gb_kc.gb_za,.gb_lc.gb_za{display:none}.gb_kc{background-color:#ccc;height:3px;overflow:hidden}.gb_kc.gb_lc{background-color:#d2e3fc}.gb_0a .gb_kc.gb_lc{background-color:rgba(138,180,248,0.24)}.gb_mc{background-color:#f4b400;height:100%;width:50%;-moz-animation:progressmove 1.5s linear 0s infinite;animation:progressmove 1.5s linear 0s infinite}.gb_lc>.gb_mc{background-color:#1a73e8}.gb_0a .gb_lc>.gb_mc{background-color:#8ab4f8}.gb_Ab,.gb_nc{height:20px;position:absolute;top:-2px;width:20px}.gb_Ob .gb_zb{display:inline-block;height:16px;position:relative;width:20px}.gb_Ab{display:inline-block}.gb_Ob[selected="true"] .gb_Ab{transform:rotate(180deg)}.gb_zb{display:none}.gb_oc{background:#fce8e6;border:1px solid #fad2cf;color:#ea4335;font-size:11px;height:22px;left:-24px;line-height:22px;margin-top:8px;position:absolute;text-align:center;transform:rotate(-45deg);width:90px;z-index:-1}.gb_0a>.gb_oc,.gb_oc.gb_pc{top:4px;z-index:auto}[dir="rtl"] .gb_oc{transform:rotate(45deg)}.gb_ke{text-align:left}.gb_ke>*{color:#bdc1c6;line-height:16px}.gb_ke div:first-child{color:white}.gb_Kc .gb_Ec{overflow:hidden}.gb_Kc .gb_Ec:hover{overflow-y:auto}.gb_We.gb_Xe{background:rgba(255,255,255,1);border:1px solid transparent;-moz-box-shadow:0 1px 1px 0 rgba(65,69,73,0.3),0 1px 3px 1px rgba(65,69,73,0.15);box-shadow:0 1px 1px 0 rgba(65,69,73,0.3),0 1px 3px 1px rgba(65,69,73,0.15)}.gb_We.gb_Xe .gb_hf{color:black;opacity:1}.gb_We.gb_Xe input::-moz-placeholder{color:rgba(0,0,0,0.54)}.gb_We.gb_Xe button svg{color:#5f6368;opacity:1}.gb_if{background:#fff;border:1px solid transparent;-moz-border-radius:0 0 8px 8px;border-radius:0 0 8px 8px;border-top:0;font:normal 16px Google Sans,Roboto,RobotoDraft,Helvetica,Arial,sans-serif;position:absolute;z-index:986;-moz-box-shadow:0 1px 1px 0 rgba(65,69,73,0.3),0 1px 3px 1px rgba(65,69,73,0.15);box-shadow:0 1px 1px 0 rgba(65,69,73,0.3),0 1px 3px 1px rgba(65,69,73,0.15)}.gb_jf{cursor:pointer;line-height:24px;padding:8px;padding-left:64px}.gb_kf{color:#999;font-weight:normal}.gb_lf{background-color:#f5f5f5}sentinel{}  |}

[@@expect.uncaught_exn {| |}]

let%expect_test _ =
  test "#a:hover {}";
  [%expect {|
  (identifiers ((Id a)))
  #a-id-rewritten:hover {

  }|}]
;;

let%expect_test _ =
  test "#a #b > #c {}";
  [%expect
    {|
    (identifiers ((Id a) (Id b) (Id c)))
    #a-id-rewritten #b-id-rewritten>#c-id-rewritten {

    }|}]
;;
