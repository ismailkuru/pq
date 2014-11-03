(* #load "str.cma";; *)

(*

This is a dummy implementation. 

assumptions: Char.code <256

*)

exception Exception of string;;

let letters = "ABCDEFGHIJKLMNOP";; (* 16 chars *)

let string_of_char c = String.make 1 c;;

(* char -> two char string; assumes Char.code <256 *)
let encodech ch = 
  let i = Char.code ch in
  let (h,l) = (i / 16,i mod 16) in
  (string_of_char (String.get letters h))^(string_of_char (String.get letters l));;

(* decode takes a two char string and produces a char? *)
let decodech s = 
  let (h,l) = (s.[0],s.[1]) in
  let (h,l) = (String.index letters h,String.index letters l) in
  let i = (h * 16) + l in
  Char.chr i;;

let rec explode s = if s = "" then [] else (String.sub s 0 1) :: (explode (String.sub s 1 (String.length s - 1)));;

let implode s = String.concat "" s;;

let encode_string s = String.concat "" (List.map (fun s -> encodech (String.get s 0)) (explode s)) ;;

let rec pre_decode_string (r,s) = 
  if s = "" then r 
  else if String.length s = 1 then raise (Exception "pre_decode_string: string has length 1")
  else pre_decode_string(r^(string_of_char (decodech (String.sub s 0 2))), String.sub s 2 (String.length s - 2))
;;

let decode_string s = pre_decode_string("",s);;

let encode_strings ss = encode_string (String.concat "," (List.map encode_string ss));;

let decode_strings s = 
  let s1 = decode_string s in
  let ss1 = Str.split (Str.regexp (Str.quote ",")) s1 in
  let ss2 = List.map decode_string ss1 in
  ss2
;;

(*


let s = encode_strings ["tom\nridge";"\\woz\\n";"ere"];;

let _ = decode_strings s;;


*)
