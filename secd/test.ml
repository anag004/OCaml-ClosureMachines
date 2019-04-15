#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;

let check_parser s = A3.main A2.read (Lexing.from_string s) ;;

let check_lexer s = A2.read (Lexing.from_string s) ;;
