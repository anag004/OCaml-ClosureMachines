(* Front-end for this -- type #use this file to see result *)
#directory "_build";; (* Consider this folder when looking for files *)
#load "secd.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "typechecker.cmo";;
open Secd ;;
open Parser ;;
open Lexer ;;
open Typechecker ;;

let constructTree s = Parser.exp_parser Lexer.read (Lexing.from_string s)
let getOpcodes t = (Secd.compile t)
let runMachine s = (Secd.secd [] [] (getOpcodes (constructTree s)) [])
