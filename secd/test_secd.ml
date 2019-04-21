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

let constructTree s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;
let getOpcodes t = (Secd.compile t) ;;
let runMachine s =
  let t = (constructTree s)
  in
  let tau = getType [] t
  in
  Secd.secd [] [] (getOpcodes t) [] ;;

(* Euler's GCD function *)
runMachine "let def Gcd:(Tint*Tint)->Tint = \\X:(Tint * Tint).(
   if (proj(1,2)X) < (proj(2,2)X) then (Gcd(((proj(2,2)X), (proj(1,2)X))))
   else
    if ((proj(1,2)X) mod (proj(2,2)X)) = 0 then (proj(2,2)X)
    else (Gcd(((proj(2,2)X), (proj(1,2)X) mod (proj(2,2)X)))) fi
   fi
) in (Gcd((91, 49))) end" ;;


(* Power function - O(n) *)
runMachine "
let def Power:(Tint*Tint)->Tint = \\X:(Tint*Tint).(
if proj(2, 2)X = 0 then 1 else (proj(1, 2)X)*(Power((proj(1, 2)X, (proj(2, 2)X)-1))) fi
) in (Power((4, 3))) end" ;;

(* Efficient power function - O(logN)  *)
runMachine "
let def Power:(Tint * Tint) -> Tint =
  \\X:(Tint * Tint).(

      if (proj(2,2)X) = 0 then 1
      else
        if ((proj(2,2)X) mod 2) = 0 then
          let def C:Tint = (Power(((proj(1,2)X), (proj(2,2)X) div 2))) in C*C end
        else
          let def C:Tint = (Power(((proj(1,2)X), (proj(2,2)X) div 2))) in C*C*(proj(1,2)X) end
        fi
      fi

  )
in
  Power((3, 7))
end
";;
