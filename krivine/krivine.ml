(* Dummy implementation of A1 *)
(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type exptree =
    Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Sub of exptree * exptree
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Cmp of exptree
  | Equals of exptree * exptree
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree

type value = NumVal of int | BoolVal of bool | FuncVal of string * exptree

type closure = Clos of exptree * table | VClos of value * table
and table = (string * closure) list

type stack_token = ADD of closure | MULT of closure | SUB of closure | CONJ of closure | DISJ of closure
| CMP | EQ of closure | IFTE of closure * closure | APP of closure

exception StackError of string
exception TableError

let rec lookupTable s t = match t with
  [] -> raise TableError
| (s1, x) :: ts -> if s = s1 then x else lookupTable s ts

let rec augment t s x = match t with
  [] -> [(s, x)]
| (s1, y) :: ts -> if s = s1 then (s, x) :: ts else (s1, y) :: (augment ts s x)

let rec krivine e s =
  match e with
    VClos(NumVal(x), t) -> (
      match s with
        [] -> NumVal(x)
      | ADD(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x + x1), t)) s_rem
      | ADD(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (ADD(VClos(NumVal(x), t)) :: s_rem)
      | MULT(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x * x1), t)) s_rem
      | MULT(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (MULT(VClos(NumVal(x), t)) :: s_rem)
      (* In subtraction, assume that we put the first operand on the stack first *)
      | SUB(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x - x1), t)) s_rem
      | SUB(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (SUB(VClos(NumVal(x), t)) :: s_rem)
      | EQ(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x = x1), t)) s_rem
      | EQ(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (EQ(VClos(NumVal(x), t)) :: s_rem)
      | CMP :: s_rem -> krivine (VClos(BoolVal(x > 0), t)) s_rem
      | _ -> raise (StackError "Incorrect stack_token for VClos of type int")
    )
  | VClos(BoolVal(x), t) -> (
      match s with
        [] -> BoolVal(x)
      | CONJ(VClos(BoolVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x && x1), t)) s_rem
      | CONJ(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (CONJ(VClos(BoolVal(x), t)) :: s_rem)
      | DISJ(VClos(BoolVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x || x1), t)) s_rem
      | DISJ(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (DISJ(VClos(BoolVal(x), t)) :: s_rem)
      | IFTE(c1, c2) :: s_rem -> if x then krivine c1 s_rem else krivine c2 s_rem
      | _ -> raise (StackError "Incorrect stack_token for VClos of type bool")
    )
  | VClos(FuncVal(x, et), t) -> (
      match s with
        [] -> FuncVal(x, et)
      | APP(c) :: s_rem -> krivine (Clos(et, (augment t x c))) s_rem
      | _ -> raise (StackError "Incorrent stack_token for VClos of type func")
    )
  | Clos(Var(x), t) -> (
      let el = (lookupTable x t)
      in
      match el with Clos(c, t1) -> krivine (Clos(c, (augment t1 x el))) s
    )
  | Clos(N(x), t) -> krivine (VClos(NumVal(x), t)) s
  | Clos(B(x), t) -> krivine (VClos(BoolVal(x), t)) s
  | Clos(Add(e1, e2), t) -> krivine (Clos(e2, t)) (ADD(Clos(e1, t)) :: s)
  | Clos(Sub(e1, e2), t) -> krivine (Clos(e2, t)) (SUB(Clos(e1, t)) :: s)
  | Clos(Mult(e1, e2), t) -> krivine (Clos(e2, t)) (MULT(Clos(e1, t)) :: s)
  | Clos(Conjunction(e1, e2), t) -> krivine (Clos(e2, t)) (CONJ(Clos(e1, t)) :: s)
  | Clos(Disjunction(e1, e2), t) -> krivine (Clos(e2, t)) (DISJ(Clos(e1, t)) :: s)
  | Clos(Cmp(e1), t) -> krivine (Clos(e1, t)) (CMP :: s)
  | Clos(Equals(e1, e2), t) -> krivine (Clos(e2, t)) (EQ(Clos(e1, t)) :: s)
  | Clos(InParen(e1), t) -> krivine (Clos(e1, t)) s
  | Clos(IfThenElse(e1, e2, e3), t) -> krivine (Clos(e1, t)) (IFTE(Clos(e2, t), Clos(e3, t)) :: s)
  | Clos(FunctionAbstraction(x, e1, tau), t) -> krivine (VClos(FuncVal(x, e1), t)) s
  | Clos(FunctionCall(e1, e2), t) -> krivine (Clos(e1, t)) (APP(Clos(e2, t)) :: s)

(* =================== Tests ===================== *)
(* Variable/Integer conversion *)
let t = [("X", Clos(N(1), []))] ;;
let e = Clos(Var "X", t) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* Boolean *)
let t = [("X", Clos(B(false), []))] ;;
let e = Clos(Var "X", t) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* Addition *)
let e = Clos(Add(N(20), N(22)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* Multiplication *)
let e = Clos(Mult(N(6), N(7)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* Subtraction *)
let e = Clos(Sub(N(100), N(58)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* Compare *)
let e = Clos(Cmp(N(42)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* Equals *)
let e = Clos(Equals(N(42), N(42)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* InParen *)
let e = Clos(InParen(N(42)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

(* IfThenElse *)
let e = Clos(IfThenElse(Equals(N(42), N(42)), N(1), N(2)), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;
