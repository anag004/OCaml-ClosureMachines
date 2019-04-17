(* Dummy implementation of A1 *)
(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type exptree =
    Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | VarRec of string
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* binary operators on integers *)
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Cmp of exptree
  | Equals of exptree * exptree
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree
and definition =
  Simple of string * exptree * exptype
| Sequence of (definition list)
| Parallel of (definition list)
| Local of definition * definition


type value = NumVal of int | BoolVal of bool | FuncVal of string * exptree | TupleVal of (int * exptree list)

type closure = Clos of exptree * table | VClos of value * table | DefClos of definition * table | VDefClos of table
and table = (string * closure) list

type stack_token =
  ABS | NEG
| DIV of closure | REM of closure | ADD of closure | MULT of closure | SUB of closure
| CONJ of closure | DISJ of closure | NOT
| CMP | EQ of closure | GT of closure | GEQ of closure | LT of closure | LEQ of closure
| IFTE of closure * closure
| PROJ of int * int
| APP of closure
| DEF of closure | SEQDEF of closure | PARDEF of closure * table

exception StackError of string
exception TableError
exception TupleLengthMismatch

let rec lookupTable s t = match t with
  [] -> raise TableError
| (s1, x) :: ts -> if s = s1 then x else lookupTable s ts

let rec augment t s x = match t with
  [] -> [(s, x)]
| (s1, y) :: ts -> if s = s1 then (s, x) :: ts else (s1, y) :: (augment ts s x)

let rec join t1 t2 =
  match t1 with
    [] -> t2
  | (s, x) :: t1_rem -> join t1_rem (augment t2 s x)

let rec krivine e s =
  match e with
    VClos(NumVal(x), t) -> (
      match s with
        [] -> NumVal(x)
      | ABS :: s_rem -> krivine (VClos(NumVal(abs x), t)) s_rem
      | NEG :: s_rem -> krivine (VClos(NumVal((-x)), t)) s_rem
      | ADD(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x + x1), t)) s_rem
      | ADD(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (ADD(VClos(NumVal(x), t)) :: s_rem)
      | MULT(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x * x1), t)) s_rem
      | MULT(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (MULT(VClos(NumVal(x), t)) :: s_rem)
      (* In subtraction, assume that we put the first operand on the stack first *)
      | SUB(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x - x1), t)) s_rem
      | SUB(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (SUB(VClos(NumVal(x), t)) :: s_rem)
      (* In division, assume that we put the first operand on the stack first *)
      | DIV(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x / x1), t)) s_rem
      | DIV(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (DIV(VClos(NumVal(x), t)) :: s_rem)
      (* In remainder, assume that we put the first operand on the stack first *)
      | REM(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(NumVal(x mod x1), t)) s_rem
      | REM(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (REM(VClos(NumVal(x), t)) :: s_rem)
      | EQ(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x = x1), t)) s_rem
      | EQ(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (EQ(VClos(NumVal(x), t)) :: s_rem)
      | CMP :: s_rem -> krivine (VClos(BoolVal(x > 0), t)) s_rem
      (* In compare, assume that we put the first operand on the stack first *)
      | GT(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x > x1), t)) s_rem
      | GT(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (GT(VClos(NumVal(x), t)) :: s_rem)
      | GEQ(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x >= x1), t)) s_rem
      | GEQ(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (GEQ(VClos(NumVal(x), t)) :: s_rem)
      | LT(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x < x1), t)) s_rem
      | LT(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (LT(VClos(NumVal(x), t)) :: s_rem)
      | LEQ(VClos(NumVal(x1), t1)) :: s_rem -> krivine (VClos(BoolVal(x <= x1), t)) s_rem
      | LEQ(Clos(x1, t1)) :: s_rem -> krivine (Clos(x1, t1)) (LEQ(VClos(NumVal(x), t)) :: s_rem)
      | _ -> raise (StackError "Incorrect stack_token for VClos of type int")
    )
  | VClos(BoolVal(x), t) -> (
      match s with
        [] -> BoolVal(x)
      | NOT :: s_rem -> krivine (VClos(BoolVal(not x), t)) s_rem
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
  | VClos(TupleVal(n, xl), t) -> (
      match s with
        [] -> TupleVal(n, xl)
      (* The only other operation on a Tuple can be project *)
      | PROJ(a, b) :: s_rem -> if b = n then krivine (Clos(List.nth xl (a-1), t)) s_rem else raise TupleLengthMismatch
    )
  | Clos(VarRec(x), t) -> (
      let el = (lookupTable x t)
      in
      match el with Clos(c, t1) -> krivine (Clos(c, (augment t1 x el))) s
    )
  | Clos(Var(x), t) -> krivine (lookupTable x t) s
  | Clos(N(x), t) -> krivine (VClos(NumVal(x), t)) s
  | Clos(B(x), t) -> krivine (VClos(BoolVal(x), t)) s
  | Clos(Abs(x), t) -> krivine (Clos(x, t)) (ABS :: s)
  | Clos(Negative(x), t) -> krivine (Clos(x, t)) (NEG :: s)
  | Clos(Add(e1, e2), t) -> krivine (Clos(e2, t)) (ADD(Clos(e1, t)) :: s)
  | Clos(Sub(e1, e2), t) -> krivine (Clos(e2, t)) (SUB(Clos(e1, t)) :: s)
  | Clos(Mult(e1, e2), t) -> krivine (Clos(e2, t)) (MULT(Clos(e1, t)) :: s)
  | Clos(Div(e1, e2), t) -> krivine (Clos(e2, t)) (DIV(Clos(e1, t)) :: s)
  | Clos(Rem(e1, e2), t) -> krivine (Clos(e2, t)) (REM(Clos(e1, t)) :: s)
  | Clos(Not(x), t) -> krivine (Clos(x, t)) (NOT :: s)
  | Clos(Conjunction(e1, e2), t) -> krivine (Clos(e2, t)) (CONJ(Clos(e1, t)) :: s)
  | Clos(Disjunction(e1, e2), t) -> krivine (Clos(e2, t)) (DISJ(Clos(e1, t)) :: s)
  | Clos(Cmp(e1), t) -> krivine (Clos(e1, t)) (CMP :: s)
  | Clos(Equals(e1, e2), t) -> krivine (Clos(e2, t)) (EQ(Clos(e1, t)) :: s)
  | Clos(GreaterT(e1, e2), t) -> krivine (Clos(e2, t)) (GT(Clos(e1, t)) :: s)
  | Clos(GreaterTE(e1, e2), t) -> krivine (Clos(e2, t)) (GEQ(Clos(e1, t)) :: s)
  | Clos(LessT(e1, e2), t) -> krivine (Clos(e2, t)) (LT(Clos(e1, t)) :: s)
  | Clos(LessTE(e1, e2), t) -> krivine (Clos(e2, t)) (LEQ(Clos(e1, t)) :: s)
  | Clos(InParen(e1), t) -> krivine (Clos(e1, t)) s
  | Clos(IfThenElse(e1, e2, e3), t) -> krivine (Clos(e1, t)) (IFTE(Clos(e2, t), Clos(e3, t)) :: s)
  | Clos(Tuple(n, el), t) -> krivine (VClos(TupleVal(n, el), t)) s
  | Clos(Project((a, b), e1), t) -> krivine (Clos(e1, t)) (PROJ(a, b) :: s)
  | Clos(FunctionAbstraction(x, e1, tau), t) -> krivine (VClos(FuncVal(x, e1), t)) s
  | Clos(FunctionCall(e1, e2), t) -> krivine (Clos(e1, t)) (APP(Clos(e2, t)) :: s)
  (* ============ Evaluating definitions =========== *)
  | Clos(Let(d, e1), t) -> krivine (DefClos(d, t)) (DEF(Clos(e1, t)) :: s)
  | DefClos(Simple(x, e1, tau), t) -> krivine (VDefClos(augment t x (Clos(e1, t)))) s
  | DefClos(Sequence([d1; d2]), t) -> krivine (DefClos(d1, t)) (SEQDEF(DefClos(d2, t)) :: s)
  | VDefClos(t1) -> (
      match s with
        DEF(Clos(e, t)) :: s_rem -> krivine (Clos(e, t1)) s_rem
      | SEQDEF(DefClos(d2, t)) :: s_rem -> krivine (DefClos(d2, t1)) (SEQDEF(VDefClos(t1)) :: s_rem)
      | SEQDEF(VDefClos(t)) :: s_rem -> krivine (VDefClos(t1)) s_rem
      | _ -> raise (StackError "Did not find the defintion opcode after defintion")
    )
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

(* Tuples -- Call by Name *)
let e = Clos(Project((2, 3), Tuple(3, [Var "Y"; N(2); N(3)])), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

print_endline "Factorial using simple definition"
(* Definition - Call by name *)
let fact_prog = FunctionAbstraction("X", IfThenElse(Equals(Var("X"), N(0)), N(1), Mult(Var("X"), FunctionCall(VarRec("Y"), Sub(Var("X"), N(1))))), Tint) ;;
let e = Clos(Let(Simple("Y", fact_prog, Tint) ,FunctionCall(VarRec "Y", N(0))), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;

print_endline "Factorial using sequential definitions"
(* Sequential definitions *)
let d = Sequence([Simple("Y", fact_prog, Tint); Simple("Z", N(5), Tint)]) ;;
let e = Clos(Let(d, FunctionCall(VarRec "Y", Var "Z")), []) ;;
match krivine e [] with
  NumVal(x) -> print_endline (string_of_int x)
| BoolVal(x) -> print_endline (string_of_bool x) ;;
