(* Dummy implementation of A1 *)
(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type  exptree =
    Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Mult of exptree * exptree        (* Multiplication * *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Cmp of exptree
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree

(* opcodes of the SECD (in the same sequence as above) *)
type opcode = VAR of string | NCONST of int | BCONST of bool | PLUS | MULT | CONJ | DISJ | CMP | PAREN | IFTE
| FABS of string * (opcode list) | FCALL of (opcode list) * (opcode list) | APP | RET

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | FABS of string * (opcode list)

(* Compile an expression into opcodes *)
let rec compile e = match e with
  Var(s) -> [VAR(s)]
| N(n) -> [NCONST(n)]
| B(b) -> [BCONST(b)]
| Add(e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
| Mult(e1, e2) -> (compile e1) @ (compile e2) @ [MULT]
| Disjunction(e1, e2) -> (compile e1) @ (compile e2) @ [DISJ]
| Conjunction(e1, e2) -> (compile e1) @ (compile e2) @ [CONJ]
| Cmp(es) -> (compile es) @ [CMP]
| InParen(es) -> (compile es) @ [CMP]
| IfThenElse(e1, e2, e3) -> (compile e3) @ (compile e2) @ (compile e1) @ [IFTE]
| FunctionAbstraction(s, es, t) -> [FABS(s, (compile es) @ [RET])]
| FunctionCall(e1, e2) -> [FCALL((compile e1), (compile e2))]

type stack_token = VClose of value * table
and table = (string * stack_token) list
