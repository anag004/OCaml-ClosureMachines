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
| DEF of closure | SEQDEF of closure | PARDEF of closure

exception StackError of string
exception TableError
exception TupleLengthMismatch

val krivine: closure -> stack_token list -> value
