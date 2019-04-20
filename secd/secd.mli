(* Dummy implementation of A1 *)
(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type exptree =
    Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Sub of exptree * exptree
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
  | Tuple of int * (exptree list)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree

(* opcodes of the SECD (in the same sequence as above) *)
type opcode = VAR of string | NCONST of int | BCONST of bool
| NEG | ABS | PLUS | MINUS | MULT | DIV | REM
| NOT | CONJ | DISJ
| CMP | EQUALS | GT | LT | GEQ | LEQ
| PAREN | IFTE of (opcode list) * (opcode list)
| FABS of string * (opcode list) | FCALL of (opcode list) * (opcode list) | APP | RET
| TUPLE of int | PROJ of int * int

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | FuncVal of string * (opcode list) | TupVal of int * (value list)

exception TableError

exception OpcodeListError

type stack_token = VClose of value * table
and table = (string * stack_token) list

val compile: exptree -> opcode list
val secd: (stack_token list) -> table -> (opcode list) -> (((stack_token list) * (table) * (opcode list)) list) -> value
