(* Dummy implementation of A1 *)
(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type exptree =
    Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | VarRec of string
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

val krivine: closure -> stack_token list -> value
