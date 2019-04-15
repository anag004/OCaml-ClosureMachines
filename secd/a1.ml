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
type opcode = VAR of string | NCONST of int | BCONST of bool | PLUS | MULT | CONJ | DISJ | CMP | PAREN | IFTE of (opcode list) * (opcode list)
| FABS of string * (opcode list) | FCALL of (opcode list) * (opcode list) | APP | RET

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | FABS of string * (opcode list)

exception TableError

exception OpcodeListError

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
| IfThenElse(e1, e2, e3) -> (compile e1) @ [IFTE((compile e2), (compile e3))]
| FunctionAbstraction(s, es, t) -> [FABS(s, (compile es) @ [RET])]
| FunctionCall(e1, e2) -> [FCALL((compile e1), (compile e2))]

type stack_token = VClose of value * table
and table = (string * stack_token) list

exception StackError

let rec lookupTable s t = match t with
  [] -> raise TableError
| (s, x) :: ts -> x
| _ :: ts -> lookupTable s ts

let rec augment t s x = match t with
  [] -> [(s, x)]
| (s, _) :: ts -> (s, x) :: ts
| a :: ts -> a :: (augment ts s x)

let rec secd s e c d = match c with
  [] -> (
      match s with
        [VClose(x, t)] -> x
      | _ -> raise StackError
    )
| NCONST(n) :: c_dash -> secd (VClose(NumVal(n), e) :: s) e c_dash d
| BCONST(b) :: c_dash -> secd (VClose(BoolVal(b), e) :: s) e c_dash d
| VAR(x) :: c_dash -> secd ((lookupTable x e) :: s) e c_dash d
| PLUS :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 + n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise StackError
  )
| MULT :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 * n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise StackError
  )
| DISJ :: c_dash -> (
    match s with
      VClose(BoolVal(b1), t1) :: VClose(BoolVal(b2), t2) :: s_dash -> (
          secd (VClose(BoolVal(b1 || b2), e) :: s_dash) e c_dash d
        )
    | _ -> raise StackError
  )
| CONJ :: c_dash -> (
    match s with
      VClose(BoolVal(b1), t1) :: VClose(BoolVal(b2), t2) :: s_dash -> (
          secd (VClose(BoolVal(b1 && b2), e) :: s_dash) e c_dash d
        )
    | _ -> raise StackError
  )
| CMP :: c_dash -> (
      match s with
        VClose(NumVal(n), t) :: s_dash -> (
          secd (VClose(BoolVal(n > 0), e) :: s_dash) e c_dash d
          )
      | _ -> raise StackError
  )
| PAREN :: c_dash -> secd s e c d
| IFTE(c1, c2) :: c_dash -> (
    match s with
      VClose(BoolVal(b), t) :: s_dash -> (
          if b then secd s_dash e (c1 @ c_dash) d
          else secd s_dash e (c2 @ c_dash) d
        )
    | _ -> raise StackError
  )
| FABS(x, c1) :: c_dash -> secd (VClose(FABS(x, c1), e) :: s) e c_dash d
| FCALL(c1, c2) :: c_dash -> secd s e (c1 @ c2 @ [APP] @ c_dash) d
| APP :: c_dash -> (
    (* Call the function *)
    match s with
      v :: VClose(FABS(x, c_func), e_func) :: s_dash -> (
          secd [] (augment e_func x v) c_func ((s_dash, e, c_dash) :: d)
        )
    | _ -> raise StackError
  )
| [RET] -> (
    (* Remove the value from the stack and dump *)
    match s with
      [VClose(v, t1)] -> (
          match d with
            (s_old, e_old, c_old) :: ds -> secd (VClose(v, e_old) :: s_old) e_old c_old ds
        )
    | _ -> raise StackError
  )
| _ -> raise OpcodeListError
