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
  | Let of definition * exptree
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree
and definition =
  Simple of string * exptree * exptype

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

(* Compile an expression into opcodes *)
let rec compile e = match e with
    Var(s) -> [VAR(s)]
  | N(n) -> [NCONST(n)]
  | B(b) -> [BCONST(b)]
  | Negative(e1) -> (compile e1) @ [NEG]
  | Abs(e1) -> (compile e1) @ [ABS]
  | Add(e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
  | Sub(e1, e2) -> (compile e2) @ (compile e1) @ [MINUS]
  | Mult(e1, e2) -> (compile e1) @ (compile e2) @ [MULT]
  | Div(e1, e2) -> (compile e2) @ (compile e1) @ [DIV]
  | Rem(e1, e2) -> (compile e2) @ (compile e1) @ [REM]
  | Not(e1) -> (compile e1) @ [NOT]
  | Disjunction(e1, e2) -> (compile e1) @ (compile e2) @ [DISJ]
  | Conjunction(e1, e2) -> (compile e1) @ (compile e2) @ [CONJ]
  | Cmp(es) -> (compile es) @ [CMP]
  | Equals(e1, e2) -> (compile e1) @ (compile e2) @ [EQUALS]
  | GreaterT(e1, e2) -> (compile e2) @ (compile e1) @ [GT]
  | GreaterTE(e1, e2) -> (compile e2) @ (compile e1) @ [GEQ]
  | LessT(e1, e2) -> (compile e2) @ (compile e1) @ [LT]
  | LessTE(e1, e2) -> (compile e2) @ (compile e1) @ [LEQ]
  | InParen(es) -> (compile es) @ [CMP]
  | IfThenElse(e1, e2, e3) -> (compile e1) @ [IFTE((compile e2), (compile e3))]
  | Tuple(n, elist) -> (
      let rec aux l acc =
        match l with
          [] -> acc
        | t :: ts -> aux ts ((compile t) @ acc)
      in
        ( aux elist [] ) @ [ TUPLE(n) ]
    )
  | Project((a, b), t) -> ( compile t ) @ [ PROJ(a, b) ]
  | Let(d, e) -> (
      (* Use the principle of correspondence *)
      match d with
        Simple(s, e1, tau) -> [FABS(s, (compile e) @ [RET])] @ (compile e1)  @ [APP]
    )
  | FunctionAbstraction(s, es, t) -> [FABS(s, (compile es) @ [RET])]
  | FunctionCall(e1, e2) -> [FCALL((compile e1), (compile e2))]


type stack_token = VClose of value * table
and table = (string * stack_token) list

exception StackError of string

let rec lookupTable s t = match t with
  [] -> raise TableError
| (s1, x) :: ts -> if s = s1 then x else lookupTable s ts

let rec augment t s x = match t with
  [] -> [(s, x)]
| (s1, y) :: ts -> if s = s1 then (s, x) :: ts else (s1, y) :: (augment ts s x)

let rec secd s e c d = match c with
  [] -> (
      match s with
        [VClose(x, t)] -> x
      | _ -> raise (StackError "SECD expects empty stack at the end")
    )
| NCONST(n) :: c_dash -> secd (VClose(NumVal(n), e) :: s) e c_dash d
| BCONST(b) :: c_dash -> secd (VClose(BoolVal(b), e) :: s) e c_dash d
| VAR(x) :: c_dash -> (
    let el = (lookupTable x e)
    in
    match el with VClose(v, t) -> secd (VClose(v, (augment t x el)) :: s) e c_dash d
  )
| NEG :: c_dash -> (
    match s with
      VClose(NumVal(n), t) :: s_dash -> secd (VClose(NumVal(-n), t) :: s) e c_dash d
    | _ -> raise (StackError "Negative does not find an integer on the stack")
  )
| ABS :: c_dash -> (
    match s with
      VClose(NumVal(n), t) :: s_dash -> secd (VClose(NumVal(abs n), t) :: s) e c_dash d
    | _ -> raise (StackError "Abs does not find an integer on the stack")
  )
| PLUS :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 + n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Add does not find two numbers on the stack")
  )
| MINUS :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 - n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Subtraction does not find two numbers on the stack")
  )
| MULT :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 * n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Multiplication does not find two numbers on the stack")
  )
| DIV :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 / n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Division does not find two numbers on the stack")
  )
| REM :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(NumVal(n1 - n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Remainder does not find two numbers on the stack")
  )
| NOT :: c_dash -> (
    match s with
      VClose(BoolVal(b), t) :: s_dash -> (
        secd (VClose(BoolVal(not b), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Not does not find a number on the stack")
  )
| DISJ :: c_dash -> (
    match s with
      VClose(BoolVal(b1), t1) :: VClose(BoolVal(b2), t2) :: s_dash -> (
          secd (VClose(BoolVal(b1 || b2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Disjunction does not find two Bools on the stack")
  )
| CONJ :: c_dash -> (
    match s with
      VClose(BoolVal(b1), t1) :: VClose(BoolVal(b2), t2) :: s_dash -> (
          secd (VClose(BoolVal(b1 && b2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Conjunction does not find two Bools on the stack")
  )
| CMP :: c_dash -> (
    match s with
      VClose(NumVal(n), t) :: s_dash -> (
        secd (VClose(BoolVal(n > 0), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Compare does not find a number on the stack")
  )
| GT :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 > n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Greater than does not find two numbers on the stack")
  )
| GEQ :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 >= n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Greater than or equal to does not find two numbers on the stack")
  )
| LT :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 < n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Less than does not find two numbers on the stack")
  )
| LEQ :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 <= n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Less than or equal to does not find two numbers on the stack")
  )
| EQUALS :: c_dash -> (
    match s with
      VClose(NumVal(n1), t1) :: VClose(NumVal(n2), t2) :: s_dash -> (
          secd (VClose(BoolVal(n1 = n2), e) :: s_dash) e c_dash d
        )
    | _ -> raise (StackError "Equals does not find two numbers on the stack")
  )

| PAREN :: c_dash -> secd s e c d
| IFTE(c1, c2) :: c_dash -> (
    match s with
      VClose(BoolVal(b), t) :: s_dash -> (
          if b then secd s_dash e (c1 @ c_dash) d
          else secd s_dash e (c2 @ c_dash) d
        )
    | _ -> raise (StackError "IFTE does not find a Boolean on the stack")
  )
| FABS(x, c1) :: c_dash -> secd (VClose(FuncVal(x, c1), e) :: s) e c_dash d
| FCALL(c1, c2) :: c_dash -> secd s e (c1 @ c2 @ [APP] @ c_dash) d
| APP :: c_dash -> (
    (* Call the function *)
    match s with
      v :: VClose(FuncVal(x, c_func), e_func) :: s_dash -> (
          secd [] (augment e_func x v) c_func ((s_dash, e, c_dash) :: d)
        )
    | _ -> raise (StackError "Apply does not find a value closure and FunctionVal on the stack")
  )
| TUPLE(n) :: c_dash -> (
    let rec aux m stack acc =
      if m = 0 then (acc, stack)
      else
        match stack with
          VClose(s1, r) :: s_rem -> aux (m-1) s_rem (acc @ [s1])
        | _ -> raise (StackError "Tuple did not find enough numbers on the stack")
    in
    match aux n s [] with
      (t, stack_rem) -> secd (VClose(TupVal(n, t), e) :: stack_rem) e c_dash d
  )
| PROJ(a, b) :: c_dash -> (
    match s with
      VClose(TupVal(n, t), t1) :: s_rem -> (
          if n != b then raise (StackError "Project does not match length")
          else secd (VClose((List.nth t (a-1)), t1) :: s_rem) e c_dash d
        )
      | _ -> raise (StackError "Proj did not find Tuple on the stack")
  )
| [RET] -> (
    (* Remove the value from the stack and dump *)
    match s with
      [VClose(v, t1)] -> (
          match d with
            (s_old, e_old, c_old) :: ds -> secd (VClose(v, e_old) :: s_old) e_old c_old ds
        )
    | _ -> raise (StackError "Stack on return is not of the form [RET]")
  )
| _ -> raise OpcodeListError

(* Factorial Function *)
let fact_prog = FunctionAbstraction("X", IfThenElse(Equals(Var("X"), N(0)), N(1), Mult(Var("X"), FunctionCall(Var("Y"), Sub(Var("X"), N(1))))), Tint) ;;
let fact_opcode = (List.nth (compile fact_prog) 0) ;;
let fact_clos = match fact_opcode with FABS(s, l) -> FuncVal(s, l) ;;
let e = [("Y", VClose(fact_clos, []))] ;;
let c = (compile (FunctionCall(Var("Y"), N(7)))) ;;
match secd [] e c [] with NumVal(i) -> print_endline (string_of_int i) ;;

(* Fibonacci function *)
let fib_prog = FunctionAbstraction("X", IfThenElse(Equals(Var("X"), N(0)), N(1), IfThenElse(Equals(Var("X"), N(1)), N(1), Add(FunctionCall(Var("Y"), Sub(Var("X"), N(2))), FunctionCall(Var("Y"), Sub(Var("X"), N(1)))))), Tint) ;;
let fib_opcode = (List.nth (compile fib_prog) 0) ;;
let fib_clos = match fib_opcode with FABS(s, l) -> FuncVal(s, l) ;;
let e = [("Y", VClose(fib_clos, []))] ;;
let c = (compile (FunctionCall(Var("Y"), N(8)))) ;;
match secd [] e c [] with NumVal(i) -> print_endline (string_of_int i) ;;

(* Fibonacci function *)
let fib_prog = FunctionAbstraction("X", IfThenElse(Equals(Var("X"), N(0)), N(1), IfThenElse(Equals(Var("X"), N(1)), N(1), Add(FunctionCall(Var("Y"), Sub(Var("X"), N(2))), FunctionCall(Var("Y"), Sub(Var("X"), N(1)))))), Tint) ;;
let fib_code = Let(Simple("Y", fib_prog, Tint) , FunctionCall(Var("Y"), N(8))) ;;
let c = (compile fib_code) ;;
match secd [] [] c [] with NumVal(i) -> print_endline (string_of_int i) ;;

(* Project and tuple *)
let tuple_prog = Project((1, 3), Tuple(3, [N(1); N(2); N(3)])) ;;
let tuple_opcode = (compile tuple_prog) ;;
match secd [] [] tuple_opcode [] with NumVal(i) -> print_endline (string_of_int i) ;;
