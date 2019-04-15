(* ===== Definitions ================================== *)

type bigint = sign * int list
and sign = Neg | NonNeg

(* ===== Presentation and Conversion functions ================================== *)

(* Converts an integer into a list *)
let rec mk_big_helper x l =
  if x = 0 then l
  else mk_big_helper (x / 10) ((x mod 10) :: l)
(* val mk_big_helper : int -> int list -> int list = <fun> *)

(* Make a bigint from a given integer *)
let mk_big x =
  if x >= 0 then (NonNeg, (mk_big_helper x []))
  else (Neg, (mk_big_helper (-x) []))
(* val mk_big : int -> bigint = <fun> *)

exception LeadingZeroException (* Raised when BigInt contains a leading zero *)

(* Prints a list in order *)
let rec print_num_helper l s =
    match l with
      [] -> s
    | x::xs -> print_num_helper xs (s ^ (string_of_int x))

(* print_num converts a biginteger to a string *)
let print_num b =
  match b with
    (NonNeg, []) -> "0"
  | (NonNeg, l) -> print_num_helper l ""
  | (Neg, l) -> print_num_helper l "-"
(* val print_num : bigint -> string = <fun> *)

(* Returns an integer from an int-list, used for debugging *)
let rec mk_int_helper l1 acc =
  match l1 with
    [] -> acc
  | x::xs -> mk_int_helper xs (acc*10 + x)

(* Returns a integer from a biginteger *)
let mk_int b1 =
  match b1 with
    (NonNeg, l1) -> (
      match l1 with
        0::xs -> raise LeadingZeroException
      | _ -> mk_int_helper l1 0
    )
  | (Neg, l1) -> (
      match l1 with
        0::xs -> raise LeadingZeroException
      | _ -> -(mk_int_helper l1 0)

    )
(* val mk_int : bigint -> int = <fun> *)

(* ===== Comparison functions ================================== *)

(* Returns if two BigInts are equal *)
let eq b1 b2 =
  match b1 with
    (s1, l1) ->
      match b2 with
        (s2, l2) ->
          (s1 = s2) && (l1 = l2)

(* Returns if b1 > b2 *)
let gt b1 b2 =
  let aux l1 l2 =
    if (List.length l1) != (List.length l2) then (List.length l1) > (List.length l2)
    else (l1 > l2)
in
  match b1 with
    (NonNeg, l1) -> (
      match b2 with
        (NonNeg, l2) -> aux l1 l2
      | (Neg, l2) -> true
    )
  | (Neg, l1) -> (
      match b2 with
        (NonNeg, l2) -> false
      | (Neg, l2) -> aux l2 l1
  )

(* Returns if b1 < b2. Equivalent to b2 > b1 *)
let lt b1 b2 = gt b2 b1
(* val lt : bigint -> bigint -> bool = <fun> *)

(* Returns if b1 <= b2. Equivalent to not b1 > b2 *)
let leq b1 b2 = not (gt b1 b2)
(* val leq : bigint -> bigint -> bool = <fun> *)

(* Returns if b1 >= b2. Equivalent to b2 <= b1 *)
let geq b1 b2 = leq b2 b1
(* val geq : bigint -> bigint -> bool = <fun> *)

(* Returns the absolute value *)
let abs b = match b with
  (_, l) -> (NonNeg, l)
(* val abs : bigint -> bigint = <fun> *)

(* ===== Helper functions internal to this module ================================== *)

(* Removes leading zeros from a list *)
let rec remzeros l =
  match l with
    [] -> []
  | 0 :: xs -> remzeros xs
  | _ -> l

exception InsufficientLengthException

(* Bites of a list of length n from the front of list l and returns the two pieces *)
let rec biteoff l n =
  match l with
    [] -> if n = 0 then ([], []) else raise InsufficientLengthException
  | x::xs -> if n = 0 then ([], l)
      else (
        match biteoff xs (n-1) with
          (l1, l2) -> (x :: l1, l2)
      )
(* val biteoff : 'a list -> int -> 'a list * 'a list = <fun> *)

(* Makes l1, l2 equal by adding leading zeros *)
let rec mk_equal l1 l2 =
  if List.length l1 = List.length l2 then (l1, l2)
  else if List.length l1 > List.length l2 then mk_equal l1 (0 :: l2)
  else mk_equal (0 :: l1) l2
(* val mk_equal : int list -> int list -> int list * int list = <fun> *)

(* ===== Addition and subtraction functions ================================== *)

(* Full adder for decimal numbers *)
let saddc a b c =
  if  a + b + c < 10 then (a + b + c, 0)
  else ( (a + b + c) mod 10, (a + b + c)/10)
(* val saddc : int -> int -> int -> int * int = <fun> *)

(* Raised when arguments to addition_helper and subtraction_helper are mismatched.
   Indicates logical flaw. *)
exception SizeMismatch

(* Adds two int lists with the same size.
   Returns an overflow value and an answer of the same size *)
let rec addition_helper (l1, l2) =
  (* Check if lists are the same length *)
  if List.length l1 != List.length l2 then raise SizeMismatch
  else
    match l1 with
      [] -> ([], 0) (* If l1 is empty, l2 is empty too since both have same size *)
    | x::xs ->
      match l2 with
        [] -> raise SizeMismatch
      | y::ys ->
        (* Do addition on the remaining lists *)
        match addition_helper (xs, ys) with
          (l, c) ->
            (* Add the single digits and get the carry *)
            match saddc x y c with
              (* Answer is the result cons l along with carry *)
              (a, b) -> (a::l, b)
(* val addition_helper : int list * int list -> int list * int = <fun> *)

(* Wrapper function which handles overflow from addition_helper *)
let addition_wrapper l1 l2 =
  match (addition_helper (mk_equal l1 l2)) with
    (* If no overflow, simply return the list *)
    (l, 0) -> l
    (* If there is overflow, cons it to the front of the list *)
  | (l, x) -> x :: l
(* val addition_wrapper : int list -> int list -> int list = <fun> *)

(* Single-digit subtraction with carry *)
let ssubc a b c =
  if a - b - c >= 0 then (a - b - c, 0)
  (* The only value of carry can be 1 here *)
  else (10 + a - b - c, 1)
(* val ssubc : int -> int -> int -> int * int = <fun> *)

exception ListEmptyException (* Raised in the subtraction_wrapper function, indicates a logical error *)
exception WrapperException (* Indicates something wrong with the wrapper functions *)

(* Subtracts two lists and returns result with carry *)
let rec subtraction_helper (l1, l2) =
match l2 with
  [] -> ([], 0) (* l1 is empty too since both have same size *)
| x::xs ->
  match l1 with
    [] -> raise SizeMismatch
  | y::ys ->
    (* Do subtraction on the remaining list *)
    match subtraction_helper (ys, xs) with
      (l, c) ->
        (* Do single digit subtraction *)
        match ssubc y x c with
          (a, b) -> (a :: l, b)
(* val subtraction_helper : int list * int list -> int list * int = <fun> *)

(* A subtraction wrapper function which handles overflow *)
let subtraction_wrapper l1 l2 =
  match subtraction_helper (mk_equal l1 l2) with
    (* If no overflow, simply return *)
    (l, 0) -> (0, remzeros l)
    (* Handle overflow here *)
  | (l, x) ->
      match (mk_equal [] l) with
        (l1, _) -> match subtraction_helper (x :: l1, 0 :: l) with
          (x::res, _) -> (1, remzeros res)
        | ([] , _) -> raise ListEmptyException

(* Unary negation *)
let minus b =
  match b with
    (NonNeg, []) -> b (* The empty string is always non-negative *)
  | (NonNeg, l) -> (Neg, l)
  | (Neg, l) -> (NonNeg, l)
(* val negation : bigint -> bigint = <fun> *)

(* The main addition/subtraction functions *)
let rec add b1 b2 =
  (* Compare signs of b1, b2 *)
  match b1 with
    (NonNeg, l1) -> (match b2 with
        (NonNeg, l2) -> (NonNeg, remzeros (addition_wrapper l1 l2))
      | (Neg, l2) -> (sub b1 ((NonNeg, l2))))
    | (Neg, l1) -> (match b2 with
        (NonNeg, l2) -> (sub b2 ((NonNeg, l1)))
      | (Neg, l2) -> (Neg, remzeros (addition_wrapper l1 l2)))
and sub b1 b2 =
  match b1 with
    (NonNeg, l1) ->
      (match b2 with
        (NonNeg, l2) ->
          (match subtraction_wrapper l1 l2 with
            (0, l) -> (NonNeg, l)
          | (1, l) -> (Neg, l)
          | (_, l) -> raise WrapperException)
      | (Neg, l2) ->
          add b1 (NonNeg, l2))
  | (Neg, l1) ->
      (match b2 with
        (NonNeg, l2) -> add b1 (Neg, l2)
      | (Neg, l2) ->
        (match subtraction_wrapper l1 l2 with
          (0, l) -> (Neg, l)
        | (1, l) -> (NonNeg, l)
        | (_, l) -> raise WrapperException))
(* val addition : bigint -> bigint -> bigint = <fun>
val subtraction : bigint -> bigint -> bigint = <fun> *)

(* ===== Multiplication functions ================================== *)

(* Single digit multiplication *)
let smult l n =
  let rec smult_helper l n =
    match l with
      [] -> ([], 0)
    | x::xs ->
      match (smult_helper xs n ) with
        (l1, c1) ->
          match saddc (x*n) c1 0 with
            (res, c2) -> (res::l1, c2)
  in
    match smult_helper l n with
      (l, 0) -> l
    | (l, c) -> c :: l
(* val smult : int list -> int -> bool -> int list = <fun> *)

(* Multiplies two lists together *)
(* Optimize the append command *)
let rec mult_wrapper l1 l2 acc =
  match l2 with
    [] -> acc
  | x::xs -> mult_wrapper l1 xs (addition_wrapper (acc @ [0]) (smult l1 x))
(* val mult_wrapper : int list -> int list -> int list -> int list = <fun> *)

(* Function to multiply bigints together *)
let mult b1 b2 =
  match b1 with
    (NonNeg, l1) -> (
      match b2 with
        (NonNeg, l2) -> (NonNeg, remzeros (mult_wrapper l1 l2 []))
      | (Neg, l2) -> (
          match remzeros (mult_wrapper l1 l2 []) with
            [] -> (NonNeg, [])
          | l -> (Neg, l)
        )
    )
  | (Neg, l1) -> (
      match b2 with
        (NonNeg, l2) -> (Neg, remzeros (mult_wrapper l1 l2 []))
      | (Neg, l2) -> (NonNeg, remzeros (mult_wrapper l1 l2 []))
    )
(* val mult : bigint -> bigint -> bigint = <fun> *)

(* ===== (Long) Division functions ================================== *)

exception SubtractionHelperOutputException (* Raised in div_helper indicates logical error *)

(* Carries out one step of the division, takes in two lists *)
let div_helper (l1, l2) =
  (* Aux takes an accumulator q *)
  let rec aux (l1, l2, q) =
    match subtraction_helper (l1, l2) with
      (l, 0) -> aux (l, l2, (q+1))
    | (_, 1) -> (q, l1)
    | (_, _) -> raise SubtractionHelperOutputException
  in
    if (List.length l1 != List.length l2) then raise SizeMismatch
    else aux (l1, l2, 0)

(* Divides two lists representing positive numbers, returns remainder and quotient *)
let rec division_wrapper l1 l2 acc_q =
  if List.length l2 > List.length l1 then (remzeros acc_q, remzeros l1)
  else
    match biteoff l1 (List.length l2) with
      (t1, t2) ->
        match div_helper (t1, l2) with
          (q, rem) ->
            (* Call division_wrapper recursively *)
            division_wrapper (rem @ t2) (0 :: l2) (acc_q @ [q])
(* val division_wrapper : int list -> int list -> int list -> int list * int list = <fun> *)

(* Assigns signs after dividing and returns BigInts.
   The convention followed is remainder has same sign as dividend and |remainder| < |divisor| *)
let assign_signs l1 l2 sq srem =
  match division_wrapper (remzeros l1) (remzeros l2) [] with
    (q, rem) ->
      if q = [] && rem = [] then ((NonNeg, []), (NonNeg, []))
      else if q = [] then ((NonNeg, []), (srem, remzeros rem))
      else if rem = [] then ((sq, remzeros q), (NonNeg, []))
      else  ((sq, remzeros q), (srem, remzeros rem))
(* val assign_signs : int list -> int list -> sign -> sign -> bigint * bigint = <fun> *)

exception DivideByZeroException

(* Divides two bigints, returns quotient and remainder as a BigInt tuple *)
let divq b1 b2 =
  if (eq b2 ((NonNeg, []))) then raise DivideByZeroException
  else (
    match b1 with
      (NonNeg, l1) -> (
        match b2 with
          (NonNeg, l2) -> assign_signs l1 l2 NonNeg NonNeg
        | (Neg, l2) -> assign_signs l1 l2 Neg NonNeg
      )
    | (Neg, l1) -> (
        match b2 with
          (NonNeg, l2) -> assign_signs l1 l2 Neg Neg
        | (Neg, l2) -> assign_signs l1 l2 NonNeg Neg
      )
  )
(* val division : bigint -> bigint -> bigint * bigint = <fun> *)

(* Returns the remainder on dividing *)
let rem b1 b2 =
  match divq b1 b2 with
    (q, rem) -> rem
(* val rem: bigint -> bigint -> bigint *)

(* Returns the quotient on dividing *)
let div b1 b2 =
  match divq b1 b2 with
    (q, rem) -> q
(* val div: bigint -> bigint -> bigint *)
