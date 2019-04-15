open A1
exception TypeMismatch
exception ParDefFail
exception Not_implemented

let rec lookupTable g s =
  match g with
    [] -> raise TypeMismatch
  | (s', t) :: g' -> if s = s' then t else lookupTable g' s


let rec single_add g (s, t) =
  match g with
    [] -> [(s, t)]
  | (s1, t1) :: g' -> if (s = s1) then (s, t) :: g' else (s1, t1) :: (single_add g' (s, t))

let rec augment g g_dash =
  match g_dash with
    [] -> g
  | g1 :: g1s -> augment (single_add g g1) g1s


(* Adds two parallel bindings to produce a new one *)
let rec parallelCompose g1 g2 =
  match g1 with
    [] -> g2
  | (s, t) :: g1rem -> (
      try
        if (lookupTable g2 s) = t then parallelCompose g1rem g2 else raise ParDefFail
      with
        TypeMismatch -> (s, t) :: (parallelCompose g1rem g2)
    )

let rec hastype g e t =
  match e with
    Var(s) -> (
      try
        t = (lookupTable g s)
      with TypeMismatch -> false
         | ParDefFail -> false
    )
  | N(_) -> (t = Tint)
  | B(_) -> (t = Tbool)
  | Abs(e1) -> (
      if hastype g e1 Tint then
        (t = Tint)
      else
        false
    )
  | Negative(e1) -> (
      if hastype g e1 Tint then
        t = Tint
      else
        false
    )
  | Not(e1) -> (
      if hastype g e1 Tbool then
        t = Tbool
      else
        false
    )
  | Add(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tint
      else
        false
    )
  | Sub(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tint
      else
        false
    )
  | Mult(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tint
      else
        false
    )
  | Div(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tint
      else
        false
    )
  | Rem(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tint
      else
        false
    )
  | Conjunction(e1, e2) -> (
      if (hastype g e1 Tbool) && (hastype g e2 Tbool) then
        t = Tbool
      else
        false
    )
  | Disjunction(e1, e2) -> (
      if (hastype g e1 Tbool) && (hastype g e2 Tbool) then
        t = Tbool
      else
        false
    )
  | Equals(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tbool
      else
        false
    )
  | GreaterTE(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tbool
      else
        false
    )
  | LessTE(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tbool
      else
        false
    )
  | GreaterT(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tbool
      else
        false
    )
  | LessT(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        t = Tbool
      else
        false
    )
  | InParen(e1) -> hastype g e1 t
  | IfThenElse(e1, e2, e3) -> (
      if hastype g e1 Tbool then
        if (hastype g e2 t) && (hastype g e3 t) then true else false
      else
        false
    )
  | Tuple(n, explist) -> (
      match t with
        Ttuple(typelist) -> (
          if (List.length typelist) = (List.length explist) then
            List.for_all2 (hastype g) explist typelist
          else
            false
        )
      | _ -> false
    )
  | Project((a, b), e1) -> (
      try
        match getType g e1 with
          Ttuple(typelist) -> if (List.length typelist) = b then t = (List.nth typelist (a - 1)) else false
        | _ -> false
      with TypeMismatch -> false
         | ParDefFail -> false
    )
  | Let(d, e) -> (
      try
        hastype (augment g (getBinding g d)) e t
      with TypeMismatch -> false
         | ParDefFail -> false
    )
  | FunctionAbstraction(s, e, t') -> (
      try
        match t with
          Tfunc(t1, t2) -> if t' = t1 then hastype (augment g [(s, t1)]) e t2 else false
        | _ -> false
      with TypeMismatch -> false
         | ParDefFail -> false
    )
  | FunctionCall(e1, e2) -> (
      try
        hastype g e1 (Tfunc((getType g e2), t))
      with TypeMismatch -> false
         | ParDefFail -> false
    )
and getType g e =
  match e with
    Var(s) -> (lookupTable g s)
  | N(_) -> Tint
  | B(_) -> Tbool
  | Abs(e1) -> (
      if hastype g e1 Tint then
        Tint
      else
        raise TypeMismatch
    )
  | Negative(e1) -> (
      if hastype g e1 Tint then
        Tint
      else
        raise TypeMismatch
    )
  | Not(e1) -> (
      if hastype g e1 Tbool then
        Tbool
      else
        raise TypeMismatch
    )
  | Add(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tint
      else
        raise TypeMismatch
    )
  | Sub(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tint
      else
        raise TypeMismatch
    )
  | Mult(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
         Tint
      else
        raise TypeMismatch
    )
  | Div(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tint
      else
        raise TypeMismatch
    )
  | Rem(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tint
      else
        raise TypeMismatch
    )
  | Conjunction(e1, e2) -> (
      if (hastype g e1 Tbool) && (hastype g e2 Tbool) then
        Tbool
      else
        raise TypeMismatch
    )
  | Disjunction(e1, e2) -> (
      if (hastype g e1 Tbool) && (hastype g e2 Tbool) then
        Tbool
      else
        raise TypeMismatch
    )
  | Equals(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tbool
      else
        raise TypeMismatch
    )
  | GreaterTE(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tbool
      else
        raise TypeMismatch
    )
  | LessTE(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tbool
      else
        raise TypeMismatch
    )
  | GreaterT(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tbool
      else
        raise TypeMismatch
    )
  | LessT(e1, e2) -> (
      if (hastype g e1 Tint) && (hastype g e2 Tint) then
        Tbool
      else
        raise TypeMismatch
    )
  | InParen(e1) -> getType g e1
  | IfThenElse(e1, e2, e3) -> (
      if hastype g e1 Tbool then
        let a = getType g e2
        and b = getType g e3
        in
          if a = b then a else raise TypeMismatch
      else
        raise TypeMismatch
    )
  | Tuple(n, explist) -> Ttuple(List.map (getType g) explist)
  | Project((a, b), e1) -> (
      match getType g e1 with
        Ttuple(typelist) -> if (List.length typelist) = b then (List.nth typelist (a - 1)) else raise TypeMismatch
      | _ -> raise TypeMismatch
    )
  | Let(d, e) -> (
      getType (augment g (getBinding g d)) e
    )
  | FunctionAbstraction(s, e, t) -> (
      Tfunc(t, getType (augment g [(s, t)]) e)
    )
  | FunctionCall(e1, e2) -> (
      let a = getType g e1
      and b = getType g e2
      in
      match a with
        Tfunc(t1, t2) -> if t1 = b then t2 else raise TypeMismatch
      | _ -> raise TypeMismatch
    )

and getBinding g d =
  match d with
    Simple(s, e, t) -> (
        let t' = getType g e
        in
        if t = t' then [(s, t)] else raise TypeMismatch
      )
  | Local(d1, d2) -> getBinding (augment g (getBinding g d1)) d2
  | Parallel([d1; d2]) -> parallelCompose (getBinding g d1) (getBinding g d2)
  | Sequence([d1; d2]) -> (
      let g' = getBinding g d1
      in
      augment g' (getBinding (augment g g') d2)
    )

let rec inTable a g =
  match g with
    [] -> false
  | b :: gs -> if a = b then true else inTable a gs

(* Check if a binding is in a table *)
let yields g d g_dash =
  try
    let rec g1 = getBinding g d
    and aux g' =
      match g' with
        [] -> true
      | g's :: g'rem -> if inTable g's g_dash then aux g'rem else false
    in (aux g1)
  with  TypeMismatch -> false
      | ParDefFail -> false
