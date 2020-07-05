# A toy language in OCaml

This is a toy language written from scratch (including lexer, parser) in OCaml. The language runs as a [SECD virtual machine](https://en.wikipedia.org/wiki/SECD_machine) which stands for Stack, Environment, Control and Dump. 

## How to use

1. `cd` into the `secd` directory. 
2. Run `make`. This requires `ocamlbuild`. 
3. Run `ocaml test_secd.ml` to run tests. 

## Features

This is Turing complete language which supports recursion, functions and variables. Here are some representative examples taken from the `test_secd.ml` file.

1. Calculate GCD recursively

```
let def Gcd:(Tint*Tint)->Tint = \\X:(Tint * Tint).(
   if (proj(1,2)X) < (proj(2,2)X) then (Gcd(((proj(2,2)X), (proj(1,2)X))))
   else
    if ((proj(1,2)X) mod (proj(2,2)X)) = 0 then (proj(2,2)X)
    else (Gcd(((proj(2,2)X), (proj(1,2)X) mod (proj(2,2)X)))) fi
   fi
) in (Gcd((91, 49))) end
```

2. Recursive `O(logn)` power function

```
let def Power:(Tint * Tint) -> Tint =
  \\X:(Tint * Tint).(

      if (proj(2,2)X) = 0 then 1
      else
        if ((proj(2,2)X) mod 2) = 0 then
          let def C:Tint = (Power(((proj(1,2)X), (proj(2,2)X) div 2))) in C*C end
        else
          let def C:Tint = (Power(((proj(1,2)X), (proj(2,2)X) div 2))) in C*C*(proj(1,2)X) end
        fi
      fi

  )
in
  Power((3, 7))
end
```
