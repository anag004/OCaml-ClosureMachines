{
  open Parser
  exception Not_implemented
}

(* ====================== Some definitions ================================================ *)
let whitespace = [' ' '\t']+
let digits = ['0'-'9']                                        (* All single digits *)
let nzdigits = ['1'-'9']                                      (* Non-zero digits *)
let intConstant = ('0' | nzdigits digits*)                    (* Regex for non-negative integers  *)

(* ====================== Definition of matching strings for operators ===================== *)
let trueSymbol = 'T'
let frontSlash = '/'
let backSlash = '\\'
let falseSymbol = 'F'
let absOperator = "abs"
let addOperator = '+'
let divOperator = "div"
let subOperator = '-'
let tildeOperatror = '~'
let multOperator = "*"
let modOperator = "mod"
let recKeyword = "rec"
let leftParan = '('
let rightParan = ')'
let notOperator = "not"
let andOperator = "/\\"
let orOperator = "\\/"
let eqOperator = '='
let gtOperator = '>'
let ltOperator = '<'
let ifKeyword = "if"
let fiKeyword = "fi"
let elseKeyword = "else"
let thenKeyword = "then"
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let underscore = '_'
let apostrophe = '\''
let identifier = uppercase (lowercase | uppercase | apostrophe | underscore )*
let defKeyword = "def"
let commaSymbol = ","
let projSymbol = "proj"
let delimiterSymbol = '$'
let semicolonSymbol = ';'
let letKeyword = "let"
let inKeyword = "in"
let endKeyword = "end"
let backslashSymbol = "\\"
let dotSymbol = "."
let defKeyword = "def"
let parallelSymbol = "||"
let intType = "Tint"
let boolType = "Tbool"
let unitType = "Tunit"
let colonSymbol = ":"

rule read = parse
|  whitespace           {read lexbuf}
|  intConstant as n     {INT(int_of_string n)}
|  trueSymbol           {BOOL(true)}
|  falseSymbol          {BOOL(false)}
|  absOperator          {ABS}
|  addOperator          {PLUS}
|  subOperator          {MINUS}
|  multOperator         {TIMES}
|  divOperator          {DIV}
|  modOperator          {REM}
|  leftParan            {LP}
|  rightParan           {RP}
|  notOperator          {NOT}
|  andOperator          {CONJ}
|  orOperator           {DISJ}
|  eqOperator           {EQ}
|  gtOperator           {GT}
|  ltOperator           {LT}
|  ifKeyword            {IF}
|  fiKeyword            {FI}
|  thenKeyword          {THEN}
|  elseKeyword          {ELSE}
|  intType              {TINT}
|  boolType             {TBOOL}
|  unitType             {TUNIT}
|  identifier as s      {ID(s)}
|  tildeOperatror       {TILDA}
|  commaSymbol          {COMMA}
|  projSymbol           {PROJ}
|  letKeyword           {LET}
|  inKeyword            {IN}
|  endKeyword           {END}
|  backslashSymbol      {BACKSLASH}
|  dotSymbol            {DOT}
|  defKeyword           {DEF}
|  eof                  {EOF}
|  colonSymbol          {COLON}
| _                     {raise Not_implemented}
