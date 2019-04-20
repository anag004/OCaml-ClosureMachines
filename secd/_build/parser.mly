%{
    open Secd
%}

/* DESIGN CHOICE: A function abstraction, and simple definition declaration must always specify a type along with them */

/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF LOCAL TUNIT TBOOL TINT COLON EOF
%start def_parser exp_parser type_parser
%type <Secd.definition> def_parser /* Returns definitions */
%type <Secd.exptree> exp_parser /* Returns expression */
%type <Secd.exptype> type_parser
%%

type_parser:
  type_expr EOF             {$1}
;

exp_parser:
    expr0 EOF                 {$1}
;

def_parser:
    def EOF                   {$1}
;

expr0:
    expr1                     {$1}
|   expr0 DISJ expr1          {Disjunction($1, $3)}
;

expr1:
    expr2                     {$1}
|   expr1 CONJ expr2          {Conjunction($1, $3)}
;

expr2:
    expr3                     {$1}
|   NOT expr3                 {Not($2)}
;

expr3:
    expr4                     {$1}
|   expr3 GT expr4            {GreaterT($1, $3)}
|   expr3 LT expr4            {LessT($1, $3)}
|   expr3 GT EQ expr4         {GreaterTE($1, $4)}
|   expr3 LT EQ expr4         {LessTE($1, $4)}
|   expr3 EQ expr4            {Equals($1, $3)}
;

expr4:
    expr5                     {$1}
|   expr4 PLUS expr5          {Add($1, $3)}
|   expr4 MINUS expr5         {Sub($1, $3)}
;

expr5:
    expr6                     {$1}
|   expr5 DIV expr6           {Div($1, $3)}
|   expr5 REM expr6           {Rem($1, $3)}
|   expr5 TIMES expr6         {Mult($1, $3)}
;

expr6:
    expr7                     {$1}
|   ABS expr7                 {Abs($2)}
;

expr7:
    expr8                     {$1}
|   TILDA expr8               {Negative($2)}
;

expr8:
    expr9                               {$1}
|   PROJ LP INT COMMA INT RP expr8      {Project(($3, $5), $7)}
;

expr9:
    expr10                          {$1}
|   function_call                   {$1}

;

expr10:
    INT                             {N($1)}
|   BOOL                            {B($1)}
|   ID                              {Var($1)}
|   ntuple                          {$1}
|   LP expr0 RP                     {InParen($2)}
|   LET def IN expr0 END            {Let($2, $4)}
|   function_abstraction            {$1}
|   IF expr0 THEN expr0 ELSE expr0 FI   {IfThenElse($2, $4, $6)}
;

ntuple:
    LP expr0 COMMA tuple_tail           {Tuple(1+(List.length $4), ($2::$4))}
;

tuple_tail:
    expr0 RP                     {[$1]}
|   expr0 COMMA tuple_tail       {$1::$3}
;

/* Definitions grammar ================================================= */

def:
  term_def                               {$1}
;


term_def:
  simple_def              {$1}
;

simple_def:
| DEF ID COLON type_expr EQ expr0   {Simple($2, $6, $4)}
;


/* Functions grammar ============================================== */


function_abstraction:
  BACKSLASH ID COLON type_expr DOT expr10   {FunctionAbstraction($2, $6, $4)}
;

function_call:
  expr10 LP expr0 RP                        {FunctionCall($1, $3)}
;

type_expr:
  basic_type_expr           {$1}
| func_type_expr            {$1}
;

func_type_expr:
  basic_type_expr MINUS GT basic_type_expr  {Tfunc($1, $4)}
;

basic_type_expr:
  TUNIT                               {Tunit}
| TINT                                {Tint}
| TBOOL                               {Tbool}
| LP func_type_expr RP                {$2}
| LP basic_type_expr RP               {$2}
| LP type_expr TIMES tail_type_expr   {Ttuple($2::$4)}
;

tail_type_expr:
  type_expr RP                    {[$1]}
| type_expr TIMES tail_type_expr  {$1::$3}
;
