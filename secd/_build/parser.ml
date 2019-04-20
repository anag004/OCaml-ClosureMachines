type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | ABS
  | TILDA
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | COMMA
  | PROJ
  | LET
  | IN
  | END
  | BACKSLASH
  | DOT
  | DEF
  | LOCAL
  | TUNIT
  | TBOOL
  | TINT
  | COLON
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Secd
# 44 "parser.ml"
let yytransl_const = [|
  260 (* ABS *);
  261 (* TILDA *);
  262 (* NOT *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* REM *);
  268 (* CONJ *);
  269 (* DISJ *);
  270 (* EQ *);
  271 (* GT *);
  272 (* LT *);
  273 (* LP *);
  274 (* RP *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* FI *);
  279 (* COMMA *);
  280 (* PROJ *);
  281 (* LET *);
  282 (* IN *);
  283 (* END *);
  284 (* BACKSLASH *);
  285 (* DOT *);
  286 (* DEF *);
  287 (* LOCAL *);
  288 (* TUNIT *);
  289 (* TBOOL *);
  290 (* TINT *);
  291 (* COLON *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\003\000\002\000\001\000\005\000\005\000\007\000\007\000\008\000\
\008\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\012\000\012\000\
\013\000\013\000\014\000\014\000\015\000\015\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\018\000\020\000\
\020\000\006\000\021\000\022\000\019\000\017\000\004\000\004\000\
\024\000\023\000\023\000\023\000\023\000\023\000\023\000\025\000\
\025\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\003\000\001\000\003\000\001\000\
\002\000\001\000\003\000\003\000\004\000\004\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\002\000\
\001\000\002\000\001\000\007\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\005\000\001\000\007\000\004\000\002\000\
\003\000\001\000\001\000\006\000\006\000\004\000\001\000\001\000\
\004\000\001\000\001\000\001\000\003\000\003\000\004\000\002\000\
\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\058\000\000\000\042\000\
\043\000\031\000\032\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\059\000\000\000\000\000\006\000\
\000\000\000\000\000\000\019\000\023\000\025\000\027\000\000\000\
\030\000\034\000\037\000\000\000\050\000\052\000\051\000\060\000\
\000\000\000\000\048\000\000\000\003\000\024\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\022\000\020\000\
\021\000\000\000\000\000\054\000\053\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\000\000\055\000\049\000\000\000\040\000\000\000\000\000\000\000\
\036\000\000\000\000\000\056\000\000\000\041\000\000\000\000\000\
\045\000\057\000\038\000\028\000"

let yydgoto = "\004\000\
\006\000\021\000\040\000\105\000\096\000\007\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\097\000\008\000\009\000\042\000\043\000\
\106\000"

let yysindex = "\101\000\
\237\254\015\255\024\255\000\000\020\255\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\138\255\025\255\128\255\015\255\
\015\255\018\255\237\254\045\255\000\000\003\000\042\255\000\000\
\120\255\053\255\159\255\000\000\000\000\000\000\000\000\034\255\
\000\000\000\000\000\000\024\255\000\000\000\000\000\000\000\000\
\071\000\074\255\000\000\049\255\000\000\000\000\000\000\120\255\
\072\255\039\255\085\255\061\255\056\255\015\255\000\000\015\255\
\128\255\064\255\096\255\128\255\128\255\128\255\128\255\128\255\
\015\255\084\255\004\255\076\255\000\000\090\255\024\255\000\000\
\015\255\015\255\088\255\015\255\024\255\042\255\000\000\053\255\
\128\255\053\255\128\255\053\255\159\255\159\255\000\000\000\000\
\000\000\057\255\024\255\000\000\000\000\024\255\093\255\104\255\
\000\000\016\255\113\255\253\254\087\255\053\255\053\255\000\000\
\027\255\000\000\000\000\015\255\000\000\015\255\015\255\100\255\
\000\000\148\255\024\255\000\000\106\255\000\000\033\255\025\255\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\195\000\000\000\
\172\000\080\000\030\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\184\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\114\255\116\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\206\000\000\000\096\000\
\000\000\112\000\000\000\128\000\047\000\064\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\144\000\160\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\002\000\254\255\107\000\083\000\082\000\
\127\000\206\255\016\000\115\000\133\000\244\255\000\000\034\000\
\000\000\000\000\000\000\044\000\000\000\000\000\224\255\122\000\
\046\000"

let yytablesize = 489
let yytable = "\022\000\
\047\000\047\000\055\000\067\000\041\000\044\000\080\000\082\000\
\084\000\054\000\005\000\070\000\029\000\049\000\050\000\010\000\
\011\000\012\000\013\000\014\000\015\000\092\000\044\000\113\000\
\045\000\010\000\011\000\012\000\054\000\016\000\102\000\016\000\
\103\000\017\000\051\000\115\000\111\000\066\000\018\000\019\000\
\036\000\016\000\020\000\017\000\116\000\054\000\017\000\053\000\
\018\000\019\000\065\000\054\000\020\000\056\000\123\000\037\000\
\038\000\039\000\074\000\060\000\061\000\107\000\090\000\018\000\
\010\000\011\000\012\000\013\000\014\000\054\000\069\000\098\000\
\095\000\100\000\104\000\085\000\086\000\081\000\101\000\010\000\
\016\000\070\000\017\000\071\000\054\000\075\000\076\000\018\000\
\019\000\072\000\077\000\020\000\091\000\093\000\073\000\015\000\
\010\000\011\000\012\000\013\000\014\000\001\000\002\000\003\000\
\094\000\117\000\108\000\124\000\119\000\083\000\099\000\011\000\
\016\000\112\000\017\000\114\000\054\000\120\000\054\000\018\000\
\019\000\109\000\047\000\020\000\048\000\052\000\110\000\012\000\
\010\000\011\000\012\000\013\000\014\000\057\000\058\000\059\000\
\078\000\079\000\010\000\011\000\012\000\048\000\014\000\013\000\
\016\000\046\000\017\000\121\000\010\000\011\000\012\000\018\000\
\019\000\118\000\016\000\020\000\017\000\068\000\000\000\014\000\
\122\000\018\000\019\000\000\000\016\000\020\000\017\000\062\000\
\063\000\064\000\000\000\008\000\019\000\000\000\000\000\020\000\
\087\000\088\000\089\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\000\000\000\000\000\000\000\000\047\000\054\000\
\000\000\000\000\047\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\047\000\029\000\044\000\
\029\000\029\000\029\000\029\000\016\000\016\000\029\000\029\000\
\000\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
\000\000\016\000\016\000\016\000\016\000\017\000\017\000\016\000\
\016\000\000\000\017\000\017\000\017\000\017\000\017\000\000\000\
\017\000\000\000\017\000\017\000\017\000\017\000\018\000\018\000\
\017\000\017\000\000\000\018\000\018\000\018\000\018\000\018\000\
\000\000\018\000\000\000\018\000\018\000\018\000\018\000\000\000\
\000\000\018\000\018\000\010\000\010\000\010\000\010\000\010\000\
\000\000\010\000\000\000\010\000\010\000\010\000\010\000\000\000\
\000\000\010\000\010\000\015\000\015\000\015\000\015\000\015\000\
\000\000\015\000\000\000\015\000\015\000\015\000\015\000\000\000\
\000\000\015\000\015\000\011\000\011\000\011\000\011\000\011\000\
\000\000\011\000\000\000\011\000\011\000\011\000\011\000\000\000\
\000\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\000\000\012\000\000\000\012\000\012\000\012\000\012\000\000\000\
\000\000\012\000\012\000\013\000\013\000\013\000\013\000\013\000\
\000\000\013\000\000\000\013\000\013\000\013\000\013\000\000\000\
\000\000\013\000\013\000\014\000\014\000\014\000\014\000\014\000\
\000\000\014\000\000\000\014\000\014\000\014\000\014\000\008\000\
\008\000\014\000\014\000\000\000\000\000\008\000\000\000\008\000\
\008\000\008\000\008\000\009\000\009\000\008\000\008\000\000\000\
\000\000\009\000\000\000\009\000\009\000\009\000\009\000\004\000\
\000\000\009\000\009\000\000\000\004\000\000\000\004\000\004\000\
\004\000\004\000\005\000\000\000\004\000\004\000\000\000\005\000\
\000\000\005\000\005\000\005\000\005\000\000\000\000\000\005\000\
\005\000"

let yycheck = "\002\000\
\000\000\014\000\000\000\036\000\003\000\000\000\057\000\058\000\
\059\000\013\001\030\001\008\001\000\000\016\000\017\000\001\001\
\002\001\003\001\004\001\005\001\006\001\018\001\003\001\027\001\
\000\000\001\001\002\001\003\001\013\001\000\000\081\000\017\001\
\083\000\019\001\017\001\009\001\021\001\036\000\024\001\025\001\
\017\001\017\001\028\001\019\001\018\001\013\001\000\000\003\001\
\024\001\025\001\017\001\013\001\028\001\012\001\022\001\032\001\
\033\001\034\001\020\001\007\001\008\001\094\000\065\000\000\000\
\001\001\002\001\003\001\004\001\005\001\013\001\000\000\074\000\
\071\000\076\000\018\001\060\000\061\000\014\001\077\000\000\000\
\017\001\008\001\019\001\035\001\013\001\001\001\026\001\024\001\
\025\001\018\001\035\001\028\001\009\001\018\001\023\001\000\000\
\001\001\002\001\003\001\004\001\005\001\001\000\002\000\003\000\
\015\001\108\000\014\001\120\000\111\000\014\001\023\001\000\000\
\017\001\001\001\019\001\029\001\013\001\018\001\013\001\024\001\
\025\001\018\001\009\001\028\001\009\001\019\000\023\001\000\000\
\001\001\002\001\003\001\004\001\005\001\014\001\015\001\016\001\
\054\000\056\000\001\001\002\001\003\001\015\000\005\001\000\000\
\017\001\013\000\019\001\114\000\001\001\002\001\003\001\024\001\
\025\001\110\000\017\001\028\001\019\001\036\000\255\255\000\000\
\115\000\024\001\025\001\255\255\017\001\028\001\019\001\009\001\
\010\001\011\001\255\255\000\000\025\001\255\255\255\255\028\001\
\062\000\063\000\064\000\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\009\001\255\255\255\255\255\255\255\255\014\001\013\001\
\255\255\255\255\018\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\029\001\018\001\026\001\
\020\001\021\001\022\001\023\001\007\001\008\001\026\001\027\001\
\255\255\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\007\001\008\001\026\001\
\027\001\255\255\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\007\001\008\001\
\026\001\027\001\255\255\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\012\001\
\013\001\026\001\027\001\255\255\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\012\001\013\001\026\001\027\001\255\255\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\013\001\
\255\255\026\001\027\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\013\001\255\255\026\001\027\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001"

let yynames_const = "\
  ABS\000\
  TILDA\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GT\000\
  LT\000\
  LP\000\
  RP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  COMMA\000\
  PROJ\000\
  LET\000\
  IN\000\
  END\000\
  BACKSLASH\000\
  DOT\000\
  DEF\000\
  LOCAL\000\
  TUNIT\000\
  TBOOL\000\
  TINT\000\
  COLON\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 20 "parser.mly"
                            (_1)
# 351 "parser.ml"
               : Secd.exptype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 24 "parser.mly"
                              (_1)
# 358 "parser.ml"
               : Secd.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    Obj.repr(
# 28 "parser.mly"
                              (_1)
# 365 "parser.ml"
               : Secd.definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 32 "parser.mly"
                              (_1)
# 372 "parser.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 33 "parser.mly"
                              (Disjunction(_1, _3))
# 380 "parser.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 37 "parser.mly"
                              (_1)
# 387 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 38 "parser.mly"
                              (Conjunction(_1, _3))
# 395 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 42 "parser.mly"
                              (_1)
# 402 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 43 "parser.mly"
                              (Not(_2))
# 409 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 47 "parser.mly"
                              (_1)
# 416 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 48 "parser.mly"
                              (GreaterT(_1, _3))
# 424 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 49 "parser.mly"
                              (LessT(_1, _3))
# 432 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr3) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 50 "parser.mly"
                              (GreaterTE(_1, _4))
# 440 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr3) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 51 "parser.mly"
                              (LessTE(_1, _4))
# 448 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 52 "parser.mly"
                              (Equals(_1, _3))
# 456 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 56 "parser.mly"
                              (_1)
# 463 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 57 "parser.mly"
                              (Add(_1, _3))
# 471 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 58 "parser.mly"
                              (Sub(_1, _3))
# 479 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 62 "parser.mly"
                              (_1)
# 486 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 63 "parser.mly"
                              (Div(_1, _3))
# 494 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 64 "parser.mly"
                              (Rem(_1, _3))
# 502 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 65 "parser.mly"
                              (Mult(_1, _3))
# 510 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 69 "parser.mly"
                              (_1)
# 517 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 70 "parser.mly"
                              (Abs(_2))
# 524 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 74 "parser.mly"
                              (_1)
# 531 "parser.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 75 "parser.mly"
                              (Negative(_2))
# 538 "parser.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr9) in
    Obj.repr(
# 79 "parser.mly"
                                        (_1)
# 545 "parser.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 80 "parser.mly"
                                        (Project((_3, _5), _7))
# 554 "parser.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr10) in
    Obj.repr(
# 84 "parser.mly"
                                    (_1)
# 561 "parser.ml"
               : 'expr9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_call) in
    Obj.repr(
# 85 "parser.mly"
                                    (_1)
# 568 "parser.ml"
               : 'expr9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
                                    (N(_1))
# 575 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 91 "parser.mly"
                                    (B(_1))
# 582 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                                    (Var(_1))
# 589 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ntuple) in
    Obj.repr(
# 93 "parser.mly"
                                    (_1)
# 596 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 94 "parser.mly"
                                    (InParen(_2))
# 603 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'def) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 95 "parser.mly"
                                    (Let(_2, _4))
# 611 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_abstraction) in
    Obj.repr(
# 96 "parser.mly"
                                    (_1)
# 618 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr0) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expr0) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 97 "parser.mly"
                                        (IfThenElse(_2, _4, _6))
# 627 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_tail) in
    Obj.repr(
# 101 "parser.mly"
                                        (Tuple(1+(List.length _4), (_2::_4)))
# 635 "parser.ml"
               : 'ntuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 105 "parser.mly"
                                 ([_1])
# 642 "parser.ml"
               : 'tuple_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_tail) in
    Obj.repr(
# 106 "parser.mly"
                                 (_1::_3)
# 650 "parser.ml"
               : 'tuple_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_def) in
    Obj.repr(
# 112 "parser.mly"
                                         (_1)
# 657 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_def) in
    Obj.repr(
# 117 "parser.mly"
                          (_1)
# 664 "parser.ml"
               : 'term_def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr0) in
    Obj.repr(
# 121 "parser.mly"
                                    (Simple(_2, _6, _4))
# 673 "parser.ml"
               : 'simple_def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr10) in
    Obj.repr(
# 129 "parser.mly"
                                            (FunctionAbstraction(_2, _6, _4))
# 682 "parser.ml"
               : 'function_abstraction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr10) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 133 "parser.mly"
                                            (FunctionCall(_1, _3))
# 690 "parser.ml"
               : 'function_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_type_expr) in
    Obj.repr(
# 137 "parser.mly"
                            (_1)
# 697 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_type_expr) in
    Obj.repr(
# 138 "parser.mly"
                            (_1)
# 704 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'basic_type_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'basic_type_expr) in
    Obj.repr(
# 142 "parser.mly"
                                            (Tfunc(_1, _4))
# 712 "parser.ml"
               : 'func_type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 146 "parser.mly"
                                      (Tunit)
# 718 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                                      (Tint)
# 724 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
                                      (Tbool)
# 730 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'func_type_expr) in
    Obj.repr(
# 149 "parser.mly"
                                      (_2)
# 737 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'basic_type_expr) in
    Obj.repr(
# 150 "parser.mly"
                                      (_2)
# 744 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tail_type_expr) in
    Obj.repr(
# 151 "parser.mly"
                                      (Ttuple(_2::_4))
# 752 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 155 "parser.mly"
                                  ([_1])
# 759 "parser.ml"
               : 'tail_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tail_type_expr) in
    Obj.repr(
# 156 "parser.mly"
                                  (_1::_3)
# 767 "parser.ml"
               : 'tail_type_expr))
(* Entry def_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry exp_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry type_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let def_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Secd.definition)
let exp_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Secd.exptree)
let type_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Secd.exptype)
