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
  | SEMICOLON
  | PARALLEL
  | LOCAL
  | TUNIT
  | TBOOL
  | TINT
  | COLON
  | EOF
  | REC

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Krivine
# 47 "parser.ml"
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
  287 (* SEMICOLON *);
  288 (* PARALLEL *);
  289 (* LOCAL *);
  290 (* TUNIT *);
  291 (* TBOOL *);
  292 (* TINT *);
  293 (* COLON *);
    0 (* EOF *);
  294 (* REC *);
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
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\018\000\
\020\000\020\000\006\000\006\000\006\000\021\000\022\000\019\000\
\017\000\004\000\004\000\024\000\023\000\023\000\023\000\023\000\
\023\000\023\000\025\000\025\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\003\000\001\000\003\000\001\000\
\002\000\001\000\003\000\003\000\004\000\004\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\002\000\
\001\000\002\000\001\000\007\000\001\000\001\000\001\000\001\000\
\001\000\002\000\001\000\003\000\005\000\001\000\007\000\004\000\
\002\000\003\000\001\000\003\000\003\000\001\000\006\000\006\000\
\004\000\001\000\001\000\004\000\001\000\001\000\001\000\003\000\
\003\000\004\000\002\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\061\000\000\000\043\000\
\046\000\031\000\032\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\062\000\000\000\000\000\
\006\000\000\000\000\000\000\000\019\000\023\000\025\000\027\000\
\000\000\030\000\035\000\038\000\000\000\053\000\055\000\054\000\
\063\000\000\000\000\000\051\000\000\000\000\000\000\000\003\000\
\024\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\045\000\044\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\020\000\021\000\000\000\
\000\000\057\000\056\000\000\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\000\000\058\000\
\052\000\000\000\041\000\000\000\000\000\000\000\037\000\000\000\
\000\000\059\000\000\000\042\000\000\000\000\000\048\000\060\000\
\039\000\028\000"

let yydgoto = "\004\000\
\006\000\022\000\041\000\111\000\102\000\007\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\103\000\008\000\009\000\043\000\044\000\
\112\000"

let yysindex = "\026\000\
\240\254\006\255\003\255\000\000\055\255\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\148\255\160\255\119\255\006\255\
\006\255\045\255\240\254\066\255\073\255\000\000\005\000\067\255\
\000\000\031\255\041\255\050\255\000\000\000\000\000\000\000\000\
\080\255\000\000\000\000\000\000\003\255\000\000\000\000\000\000\
\000\000\090\000\097\255\000\000\074\255\240\254\240\254\000\000\
\000\000\000\000\031\255\057\255\022\255\109\255\070\255\076\255\
\000\000\006\255\000\000\006\255\119\255\049\255\090\255\119\255\
\119\255\119\255\119\255\119\255\006\255\108\255\008\255\107\255\
\000\000\111\255\003\255\000\000\000\000\000\000\006\255\006\255\
\110\255\006\255\003\255\067\255\000\000\041\255\119\255\041\255\
\119\255\041\255\050\255\050\255\000\000\000\000\000\000\054\255\
\003\255\000\000\000\000\003\255\118\255\085\255\000\000\000\255\
\133\255\246\254\106\255\041\255\041\255\000\000\015\255\000\000\
\000\000\006\255\000\000\006\255\006\255\121\255\000\000\180\255\
\003\255\000\000\124\255\000\000\019\255\160\255\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\237\000\
\000\000\206\000\085\000\022\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\238\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\222\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\131\255\132\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\252\000\000\000\106\000\000\000\127\000\
\000\000\148\000\043\000\064\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\169\000\190\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\003\000\002\000\123\000\087\000\086\000\
\137\000\250\255\024\000\063\000\141\000\242\255\000\000\035\000\
\000\000\000\000\000\000\040\000\053\000\000\000\221\255\121\000\
\038\000"

let yytablesize = 540
let yytable = "\050\000\
\029\000\071\000\058\000\023\000\059\000\042\000\010\000\011\000\
\012\000\013\000\014\000\015\000\058\000\005\000\047\000\074\000\
\119\000\052\000\053\000\037\000\117\000\016\000\016\000\121\000\
\017\000\098\000\001\000\002\000\003\000\018\000\019\000\058\000\
\122\000\020\000\058\000\048\000\038\000\039\000\040\000\070\000\
\129\000\080\000\017\000\021\000\061\000\062\000\063\000\064\000\
\065\000\010\000\011\000\012\000\013\000\014\000\086\000\088\000\
\090\000\045\000\066\000\067\000\068\000\054\000\087\000\018\000\
\113\000\016\000\058\000\017\000\056\000\058\000\096\000\110\000\
\018\000\019\000\078\000\057\000\020\000\101\000\060\000\079\000\
\108\000\104\000\109\000\106\000\010\000\107\000\021\000\091\000\
\092\000\073\000\010\000\011\000\012\000\013\000\014\000\082\000\
\069\000\058\000\076\000\077\000\046\000\047\000\115\000\089\000\
\074\000\015\000\016\000\116\000\017\000\081\000\075\000\130\000\
\083\000\018\000\019\000\123\000\097\000\020\000\125\000\010\000\
\011\000\012\000\013\000\014\000\099\000\100\000\011\000\021\000\
\093\000\094\000\095\000\114\000\105\000\118\000\120\000\016\000\
\058\000\017\000\126\000\050\000\051\000\055\000\018\000\019\000\
\084\000\085\000\020\000\012\000\010\000\011\000\012\000\051\000\
\014\000\049\000\127\000\124\000\021\000\072\000\128\000\000\000\
\010\000\011\000\012\000\000\000\016\000\000\000\017\000\000\000\
\013\000\000\000\000\000\018\000\019\000\000\000\000\000\020\000\
\016\000\000\000\017\000\000\000\010\000\011\000\012\000\018\000\
\019\000\021\000\000\000\020\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\021\000\017\000\000\000\
\000\000\000\000\000\000\000\000\019\000\008\000\000\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
\029\000\058\000\029\000\000\000\029\000\029\000\029\000\029\000\
\000\000\000\000\029\000\029\000\016\000\016\000\000\000\029\000\
\029\000\016\000\016\000\016\000\016\000\016\000\000\000\016\000\
\047\000\016\000\016\000\016\000\016\000\047\000\047\000\016\000\
\016\000\017\000\017\000\000\000\016\000\016\000\017\000\017\000\
\017\000\017\000\017\000\000\000\017\000\000\000\017\000\017\000\
\017\000\017\000\046\000\047\000\017\000\017\000\018\000\018\000\
\000\000\017\000\017\000\018\000\018\000\018\000\018\000\018\000\
\000\000\018\000\000\000\018\000\018\000\018\000\018\000\000\000\
\000\000\018\000\018\000\000\000\000\000\000\000\018\000\018\000\
\010\000\010\000\010\000\010\000\010\000\000\000\010\000\000\000\
\010\000\010\000\010\000\010\000\000\000\000\000\010\000\010\000\
\000\000\000\000\000\000\010\000\010\000\015\000\015\000\015\000\
\015\000\015\000\000\000\015\000\000\000\015\000\015\000\015\000\
\015\000\000\000\000\000\015\000\015\000\000\000\000\000\000\000\
\015\000\015\000\011\000\011\000\011\000\011\000\011\000\000\000\
\011\000\000\000\011\000\011\000\011\000\011\000\000\000\000\000\
\011\000\011\000\000\000\000\000\000\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\000\000\012\000\000\000\012\000\
\012\000\012\000\012\000\000\000\000\000\012\000\012\000\000\000\
\000\000\000\000\012\000\012\000\013\000\013\000\013\000\013\000\
\013\000\000\000\013\000\000\000\013\000\013\000\013\000\013\000\
\000\000\000\000\013\000\013\000\000\000\000\000\000\000\013\000\
\013\000\014\000\014\000\014\000\014\000\014\000\000\000\014\000\
\000\000\014\000\014\000\014\000\014\000\000\000\000\000\014\000\
\014\000\008\000\008\000\000\000\014\000\014\000\000\000\008\000\
\000\000\008\000\008\000\008\000\008\000\000\000\000\000\008\000\
\008\000\009\000\009\000\000\000\008\000\008\000\000\000\009\000\
\000\000\009\000\009\000\009\000\009\000\000\000\050\000\009\000\
\009\000\004\000\000\000\050\000\009\000\009\000\004\000\050\000\
\004\000\004\000\004\000\004\000\000\000\000\000\004\000\004\000\
\005\000\000\000\050\000\004\000\004\000\005\000\000\000\005\000\
\005\000\005\000\005\000\000\000\000\000\005\000\005\000\000\000\
\000\000\000\000\005\000\005\000"

let yycheck = "\014\000\
\000\000\037\000\013\001\002\000\000\000\003\000\001\001\002\001\
\003\001\004\001\005\001\006\001\013\001\030\001\000\000\008\001\
\027\001\016\000\017\000\017\001\021\001\000\000\017\001\009\001\
\019\001\018\001\001\000\002\000\003\000\024\001\025\001\013\001\
\018\001\028\001\013\001\000\000\034\001\035\001\036\001\037\000\
\022\001\020\001\000\000\038\001\014\001\015\001\016\001\007\001\
\008\001\001\001\002\001\003\001\004\001\005\001\061\000\062\000\
\063\000\003\001\009\001\010\001\011\001\017\001\014\001\000\000\
\100\000\017\001\013\001\019\001\003\001\013\001\069\000\018\001\
\024\001\025\001\018\001\003\001\028\001\075\000\012\001\023\001\
\087\000\080\000\089\000\082\000\000\000\083\000\038\001\064\000\
\065\000\000\000\001\001\002\001\003\001\004\001\005\001\026\001\
\017\001\013\001\046\000\047\000\031\001\032\001\018\001\014\001\
\008\001\000\000\017\001\023\001\019\001\001\001\037\001\126\000\
\037\001\024\001\025\001\114\000\009\001\028\001\117\000\001\001\
\002\001\003\001\004\001\005\001\018\001\015\001\000\000\038\001\
\066\000\067\000\068\000\014\001\023\001\001\001\029\001\017\001\
\013\001\019\001\018\001\009\001\009\001\019\000\024\001\025\001\
\058\000\060\000\028\001\000\000\001\001\002\001\003\001\015\000\
\005\001\013\000\120\000\116\000\038\001\037\000\121\000\255\255\
\001\001\002\001\003\001\255\255\017\001\255\255\019\001\255\255\
\000\000\255\255\255\255\024\001\025\001\255\255\255\255\028\001\
\017\001\255\255\019\001\255\255\001\001\002\001\003\001\024\001\
\025\001\038\001\255\255\028\001\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\038\001\019\001\255\255\
\255\255\255\255\255\255\255\255\025\001\000\000\255\255\028\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\038\001\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\013\001\018\001\255\255\020\001\021\001\022\001\023\001\
\255\255\255\255\026\001\027\001\007\001\008\001\255\255\031\001\
\032\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\026\001\020\001\021\001\022\001\023\001\031\001\032\001\026\001\
\027\001\007\001\008\001\255\255\031\001\032\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\020\001\021\001\
\022\001\023\001\031\001\032\001\026\001\027\001\007\001\008\001\
\255\255\031\001\032\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\255\255\020\001\021\001\022\001\023\001\255\255\
\255\255\026\001\027\001\255\255\255\255\255\255\031\001\032\001\
\012\001\013\001\014\001\015\001\016\001\255\255\018\001\255\255\
\020\001\021\001\022\001\023\001\255\255\255\255\026\001\027\001\
\255\255\255\255\255\255\031\001\032\001\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\023\001\255\255\255\255\026\001\027\001\255\255\255\255\255\255\
\031\001\032\001\012\001\013\001\014\001\015\001\016\001\255\255\
\018\001\255\255\020\001\021\001\022\001\023\001\255\255\255\255\
\026\001\027\001\255\255\255\255\255\255\031\001\032\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\255\255\020\001\
\021\001\022\001\023\001\255\255\255\255\026\001\027\001\255\255\
\255\255\255\255\031\001\032\001\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\255\255\020\001\021\001\022\001\023\001\
\255\255\255\255\026\001\027\001\255\255\255\255\255\255\031\001\
\032\001\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\012\001\013\001\255\255\031\001\032\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\255\255\026\001\
\027\001\012\001\013\001\255\255\031\001\032\001\255\255\018\001\
\255\255\020\001\021\001\022\001\023\001\255\255\009\001\026\001\
\027\001\013\001\255\255\014\001\031\001\032\001\018\001\018\001\
\020\001\021\001\022\001\023\001\255\255\255\255\026\001\027\001\
\013\001\255\255\029\001\031\001\032\001\018\001\255\255\020\001\
\021\001\022\001\023\001\255\255\255\255\026\001\027\001\255\255\
\255\255\255\255\031\001\032\001"

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
  SEMICOLON\000\
  PARALLEL\000\
  LOCAL\000\
  TUNIT\000\
  TBOOL\000\
  TINT\000\
  COLON\000\
  EOF\000\
  REC\000\
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
# 375 "parser.ml"
               : Krivine.exptype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 24 "parser.mly"
                              (_1)
# 382 "parser.ml"
               : Krivine.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    Obj.repr(
# 28 "parser.mly"
                              (_1)
# 389 "parser.ml"
               : Krivine.definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 32 "parser.mly"
                              (_1)
# 396 "parser.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr1) in
    Obj.repr(
# 33 "parser.mly"
                              (Disjunction(_1, _3))
# 404 "parser.ml"
               : 'expr0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 37 "parser.mly"
                              (_1)
# 411 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr2) in
    Obj.repr(
# 38 "parser.mly"
                              (Conjunction(_1, _3))
# 419 "parser.ml"
               : 'expr1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 42 "parser.mly"
                              (_1)
# 426 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr3) in
    Obj.repr(
# 43 "parser.mly"
                              (Not(_2))
# 433 "parser.ml"
               : 'expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 47 "parser.mly"
                              (_1)
# 440 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 48 "parser.mly"
                              (GreaterT(_1, _3))
# 448 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 49 "parser.mly"
                              (LessT(_1, _3))
# 456 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr3) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 50 "parser.mly"
                              (GreaterTE(_1, _4))
# 464 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr3) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 51 "parser.mly"
                              (LessTE(_1, _4))
# 472 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr4) in
    Obj.repr(
# 52 "parser.mly"
                              (Equals(_1, _3))
# 480 "parser.ml"
               : 'expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 56 "parser.mly"
                              (_1)
# 487 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 57 "parser.mly"
                              (Add(_1, _3))
# 495 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr5) in
    Obj.repr(
# 58 "parser.mly"
                              (Sub(_1, _3))
# 503 "parser.ml"
               : 'expr4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 62 "parser.mly"
                              (_1)
# 510 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 63 "parser.mly"
                              (Div(_1, _3))
# 518 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 64 "parser.mly"
                              (Rem(_1, _3))
# 526 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr6) in
    Obj.repr(
# 65 "parser.mly"
                              (Mult(_1, _3))
# 534 "parser.ml"
               : 'expr5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 69 "parser.mly"
                              (_1)
# 541 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr7) in
    Obj.repr(
# 70 "parser.mly"
                              (Abs(_2))
# 548 "parser.ml"
               : 'expr6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 74 "parser.mly"
                              (_1)
# 555 "parser.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 75 "parser.mly"
                              (Negative(_2))
# 562 "parser.ml"
               : 'expr7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr9) in
    Obj.repr(
# 79 "parser.mly"
                                        (_1)
# 569 "parser.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr8) in
    Obj.repr(
# 80 "parser.mly"
                                        (Project((_3, _5), _7))
# 578 "parser.ml"
               : 'expr8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr10) in
    Obj.repr(
# 84 "parser.mly"
                                    (_1)
# 585 "parser.ml"
               : 'expr9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_call) in
    Obj.repr(
# 85 "parser.mly"
                                    (_1)
# 592 "parser.ml"
               : 'expr9))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
                                    (N(_1))
# 599 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 91 "parser.mly"
                                    (B(_1))
# 606 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                                    (Var(_1))
# 613 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                                    (VarRec(_2))
# 620 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ntuple) in
    Obj.repr(
# 94 "parser.mly"
                                    (_1)
# 627 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 95 "parser.mly"
                                    (InParen(_2))
# 634 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'def) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 96 "parser.mly"
                                    (Let(_2, _4))
# 642 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'function_abstraction) in
    Obj.repr(
# 97 "parser.mly"
                                    (_1)
# 649 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expr0) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expr0) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 98 "parser.mly"
                                        (IfThenElse(_2, _4, _6))
# 658 "parser.ml"
               : 'expr10))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_tail) in
    Obj.repr(
# 102 "parser.mly"
                                        (Tuple(1+(List.length _4), (_2::_4)))
# 666 "parser.ml"
               : 'ntuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 106 "parser.mly"
                                 ([_1])
# 673 "parser.ml"
               : 'tuple_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_tail) in
    Obj.repr(
# 107 "parser.mly"
                                 (_1::_3)
# 681 "parser.ml"
               : 'tuple_tail))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term_def) in
    Obj.repr(
# 113 "parser.mly"
                                         (_1)
# 688 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_def) in
    Obj.repr(
# 114 "parser.mly"
                                         (Parallel([_1; _3]))
# 696 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_def) in
    Obj.repr(
# 115 "parser.mly"
                                         (Sequence([_1; _3]))
# 704 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_def) in
    Obj.repr(
# 120 "parser.mly"
                          (_1)
# 711 "parser.ml"
               : 'term_def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr0) in
    Obj.repr(
# 124 "parser.mly"
                                    (Simple(_2, _6, _4))
# 720 "parser.ml"
               : 'simple_def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr10) in
    Obj.repr(
# 132 "parser.mly"
                                            (FunctionAbstraction(_2, _6, _4))
# 729 "parser.ml"
               : 'function_abstraction))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr10) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr0) in
    Obj.repr(
# 136 "parser.mly"
                                            (FunctionCall(_1, _3))
# 737 "parser.ml"
               : 'function_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_type_expr) in
    Obj.repr(
# 140 "parser.mly"
                            (_1)
# 744 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_type_expr) in
    Obj.repr(
# 141 "parser.mly"
                            (_1)
# 751 "parser.ml"
               : 'type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'basic_type_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'basic_type_expr) in
    Obj.repr(
# 145 "parser.mly"
                                            (Tfunc(_1, _4))
# 759 "parser.ml"
               : 'func_type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "parser.mly"
                                      (Tunit)
# 765 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "parser.mly"
                                      (Tint)
# 771 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 151 "parser.mly"
                                      (Tbool)
# 777 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'func_type_expr) in
    Obj.repr(
# 152 "parser.mly"
                                      (_2)
# 784 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'basic_type_expr) in
    Obj.repr(
# 153 "parser.mly"
                                      (_2)
# 791 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tail_type_expr) in
    Obj.repr(
# 154 "parser.mly"
                                      (Ttuple(_2::_4))
# 799 "parser.ml"
               : 'basic_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_expr) in
    Obj.repr(
# 158 "parser.mly"
                                  ([_1])
# 806 "parser.ml"
               : 'tail_type_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tail_type_expr) in
    Obj.repr(
# 159 "parser.mly"
                                  (_1::_3)
# 814 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Krivine.definition)
let exp_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Krivine.exptree)
let type_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Krivine.exptype)
