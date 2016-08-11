%{
    open CrabAst
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> ALPHANUM

/* Keywords */
%token DEF

/* Operators */
%token LPAREN RPAREN
%token PLUS MINUS MULT DIV
%token EQUAL
%token SEMI

%token EOF

/* Associativity */
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS

%start <CrabAst.ast list> program
%%

/* Program ->
 *      exprs EOF
 */
program:
    | stmts = funcs EOF         
            { stmts }
    ;

/* Funcs ->
 * EMPTY | func funcs
 */
funcs:
    |       { [] }
    | f = func; fs = funcs;
            { f::fs }
    ;

/* Func ->
 * DEF type name = exprs
 */
func:
    | DEF; ty = ALPHANUM; name = ALPHANUM; EQUAL; body = expr; option(SEMI);
            { let typ = match ty with
                | "int"     -> Tint
                | "float"   -> Tfloat
                | _         -> raise(Error.TypeError ("Unknown type"))
            in
                Func(typ, name, body);
            }
    ;


/* Expr ->
 *      literal | (expr) 
 *  |   - expr
 *  |   expr * expr | expr / expr
 *  |   expr + expr | expr - expr
 */
expr:
    | e1 = literal
        { Lit e1 }
    | LPAREN e1 = expr RPAREN
        { Paren e1 }
    | MINUS e1 = expr   %prec UMINUS
        { Neg e1 }
    | e1 = expr MULT e2 = expr
        { Mult (e1, e2) }
    | e1 = expr DIV e2 = expr
        { Div (e1, e2) }
    | e1 = expr PLUS e2 = expr
        { Add (e1, e2) }
    | e1 = expr MINUS e2 = expr
        { Sub (e1, e2) }
    ;

/* Literal ->
 *      INT | FLOAT
 */
literal:
    | i = INT                           
            { Integer i }
    | f = FLOAT                         
            { Float f }
    ;

