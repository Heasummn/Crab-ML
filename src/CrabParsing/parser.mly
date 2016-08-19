%{
    open CrabAst
    open Types
    open CrabEnv

    let make_loc start end_pos = Location.make start end_pos

    (* Return the appropiate type with it's annotation *)
    let make_node node start end_pos = {
        data = node; 
        position = (make_loc start end_pos); 
        tp = TEmpty
    }

    let make_func name args ty body =
        Func((name, ty), args, body)
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
%token SEMI COLON COMMA

%token EOF

/* Associativity */
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS

%start <CrabAst.toplevel list> program
%%

/* Program ->
 *      exprs EOF
 */
program:
    | stmts = funcs EOF         
            { stmts }
    ;

/* Funcs ->
 *      EMPTY | func funcs
 */
funcs:
    |       { [] }
    | f = func; fs = funcs;
            { f::fs }
    ;

/* Func ->
 *      DEF name (args): type = exprs ;?
 */
func:
    | DEF; name = ALPHANUM; args = arguments; COLON; ty = typ;
        EQUAL; body = expr; option(SEMI);
            { make_node (make_func name args ty body) $startpos $endpos }
    ;

/* Type ->
 *      ALPHANUM
 */
typ:
    | ty = ALPHANUM
            {
                lookup_type ty 
            }
/* Arguments ->
 *      ([NONE | argument :: arguments])
 */
arguments:
    | LPAREN; args = separated_list(COMMA, argument); RPAREN;
            { args }
    ;

/* Argument ->
 *      arg: type
 */

argument:
    | arg = ALPHANUM; COLON; ty = typ;
            { (arg, ty)  }
    ;
/* Expr ->
 *      literal | var
 *  |   (expr) 
 *  |   - expr
 *  |   expr * expr | expr / expr
 *  |   expr + expr | expr - expr
 */
expr:
    | e1 = literal
        { make_node (Lit e1) $startpos $endpos }
    | v = ALPHANUM
        { make_node (Var v) $startpos $endpos }
    | LPAREN e1 = expr RPAREN
        { make_node (Paren e1) $startpos $endpos }
    | MINUS e1 = expr   %prec UMINUS
        { make_node (Neg e1) $startpos $endpos }
    | e1 = expr MULT e2 = expr
        { make_node (Mult (e1, e2)) $startpos $endpos }
    | e1 = expr DIV e2 = expr
        { make_node (Div (e1, e2)) $startpos $endpos }
    | e1 = expr PLUS e2 = expr
        { make_node (Add (e1, e2)) $startpos $endpos }
    | e1 = expr MINUS e2 = expr
        { make_node (Sub (e1, e2)) $startpos $endpos }
    ;

/* Literal ->
 *      INT | FLOAT
 */
literal:
    | i = INT                           
            { make_node (Integer i) $startpos $endpos }
    | f = FLOAT                         
            { make_node (Float f) $startpos $endpos }
    ;

