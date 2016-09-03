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

    let make_op name args ty body = 
        Operator((name, ty), args, body)
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> ALPHANUM
%token <string> OPERATOR

/* Keywords */
%token DEF, LET, IN, OP, EXTERN

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
    | f = op; fs = funcs
        { f::fs }
    | f = extern; fs = funcs;
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

op:
    | OP; name = OPERATOR; args = arguments; COLON; ty = typ;
        EQUAL; body = expr; option(SEMI);
            { make_node (make_op name args ty body) $startpos $endpos }
    ;

extern:
    | EXTERN; name = ALPHANUM; args = arguments; COLON; ty = typ; option(SEMI);
        { make_node (Extern((name, ty), args)) $startpos $endpos }
/* Type ->
 *      ALPHANUM
 */
typ:
    | ty = ALPHANUM
            {
                (* TODO: when we add types, we have to maintain an Env of them *)
                match lookup_type CrabEnv.base_type_env (Symbol.symbol ty) with
                    | Some ty   -> ty
                    (* TODO: Better errors *)
                    | None      -> (raise(Error.TypeError("Unknown type " ^ ty)))
            }
    ;
/* Arguments ->
 *      ([NONE | argument, arguments])
 */
arguments:
    | LPAREN; args = separated_list(COMMA, argument); RPAREN;
            {
                if List.length args > 0 then
                    List.fold_left (fun (prev_name, arrow) (name, arg) ->
                        (List.append prev_name name, TArrow(arrow, arg))) (List.hd args) (List.tl args)
                else
                    ([""], TEmpty)
            }
    ;

/* Argument ->
 *      arg: type
 */

argument:
    | arg = ALPHANUM; COLON; ty = typ;
            { ([arg], ty)  }
    ;
/* Expr ->
 *  |   literal | var
 *  |   assignment
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
    | a = assign;
        { make_node (Assign a) $startpos $endpos } 
    | LPAREN e1 = expr RPAREN
        { make_node (Paren e1) $startpos $endpos }
    | MINUS e1 = expr   %prec UMINUS
        { make_node (Neg e1) $startpos $endpos }
    | callee = ALPHANUM; LPAREN; args = separated_list(COMMA, expr); RPAREN;
        { make_node (Call (callee, args)) $startpos $endpos }
    | e1 = expr MULT e2 = expr
        { make_node (BinOp (e1, "*", e2)) $startpos $endpos }
    | e1 = expr DIV e2 = expr
        { make_node (BinOp (e1, "/", e2)) $startpos $endpos }
    | e1 = expr PLUS e2 = expr
        { make_node (BinOp (e1, "+", e2)) $startpos $endpos }
    | e1 = expr MINUS e2 = expr
        { make_node (BinOp (e1, "-", e2)) $startpos $endpos }
    | e1 = expr; name = OPERATOR; e2 = expr;
        { make_node (BinOp (e1, name, e2)) $startpos $endpos }
    ;

/* Assign -> 
 *  |   let name: type = expr in expr
 */
assign:
    | LET name = ALPHANUM; COLON; ty = typ; EQUAL; value = expr; IN; 
        body = expr;    
            { (name, ty), value, body }
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

