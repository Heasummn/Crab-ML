%{
	open Ast
%}

%token <int> 	INT
%token <float>	FLOAT

/* Operators */
%token LPAREN RPAREN
%token PLUS MINUS

%token 			EOF

/* Non keywords */
%token SEMI

/* Associativity */
%left PLUS MINUS
%nonassoc UMINUS

%start <Ast.ast list> program
%%

program:
	| stmts = exprs EOF			
			{ stmts }
	;

/* Empty, or a expr with more exprs */
exprs:
	| 									{ [] }
	| stmt = expr; SEMI;
	 	stmts = exprs 	
	 		{ stmt :: stmts } 
	;

expr:
	| e1 = literal
		{ Lit e1 }
	| LPAREN e1=expr RPAREN
		{ Paren e1 }
	| e1 = expr MINUS e2 = expr
		{ Sub (e1, e2) }
	| e1 = expr PLUS e2 = expr
    	{ Add (e1, e2) }
    | MINUS e1 = expr 	%prec UMINUS
    	{ Neg e1 }
	;

literal:
	| i = INT 							
			{ Integer i }
	| f = FLOAT 						
			{ Float f }
	;

