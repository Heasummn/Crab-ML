%{
	open Ast
%}

%token <int> 	INT
%token <float>	FLOAT

%token 			EOF

/* Non keywords */
%token SEMI

%start <Ast.ast list> program
%%

program:
	| stmts = statements EOF	{ stmts }
	;

/* Empty, or a statement with more statements */
statements:
	| 							{ [] }
	| stmt = statement; SEMI;
	 	stmts = statements 		{ stmt :: stmts } 
	;

statement:
	| i = INT 					{ Integer i }
	| f = FLOAT 				{ Float f }
	;