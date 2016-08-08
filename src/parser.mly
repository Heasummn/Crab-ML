%{
	open Ast
%}

%token <int> 	INT
%token <float>	FLOAT

/* Operators */
%token PLUS MINUS

%token 			EOF

/* Non keywords */
%token SEMI

/* Associativity */
%left PLUS MINUS

%start <Ast.ast list> program
%%

program:
	| stmts = statements EOF			
			{ stmts }
	;

/* Empty, or a statement with more statements */
statements:
	| 									{ [] }
	| stmt = statement; SEMI;
	 	stmts = statements 	
	 		{ stmt :: stmts } 
	;

statement:
	| e1 = literal				
		{ Lit e1 }
	| e1 = statement MINUS e2 = statement
		{ Sub (e1, e2) }
	| e1 = statement PLUS e2 = statement
    	{ Add (e1, e2) }
	;

literal:
	| i = INT 							
			{ Integer i }
	| f = FLOAT 						
			{ Float f }
	;

