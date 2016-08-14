type tp = TInt | TFloat | TEmpty

let rep_type = function
	| TInt      -> "int"
	| TFloat    -> "float"
	| TEmpty	-> "empty"

let types = Hashtbl.create 2;;
Hashtbl.add types "int" TInt;;
Hashtbl.add types "float" TFloat;;

let find_type typ = 
	try
        Hashtbl.find types typ
	with 
    	| Not_found -> TEmpty
