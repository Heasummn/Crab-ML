open AnalyzeTp

let annotateAST ctx funcs = 
    List.map (annotate_func ctx) funcs
