open AnalyzeTp

let annotateAST funcs = 
    List.map annotate_func funcs