open AnalyzeTp

let annotateAST funcs = 
    List.map annotateFunc funcs