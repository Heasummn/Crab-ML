open CrabAst
open Types

let annotateFunc func = 
    { func with tp = Tint } 