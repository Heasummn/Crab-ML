open Llvm_target
open Llvm_all_backends

let init_compiler = initialize ()

let create_obj output modu = 
    let triple = Target.default_triple () in
    let target = Target.by_triple triple in
    let machine = TargetMachine.create ~triple target in
    TargetMachine.emit_to_file modu CodeGenFileType.ObjectFile output machine
        
