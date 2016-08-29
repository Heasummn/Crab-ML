open Llvm_target
open Llvm_all_backends

let init = initialize ()

let create modu = 
    let triple = Target.default_triple () in
    print_endline triple;
    let target = Target.by_triple triple in
    let machine = TargetMachine.create ~triple target in
    TargetMachine.emit_to_file modu CodeGenFileType.ObjectFile "_build/executable.o" machine
        
