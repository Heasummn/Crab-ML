open CrabTestUtil
open OUnit2

let in_dir = "test/lexing/input/"
let out_dir = "test/lexing/output/"
let test_files = List.sort (Pervasives.compare) (Array.to_list (Sys.readdir in_dir))

let base_test x _ = x

let suite =
    let in_out = 
        List.map (fun file -> in_dir ^ file, out_dir ^ file) test_files 
    in

    "Lexing">:::
        List.map (fun (input, output)   -> 
            input >:: base_test (comp_files input output)) in_out


let _ = 
        run_test_tt_main suite;