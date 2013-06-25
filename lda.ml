open Sampler
let test_f = function i -> (float_of_int (i+1))*.0.3;;
let sample_gen, set, to_set, update_with_stack, clear, show_sums, show_stats, show_vals = multi_sampler 10 test_f;;


let kkk = 256;;
let alpha = 50.0/.(float_of_int kkk);;
let beta = 0.01;;

let wcnt = ref 0;;
let tcnt = ref 0;;
let dcnt = ref 0;;

    

let read_info in_chan =
    Scanf.scanf in_chan "%i %i %i" (fun x y z -> dcnt := x; y := tcnt; z := wcnt)


let read_docs in_chan = 
    let from_line = fun line -> Scanf.sscanf line "%i %i" (fun x y -> x, y) in
    let rec iter_read accum = 
        try
            let line = input_line in_chan in
            iter_read ((from_line line)::accum)
        with End_of_file -> accum
    in iter_read [];;
            

let info_chan = open_in "data/info" in
    Scanf.fscanf info_chan "%i %i %i" (fun x y z ->(dcnt := x;tcnt := y;wcnt := z));
    close_in info_chan;;

let doc_list = 
    let docs_chan = open_in "data/doc_list" in
    let ans = read_docs docs_chan in
        close_in docs_chan; ans;;


print_int (List.length doc_list);;

(*
file_line_iter "data/docs" (fun line -> print_endline line);;
*)

