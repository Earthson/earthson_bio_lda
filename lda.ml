open Sampler
let test_f = function i -> (float_of_int (i+1))*.0.3;;
let sample_gen, set, to_set, update_with_stack, clear, show_sums, show_stats, show_vals = multi_sampler 10 test_f;;


let kkk = 256;;
let alpha = 50.0/.(float_of_int kkk);;
let beta = 0.01;;

let wcnt = ref 0;;
let tcnt = ref 0;;
let dcnt = ref 0;;


let with_i in_chan f =
    let ans = f in_chan in
    close_in in_chan; ans;;

let with_o out_chan f =
    let ans = f out_chan in
    close_out out_chan; ans;;

let read_info in_chan =
    Scanf.fscanf in_chan "%i %i %i" (fun x y z -> dcnt := x; tcnt := y; wcnt := z)


let read_docs in_chan = 
    let from_line = fun line -> Scanf.sscanf line "%i %i" (fun x y -> x, y) in
    let rec iter_read accum = 
        let stat, line = 
        try
            true, input_line in_chan
        with End_of_file -> false, ""
        in if stat == true then
            iter_read ((from_line line)::accum)
        else accum
    in iter_read [];;
            

with_i (open_in "data/info") read_info;;

let doc_list = with_i (open_in "data/doc_list") read_docs;;


print_int (List.length doc_list);;

(*
file_line_iter "data/docs" (fun line -> print_endline line);;
*)

