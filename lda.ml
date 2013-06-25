open Sampler

let _ = Random.self_init ();;

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


let countor n = let vvv = ref n in
    fun () -> print_endline (string_of_int !vvv); vvv := !vvv + 1;;

let cnt = countor 0;;

let read_info in_chan =
    Scanf.fscanf in_chan "%i %i %i" (fun x y z -> dcnt := x; tcnt := y; wcnt := z)


let read_docs in_chan = 
    let from_line = fun line -> Scanf.sscanf line "%i %i" (fun x y -> (x, y, Random.int kkk)) in
    let rec iter_read accum = 
        let stat, line = 
            try (true, (input_line in_chan))
            with End_of_file -> (false, "")
        in if stat == true then
            iter_read ((from_line line)::accum)
            else accum
    in iter_read [];;

(*read dcnt, tcnt, wcnt*)
with_i (open_in "data/info") read_info;;

(*read docs*)
let doc_list = with_i (open_in "data/doc_list") read_docs;;
(*init matrixs of count*)
let otcnt = Array.init !tcnt (fun x -> Array.make kkk 0);;
let omcnt = Array.init !dcnt (fun x -> Array.make kkk 0);;
let nk = Array.make kkk 0;;
let nm = Array.make !dcnt 0;;
let ttpc = Array.make !tcnt [];;
let mtpc = Array.make !mcnt [];;

let _ = print_endline "init_stat";;

let _ = List.iter (fun x -> let m, t, z = x in
            omcnt.(m).(z) <- omcnt.(m).(z) + 1;
            otcnt.(t).(z) <- otcnt.(t).(z) + 1);;

(*get non-zero position list for array*)
let non_zero_list a =
    let rec for_i i accum =
        if i == kkk then accum
        else if a.(i) == 0 then
            for_i (i+1) accum
        else for_i (i+1) (i::accum)
    in for_i 0 [];;

(*non zero list of documents topics*)
let _ =
    let rec iter i =
        match i with
        !dcnt -> ()
        | i -> mtpc.(i) <- non_zero_list omcnt.(i)
    in iter 0;;

(*non zero list of term topics*)
let _=
    let rec iter i =
        match i with
        !tcnt -> ()
        | i -> ttpc.(i) <- non_zero_list otcnt.(i)
    in iter 0;;

let _ = print_endline (string_of_int (List.length doc_list));;
