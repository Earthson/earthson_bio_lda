open Sampler

let _ = Random.self_init ();;


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
    fun () -> 
        let ans = !vvv in
        if !vvv mod 100000 == 0 then
            print_endline (string_of_int !vvv); 
        vvv := !vvv + 1;
        ans;;

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
let mtpc = Array.make !dcnt [];;

let _ = print_endline "init_stat";;

let _ = List.iter (fun x -> let m, t, z = x in
            omcnt.(m).(z) <- omcnt.(m).(z) + 1;
            otcnt.(t).(z) <- otcnt.(t).(z) + 1;
            nk.(z) <- nk.(z) + 1;
            nm.(m) <- nm.(m) + 1;) doc_list;;

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
        if i == !dcnt then ()
        else (mtpc.(i) <- non_zero_list omcnt.(i))
    in iter 0;;

(*non zero list of term topics*)
let _=
    let rec iter i =
        if i == !tcnt then ()
        else (ttpc.(i) <- non_zero_list otcnt.(i))
    in iter 0;;


let default_p z = beta*.alpha/.((float_of_int nk.(z)+.beta*.(float_of_int !tcnt)));;

let prob m t z = 
        ((float_of_int otcnt.(t).(z))+.beta)*.((float_of_int omcnt.(m).(z))+.alpha)/.((float_of_int nk.(z)+.beta*.(float_of_int !tcnt)));;

let sample_gen, set, to_set, update_with_stack, clear, sum_gen, show_sums, show_stats, show_vals = multi_sampler kkk default_p;;

let to_set_with_list lst m t =
    List.iter (fun z -> to_set z (prob m t z)) lst

let clear_zero tpclist tpcstat =
    let rec for_iter accum = function
        z::rlft ->
            if tpcstat.(z) == 0 then
                for_iter accum rlft
            else for_iter (z::accum) rlft
        | [] -> accum
    in for_iter [] tpclist;;

let try_tpc_regen m t z nz = 
    if omcnt.(m).(nz) == 1 then
        mtpc.(m) <- (nz::mtpc.(m));
    if omcnt.(m).(z) == 0 then
        mtpc.(m) <- (clear_zero mtpc.(m) omcnt.(m));
    if otcnt.(t).(nz) == 1 then
        ttpc.(t) <- (nz::ttpc.(t));
    if otcnt.(t).(z) == 0 then
        ttpc.(t) <- (clear_zero ttpc.(t) otcnt.(t));;

let info_len p = -.(log p)/.(log 2.)

let sample_one (pm, pt, _) (m, t, z) =
    let pre = if pm != m || pt != t then true else false in
    nk.(z) <- nk.(z) - 1;
    nm.(m) <- nm.(m) - 1;
    wcnt := !wcnt - 1;
    omcnt.(m).(z) <- omcnt.(m).(z) - 1;
    otcnt.(t).(z) <- otcnt.(t).(z) - 1;
    if pre == false then
        begin
        clear();
        to_set z (prob m t z);
        to_set_with_list ttpc.(t) m t;
        to_set_with_list mtpc.(m) m t;
        update_with_stack()
        end
    else set z (prob m t z);
    let nz = sample_gen() in
    otcnt.(t).(nz) <- otcnt.(t).(nz) + 1;
    omcnt.(m).(nz) <- omcnt.(m).(nz) + 1;
    wcnt := !wcnt + 1;
    nm.(m) <- nm.(m) + 1;
    nk.(nz) <- nk.(nz) + 1;
    set nz (prob m t nz);
    try_tpc_regen m t z nz;
    let tmpsum = 
        sum_gen()/.((float_of_int nm.(m))+.(float_of_int kkk)*.alpha)
    in (nz, info_len tmpsum);;
    
let sample_gibbs_round its = 
    let rec round accum pre sum = function
    (m, t, z)::rlft -> 
        let nz, info = sample_one pre (m, t, z) in
        round ((m, t, nz)::accum) (m, t, z) (sum+.info) rlft
    | [] -> (accum, sum)
    in let lst, sum = round [] (-1, -1, -1) 0.0 its in
    println_int "Round" (cnt());
    println_float "sum" (sum/.(float_of_int !wcnt));
    lst;;


let rec for_round i doc_list pre_time=
    if i < 100 then
        let cur_time = (Sys.time()) in
        println_float "time" (cur_time -. pre_time);
        for_round (i+1) (sample_gibbs_round doc_list) cur_time
    else ()
    in for_round 0 doc_list (Sys.time());;
