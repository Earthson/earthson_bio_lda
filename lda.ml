open Sampler
open Testutils

let _ = Random.self_init ();;

let kkk = 256;;
let alpha = 50.0/.(float_of_int kkk);;
let beta = 0.01;;

let wcnt = ref 0;;
let tcnt = ref 0;;
let dcnt = ref 0;;

let uniform_k k =
    Array.make k (1.0/.(float_of_int k));;

let with_i in_chan f =
    let ans = f in_chan in
    close_in in_chan; ans;;

let with_o out_chan f =
    let ans = f out_chan in
    close_out out_chan; ans;;

let read_info in_chan =
    Scanf.fscanf in_chan "%i %i %i" (fun x y z -> dcnt := x; tcnt := y; wcnt := z)

let read_docs lst in_chan = 
    let from_line = fun line -> Scanf.sscanf line "%i %i" (fun x y -> ((if x >= 0 then x else !dcnt), y, Random.int kkk)) in
    let rec iter_read accum = 
        let stat, line = 
            try (true, (input_line in_chan))
            with End_of_file -> (false, "")
        in if stat == true then
            iter_read ((from_line line)::accum)
            else accum
    in iter_read lst;;

(*read dcnt, tcnt, wcnt*)
with_i (open_in "data/info") read_info;;

(*read docs*)
let doc_list = with_i (open_in "data/doc_list") (read_docs []);;
let doc_list = with_i (open_in "data/reduced_sample") (read_docs doc_list);;
dcnt := !dcnt + 1;;
wcnt := List.length doc_list;;
(*init matrixs of count*)
let otcnt = Array.init !tcnt (fun x -> Array.make kkk 0);;
let omcnt = Array.init !dcnt (fun x -> Array.make kkk 0);;
let nk = Array.make kkk 0;;
let nm = Array.make !dcnt 0;;
let ttpc = Array.make !tcnt [];;
let mtpc = Array.make !dcnt [];;
let tzerocnt = Array.make !tcnt 0;;
let mzerocnt = Array.make !dcnt 0;;


let clear_array_int a =
    Array.iteri (fun i x -> a.(i) <- 0) a;;

let clear_array_float a =
    Array.iteri (fun i x -> a.(i) <- 0.0) a;;


let clear_stats () =
    Array.iter (fun x -> clear_array_int x) omcnt;
    Array.iter (fun x -> clear_array_int x) otcnt;
    clear_array_int nk;
    clear_array_int nm;;


let do_init_stat d_lst = 
    List.iter (fun (m, t, z) -> 
            omcnt.(m).(z) <- omcnt.(m).(z) + 1;
            otcnt.(t).(z) <- otcnt.(t).(z) + 1;
            nk.(z) <- nk.(z) + 1;
            nm.(m) <- nm.(m) + 1) d_lst;;

(*get non-zero position list for array*)
let non_zero_list a =
    let rec for_i i accum =
        if i == kkk then accum
        else if a.(i) == 0 then
            for_i (i+1) accum
        else for_i (i+1) (i::accum)
    in for_i 0 [];;

(*non zero list of documents topics*)
let get_nonzerod () =
    let rec iter i =
        if i == !dcnt then ()
        else 
            begin
            mtpc.(i) <- non_zero_list omcnt.(i);
            iter (i+1)
            end
    in iter 0;;

(*non zero list of term topics*)
let get_nonzerot () =
    let rec iter i =
        if i == !tcnt then ()
        else 
            begin
            ttpc.(i) <- non_zero_list otcnt.(i);
            iter (i+1)
            end
    in iter 0;;

let _ = do_init_stat doc_list;;
let _ = get_nonzerod ();;
let _ = get_nonzerot ();;


let re_stat_init d_lst =
    print_endline "init_stat";
    clear_stats ();
    do_init_stat d_lst;
    get_nonzerod ();
    get_nonzerot ();
    wcnt := List.length d_lst;;


let print_line out_chan a =
    Array.iter (fun x -> Printf.fprintf out_chan "%d " x) a;
    Printf.fprintf out_chan "\n";;

let save rd =
    with_o (open_out ("save/r"^(string_of_int rd))) (fun out_chan ->
        print_endline "Saving";
        Printf.fprintf out_chan "%d %d %d\n" kkk !dcnt !tcnt;
        Array.iter (fun x -> print_line out_chan x) omcnt;
        Array.iter (fun x -> print_line out_chan x) otcnt
        );;

let default_p z = beta*.alpha/.((float_of_int nk.(z))+.beta*.(float_of_int !tcnt));;

let prob m t z = 
        ((float_of_int otcnt.(t).(z))+.beta)*.((float_of_int omcnt.(m).(z))+.alpha)/.((float_of_int nk.(z))+.beta*.(float_of_int !tcnt));;

let doc_dist m =
    Array.init kkk (fun x -> 
        ((float_of_int omcnt.(m).(x))+.alpha)/.((float_of_int nm.(m))+.(float_of_int kkk)*.alpha))

let word_dist m t =
    let ans = Array.init kkk (fun x -> prob m t x) in
    let sum = Array.fold_left (fun x y -> x+.y) 0.0 ans in
    Array.iteri (fun i x -> ans.(i) <- (x/.sum)) ans;
    ans;;

let d_kl px py =
    let rec for_iter ans i = 
        if i == Array.length px then ans
        else for_iter (ans+.px.(i)*.(log (px.(i)/.py.(i)))/.(log 2.0)) (i+1)
    in for_iter 0.0 0;;

let d_js px py =
    let a = d_kl px py in
    let b = d_kl py px in
    (a +. b)/. 2.;;

let d_cos px py =
    let rec for_iter ans i =
        if i == Array.length px then ans
        else for_iter (ans+.px.(i)*.py.(i)) (i+1)
    in 1.0-.(for_iter 0.0 0);;

let fdis = d_cos;;

let distances_to_sample () =
    let ed = !dcnt - 1 in
    let rec for_iter i accum =
        if i == ed then accum
        else for_iter (i+1) ((fdis (doc_dist ed) (doc_dist i))::accum)
    in List.rev (for_iter 0 []);;
            
let save_distances lst rd = 
    with_o (open_out ("save/distances"^(string_of_int rd))) (fun out_chan ->
                List.iter (fun dis -> Printf.fprintf out_chan "%f " dis) lst)

let s0 = Sampler.make default_p kkk;;

let to_set_with_list lst m t =
    List.iter (fun z -> Sampler.to_set s0 z (prob m t z)) lst

let clear_zero tpclist tpcstat =
    let rec for_iter accum = function
        z::rlft ->
            if tpcstat.(z) == 0 then
                for_iter accum rlft
            else for_iter (z::accum) rlft
        | [] -> accum
    in for_iter [] tpclist;;

let try_tpc_regen m t z nz = 
    if z == nz then ()
    else
        begin
        if omcnt.(m).(nz) == 1 then
            mtpc.(m) <- (nz::mtpc.(m));
        if omcnt.(m).(z) == 0 then
            begin
            mzerocnt.(m) <- mzerocnt.(m) + 1;
            if mzerocnt.(m) > 2 then
                begin
                mtpc.(m) <- (clear_zero mtpc.(m) omcnt.(m));
                mzerocnt.(m) <- 0
                end
            end;
        if otcnt.(t).(nz) == 1 then
            ttpc.(t) <- (nz::ttpc.(t));
        if otcnt.(t).(z) == 0 then
            begin
            tzerocnt.(t) <- tzerocnt.(t) + 1;
            if tzerocnt.(t) > 2 then
                begin
                ttpc.(t) <- (clear_zero ttpc.(t) otcnt.(t));
                tzerocnt.(t) <- 0
                end
            end
        end;;

let info_len p = -.(log p)/.(log 2.)

let mdfy_z m t z v =
    nk.(z) <- nk.(z) + v;
    nm.(m) <- nm.(m) + v;
    wcnt := !wcnt + v;
    omcnt.(m).(z) <- omcnt.(m).(z) + v;
    otcnt.(t).(z) <- otcnt.(t).(z) + v;;

let sample_one (pm, pt, _) (m, t, z) =
    let pre = if pm != m || pt != t then false else true in
    mdfy_z m t z (-1);
    if pre == false then
        begin
        Sampler.clear s0;
        Sampler.to_set s0 z (prob m t z);
        to_set_with_list ttpc.(t) m t;
        to_set_with_list mtpc.(m) m t;
        Sampler.with_stack s0
        end
    else Sampler.set s0 z (prob m t z);
    let nz = Sampler.gen s0 in
    let tmpsum = 
        s0.sums.(0)/.((float_of_int nm.(m))+.(float_of_int kkk)*.alpha)
    in
    mdfy_z m t nz 1;
    Sampler.set s0 nz (prob m t nz);
    try_tpc_regen m t z nz;
    (nz, info_len tmpsum);;


let reduce_round lst =
    println_int "before_reduce" (List.length lst);
    let base_dist = doc_dist (!dcnt-1) in
    let uniform_dis = fdis base_dist (uniform_k kkk) in
    let ans_lst = 
        let rec for_iter accum = function
        [] -> accum
        |(m, t, z)::rlft -> 
            let w_dist = word_dist m t in
            let cur_dis = fdis base_dist w_dist in
            for_iter ((m, t, z, cur_dis)::accum) rlft
        in for_iter [] lst
    in 
    let _ = with_o (open_out ("save/wdist"^(string_of_int (ncnts.(9) 1)))) (fun out_chan ->
                List.iter (fun (_, _, _, dis) -> Printf.fprintf out_chan "%f\n" dis) ans_lst)
    in
    let sum, len = List.fold_left (fun (x, l) (_, _, _, y) -> (x+.y, l+1)) (0.0, 0) ans_lst in
    let avr = sum/.(float_of_int len) in
    let ans =
        let rec for_iter accum = function
            [] -> accum
            | (m, t, z, dis)::rlft ->
                if dis > avr then
                    for_iter accum rlft
                else for_iter ((m, t, Random.int kkk)::accum) rlft
        in for_iter [] ans_lst
    in
    println_int "after_reduce" (List.length ans);
    println_float "norm" uniform_dis;
    re_stat_init ans;
    ans;;

    
let sample_gibbs_list its = 
    let rec round accum pre sum = function
    (m, t, z)::rlft -> 
        let nz, info = sample_one pre (m, t, z) in
        round ((m, t, nz)::accum) (m, t, z) (sum+.info) rlft
    | [] -> (accum, sum)
    in let lst, sum = round [] (-1, -1, -1) 0.0 its in
    println_float "sum" (sum/.(float_of_int !wcnt));
    lst;;

let run_list () =
    let rec for_round i its pre_time =
        Sampler.init s0 default_p;
        println_int "Round" i;
        let tmp = List.rev (sample_gibbs_list its) in
        if i mod 50 == 49 then save i;
        if i <= 5000 then
            begin
            let cur_time = (Sys.time()) in
            println_float "time" (cur_time -. pre_time);
            show_clear_cnts();
            let step = 500 in
            if i mod step == (step-1) then
                save_distances (distances_to_sample()) i;
            if i mod step == (step-1) && i <= 3000 then 
                for_round (i+1) (reduce_round tmp) cur_time
            else
                for_round (i+1) tmp cur_time
            end
        else tmp
        in for_round 0 doc_list (Sys.time());;


let _ = run_list();;

