(*get sums of array a of length k*)
let get_sums a k =
    let sums = Array.copy a in
    let rec for_iter i =
        match i with
        0 -> sums
        | i -> 
            let t = (i-1)/2 in sums.(t) <- sums.(t) +. sums.(i);
            for_iter (i-1)
    in for_iter (k-1);;

(*
@parameters
f: int->float
    f x: for defalut values of position x
k: int
    length of values array

@return
val sample_gen : unit -> int = <fun>
    generate random variable
val set : int -> float -> unit = <fun>
    set index value: change the value without cache
val to_set : int -> float -> unit = <fun>
    to_set index val: add change to cache(stack)
val update_with_stack : unit -> unit = <fun>
    make the changes by "to_set" effective
val clear : unit -> unit = <fun>
    clear changes with stack
*)
let multi_sampler k f =
    let avals = Array.init k f in
    let sums = get_sums avals k in
    let stats = Array.make (Array.length avals) false in
    let mdfy_stack = Stack.create() in
    let sample_gen () = 
        let r_v = Random.float sums.(0) in
        let rec for_iter i r_v =
            if i >= k then 0
            else
                let new_r = r_v -. avals.(i) in
                let left = i*2 + 1 in
                if new_r <= 0.0 || left >= k then i
                else if new_r <= sums.(left) then for_iter left new_r
                else for_iter (left+1) (new_r-.sums.(left))
        in for_iter 0 r_v
    in let rec to_stack idx =
        if stats.(idx) == true then ()
        else
            begin
            stats.(idx) <- true;
            to_stack ((idx-1)/2);
            Stack.push idx mdfy_stack
            end
    in let get_sum idx = 
        if idx >= k then 0.0
        else sums.(idx)
    in let update_sum idx =
        let left = idx*2 + 1 in
        let right = left + 1 in
        sums.(idx) <- (get_sum left) +. (get_sum right) +. avals.(idx)
    in let update_with_stack () =
        Stack.iter update_sum mdfy_stack
    in let to_set idx fval =
        begin
        avals.(idx) <- fval;
        to_stack idx
        end
    in let set idx fval = 
        avals.(idx) <- fval;
        let rec deep_set idx = 
            update_sum idx;
            if idx == 0 then (stats.(idx) <- true;Stack.push idx mdfy_stack)
            else if stats.(idx) == true then deep_set ((idx-1)/2)
            else 
                begin
                stats.(idx) <- true;
                deep_set ((idx-1)/2);
                Stack.push idx mdfy_stack
                end
        in deep_set idx
    in let clear () =
        let rec for_iter ss =
            if Stack.is_empty ss then ()
            else
                let tmp = Stack.pop ss in
                begin
                    avals.(tmp) <- f tmp;
                    stats.(tmp) <- false;
                    update_sum tmp;
                    for_iter ss
                end
        in for_iter mdfy_stack
    in let show_sums () = sums
    in let show_stats () = stats
    in let show_vals () = avals
    in sample_gen, set, to_set, update_with_stack, clear, show_sums, show_stats, show_vals;;

(*testing*)
let test_f = function i -> (float_of_int (i+1))*.0.3
let gen, set, to_set, update_with_stack, clear, show_sums, show_stats, show_vals = multi_sampler 10 test_f;;
