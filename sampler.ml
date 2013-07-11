open Testutils

type t = {
    k : int;
    mutable f : int->float;
    p : float array;
    sums : float array;
    stats : bool array;
    mdfy_stack : int Stack.t
}

let init_sums a =
    Array.iteri (fun i x -> a.sums.(i) <- (a.f i)) a.sums;
    let rec for_iter i =
        match i with
        0 -> ()
        | i ->
            begin
            let t = (i-1)/2 in a.sums.(t) <- a.sums.(t) +. a.sums.(i);
            for_iter (i-1)
            end
        in for_iter (a.k - 1);;

let init a f =
    a.f <- f;
    Array.iteri (fun i x -> a.p.(i) <- (a.f i)) a.p;
    Array.iteri (fun i x -> a.stats.(i) <- false) a.stats;
    Stack.clear a.mdfy_stack;
    init_sums a;;

let make init_f n =
    let ans = {
    k = n;
    f = init_f;
    p = Array.init n init_f;
    sums = Array.init n init_f;
    stats = Array.make n false;
    mdfy_stack = Stack.create()
    } in
    init_sums ans;
    ans;;

let get_sum a idx =
    if idx >= a.k then 0.0
    else a.sums.(idx);;

let update_sum a idx =
    let left = idx*2 + 1 in
    let right = left + 1 in
    a.sums.(idx) <- (get_sum a left) +. (get_sum a right) +. a.p.(idx);;

let gen a =
    let rv = Random.float a.sums.(0) in
    let rec for_iter i rv =
        if i >= a.k then 0
        else
            let new_r = rv -. a.p.(i) in
            let left = i*2+1 in
            if new_r <= 0.0 || left >= a.k then i
            else if new_r <= a.sums.(left) then for_iter left new_r
            else for_iter (left+1) (new_r -. a.sums.(left))
    in for_iter 0 rv;;


let rec to_stack a idx =
    if a.stats.(idx) == true then ()
    else
        begin
            a.stats.(idx) <- true;
            to_stack a ((idx-1)/2);
            Stack.push idx a.mdfy_stack
        end;;

let to_set a idx fval =
    a.p.(idx) <- fval;
    to_stack a idx;;

let with_stack a =
    Stack.iter (update_sum a) a.mdfy_stack;;

let set a idx fval =
    a.p.(idx) <- fval;
    let rec deep_set idx =
        update_sum a idx;
        if idx == 0 then (
            a.stats.(idx) <- true;
            Stack.push idx a.mdfy_stack
        ) else if a.stats.(idx) == true then deep_set ((idx-1)/2)
        else (
            a.stats.(idx) <- true;
            deep_set((idx-1)/2);
            Stack.push idx a.mdfy_stack
        )
    in deep_set idx;;

let clear a =
    let rec for_iter ss =
        if Stack.is_empty ss then ()
        else
            let tmp = Stack.pop ss in (
                a.p.(tmp) <- a.f tmp;
                a.stats.(tmp) <- false;
                update_sum a tmp;
                for_iter ss
            )
    in for_iter a.mdfy_stack;;
