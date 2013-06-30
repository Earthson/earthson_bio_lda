let println_float info x =
    print_string (info^": ");
    print_float x;
    print_endline "";;

let println_int info x =
    print_string (info^": ");
    print_int x;
    print_endline "";;

let rec println_int_list = function
    x::r ->
        print_int x;
        print_string " ";
        println_int_list r
    | [] -> print_string "#\n\n";;

let println_int_array a =
    Array.iter (fun x -> print_int x;print_string " ") a;
    print_string "$$\n\n";;

let counts = Array.make 10 0;;
let ncounts = Array.make 10 0;;

let countor n = fun i -> 
    let ans = counts.(n) in
    counts.(n) <- counts.(n) + i;
    ans;;

let ncountor n = fun i ->
    let ans = ncounts.(n) in
    ncounts.(n) <- ncounts.(n) + i;
    ans;;

let show_clear_cnts () =
    Array.iteri (fun i x -> println_int ("cnt"^(string_of_int i)) x) counts;
    Array.iteri (fun i x -> counts.(i) <- 0) counts;;

let cnts = Array.init 10 (fun i -> countor i);;
let ncnts = Array.init 10 (fun i -> ncountor i);;
