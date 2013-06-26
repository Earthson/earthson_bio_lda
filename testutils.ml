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

let countor n = fun i -> 
        let ans = counts.(n) in
        counts.(n) <- counts.(n) + i;
        ans;;

let show_clear_cnts () =
    Array.iteri (fun i x -> println_int ("cnt"^(string_of_int i)) x) counts;
    Array.iteri (fun i x -> counts.(i) <- 0) counts;;

let cnt0 = countor 0;;
let cnt1 = countor 1;;
let cnt2 = countor 2;;
let cnt3 = countor 3;;
let cnt4 = countor 4;;
let cnt5 = countor 5;;
let cnt6 = countor 6;;
let cnt7 = countor 7;;
let cnt8 = countor 8;;
let cnt9 = countor 9;;
