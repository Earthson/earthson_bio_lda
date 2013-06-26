
let counts = Array.make 10 0;;

let countor n = fun i -> 
        let ans = counts.(n) in
        counts.(n) <- counts.(n) + i;
        ans;;

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
