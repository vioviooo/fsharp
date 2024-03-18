open System

let eps = 
    let rec calcEps eps =
        if 1.0 + eps / 2.0 > 1.0 then
            calcEps (eps / 2.0)
        else
            eps
    calcEps 1.0

let abs x = if x < 0.0 then -x else x

let rec factorial n =
    if n <= 1 then
        1.0
    else
        float n * factorial (n - 1)

let calcTerm x n =
    (-1.0 ** float n) * (4.0 ** float n) * (x ** (2.0 * float n))

let calcNewResult x result n =
    let term = calcTerm x n
    let denominator = factorial (2 * n)
    result + term / denominator, term / denominator

let rec dumTaylor x result n my_eps =
    let new_result, term = calcNewResult x result n
    if abs(term) < my_eps then
        new_result, n
    else
        dumTaylor x new_result (n + 1) my_eps

let calcNewRow x row0 x0 =
    (-1.0) * row0 * ((4.0 * x * x) / float((x0 + 1) * (x0 + 2)))

let rec taylorFunction x row0 taylor_val x0 num_of_operations my_eps =
    let new_row0 = calcNewRow x row0 x0
    let new_taylor_val = taylor_val + new_row0
    if abs(new_row0) < my_eps then
        new_taylor_val, num_of_operations
    else
        taylorFunction x new_row0 new_taylor_val (x0 + 2) (num_of_operations + 1) my_eps

let functionValue x =
    2.0 * (cos x * cos x - 1.0)

let rec printValues a b N step =
    if a <= b then
        let func_val = functionValue a
        let taylor, num_of_operations = taylorFunction a ((-4.0 * a * a) / 2.0) ((-4.0 * a * a) / 2.0) 2 1 eps
        let taylor_dum, num = dumTaylor a 0.0 1 eps
        printf "| %.3f\t|\t%.20f\t|\t%.20f\t|\t%d\t|\t%.20f\t|\t%d\t| \n" a func_val taylor num_of_operations taylor_dum num
        printValues (a + step) b N step

let main() =
    let N = 10 // number of segmentations of [a, b]
    let a = 0.0
    let b = 0.5
    let step = (b - a) / float N

    printf "\n|   x\t|\t  Built-in function\t|\t   Taylor function\t|   Iterations\t|\t   Naive Taylor      \t|   Iterations\t|\n"
    printValues a b N step

main()
