open System

type Function = float -> float

let eps = 
    let rec calcEps eps =
        if 1.0 + eps / 2.0 > 1.0 then
            calcEps (eps / 2.0)
        else
            eps
    calcEps 1.0

let fi_function a =
    sqrt(1.0 - a) - tan a

let fi_iter_func a =
    1.0 - tan(a) * tan(a)

let fi_derivative a =
    -(cos a * cos a + 2.0 * sqrt(1.0 - a)) / (2.0 * sqrt(1.0 - a) * cos a * cos a)
 
let se_function a =
    a + cos(float a ** 0.52 + 2.0)

let se_iter_func a =
    -cos(float a ** 0.52 + 2.0)

let se_derivative a =
    1.0 - (13.0 * sin(float a ** 0.52 + 2.0)) / (25.0 * (float a ** 0.48))

let th_function a =
    3.0 * log a * log a + 6.0 * log a - 5.0

let th_iter_func a =
    exp((5.0 - 6.0 * log(a))/(3.0 * log(a)))

let th_derivative a =
    (5.0 * exp((5.0 - 6.0 * log(a))/(3.0 * log(a)))) / ( 3.0 * a * log(a) * log(a))

let dichotomy (f: Function) (a : float) (b : float) (my_eps : float) =
    let rec res a b cnt =
        if cnt > 100 then
            -1.0
        else
            let c = (a + b) / 2.0
            if abs(a - b) <= my_eps then
                c
            else
                let fa = f a
                let fc = f c
                if fa * fc < 0.0 then
                    res a c (cnt + 1)
                else
                    res c b (cnt + 1)
    res a b 0

let iterations (f: Function) (a : float) (b : float) (my_eps : float) =
    let rec res a b cnt =
        if cnt > 100 then
            -1.0 
        else
            let x0 = (a + b) / 2.0
            let x1 = f x0
            if abs(x1 - x0) <= my_eps then
                x1
            else if x1 < x0 then
                res a x1 (cnt + 1)
            else
                res x1 b (cnt + 1)
    res a b 0
    
let newton (f: Function) (f': Function) (a: float) (my_eps: float) =
    let rec newtonInner a cnt =
        if cnt > 100 then
            -1.0
        else
            let fa = f a
            if abs(fa) <= my_eps then
                a
            else
                let df = f' a
                let c = a - fa / df
                newtonInner c (cnt + 1)
    newtonInner a 0

let main() =

    let my_eps = 0.0001
    let a1, b1 = 0.0, 1.0
    let a2, b2 = 0.5, 2.0
    let a3, b3 = 1.5, 3.0

    let dichotomy_fi = dichotomy fi_function a1 b1 my_eps
    let iterations_fi = iterations fi_iter_func a1 b1 my_eps
    let newton_fi = newton fi_function fi_derivative a1 my_eps

    let dichotomy_se = dichotomy se_function a2 b2 my_eps
    let iterations_se = iterations se_iter_func a2 b2 my_eps
    let newton_se = newton se_function se_derivative a2 my_eps 

    let dichotomy_th = dichotomy th_function a3 b3 my_eps
    let iterations_th = iterations th_iter_func a3 b3 my_eps
    let newton_th = newton th_function th_derivative a3 my_eps 

    printfn "|   #\t|  Iterations\t|   Dichotomy\t|    Newton\t|"
    printfn "|   1\t| %s\t| %s\t| %s\t|" (if iterations_fi = -1.0 then "n/a\t" else sprintf "%.10f" iterations_fi) (if dichotomy_fi = -1.0 then "n/a" else sprintf "%.10f" dichotomy_fi) (if newton_fi = -1.0 then "n/a" else sprintf "%.10f" newton_fi)
    printfn "|   2\t| %s\t| %s\t| %s\t|" (if iterations_se = -1.0 then "n/a\t" else sprintf "%.10f" iterations_se) (if dichotomy_se = -1.0 then "n/a" else sprintf "%.10f" dichotomy_se) (if newton_se = -1.0 then "n/a" else sprintf "%.10f" newton_se)
    printfn "|   3\t| %s\t| %s\t| %s\t|" (if iterations_th = -1.0 then "n/a\t" else sprintf "%.10f" iterations_th) (if dichotomy_th = -1.0 then "n/a" else sprintf "%.10f" dichotomy_th) (if newton_th = -1.0 then "n/a" else sprintf "%.10f" newton_th)

main()