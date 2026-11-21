type term =
    | Var of string
    | Lambda of string * term
    | App of term * term

let example_ast = App (Lambda ("x", Var "x"), Var "y")

let rec string_of_term term =
    match term with
    | Var name -> name
    | Lambda (param, body) -> "(\\" ^ param ^ "." ^ string_of_term body ^ ")"
    | App (fn, arg) -> "(" ^ string_of_term fn ^ " " ^ string_of_term arg ^ ")"

let () = print_endline (string_of_term example_ast)
