type stack_element = 
    I of int
  | B of bool
  | S of string 
  | N of string
  | F of string
  | U
  | E

type command =
    PushI of stack_element
  | PushB of stack_element
  | PushS of stack_element
  | PushN of stack_element
  | Push of stack_element
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Pop
  | Swap
  | Concat
  | And
  | Or
  | Not
  | Equal
  | LessThan
  | Bind
  | If
  | Begin
  | End
  | Call
  | Fun of (stack_element * stack_element)
  | InOutFun of (stack_element * stack_element)
  | FunEnd
  | Return
  | Quit

type enviroment = (string * stack_element) list
type block = command list
type frame = enviroment list
type stack_frame = (stack_element list) list
type function_definitions = (string * block * string * bool * enviroment) list

(* helper functions can make the code more readable *)
let rec fetch' (v:string) (e:enviroment) : stack_element option = 
  match e with
  | [] -> None
  | (v', i) :: rest ->  if v = v' then 
      Some i 
    else 
      (fetch' v rest)

let rec fetch (v:string) (f:frame) : stack_element option =
  match f with
  | x::rest -> (
      match fetch' v x with
      | Some a -> Some a
      | None -> fetch v rest
    )
  | [] -> None

let set (v: string) (i:stack_element) (f:frame) : frame = 
  match f with
  | x::y -> ((v, i)::x)::y
  | [] -> [[(v, i)]]

let set_function (v: string) (operations: block) (arg: string) (t: bool) (func_env: enviroment) (c: function_definitions list) : function_definitions list =
  match c with
  | x::y -> ((v, operations, arg, t, [])::x)::y
  | [] -> [(v, operations, arg, t, [])::[]]

let rec fetch_function' (v: string) (c: function_definitions) : (block*string*bool*enviroment) option = 
  match c with
  | (name, operations, argument, t, func_env)::rest -> (
      if name = v then Some (operations, argument, t, func_env) else (fetch_function' v rest)
    )
  | [] -> None

let rec fetch_function (v: string) (c: function_definitions list) : (block*string*bool*enviroment) option = 
  match c with
  | x::rest -> (
      match fetch_function' v x with
      | Some a -> Some a
      | None -> fetch_function v rest
    )
  | [] -> None

let rec readlines' (ch : in_channel) : string list = 
  match input_line ch with
  | s -> s :: readlines' ch
  | exception  End_of_file -> []

let rec writelines (path : string) (ls : string list ) : unit =
  let rec loop (ch) (ls : string list ) : unit =
    match ls with
    | [] -> ()
    | x :: xs -> let _ = Printf.fprintf ch "%s\n" x in loop ch xs
  in 
  let ch = open_out path in
  let ()  = loop ch ls in
  let () = close_out ch in
  ()

let readlines (path : string) : string list =
  let ch = open_in path in
  let lines = readlines' ch in
  let _ = close_in ch in
  lines


let explode (s : string) : char list =
  let rec expl i l =
    if i < 0 
    then l
    else expl (i - 1) (String.get s i :: l)
  in expl (String.length s - 1) []

let implode (cl : char list) : string = 
  String.concat "" (List.map (String.make 1) cl)

let string_of_stack_element (stack_ele : stack_element) : string =
  match stack_ele with
  | I i -> string_of_int i
  | B b -> ("<"^string_of_bool b^">")
  | S s -> s
  | N n -> n
  | U -> "<unit>"
  | E -> "<error>"
  | _ -> "<error>"

let is_digit (c : char): bool = 
  Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let is_alpha (c : char): bool = 
  (Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z') || (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let rec take_while' (p : char -> bool) (es : char list) : (char list) * (char list) = 
  match es with
  | []      -> ([],[])
  | x :: xs -> if p x 
    then let (chars, rest) = take_while' p xs in (x :: chars, rest) 
    else ([],es) (* the lab8_before.ml contained a small bug here *)

let take_while (p : char -> bool) (s : string) : string * string = 
  let (echars, erest) = take_while' p (explode s) 
  in (implode echars, implode erest)

let drop_first (s : string) : string option = 
  match explode s with
  | [] -> None
  | _ :: rest -> Some (implode rest)

let parse_int (s : string) : int option = 
  match int_of_string s with
  | n -> Some n
  | exception _ -> None

let parse_string (s : string) : string option =
  if String.length s >= 1 && String.get s 0 = '"' && String.get s (String.length s - 1) = '"' then
    Some (String.sub s 1 ((String.length s) - 2)) 
  else None

let parse_name (s : string) : string option = 
  if String.length s >= 1 && ( let c = (String.get s 0) in is_alpha c ||  c = '_') then
    Some s 
  else None

let parse_constant (s : string) : stack_element = 
  let s' =  String.trim s in
  match s' with
  | "<true>" -> B true
  | "<false>" -> B false
  | "<unit>" -> U
  | _ -> match parse_int s' with
    | Some i -> I i
    | _ -> match parse_string s' with
      |Some s -> S s
      | _ -> match parse_name s' with
        | Some n -> N n
        | _ -> E

let parse_line (s:string) : command = 
  match take_while is_alpha (String.trim s) with
  | ("PushI", constant) -> PushI (parse_constant constant)
  | ("PushS", constant) -> PushS (parse_constant constant)
  | ("PushN", constant) -> PushN (parse_constant constant)
  | ("PushB", constant) -> PushB (parse_constant constant)
  | ("Push", constant) -> Push (parse_constant constant)
  | ("Add", _) -> Add
  | ("Sub", _) -> Sub
  | ("Mul", _) -> Mul
  | ("Div", _) -> Div
  | ("Rem", _) -> Rem
  | ("Neg", _) -> Neg
  | ("Pop", _) -> Pop
  | ("Swap", _) -> Swap
  | ("Concat", _) -> Concat
  | ("And", _) -> And
  | ("Or", _) -> Or
  | ("Equal", _) -> Equal
  | ("Not", _) -> Not
  | ("LessThan", _) -> LessThan
  | ("Bind", _) -> Bind
  | ("If", _) -> If
  | ("Begin", _) -> Begin
  | ("End", _) -> End
  | ("Fun", header) -> (
      match String.split_on_char ' ' (String.trim header) with
      |  name::arg::_ -> Fun ((parse_constant name) ,(parse_constant arg))
      |  _ -> Push E
    )
  | ("InOutFun", header) -> (
      match String.split_on_char ' ' (String.trim header) with
      |  name::arg::_ -> InOutFun ((parse_constant name) ,(parse_constant arg))
      | _ -> Push E
    )  
  | ("FunEnd", _) -> FunEnd
  | ("Return", _) -> Return
  | ("Call", _) -> Call
  | ("Quit", _) -> Quit
  | _ -> Push E

let rec parse_function (commands: block) (function_commands: block) (c: function_definitions list): (block * block * (function_definitions list)) =
  match commands with 
  | (Fun (N name, N arg))::rest -> (
      let (rem, func, closure) = parse_function rest [] c in
      let c = set_function name func arg false [] closure in
      parse_function rem function_commands c
    )
  | (InOutFun (N name, N arg))::rest -> (
      let (rem, func, closure) = parse_function rest [] c in
      let c = set_function name func arg true [] closure in
      parse_function rem function_commands c
    )
  | Return::FunEnd::rest -> (rest, function_commands@[Return], c)
  | FunEnd::rest -> (rest, function_commands@[FunEnd], c)
  | com::rest -> parse_function rest (function_commands@[com]) c 
  | [] -> (commands, function_commands, c)

let eval (commands : block) : stack_element list = 
  let resolve (stack_val: stack_element) (env: frame) : stack_element =
    match stack_val with 
    | N n -> (
        match (fetch n env) with 
        | Some a -> a
        | None -> N n
      )
    | _ -> stack_val
  in
  let insert (stack_val : stack_element) (stack : stack_frame) : stack_frame = 
    match stack with
    | [] -> [[stack_val]]
    | x::y -> (stack_val::x)::y 
  in
  let rec eval_wrapper (stack_val : stack_frame) (commands : block) (env : frame) (func_closures: function_definitions list): stack_element list = 
    match (commands, stack_val) with
    | (Quit :: _ , top::_) -> top
    | ([], top::_) -> top
    | ([], []) -> []
    | (PushI (I i) :: rest, _) -> eval_wrapper (insert (I i) stack_val) rest env func_closures 
    | (PushB (B b) :: rest, _) -> eval_wrapper (insert (B b) stack_val) rest env func_closures
    | (PushN (N n) :: rest, _) -> eval_wrapper (insert (N n) stack_val) rest env func_closures
    | (PushS (S s) :: rest, _) -> eval_wrapper (insert (S s) stack_val) rest env func_closures
    | (Push U :: rest, _) -> eval_wrapper (insert U stack_val) rest env func_closures
    | (Push E :: rest, _) -> eval_wrapper (insert E stack_val) rest env func_closures
    | (Add::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I y) -> eval_wrapper (insert (I (x+y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Sub::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I y) -> eval_wrapper (insert (I (x-y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Mul::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I y) -> eval_wrapper (insert (I (x*y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Div::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I 0) -> eval_wrapper (insert E stack_val) rest env func_closures
        | (I x, I y) -> eval_wrapper (insert (I (x/y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Rem::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I 0) -> eval_wrapper (insert E stack_val) rest env func_closures
        | (I x, I y) -> eval_wrapper (insert (I (x mod y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Neg::rest, (x::stack_rem)::lower) -> (
        match resolve x env with
        | I x -> eval_wrapper (insert (I (-x)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Pop::rest, (x::stack_rem)::lower) -> eval_wrapper (stack_rem::lower) rest env func_closures
    | (Swap::rest, (x::y::stack_rem)::lower) -> eval_wrapper ((y::x::stack_rem)::lower) rest env func_closures
    | (Concat::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with 
        | (S x, S y) -> eval_wrapper (insert (S (x^y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (And::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (B x, B y) -> eval_wrapper (insert (B (x&&y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Or::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (B x, B y) -> eval_wrapper (insert (B (x||y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Equal::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I y) -> eval_wrapper (insert (B (x=y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (LessThan::rest, (x::y::stack_rem)::lower) -> (
        match (resolve x env,resolve y env) with
        | (I x, I y) -> eval_wrapper (insert (B (x<y)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Not::rest, (x::stack_rem)::lower) -> (
        match resolve x env with
        | B x -> eval_wrapper (insert (B (not x)) (stack_rem::lower)) rest env func_closures
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Bind::rest, (x::y::stack_rem)::lower) -> (
        match x with 
        | N x -> (
            match y with 
            | N y -> (
                match (fetch y env) with 
                | Some a -> eval_wrapper (insert U (stack_rem::lower)) rest (set x a env) func_closures
                | None -> eval_wrapper (insert E stack_val) rest env func_closures
              )
            | E -> eval_wrapper (insert E stack_val) rest env func_closures
            | _ -> eval_wrapper (insert U (stack_rem::lower)) rest (set x y env) func_closures
          )
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (If::rest, (x::y::z::stack_rem)::lower) -> (
        match resolve z env with
        | B z -> (
            if z then 
              eval_wrapper (insert y (stack_rem::lower)) rest env func_closures
            else
              eval_wrapper (insert x (stack_rem::lower)) rest env func_closures
          )
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Begin::rest, _) -> (eval_wrapper ([]::stack_val) rest ([]::env) ([]::func_closures)) 
    | ((Fun (N identity, N arg))::rest, _) -> (
        let (rem, func, clos) = parse_function rest [] func_closures in
        let c = set_function identity func arg false [] clos in
        eval_wrapper (insert U stack_val) rem env c
      )
    | ((InOutFun (N identity, N arg))::rest, _) -> (
        let (rem, func, clos) = parse_function rest [] func_closures in
        let c = set_function identity func arg true [] clos in
        eval_wrapper (insert U stack_val) rem env c
      )
    | (Call::rest, (arg::func::stack_rem)::lower) -> (
        match (resolve func env) with 
        | N n -> (
            match fetch_function n func_closures with
            | Some (function_operations, argument, t, func_env) -> (
                if t then 
                  match arg with 
                  | N n -> (
                      let new_env = set n (N argument) env in
                      eval_wrapper ([]::stack_rem::lower) (function_operations @ rest) (set argument (resolve arg env) (func_env::new_env)) func_closures
                    )
                  | E -> (eval_wrapper (insert E stack_val) rest env func_closures)
                  | _ -> eval_wrapper (insert arg ([]::stack_rem::lower)) rest env func_closures

                else
                  match arg with
                  | E -> (eval_wrapper (insert E stack_val) rest env func_closures)
                  | _ -> (eval_wrapper ([]::stack_rem::lower) (function_operations @ rest) (set argument (resolve arg env) (func_env::env)) func_closures)
              )
            | None -> eval_wrapper (insert E stack_val) rest env func_closures
          )
        | _ -> eval_wrapper (insert E stack_val) rest env func_closures
      )
    | (Return::rest, (first::top)::stack_rem) -> (
        match env with
        | func_env'::x -> (
            let func_closures' = (
              match resolve first env with
              | N n -> (
                  match fetch_function n func_closures with
                  | Some (function_oper, argument, t, func_env) -> (
                      set_function n function_oper argument t (func_env'@func_env) func_closures
                    )
                  | None -> func_closures
                )
              | _ -> (func_closures)
            ) in
            match x with 
            | (t::_)::c -> (
                match t with 
                | (n, N n') -> (
                    eval_wrapper (insert (resolve first env) stack_rem) rest (set n (resolve (N n') env) x) func_closures'
                  )
                | _ -> eval_wrapper (insert (resolve first env) stack_rem) rest x func_closures'
              )
            | _ -> eval_wrapper (insert (resolve first env) stack_rem) rest x func_closures'
          )
        | [] -> eval_wrapper (insert (resolve first env) stack_rem) rest [] func_closures
      )
    | (FunEnd::rest, top::stack_rem) -> (
        match env with
        | _::x -> (
            match x with 
            | (t::_)::c -> (
                match t with 
                | (n, N n') -> (
                    eval_wrapper stack_rem rest (set n (resolve (N n') env) x) func_closures
                  )
                | _ -> eval_wrapper stack_rem rest x func_closures
              )
            | _ -> eval_wrapper stack_rem rest x func_closures
          )
        | [] -> eval_wrapper stack_rem rest [] func_closures
      )
    | (End::rest, (top::_)::stack_rest) -> ( 
        match func_closures with
        | _::c -> (
            match env with
            | _::x -> eval_wrapper (insert top stack_rest) rest x c
            | [] -> eval_wrapper (insert top stack_rest) rest [] c
          )
        | [] -> (
            match env with
            | _::x -> eval_wrapper (insert top stack_rest) rest x []
            | [] -> eval_wrapper (insert top stack_rest) rest [] []
          )
      )
    | (_::rest, _) -> eval_wrapper (insert E stack_val) rest env func_closures
  in eval_wrapper [] commands [] []

let () =
  (* Enable backtrace of functions for debugging. *)
  Printexc.record_backtrace true;; 


let interpreter (inputFile : string) (outputFile : string) : unit =
  let input_lines = (readlines inputFile) in
  let commands = List.map parse_line input_lines in
  let stack_out = eval commands in
  writelines outputFile (List.map string_of_stack_element stack_out)