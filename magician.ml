type ident = string
type message = string

type atype =
  | TVar of ident
  | TFun of (atype list) * atype
  | TBool
  | TList of atype
  | TNil

type 'a algo = 
  | AFun of (atype * ident) list * 'a algo
  | AApp of 'a algo * ('a algo list)
  | AIf of 'a algo * 'a algo * 'a algo
  | ABool of bool
  | AList of 'a * 'a algo
  | AEmptyList
  | AAssignment of atype * ident * 'a algo
  | ASequence of 'a algo list
  | AVar of ident
  | AException of message
                    
let pile_vide = AAssignment (TBool, "pile_vide",
                             AFun ([(TList (TVar "a"),"P")], 
                                   AIf(AApp(AVar "is_empty", [AVar("P")]),
                                       ABool(true),
                                       ABool(false))))
                                  
let empiler = AAssignment (TList (TVar "a"), "empiler",
                           AFun ([(TList (TVar "a"),"P");(TVar("a"),"x")], AList (AVar ("x"), AVar ("P"))))
                   
let depiler = AAssignment (TList (TVar "a"), "depiler",
                           AFun ([(TList (TVar "a"),"P")], AIf(AApp(AVar "is_empty", [AVar("P")]),
                                                               AException("empty list"),
                                                               AApp(AVar "head", [AVar("P")]))))



let c_string_of_type = function
  | TBool -> "int"
  | TList _ -> "struct list"
  | TNil -> "void"
  | _ -> failwith "is not a type in c"

(* let rec type_of_algo ctx = function
 *   | AFun (_, body) -> type_of_algo ctx body
 *   | AApp (f, _) ->
 *      (match type_of_algo ctx f with
 *       | TFun (_, t) -> t)
 *   | AIf (_,a,_) -> type_of_algo ctx a
 *   | ABool _ -> TBool
 *   | AIsEmpty _ -> TList *)

let rec c_string_of_algo = function
  | ABool true -> "1"
  | ABool false ->"0"
  | AIf (a1, a2, a3) -> 
     let c_string_of_if_body = function
       | ASequence al -> 
          let last = List.hd (List.rev al)
          and rest = List.rev (List.tl (List.rev al)) in
          (String.concat ";" (List.map c_string_of_algo rest)) ^ ";return " ^ (c_string_of_algo last) ^ ";"
       | _ as a -> "return " ^ (c_string_of_algo a) ^ ";" in
    "if(" ^ (c_string_of_algo a1) ^ "){" ^ (c_string_of_if_body a2) ^ "}else{" ^ (c_string_of_if_body a3) ^ "}"
  | AAssignment (t, ident, AFun (params, body)) -> (c_string_of_type t) ^ " " ^ ident ^ "(" ^ (String.concat ","  (List.map (fun (t,p) -> (c_string_of_type t)^" "^p) params)) ^ "){" ^ (c_string_of_algo body)

let _ = 
  print_endline (c_string_of_algo pile_vide)
