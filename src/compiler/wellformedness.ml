open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;

(* Searches through the declared functions and see if name is declared *)
let rec func_undefined_error (decl_lst : declaration list) (arg_lst_length : int) (name : string) : string list =
  match decl_lst with 
  | [] -> ["Function " ^ name ^ " is not defined."]
  | DFunction(id, _, _):: rest -> 
    (* Comment out arity check *)
    (* DFunction(id, param_lst, _):: rest -> *)
    (* if id = name then
       let param_lst_length : int = List.length param_lst in
       if param_lst_length = arg_lst_length then
        []
       else
        ["Function "^ name ^" takes " ^ 
         (string_of_int param_lst_length) ^ 
         " arguments, but is called with " ^ 
         string_of_int arg_lst_length ^ 
         " arguments." ] 
       else 
       func_undefined_error rest arg_lst_length name
    *)
    if id = name then 
      []
    else
      func_undefined_error rest arg_lst_length name

;;

(* NO LONGER USED AFTER FALCON *)
(* Searches for function calls in an expr 
   let rec function_name_search (decl_lst : declaration list) (exp : expr) : string list =
   match exp with
   (*
   | ECall(name,arg_lst) ->
    (* ECall must eval all of it's arguments *)
    let rec eval_arg_lst (decl_lst : declaration list) (expr_lst :expr list ) : string list = 
      (match expr_lst with 
       | [] -> []
       | expr::rest -> 
         function_name_search decl_lst expr @ eval_arg_lst decl_lst rest
      )
    in (eval_arg_lst decl_lst arg_lst) @ func_undefined_error decl_lst (List.length arg_lst) name
    *)
   | EUnaryOp(_, expr) -> 
    function_name_search decl_lst expr
   | EBinaryOp(_, expr1, expr2) 
   | ELet (_, expr1, expr2) -> 
    function_name_search decl_lst expr1 @ function_name_search decl_lst expr2
   | EIf (expr1, expr2, expr3) ->
    function_name_search decl_lst expr1 @ 
    function_name_search decl_lst expr2 @
    function_name_search decl_lst expr3

   (* TODO: ADD EAppl and ETuple errors *)
   | _ -> []
   ;;
*)

(* Returns list of all the expresions in the declared functions*)
let rec get_decl_lst_exprs (decl_lst : declaration list): expr list =
  match decl_lst with 
  | [] -> []
  | DFunction(_, _, expr):: rest -> 
    [expr] @ get_decl_lst_exprs rest
;;


(* No longer used after falcon *)
(* Searches the expression list for undeclared function calls
   let rec get_undefined_func_error (decl_lst : declaration list) (expr_lst :expr list ) : string list = 
   match expr_lst with 
   | [] -> []
   | expr::rest -> 
    function_name_search decl_lst expr @ get_undefined_func_error decl_lst rest
   ;; *)


(* Checks for duplicates in a list of strings *)
let rec check_duplicates (dict : int Map.String.t) (lst : string list ) : string list = 
  match lst with
  | [] -> []
  | str::rest -> 
    (match Map.String.find_opt str dict with 
     | None -> check_duplicates (Map.String.add str 1 dict) rest
     | Some(_) -> [str] @ check_duplicates dict rest 
    )
;;


(* Checks the parameters for each function for duplicates *)
let rec check_params_duplicates (decl_lst : declaration list): string list =
  match decl_lst with 
  | [] -> []
  | DFunction(name, param_lst, _):: rest -> 
    let dups : string list = check_duplicates Map.String.empty param_lst in
    let start_str : string = "Function " ^ name ^ " declares duplicate parameter " in
    if List.length dups > 0 then 
      [start_str ^ (String.concat (".\n"^start_str) dups) ^"." ] @ check_params_duplicates rest
    else 
      [] @ check_params_duplicates rest
;;


(* Checks function names for duplicates and returns errors for ones that
   are caught *)
let check_function_duplicates (decl_lst : declaration list) : string list = 
  let rec get_function_names (decl_lst : declaration list): string list =
    match decl_lst with 
    | [] -> []
    | DFunction(name, _, _):: rest -> 
      [name] @ get_function_names rest
  in
  let function_names : string list = get_function_names decl_lst in 
  let dups : string list = check_duplicates Map.String.empty function_names in
  let start_str : string = "Duplicate definition of function " in
  if List.length dups > 0 then 
    [start_str ^ (String.concat (".\n"^start_str) dups) ^"." ] 
  else 
    []
;;


(* looks for variable usages in an expression recursivly *)
let rec helper (dict : int Map.String.t) (exp' : expr): string list = 
  match exp' with
  | EVar(name)-> 
    (match Map.String.find_opt name dict with 
     | None -> [name]
     | Some(_) -> [] 
    )
  | ELet(name, expr1, expr2) ->
    let dict' = Map.String.add name 1 dict in
    helper dict expr1 @ helper dict' expr2
  | EUnaryOp(_, expr) -> 
    helper dict expr
  | EBinaryOp(_, expr1, expr2) -> 
    helper dict expr1 @ helper dict expr2
  | EIf (expr1, expr2, expr3) ->
    helper dict expr1 @ 
    helper dict expr2 @
    helper dict expr3
  | ETuple(expr_lst) -> 
    let rec eval_tuple_elements (dict' : int Map.String.t) (expr_lst :expr list ) : string list = 
      (match expr_lst with 
       | [] -> []
       | expr'::rest -> 
         helper dict' expr' @ eval_tuple_elements dict' rest
      )
    in (eval_tuple_elements dict expr_lst)
  | EAppl (expr1, expr2, _) -> 
    helper dict expr1 @
    helper dict expr2

  | ESet (expr1, expr2, expr3) -> 
    helper dict expr1 @
    helper dict expr2 @
    helper dict expr3
    (*
  | ECall(_,arg_lst) ->
    (* ECall must eval all of it's arguments *)
    let rec eval_arg_lst (dict' : int Map.String.t) (expr_lst :expr list ) : string list = 
      (match expr_lst with 
       | [] -> []
       | expr'::rest -> 
         helper dict' expr' @ eval_arg_lst dict' rest
      )
    in (eval_arg_lst dict arg_lst)
    *)
  (* IN FUTURE DON't DO THIS, Use | EInt _ | EBool _ -> []*)
  | _ -> []
;;


(* Checks for use of variable that is not declared *)
let rec check_for_unbound_var_decl_lst (function_dict : int Map.String.t) (decl_lst : declaration list) : string list = 
  match decl_lst with
  | [] -> []
  | DFunction(_, arg_list, expr)::rest -> 
    let rec helper_args (dict: int Map.String.t) (arg_list : string list) : int Map.String.t =
      match arg_list with 
      | [] -> dict
      | name::rest -> 
        let dict' = Map.String.add name 1 dict in 
        helper_args dict' rest
    in
    let dict'' : int Map.String.t = helper_args function_dict arg_list
    in
    let unbound_vars : string list = (helper dict'' expr) @ check_for_unbound_var_decl_lst function_dict rest in
    let start_str : string = "Unbound variable " in 
    if List.length unbound_vars > 0 then 
      [start_str ^ (String.concat (".\n"^start_str) unbound_vars) ^"."]
    else 
      []
;;


(* Adds the function id's to the dictionary of bound variables *)
let rec add_function_names_to_dict (decl_lst: declaration list) (dict : int Map.String.t) : int Map.String.t = 
  match decl_lst with
  | [] -> dict
  | decl::rest -> 
    (match decl with
     | DFunction(id, _, _) -> 
       let dict' : int Map.String.t = Map.String.add id 1 dict in
       add_function_names_to_dict rest dict'  
    )
;;


(* This function produces a list of compile-time errors found in a program. *)
let check_program_for_errors (p : program) : string list =
  (*ignore p; failwith "TODO: check_program_for_errors" (*TODO: delete this line*)*)
  match p with 
  | Program (decl_lst, expr) -> 
    (* let fn_not_defined : string list = function_name_search decl_lst expr in
       fn_not_defined *)
    (* let all_exprs : expr list = (get_decl_lst_exprs decl_lst) @ [expr] in *)
    (* let func_errors : string list = get_undefined_func_error decl_lst all_exprs in *)
    let dup_param_errors : string list = check_params_duplicates decl_lst in
    let dup_func_errors : string list = check_function_duplicates decl_lst in



    (* This includes the function id's as "legal" variables *)
    let function_ids : int Map.String.t = add_function_names_to_dict decl_lst Map.String.empty in
    let args_unbound_var_errors : string list = (check_for_unbound_var_decl_lst function_ids decl_lst) in
    let main_expr_unbound_vars : string list = helper function_ids expr in
    let start_str : string = "Unbound variable " in 
    if List.length main_expr_unbound_vars > 0 then 
      let main_expr_errors : string list = [start_str ^ (String.concat (".\n"^start_str) main_expr_unbound_vars) ^"."] in
      dup_param_errors @ dup_func_errors @ args_unbound_var_errors @ main_expr_errors  
    else 
      dup_param_errors @ dup_func_errors @ args_unbound_var_errors
;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = check_program_for_errors p in
  if List.is_empty errors then () else raise (IllFormed errors);
;;


(* THIS IS THE OTHER TRY *)
(* Call each error function *)


(* Build up list of all exprs in program *)
(* let rec get_call_exprs (exp : expr ): expr list =
   match exp with 
    | ECall(name,arg_lst) ->
    (* ECall must eval all of it's arguments *)
      let rec eval_arg_lst (expr_lst :expr list ) : expr list = 
        (match expr_lst with 
        | [] -> []
        | expr::rest -> 
          get_call_expr  @ eval_arg_lst rest
        )
    in (eval_arg_lst arg_lst) 

   | EUnaryOp(_, expr) -> 
     [expr] @ get_call_exprs
   | EBinaryOp(_, expr1, expr2) 
   | ELet (_, expr1, expr2) -> 
    function_name_search decl_lst expr1 @ function_name_search decl_lst expr2
   | EIf (expr1, expr2, expr3) ->
    function_name_search decl_lst expr1 @ 
    function_name_search decl_lst expr2 @
    function_name_search decl_lst expr3
   | _ -> []
    ;; *)