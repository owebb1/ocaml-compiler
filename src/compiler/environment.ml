(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t * int;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty, 0);;



(* argument will be stored at +16 beause next instruction and old rbp will both
   be pushed *)
let param_empty_environment : environment = (16, Map.String.empty, 0);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_variable (name : string) (env : environment) : environment =
  let (offset,dict,params) = env in 
  let value : argument = ArgMemory(AddrByRegisterOffset(RBP,offset)) in 
  let dict' : argument Map.String.t = Map.String.add name value dict in
  let newOffset : int = (offset - 8) in 
  (newOffset,dict',params)
;;


(* A function to match a function name with it's closure address in the env *)
let allocate_named_function (name : string ) (env : environment) : environment =
  let (offset, dict, params) = env in
  let value : argument = ArgLabelOffset("closure_of_"^name, 1) in
  let dict' : argument Map.String.t = Map.String.add name value dict in
  (offset, dict', params)
;;


(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
  let (_,dict,_) = env in 

  match Map.String.find_opt name dict with 
  | None -> raise (UnboundVariable(name,env))
  | Some(arg) -> arg

;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  let (offset,dict,params) = env in 
  let newOffset : int = (offset - 8) in 
  let value : argument = ArgMemory(AddrByRegisterOffset(RBP,offset)) in 
  (value,(newOffset,dict,params))
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict,params) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  (Printf.sprintf "Number of Parameters: %d\n" params) ^
  string_of_mappings mappings
;;

(* DOVE Code *)

(* A function to allocate a space for a named parameter from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_parameter (name : string) (env : environment) : environment =
  let (offset,dict,params) = env in 
  let value : argument = ArgMemory(AddrByRegisterOffset(RBP,offset)) in 
  let dict' : argument Map.String.t = Map.String.add name value dict in
  let newOffset : int = (offset + 8) in 
  let params' : int = params + 1 in
  (newOffset,dict', params')
;;

let get_num_params (env : environment) : int =
  let (_, _, params) = env in
  params
;;