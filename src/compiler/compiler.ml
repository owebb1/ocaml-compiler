(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;

open Freshening;;

(*Helper to return the next-up offset as an argument type*)
let get_rbp_offset (env: environment) : argument = 
  let (arg, _, _ ) = env in 
  ArgMemory(AddrByRegisterOffset(RBP,arg))
;;

let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2))
;;

let get_true_arg_const : argument = 
  ArgConstant("0xFFFFFFFFFFFFFFFF")
;;

let get_false_arg_const : argument = 
  ArgConstant("0x7FFFFFFFFFFFFFFF")
;;



let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  (* GULL helper function*)
  let check_and_call_gc : instruction list = 
    (* *)

    let run_gc = (fresh_name "run_gc") in 
    let end_label = (fresh_name "end_gc_check") in 
    let instr : instruction list = [
      (* R8 has the predicted new heap cursor address *)
      (* AsmMov(ArgRegister(R14),ArgMemory(AddrByLabel("start_of_heap"))); *)

      AsmMov(ArgRegister(R15),ArgMemory(AddrByLabel("end_of_heap")));
      AsmLabel((fresh_name "compare_gc"));
      AsmCmp(ArgRegister(R8),ArgRegister(R15));
      (* if predicted heap cursor is greater than end of heap, run Garbage Collection*)
      AsmJg(run_gc);
      (* else jump to the end, nothing to be done here*)
      AsmJmp(end_label);

      AsmLabel(run_gc); 
      (* set the end of stack *)
      AsmMov(ArgMemory(AddrByLabel("end_of_stack")),ArgRegister(RSP));
      AsmMov(ArgRegister(R15),ArgMemory(AddrByLabel("heap_cursor")));
      AsmMov(ArgRegister(RDI),ArgRegister(R8));
      AsmSub(ArgRegister(RDI),ArgRegister(R15)); (* first argument - size of free bytes needed*)
      AsmCall("gc");


      (* Do the garbage collection*)
      AsmLabel(end_label);
      AsmMov(ArgRegister(R8),ArgMemory(AddrByLabel("heap_cursor"))); (*restore R8 invariance*)

    ] in 
    instr

  in

  (* Helper function to compare args *)
  (* Cardinal error checking helper functions*)
  let check_rax_int (env': environment) (e: expr) : instruction list =
    let instr1 : instruction list = (compile_expression env' e) in 

    let instr2 : instruction list =  [AsmMov(ArgRegister(R11),ArgRegister(RAX))] in (* save ans before checking*)

    (* check if answer is integer and save answer in RAX*)
    let instr3 : instruction list = [
      AsmShl(ArgRegister(RAX),ArgConstant("63")); 
      AsmMov(ArgRegister(R10), get_true_arg_const); (* R10 is needed to add/or 64-bit values on processor *)
      AsmXor(ArgRegister(RAX), ArgRegister(R10))
    ] in

    (* if else block to handle integer and non-integer case *)
    let int_error_label = fresh_name "int_error" in 
    let int_end_label = fresh_name "int_end" in 

    let instr4 : instruction list = [
      AsmMov(ArgRegister(R10),get_true_arg_const); (* set r10 to true*)
      AsmCmp(ArgRegister(RAX),ArgRegister(R10));
      AsmJne(int_error_label);
      AsmMov(ArgRegister(RAX),ArgRegister(R11)); (* restore answer to RAX register *)
      AsmJmp(int_end_label);
    ] in 

    let instr5 : instruction list = [
      AsmLabel(int_error_label);
      AsmMov(ArgRegister(RDI),ArgConstant("1")); (* push the argument for stopWithError i.e exit code *)
      AsmCall("stopWithError"); (* call the error handling function *)
      AsmLabel(int_end_label);
    ] in 


    instr1@instr2@instr3@instr4@instr5
  in

  let check_rax_bool (env': environment) (e:expr): instruction list = 
    let instr1 : instruction list = (compile_expression env' e) in 

    let instr2 : instruction list =  [
      AsmMov(ArgRegister(R12),ArgRegister(RAX))
    ] in (* save ans before checking*)

    (* check if answer is bool *)
    let expr_is_int : string = fresh_name "expr_is_int" in 
    let end_block : string = fresh_name "end_block" in 
    let instr3 : instruction list = [
      AsmMov(ArgRegister(R11),ArgRegister(RAX)); (* make a copy of the answer*)
      AsmShl(ArgRegister(R11),ArgConstant("63")); 
      (* R10 is needed to add/or 64-bit values on processor *)
      AsmMov(ArgRegister(R10), get_false_arg_const); 
      AsmOr(ArgRegister(R11), ArgRegister(R10));
      (* jump if value is an integer*)
      (* cmp ONLY works between REGISTERS!!! R10 has the value false!*)
      AsmCmp(ArgRegister(R11), ArgRegister(R10));
      AsmJe(expr_is_int);
      (* handle boolean and tuple case *)
      AsmShl(ArgRegister(RAX),ArgConstant("62"));
      AsmOr(ArgRegister(RAX), ArgRegister(R10)); (* should have the bool value*)
      AsmJmp(end_block);
      AsmLabel(expr_is_int);
      AsmMov(ArgRegister(RAX), get_false_arg_const);
      AsmLabel(end_block)

    ] in 

    (* if else block to handle integer and non-integer case *)
    let bool_error_label = fresh_name "bool_error" in 
    let bool_end_label = fresh_name "bool_end" in 

    let instr4 : instruction list = [
      AsmMov(ArgRegister(R10),get_true_arg_const); (* set r10 to true*)
      AsmCmp(ArgRegister(RAX),ArgRegister(R10));
      AsmJne(bool_error_label);
      AsmMov(ArgRegister(RAX),ArgRegister(R12)); (* restore answer to RAX register *)
      AsmJmp(bool_end_label);
    ] in 

    let instr5 : instruction list = [
      AsmLabel(bool_error_label);
      AsmMov(ArgRegister(RDI),ArgConstant("2")); (* push the argument for stopWithError i.e exit code *)
      AsmCall("stopWithError"); (* call the error handling function *)
      AsmLabel(bool_end_label);
    ] in 

    instr1@instr2@instr3@instr4@instr5
  in

  let check_rax_tuple (env': environment) (e:expr) : instruction list = 
    let instr1 : instruction list = (compile_expression env' e) in 
    let instr2 : instruction list =  [
      AsmMov(ArgRegister(R12),ArgRegister(RAX))
    ] in (* save ans before checking*)


    let expr_is_int : string = fresh_name "expr_is_int" in 
    let end_block : string = fresh_name "end_block" in 
    let instr3 : instruction list = [
      AsmMov(ArgRegister(R11),ArgRegister(RAX)); (* make a copy of the answer*)
      AsmShl(ArgRegister(R11),ArgConstant("63")); 
      (* R10 is needed to add/or 64-bit values on processor *)
      AsmMov(ArgRegister(R10), get_false_arg_const); 
      AsmOr(ArgRegister(R11), ArgRegister(R10));
      (* jump if value is an integer*)
      (* cmp ONLY works between REGISTERS!!! R10 has the value false!*)
      AsmCmp(ArgRegister(R11), ArgRegister(R10));
      AsmJe(expr_is_int);
      (* handle boolean and tuple case *)
      AsmShl(ArgRegister(RAX),ArgConstant("62"));
      AsmMov(ArgRegister(R10),ArgConstant("0xBFFFFFFFFFFFFFFF"));
      (* contains true if pointer else false*)
      AsmXor(ArgRegister(RAX), ArgRegister(R10)); 
      (* if expr is boolean (and not pointer ), skip to end (RAX contains False)*)
      AsmMov(ArgRegister(R10), get_false_arg_const);
      AsmCmp(ArgRegister(RAX), ArgRegister(R10));
      AsmJe(end_block);

      (* handle tuple vs closure case *)
      (* mv closure pointer away from safe reg *)
      AsmMov(ArgRegister(R13), ArgRegister(R12));
      (* subtract one to make machine ptr from bird ptr *)
      AsmSub(ArgRegister(R13), ArgConstant("1"));
      (* Find value at closure ptr i.e number of args *)
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R13)));
      (* mov high bit set value to R13 *)
      AsmMov(ArgRegister(R13), ArgConstant("0x8000000000000000"));
      (* And to check if high bit is set *)
      AsmAnd(ArgRegister(RAX), ArgRegister(R13));
      AsmMov(ArgRegister(R13), get_true_arg_const);
      (* RAX holds true if tuple, false if closure *)
      AsmXor(ArgRegister(RAX), ArgRegister(R13));

      AsmJmp(end_block);
      AsmLabel(expr_is_int);
      AsmMov(ArgRegister(RAX), get_false_arg_const);
      AsmLabel(end_block)
    ] in 
    (* if else block to handle integer and non-integer case *)
    let tuple_error_label = fresh_name "tuple_error" in 
    let tuple_end_label = fresh_name "tuple_end" in 

    let instr4 : instruction list = [
      AsmMov(ArgRegister(R10),get_true_arg_const); (* set r10 to true*)
      AsmCmp(ArgRegister(RAX),ArgRegister(R10));
      AsmJne(tuple_error_label);
      AsmMov(ArgRegister(RAX),ArgRegister(R12)); (* restore answer to RAX register *)
      AsmJmp(tuple_end_label);
    ] in 

    let instr5 : instruction list = [
      AsmLabel(tuple_error_label);
      AsmMov(ArgRegister(RDI),ArgConstant("3")); (* push the argument for stopWithError i.e exit code *)
      AsmCall("stopWithError"); (* call the error handling function *)
      AsmLabel(tuple_end_label);
    ] in 

    instr1@instr2@instr3@instr4@instr5

  in  
  let check_index_inbound (sizeReg: register) (indexReg: argument) : instruction list = 

    (* if else block to handle integer and non-integer case *)
    let tuple_idx_error_label = fresh_name "tuple_idx_error" in 
    let tuple_idx_end_label = fresh_name "tuple_idx_end" in 

    let instr1: instruction list = [
      AsmCmp(ArgMemory(AddrByRegister(sizeReg)),indexReg); (*deference and compare size*)

      (* Because we logically bitshift the indexReg by 1 prior to calling check_index_inbound,
         we are guarenteed to have a positive number whether the original index was positive or negative.
         If it was negative, we can guarentee that the 64th bit will be 0 and 63rd bit will be 1.
         This is a positive number much bigger than the tuple size ever can be. Note: A single entry 
         in a tuple is 8 bytes long, so longest tuple can be as long as 2^64 / 8 i.e. the number of bytes.*)
      AsmJg(tuple_idx_end_label); (* valid index*) 
    ] in 


    let instr2 : instruction list = [
      AsmLabel(tuple_idx_error_label);
      AsmMov(ArgRegister(RDI),ArgConstant("4")); (* push the argument for stopWithError i.e exit code *)
      AsmCall("stopWithError"); (* call the error handling function *)
      AsmLabel(tuple_idx_end_label);
    ] in 

    instr1 @ instr2

  in 

  let check_rax_closure (env': environment) (e:expr) : instruction list = 
    let instr1 : instruction list = (compile_expression env' e) in 
    let instr2 : instruction list =  [
      AsmMov(ArgRegister(R12),ArgRegister(RAX))
    ] in (* save ans before checking*)

    let expr_is_int : string = fresh_name "expr_is_int" in 
    let end_block : string = fresh_name "end_block" in 
    let instr3 : instruction list = [
      AsmMov(ArgRegister(R11),ArgRegister(RAX)); (* make a copy of the answer*)
      AsmShl(ArgRegister(R11),ArgConstant("63")); 
      (* R10 is needed to add/or 64-bit values on processor *)
      AsmMov(ArgRegister(R10), get_false_arg_const); 
      AsmOr(ArgRegister(R11), ArgRegister(R10));
      (* jump if value is an integer*)
      (* cmp ONLY works between REGISTERS!!! R10 has the value false!*)
      AsmCmp(ArgRegister(R11), ArgRegister(R10));
      AsmJe(expr_is_int);
      (* handle boolean and tuple case *)
      AsmShl(ArgRegister(RAX),ArgConstant("62"));
      AsmMov(ArgRegister(R10),ArgConstant("0xBFFFFFFFFFFFFFFF"));
      (* contains true if pointer else false*)
      AsmXor(ArgRegister(RAX), ArgRegister(R10)); 
      (* if expr is boolean (and not pointer ), skip to end (RAX contains False)*)
      AsmMov(ArgRegister(R10), get_false_arg_const);
      AsmCmp(ArgRegister(RAX), ArgRegister(R10));
      AsmJe(end_block);

      (* handle tuple vs closure case *)

      (* mv closure pointer away from safe reg *)
      AsmMov(ArgRegister(R13), ArgRegister(R12));
      (* subtract one to make machine ptr from bird ptr *)
      AsmSub(ArgRegister(R13), ArgConstant("1"));
      (* Find value at closure ptr i.e number of args *)
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R13)));
      (* mov high bit set value to R13 *)
      AsmMov(ArgRegister(R13), ArgConstant("0x8000000000000000"));
      (* And to check if high bit is set *)
      AsmAnd(ArgRegister(RAX), ArgRegister(R13));
      AsmMov(ArgRegister(R13), get_false_arg_const);
      (* RAX holds true if closure false if tuple *)
      AsmXor(ArgRegister(RAX), ArgRegister(R13));

      AsmJmp(end_block);
      AsmLabel(expr_is_int);
      AsmMov(ArgRegister(RAX), get_false_arg_const);
      AsmLabel(end_block)
    ] in 
    (* if else block to handle integer and non-integer case *)
    let closure_error_label = fresh_name "closure_error" in 
    let closure_end_label = fresh_name "closure_end" in 

    let instr4 : instruction list = [
      AsmMov(ArgRegister(R10),get_true_arg_const); (* set r10 to true*)
      AsmCmp(ArgRegister(RAX),ArgRegister(R10));
      AsmJne(closure_error_label);
      AsmMov(ArgRegister(RAX),ArgRegister(R12)); (* restore answer to RAX register *)
      AsmJmp(closure_end_label);
    ] in 

    let instr5 : instruction list = [
      AsmLabel(closure_error_label);
      AsmMov(ArgRegister(RDI),ArgConstant("5")); (* push the argument for stopWithError i.e exit code *)
      AsmCall("stopWithError"); (* call the error handling function *)
      AsmLabel(closure_end_label);
    ] in 

    instr1@instr2@instr3@instr4@instr5

  in  


  (* compares two expressions and sets the "compare" flag *)
  let compare_args_start (env : environment) (expr1 : expr) (expr2 : expr) : instruction list = 
    let instr1 : instruction list = (check_rax_int env expr1) in
    let (arg,env') : argument * environment = allocate_temp_variable env in  
    let instr2 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 
    let instr3 : instruction list = (check_rax_int env' expr2) in 
    let instr4 : instruction list = [ AsmCmp(arg,ArgRegister(RAX))] in
    let zero_out_temp: instruction list = [
      AsmMov(ArgRegister(R8),ArgConstant("0"));
      AsmMov(arg,ArgRegister(R8))
    ] in 
    instr1@instr2@instr3@instr4@zero_out_temp
  in

  (* *)
  let compare_args_end (label_1 : string) : instruction list = 
    let instr5 : instruction list = [AsmMov(ArgRegister(RAX), get_false_arg_const)] in
    let label_2 = fresh_name "end_compare" in
    let instr6 : instruction list = [AsmJmp(label_2)] in
    let instr7 : instruction list = [AsmLabel(label_1); 
                                     AsmMov(ArgRegister(RAX), get_true_arg_const)] in 
    let instr8 : instruction list = [AsmLabel(label_2)] in 
    instr5@instr6@instr7@instr8
  in

  match e with
  | EInt(num) -> 
    let argReg : argument = ArgRegister(RAX) in 
    let const : argument = ArgConstant(string_of_twice_int num) in
    [ AsmMov(argReg, const)]
  | EBool(boo) -> 
    ( match boo with
      | true -> 
        let argReg : argument = ArgRegister(RAX) in 
        let const : argument = get_true_arg_const in
        [AsmMov(argReg, const)]
      | false -> 
        let argReg : argument = ArgRegister(RAX) in 
        let const : argument = get_false_arg_const in
        [AsmMov(argReg, const)]
    )
  | EUnaryOp(op,e') -> 
    (match op with 
     | OpAfter -> 
       (* doubled for int representation *)
       (check_rax_int env e') @[ AsmAdd(ArgRegister(RAX),ArgConstant("2"))] 
     | OpBefore -> 
       (check_rax_int env e') @[ AsmSub(ArgRegister(RAX),ArgConstant("2"))]

     | OpIsInt -> (compile_expression env e') @ 
                  [AsmShl(ArgRegister(RAX),ArgConstant("63")); 
                   (* R10 is needed to add/or 64-bit values on processor *)
                   AsmMov(ArgRegister(R10), get_true_arg_const); 
                   AsmXor(ArgRegister(RAX), ArgRegister(R10))]
     | OpIsBool  -> 
       let expr_is_int : string = fresh_name "expr_is_int" in 
       let end_block : string = fresh_name "end_block" in 
       (compile_expression env e') @ [
         AsmMov(ArgRegister(R11),ArgRegister(RAX)); (* make a copy of the answer*)
         AsmShl(ArgRegister(R11),ArgConstant("63")); 
         (* R10 is needed to add/or 64-bit values on processor *)
         AsmMov(ArgRegister(R10), get_false_arg_const); 
         AsmOr(ArgRegister(R11), ArgRegister(R10));
         (* jump if value is an integer*)
         (* cmp ONLY works between REGISTERS!!! R10 has the value false!*)
         AsmCmp(ArgRegister(R11), ArgRegister(R10));
         AsmJe(expr_is_int);
         (* handle boolean and tuple case *)
         AsmShl(ArgRegister(RAX),ArgConstant("62"));
         AsmOr(ArgRegister(RAX), ArgRegister(R10)); (* should have the bool value*)
         AsmJmp(end_block);
         AsmLabel(expr_is_int);
         AsmMov(ArgRegister(RAX), get_false_arg_const);
         AsmLabel(end_block)
       ]
     | OpPrint ->
       let instr1: instruction list = compile_expression env e' in
       let setup_caller: instruction list = [
         AsmPush(ArgRegister(RAX)); (* save RAX since it is a volatile register*)
         AsmMov(ArgRegister(RDI),ArgRegister(RAX));
         AsmCall("printValue");
       ] in 
       (*TODO: add tear down if padding or more than six args*)

       let tear_down_caller: instruction list = [
         AsmPop(ArgRegister(RAX))
       ] in 
       instr1 @ setup_caller @ tear_down_caller
     | OpIsTuple ->
       let instr1 : instruction list = (compile_expression env e') in 
       let instr2 : instruction list =  [
         AsmMov(ArgRegister(R12),ArgRegister(RAX))
       ] in (* save ans before checking*)


       let expr_is_int : string = fresh_name "expr_is_int" in 
       let end_block : string = fresh_name "end_block" in 
       let instr3 : instruction list = [
         AsmMov(ArgRegister(R11),ArgRegister(RAX)); (* make a copy of the answer*)
         AsmShl(ArgRegister(R11),ArgConstant("63")); 
         (* R10 is needed to add/or 64-bit values on processor *)
         AsmMov(ArgRegister(R10), get_false_arg_const); 
         AsmOr(ArgRegister(R11), ArgRegister(R10));
         (* jump if value is an integer*)
         (* cmp ONLY works between REGISTERS!!! R10 has the value false!*)
         AsmCmp(ArgRegister(R11), ArgRegister(R10));
         AsmJe(expr_is_int);
         (* handle boolean and tuple case *)
         AsmShl(ArgRegister(RAX),ArgConstant("62"));
         AsmMov(ArgRegister(R10),ArgConstant("0xBFFFFFFFFFFFFFFF"));
         (* contains true if pointer else false*)
         AsmXor(ArgRegister(RAX), ArgRegister(R10)); 
         (* if expr is boolean (and not pointer ), skip to end (RAX contains False)*)
         AsmMov(ArgRegister(R10), get_false_arg_const);
         AsmCmp(ArgRegister(RAX), ArgRegister(R10));
         AsmJe(end_block);
         AsmLabel((fresh_name "closure_vs_tuple"));
         (* handle tuple vs closure case *)
         (* mv closure pointer away from safe reg *)
         AsmMov(ArgRegister(R13), ArgRegister(R12));
         (* subtract one to make machine ptr from bird ptr *)
         AsmSub(ArgRegister(R13), ArgConstant("1"));
         (* Find value at closure ptr i.e number of args *)
         AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(R13)));
         (* mov high bit set value to R13 *)
         AsmMov(ArgRegister(R13), ArgConstant("0x8000000000000000"));
         (* And to check if high bit is set *)
         AsmAnd(ArgRegister(RAX), ArgRegister(R13));
         AsmMov(ArgRegister(R13), get_true_arg_const);
         (* RAX holds true if tuple, false if closure *)
         AsmXor(ArgRegister(RAX), ArgRegister(R13));

         AsmJmp(end_block);
         AsmLabel(expr_is_int);
         AsmMov(ArgRegister(RAX), get_false_arg_const);
         AsmLabel(end_block)
       ] in 
       instr1@instr2@instr3
    )      
  | EVar(var) ->
    let arg: argument = find_named_variable var env in 
    [ AsmMov(ArgRegister(RAX),arg)]
  | EBinaryOp(op,expr1,expr2) ->
    (match op with 
     | OpPlus ->
       let instr1 : instruction list = (check_rax_int env expr1) in
       let (arg,env') : argument * environment = allocate_temp_variable env in  
       let instr2 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 
       let instr3 : instruction list = (check_rax_int env' expr2) in 
       let instr4 : instruction list = [ AsmAdd(ArgRegister(RAX),arg)] in
       let zero_out_temp: instruction list = [
         AsmMov(ArgRegister(R8),ArgConstant("0"));
         AsmMov(arg,ArgRegister(R8))
       ] in 
       instr1@instr2@instr3@instr4@zero_out_temp

     | OpMinus ->
       let instr1 : instruction list = (check_rax_int env expr1) in
       let (arg,env') : argument * environment = allocate_temp_variable env in  
       let instr2 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 
       let instr3 : instruction list = (check_rax_int env' expr2) in 
       (*add second second temporary var*)
       let (arg',_) : argument * environment = allocate_temp_variable env' in  
       let instr4 : instruction list = [ AsmMov(arg',ArgRegister(RAX))] in
       let instr5 : instruction list = [AsmMov(ArgRegister(RAX),arg)] in 
       let instr6 : instruction list = [ AsmSub(ArgRegister(RAX),arg')] in

       let zero_out_temp: instruction list = [
         AsmMov(ArgRegister(R8),ArgConstant("0"));
         AsmMov(arg,ArgRegister(R8));
         AsmMov(arg',ArgRegister(R8))
       ] in 
       instr1@instr2@instr3@instr4@instr5@instr6@zero_out_temp
     | OpTimes ->
       let instr1 : instruction list = (check_rax_int env expr1) in
       let (arg,env') : argument * environment = allocate_temp_variable env in  
       (* divide by 2 before multiplying --- bluebird int*)
       let instr2 : instruction list = [AsmShr(ArgRegister(RAX),ArgConstant("1"))] in
       let instr3 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 
       let instr4 : instruction list = (check_rax_int env' expr2) in 
       let instr5 : instruction list = [ AsmIMul(ArgRegister(RAX),arg)] in
       let zero_out_temp: instruction list = [
         AsmMov(ArgRegister(R8),ArgConstant("0"));
         AsmMov(arg,ArgRegister(R8))
       ] in 
       instr1@instr2@instr3@instr4@instr5@zero_out_temp
     | OpLessThan -> 
       let instr1 = compare_args_start env expr1 expr2 in 
       let label_1 = fresh_name "return_true" in 
       let instr2 : instruction list = [AsmJl(label_1)] in (* if statement*)
       let instr3 : instruction list = compare_args_end label_1 in (* else statement*)
       instr1@instr2@instr3
     | OpGreaterThan -> 
       let instr1 = compare_args_start env expr1 expr2 in 
       let label_1 = fresh_name "return_true" in 
       let instr2 : instruction list = [AsmJg(label_1)] in 
       let instr3 : instruction list = compare_args_end label_1 in
       instr1@instr2@instr3
     | OpEqualTo -> let instr1 = compare_args_start env expr1 expr2 in 
       let label_1 = fresh_name "return_true" in 
       let instr2 : instruction list = [AsmJe(label_1)] in 
       let instr3 : instruction list = compare_args_end label_1 in
       instr1@instr2@instr3
     | OpAnd -> 
       let instr1 : instruction list = (check_rax_bool env expr1) in
       let (arg,env') : argument * environment = allocate_temp_variable env in  
       let instr2 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 
       let instr3 : instruction list = (check_rax_bool env' expr2) in 
       let instr4 : instruction list = [ AsmAnd(ArgRegister(RAX),arg)] in
       let zero_out_temp: instruction list = [
         AsmMov(ArgRegister(R8),ArgConstant("0"));
         AsmMov(arg,ArgRegister(R8))
       ] in 
       instr1@instr2@instr3@instr4@zero_out_temp
     | OpOr -> let instr1 : instruction list = (check_rax_bool env expr1) in
       let (arg,env') : argument * environment = allocate_temp_variable env in  
       let instr2 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 
       let instr3 : instruction list = (check_rax_bool env' expr2) in 
       let instr4 : instruction list = [ AsmOr(ArgRegister(RAX),arg)] in
       let zero_out_temp: instruction list = [
         AsmMov(ArgRegister(R8),ArgConstant("0"));
         AsmMov(arg,ArgRegister(R8))
       ] in 
       instr1@instr2@instr3@instr4@zero_out_temp
     | OpTupleIndex ->
       let check_tuple : instruction list = check_rax_tuple env expr1 in 

       let (arg,env') : argument * environment = allocate_temp_variable env in  
       (* arg stores the answer from expr1 i.e. memory address of the tuple*)
       let store_ans : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in 

       (* the value i.e. the index of the tuple SHOULD be in RAX*)
       let check_index_type : instruction list = check_rax_int env' expr2 in 

       (* the index returned by compile expression is in bird index. convert to machine index*)
       let divide_by_two : instruction list = [AsmShr(ArgRegister(RAX),ArgConstant("1"))] in 

       let pre_check_idx : instruction list = [
         AsmMov(ArgRegister(R12),arg);
         AsmSub(ArgRegister(R12),ArgConstant("1"));
       ] in (* mov tuple address to this pointer*) 
       let zero_out_temp: instruction list = [
         AsmMov(ArgRegister(R8),ArgConstant("0"));
         AsmMov(arg,ArgRegister(R8))
       ] in 

       let check_idx_inbound : instruction list = (check_index_inbound (R12) (ArgRegister(RAX))) in

       let return_value : instruction list = [
         (* AsmSub(ArgRegister(R12),ArgConstant("1")); *)
         AsmAdd(ArgRegister(RAX),ArgConstant("2")); (* adjust index i.e. skip over tuple size and GC word*)

         AsmMov(ArgRegister(RAX),ArgMemory(AddrByRegisterProductOffset(R12,RAX,8)));

       ] in 


       check_tuple@store_ans@check_index_type@divide_by_two@pre_check_idx@zero_out_temp@check_idx_inbound@return_value
    )

  | ELet(str,expr1,expr2) ->
    let instr1: instruction list = (compile_expression env expr1) in 
    let rbp_offset: argument = get_rbp_offset env in
    let instr2 : instruction list =  [AsmMov(rbp_offset,ArgRegister(RAX))] in 
    let env' : environment = allocate_named_variable str env in  
    let instr3: instruction list = (compile_expression env' expr2) in  

    (* find the stack address where variable is stored*)
    let to_zero : argument  = find_named_variable str env' in
    (* set the stack address value to zero *) 
    let zero_out : instruction list = [
      AsmLabel((fresh_name "zero_out"));
      AsmMov(ArgRegister(R11),ArgConstant("0"));
      AsmMov(to_zero,ArgRegister(R11));
    ]
    in 

    instr1@instr2@instr3@zero_out

  | EIf(expr1, expr2, expr3) -> 
    let instr1 : instruction list = check_rax_bool env expr1 in
    let instr2 : instruction list = [AsmMov(ArgRegister(R10), get_true_arg_const)] in 
    let instr3 : instruction list = [AsmCmp(ArgRegister(RAX), ArgRegister(R10))] in
    let label_else = fresh_name "label_else" in
    let instr4 : instruction list = [AsmJne(label_else)] in
    let instr5 : instruction list = compile_expression env expr2 in
    let label_end = fresh_name "label_end" in
    let instr6 : instruction list = [AsmJmp(label_end); AsmLabel(label_else)] in
    let instr7 : instruction list = compile_expression env expr3 in
    let instr8 : instruction list = [AsmLabel(label_end)] in 

    instr1@instr2@instr3@instr4@instr5@instr6@instr7@instr8
    (*
  | ECall(fun_name,arg_list) -> 
    let setup : instruction list = [
      (* this is where we would save caller saved registers *)
      (* Ex: AsmPush(ArgRegister(RAX)); *)
    ]
    in

    let rec compile_args (arguments : expr list) : instruction list = 
      match arguments with
      | [] -> []
      | arg::rest ->  (compile_expression env arg) @ [AsmPush(ArgRegister(RAX))] @ (compile_args rest)
    in let args_instrs = compile_args arg_list in 

    let meat_instrs : instruction list = 
      [
        AsmCall("fun_" ^ fun_name)
      ] in


    let rec teardown_args (arguments : expr list) : instruction list =
      match arguments with
      | [] -> []
      | _::rest -> [AsmPop(ArgRegister(R10))] @ teardown_args rest
    in let args_teardown = teardown_args arg_list in 

    let teardown : instruction list = [
      (* This is where we would pop caller saved registers *)
      (* Ex: AsmPop(ArgRegister(RAX)); *)
    ] in

    setup @ args_instrs @ meat_instrs @ args_teardown @ teardown
    *)
  | EAppl(expr1, expr2, is_tail) -> 
    (* let instr1 : instruction list = compile_expression env expr1 in *)
    let instr1 : instruction list = check_rax_closure env expr1 in 
    let (closure_ptr,env') : argument * environment = allocate_temp_variable env in  
    let instr2 : instruction list =  [AsmMov(closure_ptr,ArgRegister(RAX))] in
    let instr3 : instruction list = compile_expression env' expr2 in
    let (arg,env'') : argument * environment = allocate_temp_variable env' in 
    let instr4 : instruction list =  [AsmMov(arg,ArgRegister(RAX))] in

    let label_call_function : string = fresh_name "call_function" in
    let label_create_new_closure : string = fresh_name "create_new_closure" in
    let label_appl_end : string = fresh_name "appl_end" in 
    let test_label : string = fresh_name "test_label" in
    (* let label_check_tail_call : string = fresh_name "check_tail_call" in *)
    let label_setup_args : string = fresh_name "setup_args" in
    let (num_params, env''') : argument * environment = allocate_temp_variable env'' in  
    (* Compare the num args + 1 with num params *)
    let instr5 : instruction list = [
      AsmLabel(test_label); (* TODO: Dont GET RID OF THIS *)
      AsmMov(ArgRegister(R9), closure_ptr);
      AsmSub(ArgRegister(R9), ArgConstant("1")); (* get machine ptr *)
      AsmMov(ArgRegister(R12), ArgMemory(AddrByRegister(R9))); (* get num args *)
      AsmMov(ArgRegister(R13), get_false_arg_const); (* get rid of high bit set *)
      AsmAnd(ArgRegister(R12), ArgRegister(R13));
      AsmMov(ArgRegister(R11), ArgMemory(AddrByLabelOffset("r9", 16))); (* get num params i.e skip over num arg & GC word*)
      AsmMov(num_params, ArgRegister(R11));
      AsmAdd(ArgRegister(R12), ArgConstant("1")); (* args + 1 *)
      AsmCmp(ArgRegister(R12), ArgRegister(R11)); (* args + 1 < params *)
      AsmJe(label_setup_args);
      AsmJmp(label_create_new_closure);
    ] in
    (* Create new closure *)
    let instr6 : instruction list = [
      AsmLabel(label_create_new_closure);
      (* copy over the old closure to new heap space *)
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByLabel("heap_cursor"))); (* new closure ptr *)


      (* update heap cursor location to end of the closure *)
      AsmMov(ArgRegister(R8), ArgRegister(RAX));
      AsmMov(ArgRegister(R13),ArgRegister(R12)); (* R12 has number or args + 1, need space for the new arg*)
      AsmAdd(ArgRegister(R13), ArgConstant("4")); (* start + (num_arg + 4) * 8 *)
      AsmIMul(ArgRegister(R13), ArgConstant("8"));
      AsmAdd(ArgRegister(R8), ArgRegister(R13)); (* predicted new heap cursor *)
      (* INVARIANCE: 
         - R8 MUST hold the predicted new heap cursor value BEFORE calling GC!
         - R8 MUST hold the confirmed new heap cursor value AFTER calling GC!
      *)
    ] in 
    let instr7 : instruction list = check_and_call_gc in 

    let instr8 : instruction list = [
      (* R8 has the current heap cursor *)
      AsmMov(ArgRegister(RAX),ArgRegister(R8));
      AsmAdd(ArgRegister(R8), ArgRegister(R13)); (* new heap cursor *)
      AsmMov(ArgMemory(AddrByLabel("heap_cursor")),ArgRegister(R8)); (* update the new cursor *)

      (* R12 contains num_args + 1. Num of bytes to copy is num_args + 4. So add 3 to R12 instead of 4*)
      (* Heap Layout:  numArgs | GCword | numParams | FuncPointer | args... *)
      AsmAdd(ArgRegister(R12), ArgConstant("3"));

      (* Set up regs for rep movsq *)
      AsmMov(ArgRegister(RSI), closure_ptr);(* get where closure bird ptr*)
      AsmSub(ArgRegister(RSI),ArgConstant("1")); (*convert to machine pointer -> src*)
      AsmMov(ArgRegister(RDI), ArgRegister(RAX)); (* dst *)
      AsmMov(ArgRegister(RCX), ArgRegister(R12));
      AsmRepMovsq;


      (* attach new argument into heap_cursor (R13) - 8 *)
      AsmMov(ArgRegister(R14), arg);
      AsmMov(ArgMemory(AddrByRegisterOffset(R8,-8)), ArgRegister(R14));

      (* Increment num_args + 1 *)
      AsmMov(ArgRegister(R14), ArgMemory(AddrByRegister(RAX)));
      AsmAdd(ArgRegister(R14), ArgConstant("1"));
      AsmMov(ArgMemory(AddrByRegister(RAX)), ArgRegister(R14));

      (* Convert RAX to bird ptr*)
      AsmAdd(ArgRegister(RAX), ArgConstant("1"));

      (* update heap curosr *)

      AsmJmp(label_appl_end);
    ] in

    let (num_arg_bytes,_) : argument * environment = allocate_temp_variable env''' in

    let setup_args_instr : instruction list = [
      AsmLabel(label_setup_args);
      (* Setup args for call *)

      (* Push last arg to stack *)
      AsmMov(ArgRegister(R14), arg);
      AsmPush(ArgRegister(R14));

      (* R11 already stores number of params *)
      (* Allocate stack space *)
      AsmSub(ArgRegister(R11), ArgConstant("1"));
      AsmIMul(ArgRegister(R11), ArgConstant("8"));
      AsmSub(ArgRegister(RSP), ArgRegister(R11));
      AsmMov(num_arg_bytes,ArgRegister(R11));

      (* Copy args from closure to stack *)
      AsmMov(ArgRegister(RSI), closure_ptr); (* src *)
      AsmSub(ArgRegister(RSI), ArgConstant("1"));
      AsmAdd(ArgRegister(RSI), ArgConstant("32")); (* 32-bytes to skip over before args start*)
      AsmMov(ArgRegister(RDI), ArgRegister(RSP)); (* dest *)
      AsmSub(ArgRegister(R12), ArgConstant("1")); (* number of arguments to push to the stack*)
      AsmMov(ArgRegister(RCX), ArgRegister(R12)); (* number of 8-byte words to copy *)
      AsmRepMovsq;
    ] in

    let start_tail_call : string = fresh_name "start_tail_call" in 
    let is_tail_instr = 
      (if is_tail = true then
         [AsmJmp(start_tail_call)]
       else
         [AsmJmp(label_call_function)]) in  
    let caller_num_params : int = get_num_params env in 
    (* Tail call *)
    (* check for params *)

    let tail_call_instr = [
      AsmLabel(start_tail_call);
      (* tail callee params are in num_params *)
      (* tail caller params are in env*)
      AsmMov(ArgRegister(R11), num_params);
      (* move number of caller params to R13*)
      AsmMov(ArgRegister(R13), ArgConstant(string_of_int caller_num_params));
      (* if caller_num_param < calle_num_param*)
      AsmCmp(ArgRegister(R13), ArgRegister(R11));
      (* then do not tail call*)
      AsmJl(label_call_function); 
      (* else tail call*)

      (* Copy callee params from caller stack to original stack *)
      AsmMov(ArgRegister(RSI), ArgRegister(RSP)); (* src *)
      AsmMov(ArgRegister(RDI), ArgRegister(RBP)); (* dest *)
      AsmAdd(ArgRegister(RDI), ArgConstant("16")); (* skip over old rbp and ret addr *)
      (* number of arguments to copy to the stack*)
      AsmMov(ArgRegister(RCX), num_params); (* number of 8-byte words to copy *)
      AsmRepMovsq;


      (* teardown *)
      AsmMov(ArgRegister(R13), closure_ptr);(* save temp variable *)

      (* mov oldrbp to save it *)
      AsmMov(ArgRegister(R14), ArgMemory(AddrByRegister(RBP)));

      (* increase rbp by 8 to remove old rpb but save ret addr*)
      AsmAdd(ArgRegister(RBP), ArgConstant("8"));

      (* update rsp to get rid of stack frame *)
      AsmMov(ArgRegister(RSP), ArgRegister(RBP));

      (* Establishes rbp back as old rbp before call *)
      AsmMov(ArgRegister(RBP), ArgRegister(R14));

      (* actually make the call *)
      AsmSub(ArgRegister(R13), ArgConstant("1")); (* sub one to get machine pointer *)
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(R13, 24))); (* func ptr is 24 bytes right from closure ptr*)


      (* JUMP TO FUNCTION *)
      AsmJmp("rax");


      (* made it to end of tail call *)
      AsmJmp(label_appl_end); 

    ] in 


    let instr9 : instruction list = [
      AsmLabel(label_call_function);

      (* actually make the call *)
      AsmMov(ArgRegister(R13), closure_ptr);
      AsmSub(ArgRegister(R13), ArgConstant("1")); (* sub one to get machine pointer *)
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(R13, 24))); (* func ptr is 24 bytes right from closure ptr*)

      AsmCall("rax"); (* CALLING A FUNCTION HERE, MAY CHANGE registers!*)


      (* move rsp back to teardown args *)
      AsmMov(ArgRegister(R11),num_arg_bytes); (* R11 is a caller-saved register (we don't save it)*)
      AsmAdd(ArgRegister(RSP), ArgRegister(R11));
      AsmPop(ArgRegister(R13));

    ] in
    let zero_out_temp: instruction list = [
      AsmLabel(label_appl_end);
      AsmLabel((fresh_name "zero_out_temp"));
      AsmMov(ArgRegister(R8),ArgConstant("0"));
      AsmMov(closure_ptr,ArgRegister(R8));
      AsmMov(arg,ArgRegister(R8));
      AsmMov(num_arg_bytes,ArgRegister(R8));
      AsmMov(num_params, ArgRegister(R8));
    ] in 

    instr1@instr2@instr3@instr4@instr5@instr6@instr7@instr8@setup_args_instr@is_tail_instr@tail_call_instr@instr9@zero_out_temp

  | ETuple(expr_list) ->
    (*Tuple Helper function*)
    let rec insert_tuples (expr_lst : expr list ) (heap_idx: int) (env3: environment): instruction list = 
      (* loop through the list of expressions *)
      (* for each expression, evaluate and insert the value *)
      (* at each iteration, pass in the next index *)
      (* *)
      match expr_lst with
      | [] -> []
      | tuple_expr::rest ->
        let eval_tuple_expr : instruction list = (compile_expression env3 tuple_expr) in 
        let body: instruction list = [
          AsmPop(ArgRegister(R10));
          (* dereference the value at r10 -> [r10] and move value in RAX into it*)
          AsmMov(ArgMemory(AddrByRegisterOffset(R10,heap_idx)),ArgRegister(RAX));

          (* put starting address back on the stack *)
          AsmPush(ArgRegister(R10));

        ] in 
        eval_tuple_expr@body@(insert_tuples rest (heap_idx+8) env3) 
    in

    let tuple_size: string = string_of_int ((List.length expr_list) + 2) in 

    let (tuple_start_addr,env'') : argument * environment = allocate_temp_variable env in  



    let setup_1 : instruction list = [
      AsmMov(ArgRegister(R8),ArgMemory(AddrByLabel("heap_cursor")));
      AsmMov(ArgRegister(R10),ArgRegister(R8)); (*save the starting cursor addr*)
      AsmMov(ArgRegister(R12),ArgConstant(tuple_size));
      (* heap_cursor + num_elements*8 (byte offset) *)
      AsmIMul(ArgRegister(R12),ArgConstant("8")); (* size of tuple in bytes *)
      (* AsmMov(ArgRegister(R10),ArgMemory(AddrByRegisterProductOffset(R10,R11,8)));  *)
      (* AsmMov(ArgRegister(R8),ArgRegister(R10)); *)
      AsmAdd(ArgRegister(R8), ArgRegister(R12)); (* shift the cursor to the right*)

    ] in 

    let call_gc: instruction list = check_and_call_gc in 

    let setup_2: instruction list = [
      (* heap curosor in R8 *)
      AsmMov(ArgRegister(R10),ArgRegister(R8)); (* move start of heap cursor to R10 AGAIN*)
      AsmAdd(ArgRegister(R8), ArgRegister(R12)); (* shfift heap cursor by tuple size*)
      AsmMov(ArgMemory(AddrByLabel("heap_cursor")),ArgRegister(R8)); (*update the heap cursor*)
      AsmMov(ArgRegister(R11),ArgConstant(tuple_size)); (* re-move size into R11 to get true size *)
      AsmSub(ArgRegister(R11),ArgConstant("2")); (* insert the size at the start of the tuple *)
      AsmMov(ArgMemory(AddrByRegister(R10)),ArgRegister(R11));
      AsmMov(ArgRegister(R11),ArgConstant("0")); (* for zeroing out*)
      AsmMov(ArgMemory(AddrByRegisterOffset(R10,8)),ArgRegister(R11)); (* Move zero to the GC word *)
      AsmMov(ArgRegister(R11),ArgRegister(R10));
      AsmOr(ArgRegister(R11),ArgConstant("1"));
      AsmMov(tuple_start_addr,ArgRegister(R11));
      AsmPush(ArgRegister(R10)); (* save the starting address of the tuple*)
    ] in 
    let heap_idx : int = 16 in (* tuple values start after tuple size and GC word *) 
    let insert_tuple_values : instruction list = insert_tuples expr_list heap_idx env'' in

    let tear_down : instruction list  = [
      AsmPop(ArgRegister(R10)); (**)
      AsmMov(ArgRegister(RAX),tuple_start_addr)
    ] in 
    let zero_out_temp: instruction list = [
      AsmMov(ArgRegister(R8),ArgConstant("0"));
      AsmMov(tuple_start_addr,ArgRegister(R8));
    ] in 
    setup_1@call_gc@setup_2@insert_tuple_values@tear_down@zero_out_temp

  | ESet(tuple,index,value) ->
    let check_tuple : instruction list = check_rax_tuple env tuple in 

    let (tuple_ptr,env') : argument * environment = allocate_temp_variable env in  
    (* arg stores the answer from expr1 i.e. memory address of the tuple*)
    let store_ans : instruction list =  [AsmMov(tuple_ptr,ArgRegister(RAX))] in 

    (* the value i.e. the index of the tuple SHOULD be in RAX*)
    let check_index_type : instruction list = check_rax_int env' index in 

    (* the index returned by compile expression is in bird index. convert to machine index*)
    let divide_by_two : instruction list = [AsmShr(ArgRegister(RAX),ArgConstant("1"))] in 

    (* *)
    let pre_check_idx : instruction list = [
      AsmMov(ArgRegister(R12),tuple_ptr); (* bird pointer to the tuple*)
      AsmSub(ArgRegister(R12),ArgConstant("1")); (* machine pointer to the tuple in R12*)
    ] in 

    let check_idx_inbound : instruction list = (check_index_inbound (R12) (ArgRegister(RAX))) in

    let (index_var,env'') : argument * environment = allocate_temp_variable env' in  

    (* store index so it does not get overwritten*)
    let store_index : instruction list = [ AsmMov(index_var,ArgRegister(RAX))] in 

    (* evaluate the value to set - value will be in RAX*)
    let compile_value : instruction list = (compile_expression env'' value) in 


    let set_value : instruction list = [
      AsmMov(ArgRegister(R13),index_var); 

      (* adjust index by 2 to skip over tuple size and GC in the heap layout *)
      AsmAdd(ArgRegister(R13),ArgConstant("2"));

      (* restore the tuple pointer *)
      AsmMov(ArgRegister(R12),tuple_ptr); (* bird pointer to the tuple*)
      AsmSub(ArgRegister(R12),ArgConstant("1")); (* machine pointer to the tuple in R12*)

      (* set the value at the given index*)
      AsmMov(ArgMemory(AddrByRegisterProductOffset(R12,R13,8)),ArgRegister(RAX));

    ] in 
    let zero_out_temp: instruction list = [
      AsmMov(ArgRegister(R8),ArgConstant("0"));
      AsmMov(tuple_ptr,ArgRegister(R8));
      AsmMov(index_var,ArgRegister(R8));
    ] in 
    check_tuple@store_ans@check_index_type@divide_by_two@pre_check_idx@check_idx_inbound@store_index@compile_value@set_value@zero_out_temp

;;

(* give a a list of instructions, lopp through to find the maximum offset from RBP*)
let rec get_stack_size (instr_list: instruction list) (max: int) : int = 
  (* largest negative number *)
  match instr_list with
  | [] -> max 
  | instr::rest ->
    match instr with 
    | AsmMov(ArgMemory(AddrByRegisterOffset(reg,offset)),_) ->
      (match reg with 
       | RBP ->
         if offset < max then
           get_stack_size rest offset
         else
           get_stack_size rest max
       | _ ->
         get_stack_size rest max
      )
    | _ ->
      get_stack_size rest max
;;

let compile_function_declaration (env : environment) (decl : declaration ) : instruction list =
  let (_, dict', _) = env in
  let param_env : environment = (16, dict',0) in
  match decl with
  | DFunction(id, param_lst, expr) -> 
    (* loop through param list and add values to enviorment *)

    (*reverse list to add the parameters in opposite order on the stack *)
    (* let rec reverse_list (l : string list) (a : string list) : string list = 
       match l with
       |  [] -> a
       | (x::xs) -> reverse_list xs (x::a)
       in  *)
    (* let rev_params = reverse_list param_lst []  *)
    (* in *)
    let rec loop_params (p_lst : string list) (env' : environment) : environment =
      (match p_lst with 
       | [] -> env'
       | param::rest -> 
         let env'' : environment = allocate_named_parameter param env' in 
         loop_params rest env''
      )
    in 
    let param_env : environment = loop_params param_lst param_env in 

    let (_, dict, params) = param_env in


    (* Seems fishy *)
    (* adjust to negative offsets for the rest of the function *)
    (* TODO: CHECK WITH PALMER *)
    let func_env : environment = (-8, dict, params) in (* reset env offset*)

    (* all function labels are prefixed with fun_ *)
    let label_instr : instruction list = [AsmLabel("fun_"^id)] in
    let body_instr : instruction list = compile_expression func_env expr in 
    let local_mem_size : int = get_stack_size body_instr 0 in
    let setup : instruction list = [
      AsmPush(ArgRegister(RBP));
      AsmMov(ArgRegister(RBP), ArgRegister(RSP));
      (* Add is because local_mem_size is a negative value *)
      AsmAdd(ArgRegister(RSP),ArgConstant(string_of_int local_mem_size));

      AsmLabel((fresh_name "before_zeroing"));

      (* GULL Zero out Stack *)
      AsmMov(ArgRegister(RCX),ArgConstant(string_of_int local_mem_size));
      AsmIMul(ArgRegister(RCX),ArgConstant("-1"));
      AsmShr(ArgRegister(RCX),ArgConstant("3")); (* convert from number of bits to number or 8-byte words to zero out*)

      AsmMov(ArgRegister(RAX),ArgConstant("0")); (* move 0x0..00 to Rax*)
      AsmMov(ArgRegister(RDI),ArgRegister(RSP)); (* start from RSP and zero out stack values*)
      AsmRepStosq;

      AsmLabel((fresh_name "after_zeroing"));

      (* Save all callee-saved registers *)
      AsmPush(ArgRegister(RBX));
      AsmPush(ArgRegister(R12));
      AsmPush(ArgRegister(R13));
      AsmPush(ArgRegister(R14));
      AsmPush(ArgRegister(R15));
    ]
    in 
    let teardown : instruction list = [
      AsmPop(ArgRegister(R15));
      AsmPop(ArgRegister(R14));
      AsmPop(ArgRegister(R13));
      AsmPop(ArgRegister(R12));
      AsmPop(ArgRegister(RBX));
      AsmMov(ArgRegister(RSP), ArgRegister(RBP));
      AsmPop(ArgRegister(RBP));
      AsmRet
    ]
    in 
    label_instr@setup@body_instr@teardown
;;

let compile_closure_definitions (decl : declaration) : instruction list =
  match decl with
  | DFunction(id, param_lst, _) ->
    let align_instruction : instruction list = [AsmAlign(8)] in
    let label_instruction :instruction list = [AsmLabel("closure_of_"^id)] in 
    let num_params : string = string_of_int (List.length param_lst) in
    let dq_instruction : instruction list = [AsmDq(["0x8000000000000000"; "0x0000000000000000"; num_params; "fun_"^id])] in
    align_instruction@label_instruction@dq_instruction
;;


let rec bind_var_to_closures (decl_lst: declaration list) (env : environment) : environment = 
  match decl_lst with
  | [] -> env
  | decl::rest -> 
    (match decl with
     | DFunction(id, _, _) -> 
       let env' : environment = allocate_named_function id env in
       bind_var_to_closures rest env'  
    )
;;


let compile_program (p : program) : string =
  match p with
  | Program(decl_lst, expr) -> 
    (* Declares closure for each function declaration in the data section of the assembly *)
    let closure_def_instrs : instruction list = (List.concat_map compile_closure_definitions decl_lst) in 
    let env : environment = bind_var_to_closures decl_lst empty_environment in
    (* concat_map just takes list of list and returns list*)
    let part_f = compile_function_declaration env in
    let func_instructions : instruction list = (List.concat_map part_f decl_lst) in

    let instructions = compile_expression env expr in
    let local_mem_size: int = get_stack_size instructions 0 in 
    let setup : instruction list = [
      AsmPush(ArgRegister(RBP));
      AsmMov(ArgRegister(RBP),ArgRegister(RSP));
      (* Add is because local_mem_size is a negative value *)
      AsmAdd(ArgRegister(RSP),ArgConstant(string_of_int local_mem_size));
      (* get the address of the heap where memory is allocated and put it in heap_cursor*)
      AsmMov(ArgMemory(AddrByLabel("heap_cursor")),ArgRegister(RDI));
      AsmMov(ArgMemory(AddrByLabel("start_of_heap")),ArgRegister(RDI));
      AsmMov(ArgMemory(AddrByLabel("end_of_heap")),ArgRegister(RSI));
      AsmMov(ArgMemory(AddrByLabel("start_of_stack")),ArgRegister(RBP));
    ] in 
    (* TODO: add callee-saved registers *)
    let tear_down: instruction list = [
      AsmMov(ArgRegister(RSP),ArgRegister(RBP));
      AsmPop(ArgRegister(RBP));
    ] in


    (*********** After FALCON (moved assembly setup from compile_to_assembly)********)


    let heap_setup : instruction list= [
      AsmSection(".data"); AsmAlign(8); 
      AsmLabel("heap_cursor"); AsmDq(["0"]);
      AsmLabel("start_of_heap"); AsmDq(["0"]);
      AsmLabel("end_of_heap"); AsmDq(["0"]);
      AsmLabel("start_of_stack"); AsmDq(["0"]);
      AsmLabel("end_of_stack"); AsmDq(["0"]);
    ] in 

    let instructions = setup  @ instructions @ tear_down @ [AsmRet] @ func_instructions in
    let instructions_asm = code_of_instruction_list instructions in 
    let heap_asm = code_of_instruction_list heap_setup in
    let closure_asm = code_of_instruction_list closure_def_instrs in

    heap_asm ^
    closure_asm^
    "section .text\n" ^
    "global bird_main\n" ^
    "global heap_cursor\n" ^ 
    "global start_of_heap\n" ^
    "global end_of_heap\n" ^
    "global start_of_stack\n" ^
    "global end_of_stack\n"^
    "bird_main:\n" ^
    "extern printValue\n" ^
    "extern stopWithError\n" ^ 
    "extern gc\n" ^
    instructions_asm ^
    "\n"
;;

let rec mark_expr (expr : expr) : expr = 
  match expr with
  | EAppl(expr1, expr2, _) ->
    EAppl(expr1, expr2, true)
  | EIf(e1, e2, e3) ->
    let e2' : expr = mark_expr e2 in
    let e3' : expr= mark_expr e3 in
    EIf(e1, e2', e3')
  | ELet(var, e1, e2) ->
    let e2' : expr = mark_expr e2 in
    ELet(var, e1, e2')
  | _ -> expr
;;

let mark_decl (decl : declaration) : declaration =
  match decl with
  | DFunction(id, params, expr) -> 
    let expr' : expr = mark_expr expr in
    DFunction(id, params, expr')
;;

(* will mark all tail calls as true *)
let mark_tail_call (p : program ) : program = 
  match p with
  | Program(decl_lst, expr) -> 
    let decl' : declaration list = List.map mark_decl decl_lst in 
    (* let expr' : expr = mark_expr expr in  *)
    Program(decl',expr)
;;


let compile_to_assembly_code (p : program) : string =
  let marked_p : program = mark_tail_call p in
  let instructions = compile_program marked_p in
  instructions 
;;
