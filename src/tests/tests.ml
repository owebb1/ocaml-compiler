(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

open HatchCompiler;;

open Assembly;;

open Environment;;

open Compiler;;


let code_of_register_test_1 = 
  "code_of_register: rax" >:: fun _ ->
    let reg : register = RAX in 
    assert_equal "rax" (code_of_register reg)
;;

let code_of_register_test_2 = 
  "code_of_register: rsp" >:: fun _ ->
    let reg : register = RSP in 
    assert_equal "rsp" (code_of_register reg)
;;

let code_of_address_test_1 = 
  "code_of_address: register" >:: fun _ ->
    let reg : register = RAX in
    let addr : address = AddrByRegister(reg) in
    assert_equal "[rax]" (code_of_address addr)
;;

let code_of_address_test_2 = 
  "code_of_address: register offset" >:: fun _ ->
    let reg : register = RAX in
    let offset: int = -8 in 
    let addr : address = AddrByRegisterOffset(reg,offset) in
    assert_equal "[rax+-8]" (code_of_address addr)
;;

let code_of_argument_test_1 = 
  "code_of_argument: argument" >:: fun _ ->
    let reg : register = RAX in
    let offset: int = -8 in 
    let addr : address = AddrByRegisterOffset(reg,offset) in
    let arg : argument = ArgMemory(addr) in 
    assert_equal "[rax+-8]" (code_of_argument arg)
;;      

let code_of_argument_test_2 = 
  "code_of_argument: constant" >:: fun _ ->
    let const: argument = ArgConstant("test") in 
    assert_equal "test" (code_of_argument const)
;;  

let code_of_instruction_test_1 = 
  "code_of_instruction: add" >:: fun _ ->
    let arg2 : argument = ArgMemory(AddrByRegister(R12)) in 
    let arg1 : argument = ArgRegister(RAX) in 
    let instr : instruction = AsmAdd(arg1,arg2) in 
    assert_equal "add rax, [r12]" (code_of_instruction instr)
;;  

let code_of_instruction_test_2 = 
  "code_of_instruction: ret" >:: fun _ ->
    assert_equal "  ret" (code_of_instruction AsmRet)
;;  

let code_of_instruction_list_test_1 = 
  "code_of_instruction: add1" >:: fun _ ->
    let arg2 : argument = ArgMemory(AddrByRegister(R13)) in 
    let arg1 : argument = ArgRegister(RAX) in 
    let instr1 : instruction = AsmAdd(arg1,arg2) in 
    let instr2: instruction = AsmMov(arg1,arg2) in 
    let instr3 : instruction = AsmRet in 
    let instr_list:  instruction list = [instr1;instr2;instr3] in 
    let expected : string = 
      "add rax, [r13]\nmov rax, [r13]\n  ret\n" in 

    assert_equal ~printer:(fun x -> x) expected (code_of_instruction_list instr_list)
;;  

let code_of_instruction_list_test_2 = 
  "code_of_instruction: add2" >:: fun _ ->
    let arg2 : argument = ArgMemory(AddrByRegister(R13)) in 
    let arg1 : argument = ArgRegister(RAX) in 
    let instr1 : instruction = AsmIMul(arg1,arg2) in 
    let instr2: instruction = AsmSub(arg1,arg2) in 
    let instr3 : instruction = AsmRet in 
    let instr_list:  instruction list = [instr1;instr2;instr3] in 
    let expected : string = 
      "imul rax, [r13]\nsub rax, [r13]\n  ret\n" in 

    assert_equal ~printer:(fun x -> x) expected (code_of_instruction_list instr_list)
;;  

let allocate_named_variable_test = 
  "allocate_named_variable: env" >:: fun _ ->
    let arg : argument =  ArgMemory(AddrByRegisterOffset(RBP,-8)) in 
    let dict: argument Map.String.t = Map.String.add "hello" arg Map.String.empty in   
    let expected : environment = (-16, dict,0) in 
    assert_equal expected (allocate_named_variable "hello" empty_environment)

;;

let find_named_variable_test = 
  "find_named_variable: env" >:: fun _ ->
    let arg : argument =  ArgMemory(AddrByRegisterOffset(RBP,-8)) in 
    let dict: argument Map.String.t = Map.String.add "hello" arg Map.String.empty in   
    let env : environment = (-16, dict,0) in 
    assert_equal arg (find_named_variable "hello" env)
;;

let shl_test = 
  "shl: 8, 1" >:: fun _-> 
    let expected : string = "shl 8, 1" in
    let arg1 : argument = ArgConstant("8") in
    let arg2 : argument = ArgConstant("1") in 
    let instr : instruction =  AsmShl(arg1,arg2) in
    assert_equal expected (code_of_instruction instr)
;;
let sar_test = 
  "sar: 8, 1" >:: fun _-> 
    let expected : string = "sar 8, 1" in
    let arg1 : argument = ArgConstant("8") in
    let arg2 : argument = ArgConstant("1") in 
    let instr : instruction =  AsmSar(arg1,arg2) in
    assert_equal expected (code_of_instruction instr)
;;
let and_test = 
  "and: 8,1" >:: fun _-> 
    let expected : string = "and 8, 1" in
    let arg1 : argument = ArgConstant("8") in
    let arg2 : argument = ArgConstant("1") in 
    let instr : instruction =  AsmAnd(arg1,arg2) in
    assert_equal expected (code_of_instruction instr)
;;

let or_test = 
  "or: 8,1" >:: fun _-> 
    let expected : string = "or 8, 1" in
    let arg1 : argument = ArgConstant("8") in
    let arg2 : argument = ArgConstant("1") in 
    let instr : instruction =  AsmOr(arg1,arg2) in
    assert_equal expected (code_of_instruction instr)
;;
let xor_test = 
  "xor: 8,1" >:: fun _-> 
    let expected : string = "xor 8, 1" in
    let arg1 : argument = ArgConstant("8") in
    let arg2 : argument = ArgConstant("1") in 
    let instr : instruction =  AsmXor(arg1,arg2) in
    assert_equal expected (code_of_instruction instr)
;;

let label_test = 
  "label: begin_52" >:: fun _-> 
    let expected : string = "begin_52: " in
    let instr : instruction =  AsmLabel("begin_52") in
    assert_equal expected (code_of_instruction instr)
;;

let jmp_test = 
  "jmp begin_52" >:: fun _ -> 
    let expected : string = "jmp begin_52" in
    let instr : instruction = AsmJmp("begin_52") in
    assert_equal expected (code_of_instruction instr)
;;

let code_of_register_test_3 = 
  "code_of_register: r10" >:: fun _ ->
    let reg : register = R10 in 
    assert_equal "r10" (code_of_register reg)
;;


let get_stack_size_test = 
  "get_stack_size: -16" >:: fun _ ->
    let arg1 : argument = ArgMemory(AddrByRegisterOffset(RBP,-8)) in 
    let arg2 : argument = ArgConstant("2") in 
    let arg3 : argument = ArgMemory(AddrByRegisterOffset(RBP,-16)) in 
    let instr1 : instruction = AsmMov(arg1,arg2) in 
    let instr2: instruction = AsmMov(arg3,arg2) in 
    let instr3 : instruction = AsmRet in 
    let instr_list:  instruction list = [instr1;instr2;instr3] in 
    let expected : int = -16 in 
    assert_equal expected (get_stack_size instr_list 0)

let push_test = 
  "push: rbp" >:: fun _-> 
    let expected : string = "push rbp" in
    let arg1 : argument = ArgRegister(RBP) in
    let instr : instruction =  AsmPush(arg1) in
    assert_equal expected (code_of_instruction instr)
;;


(* Dove Experiments *)
let param_env_test = 
  "param env " >:: fun _ -> 
    let arg : argument =  ArgMemory(AddrByRegisterOffset(RBP,16)) in 
    let dict: argument Map.String.t = Map.String.add "hello" arg Map.String.empty in   
    let expected : environment = (24, dict,1) in 
    assert_equal expected (allocate_named_parameter "hello" param_empty_environment)


let all_tests =
  [
    (***************************** Unit tests*********************)
    (* Auklet *)
    code_of_register_test_1;
    code_of_register_test_2;
    code_of_address_test_1;
    code_of_address_test_2;
    code_of_argument_test_1;
    code_of_argument_test_2;
    code_of_instruction_test_1;
    code_of_instruction_test_2;
    code_of_instruction_list_test_1;
    code_of_instruction_list_test_2;
    allocate_named_variable_test;
    find_named_variable_test;

    (* Bluebird *)
    shl_test;
    and_test;
    or_test;
    xor_test;
    label_test;
    jmp_test;
    code_of_register_test_3;

    (* Cardinal *)
    get_stack_size_test;
    push_test;

    (* Dove *)
    param_env_test;

    (*******************Integration tests*********************)

    (* Auklet *)
    test_success "test_code/4.bird" "4";
    test_success "test_code/-4.bird" "-4";
    test_success "test_code/zero.bird" "0";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/before_4.bird" "3";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/before_zero.bird" "-1";
    test_success "test_code/let_test_1.bird" "10";
    test_success "test_code/let_test_2.bird" "10";
    test_success "test_code/plus1.bird" "2";
    test_success "test_code/plus2.bird" "6";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus2.bird" "-2";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/times1.bird" "8";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/compound_expr_1.bird" "0";
    test_success "test_code/compound_expr_2.bird" "92";
    (* test_compile_failure "test_code/a_plus_2.bird" 
       "Unbound variable a with environment: Next free offset: -16\nb stored at [rbp+-8]"; *)

    (* Bluebird *)
    test_success "test_code/addbignumber.bird" "4294967300";
    test_success "test_code/true.bird" "true";
    test_success "test_code/false.bird" "false";
    test_success "test_code/isbool_true.bird" "true";
    test_success "test_code/isbool_false.bird" "false";
    test_success "test_code/isint_true.bird" "true";
    test_success "test_code/isint_false.bird" "false";
    test_success "test_code/islessthan_true.bird" "true";
    test_success "test_code/islessthan_false.bird" "false";
    test_success "test_code/isgreaterthan_true.bird" "true";
    test_success "test_code/isgreaterthan_false.bird" "false";
    test_success "test_code/isequalto_true.bird" "true";
    test_success "test_code/isequalto_false.bird" "false";
    test_success "test_code/and_true.bird" "true";
    test_success "test_code/and_false.bird" "false";
    test_success "test_code/or_true.bird" "true";
    test_success "test_code/or_false.bird" "false";
    test_success "test_code/if_true_5.bird" "5";
    test_success "test_code/if_false_8.bird" "8";
    test_success "test_code/bluebird_compound_test_1.bird" "96";
    test_success "test_code/bluebird_compound_test_2.bird" "true";

    (* Cardinal *)
    test_success "test_code/print_1.bird" "5\n5";
    test_success "test_code/print_after_5.bird" "6\n6";
    test_success "test_code/print_inside.bird" "15\n19";
    test_runtime_failure "test_code/check_int_type_after.bird" 1;
    test_runtime_failure "test_code/check_int_type_before.bird" 1;
    test_runtime_failure "test_code/check_int_type_plus.bird" 1;
    test_runtime_failure "test_code/check_int_type_minus.bird" 1;
    test_runtime_failure "test_code/check_int_type_times.bird" 1;
    test_runtime_failure "test_code/check_int_type_less_than.bird" 1;
    test_runtime_failure "test_code/check_int_type_greater_than.bird" 1;
    test_runtime_failure "test_code/check_bool_type_and.bird" 2;
    test_runtime_failure "test_code/check_bool_type_or.bird" 2;
    test_runtime_failure "test_code/check_bool_type_if_else.bird" 2;


    (* Dove *)
    (* test_success "test_code/function_zero_params.bird" "5";
       test_success "test_code/function_one_param.bird" "20";
       test_success "test_code/function_two_params.bird" "-1";
       test_success "test_code/function_three_params.bird" "60";
       test_success "test_code/function_compound.bird" "1";
       test_success "test_code/function_compound_2.bird" "4\n3\n2\n1";

       test_compile_failure "test_code/duplicate_parameter.bird" 
       (
        "Function hello declares duplicate parameter x.\n"^
        "Function hello declares duplicate parameter y.\n"^
        "Function hello declares duplicate parameter z.");

       test_compile_failure "test_code/duplicate_function.bird"
       ("Duplicate definition of function f.");

       test_compile_failure "test_code/unbound_var.bird"
       ("Unbound variable a.");

       test_compile_failure "test_code/giant_compile_static_err.bird" 
       ("Function one_am declares duplicate parameter pain.\n"^
       "Duplicate definition of function one_am.\n"^
       "Unbound variable my_brain.\n"^
       "Unbound variable work.");

       test_success "test_code/function_argument_order.bird" "5\n4\n1";

       (* Eaaaagle*)
       test_success "test_code/tuple_test_1.bird" "(11, 17)";
       test_success "test_code/tuple_is_tuple_test.bird" "true";
       test_success "test_code/tuple_index_test.bird" "9";
       test_runtime_failure "test_code/tuple_runtime_error.bird" 4;
       test_runtime_failure "test_code/tuple_not_tuple.bird" 3;
       test_success "test_code/tuple_in_tuple.bird" "(true, false)";
       test_runtime_failure "test_code/tuple_negative_index.bird" 4;
       test_compile_failure "test_code/unbound_var_inside_tuple.bird" 
       ("Unbound variable badfn.");

       (* Falcon *)
       test_success "test_code/closure_tuple_arg.bird" "12";
       test_success "test_code/closure_as_parameter.bird" "5";
       test_success "test_code/closure_in_tuple.bird" "110";
       test_runtime_failure "test_code/excess_numb_args.bird" 5;
       test_compile_failure "test_code/closure_unbound_arg.bird" 
       "Unbound variable a.";
       test_success "test_code/check_tuple_vs_closure.bird" "false";
       test_runtime_failure "test_code/fail_on_closure_indexing.bird" 3;
       test_success "test_code/test_stack_alignment.bird" "(4, 8)";
       (* the test below may fail if func ptr address is defined at other addresses*)
       (* test_success "test_code/closure_definitions.bird" "<closure@0000000000401433>[2/3](1, 2, ?)"; *)

       (* Gull *)
       test_success "test_code/tuple_set.bird" "12";
       test_runtime_failure "test_code/tuple_set_not_tuple.bird" 3;
       test_runtime_failure "test_code/tuple_set_non_int_index.bird" 1;
       test_runtime_failure "test_code/tuple_set_invalid_index.bird" 4;
       test_success "test_code/tuple_set_compound_test.bird" "((1, 2, (3, 3)), 4, 5, 6)";
       test_success "test_code/zeroing.bird" "60";
       (* test_runtime_failure "test_code/loads_of_tuple.bird" 7; might even fail after GC *)
       test_success "test_code/cycle_tuple_memory.bird" "128";
       (* test_runtime_failure "test_code/use_tuple_memory.bird" 7;   *)
       test_success "test_code/cycle_closure_memory.bird" "8";
       (* test_runtime_failure "test_code/use_closure_memory.bird" 7; *)
       test_success "test_code/original_cycles.bird" "128";
       (* test_success "test_code/large_memory_gc_test.bird" "524288"; run this on a super computer  *)

       (* Hoopoe *)
       (* TODO: Uncomment to run longer tests*)
       (* test_success "test_code/stack_overflow.bird" "1"; *)
       test_success "test_code/mark_test.bird" "4\n4";
       (* test_success "test_code/parameter_test_tail_call.bird" "(10000003, true)"; *)
       test_success "test_code/too_many_params_tail_call.bird" "12";
       (* test_success "test_code/lots_of_stack_mem.bird" "1"; *)
       test_compile_failure "test_code/Eset_unbound_var.bird" 
       ("Unbound variable x.\n"^
       "Unbound variable z.\n"^
       "Unbound variable a.");
       test_success "test_code/plus_plus_tail_call.bird" "6"
    *)

    test_success "test_code/lexer_test.bird" "5\nfalse";
    test_success "test_code/parser_test.bird" "true";
    test_success "test_code/final.bird" "8\n20"
  ];; 




let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;

(*

let tuple = (10,new2,10) in 
(tuple[1]) (tuple[2])

*)