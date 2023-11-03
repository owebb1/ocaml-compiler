(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | R10
  | RDI
  | RDX
  | RCX
  | R8
  | R9 
  | RBP
  | R11
  | RSI
  | RBX
  | R12
  | R13
  | R14
  | R15
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByLabel of string
  | AddrByRegisterProductOffset of register * register * int 
  | AddrByLabelOffset of string * int 
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * int
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmShl of argument * argument
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string 
  | AsmJl of string
  | AsmJg of string
  | AsmPush of argument
  | AsmPop of argument
  | AsmCall of string 
  | AsmSection of string
  | AsmAlign of int 
  | AsmDq of string list
  | AsmRet
  | AsmRepMovsq
  | AsmRepStosq
  | AsmMovq of argument * argument
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with
  | RAX -> "rax"
  | RSP -> "rsp"
  | R10 -> "r10"
  | RDI -> "rdi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | R8 -> "r8"
  | R9 -> "r9"
  | RBP -> "rbp"
  | R11 -> "r11"
  | RSI -> "rsi"
  | RBX -> "rbx"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | AddrByRegister(reg) -> "[" ^ (code_of_register reg) ^ "]"
  | AddrByRegisterOffset(reg, offset) -> 
    "[" ^ code_of_register reg ^ "+" ^ string_of_int offset ^ "]"
  | AddrByLabel(str) ->
    "[" ^ str ^ "]"
  | AddrByRegisterProductOffset(first,second,multiplier) ->
    "[" ^ (code_of_register first) ^ " + "
    ^ (code_of_register second) ^ " * " ^ (string_of_int multiplier) ^ "]"
  | AddrByLabelOffset(label, offset) -> 
    "[" ^ label ^ "+" ^ string_of_int offset ^ "]"
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with
  | ArgConstant(value) -> value 
  | ArgRegister(reg) -> code_of_register reg
  | ArgMemory(addr) -> code_of_address addr
  | ArgLabelOffset(label, offset) -> label ^"+"^(string_of_int offset)
;;

let string_creator (str: string) (arg1: argument) (arg2: argument) : string =
  str ^ " " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2)
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
*)
let code_of_instruction (instruction : instruction) : string =
  match instruction with
  | AsmAdd(arg1, arg2) -> 
    string_creator "add" arg1 arg2
  | AsmIMul(arg1, arg2) -> 
    string_creator "imul" arg1 arg2
  | AsmMov(arg1, arg2) -> 
    string_creator "mov" arg1 arg2
  | AsmSub(arg1, arg2) -> 
    string_creator "sub" arg1 arg2
  | AsmShl(arg1, arg2) -> 
    string_creator "shl" arg1 arg2
  | AsmShr(arg1, arg2) -> 
    string_creator "shr" arg1 arg2
  | AsmSal(arg1, arg2) -> 
    string_creator "sal" arg1 arg2
  | AsmSar(arg1, arg2) -> 
    string_creator "sar" arg1 arg2
  | AsmAnd(arg1, arg2) -> 
    string_creator "and" arg1 arg2
  | AsmOr(arg1, arg2) -> 
    string_creator "or" arg1 arg2
  | AsmXor(arg1, arg2) -> 
    string_creator "xor" arg1 arg2
  | AsmLabel(str)->
    str ^ ": "
  | AsmCmp(arg1, arg2) -> 
    string_creator "cmp" arg1 arg2
  | AsmJmp(str)->
    "jmp " ^ str
  | AsmJe(str)->
    "je " ^ str
  | AsmJne(str)->
    "jne " ^ str
  | AsmJg(str) ->
    "jg " ^ str
  | AsmJl (str) ->
    "jl " ^ str
  | AsmPush (arg) ->
    "push " ^ (code_of_argument arg)
  | AsmPop (arg) ->
    "pop " ^ (code_of_argument arg)
  | AsmCall (str) ->
    "call " ^ str
  | AsmSection (str) ->
    "section " ^ str
  | AsmAlign (num) ->
    "align " ^ string_of_int(num)
  | AsmDq (lst) ->
    "dq " ^ (String.concat ", " lst) 
  | AsmRet ->
    "  ret"
  | AsmRepMovsq ->
    "rep movsq"
  | AsmRepStosq ->
    "rep stosq"
  | AsmMovq(arg1, arg2) -> 
    string_creator "mov" arg1 arg2
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let code_of_instruction_list (instruction_list : instruction list) : string =
  let rec helper (instr_lst: instruction list) : string = 
    match instr_lst with
    | [] -> ""
    | instr :: rest ->
      code_of_instruction instr ^ "\n" ^ helper rest
  in helper instruction_list
;;
