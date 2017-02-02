open Printf
open Types
open Pretty

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;

exception BindingError of string
let check_scope (e : (Lexing.position * Lexing.position) expr) : unit =
  let rec help e env =
    match e with
    | EBool _ -> ()
    | ENumber _ -> ()
    | EId (x, loc) ->
       (try ignore (List.assoc x env)
        with Not_found ->
             raise (BindingError(sprintf "The identifier %s, used at <%s>, is not in scope" x (string_of_pos loc))))
    | EPrim1(_, e, _) -> help e env
    | EPrim2(_, l, r, _) -> help l env; help r env
    | EIf(c, t, f, _) -> help c env; help t env; help f env
    | ELet(binds, body, _) ->
       let env2 =
         List.fold_left
           (fun env (x, e, loc) ->
             try
               let existing = List.assoc x env in
               raise (BindingError(sprintf "The identifier %s, defined at <%s>, shadows one defined at <%s>"
                                           x (string_of_pos loc) (string_of_pos existing)))
             with Not_found -> (x, loc)::env)
           env binds in
       help body env2
  in help e []

type tag = int
let tag (e : 'a expr) : tag expr =
  let rec help (e : 'a expr) (num : int) : (tag expr * tag) =
    match e with
    | EId(x, _) -> (EId(x, num), num + 1)
    | ENumber(n, _) -> (ENumber(n, num), num + 1)
    | EBool(b, _) -> (EBool(b, num), num + 1)
    | EPrim1(op, e, _) ->
       let (tag_e, new_n) = help e (num + 1) in
       (EPrim1(op, tag_e, num), new_n)
    | EPrim2(op, e1, e2, _) ->
       let (tag_e1, num_e1) = help e1 (num + 1) in
       let (tag_e2, num_e2) = help e2 (num_e1) in
       (EPrim2(op, tag_e1, tag_e2, num), num_e2)
    | ELet(binds, body, _) ->
       let (new_binds, num_binds) =
         List.fold_left
           (fun (rev_binds, next_num) (x, b, _) ->
             let (tag_b, num_b) = help b (next_num + 1) in
             ((x, tag_b, next_num)::rev_binds, num_b))
           ([], num + 1) binds in
       let (tag_body, num_body) = help body num_binds in
       (ELet(List.rev new_binds, tag_body, num), num_body)
    | EIf(cond, thn, els, _) ->
       let (tag_cond, num_cond) = help cond (num + 1) in
       let (tag_thn, num_thn) = help thn (num_cond) in
       let (tag_els, num_els) = help els (num_thn) in
       (EIf(tag_cond, tag_thn, tag_els, num), num_els)
  in let (ans, _) = help e 1
     in ans

let rec untag (e : 'a expr) : unit expr =
  match e with
  | EId(x, _) -> EId(x, ())
  | ENumber(n, _) -> ENumber(n, ())
  | EBool(b, _) -> EBool(b, ())
  | EPrim1(op, e, _) ->
     EPrim1(op, untag e, ())
  | EPrim2(op, e1, e2, _) ->
     EPrim2(op, untag e1, untag e2, ())
  | ELet(binds, body, _) ->
     ELet(List.map(fun (x, b, _) -> (x, untag b, ())) binds, untag body, ())
  | EIf(cond, thn, els, _) ->
     EIf(untag cond, untag thn, untag els, ())

let anf (e : tag expr) : unit expr =
  let rec helpC (e : tag expr) : (unit expr * (string * unit expr) list) =
    match e with
    | EPrim1(op, arg, _) ->
       let (arg_imm, arg_setup) = helpI arg in
       (EPrim1(op, arg_imm, ()), arg_setup)
    | EPrim2(op, left, right, _) ->
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (EPrim2(op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf(cond, _then, _else, _) ->
       let (cond_imm, cond_setup) = helpI cond in
       (EIf(cond_imm, anf _then, anf _else, ()), cond_setup)
    | ENumber(n, _) -> (ENumber(n, ()), [])
    | EBool(b, _) -> (EBool(b, ()), [])
    | ELet([], body, _) -> helpC body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpC (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EId(name, _) -> (EId(name, ()), [])
  and helpI (e : tag expr) : (unit expr * (string * unit expr) list) =
    match e with
    | EPrim1(op, arg, tag) ->
       let tmp = sprintf "unary_%d" tag in
       let (arg_imm, arg_setup) = helpI arg in
       (EId(tmp, ()), arg_setup @ [(tmp, EPrim1(op, arg_imm, ()))])
    | EPrim2(op, left, right, tag) ->
       let tmp = sprintf "binop_%d" tag in
       let (left_imm, left_setup) = helpI left in
       let (right_imm, right_setup) = helpI right in
       (EId(tmp, ()), left_setup @ right_setup @ [(tmp, EPrim2(op, left_imm, right_imm, ()))])
    | EIf(cond, _then, _else, tag) ->
       let tmp = sprintf "if_%d" tag in
       let (cond_imm, cond_setup) = helpI cond in
       (EId(tmp, ()), cond_setup @ [(tmp, EIf(cond_imm, anf _then, anf _else, ()))])
    | ENumber(n, _) -> (ENumber(n, ()), [])
    | EBool(b, _) -> (EBool(b, ()), [])
    | ELet([], body, _) -> helpI body
    | ELet((bind, exp, _)::rest, body, pos) ->
       let (exp_ans, exp_setup) = helpC exp in
       let (body_ans, body_setup) = helpI (ELet(rest, body, pos)) in
       (body_ans, exp_setup @ [(bind, exp_ans)] @ body_setup)
    | EId(name, _) -> (EId(name, ()), [])
  and anf e =
    let (ans, ans_setup) = helpI e in
    List.fold_right (fun (bind, exp) body -> ELet([bind, exp, ()], body, ())) ans_setup ans
  in
  anf e
;;


let r_to_asm (r : reg) : string =
  match r with
  | EAX -> "eax"
  | EDX -> "edx"
  | ESP -> "esp"
  | EBP -> "ebp"

let rec arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%d" n
  | HexConst(n) -> sprintf "0x%x" n
  | Reg(r) -> r_to_asm r
  | RegOffset(n, r) ->
     if n >= 0 then
       sprintf "[%s+%d]" (r_to_asm r) (word_size * n)
     else
       sprintf "[%s-%d]" (r_to_asm r) (-1 * word_size * n)
  | Sized(size, a) ->
     sprintf "%s %s"
             (match size with | DWORD_PTR -> "DWORD" | WORD_PTR -> "WORD" | BYTE_PTR -> "BYTE")
             (arg_to_asm a)

let rec i_to_asm (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
     sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IAdd(dest, to_add) ->
     sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
  | ISub(dest, to_sub) ->
     sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
  | IMul(dest, to_mul) ->
     sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
  | ICmp(left, right) ->
     sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
  | ILabel(name) ->
     name ^ ":"
  | IJo(label) ->
     sprintf "  jo %s" label
  | IJe(label) ->
     sprintf "  je %s" label
  | IJne(label) ->
     sprintf "  jne %s" label
  | IJl(label) ->
     sprintf "  jl %s" label
  | IJle(label) ->
     sprintf "  jle %s" label
  | IJg(label) ->
     sprintf "  jg %s" label
  | IJge(label) ->
     sprintf "  jge %s" label
  | IJmp(label) ->
     sprintf "  jmp %s" label
  | IJz(label) ->
     sprintf "  jz %s" label
  | IJnz(label) ->
     sprintf "  jnz %s" label
  | IAnd(dest, value) ->
     sprintf "  and %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IOr(dest, value) ->
     sprintf "  or %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IXor(dest, value) ->
     sprintf "  xor %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IShl(dest, value) ->
     sprintf "  shl %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IShr(dest, value) ->
     sprintf "  shr %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | ISar(dest, value) ->
     sprintf "  sar %s, %s" (arg_to_asm dest) (arg_to_asm value)
  | IPush(value) ->
     sprintf "  push %s" (arg_to_asm value)
  | IPop(dest) ->
     sprintf "  pop %s" (arg_to_asm dest)
  | ICall(label) ->
     sprintf "  call %s" label
  | IRet ->
     "  ret"
  | ITest(arg, comp) ->
     sprintf "  test %s, %s" (arg_to_asm arg) (arg_to_asm comp)
  | ILineComment(str) ->
     sprintf "  ;; %s" str
  | IInstrComment(instr, str) ->
     sprintf "%s ; %s" (i_to_asm instr) str

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let rec find (ls : (string * 'a) list) (x : string) : 'a =
  match ls with
  | [] -> failwith (sprintf "Name %s not found" x)
  | (y,v)::rest ->
     if y = x then v else find rest x

(* NOTE: Assumes that e is in ANF *)
let rec count_vars (e : 'a expr) =
  match e with
  | EIf(_, t, f, _) -> max (count_vars t) (count_vars f)
  | ELet([_, b, _], body, _) ->
     1 + (max (count_vars b) (count_vars body))
  | _ -> 0

let rec replicate (x : 'a) (i : int) : 'a list =
  if i = 0 then []
  else x :: (replicate x (i - 1))

let rec compile_expr (e : tag expr) (si : int) (env : (string * int) list) : instruction list =
    match e with
    | ELet([id, e, _], body, _) ->
        let prelude = compile_expr e (si + 1) env in
        let body = compile_expr body (si + 1) ((id, si)::env) in
        prelude @ [ IMov(RegOffset(~-si, EBP), Reg(EAX)) ] @ body
    | EPrim1 (op, e, t) ->
        let eReg = compile_imm e env in
        (match op with
        | Add1 ->
            compile_expr (EPrim1(IsNum, e, t)) si env @ [
            IMov(Reg(EAX), eReg);
            IAdd(Reg(EAX), Const(1)) ]
            (* jump if overflow here *)
        | Sub1 ->
            compile_expr (EPrim1(IsNum, e, t)) si env @ [
            IMov(Reg(EAX), eReg);
            IAdd(Reg(EAX), Const(1)) ]
            (* jump if overflow here *)
        | Print -> [
            (* Call print function here *)
        ]
        | IsBool -> [
            ITest(eReg, HexConst(0x1));
            (* Call error function here *)
            IJz("error"); ]
        | IsNum -> [
            ITest(eReg, HexConst(0x1));
            (* Call error function here *)
            IJnz("error"); ]
        | Not ->
            compile_expr (EPrim1(IsBool, e, t)) si env @ [
            (* Somehow invert true to false and vice versa here *)
            ]
        | PrintStack -> [
            (* Pop stack while esp!=ebp and print each? *)
        ]
        )
    | EPrim2 (op, e1, e2, t) ->
        let ptype = match op with
            | Plus | Minus | Times | Greater | GreaterEq | Less | LessEq | Eq -> IsNum
            | And | Or -> IsBool in
        let eReg1 = compile_imm e1 env in
        let eReg2 = compile_imm e2 env in
        let prelude =
            compile_expr (EPrim1(ptype, e1, t)) si env @
            compile_expr (EPrim1(ptype, e2, t)) si env
        in let main = (match op with
        | Plus -> [
            IMov(Reg(EAX), eReg1);
            IAdd(Reg(EAX), eReg2);
            (* jump if overflow here *)
        ]
        | Minus -> [
            IMov(Reg(EAX), eReg1);
            ISub(Reg(EAX), eReg2);
            (* jump if overflow here *)
        ]
        | Times -> [
            IMov(Reg(EAX), eReg1);
            IMul(Reg(EAX), eReg2);
            (* jump if overflow here *)
        ]
        | And -> [
            IMov(Reg(EAX), eReg1);
            IAnd(Reg(EAX), eReg2);
        ]
        | Or -> [
            IMov(Reg(EAX), eReg1);
            IOr(Reg(EAX), eReg2);
        ]
        | Greater -> []
        | GreaterEq -> []
        | Less -> []
        | LessEq -> []
        | Eq -> []
        )
        in prelude @ main
    | EIf _ -> failwith "Fill in here"
    | ENumber(n, _) -> [ IMov(Reg(EAX), compile_imm e env) ]
    | EBool(n, _) -> [ IMov(Reg(EAX), compile_imm e env) ]
    | EId(x, _) -> [ IMov(Reg(EAX), compile_imm e env) ]
    | _ -> failwith "Impossible: Not in ANF"
and compile_imm (e : tag expr) (env : (string * int) list) : arg =
    match e with
    | ENumber(n, _) ->
        if n > 1073741823 || n < -1073741824 then
           failwith ("Compile-time integer overflow: " ^ (string_of_int n))
        else
           Const(n lsl 1)
    | EBool(true, _) ->
        Const(0xFFFFFFFFF)
    | EBool(false, _) ->
        Const(0x7FFFFFFFF)
    | EId(x, _) ->
        RegOffset(~-(find env x), EBP)
    | _ -> failwith "Impossible: not an immediate"
;;

let compile_anf_to_string (anfed : tag expr) : string =
  let prelude =
    "section .text
extern error
extern print
global our_code_starts_here" in
  let stack_setup = [
      (* FILL: insert instructions for setting up stack here *)
    ] in
  let postlude = [
      IRet
      (* FILL: insert instructions for cleaning up stack, and maybe
       some labels for jumping to errors, here *) ] in
  let body = (compile_expr anfed 1 []) in
  let as_assembly_string = (to_asm (stack_setup @ body @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string


let compile_to_string (prog : 'a expr) =
  check_scope prog;
  let tagged : tag expr = tag prog in
  let anfed : tag expr = tag (anf tagged) in
  (* printf "Prog:\n%s\n" (ast_of_expr prog); *)
  (* printf "Tagged:\n%s\n" (format_expr tagged string_of_int); *)
  (* printf "ANFed/tagged:\n%s\n" (format_expr anfed string_of_int); *)
  compile_anf_to_string anfed

