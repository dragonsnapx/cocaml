open Core
module L = Llvm
module S = Syntax_node

let ignore _ = ()

let process_binop (op: S.Expr.bin_op) (lhs: L.llvalue) (rhs: L.llvalue) (builder: L.llbuilder) =
  let name = op |> S.Expr.sexp_of_bin_op |> string_of_sexp in
  let arith instr = instr lhs rhs (name ^ "_instr") builder in
  let cmp vs = L.build_icmp vs lhs rhs (name ^ "_icmp") builder in
  let assign instr =
    let old_val = L.build_load (L.element_type (L.type_of lhs)) lhs ("old_" ^ name ^ "_val") builder in
    let new_val = instr old_val rhs ("new_" ^ name ^ "_val") builder in
    L.build_store new_val lhs builder |> ignore;
    new_val
  in
  match op with
  | Plus -> arith L.build_add
  | Minus -> arith L.build_sub
  | Times -> arith L.build_mul
  | Divide -> arith L.build_sdiv
  | Modulo -> arith L.build_srem
  | BitwiseAnd -> arith L.build_and
  | BitwiseOr -> arith L.build_or
  | BitwiseXor -> arith L.build_xor
  | LogicalAnd -> arith L.build_and
  | LogicalOr -> arith L.build_or
  | LeftShift -> arith L.build_shl
  | RightShift -> arith L.build_ashr
  | Equal -> cmp L.Icmp.Eq
  | NotEqual ->cmp L.Icmp.Ne
  | Less -> cmp L.Icmp.Slt
  | LessEqual -> cmp L.Icmp.Sle
  | Greater -> cmp L.Icmp.Sgt
  | GreaterEqual -> cmp L.Icmp.Sge
  | PlusAssign -> assign L.build_add
  | MinusAssign -> assign L.build_sub
  | TimesAssign -> assign L.build_mul
  | DivideAssign -> assign L.build_sdiv
  | ModuloAssign -> assign L.build_srem
  | BitwiseAndAssign -> assign L.build_and
  | BitwiseOrAssign -> assign L.build_or
  | BitwiseXorAssign -> assign L.build_xor