open Core
module L = Llvm
module S = Syntax_node

let process_binop (op: S.bin_op) =
  match op with
  | Plus -> L.build_add lhs rhs "add_instr" C.builder
  | Minus -> L.build_sub lhs rhs "sub_instr" C.builder
  | Times -> L.build_mul lhs rhs "mul_instr" C.builder
  | Divide -> L.build_sdiv lhs rhs "div_instr" C.builder
  | Modulo -> L.build_srem lhs rhs "mod_instr" C.builder
  | Equal -> L.build_icmp L.Icmp.Eq lhs rhs "eq_icmp" C.builder
  | NotEqual -> L.build_icmp L.Icmp.Ne lhs rhs "neq_icmp" C.builder
  | Less -> L.build_icmp L.Icmp.Slt lhs rhs "lt_icmp" C.builder
  | LessEqual -> L.build_icmp L.Icmp.Sle lhs rhs "leq_icmp" C.builder
  | Greater -> L.build_icmp L.Icmp.Sgt lhs rhs "gt_icmp" C.builder
  | GreaterEqual -> L.build_icmp L.Icmp.Sge lhs rhs "gte_icmp" C.builder
  | BitwiseAnd -> L.build_and lhs rhs "and_bit" C.builder
  | BitwiseOr -> L.build_or lhs rhs "or_bit" C.builder
  | BitwiseXor -> L.build_xor lhs rhs "xor_bit" C.builder
  | LogicalAnd -> L.build_and lhs rhs "and_instr" C.builder
  | LogicalOr -> L.build_or lhs rhs "or_instr" C.builder
  | LeftShift -> L.build_shl lhs rhs "shl_instr" C.builder
  | RightShift -> L.build_ashr lhs rhs "shr_instr" C.builder
  | PlusAssign -> failwith "TODO"
  | MinusAssign -> failwith "TODO"
  | TimesAssign -> failwith "TODO"
  | DivideAssign -> failwith "TODO"
  | ModuloAssign -> failwith "TODO"
  | BitwiseAndAssign -> failwith "TODO"
  | BitwiseOrAssign -> failwith "TODO"
  | BitwiseXorAssign -> failwith "TODO"