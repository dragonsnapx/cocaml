module L = Llvm
module S = Syntax_node

val process_binop : S.Expr.bin_op -> L.llvalue -> L.llvalue -> L.llbuilder -> L.llvalue

val simple_add : L.llvalue -> L.llvalue -> L.llbuilder -> L.llvalue