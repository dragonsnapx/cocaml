module L = Llvm
module S = Syntax_node
module K = Stack_frame

module type ModuleParameter =
  sig
    val module_name: string
  end

module Make (_ : ModuleParameter) : sig
  exception TranslationError of string

  val raise_transl_err : string -> 'a

  val context : Llvm.llcontext
  val builder : Llvm.llbuilder
  val this_module : Llvm.llmodule
  val var_env : Stack_frame.t

  val ignore_llvalue : Llvm.llvalue -> unit

  val ll_char_t : Llvm.lltype
  val ll_int_t : Llvm.lltype
  val ll_long_t : Llvm.lltype
  val ll_double_t : Llvm.lltype
  val ll_float_t : Llvm.lltype
  val ll_void_t : Llvm.lltype
  val ll_gen_ptr_t : Llvm.lltype

  val const_ll_int_t : int -> Llvm.llvalue
  val const_ll_float_t : float -> Llvm.llvalue
  val const_ll_char_t : int -> Llvm.llvalue
  val const_ll_long_t : int -> Llvm.llvalue

  val const_llvalue_zero : Llvm.llvalue
  val const_llvalue_one : Llvm.llvalue
  val const_llvalue_negone : Llvm.llvalue

  val append_block : string -> Llvm.llvalue -> Llvm.llbasicblock
  val position_at_end : Llvm.llbasicblock -> unit
end