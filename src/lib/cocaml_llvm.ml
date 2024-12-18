module L = Llvm

module type ModuleParameter = 
  sig
    val module_name: string
  end

module Make(Param : ModuleParameter) = struct
  let context = L.global_context ()
  let builder = L.builder context
  let this_module = L.create_module context Param.module_name

  let ignore_llvalue (_: L.llvalue) : unit = ()

  let append_block name fn = L.append_block context name fn
  let position_at_end block = L.position_at_end block builder

  let ll_char_t = L.i8_type context
  let ll_int_t = L.i32_type context
  let ll_long_t = L.i64_type context
  let ll_double_t = L.double_type context
  let ll_float_t = L.float_type context
  let ll_void_t = L.void_type context

  let const_ll_int_t = L.const_int ll_int_t
  let const_ll_float_t = L.const_float ll_float_t
  let const_ll_char_t = L.const_int ll_char_t
  let const_ll_long_t = L.const_int ll_long_t
  
end