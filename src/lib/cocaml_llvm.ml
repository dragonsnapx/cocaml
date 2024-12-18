module L = Llvm
module S = Syntax_node

module type ModuleParameter = 
  sig
    val module_name: string
  end

module Make(Param : ModuleParameter) = struct
  open Stack_frame.StackFrame

  (* Custom translation errors*)

  exception TranslationError of string

  let raise_transl_err msg =
    raise (TranslationError ("Translation Error: " ^ msg))
  

  let context = L.global_context ()
  let builder = L.builder context
  let this_module = L.create_module context Param.module_name

  let var_env = create()

  let ignore_llvalue (_: L.llvalue) : unit = ()

  (* Shorthands for LLVM types *)
  let ll_char_t = L.i8_type context
  let ll_int_t = L.i32_type context
  let ll_long_t = L.i64_type context
  let ll_double_t = L.double_type context
  let ll_float_t = L.float_type context
  let ll_void_t = L.void_type context
  let ll_gen_ptr_t = L.pointer_type context

  (* Constants for LLVM types *)
  let const_ll_int_t = L.const_int ll_int_t
  let const_ll_float_t = L.const_float ll_float_t
  let const_ll_char_t = L.const_int ll_char_t
  let const_ll_long_t = L.const_int ll_long_t

  (* Helpers for LLVM shorthands *)
  let append_block name fn = L.append_block context name fn
  let position_at_end block = L.position_at_end block builder

  (* LLVM routine helpers *)
  let ptr_of (expr: S.Expr.t) = 
    match expr with
    | Var (id, _) -> (lookup_variable var_env id).value
    | MemberAccess _ | PointerMemberAccess _ | ArrayAccess _ -> failwith "TODO"
    | _ -> raise_transl_err "Cannot take address of a non-lvalue"
  
end