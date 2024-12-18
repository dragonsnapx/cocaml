; ModuleID = 'example_module'
source_filename = "example_module"

%Message = type { i32, i64 }

define i32 @square(i32 %x) {
entry:
  %y = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  %x3 = load i32, ptr %x1, align 4
  %Times_instr = mul i32 %x2, %x3
  store i32 %Times_instr, ptr %y, align 4
  %y4 = load i32, ptr %y, align 4
  ret i32 %y4
}

define i32 @main(i32 %argc) {
entry:
  %z = alloca i32, align 4
  %pd = alloca i32, align 4
  %p = alloca ptr, align 8
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %message = alloca %Message, align 8
  %argc1 = alloca i32, align 4
  store i32 %argc, ptr %argc1, align 4
  store i32 3, ptr %y, align 4
  %x2 = load i32, ptr %x, align 4
  store ptr %x, ptr %p, align 8
  %p3 = load ptr, ptr %p, align 8
  %deref_val = load i32, ptr %p3, align 4
  store i32 %deref_val, ptr %pd, align 4
  store i32 7, ptr %z, align 4
  %message4 = load %Message, ptr %message, align 4
  %tmp_struct = alloca %Message, align 8
  store %Message %message4, ptr %tmp_struct, align 4
  %field_ptr = getelementptr inbounds %Message, ptr %tmp_struct, i32 0, i32 0
  %field_val = load i32, ptr %field_ptr, align 4
  store i32 %field_val, ptr %z, align 4
  %fn_call_square = call i32 @square(i32 3)
  %fn_call_square5 = call i32 @square(i32 3)
  ret i32 %fn_call_square5
}
