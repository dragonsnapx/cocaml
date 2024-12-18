; ModuleID = 'cocaml_mod'
source_filename = "cocaml_mod"

define i32 @square(i32 %x) {
entry:
  %y = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  %x3 = load i32, ptr %x1, align 4
  %add_instr = add i32 %x2, %x3
  store i32 %add_instr, ptr %y, align 4
  %y4 = load i32, ptr %y, align 4
  ret i32 %y4
}

define i32 @main(i32 %argc) {
entry:
  %z = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %argc1 = alloca i32, align 4
  store i32 %argc, ptr %argc1, align 4
  store i32 3, ptr %y, align 4
  store i32 7, ptr %z, align 4
  %fn_call_square = call i32 @square(i32 3)
  ret i32 0
}
