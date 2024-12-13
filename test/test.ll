; ModuleID = 'module_name_here'
source_filename = "module_name_here"

define i32 @main(i32 %argc) {
entry:
  %z = alloca i32, align 4
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %argc1 = alloca i32, align 4
  store i32 %argc, ptr %argc1, align 4
  store i32 3, ptr %y, align 4
  store i32 7, ptr %z, align 4
  ret i32 0
}
