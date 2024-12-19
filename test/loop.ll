; ModuleID = 'example_module'
source_filename = "example_module"

define i32 @square(i32 %x) {
entry:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  %x3 = load i32, ptr %x1, align 4
  %Times_instr = mul i32 %x2, %x3
  ret i32 %Times_instr
}

define i32 @main() {
entry:
  %x = alloca i32, align 4
  store i32 0, ptr %x, align 4
  %i = alloca i32, align 4
  store i32 0, ptr %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.incr, %entry
  %i1 = load i32, ptr %i, align 4
  %Less_icmp = icmp slt i32 %i1, 10
  br i1 %Less_icmp, label %for.loop, label %for.end

for.loop:                                         ; preds = %for.cond
  %x2 = load i32, ptr %x, align 4
  %fn_call_square = call i32 @square(i32 %x2)
  store i32 %fn_call_square, ptr %x, align 4
  br label %for.incr

for.incr:                                         ; preds = %for.loop
  %i3 = load i32, ptr %i, align 4
  store i32 %i3, ptr %i, align 4
  %Plus_instr = add i32 %i3, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %x4 = load i32, ptr %x, align 4
  ret i32 %x4
}
