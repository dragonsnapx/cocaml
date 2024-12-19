	.text
	.file	"example_module"
	.globl	square                          # -- Begin function square
	.p2align	4, 0x90
	.type	square,@function
square:                                 # @square
	.cfi_startproc
# %bb.0:                                # %entry
	movl	%edi, %eax
	movl	%edi, -4(%rsp)
	imull	%edi, %eax
	retq
.Lfunc_end0:
	.size	square, .Lfunc_end0-square
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$0, (%rsp)
	movl	$0, 4(%rsp)
	cmpl	$9, 4(%rsp)
	jg	.LBB1_3
	.p2align	4, 0x90
.LBB1_2:                                # %for.loop
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsp), %edi
	callq	square@PLT
	movl	%eax, (%rsp)
	cmpl	$9, 4(%rsp)
	jle	.LBB1_2
.LBB1_3:                                # %for.end
	movl	(%rsp), %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
