	.text
	.file	"example_module"
	.globl	square                          # -- Begin function square
	.p2align	4, 0x90
	.type	square,@function
square:                                 # @square
	.cfi_startproc
# %bb.0:                                # %entry
	movl	%edi, %eax
	movl	%edi, -8(%rsp)
	imull	%edi, %eax
	movl	%eax, -4(%rsp)
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
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movl	%edi, 16(%rsp)
	movl	$3, 20(%rsp)
	leaq	12(%rsp), %rax
	movq	%rax, 64(%rsp)
	movl	12(%rsp), %eax
	movl	%eax, 24(%rsp)
	movq	56(%rsp), %rax
	movl	48(%rsp), %ecx
	movl	%ecx, 32(%rsp)
	movq	%rax, 40(%rsp)
	movq	%rax, 28(%rsp)
	movl	$3, %edi
	callq	square@PLT
	movl	$3, %edi
	callq	square@PLT
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
