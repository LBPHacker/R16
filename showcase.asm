; showcase.asm
;
; The program that runs in the showcase save.
;
; 32-bit fibonacci sequence. Elements output on ports #2 and #3.
; High word on #2, low word on #3.

	mov ax, 0
	mov bx, 0
	mov cx, 1
	mov dx, 0
spin:
	recv ex, 0				; * wait for a send on port #0
	mov ex, ax
	mov sp, bx				; * lol, we're not using the stack, so ...
	add ax, cx
	adc bx, dx
	jc die
	send 2, bx
	send 3, ax
	recv cx, 3				; * wait for the low word display to finish
							; * don't worry about cx
	mov cx, ex
	mov dx, sp
	jmp spin
die:
	hlt
	jmp die
