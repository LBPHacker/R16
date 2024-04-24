	mov sp, 0x400     ; init the stack
	
	mov cx, 0x7090    ; coords of first row: (112; 144)
	send 0, cx
	mov ax, 2         ; hax; number of 16-cell packs (32 / 16)
	mov ex, [stateptr_flip]
loop_initial:         ; <-- CELL PACK LOOP
	mov cx, [ex]
	send 1, cx
	add ex, 1
	recv cx, 0
	sub ax, 1
	jnz loop_initial  ; CELL PACK LOOP -->
	
	mov cx,  32       ; number of rows to plot
	
loop_row:             ; <-- ROW LOOP
	push cx
	mov cx, 0
	
	mov dx, [stateptr_flip]
	mov [stateptr_src], dx
	xor dx, 0x0004    ; swap source/destination state
	mov [stateptr_dest], dx
	mov [stateptr_flip], dx
	
	mov dx, [stateptr_src]
	mov bx, [dx]
	shl bx, cx, 1
	mov ax, 0
	shld ax, cx, 1    ; init row by shifting one cell in
	
	mov ex, 0
	mov dx,  32       ; number of cells to plot
loop_cell:            ; <-- CELL LOOP
	
	sub dx, 1
	push dx
	and dx, 15
	jnz .no_fetch     ; check whether we're on a word boundary
	
	mov dx, [stateptr_src]
	add dx, 1
	mov [stateptr_src], dx
	mov bx, [dx]
	
.no_fetch:
	shl bx, cx, 1
	shld ax, cx, 1    ; shift one cell in
	
	mov cx, ax
	and cx, 7
	mov dx, [rule]    ; consult the rule
	shr dx, cx
	and dx, 1
	shl ex, 1
	or ex, dx         ; commit cell to buffer
	
	mov cx, [sp]      ; get old dx; dx is already decremented by 1
	and cx, 15
	jnz .no_flush

	mov cx, [stateptr_dest]
	mov [cx], ex
	add cx, 1
	mov [stateptr_dest], cx
	
	mov cx, 128
	mov dx, [sp]
	sub cx, dx
	shl cx, 8
	mov dx, [sp+1]
	or cx, dx
	add cx, 111
	send 0, cx        ; send the coords
	send 1, ex        ; send partial data to buffer

.no_flush:
	pop dx            ; dx is already decremented by 1
	cmp dx, 0
	jnz loop_cell     ; CELL LOOP -->
	
	pop cx
	sub cx, 1
	jnz loop_row      ; ROW LOOP -->
	hlt

stateptr_src: dw 0
stateptr_dest: dw 0
stateptr_flip: dw 0x0200

@org 0x0200
	dw 0x0000         ; initial state
	dw 0x8000
@org 0x0204
	dw 0x0000         ; twin state
	dw 0x0000

@org 0x0300
rule:
	dw 90

