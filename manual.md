% R16K1S60
% LBPHacker
% 10-06-2018

# Manual and Instruction Reference for R16K1S60

## Index

* [Foreword](#foreword)
* [Links](#links)
* [Driving the thing](#driving)
* [Architecture](#arch)
* [Programming](#programming)
* [Instruction reference](#instructions)
* [Shift sorcery](#shifts)
* [Example programs](#examples)
* [Handling subframe tech safely](#subframe)
* [Changelog](#changelog)

Feel free to PM me if something is unclear or you have an improvement in mind for this manual.

## <a name="foreword"></a> Foreword

Sometime in 2015 [I published **Ray162K**](http://powdertoy.co.uk/Discussions/Thread/View.html?Thread=19916) (or **R162K** in short), my first ever **TPT** computer. It was huge (360x330 particle area), it was slow (16 frame clock cycle), but it was a bloody 16-bit computer nonetheless. It was fun to build and, in my opinion, to use.

The day I published **R162K**, [Schmolendevice](http://powdertoy.co.uk/User.html?Name=Schmolendevice) contacted me and introduced me to the world of subframe technology (thanks for that!). I was amazed by how smoothly everything worked in subframe, but since I'd just finished building **R162K**, I couldn't muster the strength to look deeper into it right away. I was out of it for quite some time.

When I came back to **TPT**, I half-expected to be greeted by the sight of countless subframe computers on FP. As you can probably guess, that's not what happened. I thought, well, I could just build one myself. I set my goals and built **R16K1S60**, which features:

* 16-bit architecture (**"R16"**, R stands for Ray)
* a RAM with 1024 16-bit cells (**"K1"**, 1 kibi-cell)
* Single frame clock cycles (**"S60"**, 60Hz with the default framerate cap)
* 5 general purpose registers, instruction pointer and stack pointer
* 4 16-bit IO ports
* Shifts, rotates, arithmetical and logical instructions
* Conditional jumps
* Stack pointer based stack instructions, subroutine calls and returns
* 3 addressing modes
* 16-bit instruction set; code can write code (**R162K** had a 28-bit instruction set and couldn't write code)
* Exactly zero WIFI particles
* Subframe technology ([careful with that](#subframe))

Also, it's so small that you can fit four of them in a standard 612x384 **TPT** simulation.

## <a name="links"></a> Links

* [Link to the **R16K1S60** thread](http://powdertoy.co.uk/Discussions/Thread/View.html?Thread=20691)
* [Link to the **R16K1S60** showcase save](http://powdertoy.co.uk/Browse/View.html?ID=1932845)
* [Link using the ptsave protocol](ptsave:1932845) (opens **TPT** if configured properly)

## <a name="driving"></a> Driving the thing

On its bottom side it has four buttons, a cartridge slot and an LRCY lamp. These are, from left to right:

* the Reset button (resets Ray, copies the full ROM into the RAM, sets `IP` to 0x0000, doesn't reset any other register)
* the Start button (starts execution of the code in the RAM from address 0x0000)
* the Resume button (starts execution of the code in the RAM from the address where execution last stopped)
* the Stop button (stops execution, doesn't reset anything, equivalent of a `HLT` instruction)
* a cartridge slot (contains the 1024-cell ROM and a partial reset circuit)
* the Execution Indicator (some LRCY that turns on when code is being executed)

On its right side, it has 4 IO ports: #0, #1, #2 and #3 from top to bottom.

To run a program on a cartridge, insert the cartridge into the cartridge slot and press the Start button. Install the peripherals required by the program beforehand.

## <a name="arch"></a> Architecture

A 16-bit ALU, 4 16-bit read-write IO ports, 1024 16-bit cells of RAM, shared code/data/stack space. Five general purpose registers (called `AX`, `BX`, `CX`, `DX` and `EX`), stack pointer (`SP`, always points to the bottom of the stack), instruction pointer (`IP`, which always points to the first cell of the next instruction about to be executed).

The stack grows downward, and zeroing `SP` might be the first thing your program should do if it uses the stack (as none of the registers are reset apart from `IP` when Ray is reset).

Flags that reflect properties of the result of the last instruction executed are present. These flags are the Zero Flag (`Zf`, set if the result is `0`), the Sign Flag (`Sf`, set if the signed result is negative), the Carry Flag (`Cf`, set if an unsigned operation yielded a carry) and the Overflow Flag (`Of`, set if a signed operation yielded a carry).

Unconditional and conditional jumps (based on the above flags), subroutine calls and returns, stack operations and base-plus-offset addressing are supported.

A subroutine call pushes the return address (the address of the instruction after the `CALL` instruction) to the stack and jumps. A subroutine return pops said address and jumps to it.


## <a name="programming"></a> Programming

*This section assumes a basic knowledge of assembly programming.*

There's an instruction reference below this section. The mnemonics used there are recognized by **rasm** and even highlighted by **RasmUI**.

Once you've downloaded [rasm.lua](./rasm.lua) (**Ray assembler**) and [rasmui.lua](./rasmui.lua) (user interface for **rasm**), there are several ways of going about this:

* The sensible way: Bring up an instance of **RasmUI** by issuing `dofile("/path/to/rasmui.lua")` in the console. Load an assembly file, open the same file in a text editor, enable "Reload source on Assemble", select a Ray with the "Select next/previous machine" buttons. Hit "Assemble" when you feel like flashing the code into Ray's ROM.
* The fun way: The same thing as above without an external text editor, using the crappy editor in **RasmUI**.
* The hardcore way: Use **rasm** from the console. Issue `loadfile("/path/to/rasm.lua")("/path/to/source.asm", ramPosX, ramPosY)` in the console. `ramPosX` and `ramPosY` are the coordinates of the one and only QRTZ particle present in Ray with a `.ctype` of `0x1864A205`.
* The insane way: Grabbing the PROP tool and setting the `.ctype` of the FILT particles in the ROM by hand to `0x20000000 | opcode`.

**RasmUI** is also good for debugging. You can choose which instance of Ray to monitor (yes, as I've mentioned before, you *can* have multiple Rays on screen). It's also got tooltips to make it more difficult to get lost in the mess of its controls. You can even open several **RasmUI** instances simultaneously.

A few words about **rasm**:

* Only accepts Intel-style assembly syntax (`opcode destination, source`).
* Doesn't encode immediate values with a value of `0`, since immediate values default to `0` anyway (holy crap, optimization).
* Supports forward references of not-yet-defined labels.
* Supports hexadecimal numbers.

A few words about **RasmUI**:

* Has an "Advance the simulation by a single frame" button. It's like pressing `F`.
* Can draw an overlay over the currently selected Ray.
* Can't find on-screen Rays by itself. You have to use "Select next/previous machine" to make it search for one.
* In a later version you'll be able to follow the execution of the code in the editor. _[02-02-2016]_
* While **rasm** is case insensitive when it comes to mnemonic names, **RasmUI** doesn't highlight mnemonic names with capital letters. I might have to fix that. _[02-05-2016]_
* You can choose between keyboard layouts by setting `tpt.RASMUI_KBDLAYOUT`. If it's `nil`, **RasmUI** defaults to **en** layout. Currently upported layouts: _[02-09-2016]_
	* **en**
	* **hu**

In an assembly source, the following symbols have special meaning:

* Registers: `ax`, `bx`, `cx`, `dx`, `ex`, `sp`, `ip`
* Mnemonics: `xchg`, `nop`, `hlt`, `call`, `ret`, `push`, `pop`, `jnb`, `jae`, `jnc`, `jb`, `jnae`, `jc`, `jno`, `jo`, `jns`, `js`, `jne`, `jnz`, `je`, `jz`, `jg`, `jnle`, `jle`, `jng`, `jge`, `jnl`, `jl`, `jnge`, `ja`, `jnbe`, `jbe`, `jna`, `jmp`, `send`, `recv`, `shl`, `shr`, `shld`, `shrd`, `rol`, `ror`, `add`, `adc`, `sub`, `sbb`, `bump`, `wait`, `cmp`, `mov`, `and`, `or`, `xor`, `test`
* Mnemonic prefixes: `@flags`, `@noflags` (these override the default flag setting for updating the flags)
* Data fields: `dw`, `db` (these store word or byte sized initialized data)
* Address override: `@org` (this changes the current output address)


## <a name="instructions"></a> Instruction reference

Instructions are encoded as 16-bit opcodes. They have zero to three input operands and zero to two output operands. These operands may be registers or 16-bit immediate values. Any instruction is capable of encoding a single immediate value and not more than that, which means that instructions such as `mov [0xCAFE], 0xBABE` are not possible to encode. An immediate value defaults to 0 if an instruction references it and no immediate value is encoded in the opcode. In the reference, the flag that determines whether an immediate value is encoded or not is referred to as `g` (set if an immediate value *is* encoded).

Flags are set according to the result written to the Primary operand line, if any. In the reference, the flag that determines whether the flags are to be updated or not is referred to as `f` (set if they *are* to be updated).

Instructions read their inputs from the Primary, Secondary and/or Tertiary operand lines and write results to the Primary and/or the Secondary operand lines. 2- or 3-bit fields in the instruction tell the operand lines which register (or immediate value) to map to. A 3-bit `000` field maps to the immediate value encoded in the instruction, `001` maps to `AX`, `010` maps to `BX`, etc. `110` maps to `SP` and `111` maps to `IP`. These fields are expressed as 3 letters in the opcodes below, `ppp` for the primary operand, `sss` for the secondary operand and `tt` for the tertiary operand. You might notice that `tt` fields are only 2-bit long. This means that only an immediate value, `AX`, `BX` or `CX` can be encoded as tertiary operands.

Keep in mind that e.g. *secondary operand* doesn't necessarily mean *second operand*, as the former is an architectural detail, while the latter only exists in the assembly source and is translated by **rasm**.

Some opcodes include hacks such as discarding the result of an instruction by saving it to operand `000`, effectively the immediate value. Since the immediate value is a read-only value, the result is discarded.

Dots mean indifferent bits; they have no effect on the instruction encoded in the opcode they appear in.

* ### `XCHG ppp, sss` - Exchange values

	* Description: Exchanges the contents of its two operands.
	* Opcode: `0000 ..pp psss f..g`
	* Execution time: 1 cycle

* ### `NOP` - No operation

	* Description: Special case of `XCHG`. `ppp` and `sss` are the same and thus nothing actually happens, while a cycle is wasted.
	* Useful for: Timing.
	* Execution time: 1 cycle

* ### `HLT` - Halt

	* Description: Stops execution. Execution may be resumed by pressing the Resume button. Execution is resumed from the instruction after `HLT`.
	* Useful for: Debug breaks.
	* Opcode: `0001............`
	* Execution time: 1 cycle

* ### `CALL sss` - Call subroutine

	* Description: Pushes `IP` onto the stack and jumps to the address pointed to by `sss`.
	* Opcode: `0010 00.. .sss f..g`
	* Execution time: 2 cycles

* ### `PUSH sss` - Push value

	* Description: Pushes the value of `sss` onto the stack.
	* Opcode: `0010 01.. .sss f..g`
	* Execution time: 2 cycles

* ### `RET` - Return from subroutine

	* Description: Pops an instruction pointer from the stack and jumps to it.
	* Opcode: `0010 10.. .000....`
	* Execution time: 2 cycles

* ### `POP sss` - Pop value

	* Description: Pops a value from the stack into `sss`.
	* Opcode: `0010 11.. .sss....`
	* Execution time: 2 cycles

* ### `MOV [ppp], sss` - Write to memory

	* Description: Writes the value of `sss` to the address pointed to by `ppp`.
	* Opcode: `0011 00pp psss 0..g`
	* Execution time: 2 cycles

* ### `MOV [ppp+tt], sss` - Write to memory with positive offset

	* Description: Writes the value of `sss` to the address pointed to by `ppp+tt`.
	* Opcode: `0011 00pp psss 1ttg`
	* Execution time: 2 cycles

* ### `MOV [ppp-tt], sss` - Write to memory with negative offset

	* Description: Writes the value of `sss` to the address pointed to by `ppp-tt`.
	* Opcode: `0011 01pp psss 1ttg`
	* Execution time: 2 cycles

* ### `MOV sss, [ppp]` - Read from memory

	* Description: Reads a value from the address pointed to by `ppp` into `sss`.
	* Opcode: `0011 10pp psss 0..g`
	* Execution time: 2 cycles

* ### `MOV sss, [ppp+tt]` - Read from memory with positive offset

	* Description: Reads a value from the address pointed to by `ppp+tt` into `sss`.
	* Opcode: `0011 10pp psss 1ttg`
	* Execution time: 2 cycles

* ### `MOV sss, [ppp-tt]` - Read from memory with negative offset

	* Description: Reads a value from the address pointed to by `ppp-tt` into `sss`.
	* Opcode: `0011 11pp psss 1ttg`
	* Execution time: 2 cycles

* ### `Jcc sss` - Jump (similar to [x86 jumps](http://unixwiz.net/techtips/x86-jumps.html))

	* Description: Jumps to `sss` if condition `cc` is met.
	* `Jcc` refers to one of the following:
		* `0100 1110 .sss f..g` - `JMP`: jump unconditionally
		* `0100 0010 .sss f..g` - `JO`: jump if overflow (signed)
		* `0100 0011 .sss f..g` - `JNO`: jump if not overflow (signed)
		* `0100 0100 .sss f..g` - `JS`: jump if sign
		* `0100 0101 .sss f..g` - `JNS`: jump if not sign
		* `0100 0110 .sss f..g` - `JE`: jump if equal
		* `0100 0110 .sss f..g` - `JZ`: jump if zero
		* `0100 0111 .sss f..g` - `JNE`: jump if not equal
		* `0100 0111 .sss f..g` - `JNZ`: jump if not zero
		* `0100 0000 .sss f..g` - `JB`: jump if below (unsigned)
		* `0100 0000 .sss f..g` - `JNAE`: jump if not above or equal (unsigned)
		* `0100 0000 .sss f..g` - `JC`: jump if carry (unsigned)
		* `0100 0001 .sss f..g` - `JNB`: jump if not below (unsigned)
		* `0100 0001 .sss f..g` - `JAE`: jump if above or equal (unsigned)
		* `0100 0001 .sss f..g` - `JNC`: jump if not carry (unsigned)
		* `0100 1100 .sss f..g` - `JBE`: jump if below or equal (unsigned)
		* `0100 1100 .sss f..g` - `JNA`: jump if not above (unsigned)
		* `0100 1101 .sss f..g` - `JA`: jump if above (unsigned)
		* `0100 1101 .sss f..g` - `JNBE`: jump if not below or equal (unsigned)
		* `0100 1010 .sss f..g` - `JL`: jump if less (signed)
		* `0100 1010 .sss f..g` - `JNGE`: jump if not greater or equal (signed)
		* `0100 1011 .sss f..g` - `JGE`: jump if greater or equal (signed)
		* `0100 1011 .sss f..g` - `JNL`: jump if not less (signed)
		* `0100 1000 .sss f..g` - `JLE`: jump if less or equal (signed)
		* `0100 1000 .sss f..g` - `JNG`: jump if not greater (signed)
		* `0100 1001 .sss f..g` - `JG`: jump if greater (signed)
		* `0100 1001 .sss f..g` - `JNLE`: jump if not less or equal (signed)
	* Execution time: 1 cycle

* ### `SEND sss, ppp` - Send value on port

	* Description: Sends the value of `ppp` on port # value of `sss`.
	* Opcode: `0101 00pp psss f..g`
	* Execution time: 1 cycle

* ### `RECV ppp, sss` - Receive value from port

	* Description: Waits for a value on port # value of `sss` and stores it into `ppp`.
	* Opcode: `0101 01pp psss f..g`
	* Execution time: 1 cycle in the best case

* ### `BUMP sss` - Send Bump on port

	* Description: Asserts the Bump line on port # value of `sss`. The Bump line remains asserted until a `SEND` instruction overwrites the state of the port.
	* Opcode: `0101 10.. .sss f..g`
	* Execution time: 1 cycle

* ### `WAIT sss` - Wait for Bump on any port

	* Description: Waits until the Bump line is asserted on any of the ports, then returns the number of the port on which the Bump line was asserted in `sss`. The first port to be checked for an asserted Bump line is #0, then #1, #2 and #3 are checked.
	* Opcode: `0101 11.. .sss f..g`
	* Execution time: 1 cycle in the best case
	
* ### `ADD ppp, sss` - Integer addition

	* Description: Adds the value of `sss` to the value of `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1000 00pp psss f..g`
	* Execution time: 1 cycle
	
* ### `SUB ppp, sss` - Integer subtraction

	* Description: Subtracts the value of `sss` from the value of `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1000 01pp psss f..g`
	* Execution time: 1 cycle
	
* ### `CMP ppp, sss` - Value comparison

	* Description: Same as `SUB`, but the result is discarded.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1001 01pp psss f..g`
	* Execution time: 1 cycle
	
* ### `ADC ppp, sss` - Integer addition with carry

	* Description: Adds the value of `sss` and `Cf` (`0` or `1`) to the value of `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1000 10pp psss f..g`
	* Execution time: 1 cycle
	
* ### `SBB ppp, sss` - Integer subtraction with borrow

	* Description: Subtracts the value of `sss` and `Cf` (`0` or `1`) from the value of `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1000 11pp psss f..g`
	* Execution time: 1 cycle

* ### `SHL ppp, tt` and `SHL ppp, sss, tt` - Shift left

	* Description: Shifts the value of `ppp` to the left by value of `tt` bits, stores the result in `ppp`. All bits shifted in are `0`. The 3-operand variant saves the bits shifted out into `sss`.
	* Updates the flags by default when compiled with **rasm**
	* Opcodes
		* 2-operand variant: `1010 00pp p000 fttg` (`sss` is hardcoded to `000`)
		* 3-operand variant: `1010 00pp psss fttg`
	* Execution time: 1 cycle

* ### `SHR ppp, tt` and `SHR ppp, sss, tt` - Shift right

	* Description: Shifts the value of `ppp` to the right by value of `tt` bits, stores the result in `ppp`. All bits shifted in are `0`. The 3-operand variant saves the bits shifted out into `sss`.
	* Updates the flags by default when compiled with **rasm**
	* Opcodes
		* 2-operand variant: `1010 01pp p000 fttg` (`sss` is hardcoded to `000`)
		* 3-operand variant: `1010 01pp psss fttg`
	* Execution time: 1 cycle

* ### `SHLD ppp, sss, tt` - Shift left through two operands

	* Description: Shifts the value of `ppp` to the left by value of `tt` bits, stores the result in `ppp`. The bits shifted in are the most significant bits of `sss`. The bits shifted out are saved back into `sss`.
	* Updates the flags by default when compiled with **rasm**
	* Useful for: Multiword left shifts
	* Opcode: `1010 10pp psss fttg`
	* Execution time: 1 cycle

* ### `ROL ppp, sss` - Rotate left
	
	* Description: Special case of `SHLD`. `ppp` and `sss` are the same and thus the value of `ppp` is rotated left by value of `tt` bits.
	* Updates the flags by default when compiled with **rasm**
	* Execution time: 1 cycle
	
* ### `SHRD ppp, sss, tt` - Shift right through two operands

	* Description: Shifts the value of `ppp` to the right by value of `tt` bits, stores the result in `ppp`. The bits shifted in are the least significant bits of `sss`. The bits shifted out are saved back into `sss`.
	* Updates the flags by default when compiled with **rasm**
	* Useful for: Multiword right shifts
	* Opcode: `1010 11pp psss fttg`
	* Execution time: 1 cycle

* ### `ROR ppp, sss` - Rotate right
	
	* Description: Special case of `SHRD`. `ppp` and `sss` are the same and thus the value of `ppp` is rotated right by value of `tt` bits.
	* Updates the flags by default when compiled with **rasm**
	* Execution time: 1 cycle
	
* ### `MOV ppp, sss` - Value copy

	* Description: Copies the value of `sss` into `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1100 00pp psss f..g`
	* Execution time: 1 cycle
	
* ### `AND ppp, sss` - Bitwise AND

	* Description: Performs a bitwise AND on the values of `sss` and `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1100 01pp psss f..g`
	* Execution time: 1 cycle
	
* ### `TEST ppp, sss` - Bit test

	* Description: Same as `AND`, but the result is discarded.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1101 01pp psss f..g`
	* Execution time: 1 cycle
	
* ### `OR ppp, sss` - Bitwise OR

	* Description: Performs a bitwise OR on the values of `sss` and `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1100 10pp psss f..g`
	* Execution time: 1 cycle
	
* ### `XOR ppp, sss` - Bitwise XOR

	* Description: Performs a bitwise XOR on the values of `sss` and `ppp`, stores the result in `ppp`.
	* Updates the flags by default when compiled with **rasm**
	* Opcode: `1100 11pp psss f..g`
	* Execution time: 1 cycle

## <a name="shifts"></a> Shift sorcery

Ray implements shifts in a funny way: No `SAR`, no MSB preservation, no nothing. It makes up for that with multiword shifts and rotates. This section describes the inner workings of the Shifter Unit and how it can be used to do all sorts of fun stuff.

Consider `SHL` (the 2-operand variant). It shifts `ppp` left by `tt` bits. Let's say `tt` is `1`. That's easy: `a` is discarded, `0` appears on the right (the LSB).

	              PPP
	 
	before: abcdefghijklmnop
	after:  bcdefghijklmnop0

The 3-operand variant does some more work and makes a backup of `ppp` in `sss`. Let's use that and shift `ppp` to the left by 3 bits this time:

	              PPP              SSS

	before: abcdefghijklmnop................
	after:  defghijklmnop000 abcdefghijklmnop

`a`, `b` and `c` are discarded, `sss` now contains the initial value of `ppp`.

Do another shift using `SHLD`. It gets the bits to shift in from `sss`. `A`, `B` and `C` are discarded once again, but instead of `0`s, `a`, `b` and `c`, the MSBs of the initial value of `ppp` from the previous `SHL`, appear on the right:

	              PPP              SSS
	 
	before: ABCDEFGHIJKLMNOP abcdefghijklmnop
	after:  DEFGHIJKLMNOPabc ABCDEFGHIJKLMNOP

The initial value of `ppp` is once again backed up in `sss`.

So, with an `SHL` followed by as many `SHLD`s as you want, you can shift as big a number as you want by 16 bits at most. The same applies to right shifts (`SHR` and `SHRD`).

To see how `ROL` works, we have to break the above before/after diagram of `SHLD` into steps:

	              PPP              SSS        temporal storage
	 
	step 0: ABCDEFGHIJKLMNOP abcdefghijklmnop ................ // initial state
	step 1: ABCDEFGHIJKLMNOP abcdefghijklmnop abcdefghijklmnop // sss is copied into temp
	step 2: ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP abcdefghijklmnop // ppp is copied into sss
	step 3: DEFGHIJKLMNOPabc ABCDEFGHIJKLMNOP abcdefghijklmnop // ppp is shifted, bits get shifted in from temp

What happens if `ppp` and `sss` map to the same source? We get a rotate:

	              PPP              SSS        temporal storage
	 
	step 0: ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP................
	step 1: ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP
	step 2: ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP
	step 3: DEFGHIJKLMNOPABC ABCDEFGHIJKLMNOP ABCDEFGHIJKLMNOP

Since `ppp` and `sss` map to the same destination as well, the question of whether the Primary result or the Secondary result is saved to that destination arises. The answer is simple: the Primary operand has precedence over the Secondary operand, so the Primary result is saved. So after the `ROL` above, `ppp` will hold `DEFGHIJKLMNOPABC`.

This means that `ROL x, y` is just a shorthand for `SHLD x, x, y`. The same applies to `ROR`.

A mutliword rotate is just a big shift (chained `SHLD`s) and a bit of playing around with the bits shifted out at the end:

<!--prettify lang=r1asm-->

	; Let's rotate the cx:bx:ax 48-bit integer left by 9 bits.
	; Yeah, it's not pretty. We're basically doing shld's work *before*
	; the other shifts take place.
	
	mov dx, ax			; * ((shl ax, dx, 9)) would do the same to dx
	shld bx, dx, 9		; * go on with the multiword shift
	shld cx, dx, 9		; * go on with the multiword shift
	shld ax, dx, 9		; * shift ax, get bits to shift in from the previous shld

And to think that I wasn't even planning to support rotates at all...

## <a name="examples"></a> Example programs

 * Fibonacci sequence generator

<!--prettify lang=r1asm-->

	; fibonacci.asm
	;
	; Calculates the first few elements
	; of the Fibonacci sequence using
	; 16-bit unsigned integers, outputs
	; them on port #3.
	
		mov dx, 3
		mov bx, 0       ; * start off with 0, 1 (we won't output those)
		mov ax, 1
	spin:
		mov cx, ax      ; * backup ax
		add ax, bx      ; * calculate the next element
		jc stop         ; * stop of addition had a carry
		mov bx, cx      ; * recall old ax
		send dx, ax     ; * output element
		jmp spin
	stop:
		hlt

 * The original showcase

<!--prettify lang=r1asm-->

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
		mov sp, bx				; * lol, we're not using the stack, so...
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

 * A Brainf\*\*k compiler

<!--prettify lang=r1asm-->

	; bfc.asm
	;
	; Brainf**k compiler. Yeah. I mean it.
	; Writes compiled code to 0x200 (plus the bootstrapper).
	; Clears a 0x200 cell array for the BF program.
	; Not optimized at all. 23 +'s will compile to 23 ((add ax, 1))'s.
	; . and , send and expect raw character codes.
	;
	; Unusable without a peripheral that can interpret those character codes.
	;
	; In theory, a sanity check could be added to check if the BF source overlaps
	; with the region reserved for the compiled code. Something like
	;     if (compile.end > util_bootstrap) nope();

	compile:
		mov sp, 0x0200              ; * so the stack doesn't interfere with the result
		mov cx, util_bootstrap.code_end
		mov bx, .source
	.loop:
		add cx, 1
		mov ax, [bx]
		cmp ax, 0
		jz .done
		add bx, 1
		cmp ax, 60
		jb .br__inc_in_dec_out      ; * char's below 60, it's one of '+' (43), ',' (44), '-' (45) and '.' (46)
									; * char's not below 60, it's one of '<' (60), '>' (62), '[' (91) and ']' (93)
		cmp ax, 91
		jb .br__left_right          ; * char's below 91, it's either '<' (60) or '>' (62)
									; * char's not below 91, it's either '[' (91) or ']' (93)
		cmp ax, 93
		jb .br__while               ; * char's below 93, it's '[' (91)
									; * char's not below 93, it's ']' (93)
	; HANDLING ']' HERE
		pop ax                      ; * pop address of the previous '['
									; * there's no error checking here, although it'd be as simple as "cmp sp, 0" and "jz error"
		mov [cx], 0x4E01            ; * we're jumping to the previous '[' unconditionally
		mov [cx+1], ax              ; * we also need the address for the jump stored as an immediate value
		add cx, 2                   ; * skip to the instruction after the ']',
		mov [ax+2], cx              ;   the conditional jump at the previous '[' needs its address to know where to jump
		sub cx, 1                   ; * but we're not there yet
		jmp .loop
	.br__while:
	; HANDLING '[' HERE
		push cx                     ; * push the address for the next ']'
		mov [cx], 0x9488            ; * encodes "cmp ax, 0"
		add cx, 1                   ; * skip one cell for the conditional jump
		mov [cx], 0x4601            ; * encodes "jz imm"
		add cx, 1                   ; * skip one cell for the immediate value
		jmp .loop
	.br__left_right:
		cmp ax, 62
		mov ax, -1                  ; * _something_ is -1 at a '<' (flags are not affected)
		jb .br__left                ; * char's below 62, it's '<' (60)
									; * char's not below 62, it's '>' (62)
		mov ax, 1                   ; * _something_ is +1 at a '>'
	.br__left:
	; HANDLING '<' AND '>' HERE
		mov [cx], 0x3110            ; * encodes "mov [bx], ax", which writes back the data to the pointer
		add cx, 1
		mov [cx], 0x8101            ; * encodes "add bx, _something_", bumping the pointer
		add cx, 1
		mov [cx], 0xC501            ; * encodes "and bx, 0x1FF", masking the pointer
		add cx, 1
		mov [cx], 0x01FF
		add cx, 1
		mov [cx], ax                ; * actually, _something_ is encoded here
		add cx, 1
		mov [cx], 0x3910            ; * encodes "mov ax, [bx]", which gets the data pointer over to data
		jmp .loop
	.br__inc_in_dec_out:
		cmp ax, 45
		jb .br__inc_in              ; * char's below 45, it's either '+' (43) or ',' (44)
									; * char's not below 45, it's either '-' (45) or '.' (46)
		cmp ax, 46
		jb .br__dec                 ; * char's below 46, it's '-' (45)
									; * char's not below 46, it's '.' (46)
	; HANDLING '.' HERE
		mov [cx], 0x50C0            ; * encodes "send dx, ax"
		jmp .loop
	; HANDLING '-' HERE
	.br__dec:
		mov ax, -1                  ; * _something_ is -1 at a '-'
	; HANDLING THE REST OF '+' AND '-' HERE
	.br__dec_inc_common:
		mov [cx], 0x8081            ; * encodes "add ax, _something_", bumping the data
		add cx, 1                   ; * skip to the immediate value
		mov [cx], ax                ; * actually, _something_ is encoded here
		jmp .loop
	.br__inc_in:
		cmp ax, 44
		jb .br__inc                 ; * char's below 44, it's '+' (43)
									; * char's not below 44, it's ',' (44)
	; HANDLING ',' HERE
		mov [cx], 0x54C0            ; * encodes "recv ax, dx"
		jmp .loop
	; HANDLING '+' HERE
	.br__inc:
		mov ax, 1                   ; * _something_ is +1 at a '+'
		jmp .br__dec_inc_common
	.done:
		mov [cx], 0x1000            ; * encodes "hlt" at the end of the code
		hlt
		jmp util_bootstrap
	.source: dw "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.", 0 
	.end:                           ; ^ helloworld taken from Wikipedia

	@org 0x0200
	util_bootstrap:
		mov cx, compile.end			; * part that kills the compiler begins
	.loop:
		sub cx, 1
		mov [cx], 0
		jnz .loop					; * part that kills the compiler ends
		mov sp, 0
		mov ax, 0
		mov bx, 0
		mov dx, 3					; * configure this - this is the output port
	.code_end:
		nop

## <a name="subframe"></a>Handling subframe tech safely

***-- Oh noes, it broke!***

That most likely happened because you tried adding something to the save when it was not paused or messed up the particle order some other way. When it comes to subframe tech, you can't just place or remove particles, as doing that might change the order of evaluation.

Subframe tech relies on that order of evaluation, or **particle order**, which determines which particle is evaluated after which. When you open a save, particles are assigned their IDs in a way that the ID of a particle is always bigger than that of a particle above it or on its left. This order must be preserved as subframe tech uses it to schedule its tasks inside a frame (hence the term **subframe**).

If this order is broken by any means (typically by placing or removing particles), it must be restored before a frame elapses, otherwise the results may be catastrophic. This restoration is done by saving and reopening the save, as this resets particle order in the manner described above. The simulation must be paused while particles are being placed or removed for this technique to be effective.

I also recommend turning Heat Simulation, Air and any sort of Gravity off.

## <a name="changelog"></a> Changelog

* 12-30-2015: Initial WIP release
* 01-30-2016: Huge overhaul, added **Programming** section
* 02-02-2016: Minor fixes, added **Shift sorcery** section
* 02-03-2016: Minor fixes, added **Example programs** section
* 02-05-2016: Added **Handling subframe tech safely** section
* 10-06-2018: Moved to new site, fixed stuff

