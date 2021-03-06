#"There are two big problems in assembly programming: type-errors with loop-counters, off-by-one-errors and off-by-one-errors with loop-counters."

#haraldur@arch-htg ~ % as -v	#I am uncertain as to how important assembly-language compatibility is for the GNU project; in particular, I worry about macro syntax and other directives.
#GNU assembler version 2.25 (x86_64-unknown-linux-gnu) using BFD version (GNU Binutils) 2.25

#To assemble and run on a file called "test.asm":
#as hack_assembler.s -o hack_assembler.o && ld -m elf_x86_64 hack_assembler.o -o hack_assembler.elf && ./hack_assembler.elf test.asm && echo $?

#To compare input and output with input file "test.asm":
#pr -mt test2.{asm,hack}|sed 's/ //g'|expand|sed 's/ /\./g'
#Note that labels shall break the alignment, so it is better to test them near the bottom of the "test.asm" file.

#This program successfully assembles the source files provided here: http://www.nand2tetris.org/06.php. It implements an assembler for the Hack platform. Output from this program can be compared with output from the programs in the link. Note that one should not pass arguments like ./foo, ../foo or ../../foo.

#NOTE: This is my first and (as of typing) only assembly-language program, for any architecture. At first I knew very little, so there are some messy parts from when I first started. There are few validation checks, as the nand2tetris assignments suggest to assume correct input. Additionally, the program cannot correctly handle more than 254 characters of comment (including // and excluding terminating newlines) or command (excluding any whitespace) nor more than 0x8000 (non-label, non-blank, non-comment) lines.
#Also, the (rather silly, given the probably-non-optimal algorithms in use and lack of a need for escpecially low runtimes) notes about the speeds of certain instructions may not apply to non-Haswell processors. I used http://www.agner.org/optimize/instruction_tables.pdf for reference.
#Thanks to http://programminggroundup.blogspot.co.uk/ -- an excellent introduction, despite being 32-bit only.

#To search for registers in vim, try this (or variants for only some or one register(s)): /%[re]\?[abcd][xlh]\|%[re]\?\([ds]i\|[bs]p\)l\?\|%r\(1[0-5]\|[89]\)[bsl]\?

#Syscall numbers found in /usr/include/asm/unistd_64.h
.equiv SYS_READ, 0
	.equiv STDIN, 0
.equiv SYS_WRITE, 1
	.equiv STDOUT, 1
	.equiv STDERR, 2
.equiv SYS_OPEN, 2
	.equiv PERM_RDWR_UGO, 0666#No execute permissions: read is 4, write is 2, execute is 1
	#/usr/include/asm/fcntl.h or /usr/include/asm-generic/fcntl.h:
	.equiv O_RDONLY, 0
	.equiv O_CREAT_WRONLY_TRUNC, 01101
.equiv SYS_CLOSE, 3
.equiv SYS_LSEEK, 8
	.equiv SEEK_SET, 0#/usr/include/linux/fs.h
.equiv SYS_MPROTECT, 10
	#/usr/include/bits/mman-linux.h:
	.equiv PROT_READ, 0x1
	.equiv PROT_WRITE, 0x2
	.equiv PROT_EXEC, 0x4
.equiv SYS_EXIT, 60
	.equiv EXIT_SUCCESS, 0#/usr/include/stdlib.h:

.macro MOV_MULT source, destination, args:vararg#This optionally 'movq's values. These values can be skipped, like 'MOV_MULT , %rax, $0, %rbx, , %rcx, $1, %rdx' as long as the destinations for non-terminal blank sources/values are not themselves blank.
	.ifnb \destination
		.ifnb \source
			movq \source, \destination
		.endif
		MOV_MULT \args
	.endif
.endm
.macro __SYSCALL rax_arg, rdi_arg, rsi_arg, rdx_arg, r10_arg, r8_arg, r9_arg#Syscalls look boring and take up space. This macro shortens syscalls in source code by setting the (optional) registers, if the arguments are defined, and then syscalling.
	MOV_MULT \rax_arg, %rax, \rdi_arg, %rdi, \rsi_arg, %rsi, \rdx_arg, %rdx, \r10_arg, %r10, \r8_arg, %r8, \r9_arg, %r9
	syscall#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
.endm

.equiv ST_FD_OUT, -16#Output file descriptor location.
.equiv ST_FD_IN, -8#Input file descriptor location.
.equiv ST_ARGC, 0#64-bit integer, unused, contains number of arguments (assumed to be 2)
.equiv ST_ARGV_0, 8#64-bit pointer, unused, points to program name
.equiv ST_ARGV_1, 16#64-bit pointer, points to filename of file to assemble

.equiv PROGRAM_CALL_DEPTH, 3#Need not include syscalls, but would probably not cause harm (the stack is (should be) large).
.equiv SYMBOL_TABLE_STACK_OFFSET, 8 * PROGRAM_CALL_DEPTH#In this program, all calls pass parameters in registers, so this is merely unwise, rather than broken (in principle -- FRAGILE).

.section .data
output:
	.ascii "0000000000000000\n"#This is modified to represent the machine-code instruction for the Hack platform that is then output to the .hack file.
.equiv OUTPUT_LENGTH, . - output
preset_symbols:#Populate the symbol table initially with these preset symbols:

	#Each symbol has a size, a name and a value, in that order in memory:
	.equiv SYMBOL_RECORD_SIZE_SIZE, 1
	.equiv SYMBOL_RECORD_VALUE_SIZE, 2
	.macro INSERT_SYMBOL name:req, value:req
		.byte NAME_LENGTH_\@ + SYMBOL_RECORD_SIZE_SIZE + SYMBOL_RECORD_VALUE_SIZE#\@ is a count of the invocations of the macro, making sufficiently unique symbols in GAS.
	name_label_\@:
		.ascii "\name"#The label allows the calculation of string length.
		.equiv NAME_LENGTH_\@, . - name_label_\@# a single . is the current position in the assembled file
		.short \value
	.endm

	.irp count, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
	INSERT_SYMBOL R\count, \count
	.endr
	INSERT_SYMBOL SP, 0
	INSERT_SYMBOL LCL, 1
	INSERT_SYMBOL ARG, 2
	INSERT_SYMBOL THIS, 3
	INSERT_SYMBOL THAT, 4
	INSERT_SYMBOL SCREEN, 0x4000#16384
	INSERT_SYMBOL KBD, 0x6000#24576
.equiv PRESET_SYMBOLS_LENGTH, . - preset_symbols# a single . is the current position in the assembled file


.section .bss
.equiv INPUT_BUFFER_SIZE, (1<<(8 * SYMBOL_RECORD_SIZE_SIZE)) - SYMBOL_RECORD_SIZE_SIZE - SYMBOL_RECORD_VALUE_SIZE + 2#INPUT_BUFFER_SIZE should accommodate this input: "(maximum_label_length)\r\|\n\|\t\| ". maximum_label_length = 2^(8 * SYMBOL_RECORD_SIZE_SIZE) - SYMBOL_RECORD_SIZE_SIZE - SYMBOL_RECORD_VALUE_SIZE - 1. Adding 3 accounts for parentheses and whitespace. As of typing, the total is 0xff, i.e. 255. In Hack assembly, parentheses define label symbols.
.lcomm INPUT_BUFFER, INPUT_BUFFER_SIZE#Reserve INPUT_BUFFER_SIZE bytes for "local common" denoted INPUT_BUFFER.

.section .text
#IMPORTANT NOTE: The following registers are considered "global" -- not to be thoughtlessly tampered with anywhere, but in the 'main'/_start procedure: %rsp(excepting calls and rets), %rbp, %r8, %r12, %r13, %r14, %r15.
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

label_symbols:
#Call this on a line of the form (<something>), then it shall add a label symbol to the symbol table with the value as the number of non-label, non-comment, non-whitespace lines of code in the source file.
#Accepts: %r8 representing line length, %r14 representing the start of the symbol table, %r12 is the number of (non-label) lines read
#Modifies: %r14, %rax, %rcx, %rdx, %rsi, %rdi
#Intended "returns": %r14.
	leaq -2(%r8), %rax#'Remove' parentheses around label.
	movq $(INPUT_BUFFER + 1), %rsi#Start from after the first '('.
	movq %r12, %rdx
#		********FALLTHROUGH********
add_symbol:
#Adds a new symbol to the symbol table and updates the %r14 register with the new address of the start of the symbol table.
#Accepts: %rax representing symbol string length, %r14 representing the start of the symbol table, %rdx is the value of the symbol, %rsi is the address to read the symbol from
#Modifies: %r14, %rax, %rcx, %rdi
#Intended "returns": %r14.
	movw %dx, -SYMBOL_RECORD_VALUE_SIZE(%r14)#NOTE: If SYMBOL_RECORD_VALUE_SIZE is not 2, this shoud be changed.
	movq %rax, %rcx#%rcx used by rep movsb.
	addq $(SYMBOL_RECORD_SIZE_SIZE + SYMBOL_RECORD_VALUE_SIZE), %rax#Make room on the stack/symbol table, including the size and value fields...
	subq %rax, %r14#...
	movb %al, (%r14)#Record size of new symbol.
	movq %r14, %rdi
	addq $SYMBOL_RECORD_SIZE_SIZE, %rdi#Overwrite not the size field.
	cld#Ensure forward direction (incrementing %rsi and %rdi)
	rep movsb#Uses %rsi passed into this procedure.
	ret#I wonder, does register renaming on modern CPUs extend to return addresses on the stack?

a_instr:#Converts number after @ from an ASCII string to a binary number (or converts a non-number after to a binary number by finding the matching string in the symbol table and getting the symbol value), and then into an ASCII representation of that binary number! Then writes it to the file.
#Accepts: %r8 representing line length, ST_FD_OUT(%rbp) is the file descriptor, %r14 is the start of the symbol table, %r13 is the end of the symbol table, %r12 is the number of currently existing variable symbols
#Modifies: %rax, %rbx, %rcx, %rdx, %rdi, %rsi, %r9, %r10, %r11, %r12, %r14.
#Intended "returns": %r14.
	movb $'0, output#leftmost/most-significant 'bit' should be '0' to indicate an instruction that sets the A register in the Hack machine.
	cmpb $'0, (INPUT_BUFFER + 1)#Is the first character after '@' a non-numeral? If so, try to find a pre-existing symbol that matches or, if that fails, add a new symbol using the whole string...
	jl symbol#...
	cmpb $'9, (INPUT_BUFFER + 1)#...	It only would be faster to use a register here (according to http://www.agner.org/optimize/instruction_tables.pdf) if comparing more than twice.
	jg symbol#...
	jmp no_symbol#...
symbol:
	xorq %rdx, %rdx#Store zeroes in high bytes to allow correct addition using the low byte.
	xorq %rbx, %rbx#Just in case: the higher bytes must be clear for adding to %rax, cmping %rbx and setting %rcx.
	movq %r14, %rax#Start search at beginning of symbol table.
symbol_search_loop:
	movb (%rax), %bl#Get length of current symbol.
	subb $SYMBOL_RECORD_VALUE_SIZE, %bl#Symbol length without value.
	cmpb %r8b, %bl#As the first character in the input is '@' and the first 'character' of the symbol is its size, this is valid.
	jne no_symbol_match#Of course, matching symbols must have matching lengths.
	movq $(INPUT_BUFFER + 1), %rsi#Start from 1, after '@' and size, for input and symbol, respectively. If SYMBOL_VALUE_SIZE_SIZE is not 1, then this is invalid.
	leaq 1(%rax), %rdi#Start from 1, after '@' and size, for input and symbol, respectively. If SYMBOL_VALUE_SIZE_SIZE is not 1, then this is invalid.
	leaq -1(%rbx), %rcx#Compare all characters except the first ('@' or size number). If SYMBOL_VALUE_SIZE_SIZE is not 1, then this is invalid.
	cld#Ensure forward direction (incrementing %rsi and %rdi).
	repe cmpsb#Compares all the characters in the current symbol and the input, and stops early if there is a mismatch.
	jne no_symbol_match#If the repe ends with one of the comparisons being unequal, the current symbol failed to match the input.
	movw (%rax, %rbx, 1), %dx#Finally, extract the symbol's value.
	jmp end_symbol
no_symbol_match:
	movb (%rax), %dl
	addq %rdx, %rax#This works because %rdx is initialized to 0, so all high bytes are always 0
	cmpq %r13, %rax
	jl symbol_search_loop
#If no symbol is found, add it (a variable symbol) here (%r12 contains the number of such variable symbols added; setting %rdx to %r12 + 16 sets the symbol value AND sets up the value written to the file here):
	leaq -1(%r8), %rax#Same speed as two adds, according to http://www.agner.org/optimize/instruction_tables.pdf. More importantly, slightly less code! Subtract 1 to account for '@'.
	leaq 16(%r12), %rdx#Same speed as two adds, according to http://www.agner.org/optimize/instruction_tables.pdf. More importantly, slightly less code!
	movq $(INPUT_BUFFER + 1), %rsi#Start from after the first '@'.
	call add_symbol
	incq %r12#Increment this so the next variable symbol has a different memory address in the Hack language code.
	jmp end_symbol
no_symbol:
	xorq %rax, %rax#Store zeroes in high bytes to allow correct addition using the low byte.
	xorq %rcx, %rcx#%rcx counts through the decimal digits in ASCII, starting from 1 (i.e. not '@')
	xorq %rdx, %rdx#%rdx stores the number represented by the decimal ASCII
a_instr_to_binary_loop:
	incq %rcx
	cmpq %rcx, %r8
	je end_symbol
	imulw $10, %dx#Should be fine, as correct inputs are never greater than 0xffff.
	movb INPUT_BUFFER(%rcx), %al
	addw %ax, %dx
	subw $'0, %dx#Thankfully, I can assume correct input.
	jmp a_instr_to_binary_loop
end_symbol:
	movw $14, %cx#The leftmost bit (15) has already been set to 0.
	xorq %rax, %rax#%rax counts through the binary digits in output ASCII, starting from the second leftmost/most-significant
binary_to_ascii_loop:
	incq %rax
	btw %cx, %dx#Could do shrw %cl, %bx after a movw %dx, %bw and then andb $1, %bl, but this is clearer in intent and (on Intel's Haswell architecture, at least) faster and not limited to a one-byte offset (i.e. %cl) register.
	setc %bl
	addb $'0, %bl#Adding 1 to the ASCII code for '0' gives '1', while 0 gives '0'
	movb %bl, output(%rax)#Finally, set the value in output string
	decw %cx
	cmpq $(OUTPUT_LENGTH - 2), %rax#output($(OUTPUT_LENGTH - 1)) is '\n'
	jl binary_to_ascii_loop
	__SYSCALL $SYS_WRITE, ST_FD_OUT(%rbp), $output, $OUTPUT_LENGTH#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	ret

.macro FIND_CHAR character:req#Does this count as abuse of the RFLAGS register?
#Accepts: %rbx as start-point (inclusive), %rdx as end-point (exclusive)
#Modifies: %rax, %rcx, %rdi, %rflags
#Intended "returns": %rflags, %rdi.
	movq %rdx, %rcx
	subq %rbx, %rcx
	movq %rbx, %rdi
	movb \character, %al
	repne scasb
.endm

.macro ZERO_ASCII_OUT bit:req, args:vararg#ZERO the output 'bits' using a recursive loop on args (if not blank).
	movb $'0, \bit
	.ifnb \args
		ZERO_ASCII_OUT \args
	.endif
.endm

.macro COPY_BYTE source:req, dest:req
#Modifies: %rax
	movb \source, %al
	movb %al, \dest
.endm

.macro RECORD_EFFECT_OF_CHAR effect:req, character:req, output_bit:req, args:vararg#If the character is or is not found, modify the supplied 'bit' dependent on the selected jump instruction.
#Accepts: %rbx as start-point (inclusive), %rdx as end-point (exclusive)
#Modifies: %rax, %rcx, %rdi, %rflags, output
#Intended "returns": %rflags, %rdi output.
	FIND_CHAR \character
	\effect effect_label\@#Could add a test here to allow to skip to an arbitrary address (and then jump to that address at the end of the macro, as well), which would cut out redundant jumps and improve speed, but probably with a significant readability cost. Sha'n't do it.
	ZERO_ASCII_OUT \output_bit \args
effect_label\@:#\@ is a count of the invokations of the macro, making a unique label.
.endm

.macro RECORD_PRESENCE_OF_CHAR character:req, output_bit:req, args:vararg#If the character is found, the supplied 'bit' is unset.
#Accepts: %rbx as start-point (inclusive), %rdx as end-point (exclusive)
#Modifies: %rax, %rcx, %rdi, %rflags, output
#Intended "returns": %rflags, %rdi output.
	RECORD_EFFECT_OF_CHAR jne, \character, \output_bit, \args
.endm

.macro RECORD_LACK_OF_CHAR character:req, output_bit:req, args:vararg#If the character is NOT found, the supplied 'bit' is unset.
#Accepts: %rbx as start-point (inclusive), %rdx as end-point (exclusive)
#Modifies: %rax, %rcx, %rdi, %rflags, output
#Intended "returns": %rflags, %rdi output.
	RECORD_EFFECT_OF_CHAR je, \character, \output_bit, \args
.endm

c_instr:#Spaghetti code? Must determine which sub-instructions exist based on the presence or absence of = and ; characters. Then generate binary ASCII representation of each sub-instruction, including nulls. Then write to file.
#Accepts: %r8 representing line length, ST_FD_OUT(%rbp) is the file descriptor
#Modifies: %rax, %rbx, %rcx, %rdx, %rdi, %rsi, %r9, %r10
#Intended "returns": none.
#0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15
#15	14	13	12	11	10	9	8	7	6	5	4	3	2	1	0
#1	1	1	a	c1	c2	c3	c4	c5	c6	d1	d2	d3	j1	j2	j3
#Bits 13-15 (or 15-13, i.e. memory locations 'output + $' with $ ranging from 0-2) are always '1' for c-instructions in Hack.
	.equiv A_BIT,		output + 3#a	#If '1', then use the 'M' 'register', i.e. memory pointed to by 'A' register; otherwise use 'A' register.
	.equiv ZX_BIT,		output + 4#c1	#If '1', the ALU zeroes the x-input (D register).
	.equiv NX_BIT,		output + 5#c2	#If '1', the ALU negates the x-input (D register).
	.equiv ZY_BIT,		output + 6#c3	#If '1', the ALU zeroes the y-input (A or M registers).
	.equiv NY_BIT,		output + 7#c4	#If '1', the ALU negates the y-input (A or M registers).
	.equiv F_BIT,		output + 8#c5	#If '1', the ALU outputs x+y, else x&y.
	.equiv NO_BIT,		output + 9#c6	#If '1'. the ALU negates the output.
	.equiv A_DEST_BIT,	output + 10#d1	#If '1', the corresponding register stores the ALU output...
	.equiv D_DEST_BIT,	output + 11#d2	#...
	.equiv M_DEST_BIT,	output + 12#d3	#...
	.equiv JL_BIT,		output + 13#j1	#Jump is lesser...
	.equiv JE_BIT,		output + 14#j2	#...or equal...
	.equiv JG_BIT,		output + 15#j3	#...or greater.
	movq $INPUT_BUFFER, %r9#initial start-point
	movq $INPUT_BUFFER, %rsi
	addq %r8, %rsi#initial end-point
	movq $output, %rdi
	movq $(OUTPUT_LENGTH - 1), %rcx
	movb $'1, %al
	cld#Ensure forward direction (incrementing %rdi) for all reps in this procedure (including those in macroes).
	rep stosb#Set all the characters in output(%rdi) to '1', using %rcx(counter) and %al(value)
#dest:	d1-A:	d2-D:	d3-M:
#null	0	0	0
#M	0	0	1
#D	0	1	0
#MD	0	1	1
#A	1	0	0
#AM	1	0	1
#AD	1	1	0
#ADM	1	1	1
	movq %r9, %rbx
	movq %rsi, %rdx
	RECORD_LACK_OF_CHAR $'=, A_DEST_BIT, D_DEST_BIT, M_DEST_BIT#First determine whether there are any destinations. If no '=', then null the destination bits as there are no destinations.
	jne find_jump
	movq %rdi, %r9#Important to NOT search for anything else BEFORE the '=' sign, and better not to include it.
	decq %rdi#repne in FIND_CHAR increments %rdi beyond end-point.
	movq %rdi, %rdx#Important to NOT search for destinations AFTER the '=' sign.
	RECORD_LACK_OF_CHAR $'A, A_DEST_BIT
	RECORD_LACK_OF_CHAR $'D, D_DEST_BIT
	RECORD_LACK_OF_CHAR $'M, M_DEST_BIT
	movq %r9, %rbx#Set so that items before the '=' sign are no-longer searched. Makes no functional difference here, but should speed up later FIND_CHAR for 'J', as rep scasb has complexity 2n according to: http://www.agner.org/optimize/instruction_tables.pdf
	movq %rsi, %rdx#Now MUST search for destinations after the '=' sign.
find_jump:#All jumps begin with 'J', so no need to worry about the semi-colon
#jump:	j1-L:	j2-E:	j3-G:
#null	0	0	0
#JGT	0	0	1
#JEQ	0	1	0
#JGE	0	1	1
#JLT	1	0	0
#JNE	1	0	1
#JLE	1	1	0
#JMP	1	1	1
	RECORD_LACK_OF_CHAR $'J, JL_BIT, JE_BIT, JG_BIT#If no 'J', then null the jump bits and skip forward, as no jump is possible.
	jne comp
	movq %rdi, %rsi#comp part should not search after 'J'
	movq %rsi, %rbx#Search only after the 'J' found that indicates a jump
	subq $2, %rsi#comp part should not search after the ';'.
	FIND_CHAR $'M
	je comp_if_jump#If there IS an 'M' after the 'J', it can only be a 'JMP', so nothing should be done
	RECORD_PRESENCE_OF_CHAR $'N, JE_BIT
	je comp_if_jump#If no 'N', then must be some other predicate jump, if there IS an 'N' after the 'J', it can only be 'JNE', so no more should be done.
	RECORD_LACK_OF_CHAR $'L, JL_BIT
	RECORD_LACK_OF_CHAR $'E, JE_BIT
	RECORD_LACK_OF_CHAR $'G, JG_BIT
comp_if_jump:
	movq %r9, %rbx#Reset for comp.
	movq %rsi, %rdx#Necessary if there was a jump -- see earlier change to %rsi.
comp:#Almost the entire rest of this procedure deals with the computation/ALU bits.
#computation:	c1-zx:	c2-nx:	c3-zy:	c4-ny:	c5-f:	c6-no:
#a=0:*	a=1:*	x=0:	x=!x:	y=0:	y=!y:	**	out=!out
#0	0	1	0	1	0	1	0	#Handle all the one-character cases first. Also handle '0' first, as there is only one valid use.
#1	1	1	1	1	1	1	1
#D	D	0	0	1	1	0	0
#A	M	1	1	0	0	0	0
#!D	!D	0	0	1	1	0	1	#Next handle all the two-character cases.
#!A	!M	1	1	0	0	0	1
#-D	-D	0	0	1	1	1	1
#-A	-M	1	1	0	0	1	1
#-1	-1	1	1	1	0	1	0
#D+1	D+1	0	1	1	1	1	1
#A+1	M+1	1	1	0	1	1	1
#D+A	D+M	0	0	0	0	1	0
#D&A	D&M	0	0	0	0	0	0
#D|A	D|M	0	1	0	1	0	1
#D-1	D-1	0	0	1	1	1	0
#A-1	M-1	1	1	0	0	1	0
#D-A	D-M	0	1	0	0	1	1
#A-D	M-D	0	0	0	1	1	1
#*a determines whether x is A or M.
#** f determines whether the output uses x+y (when '1') or x&y (when '0').
	RECORD_PRESENCE_OF_CHAR $'0, A_BIT, NX_BIT, NY_BIT, NO_BIT
	je write_output#If there is a '0', no more processing should be needed, otherwise invalid
	RECORD_LACK_OF_CHAR $'M, A_BIT#It seems the reference assembler only sets the a-bit to '1' if 'M' is there, so not for '1', '-1' etc..
	jne no_M
	ZERO_ASCII_OUT ZY_BIT#If 'M', then the zy-bit is not set
	jmp yes_M
no_M:
	RECORD_PRESENCE_OF_CHAR $'A, ZY_BIT#Unless either 'A' or 'M' exist, zy-bit is set
yes_M:
	RECORD_PRESENCE_OF_CHAR $'D, ZX_BIT#Unless 'D' exists, zx-bit is set
	movq %rsi, %r10
	subq %r9, %r10#This gives the length of the comp section, should be 1-3 characters
	cmpq $2, %r10
	je two_character_comp
	jg three_character_comp
	FIND_CHAR $'1
	je write_output#For '1', the bits (apart from the a-bit) are all '1'
	COPY_BYTE ZX_BIT, NX_BIT#Already done the '0' case and the '1' case, so this applies to the other two 1-character cases
	COPY_BYTE ZY_BIT, NY_BIT#...
	ZERO_ASCII_OUT NO_BIT, F_BIT
	jmp write_output#All 1-character cases accounted for.
two_character_comp:
	COPY_BYTE ZX_BIT, NX_BIT#In all 2-character cases, this applies.
	RECORD_PRESENCE_OF_CHAR $'1, NY_BIT, NO_BIT#If '1' and two-characters, must be '-1'.
	je write_output
	COPY_BYTE ZY_BIT, NY_BIT#In all remaining 2-character cases, this applies.
	RECORD_LACK_OF_CHAR $'-, F_BIT#Affects only the '!$' cases.
	jmp write_output
three_character_comp:
	RECORD_PRESENCE_OF_CHAR $'|, F_BIT
	je write_output
	RECORD_PRESENCE_OF_CHAR $'&, NX_BIT, NY_BIT, F_BIT, NO_BIT
	je write_output
	FIND_CHAR $'+
	je add_comp
	FIND_CHAR $'1#Now, only must deal with subtraction, '$-1' is the easiest case.
	je subtract_one_comp
	cmpb $'D, (%rbx)
	je d_initial_subtraction
	ZERO_ASCII_OUT NX_BIT
	jmp write_output
d_initial_subtraction:
	ZERO_ASCII_OUT NY_BIT
	jmp write_output
add_comp:
	cmpb $'0, ZX_BIT#If '1', then 'D+1', all bits accounted for.
	jne write_output
	cmpb $'0, ZY_BIT#If '1', then 'A+1' or 'M+1', all bits accounted for.
	jne write_output
	ZERO_ASCII_OUT NX_BIT, NY_BIT, NO_BIT
	jmp write_output
subtract_one_comp:
	COPY_BYTE ZX_BIT, NX_BIT
	COPY_BYTE ZY_BIT, NY_BIT
	ZERO_ASCII_OUT NO_BIT
write_output:
	__SYSCALL $SYS_WRITE, ST_FD_OUT(%rbp), $output, $OUTPUT_LENGTH#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	ret

.globl _start
_start:
	movq %rsp, %rbp#Change NOT %rbp hereafter.
	subq $-ST_FD_OUT, %rsp#This prevents overlapping with the file-descriptors...
#Copy built-in symbols to stack.
	movq %rsp, %r14#%r14 shall contain the start address of the symbol table.
	subq $(SYMBOL_TABLE_STACK_OFFSET + PRESET_SYMBOLS_LENGTH), %r14
	movq %rsp, %r13#%r13 shall contain the end adress of the symbol table.
	subq $SYMBOL_TABLE_STACK_OFFSET, %r13
	movq $preset_symbols, %rsi
	movq %r14, %rdi
	movq $PRESET_SYMBOLS_LENGTH, %rcx
	cld#Ensure forward direction (incrementing %rdi)
	rep movsb#After this, can now use space in .data section for preset_symbols as scratch space.

	movq ST_ARGV_1(%rbp), %rdi#Input filename.
	xorq %rcx, %rcx
filename_copy:#Stores filename in preset_symbols area, so there is a limit to the filename length, greater than 100 characters. More than enough, surely? :-P
	movb (%rdi, %rcx, 1), %al
	movb %al, preset_symbols(%rcx)
	incq %rcx
	cmpb $0, %al#Let us not try to copy all memory.
	jne filename_copy#Is there really no way to do this with x86_64 string operations, repne movsb?
	subq $4, %rcx
	movb $'h,  (preset_symbols + 0)(%rcx)#Now change the extension from .asm to .hack:
	movb $'a,  (preset_symbols + 1)(%rcx)#...
	movb $'c,  (preset_symbols + 2)(%rcx)#...
	movb $'k,  (preset_symbols + 3)(%rcx)#...
	movb $0,   (preset_symbols + 4)(%rcx)#...
	__SYSCALL $SYS_OPEN,                , $O_RDONLY, $PERM_RDWR_UGO#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	movq %rax, ST_FD_IN(%rbp)
	__SYSCALL $SYS_OPEN, $preset_symbols, $O_CREAT_WRONLY_TRUNC#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	movq %rax, ST_FD_OUT(%rbp)
	
read_loop_init:
	xorq %r15, %r15#Reserved for seeking within the input file. Use for nothing else until end.
	xorq %r12, %r12#Clear for use as line count for labels in 1st loop, variable count in 2nd.
read_loop_begin:#Read every line in the input file and process differently depending on the content.
#NOTE: Within this loop is the only safe place to modify %r8.
	__SYSCALL $SYS_LSEEK, ST_FD_IN(%rbp), %r15, $SEEK_SET#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	__SYSCALL $SYS_READ, ST_FD_IN(%rbp), $INPUT_BUFFER, $INPUT_BUFFER_SIZE#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	cmpq $0, %rax
	jle   end_read_loop

	.macro CMP_JE label:req, input_value:req, character:req, args:vararg
		cmpb \character, \input_value
		je \label
		.ifnb \args
			CMP_JE \label \input_value, \args
		.endif
	.endm
	movq $-1, %r8#Integer underflow.
find_command:#Skip through whitespace and stop at first non-whitespace character, then reset outer loop and then, finally, allow processing of commands or comments:
	incq %r8
	cmpq $INPUT_BUFFER_SIZE, %r8
	jge continue_read_loop#Maximum line-size is $INPUT_BUFFER_SIZE
	movb INPUT_BUFFER(%r8), %al
	cmpb $' ', %al#See the ASCII table -- all below 0x20 (SPACE) are also whitespace...
	jbe find_command#... or otherwise not printed.
	cmpq $0, %r8#This preserves the $INPUT_BUFFER_SIZE-character-per-line maximum.
	jne continue_read_loop#...
	CMP_JE comment_line, %al, $'/#Is it ever possible to have a non-comment line that starts with '/'?
	jmp command_line
continue_read_loop:
	addq %r8, %r15
	jmp read_loop_begin

comment_line:
	movq $-1, %r8#Integer underflow.
find_newline:#Find end of the comment line and then return to skipping through whitespace in the find_command loop, above.
	incq %r8
	cmpq $INPUT_BUFFER_SIZE, %r8
	jge end_read_loop#Handling longer comments without confusing the contents for commands would add complication.
	CMP_JE find_command, INPUT_BUFFER(%r8), $'\n, $'\r#Should account for newlines in GNU/Linux (\n, LF), OSX (\r, CR) and Windows (\r\n, CRLF).
	jmp find_newline

command_line:
	movq $-1, %r8#Integer underflow.
find_non_command:#Determine the point at which the command 'ends' and store it in %r8 so that it is passed to a_instr, c_instr or label_symbols. Also add it to %r15 so that the next command read is 'pure'.
	incq %r8
	cmpq $INPUT_BUFFER_SIZE, %r8
	jge end_read_loop#Maximum line-size is $INPUT_BUFFER_SIZE. The symbol table could not handle more, and any instruction that did not use symbols could not (correctly) use anywhere near this much.
	movb INPUT_BUFFER(%r8), %al
	cmpb $' ', %al#See the ASCII table -- all below 0x20 (SPACE) are also whitespace...
	jbe pass_decision#... or otherwise not printed.
	CMP_JE pass_decision, %al, $'/#stop at the beginning of a comment. Is OK as correct comments have two '/'s.
	jmp find_non_command

pass_decision:
	jmp first_pass_loop#This is modified. <<<<<<<<<<<----||||||

second_pass_loop:
	CMP_JE call_a_instr, INPUT_BUFFER, $'@#Expect more of these than labels, so it should be marginally faster to test for these first.
	CMP_JE find_command, INPUT_BUFFER, $'(#Ignore labels after first pass
	call c_instr#At this point, everything else should be a c-instruction, assuming correct input (as nand2tetris suggests).
	jmp find_command
call_a_instr:
	call a_instr
	jmp find_command

first_pass_loop:#Add all labels in the Hack assembly file to the symbol table.
	incq %r12#Each non-label line has a line address, and so is potentially addressable by a label.
	cmpb $'(, INPUT_BUFFER
	jne find_command
	decq %r12#Increment not if label, as labels don't enter machine code and so aren't addressable
	call label_symbols
	jmp find_command

end_read_loop:
	jmp end_first_loop#This is modified. <<<<<<<<<<<----||||||
	__SYSCALL $SYS_CLOSE, ST_FD_OUT(%rbp)#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	__SYSCALL $SYS_CLOSE, ST_FD_IN(%rbp)#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	__SYSCALL $SYS_EXIT, $EXIT_SUCCESS#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11

end_first_loop:#Self-modifying code that depends on the page size being 4096 (0x1000) bytes.
#Unfortunately, no easy way (seemingly) to operate on the label values at assembling time. That is, to perform rounding at assembly-time.
#From the GNU as info document:
#"Infix operators" take two arguments, one on either side.  Operators
#have precedence, but operations with equal precedence are performed left
#to right.  Apart from `+' or `-', both arguments must be absolute, and
#the result is absolute.
#CURSES! Confound it!
#This could be done with multiplication and division rather than bit-twiddling, but this problem is not worth writing multiplication and division macroes that use additions and subtractions in GAS macro syntax.
#Or is it?
	.equiv page_size, 0x1000#Would be better to determine page size at runtime.
	.equiv page_rounder, ~(page_size - 1)#For page_size of 0x1000, 0xfffffffffffff000
	movq $pass_decision, %rdi
	andq $page_rounder, %rdi#Round to page boundary. Should be 0x400000.
	__SYSCALL $SYS_MPROTECT, , $0x1000, $(PROT_READ|PROT_WRITE|PROT_EXEC)#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	movw $0x9090, pass_decision#Replace jump to this position with two NOPs. Should be 8-bit relative jump like: eb 2c. Changed to: 90 90
	movq $end_read_loop, %rdi#Again, just in case this is on a different page.
	andq $page_rounder, %rdi#Round to page boundary. Should be 0x400000.
	__SYSCALL $SYS_MPROTECT#number: rax, return: rax, args: rdi, rsi, rdx, r10, r8, r9, destroys: rcx, r10, r11
	movw $0x9090, end_read_loop#Replace jump to this position with two NOPs. Should be 8-bit relative jump like: eb 2a. Changed to: 90 90
	jmp read_loop_init
