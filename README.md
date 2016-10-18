# x86_64-nand2tetris-Hack-assembler

nand2tetris Hack Assembler written in x86_64 assembly language, GAS/GNU/AT&amp;T syntax. That is, a complex "real" assembly language is used to write an assembler for a "toy"/conceptual CPU. It should be able to assemble the test programs provided by the course (http://nand2tetris.org/06.php). It has been tested with GNU assembler version 2.25.

To assemble and run on a file called "test.asm":
```bash
as hack_assembler.s -o hack_assembler.o && ld -m elf_x86_64 hack_assembler.o -o hack_assembler.elf && ./hack_assembler.elf test.asm && echo $?
```

The creators of the nand2tetris course have requested that answers for the assignments (like this one) not be released to the public. In my defence, anyone having trouble writing the assembler in a higher-level language would likely find the x86_64 assembly harder to work out than the assignment itself (I did; I used the assignment as an excuse to teach myself some x86_64 assembly). Also, I suspect that anyone familiar with x86_64 assembly (especially in GAS/GNU/AT&amp;T syntax) would not need, nor seek out, any help.
