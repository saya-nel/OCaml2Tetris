// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/07/StackArithmetic/SimpleAdd/SimpleAddVME.tst

load,
output-file Main.out,
output-list RAM[0]%D1.6.1 RAM[261]%D1.6.1 RAM[262]%D1.6.1;

set RAM[0] 256,  // initializes the stack pointer

repeat 20000000 {       // Main.vm has 3 instructions
  vmstep;
}

output;