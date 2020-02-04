load,

output-file test.out,
output-list RAM[0]%D1.6.1 RAM[261]%D1.6.1;

repeat 10000 {
  vmstep;
}

output;