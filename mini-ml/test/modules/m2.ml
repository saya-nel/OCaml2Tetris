let x = M1.x + 1

let _ = assert (x = 17);
        assert (M1.f 41 = 42);
        print_int ((M1.id M1.id) 42);
        print_int (M1.id 42);
        assert ((M1.id 42) = ((M1.id M1.id) 42));
        assert (x = M1.x + 1)