let mouvement de vers =
    N2t.print_string de; 
    N2t.print_string "-->";
    N2t.print_string vers; 
    N2t.print_newline ();;

let rec hanoi depart milieu arrivee = function
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
       mouvement depart arrivee;
       hanoi milieu depart arrivee (n - 1);;

mouvement "A" "B" ;;
hanoi "A" "B" "C" 1 ;;