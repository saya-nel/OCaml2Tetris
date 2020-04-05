# mini-ml

```
$ cd mini-ml
$ make
$ ./compile -src=exemple/src -dst=exemple/bin   m1.ml m2.ml
$ VMEmulator.sh
~> click File->Load Script->exemple/bin/Main.tst
{click "animate" : No Animation}
{click "view" : Screen}
~> click Run->Run-> (Yes)
```

Par defaut, -src="", dst="generated_files"

```
<prog> := <decl> <prog> 
        | <decl> ;; <prog>

<decl> := let () = <expr> 
        | let _ = <expr> 
        | let g = <expr> 
        |let <ident> <ident> ... = <expr> 
        | type <ident> = <Constr> | <Constr> | ... # non paramétré
        | let rec <ident> <ident> ... = <expr> 

<expr> := (<expr>)
        | (<expr> : <expr_ty>)
        | <constant>
        | <expr> <op> <expr>
        | <op> <expr>
        | let <ident> = <expr> in <expr> 
        | <expr> ; <expr>
        | if <expr> then <expr> else <expr>
        | match <expr> with | <constant> -> <expr> | ... | _ -> <expr>  # (if imbriqués dichotomiques)
        | while <expr> do <expr> done
        | for <ident> = <expr> to <expr> do <expr> done
        | ref <expr>
        | !<expr>
        | <expr> := <expr>
        | [|<expr>;<expr>;...|]
        | <expr>.(<expr>)
        | <expr>.(<expr>) <- <expr>
        | assert <expr>
        | <primitives> <expr> ...
        | <ident> <expr> ... # application
<constant> := <bool>
            | <int>
            | ()
            | [||]
            | <Constr>
            | <string>

<expr_ty> := <ident>
           | <expr_ty> * ... * <expr_ty>
           | <expr_ty> -> <expr_ty>

<binop> := + | - | = | < | <= | > | >= | '&&' | '||'
<unop> := - | not
# remarque : =, > et < ne fonctionne qu'avec des entiers. 

<primitives> :=  print_char | print_int | print_string | print_newline | read_int | exit | incr | decr | Array.length | Array.get | Array.set | Array.make | String.get | String.make
...

```
