# mini-ml

```
$ cd mini-ml
$ make
$ ./compile file.ml ...
```

```
<prog> := <decl> <prog> 
        | <decl> ;; <prog>

<decl> := let () = <expr> 
        | let _ = <expr> 
        | let g = <expr> 
        |let <ident> <ident> ... = <expr> 
      //| let rec <ident> <ident> ... = <expr> 
      //| type <ident> = <Constr> | <Constr> | ... # non paramétré
      //| external <ident> : <expr_ty> = "<ident>"

<expr> := (<expr>)
        | <constant>
        | <expr> <op> <expr>
        | <op> <expr>
        | let <ident> = <expr> in <expr> 
        | <expr> ; <expr>
        | if <expr> then <expr> else <expr>
        | match <expr> with | <constant> -> <expr> | ... | _ -> <expr>   # pour le moment, transformé en if imbriqués
        | while <expr> do <expr> done
        | for <ident> = <expr> to <expr> do <expr> done  # pas encore de génération de code
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
            | <Const>
            | <string>

<op> := + | - | = | < | <= | > | >= | `&&` | `||` | not
# remarque : `=`,`>` et `<` ne fonctionne qu'avec des entiers. 

<primitives> :=  print_char | print_int | print_string | print_newline | read_int | exit | Array.length | Array.get | Array.set | Array.make | Pervasives.incr | Pervasives.decr | incr | decr
...
```