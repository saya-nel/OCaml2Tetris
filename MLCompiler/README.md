# MLCompiler

```
$ cd MLCompiler
$ make
$ ./ Samples/main.ml
~> créé un fichier Samples/Main.vm contenant le bytecode Nand2Tetris correspondant au programme ml d'entrée

~> puis, tester Samples/main.vm avec VMEmulator.sh 

```

```
<prog> := <decl> <prog> 
        | <decl> ;; <prog>

<decl> := let <ident> <ident> ... = <expr> 
        | let rec <ident> <ident> ... = <expr> 
        | type <ident> = <Constr> | <Constr> | ... # non paramétré
        | external <ident> : <expr_ty> = "<ident>"
        
<expr> := (<expr>)
        | <constant>
        | <expr> <op> <expr>
        | <op> <expr>
        | let <ident> = <expr> in <expr> 
        | <expr> ; <expr>
        | if <expr> then <expr> else <expr>
        | match <expr> with | <constant> -> <expr> | ... | _ -> <expr>   # pour le moment, transformé en if imbriqués
        | while <expr> do <expr> done
        | for <expr> = <expr> to <expr> do <expr> done  # pas encore de génération de code
        | ref <expr>
        | !<expr>
        | <expr> := <expr>
        | [|<expr>;<expr>;...|]
        | <expr>.(<expr>)
        | <expr>.(<expr>) <- <expr>
        | assert <expr>

<constant> := <bool>
            | <int>
            | ()
            | [||]
            | <Const>
            | <string>

<op> := + | - | = | < | <= | > | >= | `&&` | `||` | not

# remarque : pour le moment, `=`,`>` et `<` ne fonctionne qu'avec des entiers. 

<primitives> :=  print_char | print_int | print_string | print_newline | read_int | exit | Array.length
```

## Compilation 

```
$ make
$ ./MLCompiler.sh <file-name>
```
* parse un fichier d'entrée suivant la gramaire ci-dessus. Stop si erreur de parsing.
* vérifie que le programme est bien typé grâce au typeur d'ocamlopt. Stop si le programme et mal-typé.
* engendre du bytecode exécutable sur la plateforme Nand2Tetris
