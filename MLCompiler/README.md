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

<expr> := (<expr>)
        | <constant>
        | <expr> <binop> <expr>
        | <unop> <expr>
        | let <ident> = <expr> in <expr> 
        | ( <expr> ; <expr> ...)
        | if <expr> then <expr> else <expr>
        | match <expr> with | <constant> -> <expr> | ... | _ -> <expr>   # pour le moment, transformé en if imbriqués
        | while <expr> do <expr> done
        | for <expr> = <expr> to <expr> do <expr> done     # pas encore de génération de code
        | ... # tableaux, reference etc... pas encore fini

<constant> := <bool>
            | <int>
            | ()
            | <string>

<binop> := + | - | = | < | > | `&&` | `||`    

# remarque : pour le moment, `=`,`>` et `<` ne fonctionne qu'avec des entiers.     

<unop> := not
```

## Compilation 
* parse un fichier d'entrée suivant la gramaire ci-dessus. Stop si erreur de parsing.
* vérifie que le programme est bien typé grace au typeur d'ocamlopt. Stop si le programme et mal-typé.
* engendre du bytecode exécutable sur la plateforme Nand2Tetris