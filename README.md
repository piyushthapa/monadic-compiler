# monadic-compiler
Project work done for Cardano Developer Associate Course  from Emurgo

## Exercise detail
http://www.cs.nott.ac.uk/~pszgmh/afp-cwk2.pdf

sample codes 
### Factorial
```
Seqn [
            Assign 'A' (Val 1), 
            Assign 'B' (Val n),
            While (Var 'B') (Seqn [
                Assign 'A' (App Mul (Var 'A') (Var 'B')),
                Assign 'B' (App Sub (Var 'B') (Val 1))
            ])
        ]
```

### Sum of N Number
```
 Seqn [
            Assign 'A' (Val 0),
            Assign 'B' (Val n),
            While (Var 'B') (Seqn [ 
                Assign 'A' (App Add (Var 'A') (Var 'B')),
                Assign 'B' (App Sub (Var 'B') (Val 1))
            ])
      ]
```

### TODO
1. Use WriterT monad to generate Code 
2. Add more Arth Operator (Mod)
3. Add Boolean Operator (AND, OR , NOT) 
4. Parser

