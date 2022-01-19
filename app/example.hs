module BasicCompiler.Example where

fac n = Seqn [
            Assign 'A' (Val 1), 
            Assign 'B' (Val n),
            While (Var 'B') (Seqn [
                Assign 'A' (App Mul (Var 'A') (Var 'B')),
                Assign 'B' (App Sub (Var 'B') (Val 1))
            ])
        ]