module Main where
import Control.Monad.State.Lazy
import Control.Monad.Reader

main :: IO ()
main = putStrLn "Hello, Haskell!"



data Prog = Assign Name Expr
          | If Expr Prog Prog
          | While Expr Prog
          | Seqn [Prog]
        deriving Show

data Op = Add | Sub | Mul | Div 
            deriving Show

data Expr = Val Int | Var Name | App Op Expr Expr 
            deriving Show

type Name = Char



-- 

-- Virtual Machine 

type Stack = [Int]
type Mem = [(Name, Int)]
type Vm = (Stack, Mem)

type Code = [Inst]

data Inst = PUSH Int
          | PUSHV Name
          | POP Name
          | DO Op
          | JUMP Label 
          | JUMPZ Label
          | LABEL Label
            deriving Show

type Label = Int
    


calculateCode:: Prog -> State Int Code
--calculateCode (Seqn projects) = do
    
calculateCode (Assign name (Val n)) = do
     return [PUSH n, POP name]

calculateCode (Assign name (App op expr1 expr2)) = do
  return (exprToInst expr1 ++ exprToInst expr2 ++ [DO op, POP name])

calculateCode (Seqn (p: ps)) = do
  c_inst <- calculateCode p
  other_inst' <- calculateCode (Seqn ps)
  return (c_inst ++ other_inst')

calculateCode (While expr1 prg) = do 
     label <- get
     put (label + 1)
     inst <- calculateCode prg
     return ([LABEL label] ++ exprToInst expr1 ++ [JUMPZ (label+1)] ++ inst ++ [JUMP label] ++ [LABEL (label+1)])

calculateCode (If expr prg1 prg2) = do
  label <- get 
  put (label + 2)
  inst1 <- calculateCode prg1
  inst2 <- calculateCode prg2
  return  ([LABEL label] ++ exprToInst expr ++ [JUMPZ label] ++ inst1 ++ [LABEL (label + 1)])

calculateCode prg = return []

exprToInst :: Expr -> [Inst]
exprToInst (Val n) = [PUSH n]
exprToInst (Var name) = [PUSHV name]
exprToInst _ = []

comp :: Prog -> Code
comp prg =  evalState (calculateCode prg) 0


-- Couldn't find soln with below types
-- handleCode :: Code -> StateT Stack (Reader Code) Mem
handleCode :: Code -> StateT Vm (Reader Code) ()
handleCode [] = return  ()
handleCode (PUSH v: cs) = do
  (s, m) <- get 
  put (v:s, m)
  handleCode cs

handleCode (POP var: cs) = do
  (s, m) <- get
  let val = safeHead s
  put (safeTail s, safePutMem m var val)
  handleCode cs
  

handleCode ((PUSHV var) : cs) = do
  (s, m) <- get
  put (handlePushV s m var, m)
  handleCode cs

handleCode ((JUMP lbl) : cs) = do
  code <- lift ask 
  handleCode $ codeAfterLabel code lbl

handleCode ((JUMPZ lbl) : cs) = do
  (s,m) <- get 
  code <- lift ask

  case s of 
    (0: _) -> handleCode $ codeAfterLabel code lbl
    _ -> handleCode cs

handleCode (inst@(DO _) : cs) = do
  (s,m) <- get 
  put (handleOps inst s, m)
  handleCode cs

handleCode (_:cs) = handleCode cs

handleOps :: Inst -> Stack -> Stack
handleOps _ [] = []
handleOps (DO Mul) (x:y:xs) = (x*y) : xs
handleOps (DO Sub) (x:y:xs) = (y-x) : xs
handleOps (DO Add) (x:y:xs) = (x+y) : xs
handleOps (DO Div) (x:y:xs) = (y `div` x) : xs
handleOps _ stack = stack  

handlePushV :: Stack -> Mem -> Name -> Stack
handlePushV stack mem var = 
  case memLookup var mem of 
    Just v -> v : stack
    Nothing  -> stack

memLookup :: Name -> Mem -> Maybe Int
memLookup _ [] = Nothing 
memLookup var ((mkey, mval): ms)
  | var == mkey = Just mval
  | otherwise =  memLookup var ms

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (x: _) = Just x

safePutMem :: Mem -> Char -> Maybe Int -> Mem
safePutMem mem _ Nothing  = mem
safePutMem mem var (Just v) = upsertPairs mem var v 

upsertPairs:: Eq a =>  [(a,b)] -> a -> b -> [(a, b)] 
upsertPairs [] k v = [(k,v)]
upsertPairs ((pkey, pval) : ps) k v 
  | pkey == k = (k,v) : ps
  | otherwise = (pkey, pval) : upsertPairs ps k v

codeAfterLabel :: Code -> Int -> Code
codeAfterLabel [] _ = []
codeAfterLabel ((LABEL x) : cs) n
  | n == x = cs 
  | otherwise = codeAfterLabel cs n

codeAfterLabel (_: cs) n = codeAfterLabel cs n

-- ((), ([stack], [memory]))
-- ([stack], [memory])
-- [memory]
exec :: Code -> Mem
exec code  =  snd $ snd $ runReader (runStateT (handleCode code)([],[])) code

-- Some Examples 
fac n = Seqn [
            Assign 'A' (Val 1), 
            Assign 'B' (Val n),
            While (Var 'B') (Seqn [
                Assign 'A' (App Mul (Var 'A') (Var 'B')),
                Assign 'B' (App Sub (Var 'B') (Val 1))
            ])
        ]

nsum n = Seqn [
            Assign 'A' (Val 0),
            Assign 'B' (Val n),
            While (Var 'B') (Seqn [ 
                Assign 'A' (App Add (Var 'A') (Var 'B')),
                Assign 'B' (App Sub (Var 'B') (Val 1))
            ])
      ]


