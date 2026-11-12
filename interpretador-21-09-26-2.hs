-- stack ghci --package silently --package containers --package HUnit interpretador-25-04-26.hs

module Interp where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit
import System.IO.Silently (capture_)

-- ============================================================
-- Tipos da arvore sintatica 
-- ============================================================

data Type
  = TInt
  | TBool
  | TFloat  -- Tipo Float 
  | TVoid
  | TArray Type  -- [Antônio Guedes] Adicionado tipo Array
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Gt  | Le | Ge
  | And | Or
  deriving (Show, Eq)

data Expr
  = Lit   Int
  | FLit  Float 
  | BLit  Bool
  | Var   String
  | BinE  BinOp Expr Expr
  | Not   Expr
  | Call  String [Expr]
  | ArrInit Type Expr        -- [Antônio Guedes] Inicializar array: novo tipo[tamanho]
  | ArrGet  String Expr      -- [Antônio Guedes] Acessar índice: arr[i]
  deriving (Show, Eq)

data Stmt
  = Decl   Type String Expr    
  | Assign String Expr         
  | ArrSet String Expr Expr    -- [Antônio Guedes] Atribuir em índice: arr[i] = x
  | If     Expr Stmt Stmt
  | While  Expr Stmt
  | For    Stmt Expr Stmt Stmt -- [Antônio Guedes] Limitação Extra: For (Init) (Cond) (Inc) (Body)
  | Block  [Stmt]
  | Return Expr                
  | Print  Expr                
  | Skip
  deriving (Show, Eq)

data FunDecl = FunDecl
  { funName   :: String
  , funParams :: [(Type, String)]
  , funBody   :: Stmt
  } deriving (Show, Eq)

-- ============================================================
-- Valores e estado do programa (ambiente)
-- ============================================================

data Value
  = VInt  Int
  | VFloat  Float 
  | VBool Bool
  | VVoid
  | VArray [Value]  -- [Antônio Guedes] Representação do Array na memória
  deriving (Show, Eq)

type Env    = Map String Value
type FunEnv = Map String FunDecl

data StmtResult
  = Next     Env
  | Returned Value Env
  deriving (Show)

-- ============================================================
-- Avaliação de Expressões 
-- ============================================================

evalExpr :: FunEnv -> Env -> Expr -> IO Value
evalExpr fenv env expr = case expr of

  Lit  n -> return (VInt n)
  BLit b -> return (VBool b)

  Var x  -> case Map.lookup x env of
               Nothing -> error ("Variavel nao declarada: " ++ x)
               Just v  -> return v

  Not e  -> do
    v <- evalExpr fenv env e
    case v of
      VBool b -> return (VBool (not b))
      _       -> error "Not aplicado a nao-booleano"

  BinE op l r -> do
    vl <- evalExpr fenv env l
    vr <- evalExpr fenv env r
    return (evalBinOp op vl vr)

  -- [Antônio Guedes] Inicialização de Array (com valores padrão)
  ArrInit t sizeExpr -> do
    sizeVal <- evalExpr fenv env sizeExpr
    case sizeVal of
      VInt size -> do
        let defVal TInt       = VInt 0
            defVal TBool      = VBool False
            defVal TVoid      = VVoid
            defVal (TArray _) = VVoid
        if size >= 0
          then return (VArray (replicate size (defVal t)))
          else error "Tamanho do array invalido"
      _ -> error "Tamanho do array deve ser um numero inteiro"

  -- [Antônio Guedes] Acesso a índice de Array
  ArrGet x idxExpr -> do
    idxVal <- evalExpr fenv env idxExpr
    case (Map.lookup x env, idxVal) of
      (Just (VArray arr), VInt i) ->
        if i >= 0 && i < length arr
          then return (arr !! i)
          else error ("Indice " ++ show i ++ " fora dos limites do array " ++ x)
      (Nothing, _) -> error ("Array nao declarado: " ++ x)
      _            -> error "Variavel nao eh um array ou indice invalido"

  Call fname args ->
    case Map.lookup fname fenv of
      Nothing  -> error ("Funcao nao declarada: " ++ fname)
      Just fun -> do
        let nparams = length (funParams fun)
            nargs   = length args
        if nparams /= nargs
          then error ("Funcao " ++ fname ++ " espera " ++ show nparams ++ " argumentos")
          else do
            vals <- mapM (evalExpr fenv env) args
            let params   = map snd (funParams fun)
                envLocal = Map.fromList (zip params vals)
            result <- execStmt fenv envLocal (funBody fun)
            case result of
              Returned v _ -> return v
              Next _       -> return VVoid

-- ============================================================
-- evalBinOp
-- ============================================================

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Add (VInt  a) (VInt  b) = VInt  (a + b)
evalBinOp Add (VFloat a) (VInt b) = VFloat (a + b)      
evalBinOp Add (VInt a) (VFloat b)  = VFloat (a + b)
evalBinOp Add (VFloat a) (VFloat b)  = VFloat (a + b)
evalBinOp Sub (VInt  a) (VInt  b) = VInt  (a - b)
evalBinOp Mul (VInt  a) (VInt  b) = VInt  (a * b)
evalBinOp Div (VInt  a) (VInt  b) = VInt  (a `div` b)
evalBinOp Mod (VInt  a) (VInt  b) = VInt  (a `mod` b)
evalBinOp Eq  va        vb        = VBool (va == vb)
evalBinOp Neq va        vb        = VBool (va /= vb)
evalBinOp Lt  (VInt  a) (VInt  b) = VBool (a <  b)
evalBinOp Gt  (VInt  a) (VInt  b) = VBool (a >  b)
evalBinOp Le  (VInt  a) (VInt  b) = VBool (a <= b)
evalBinOp Ge  (VInt  a) (VInt  b) = VBool (a >= b)
evalBinOp And (VBool a) (VBool b) = VBool (a && b)
evalBinOp Or  (VBool a) (VBool b) = VBool (a || b)
evalBinOp op  _         _         = error ("Operacao invalida ou tipos incompativeis: " ++ show op)

-- ============================================================
-- Execução de Statements
-- ============================================================

execStmt :: FunEnv -> Env -> Stmt -> IO StmtResult
execStmt fenv env stmt = case stmt of

  Skip -> return (Next env)

  Print e -> do
    v <- evalExpr fenv env e
    putStrLn (showValue v)
    return (Next env)

  Decl _ x e -> do
    v <- evalExpr fenv env e
    let env' = Map.insert x v env
    return (Next env')

  Assign x e ->
    case Map.lookup x env of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just _  -> do
        v <- evalExpr fenv env e
        let env' = Map.insert x v env
        return (Next env')

  -- [Antônio Guedes] Atribuição em índice de Array
  ArrSet x idxExpr valExpr -> do
    idxVal <- evalExpr fenv env idxExpr
    val    <- evalExpr fenv env valExpr
    case (Map.lookup x env, idxVal) of
      (Just (VArray arr), VInt i) ->
        if i >= 0 && i < length arr
          then do
            -- Substitui o valor no índice específico da lista
            let arr' = take i arr ++ [val] ++ drop (i + 1) arr
            let env' = Map.insert x (VArray arr') env
            return (Next env')
          else error ("Indice " ++ show i ++ " fora dos limites do array " ++ x)
      (Nothing, _) -> error ("Array nao declarado: " ++ x)
      _            -> error "Variavel nao eh um array ou indice invalido"

  If cond t f -> do
    v <- evalExpr fenv env cond
    case v of
      VBool True  -> execStmt fenv env t
      VBool False -> execStmt fenv env f
      _           -> error "Condicao do If nao eh booleana"

  Return e -> do
    v <- evalExpr fenv env e
    return (Returned v env)

  Block stmts -> execBlock fenv env stmts

  While cond body -> do
    v <- evalExpr fenv env cond
    case v of
      VBool False -> return (Next env)
      VBool True  -> do
        r <- execStmt fenv env body
        case r of
          Returned v' env' -> return (Returned v' env')
          Next        env' -> execStmt fenv env' (While cond body)
      _ -> error "Condicao do While nao eh booleana"

  -- [Antônio Guedes] Execução do Laço For
  For initStmt condExpr incStmt bodyStmt -> do
    rInit <- execStmt fenv env initStmt
    case rInit of
      Returned v' env' -> return (Returned v' env')
      Next envInit     -> loop envInit
    where
      loop currentEnv = do
        vCond <- evalExpr fenv currentEnv condExpr
        case vCond of
          VBool False -> return (Next currentEnv)
          VBool True  -> do
            rBody <- execStmt fenv currentEnv bodyStmt
            case rBody of
              Returned v' _ -> return (Returned v' currentEnv)
              Next envBody  -> do
                rInc <- execStmt fenv envBody incStmt
                case rInc of
                  Returned v' _ -> return (Returned v' envBody)
                  Next envInc   -> loop envInc
          _ -> error "Condicao do For nao eh booleana"

-- ============================================================
-- Execução de um bloco
-- ============================================================

execBlock :: FunEnv -> Env -> [Stmt] -> IO StmtResult
execBlock _    env []     = return (Next env)
execBlock fenv env (s:ss) = do
  r <- execStmt fenv env s
  case r of
    Returned v env' -> return (Returned v env')
    Next       env' -> execBlock fenv env' ss

-- ============================================================
-- Utilitários
-- ============================================================

showValue :: Value -> String
showValue (VInt  n)   = show n
showValue (VBool b)   = if b then "verdadeiro" else "falso"
showValue VVoid       = "()"
showValue (VArray arr)= "[" ++ unwords (map showValue arr) ++ "]" -- [Antônio Guedes] Exibe os elementos

runProg :: FunEnv -> Stmt -> IO Value
runProg fenv prog = do
  result <- execStmt fenv Map.empty prog
  case result of
    Returned v _ -> return v
    Next _       -> return VVoid

runProgIO :: FunEnv -> Stmt -> IO String
runProgIO fenv prog = capture_ (execStmt fenv Map.empty prog)

-- ============================================================
-- Programas de teste
-- ============================================================

-- [Antônio Guedes] Teste Array: int[] arr = new int[3]; arr[1] = 20; return arr[1];
progArray :: Stmt
progArray = Block
  [ Decl (TArray TInt) "arr" (ArrInit TInt (Lit 3))
  , ArrSet "arr" (Lit 1) (Lit 20)
  , Return (ArrGet "arr" (Lit 1))
  ]

-- [Antônio Guedes] Teste For: int soma = 0; for(int i=0; i<3; i=i+1) { soma = soma + i }; return soma;
progFor :: Stmt
progFor = Block
  [ Decl TInt "soma" (Lit 0)
  , For (Decl TInt "i" (Lit 0))
        (BinE Lt (Var "i") (Lit 3))
        (Assign "i" (BinE Add (Var "i") (Lit 1)))
        (Assign "soma" (BinE Add (Var "soma") (Var "i")))
  , Return (Var "soma")
  ]

-- ============================================================
-- Suite de testes HUnit
-- ============================================================

testArray :: Test
testArray = TestCase $ do
  v <- runProg Map.empty progArray
  assertEqual "array[1] deve ser 20" (VInt 20) v

testForLoop :: Test
testForLoop = TestCase $ do
  v <- runProg Map.empty progFor
  assertEqual "for soma 0+1+2" (VInt 3) v

suite :: Test
suite = TestList
  [ TestLabel "teste de manipulacao de array" testArray
  , TestLabel "teste de laco for"             testForLoop
  ]

main :: IO ()
main = runTestTTAndExit suite