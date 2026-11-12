-- stack ghci --package silently --package containers --package HUnit interpretador-25-04-26.hs

module Interp where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit
import System.IO.Silently (capture_)
import Control.Exception (try, ErrorCall)  -- [Guilherme Augusto] usado no teste de erro (local nao vaza)

-- ============================================================
-- Tipos da arvore sintatica 
-- ============================================================

data Type
  = TInt
  | TBool
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
  | VBool Bool
  | VVoid
  | VArray [Value]  -- [Antônio Guedes] Representação do Array na memória
  deriving (Show, Eq)

type Env    = Map String Value
type FunEnv = Map String FunDecl

-- [Guilherme Augusto] Escopo = ambiente global + ambiente local da função em execução.
--   globais  : variáveis declaradas no nível do programa (visíveis em todas as funções)
--   locais   : parâmetros e variáveis declaradas dentro da função atual
--   emFuncao : True dentro do corpo de uma função (Decl vai para locais);
--              False no nível do programa (Decl vai para globais)
data Scope = Scope
  { globais  :: Env
  , locais   :: Env
  , emFuncao :: Bool
  } deriving (Show)

-- [Guilherme Augusto] Escopo do programa principal: tudo vazio, fora de função
escopoInicial :: Scope
escopoInicial = Scope Map.empty Map.empty False

-- [Guilherme Augusto] Busca: local tem prioridade (sombreamento); se não achar, cai para global
lookupVar :: String -> Scope -> Maybe Value
lookupVar x sc = case Map.lookup x (locais sc) of
  Just v  -> Just v
  Nothing -> Map.lookup x (globais sc)

-- [Guilherme Augusto] Atualiza variável já existente: local primeiro, depois global.
-- Devolve Nothing se a variável não foi declarada em nenhum dos dois.
setVar :: String -> Value -> Scope -> Maybe Scope
setVar x v sc
  | Map.member x (locais sc)  = Just sc { locais  = Map.insert x v (locais sc) }
  | Map.member x (globais sc) = Just sc { globais = Map.insert x v (globais sc) }
  | otherwise                 = Nothing

-- [Guilherme Augusto] Declaração: dentro de função cria local (pode sombrear global);
-- no programa principal cria global.
declVar :: String -> Value -> Scope -> Scope
declVar x v sc
  | emFuncao sc = sc { locais  = Map.insert x v (locais sc) }
  | otherwise   = sc { globais = Map.insert x v (globais sc) }

-- [Guilherme Augusto] StmtResult agora carrega o Scope (e não só um Env)
data StmtResult
  = Next     Scope
  | Returned Value Scope
  deriving (Show)

-- ============================================================
-- Avaliação de Expressões 
-- ============================================================

-- [Guilherme Augusto] evalExpr devolve também o Scope resultante, pois avaliar uma
-- expressão (Call) pode alterar variáveis globais. Cada sub-expressão recebe
-- o escopo deixado pela anterior (ordem esquerda -> direita).
evalExpr :: FunEnv -> Scope -> Expr -> IO (Value, Scope)
evalExpr fenv sc expr = case expr of

  Lit  n -> return (VInt n, sc)
  BLit b -> return (VBool b, sc)

  Var x  -> case lookupVar x sc of
               Nothing -> error ("Variavel nao declarada: " ++ x)
               Just v  -> return (v, sc)

  Not e  -> do
    (v, sc1) <- evalExpr fenv sc e
    case v of
      VBool b -> return (VBool (not b), sc1)
      _       -> error "Not aplicado a nao-booleano"

  BinE op l r -> do
    (vl, sc1) <- evalExpr fenv sc  l
    (vr, sc2) <- evalExpr fenv sc1 r
    return (evalBinOp op vl vr, sc2)

  -- [Antônio Guedes] Inicialização de Array (com valores padrão)
  ArrInit t sizeExpr -> do
    (sizeVal, sc1) <- evalExpr fenv sc sizeExpr
    case sizeVal of
      VInt size -> do
        let defVal TInt       = VInt 0
            defVal TBool      = VBool False
            defVal TVoid      = VVoid
            defVal (TArray _) = VVoid
        if size >= 0
          then return (VArray (replicate size (defVal t)), sc1)
          else error "Tamanho do array invalido"
      _ -> error "Tamanho do array deve ser um numero inteiro"

  -- [Antônio Guedes] Acesso a índice de Array
  -- [Guilherme Augusto] busca do array via lookupVar (local, depois global)
  ArrGet x idxExpr -> do
    (idxVal, sc1) <- evalExpr fenv sc idxExpr
    case (lookupVar x sc1, idxVal) of
      (Just (VArray arr), VInt i) ->
        if i >= 0 && i < length arr
          then return (arr !! i, sc1)
          else error ("Indice " ++ show i ++ " fora dos limites do array " ++ x)
      (Nothing, _) -> error ("Array nao declarado: " ++ x)
      _            -> error "Variavel nao eh um array ou indice invalido"

  -- [Guilherme Augusto] Chamada de função:
  --   1) argumentos avaliados esquerda -> direita, propagando o escopo (evalArgs)
  --   2) o corpo roda com as globais ATUAIS (já alteradas pelos argumentos)
  --      e como locais apenas os parâmetros
  --   3) ao voltar, só as globais são devolvidas ao chamador; os locais da
  --      função são descartados e os locais do chamador permanecem intactos
  Call fname args ->
    case Map.lookup fname fenv of
      Nothing  -> error ("Funcao nao declarada: " ++ fname)
      Just fun -> do
        let nparams = length (funParams fun)
            nargs   = length args
        if nparams /= nargs
          then error ("Funcao " ++ fname ++ " espera " ++ show nparams ++ " argumentos")
          else do
            (vals, scArgs) <- evalArgs fenv sc args
            let params   = map snd (funParams fun)
                scCallee = Scope { globais  = globais scArgs
                                 , locais   = Map.fromList (zip params vals)
                                 , emFuncao = True }
            result <- execStmt fenv scCallee (funBody fun)
            let (v, scFim) = case result of
                               Returned val s -> (val, s)
                               Next s         -> (VVoid, s)
            return (v, scArgs { globais = globais scFim })

-- [Guilherme Augusto] Avalia a lista de argumentos da esquerda para a direita,
-- passando o escopo de cada um para o seguinte (substitui o mapM).
evalArgs :: FunEnv -> Scope -> [Expr] -> IO ([Value], Scope)
evalArgs _    sc []     = return ([], sc)
evalArgs fenv sc (a:as) = do
  (v,  sc1) <- evalExpr fenv sc  a
  (vs, sc2) <- evalArgs fenv sc1 as
  return (v : vs, sc2)

-- ============================================================
-- evalBinOp
-- ============================================================

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Add (VInt  a) (VInt  b) = VInt  (a + b)
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

-- [Guilherme Augusto] execStmt agora recebe/devolve Scope em vez de Env
execStmt :: FunEnv -> Scope -> Stmt -> IO StmtResult
execStmt fenv sc stmt = case stmt of

  Skip -> return (Next sc)

  Print e -> do
    (v, sc1) <- evalExpr fenv sc e
    putStrLn (showValue v)
    return (Next sc1)

  -- [Guilherme Augusto] Decl: cria local dentro de função (sombreia global) ou global no programa principal
  Decl _ x e -> do
    (v, sc1) <- evalExpr fenv sc e
    return (Next (declVar x v sc1))

  -- [Guilherme Augusto] Assign: atualiza o local se existir, senão o global.
  -- O escopo usado no setVar é o devolvido por evalExpr (e pode ter globais alteradas).
  Assign x e ->
    case lookupVar x sc of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just _  -> do
        (v, sc1) <- evalExpr fenv sc e
        case setVar x v sc1 of
          Nothing  -> error ("Variavel nao declarada: " ++ x)
          Just sc2 -> return (Next sc2)

  -- [Antônio Guedes] Atribuição em índice de Array
  -- [Guilherme Augusto] índice e valor avaliados em sequência; escrita via setVar
  ArrSet x idxExpr valExpr -> do
    (idxVal, sc1) <- evalExpr fenv sc  idxExpr
    (val,    sc2) <- evalExpr fenv sc1 valExpr
    case (lookupVar x sc2, idxVal) of
      (Just (VArray arr), VInt i) ->
        if i >= 0 && i < length arr
          then do
            -- Substitui o valor no índice específico da lista
            let arr' = take i arr ++ [val] ++ drop (i + 1) arr
            case setVar x (VArray arr') sc2 of
              Just sc3 -> return (Next sc3)
              Nothing  -> error ("Array nao declarado: " ++ x)
          else error ("Indice " ++ show i ++ " fora dos limites do array " ++ x)
      (Nothing, _) -> error ("Array nao declarado: " ++ x)
      _            -> error "Variavel nao eh um array ou indice invalido"

  If cond t f -> do
    (v, sc1) <- evalExpr fenv sc cond
    case v of
      VBool True  -> execStmt fenv sc1 t
      VBool False -> execStmt fenv sc1 f
      _           -> error "Condicao do If nao eh booleana"

  Return e -> do
    (v, sc1) <- evalExpr fenv sc e
    return (Returned v sc1)

  Block stmts -> execBlock fenv sc stmts

  While cond body -> do
    (v, sc1) <- evalExpr fenv sc cond
    case v of
      VBool False -> return (Next sc1)
      VBool True  -> do
        r <- execStmt fenv sc1 body
        case r of
          Returned v' sc2 -> return (Returned v' sc2)
          Next        sc2 -> execStmt fenv sc2 (While cond body)
      _ -> error "Condicao do While nao eh booleana"

  -- [Antônio Guedes] Execução do Laço For
  -- [Guilherme Augusto] adaptado para Scope; em um Return, o escopo devolvido é o do
  -- próprio Return (antes devolvia o escopo anterior ao corpo/incremento, o que
  -- perderia alterações em globais feitas até o return).
  For initStmt condExpr incStmt bodyStmt -> do
    rInit <- execStmt fenv sc initStmt
    case rInit of
      Returned v' sc' -> return (Returned v' sc')
      Next scInit     -> loop scInit
    where
      loop cur = do
        (vCond, scCond) <- evalExpr fenv cur condExpr
        case vCond of
          VBool False -> return (Next scCond)
          VBool True  -> do
            rBody <- execStmt fenv scCond bodyStmt
            case rBody of
              Returned v' sc' -> return (Returned v' sc')
              Next scBody     -> do
                rInc <- execStmt fenv scBody incStmt
                case rInc of
                  Returned v' sc' -> return (Returned v' sc')
                  Next scInc      -> loop scInc
          _ -> error "Condicao do For nao eh booleana"

-- ============================================================
-- Execução de um bloco
-- ============================================================

execBlock :: FunEnv -> Scope -> [Stmt] -> IO StmtResult
execBlock _    sc []     = return (Next sc)
execBlock fenv sc (s:ss) = do
  r <- execStmt fenv sc s
  case r of
    Returned v sc' -> return (Returned v sc')
    Next       sc' -> execBlock fenv sc' ss

-- ============================================================
-- Utilitários
-- ============================================================

showValue :: Value -> String
showValue (VInt  n)   = show n
showValue (VBool b)   = if b then "verdadeiro" else "falso"
showValue VVoid       = "()"
showValue (VArray arr)= "[" ++ unwords (map showValue arr) ++ "]" -- [Antônio Guedes] Exibe os elementos

-- [Guilherme Augusto] runProg/runProgIO começam com escopoInicial (assinaturas inalteradas)
runProg :: FunEnv -> Stmt -> IO Value
runProg fenv prog = do
  result <- execStmt fenv escopoInicial prog
  case result of
    Returned v _ -> return v
    Next _       -> return VVoid

runProgIO :: FunEnv -> Stmt -> IO String
runProgIO fenv prog = capture_ (execStmt fenv escopoInicial prog)

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

-- ------------------------------------------------------------
-- [Guilherme Augusto] Funções e programas para os testes de escopo/argumentos
-- ------------------------------------------------------------

-- int inc() { g = g + 1; return g; }          (efeito colateral na global g)
funInc :: FunDecl
funInc = FunDecl "inc" []
  (Block [ Assign "g" (BinE Add (Var "g") (Lit 1))
         , Return (Var "g") ])

-- int f(int a, int b) { return a*10 + b; }    (a ordem dos argumentos fica visível)
funF :: FunDecl
funF = FunDecl "f" [(TInt, "a"), (TInt, "b")]
  (Return (BinE Add (BinE Mul (Var "a") (Lit 10)) (Var "b")))

-- int sombra(int x) { return x; }             (parâmetro com o mesmo nome da global x)
funSombra :: FunDecl
funSombra = FunDecl "sombra" [(TInt, "x")] (Return (Var "x"))

-- int loc() { int x = 5; x = x + 1; return x; }  (local x sombreia a global x)
funLoc :: FunDecl
funLoc = FunDecl "loc" []
  (Block [ Decl TInt "x" (Lit 5)
         , Assign "x" (BinE Add (Var "x") (Lit 1))
         , Return (Var "x") ])

-- int lerG() { return g; }                    (lê global dentro da função)
funLerG :: FunDecl
funLerG = FunDecl "lerG" [] (Return (Var "g"))

-- int setG() { g = 9; return 0; }             (escreve global dentro da função)
funSetG :: FunDecl
funSetG = FunDecl "setG" []
  (Block [ Assign "g" (Lit 9), Return (Lit 0) ])

-- int criaTmp() { int tmp = 5; return tmp; }  (tmp é local e não pode vazar)
funCriaTmp :: FunDecl
funCriaTmp = FunDecl "criaTmp" []
  (Block [ Decl TInt "tmp" (Lit 5), Return (Var "tmp") ])

-- int duasVezes() { int a = inc(); int b = inc(); return g; }  (chamada aninhada)
funDuasVezes :: FunDecl
funDuasVezes = FunDecl "duasVezes" []
  (Block [ Decl TInt "a" (Call "inc" [])
         , Decl TInt "b" (Call "inc" [])
         , Return (Var "g") ])

fenvTeste :: FunEnv
fenvTeste = Map.fromList
  [ (funName f, f)
  | f <- [funInc, funF, funSombra, funLoc, funLerG, funSetG, funCriaTmp, funDuasVezes] ]

-- g = 0; return f(inc(), inc());   -> a=1, b=2 => 12
progArgsEsqDir :: Stmt
progArgsEsqDir = Block
  [ Decl TInt "g" (Lit 0)
  , Return (Call "f" [Call "inc" [], Call "inc" []]) ]

-- g = 0; return f(inc(), g);       -> a=1, b enxerga g já atualizada (1) => 11
progArgLeGlobalNova :: Stmt
progArgLeGlobalNova = Block
  [ Decl TInt "g" (Lit 0)
  , Return (Call "f" [Call "inc" [], Var "g"]) ]

-- g = 0; r = f(inc(), inc()); return g;   -> g continua 2 depois da chamada
progGlobalPersiste :: Stmt
progGlobalPersiste = Block
  [ Decl TInt "g" (Lit 0)
  , Decl TInt "r" (Call "f" [Call "inc" [], Call "inc" []])
  , Return (Var "g") ]

-- x = 1; r = sombra(2); return r*10 + x;  -> 2*10 + 1 = 21
progSombraParam :: Stmt
progSombraParam = Block
  [ Decl TInt "x" (Lit 1)
  , Decl TInt "r" (Call "sombra" [Lit 2])
  , Return (BinE Add (BinE Mul (Var "r") (Lit 10)) (Var "x")) ]

-- x = 1; r = loc(); return r*10 + x;      -> 6*10 + 1 = 61
progSombraLocal :: Stmt
progSombraLocal = Block
  [ Decl TInt "x" (Lit 1)
  , Decl TInt "r" (Call "loc" [])
  , Return (BinE Add (BinE Mul (Var "r") (Lit 10)) (Var "x")) ]

-- g = 7; return lerG();                   -> 7
progLeGlobal :: Stmt
progLeGlobal = Block
  [ Decl TInt "g" (Lit 7)
  , Return (Call "lerG" []) ]

-- g = 0; r = setG(); return g;            -> 9
progEscreveGlobal :: Stmt
progEscreveGlobal = Block
  [ Decl TInt "g" (Lit 0)
  , Decl TInt "r" (Call "setG" [])
  , Return (Var "g") ]

-- g = 0; return duasVezes();              -> 2
progGlobalAninhado :: Stmt
progGlobalAninhado = Block
  [ Decl TInt "g" (Lit 0)
  , Return (Call "duasVezes" []) ]

-- r = criaTmp(); return tmp;              -> erro: tmp era local da função
progLocalVaza :: Stmt
progLocalVaza = Block
  [ Decl TInt "r" (Call "criaTmp" [])
  , Return (Var "tmp") ]

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

-- [Guilherme Augusto] Testes de argumentos e escopo

testArgsEsqDir :: Test
testArgsEsqDir = TestCase $ do
  v <- runProg fenvTeste progArgsEsqDir
  assertEqual "f(inc(), inc()) deve avaliar esquerda->direita: 1*10+2" (VInt 12) v

testArgLeGlobalNova :: Test
testArgLeGlobalNova = TestCase $ do
  v <- runProg fenvTeste progArgLeGlobalNova
  assertEqual "2o argumento deve enxergar a global alterada pelo 1o: 1*10+1" (VInt 11) v

testGlobalPersiste :: Test
testGlobalPersiste = TestCase $ do
  v <- runProg fenvTeste progGlobalPersiste
  assertEqual "global alterada pelos argumentos continua alterada apos a chamada" (VInt 2) v

testSombraParam :: Test
testSombraParam = TestCase $ do
  v <- runProg fenvTeste progSombraParam
  assertEqual "parametro x sombreia global x (2*10) e a global fica intacta (+1)" (VInt 21) v

testSombraLocal :: Test
testSombraLocal = TestCase $ do
  v <- runProg fenvTeste progSombraLocal
  assertEqual "local x sombreia global x (6*10) e a global fica intacta (+1)" (VInt 61) v

testLeGlobal :: Test
testLeGlobal = TestCase $ do
  v <- runProg fenvTeste progLeGlobal
  assertEqual "funcao deve enxergar variavel global" (VInt 7) v

testEscreveGlobal :: Test
testEscreveGlobal = TestCase $ do
  v <- runProg fenvTeste progEscreveGlobal
  assertEqual "atribuicao a global dentro da funcao deve persistir" (VInt 9) v

testGlobalAninhado :: Test
testGlobalAninhado = TestCase $ do
  v <- runProg fenvTeste progGlobalAninhado
  assertEqual "alteracoes de globais em chamadas aninhadas devem propagar" (VInt 2) v

testLocalNaoVaza :: Test
testLocalNaoVaza = TestCase $ do
  r <- try (runProg fenvTeste progLocalVaza) :: IO (Either ErrorCall Value)
  case r of
    Left  _ -> return ()
    Right v -> assertFailure ("variavel local nao deveria existir fora da funcao, mas retornou " ++ show v)

suite :: Test
suite = TestList
  [ TestLabel "teste de manipulacao de array" testArray
  , TestLabel "teste de laco for"             testForLoop
  , TestLabel "args esquerda->direita"        testArgsEsqDir
  , TestLabel "arg enxerga global atualizada" testArgLeGlobalNova
  , TestLabel "global persiste apos chamada"  testGlobalPersiste
  , TestLabel "sombreamento por parametro"    testSombraParam
  , TestLabel "sombreamento por local"        testSombraLocal
  , TestLabel "funcao le global"              testLeGlobal
  , TestLabel "funcao escreve global"         testEscreveGlobal
  , TestLabel "globais em chamadas aninhadas" testGlobalAninhado
  , TestLabel "local nao vaza da funcao"      testLocalNaoVaza
  ]

main :: IO ()
main = runTestTTAndExit suite
