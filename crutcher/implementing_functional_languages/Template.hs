module Template where

import Control.Monad.Error
import Data.Foldable
import Data.List

import Expr
import Heap

data TiNode
  -- | Application
  = NAp HeapAddr HeapAddr
  -- | Supercombinator
  | NSupercomb Name [Name] CoreExpr
  -- | A number
  | NNum Int
  deriving(Show)

data TiState = TiState {
  tiStack :: [HeapAddr],
  tiDump :: [[HeapAddr]],
  tiGlobals :: [(Name, HeapAddr)],
  tiHeap :: TiHeap }

instance Show TiState where
  show s = "Stack: " ++ show (tiStack s)

type TiError = String
type TiHeap = MHeap TiNode

tiInitial :: TiState
tiInitial = TiState [] [] [] hEmpty

tiCompile :: CoreProgram -> Either TiError TiState
tiCompile defs = do
  s <- foldrM tiAddSuperCombinator tiInitial defs
  tiBoot "main" s

tiAddSuperCombinator :: CoreScDefn -> TiState -> Either TiError TiState
tiAddSuperCombinator (n, as, b) s = do
  case lookup n (tiGlobals s) of
    Nothing -> return ()
    (Just _) -> fail ("Name conflict for super-combinator: " ++ n)
  let (h, k) = hAlloc (tiHeap s) (NSupercomb n as b)
  return $ s { tiGlobals = (n,k) : tiGlobals s, tiHeap = h }

-- | Reset the stack to contain only the entry point
tiBoot :: String -> TiState -> Either TiError TiState
tiBoot n s = do
  case lookup n (tiGlobals s) of
    Nothing -> fail ("Failed to locate entry point: " ++ n)
    (Just x) -> return $ s { tiStack = [x] }

tiRun :: Either TiError TiState -> [Either TiError TiState]
tiRun x = case x of
  (Left _) -> [x]
  (Right s) -> x : if tiFinal s then [] else tiRun (tiStep s)

tiRunProgram :: CoreProgram -> [Either TiError TiState]
tiRunProgram = tiRun . tiCompile

tiTest :: CoreProgram -> IO ()
tiTest defs = do
  let res = last $ tiRunProgram defs
  case res of
    (Left e) -> putStrLn ("Error: " ++ e)
    (Right s) -> putStrLn ("Result: " ++ show s)

tiFinal :: TiState -> Bool
tiFinal (s { tiStack = [k] }) = True
hLookup (tiHeap s) k
tiFinal _ = False

tiStep :: TiState -> Either TiError TiState
tiStep = undefined

