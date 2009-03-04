module Template where

import Control.Monad.Error
import Data.Either
import Data.Foldable
import Data.List

import Expr
import Heap

tiTest :: CoreProgram -> IO ()
tiTest defs = do
  let res = last $ tiRunProgram defs
  case res of
    (Left e) -> putStrLn ("Error: " ++ e)
    (Right s) -> putStrLn ("Result: " ++ show s)

data TiNode
  -- | A number
  = NNum Int
  -- | Application
  | NAp HeapAddr HeapAddr
  -- | Supercombinator
  | NSupercomb Name [Name] CoreExpr
  deriving(Show)

isDataNode :: TiNode -> Bool 
isDataNode (NNum _) = True 
isDataNode _ = False 

data TiState = TiState {
  tiStack :: [HeapAddr],
  tiDump :: [[HeapAddr]],
  tiGlobals :: [(Name, HeapAddr)],
  tiHeap :: TiHeap }

instance Show TiState where
  show s = "Stack: " ++ show stack
    where stack = rights $ map (hLookup (tiHeap s) :: HeapAddr -> Either String TiNode) (tiStack s)

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

tiRunProgram :: CoreProgram -> [Either TiError TiState]
tiRunProgram = tiRun . tiCompile

tiRun :: Either TiError TiState -> [Either TiError TiState]
tiRun x = x : maybe [] tiRun (tiNext x)

tiNext :: Either TiError TiState -> Maybe (Either TiError TiState)
tiNext x@(Left _) = Just x
tiNext (Right s) = case s of
  (TiState{tiStack=[]}) -> Just $ fail "Empty stack!"
  (TiState{tiStack=ks@(k:ks'), tiHeap=h}) -> case hLookup h k of
    (Left e) -> Just $ fail e
    (Right x) -> case x of
      (NNum _) -> if null ks'
                    then Nothing -- final state
          	    else Just $ fail "Number applied as a function!"
      (NAp a b) -> Just $ return $ s { tiStack=a:ks }
      (NSupercomb n as b) -> Just $ fail "NSupercomb Not implemented!"

