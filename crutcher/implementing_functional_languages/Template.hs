module Template where

import Data.List

import Expr
import Utils

eval state = state : rest_states 
  where 
  rest_states | isFinal state = [] 
              | otherwise     = eval next_state 
  next_state = doAdmin (step state) 
  doAdmin state = applyToLog tiLogIncSteps state 


step :: TiState -> TiState
step s = dispatch (hLookup (head $ stack s) (heap s))
  where 
  dispatch (NNum n) = numStep s n 
  dispatch (NAp a1 a2) = apStep s a1 a2 
  dispatch (NSupercomb sc args body) = scStep s sc args body 

numStep :: TiState -> Int -> TiState
numStep = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep s a1 a2 = s { stack = a1 : (stack s) }

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState 
scStep s sc_name arg_names body = s { stack = new_stack, heap = new_heap }
  where 
  new_stack = result_addr : (drop (length arg_names+1) $ stack s) 
  (new_heap, result_addr) = instantiate body (heap s) env 
  env = arg_bindings ++ (globals s)
  arg_bindings = zip arg_names (getargs (heap s) (stack s)) 

getargs :: TiHeap -> [Addr] -> [Addr]
getargs heap (sc:stack) = map get_arg stack 
  where
  get_arg addr = arg
    where
    (NAp fun arg) = hLookup addr heap 

instantiate :: CoreExpr -> TiHeap -> [(Name,Addr)] -> (TiHeap, Addr)
instantiate (ENum n) h env = hAlloc (NNum n) h
instantiate (EAp e1 e2) h env = hAlloc (NAp a1 a2) h2
  where (h1, a1) = instantiate e1 h env
	(h2, a2) = instantiate e2 h1 env 
instantiate (EVar v) h env = (h, aLookup v (error ("Undefined name " ++ show v)) env) 
instantiate (EConstr tag arity) h env = instantiateConstr tag arity h env 
instantiate (ELet isrec defs body) h env = instantiateLet isrec defs body h env 
instantiate (ECase e alts) h env = error "Can't instantiate case exprs" 

instantiateConstr tag arity h env = error "Can't instantiate constructors yet" 
instantiateLet isrec defs body h env = error "Can't instantiate let(rec)s yet" 



data TiState = TiState {
  stack :: [Addr],
  dump :: [[Addr]],
  heap :: TiHeap,
  globals :: [(Name,Addr)],
  stats :: TiLog }

isFinal :: TiState -> Bool 
isFinal (TiState{stack=[]}) = error "Empty stack!" 
isFinal (TiState {stack=[sole_addr], heap=heap}) = isDataNode (hLookup sole_addr heap)
isFinal _ = False -- Stack contains more than one item 


type TiHeap = PJLHeap TiNode

data TiLog = TiLog {
  steps :: !Int }
  deriving(Show)

tiLogInitial :: TiLog 
tiLogInitial = TiLog { steps = 0 }

tiLogIncSteps :: TiLog -> TiLog 
tiLogIncSteps stats = stats { steps = (steps stats) + 1 }

applyToLog :: (TiLog -> TiLog) -> TiState -> TiState
applyToLog f s = s { stats = f (stats s) }


data TiNode
  -- | Application
  = NAp Addr Addr
  -- | Supercombinator
  | NSupercomb Name [Name] CoreExpr
  -- | A number
  | NNum Int
  deriving(Show)

isDataNode :: TiNode -> Bool 
isDataNode (NNum _) = True 
isDataNode _ = False 


compileTiState :: CoreProgram -> TiState
compileTiState sc_defs = TiState {
    stack = stack,
    dump = dump,
    heap = heap,
    globals = globals,
    stats = tiLogInitial }
  where
  stack = [main]
  dump = []
  (heap, globals) = compileTiHeap sc_defs
  main = aLookup "main" (error "main is not defined") globals

compileTiHeap :: CoreProgram -> (TiHeap, [(Name, Addr)])
compileTiHeap = mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
  (heap', addr) = hAlloc (NSupercomb name args body) heap



