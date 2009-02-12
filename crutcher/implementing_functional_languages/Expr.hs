module Expr where

type CoreProgram = Program Name
type CoreScDefn = ScDefn Name 
type CoreExpr = Expr Name
type CoreDefn = Defn Name
type CoreAlt = Alter Name

type Name = String

type Program a = [ScDefn a] 

type ScDefn a = (Name, [a], Expr a) 

type IsRec = Bool 
recursive, nonRecursive :: IsRec 
recursive = True 
nonRecursive = False 

type Defn a = (a, Expr a)

type Alter a = (Int, [a], Expr a)

data Expr a 
  -- | Variables
  = EVar { name :: Name }
  -- | Numbers
  | ENum { val :: Int }
  -- | Constructors
  | EConstr {
     -- | the constructor id tag
     tag :: Int,
     -- | the arity of the constructor
     arity :: Int }
  -- | Applications
  | EAp { func :: Expr a, arg :: Expr a }
  -- | Let(rec) expressions 
  | ELet {
      -- | Boolean with True = recursive
      isRec :: IsRec,
      -- | Values defined in the let(rec)
      defs :: [Defn a],
      -- | Expression body of the let(rec)
      expr :: Expr a }
  -- | Case expression 
  | ECase {
      -- | Expression to scrutinise 
      expr :: Expr a,
      -- | Alternatives 
      alts :: [Alter a] }
  -- | Lambda abstractions 
  | ELam {
      -- | Arguments to the lambda
      args :: [a],
      -- | Lambda body
      expr :: Expr a }
  deriving (Show,Eq) 

isAtomicExpr :: Expr a -> Bool 
isAtomicExpr (EVar _) = True 
isAtomicExpr (ENum _) = True 
isAtomicExpr _ = False 

bindersOf :: [Defn a] -> [a] 
bindersOf = map fst

rhssOf :: [Defn a] -> [Expr a] 
rhssOf = map snd

