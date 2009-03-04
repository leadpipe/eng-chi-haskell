import Data.List

-- file: ch13/numsimple.hs
-- The "operators" that we're going to support
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq)

instance Show Op where
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "**"

opPrecedence :: Op -> Int
opPrecedence Plus = 1
opPrecedence Minus = 1
opPrecedence Mul = 2
opPrecedence Div = 2
opPrecedence Pow = 3

--------------------------------------------------
-- Symbolic/units manipulation
--------------------------------------------------

{- The core symbolic manipulation type.  It can be a simple number,
a symbol, a binary arithmetic operation (such as +), or a unary
arithmetic operation (such as cos)

Notice the types of BinaryOp and UnaryOp: it's a recursive
type.  So, we could represent a (+) over two SymbolicManips. -}
data SymbolicManip a = 
          Number a           -- Simple number, such as 5
        | Symbol String      -- A symbol, such as x
        | UnaryOp String (SymbolicManip a)
        | BinaryOp Op (SymbolicManip a) (SymbolicManip a)
          deriving (Eq)

{- SymbolicManip will be an instance of Num.  Define how the Num
operations are handled over a SymbolicManip.  This will implement things
like (+) for SymbolicManip. -}
instance Num a => Num (SymbolicManip a) where
    a + b = BinaryOp Plus a b
    a - b = BinaryOp Minus a b
    a * b = BinaryOp Mul a b
    negate a = BinaryOp Mul (Number (-1)) a
    abs a = UnaryOp "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

{- Make SymbolicManip an instance of Fractional -}
instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryOp Div a b
    recip a = BinaryOp Div (Number 1) a
    fromRational r = Number (fromRational r)

{- Make SymbolicManip an instance of Floating -}
instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryOp "exp" a
    log a = UnaryOp "log" a
    sqrt a = UnaryOp "sqrt" a
    a ** b = BinaryOp Pow a b
    sin a = UnaryOp "sin" a
    cos a = UnaryOp "cos" a
    tan a = UnaryOp "tan" a
    asin a = UnaryOp "asin" a
    acos a = UnaryOp "acos" a
    atan a = UnaryOp "atan" a
    sinh a = UnaryOp "sinh" a
    cosh a = UnaryOp "cosh" a
    tanh a = UnaryOp "tanh" a
    asinh a = UnaryOp "asinh" a
    acosh a = UnaryOp "acosh" a
    atanh a = UnaryOp "atanh" a

{- Show a SymbolicManip as a String, using conventional
algebraic notation -}
prettyShow :: (Show a, Num a) => SymbolicManip a -> String

-- Show a number or symbol as a bare number or serial
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (UnaryOp opstr a) = 
    opstr ++ "(" ++ show a ++ ")"

prettyShow (BinaryOp op a b) =
    let pa = prettyParen op a
        pb = prettyParen op b
        in pa ++ (show op) ++ pb

prettyParen :: (Show a, Num a) => Op -> SymbolicManip a -> String
prettyParen _ x@(Number _) = prettyShow x
prettyParen _ x@(Symbol _) = prettyShow x
prettyParen _ x@(UnaryOp _ _) = prettyShow x
prettyParen op x@(BinaryOp xop _ _)
  | opPrecedence op > opPrecedence xop = "(" ++ prettyShow x ++ ")"
  | otherwise = prettyShow x

{- Showing a SymbolicManip calls the prettyShow function on it -}
instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow

-- XXX: You're also missing rpnShow over here

{- Perform some basic algebraic simplifications on a SymbolicManip. -}
simplify :: (Num a) => SymbolicManip a -> SymbolicManip a
simplify (UnaryOp op a) = UnaryOp op (simplify a)
simplify (BinaryOp op x y) = case (op, simplify x, simplify y) of 
    (Mul, Number 1, y) -> y
    (Mul, x, Number 1) -> x
    (Mul, Number 0, y) -> Number 0
    (Mul, x, Number 0) -> Number 0
    (Div, x, Number 1) -> x
    (Plus, x, Number 0) -> x
    (Plus, Number 0, y) -> y
    (Minus, x, Number 0) -> x
    (Pow, x, Number 0) -> Number 1
    (_, x, y) -> BinaryOp op x y
simplify x = x
-- 

{- New data type: Units.  A Units type contains a number
and a SymbolicManip, which represents the units of measure.
A simple label would be something like (Symbol "m") -}
data Num a => Units a = Units a (SymbolicManip a)
           deriving (Eq)

{- Implement Units for Num.  We don't know how to convert between
arbitrary units, so we generate an error if we try to add numbers with
different units.  For multiplication, generate the appropriate
new units. -}
instance (Num a) => Num (Units a) where
    (Units xa ua) + (Units xb ub) 
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add or subtract"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

{- Make Units an instance of Fractional -}
instance (Fractional a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

instance (Floating a) => Floating (Units a) where
    pi = (Units pi (Number 1))
    exp _ = error "exp not yet implemented in Units"
    log _ = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub) 
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua) 
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua) 
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua) 
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for atan must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"

{- A simple function that takes a number and a String and returns an
appropriate Units type to represent the number and its unit of measure -}
units :: (Num z) => z -> String -> Units z
units a b = Units a (Symbol b)

{- Extract the number only out of a Units type -}
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x
                                                    
{- Utilities for the Unit implementation -}
deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

-- This is necessary to make Units a Num, as Num is a subclass of Show
{- Showing units: we show the numeric component, an underscore,
then the prettyShow version of the simplified units -}
instance (Show a, Num a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

main = putStrLn $ show $ simplify xyzzy
  where xyzzy = (2 ** 0) + 7 - 9 * 2 + 3 * (2 + 4) / 7 :: (SymbolicManip Float)

