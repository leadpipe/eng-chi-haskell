-- Ben screwing around with complex numbers, do to fractall-y things.

-- I'm sure that (a) there's a more powerful template-like system in
-- Haskell to do this, complete with operator overloading or
-- something, and (b) this is already a standard library.  But it's a
-- good learning exercise!

data ComplexNum = ComplexNum { real :: Double,
                               imaginary :: Double }
                  deriving (Show)

complexMagnitude :: ComplexNum -> Double
complexMagnitude num = sqrt ((real num)**2 + (imaginary num)**2)

complexEqual :: ComplexNum -> ComplexNum -> Bool
complexEqual a b = ((real a) == (real b)) && ((imaginary a) == (imaginary b))

complexAdd :: ComplexNum -> ComplexNum -> ComplexNum
complexAdd a b = ComplexNum (real a + real b) (imaginary a + imaginary b)

complexSubtract :: ComplexNum -> ComplexNum -> ComplexNum
complexSubtract a b = ComplexNum (real a - real b) (imaginary a - imaginary b)

complexMultiply :: ComplexNum -> ComplexNum -> ComplexNum
complexMultiply a b = ComplexNum 
                        ((real a * real b) - (imaginary a * imaginary b))
                        ((imaginary a * real b ) + (real a * imaginary b ))


-- We can create a specific julia set by choosing a single point C on
-- the complex plane, and then generate a one-time quadratic function
-- which we can use for iteration:
-- 
--   let f = (juliaFunc (ComplexNum 3 4))
--
-- The quadratic function for a Julia Set is: f(z) = z**2 + C

juliaFunc :: ComplexNum -> ComplexNum -> ComplexNum
juliaFunc c z = complexAdd c (complexMultiply z z) 


-- To iterate a julia func, we use these params:
--
--     upperbound:  magnitude at which we decide a cycle has "diverged"
--     maxiter:  number of iterations before we decide a cycle has "converged"
--
-- We return either 0 (meaning the cycle converged), or a positive int (<=
-- maxiter) indicating how many iterations happened before the cycle
-- diverged (the "speed" of divergence).  To increase the detail of
-- the set's boundary (and compute time), we simply increase maxiter.

_iterate upperbound maxiter iter iterfunc z
     | magnitude >= upperbound  = iter  {- diverged -}
     | iter >= maxiter          = 0     {- converged -}
     | otherwise = _iterate upperbound maxiter (iter + 1) iterfunc newval
   where newval = iterfunc z
         magnitude = complexMagnitude newval
     

doIteration :: Double -> Integer -> ComplexNum -> ComplexNum -> Integer
doIteration upperbound maxiter c z = 
                 _iterate upperbound maxiter 0 func z
                 where func = (juliaFunc c)

