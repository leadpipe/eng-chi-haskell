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


-- We create a julia set by choosing a single point C on the complex
-- plane, using an iterative quadratic function: f(z) = z**2 + C 
-- We then test every value z on the complex plane, to decide if it
-- converges or diverges under iteration.
-- 
-- For example, for C = 3+4i, we would use this iterator function:
--
--   let f = (juliaFunc (ComplexNum 3 4))

juliaFunc :: ComplexNum -> ComplexNum -> ComplexNum
juliaFunc c z = complexAdd c (complexMultiply z z) 


-- iterateJulia:  main iterator logic for a Julia set.
--
--   upperbound:  magnitude at which we decide a cycle has "diverged"
--   maxiter:  number of iterations at which we decide a cycle has "converged"
--   c:  complex constant which defines the julia set
--   z:  complex value to test under iteration
--
-- Return either 0 (meaning the cycle converged), or a positive int (<=
-- maxiter) indicating how many iterations happened before the cycle
-- diverged (the "speed" of divergence). 

_iterate upperbound maxiter iter iterfunc z
     | magnitude >= upperbound  = iter  {- diverged -}
     | iter >= maxiter          = 0     {- converged -}
     | otherwise = _iterate upperbound maxiter (iter + 1) iterfunc newval
   where newval = iterfunc z
         magnitude = complexMagnitude newval
     
iterateJulia :: Double -> Integer -> ComplexNum -> ComplexNum -> Integer
iterateJulia upperbound maxiter c z = 
               let func = juliaFunc c
                in _iterate upperbound maxiter 0 func z


-- Compute a single row for Julia set C at imaginary height Y, going
-- from real values XMIN to XMAX, stepping at resolution STEP.  (Total
-- length of the row will be (xmax-xmin)/step.)  UPPERBOUND and
-- MAXITER are passed to iterateJulia.

computeRow :: ComplexNum -> Double -> Double -> Double -> Double ->
              Integer -> Double -> [Integer]
computeRow c xmin xmax step upperbound maxiter y =
     map iterfunc itervals
     where iterfunc = iterateJulia upperbound maxiter c
           itervals = map (\x -> ComplexNum x y) [xmin, (xmin + step)..xmax]


-- Parameters for a 'window' onto the complex plane, into which we'll
-- render the Julia set as a set of integers.  Define the window via
-- UPPERLEFT and LOWERRIGHT coordinates.  RESOLUTION defines how to
-- divide the window into discrete points. (e.g. if length and width
-- are 3 and resolution is 0.1, then the window will be an array of
-- size 30x30).  UPPERBOUND is the magnitude at which we decide a
-- cycle has diverged, and MAXITER is the number of iterations before
-- we decide a cycle has converged.  (Increase maxiter to increase
-- accuracy and compute-time.)

data ComplexWindow = ComplexWindow { upperleft :: ComplexNum,
                                     lowerright :: ComplexNum,
                                     resolution :: Double,
                                     upperbound :: Double, 
                                     maxiter :: Integer }
                     deriving (Show)


-- Given a window on the complex plane generate a 2-D array of
-- integers which represents the Julia Set for complex number C.

juliaWindow :: ComplexNum -> ComplexWindow -> [[Integer]]
juliaWindow c window =
     map rowgenerator rowlist
     where xmin = real (upperleft window)
           xmax = real (lowerright window)
           ymin = imaginary (lowerright window)
           ymax = imaginary (upperleft window)
           step = resolution window
           rowgenerator = 
              computeRow c xmin xmax step (upperbound window) (maxiter window)
           rowlist = [ymax, (ymax - step).. ymin]
