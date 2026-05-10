{-# LANGUAGE GADTs, FlexibleContexts #-}
--The language hsa nothing to do with Big Mac from Mcdonald, we named it because that our
--PDA final porject machine is what we try to minmic.

import Control.Monad

data BigMacLang where
    Bnum :: BigMacLang
    Bbool :: BigMacLang
    --adding required types for a vector
    Bvec :: BigMacLang
    (:->:) :: BigMacLang -> BigMacLang -> BigMacLang
    deriving (Show, Eq)

data BigMacExpr where
    Num :: Int -> BigMacExpr
    Bool :: Bool -> BigMacExpr
    Id :: String -> BigMacExpr
    Plus :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Minus :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Div :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Exp :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Lambda :: String -> BigMacLang -> BigMacExpr -> BigMacExpr
    App :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Mult :: BigMacExpr -> BigMacExpr -> BigMacExpr
    And :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Or :: BigMacExpr -> BigMacExpr -> BigMacExpr
    Leq :: BigMacExpr -> BigMacExpr -> BigMacExpr
    IsZero :: BigMacExpr -> BigMacExpr
    If :: BigMacExpr -> BigMacExpr -> BigMacExpr -> BigMacExpr
    Fix :: BigMacExpr -> BigMacExpr
    Between :: BigMacExpr -> BigMacExpr -> BigMacExpr -> BigMacExpr
    --Adding required operations for a vector
    EmptyVec :: BigMacExpr
    VecCons :: BigMacExpr -> BigMacExpr -> BigMacExpr
    VecHead :: BigMacExpr -> BigMacExpr
    VecTail :: BigMacExpr -> BigMacExpr
    VecDot :: BigMacExpr -> BigMacExpr -> BigMacExpr
    IsEmpty :: BigMacExpr -> BigMacExpr
    deriving (Show, Eq)

data BigMacExtend where
  NumX :: Int -> BigMacExtend
  BoolX :: Bool -> BigMacExtend
  IdX :: String -> BigMacExtend
  PlusX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  MinusX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  DivX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  ExpX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  LambdaX :: String -> BigMacLang -> BigMacExtend -> BigMacExtend
  AppX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  MultX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  AndX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  OrX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  LeqX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  IsZeroX :: BigMacExtend -> BigMacExtend
  IfX :: BigMacExtend -> BigMacExtend -> BigMacExtend -> BigMacExtend
  FixX :: BigMacExtend -> BigMacExtend
  BindX :: String -> BigMacLang -> BigMacExtend -> BigMacExtend -> BigMacExtend
  BetweenX :: BigMacExtend -> BigMacExtend -> BigMacExtend -> BigMacExtend
  --Adding required extended operations for a vector
  VecConsX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  VecHeadX :: BigMacExtend -> BigMacExtend
  VecTailX :: BigMacExtend -> BigMacExtend
  VecDotX :: BigMacExtend -> BigMacExtend -> BigMacExtend
  IsEmptyX :: BigMacExtend -> BigMacExtend
  deriving (Show, Eq)

data BigMacVal where
    NumMac :: Int -> BigMacVal
    BoolMac :: Bool -> BigMacVal
    ClosureMac :: String -> BigMacExpr -> McdonaldEnv -> BigMacVal
    --Adding required values for a vector
    VecMac :: [BigMacVal] -> BigMacVal
    deriving (Show, Eq)

type McdonaldEnv = [(String, BigMacVal)]
type McdonaldCont = [(String, BigMacLang)]

data Reader e a = Reader (e -> Maybe a)

ask :: Reader a a 
ask = Reader $ \e -> Just e

runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e 

local :: (e -> t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> runR r (f e)

useClosure :: String -> BigMacVal -> McdonaldEnv -> McdonaldEnv -> McdonaldEnv
useClosure i v e _ = (i,v):e

instance Monad (Reader e) where
    g >>= f = Reader $ \e -> 
        case runR g e of
      Nothing -> Nothing
      Just v  -> runR (f v) e

instance Functor (Reader e) where
    fmap f (Reader g) = Reader $ \e ->
        case g e of
        Nothing -> Nothing
        Just v  -> Just (f v)

instance Applicative (Reader e) where
    pure x = Reader $ \e -> Just x
    (Reader f) <*> (Reader g) = Reader $ \e ->
        case f e of
      Nothing -> Nothing
      Just h  ->
        case g e of
          Nothing -> Nothing
          Just x  -> Just (h x)

instance MonadFail (Reader e) where
  fail _ = Reader $ \_ -> Nothing



--Type Inference
typeof :: BigMacExpr -> Reader McdonaldCont BigMacLang
typeof (Num _) = return Bnum
typeof (Bool _) = return Bbool

typeof (Id x) = do
  env <- ask
  case lookup x env of
    Just t  -> return t
    Nothing -> fail "Unbound variable"

typeof (Plus a b) = numOp a b
typeof (Minus a b) = numOp a b
typeof (Mult a b) = numOp a b
typeof (Div a b) = numOp a b
typeof (Exp a b) = numOp a b

typeof (Between a b c) = do
  t1 <- typeof a; t2 <- typeof b; t3 <- typeof c
  if t1 == Bnum && t2 == Bnum && t3 == Bnum
    then return Bbool
    else fail "Type error in between"

typeof (If c t e) = do
  tc <- typeof c
  tt <- typeof t
  te <- typeof e
  if tc == Bbool && tt == te then return tt else fail "Type error in if"

typeof (And a b) = boolOp a b
typeof (Or a b) = boolOp a b

typeof (Leq a b) = do
  t1 <- typeof a; t2 <- typeof b
  if t1 == Bnum && t2 == Bnum then return Bbool else fail "Type error"

typeof (IsZero e) = do
  t <- typeof e
  if t == Bnum then return Bbool else fail "Type error"

typeof (Lambda x ty body) = do
  tBody <- local ((x,ty):) (typeof body)
  return (ty :->: tBody)

typeof (App f a) = do
  tf <- typeof f
  ta <- typeof a
  case tf of
    (t1 :->: t2) -> if t1 == ta then return t2 else fail "Type mismatch"
    _ -> fail "Not a function"

typeof (Fix f) = do
  tf <- typeof f
  case tf of
    (t1 :->: t2) -> if t1 == t2 then return t1 else fail "Fix type error"
    _ -> fail "Fix expects function"

--type checking for vector operations, should be Bvec for both operands and return Bnum for dot product
typeof (VecDot a b) = do
  ta <- typeof a
  tb <- typeof b
  case (ta, tb) of
    (Bvec, Bvec) -> return Bnum
    _-> fail "Type error in VecDot: expected two Vec"

typeof (EmptyVec) = return Bvec

typeof (VecCons a b) = do
  ta <- typeof a
  tb <- typeof b
  if tb == Bvec then return Bvec else fail "Type error in VecCons: expected Vec"
typeof (VecHead v) = do
  tv <- typeof v
  if tv == Bvec then return Bnum else fail "Type error in VecHead: expected Vec"
typeof (VecTail v) = do
  tv <- typeof v
  if tv == Bvec then return Bvec else fail "Type error in VecTail: expected Vec"
typeof (IsEmpty e) = do
  t <- typeof e
  case t of
    Bvec -> return Bbool
    _    -> fail "Type error in IsEmpty: expected Vec"

numOp :: BigMacExpr -> BigMacExpr -> Reader McdonaldCont BigMacLang
numOp e1 e2 = do
  t1 <- typeof e1
  t2 <- typeof e2
  if t1 == Bnum && t2 == Bnum then return Bnum else fail "Numeric error"

boolOp :: BigMacExpr -> BigMacExpr -> Reader McdonaldCont BigMacLang
boolOp e1 e2 = do
  t1 <- typeof e1
  t2 <- typeof e2
  if t1 == Bbool && t2 == Bbool then return Bbool else fail "Boolean error"

eval :: BigMacExpr -> Reader McdonaldEnv BigMacVal
eval (Num n) = return (NumMac n)


--Evaluation for vector operations
eval EmptyVec = return (VecMac [])

eval (VecCons x xs) = do
  v  <- eval x
  vs <- eval xs
  case vs of
    VecMac rest -> return (VecMac (v : rest))
    _           -> fail "VecCons: tail must be a vector"

eval (VecHead e) = do
  v <- eval e
  case v of
    VecMac (x:_) -> return x
    VecMac []    -> fail "VecHead: empty vector"
    _            -> fail "VecHead: not a vector"

eval (VecTail e) = do
  v <- eval e
  case v of
    VecMac (_:xs) -> return (VecMac xs)
    VecMac []     -> fail "VecTail: empty vector"
    _             -> fail "VecTail: not a vector"

eval (IsEmpty e) = do
  v <- eval e
  case v of
    VecMac [] -> return (BoolMac True)
    VecMac _  -> return (BoolMac False)
    _         -> fail "IsEmpty: not a vector"

--implemetation for dot product using fixpoint to recursively compute the sum of products of corresponding elements
eval (VecDot a b) = do
  va <- eval a
  vb <- eval b
  case (va, vb) of
    (VecMac xs, VecMac ys) ->
      if length xs == length ys
        then do
          let dotHelper = Fix (Lambda "self" (Bvec :->: (Bvec :->: Bnum))
                            (Lambda "v1" Bvec
                              (Lambda "v2" Bvec
                                (If (IsEmpty (Id "v1"))
                                  (Num 0)
                                  (Plus
                                    (Mult (VecHead (Id "v1")) (VecHead (Id "v2")))
                                    (App (App (Id "self") (VecTail (Id "v1"))) (VecTail (Id "v2")))
                                  )
                                )
                              )
                            ))
          eval (App (App dotHelper a) b)
        else fail "VecDot: vectors must be same length"
    _ -> fail "VecDot: expected two vectors"

--Elaboration
macterm :: BigMacExtend -> BigMacExpr

macterm (NumX n) = Num n
macterm (BoolX b) = Bool b
macterm (IdX x) = Id x
macterm (PlusX a b) = Plus (macterm a) (macterm b)
macterm (MinusX a b) = Minus (macterm a) (macterm b)
macterm (MultX a b) = Mult (macterm a) (macterm b)
macterm (DivX a b) = Div (macterm a) (macterm b)
macterm (ExpX a b) = Exp (macterm a) (macterm b)
macterm (BetweenX a b c) = Between (macterm a) (macterm b) (macterm c)
macterm (IfX c t e) = If (macterm c) (macterm t) (macterm e)
macterm (AndX a b) = And (macterm a) (macterm b)
macterm (OrX a b) = Or (macterm a) (macterm b)
macterm (LeqX a b) = Leq (macterm a) (macterm b)
macterm (IsZeroX e) = IsZero (macterm e)
macterm (LambdaX x ty b) = Lambda x ty (macterm b)
macterm (AppX f a) = App (macterm f) (macterm a)
macterm (BindX x ty e1 e2) =
  App (Lambda x ty (macterm e2)) (macterm e1)
macterm (FixX e) = Fix (macterm e)
--Elaboration for vector operations
macterm (VecDotX a b) = VecDot (macterm a) (macterm b)
macterm (VecConsX a b) = VecCons (macterm a) (macterm b)
macterm (VecHeadX v) = VecHead (macterm v)
macterm (VecTailX v) = VecTail (macterm v)
macterm (IsEmptyX e) = IsEmpty (macterm e)

--Test cases for vector operations

-- Vector [1,2,3]
vec123 :: BigMacExpr
vec123 =
  VecCons (Num 1)
    (VecCons (Num 2)
      (VecCons (Num 3)
        EmptyVec))

-- Vector [4,5,6]
vec456 :: BigMacExpr
vec456 =
  VecCons (Num 4)
    (VecCons (Num 5)
      (VecCons (Num 6)
        EmptyVec))

-- Vector [10,20]
vec1020 :: BigMacExpr
vec1020 =
  VecCons (Num 10)
    (VecCons (Num 20)
      EmptyVec)

-- Vector [7,8]
vec78 :: BigMacExpr
vec78 =
  VecCons (Num 7)
    (VecCons (Num 8)
      EmptyVec)

-- Empty vector
vecEmpty :: BigMacExpr
vecEmpty = EmptyVec


-- Test 1: [1,2,3] · [4,5,6]
-- 1*4 + 2*5 + 3*6 = 32
-- Expected: Just (NumMac 32)
testDot1 =
  runR (eval (VecDot vec123 vec456)) []

-- Test 2: [10,20] · [7,8]
-- 10*7 + 20*8 = 230
-- Expected: Just (NumMac 230)
testDot2 =
  runR (eval (VecDot vec1020 vec78)) []

-- Test 3: [] · []
-- Expected: Just (NumMac 0)
testDot3 =
  runR (eval (VecDot vecEmpty vecEmpty)) []

-- Test 4: Length mismatch
-- [1,2,3] · [7,8]
-- Expected: Nothing
testDot4 =
  runR (eval (VecDot vec123 vec78)) []

-- Test 5: Type checking
-- Expected: Just Bnum
testDotType =
  runR (typeof (VecDot vec123 vec456)) []

-- Test 6: Type error (second argument is not a vector)
-- Expected: Nothing
testDotTypeError =
  runR (typeof (VecDot vec123 (Num 5))) []