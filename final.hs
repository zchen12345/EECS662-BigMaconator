{-# LANGUAGE GADTs, FlexibleContexts #-}
--The language hsa nothing to do with Big Mac from Mcdonald, we named it because that our
--PDA final porject machine is what we try to minmic.

import Control.Monad

data BigMacLang where
    Bnum :: BigMacLang
    Bbool :: BigMacLang
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
    Bind :: String -> BigMacLang -> BigMacExpr -> BigMacExpr -> BigMacExpr
    deriving (Show, Eq)

data BigMacVal where
    NumMac :: Int -> BigMacVal
    BoolMac :: Bool -> BigMacVal
    ClosureMac :: String -> BigMacExpr -> McdonaldEnv -> BigMacVal
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

