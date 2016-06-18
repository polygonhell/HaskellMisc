{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
--{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Lens
import Control.Applicative.Free
import Control.Monad.State
import Prelude


data Algebra a
  = CursorLeft a
  | CursorRight a
  | CursorUp a
  | CursorDown a 
  | SplitHoriz a
  deriving (Show)

type DslApp a = Ap Algebra a

cursorLeft :: DslApp ()
cursorLeft = liftAp (CursorLeft ())
cursorRight :: DslApp ()
cursorRight = liftAp (CursorRight ())
cursorUp :: DslApp ()
cursorUp = liftAp (CursorUp ())
cursorDown :: DslApp ()
cursorDown = liftAp (CursorDown ())
splitHoriz :: DslApp ()
splitHoriz = liftAp (SplitHoriz ())


data ILayout a = Leaf {_v :: a}
            | HSplit {_row:: Int, _top, _bottom :: ILayout a }
            | VSplit {_col:: Int, _left, _right :: ILayout a }
            deriving (Show)
makeLenses ''ILayout     
-- makePrisms ''ILayout       


data Focus f a = Focus {_cursor:: LensType f a , parent:: Maybe (Focus f a)}  
instance Show (Focus f a) where
  show _ = "Focus "
-- cursor :: Simple Lens (Focus f a) (LensType f a)
-- cursor = lens (_cursor) (\f a -> f{_cursor = a})

data Layout f a = Layout {_poo:: Int, _layout:: ILayout a, _focus:: Focus f a} deriving (Show)


type LensType f a = (Applicative f) =>
                 (ILayout a -> f (ILayout a))
                 -> Layout f a -> f (Layout f a)
-- Simple Lens (Layout a) (ILayout a)
makeLenses ''Layout            





testLayout :: ILayout String
testLayout = HSplit 10 (VSplit 10 (Leaf "1") (Leaf "2")) (Leaf "3")

testf :: Focus f String
testf = Focus (layout . top) (Just (Focus layout Nothing)) 

test :: Layout f String
test = Layout 14 testLayout (Focus (layout.top.left) (Just testf)) 

-- testApProg :: DslApp ()
-- testApProg = do
--   _ <- cursorRight
--   a <- cursorDown
--   return a

testApProg2 :: DslApp ()
testApProg2 = cursorRight *> cursorDown *> splitHoriz

printDsl :: DslApp a -> IO a
printDsl (Pure a) = return a
printDsl (Ap (CursorLeft a) c) = do
  putStrLn "CursorLeft"
  printDsl (c <*> pure a)
printDsl (Ap (CursorRight a) c) = do
  putStrLn "CursorRight"
  printDsl (c <*> pure a)
printDsl (Ap (CursorUp a) c) = do
  putStrLn "CursorUp"
  printDsl (c <*> pure a)
printDsl (Ap (CursorDown a) c) = do
  putStrLn "CursorDown"
  printDsl (c <*> pure a)
printDsl (Ap (SplitHoriz a) c) = do
  putStrLn "SplitHoriz"
  printDsl (c <*> pure a)

type LayoutState a = State (Layout Identity String) a


evalDsl :: DslApp a -> LayoutState a
evalDsl (Pure a) = return a
evalDsl (Ap (CursorLeft a) c) = do 
  poo .= 5
  evalDsl (c <*> pure a)
evalDsl (Ap (CursorRight a) c) = do 
  poo .= 5
  evalDsl (c <*> pure a)
evalDsl (Ap (CursorUp a) c) = do 
  poo += 2
  evalDsl (c <*> pure a)
evalDsl (Ap (CursorDown a) c) = do 
  poo += 2
  evalDsl (c <*> pure a)
evalDsl (Ap (SplitHoriz a) c) = do 
  poo += 2
  aa <- get
  -- let poo = aa & _cursor (aa ^. focus) .~ Leaf "Poo"
  put $ aa & _cursor (aa ^. focus) .~ Leaf "More Poo"

  -- layout .= Leaf "HH"
  -- (_cursor aa) .= Leaf "jjjjj"
  -- poo .= 4

  -- focus.
  -- let l = view focus :: Focus f String
  -- focus .= Leaf "Hello Poo"
  evalDsl (c <*> pure a)



main :: IO ()
main = do
  print test
  -- What is the focus' lens is looking at
  print $ test ^?  _cursor (test ^. focus) 
  -- let l1 = layout . lens (test ^. focus)
  putStrLn "-----"
  print $ set (_cursor (test ^. focus)) (Leaf "Hello") test  
  print $ test & _cursor (test ^. focus) .~ Leaf "Poo"

