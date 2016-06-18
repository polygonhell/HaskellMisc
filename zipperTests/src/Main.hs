{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE  ImpredicativeTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Lens
import Control.Applicative.Free
import Control.Monad.State
import Prelude

-- Lens with an applicative constraint - Required for the tree
type AppLens s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t

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

type LensType a = Simple AppLens (Layout a) (ILayout a)

data Focus a = Focus {_cursor:: LensType a , parent:: Maybe (Focus a)}  
instance Show (Focus a) where
  show _ = "Focus "
-- I cannot makeLens on Focus
cursor :: Simple Lens (Focus a) (LensType a)
cursor = lens _cursor setter where 
  setter :: Focus a -> LensType a -> Focus a
  setter f a = f{_cursor = a}

data Layout a = Layout {_poo:: Int, _layout:: ILayout a, _focus:: Focus a} deriving (Show)

-- Simple Lens (Layout a) (ILayout a)
makeLenses ''Layout           
-- makeLenses ''Focus 





testLayout :: ILayout String
testLayout = HSplit 10 (VSplit 10 (Leaf "1") (Leaf "2")) (Leaf "3")

testf :: Focus String
testf = Focus (layout . top) (Just (Focus layout Nothing)) 

test :: Layout String
test = Layout 14 testLayout (Focus (layout.top.left) (Just testf)) 

-- testApProg :: DslApp ()
-- testApProg = do
--   _ <- cursorRight
--   a <- cursorDown
--   return a

testApProg :: DslApp ()
testApProg = cursorRight 
          *> cursorDown 
          *> splitHoriz

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

type LayoutState a = State (Layout String) a


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
  l <- use (focus.cursor)
  l .= Leaf "a Poop"

  evalDsl (c <*> pure a)



main :: IO ()
main = do
  print test
  -- What is the focus' lens is looking at
  print $ test ^? (test ^. focus.cursor) 
  print $ test ^?  _cursor (test ^. focus) 
  -- let l1 = layout . lens (test ^. focus)
  putStrLn "-----"
  print $ set (_cursor (test ^. focus)) (Leaf "Hello") test  
  print $ test & (test^.focus.cursor) .~ Leaf "Poo"
  putStrLn "-----"
  print $ runState (evalDsl testApProg) test
