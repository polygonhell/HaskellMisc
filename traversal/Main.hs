{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}



module Main where

import Control.Lens hiding (Empty, children, lens)

data LH a = Leaf { _l :: a }
          | Node { _l :: a , _c1, _c2 :: LH a} deriving (Show, Functor, Foldable, Traversable)
makeLenses ''LH

data Layout a = Empty | HSplit { row :: Int } | VSplit { col :: Int } | Value { _v :: a } deriving Show
makeLenses ''Layout


type Buffer = String
data Focus = Focus {_lens::ALens' State (LH (Layout Buffer)), _parent :: Maybe Focus} 

data State = State { _focus :: Focus, _tree :: LH (Layout Buffer)}
makeLenses ''Focus
makeLenses ''State

emptyState :: State
emptyState = State (Focus tree Nothing)  (Leaf (Value "Hello"))

testState :: State
testState = splitV 4 $ splitH 5 emptyState


split :: (Int -> Layout Buffer) -> Int -> State -> State
split c r st = st''' where
  f = st^.focus.lens
  currNode = st ^# f
  st' = st & f #~ Node (c r) currNode (Leaf Empty)
  st'' = st' & focus.lens .~ (f . unsafeSingular c1)
  st''' = st'' & focus.parent .~ Just (st ^.focus)

splitH :: Int -> State -> State
splitH = split HSplit
splitV :: Int -> State -> State
splitV = split VSplit

printStuff :: State -> IO (LH ())
printStuff st = do
  let t = st ^.tree
  mapM print t

main :: IO ()
main = do 
  let f = testState ^. focus.lens
  print $ testState ^.  tree
  print $ testState ^#  f
  print $ (testState & cloneLens f . l . unsafeSingular v .~  "Poop")  ^. tree
