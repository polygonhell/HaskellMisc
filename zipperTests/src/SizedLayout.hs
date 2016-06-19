{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SizedLayout(SizedLayout(..), top, left, layout) where

import Control.Lens
import ILayout

data SizedLayout a = SizedLayout {_layout :: ILayout a,  size:: Int} deriving (Show)
makeLenses ''SizedLayout

topGetter:: SizedLayout a -> SizedLayout a
topGetter (SizedLayout HSplit{..} i) = SizedLayout _top (i - 1) 
topGetter _ = error "top called on None HSplit"
topSetter:: SizedLayout a -> SizedLayout a -> SizedLayout a
topSetter (SizedLayout h@HSplit{..} s) (SizedLayout l _) = SizedLayout h{_top = l}  s
topSetter _ _ = error "top called on None HSplit"
top :: Simple Traversal (SizedLayout a) (SizedLayout a)
top = lens topGetter topSetter

leftGetter:: SizedLayout a -> SizedLayout a
leftGetter (SizedLayout VSplit{..} i) = SizedLayout _left (i - 1) 
leftGetter _ = error "left called on None VSplit"
leftSetter:: SizedLayout a -> SizedLayout a -> SizedLayout a
leftSetter (SizedLayout v@VSplit{..} s) (SizedLayout l _) = SizedLayout v{_left = l}  s
leftSetter _ _ = error "left called on None VSplit"
left :: Simple Traversal (SizedLayout a) (SizedLayout a)
left = lens leftGetter leftSetter