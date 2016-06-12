{-# LANGUAGE DeriveFunctor #-}
module Main where

-- import Control.Applicative
import Control.Applicative.Free

data User = User { userName :: String
                 , fullName :: String
                 , id :: Int 
                 } deriving Show

data Option a = Option { optName :: String
                       , optDefault :: Maybe a
                       , optReader :: String -> Maybe a
                       } deriving Functor

type OptionAp = Ap Option

option :: String -> Maybe a -> (String -> Maybe a) -> OptionAp a
option n d r = liftAp (Option n d r)



readInt :: String -> Maybe Int
readInt str  = Just (read str :: Int)

createStringListParser :: OptionAp a -> [String] -> Maybe a
createStringListParser apP strs = runAp doWork apP where
  doWork p = recurse p strs
  recurse p (h:t) | h == optName p = optReader p (head t)
  recurse p (_:t) = recurse p (tail t)
  recurse p _ = optDefault p


userP :: Ap Option User
userP = User <$> option "username" Nothing Just
             <*> option "fullname" (Just "") Just
             <*> option "id" Nothing readInt

userParser :: [String] -> Maybe User
userParser = createStringListParser userP


main :: IO ()
main = do 
  putStrLn "Hello, Haskell!"
  let strs = ["username", "rob", "fullname", "Rob", "id", "77"]
      strs2 = ["username", "rob", "id", "77"]
      strs3 = ["username", "rob", "fullname", "Rob"]
      
  print $ userParser strs  
  print $ userParser strs2  
  print $ userParser strs3