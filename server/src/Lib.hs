{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Data.Aeson
import qualified Data.String as Data.Text
import Data.Text
import GHC.Generics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Person = Person
  { name :: Text,
    age :: Int,
    firstName :: Text,
    displayName :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

-- encode (Person {name = "John", age = 27})
