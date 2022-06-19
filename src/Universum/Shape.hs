{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Universum.Shape(_Circle, _Rhombus, _Star, Shape(..)) where

import Data.Aeson
  ( FromJSON(..)
  , Value(String)
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (prependFailure, typeMismatch)
import Control.Lens(makePrisms)

data Shape
  = Rhombus
  | Star
  | Circle
  deriving (Show, Eq)

makePrisms ''Shape

instance FromJSON Shape where
  parseJSON (String "STAR") = pure Star
  parseJSON (String "RHOMBUS") = pure Rhombus
  parseJSON (String "CIRCLE") = pure Circle
  parseJSON (String x) =
    fail ("parsing shape failed, invalid string \"" <> show x <> "\": expected STAR, RHOMBUS or CIRCLE")
  parseJSON invalid =
    prependFailure "parsing shape failed, " (typeMismatch "String" invalid)
