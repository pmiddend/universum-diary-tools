{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Universum.Document (Document, documentVersion, documentMarks) where

import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (..),
    Value (String),
    eitherDecode,
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Universum.Mark (Mark)

data Document = Document
  { _documentVersion :: Text,
    _documentMarks :: [Mark]
  }

makeLenses ''Document

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v -> Document <$> (v .: "version") <*> (v .: "marks")
