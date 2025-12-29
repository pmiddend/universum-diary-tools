{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Universum.Mark
  ( Mark (..),
    markChanged,
    markCreated,
    markDate,
    markId,
    markTime,
    markMetadata,
    comment,
    grade,
    _MarkComment,
    _MarkRate,
    _MarkIdea,
    _MarkMoney,
    _MarkShape,
    _MarkPhoto,
    MarkMetadata,
  )
where

import Control.Lens (Fold, Getter, makeLenses, makePrisms, to)
import Data.Aeson
  ( FromJSON (..),
    Value (String),
    eitherDecode,
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Universum.Shape (Shape)

data MarkMetadata = MarkMetadata
  { _markId :: Text,
    _markDate :: Day,
    _markTime :: TimeOfDay,
    _markCreated :: UTCTime,
    _markChanged :: UTCTime
  }

makeLenses ''MarkMetadata

data Mark
  = MarkComment {_markMetadata :: MarkMetadata, _comment :: Text}
  | MarkRate {_markMetadata :: MarkMetadata, _grade :: Int}
  | MarkIdea {_markMetadata :: MarkMetadata, _comment :: Text}
  | MarkMoney {_markMetadata :: MarkMetadata, _money :: Float, _comment :: Text}
  | MarkShape {_markMetadata :: MarkMetadata, _shape :: Shape}
  | MarkPhoto {_markMetadata :: MarkMetadata, _photo :: FilePath, _comment :: Text}

makePrisms ''Mark

markMetadata :: Getter Mark MarkMetadata
markMetadata = to _markMetadata

comment' :: forall f. (Applicative f) => (Text -> f Text) -> Mark -> f Mark
comment' f x = case x of
  MarkComment m comment'' -> MarkComment <$> pure m <*> f comment''
  MarkIdea m comment'' -> MarkIdea <$> pure m <*> f comment''
  MarkMoney m money comment'' -> MarkMoney <$> pure m <*> pure money <*> f comment''
  MarkPhoto m photo comment'' -> MarkPhoto <$> pure m <*> pure photo <*> f comment''
  _ -> pure x

comment :: Fold Mark Text
comment f mark = comment' f mark

grade' :: forall f. (Applicative f) => (Int -> f Int) -> Mark -> f Mark
grade' f x = case x of
  MarkRate m grade'' -> MarkRate <$> pure m <*> f grade''
  _ -> pure x

grade :: Fold Mark Int
grade f mark = grade' f mark

markPrefix :: (IsString s) => s
markPrefix = "ru.schustovd.diary.api."

instance FromJSON Mark where
  parseJSON =
    withObject "Mark" $ \v -> do
      id <- v .: "id"
      date <- v .: "date"
      time <- v .: "time"
      createdInt <- v .: "created"
      changedInt <- v .: "changed"
      let created = posixSecondsToUTCTime (fromInteger $ createdInt `div` 1000)
          changed = posixSecondsToUTCTime (fromInteger $ changedInt `div` 1000)
          metadata = MarkMetadata id date time created changed
      type_ <- v .: "type"
      if markPrefix `Text.isPrefixOf` type_
        then case Text.drop 23 type_ of
          "CommentMark" -> MarkComment metadata <$> (v .: "comment")
          "RateMark" -> MarkRate metadata <$> (v .: "grade")
          "ShapeMark" -> MarkShape metadata <$> (v .: "shape")
          "IdeaMark" -> MarkIdea metadata <$> (v .: "comment")
          "PhotoMark" -> MarkPhoto metadata <$> (v .: "photo") <*> (v .: "comment")
          "MoneyMark" -> MarkMoney metadata <$> (v .: "money") <*> (v .: "comment")
          invalidType -> fail $ "unknown mark type " ++ Text.unpack type_
        else fail $ "invalid mark type, doesn't begin with " ++ markPrefix ++ ": " ++ Text.unpack type_
