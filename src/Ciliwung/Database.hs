{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ciliwung.Database where

import Control.Monad.Identity (Identity)
import Database.Beam
  ( Beamable
  , Database
  , DatabaseSettings
  , Table
  , Columnar
  , PrimaryKey
  , TableEntity
  , primaryKey
  , defaultDbSettings
  )
import Database.Beam.MySQL
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthorT f = Author
  { _authorId :: Columnar f Text
  , _authorEmail :: Columnar f Text
  , _authorPassword :: Columnar f Text
  , _authorFullname :: Columnar f Text
  } deriving (Generic, Beamable)

type Author = AuthorT Identity
type AuthorId = PrimaryKey AuthorT Identity

deriving instance Show Author
deriving instance Eq Author

instance Table AuthorT where
  data PrimaryKey AuthorT f = AuthorId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = AuthorId . _authorId

data CiliwungDb f = CiliwungDb { _ciliwungAuthors :: f (TableEntity AuthorT) }
  deriving (Generic, Database be)

ciliwungDb :: DatabaseSettings be CiliwungDb
ciliwungDb = defaultDbSettings
