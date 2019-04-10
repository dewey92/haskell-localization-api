{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Entity where

-- | Identify as Entity where Int is indeed the ID of the entity
newtype Entity a = Entity (Int, a)

class DbEntity a b where
  toDbEntity :: a -> b
  fromDbEntity :: b -> a



data JihadDb = JihadDb { name :: String, age :: Int, istri :: Int }
data Value = Value { vName :: String, vAge :: Int }

jihad = Entity (1, JihadDb "jihad" 26 1)

instance DbEntity JihadDb Value where
  toDbEntity a = undefined
  fromDbEntity b = undefined
