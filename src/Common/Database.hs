module Common.Database where

class Monad m => MonadDb m where
  getById :: conn -> entityId -> m result
  insert :: conn -> record -> m ()
  update :: conn -> predicate -> record -> m updatedRecord
  delete :: conn -> predicate -> m deletedRecord
