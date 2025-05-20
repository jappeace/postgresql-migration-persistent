{-# LANGUAGE DataKinds #-}

-- | uses Postgres migration to run all found
--   migrations in a transaction.
--   then use persistent to check if the persistent
--   model aligns with what's in the database.
module PostgreSQL.Migration.Persistent
  ( runMigrations,
    PMMigrationResult (..),

    -- * options
    defaultOptions,
    PersistentMigrationOptions (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), asks)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.Postgresql (getSimpleConn, runSqlPool)
import Database.Persist.Sql (Migration, SqlBackend, showMigration)
import Database.PostgreSQL.Simple (Connection, rollback)
import Database.PostgreSQL.Simple.Migration qualified as Migration
import Database.PostgreSQL.Simple.Util qualified as Migration
import Prelude

-- | recommended default options for production settnigs
defaultOptions ::
  -- | migrations folder, for exmaple "migrations/up"
  FilePath ->
  -- | the logging options, for example
  --
  -- @
  --    (\case
  --       Left errmsg -> runInIO $ $logTM AlertS $ logStr errmsg
  --       Right infoMsg -> runInIO $ $logTM InfoS $ logStr infoMsg)
  -- @
  (Either Text Text -> IO ()) ->
  PersistentMigrationOptions
defaultOptions filepath logMsgs =
  PersistentMigrationOptions
    { pmoMigrationOptions =
        Migration.defaultOptions
          { Migration.optVerbose = Migration.Verbose,
            Migration.optLogWriter = logMsgs,
            -- NB we do the transaction around the entire thing
            Migration.optTransactionControl = Migration.NoNewTransaction
          },
      pmoMigrationSource = Migration.MigrationDirectory filepath,
      pmoSchemaMigrationTableName = "schema_migrations"
    }

-- | The result of the postgresql-migration operation.
data PMMigrationResult
  = MigrationConsistent
  | -- | rollback due to persistent inconsistencies
    MigrationRollbackDueTo [Text]
  | -- | some error from the postgres migration libraries
    MigrationLibraryError String
  | -- | caused by 'getSimpleConn' returning 'Nothing'
    MigrationNotBackedByPg
  deriving stock (Show, Eq)

data PersistentMigrationOptions = PersistentMigrationOptions
  { pmoMigrationOptions :: Migration.MigrationOptions,
    pmoMigrationSource :: Migration.MigrationCommand,
    pmoSchemaMigrationTableName :: String
  }

-- | Run the given migrations in a single transaction.  If the migration fails
-- somehow the transaction is rolled back.
runMigrations ::
  -- | eg 'defaultOptions'
  PersistentMigrationOptions ->
  -- | the Automatic migration. eg 'migrateAll', or migrateModels $(discoverEntities). note this is
  Migration ->
  -- | sql pool, created with for example 'withPostgresqlPool'.
  Pool SqlBackend ->
  IO PMMigrationResult
runMigrations config migrateAll pool =
  flip runSqlPool pool $ do
    asks getSimpleConn >>= \case
      Nothing -> pure $ MigrationNotBackedByPg
      Just conn -> do
        result <- runMigrationCommands config conn
        case result of
          Migration.MigrationError err ->
            pure $ MigrationLibraryError err
          Migration.MigrationSuccess ->
            assertPersistentConsistency migrateAll >>= \case
              MigrationConsistent -> pure MigrationConsistent
              failure -> liftIO $ failure <$ rollback conn

runMigrationCommands :: PersistentMigrationOptions -> Connection -> ReaderT SqlBackend IO (Migration.MigrationResult String)
runMigrationCommands options conn = do
  initialized <- liftIO $ Migration.existsTable conn $ pmoSchemaMigrationTableName options
  let migrations =
        if initialized
          then [pmoMigrationSource options]
          else [Migration.MigrationInitialization, pmoMigrationSource options]
  liftIO $ Migration.runMigrations conn (pmoMigrationOptions options) migrations

assertPersistentConsistency :: Migration -> ReaderT SqlBackend IO PMMigrationResult
assertPersistentConsistency migrateAll = do
  persistentAutoMig <- showMigration migrateAll
  if null persistentAutoMig
    then pure MigrationConsistent
    else do
      pure $ MigrationRollbackDueTo persistentAutoMig
