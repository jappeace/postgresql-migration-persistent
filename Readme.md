
> Don't you *ever* trust computers.
> \- first rule of programist bible.


This library combines `postgresql-migration` and `persistent`,
for the common use case of:
1. Run my manually defined migrations.
2. Check if the schema defined in persistent aligns with the database.
3. If not, rollback and error with the migration plan persistent wants to do.

## Usage

It could for example look something like this with katip logging:
```haskell
import Database.Schema.User()
import Database.Schema.Company()

migrateAll :: Migration
migrateAll = migrateModels $(discoverEntities)

main :: IO ()
main = do
  katipConfig <- mkKatipConfig "server" environment
  let migDir = "migrations/up"
  let migrationOptions = defaultOptions migDir $ \case
        Left errmsg -> runContext katipConfig $ $logTM AlertS $ logStr errmsg
        Right infoMsg -> runContext katipConfig $ $logTM InfoS $ logStr infoMsg

  result <- runMigrations migrationOptions migrateAll rawPool
  runContext katipConfig $ case result of
    MigrationConsistent -> $logTM InfoS "migration consistent"
    MigrationRollbackDueTo rollback -> do
      $logTM ErrorS $ logStr $ errorMessage rollback
      error "invalid migrations"
    MigrationLibraryError err' -> do
      $logTM ErrorS $ logStr err'
      error "migration library error"
    MigrationNotBackedByPg ->
      error "app expects pg backing for migrations to work"
      
  runMyApp
```

By default the migrations are applied in a large transaction,
but you can modify this behavior by overriding options record,
for example:

```haskell
import Database.PostgreSQL.Simple.Migration qualified as Migration

main = do
  ...
  let migrationOptions = defaultOptions migDir $ \case
        Left errmsg -> runContext katipConfig $ $logTM AlertS $ logStr errmsg
        Right infoMsg -> runContext katipConfig $ $logTM InfoS $ logStr infoMsg
  let overridenOptions = migrationOptions { pmoMigrationOptions = (pmoMigrationOptions migrationOptions ) {Migration.optTransactionControl = Migration.TransactionPerStep }}
```

pretty much all options are exposed from the underlying postgresql-migration
library.

### Tools
Enter the nix shell.
```
nix develop
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
