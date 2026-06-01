{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Setup where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

import Orville.SQLite

{- | A simple entity for testing, with an auto-increment primary key.
The PK (personId) is read-only — SQLite populates it via INTEGER PRIMARY KEY.
-}
data Person = Person
    { personId :: Int64
    , firstName :: Text
    , lastName :: Text
    , age :: Int64
    }
    deriving (Show, Eq)

personIdField :: FieldDefinition 'NotNull Int64
personIdField = integerField "id"

firstNameField :: FieldDefinition 'NotNull Text
firstNameField = textField "first_name"

lastNameField :: FieldDefinition 'NotNull Text
lastNameField = textField "last_name"

ageField :: FieldDefinition 'NotNull Int64
ageField = integerField "age"

personMarshaller :: SqlMarshaller Person Person
personMarshaller =
    Person
        <$> marshallReadOnlyField personIdField
        <*> marshallField firstName firstNameField
        <*> marshallField lastName lastNameField
        <*> marshallField age ageField

personTable :: TableDefinition Int64 Person Person
personTable =
    mkTableDefinition "person" (primaryKey personId personIdField) personMarshaller

-- | An entity where the PK is part of the write entity (not auto-increment).
data Widget = Widget
    { widgetId :: Int64
    , widgetLabel :: Text
    }
    deriving (Show, Eq)

widgetIdField :: FieldDefinition 'NotNull Int64
widgetIdField = integerField "widget_id"

widgetLabelField :: FieldDefinition 'NotNull Text
widgetLabelField = textField "label"

widgetMarshaller :: SqlMarshaller Widget Widget
widgetMarshaller =
    Widget
        <$> marshallField widgetId widgetIdField
        <*> marshallField widgetLabel widgetLabelField

widgetTable :: TableDefinition Int64 Widget Widget
widgetTable =
    mkTableDefinition "widget" (primaryKey widgetId widgetIdField) widgetMarshaller

-- | An entity with a nullable field.
data Task = Task
    { taskId :: Int64
    , taskDescription :: Text
    , taskDueDay :: Maybe Int64
    }
    deriving (Show, Eq)

taskIdField :: FieldDefinition 'NotNull Int64
taskIdField = integerField "task_id"

taskDescriptionField :: FieldDefinition 'NotNull Text
taskDescriptionField = textField "description"

taskDueDayField :: FieldDefinition 'Nullable Int64
taskDueDayField = nullableField (integerField "due_day")

taskMarshaller :: SqlMarshaller Task Task
taskMarshaller =
    Task
        <$> marshallReadOnlyField taskIdField
        <*> marshallField taskDescription taskDescriptionField
        <*> marshallMaybe taskDueDay taskDueDayField

taskTable :: TableDefinition Int64 Task Task
taskTable =
    mkTableDefinition "task" (primaryKey taskId taskIdField) taskMarshaller

{- | Run an OrvilleM action against a fresh in-memory database,
with the given table created via auto-migration.
-}
withFreshDb :: TableDefinition key w r -> OrvilleM a -> IO a
withFreshDb tableDef action = do
    db <- openConnection ":memory:"
    result <- withConnection db $ do
        autoMigrateSchema defaultOptions [schemaTable tableDef []]
        action
    closeConnection db
    pure result

{- | Run an OrvilleM action against a fresh in-memory database, creating the
table with custom SQL (to support UNIQUE constraints etc.) before running the
standard auto-migration. The custom SQL is run first, so any tables it creates
will be skipped by auto-migration's 'CreateTable' step.
-}
withFreshDbExtras ::
    [T.Text] ->
    TableDefinition key w r ->
    OrvilleM a ->
    IO a
withFreshDbExtras extras tableDef action = do
    db <- openConnection ":memory:"
    result <- withConnection db $ do
        mapM_ execute extras
        autoMigrateSchema defaultOptions [schemaTable tableDef []]
        action
    closeConnection db
    pure result

-- | An entity with a UNIQUE constraint on email (used for ByField testing).
-- The table must be created manually with a UNIQUE constraint since the
-- auto-migration doesn't support them yet.
data PersonUniqueEmail = PersonUniqueEmail
    { pueId :: Int64
    , pueName :: Text
    , pueEmail :: Text
    }
    deriving (Show, Eq)

pueIdField :: FieldDefinition 'NotNull Int64
pueIdField = integerField "id"

pueNameField :: FieldDefinition 'NotNull Text
pueNameField = textField "name"

pueEmailField :: FieldDefinition 'NotNull Text
pueEmailField = textField "email"

pueMarshaller :: SqlMarshaller PersonUniqueEmail PersonUniqueEmail
pueMarshaller =
    PersonUniqueEmail
        <$> marshallReadOnlyField pueIdField
        <*> marshallField pueName pueNameField
        <*> marshallField pueEmail pueEmailField

pueTable :: TableDefinition Int64 PersonUniqueEmail PersonUniqueEmail
pueTable =
    mkTableDefinition "person_unique_email" (primaryKey pueId pueIdField) pueMarshaller
