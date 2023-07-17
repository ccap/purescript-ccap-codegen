module Ccap.Codegen.Database
  ( domainModule
  , tableModule
  ) where

import Prelude
import Ccap.Codegen.Cst as Cst
import Ccap.Codegen.DbSupportType (dbSupportTypes)
import Control.Monad.Except (ExceptT, except, withExceptT, runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (note)
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid as Monoid
import Data.String as String
import Data.Traversable (for)
import Database.PostgreSQL (Connection, PGError(..))
import Database.PostgreSQL.Aff (Query(..))
import Database.PostgreSQL.PG (query, withConnection)
import Database.PostgreSQL.Pool (Pool)
import Database.PostgreSQL.Row (Row0(..), Row1(..), Row3(..), Row6(..))
import Effect.Aff (Aff)
import Parsing (Position(..))

emptyPos :: Position
emptyPos = Position { index: 0, line: 0, column: 0 }

type Domain
  = { domainName :: String
    , dataType :: String
    , charMaxLen :: Maybe Int
    }

type AliasedType
  = { name :: String
    , type :: Cst.Typ
    }

type Config
  = { dbManagedColumns :: Maybe (Array String)
    , scalaPkg :: String
    , pursPkg :: String
    }

aliasedTypes :: Array AliasedType
aliasedTypes =
  [ { name: "CaseNoT"
    , type:
        Cst.Ref
          emptyPos
          { mod: Just (Cst.ModuleRef "CaseNoSupport")
          , typ: "CaseNo"
          , params: []
          }
    }
  ]

rowToDomain :: Row3 String String (Maybe Int) -> Domain
rowToDomain (Row3 domainName dataType charMaxLen) =
  { domainName
  , dataType
  , charMaxLen
  }

domainTypeDecl :: Domain -> Cst.TypeDecl
domainTypeDecl domain =
  Cst.TypeDecl
    { position: emptyPos
    , name: domain.domainName
    , topType: Cst.Wrap (dbType domain.dataType)
    , annots: Array.cons (instancesAnnotation domain.dataType) (annotations domain)
    , params: []
    }

annotations :: forall r. { charMaxLen :: Maybe Int | r } -> Array Cst.Annotation
annotations = Array.fromFoldable <<< map maxLengthAnnotation <<< _.charMaxLen

maxLengthAnnotation :: Int -> Cst.Annotation
maxLengthAnnotation = Cst.Annotation "validations" emptyPos <<< Array.singleton <<< param
  where
  param = Cst.AnnotationParam "maxLength" emptyPos <<< Just <<< show

domainModule :: Array Pool -> Config -> ExceptT String Aff (Maybe Cst.Module)
domainModule pools { scalaPkg, pursPkg } = do
  modules <- withExceptT show $ for pools domainsForOneDb
  pure
    $ map buildDomains
    $ NonEmptyArray.fromArray modules
  where
  buildDomains :: NonEmptyArray PartialDomainsModule -> Cst.Module
  buildDomains partialDomains =
    { types: partialDomains >>= _.types # NonEmptyArray.nubEq
    , imports: NonEmptyArray.toArray partialDomains >>= _.imports # Array.nub # Array.sort
    , exports: { pursPkg, scalaPkg }
    , name: Cst.ModuleName "Domains"
    }

  domainsForOneDb :: Pool -> ExceptT PGError Aff PartialDomainsModule
  domainsForOneDb pool =
    withConnection runExceptT pool \conn -> do
      results <- query conn (Query sql) Row0
      let
        types = Array.sortWith Cst.typeDeclName $ domainTypeDecl <<< rowToDomain <$> results

        typesWithoutAlias =
          Array.filter
            ( \(Cst.TypeDecl typeDecl) ->
                not Foldable.any (\aliasedType -> typeDecl.name == aliasedType.name) aliasedTypes
            )
            types
      nelTypes <-
        except
          ( note
              (ConversionError "Expected at least one type")
              (NonEmptyArray.fromArray typesWithoutAlias)
          )
      pure
        { types: nelTypes
        , imports: types >>= tableImports # Array.nub # Array.sort
        }

  sql =
    """
          select distinct domain_name, data_type, character_maximum_length
          from information_schema.domains
          where domain_schema in ('public', 'dbtran') and
                  data_type in ('numeric', 'character varying', 'character',
                                'integer', 'smallint', 'text', 'uuid',
                                'boolean', 'date', 'time without time zone',
                                'timestamp with time zone', 'interval') and
                  domain_name not in ('BatchIDT', 'XMLT')
          """

type PartialDomainsModule
  = { types :: NonEmptyArray Cst.TypeDecl
    , imports :: Array Cst.Import
    }

type DbColumn
  = { columnName :: String
    , dataType :: String
    , domainName :: Maybe String
    , charMaxLen :: Maybe Int
    , isDbManaged :: Boolean
    , isNullable :: String
    , isPrimaryKey :: Boolean
    }

dbRowToColumn :: Maybe (Array String) -> Row6 String String (Maybe String) (Maybe Int) String Boolean -> DbColumn
dbRowToColumn dbManagedColumns (Row6 columnName dataType domainName charMaxLen isNullable isPrimaryKey) =
  { columnName
  , dataType
  , domainName
  , charMaxLen
  , isDbManaged: Foldable.any (Foldable.elem columnName) dbManagedColumns
  , isNullable
  , isPrimaryKey
  }

occIdColumn :: DbColumn
occIdColumn =
  { columnName: "occId"
  , dataType: "occid"
  , domainName: Nothing
  , charMaxLen: Nothing
  , isDbManaged: true
  , isNullable: "NO"
  , isPrimaryKey: false
  }

tableModule :: Pool -> Config -> String -> ExceptT String Aff Cst.Module
tableModule pool { dbManagedColumns, scalaPkg, pursPkg } tableName =
  withExceptT show
    $ withConnection runExceptT pool \conn -> do
        columns <- queryColumns dbManagedColumns tableName conn
        nelColumns <- except (note (ConversionError ("Expected at least one column. Does the \"" <> tableName <> "\" table exist?")) (NonEmptyArray.fromArray columns))
        let
          decl = tableType tableName (nelColumns `NonEmptyArray.snoc` occIdColumn)
        pure
          { types: NonEmptyArray.singleton decl
          , imports: tableImports decl # Array.sort
          , exports:
              { scalaPkg
              , pursPkg
              }
          , name: Cst.ModuleName tableName
          }

tableImports :: Cst.TypeDecl -> Array Cst.Import
tableImports =
  Cst.typeDeclTopType
    >>> Cst.topTypeReferences
    >>> map _.mod
    >>> Array.catMaybes
    >>> Array.nub
    >>> map (\(Cst.ModuleRef name) -> Cst.Import emptyPos name)

queryColumns :: Maybe (Array String) -> String -> Connection -> ExceptT PGError Aff (Array DbColumn)
queryColumns dbManagedColumns tableName conn = do
  results <- query conn (Query sql) (Row1 tableName)
  pure $ dbRowToColumn dbManagedColumns <$> results
  where
  sql =
    """
    SELECT
        c.column_name
      , c.data_type
      , c.domain_name
      , c.character_maximum_length
      , c.is_nullable
      , i.indisprimary is true
    FROM
      information_schema.columns c
      INNER JOIN pg_attribute a ON (
        quote_ident(c.table_name) :: regclass = a.attrelid
        AND c.column_name = a.attname
      )
      LEFT OUTER JOIN pg_index i ON (
        a.attrelid = i.indrelid
        AND a.attnum = ANY(i.indkey)
        AND i.indisprimary
      )
    WHERE
      c.table_name = $1
      AND c.data_type IN
        ('numeric', 'character varying', 'character',
         'integer', 'smallint', 'text', 'uuid',
         'boolean', 'date', 'time without time zone',
         'timestamp with time zone', 'interval'
        )
    ORDER BY c.ordinal_position ;
    """

tableType :: String -> NonEmptyArray DbColumn -> Cst.TypeDecl
tableType tableName columns =
  Cst.TypeDecl
    { position: emptyPos
    , name: tableName
    , topType: Cst.Record (map dbRecordProp columns)
    , annots: []
    , params: []
    }

dbRecordProp :: DbColumn -> Cst.RecordProp
dbRecordProp col@{ columnName, domainName, dataType, isDbManaged, isNullable, isPrimaryKey } =
  let
    baseType = maybe (dbType dataType) domainRef domainName

    optioned = if isNullable == "YES" then Cst.Option (Cst.TType baseType) else baseType

    annots =
      ( (Array.fromFoldable (dbManagedAnnotation isDbManaged))
          <> (Array.fromFoldable (primaryKeyAnnotation isPrimaryKey))
      )
        <> Monoid.guard (isNothing domainName) annotations col
  in
    { name: columnName, typ: Cst.TType optioned, annots, position: emptyPos }

domainRef :: String -> Cst.Typ
domainRef domainName = case Array.find (\aliasedType -> aliasedType.name == domainName) aliasedTypes of
  Just aliasedType -> aliasedType.type
  Nothing -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "Domains"), typ: domainName, params: [] }

dbType :: String -> Cst.Typ
dbType dataType = case dataType of
  "numeric" -> Cst.Primitive Cst.PDecimal
  "character varying" -> Cst.Primitive Cst.PString
  "character" -> Cst.Primitive Cst.PString
  "integer" -> Cst.Primitive Cst.PInt
  "smallint" -> Cst.Primitive Cst.PSmallInt
  "text" -> Cst.Primitive Cst.PString
  "boolean" -> Cst.Primitive Cst.PBoolean
  "date" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Date", params: [] }
  "time without time zone" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Time", params: [] }
  "timestamp with time zone" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Timestamp", params: [] }
  "uuid" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "UUIDSupport"), typ: "UUID", params: [] }
  "interval" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "DateTimeSupport"), typ: "Duration", params: [] }
  "occid" -> Cst.Ref emptyPos { mod: Just (Cst.ModuleRef "OccSupport"), typ: "OccId", params: [] }
  _ -> Cst.Primitive Cst.PString -- XXX

dbManagedAnnotation :: Boolean -> Maybe Cst.Annotation
dbManagedAnnotation isDbManaged =
  if isDbManaged then
    Just (Cst.Annotation "dbManaged" emptyPos [])
  else
    Nothing

instancesAnnotation :: String -> Cst.Annotation
instancesAnnotation dataType =
  let
    attr attrName s = Cst.AnnotationParam attrName emptyPos (Just s)

    attrs e m =
      Array.cons
        (attr "equal" e)
        (Array.fromFoldable (map (attr "meta") m))

    capitalize s = (String.toUpper (String.take 1 s)) <> String.drop 1 s

    primAttrs t =
      attrs
        ("cats.instances." <> t <> ".catsKernelStdOrderFor" <> capitalize t)
        Nothing

    intAttrs = primAttrs "int"

    stringAttrs = primAttrs "string"
  in
    Cst.Annotation
      "instances"
      emptyPos
      ( maybe
          ( case dataType of
              "boolean" -> primAttrs "boolean"
              "character varying" -> stringAttrs
              "character" -> stringAttrs
              "integer" -> intAttrs
              "numeric" -> primAttrs "bigDecimal"
              "smallint" -> primAttrs "short"
              "text" -> stringAttrs
              _ -> stringAttrs
          )
          ( \{ instances } ->
              [ attr "equal" instances.equal
              , attr "meta" instances.meta
              ]
          )
          (Array.find (eq dataType <<< _.dataType) dbSupportTypes)
      )

primaryKeyAnnotation :: Boolean -> Maybe Cst.Annotation
primaryKeyAnnotation isPrimaryKey =
  if isPrimaryKey then
    Just (Cst.Annotation "primaryKey" emptyPos [])
  else
    Nothing
