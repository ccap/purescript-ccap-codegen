module Ccap.Codegen.DbSupportType
  ( DbSupportType
  , dbSupportTypes
  , SupportType
  , supportTypes
  ) where

import Data.Maybe (Maybe(..))

type SupportType
  = { moduleName :: String
    , typeName :: String
    , underlyingSqlType :: String
    }

supportTypes :: Array SupportType
supportTypes =
  [ { moduleName: "CaseNoSupport"
    , typeName: "CaseNo"
    , underlyingSqlType: "text"
    }
  , { moduleName: "OccSupport"
    , typeName: "OccId"
    , underlyingSqlType: "text"
    }
  ]

type DbSupportType
  = { dataType :: String
    , instances ::
        { equal :: String
        , get :: Maybe String
        , meta :: String
        }
    , moduleName :: String
    , typeName :: String
    , underlyingSqlType :: String
    }

dbSupportTypes :: Array DbSupportType
dbSupportTypes =
  [ { dataType: "date"
    , instances:
        { equal: "gov.wicourts.common.instances.dates.localDateEqual"
        , meta: "gov.wicourts.common.Meta.metaLocalDate"
        , get: Nothing
        }
    , moduleName: "DateTimeSupport"
    , typeName: "Date"
    , underlyingSqlType: "date"
    }
  , { dataType: "interval"
    , instances:
        { equal: "gov.wicourts.jsoncommon.data.DurationWithLife.Duration.eqDuration"
        , get: Nothing
        , meta: "gov.wicourts.jsoncommon.data.DurationWithLife.Duration.metaDuration"
        }
    , moduleName: "DateTimeSupport"
    , typeName: "Duration"
    , underlyingSqlType: "interval"
    }
  , { dataType: "time without time zone"
    , instances:
        { equal: "gov.wicourts.common.instances.times.equalLocalTime"
        , get: Nothing
        , meta: "gov.wicourts.common.Meta.metaLocalTime"
        }
    , moduleName: "DateTimeSupport"
    , typeName: "Time"
    , underlyingSqlType: "time"
    }
  , { dataType: "timestamp with time zone"
    , instances:
        { equal: "gov.wicourts.common.instances.dates.localDateTimeEqual"
        , get: Nothing
        , meta: "gov.wicourts.common.Meta.metaLocalDateTime"
        }
    , moduleName: "DateTimeSupport"
    , typeName: "Timestamp"
    , underlyingSqlType: "timestamp"
    }
  , { dataType: "uuid"
    , instances:
        { equal: "gov.wicourts.common.instances.uuid.equalUUID"
        , get: Just "gov.wicourts.common.instances.uuid.getUUID"
        , meta: "gov.wicourts.common.Meta.metaUuid"
        }
    , moduleName: "UUIDSupport"
    , typeName: "UUID"
    , underlyingSqlType: "uuid"
    }
  ]
