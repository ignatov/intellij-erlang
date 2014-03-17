parse_message(id_report, << ScriptVersion
, ConfigVersion
, AppVersion:3/binary
, VehicleClass
, UnitStatus
, ModemSelection
, ApplicationId
, MobileIdType
, QueryId:32
, ESN:8/binary
, IMEI:8/binary
, IMSI:8/binary
, MIN:8/binary
, ICC_ID:10/binary
, Exts/binary >>) ->
    ok.