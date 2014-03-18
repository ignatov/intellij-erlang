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
    %% 2.7 ID Report Message (Message Type 3)
      #id_report{script_version  = ScriptVersion, config_version = ConfigVersion, app_version = AppVersion, vehicle_class = VehicleClass
        ,        unit_status     = unit_status(UnitStatus)
        ,        modem_selection = ModemSelection
        ,        application_id  = ApplicationId, mobile_id_type = mobile_id_type(MobileIdType)
        ,        query_id        = QueryId
        ,        esn             = unpack_bcd(ESN)
        ,        imei   = unpack_bcd(IMEI)
        ,        imsi                                            = unpack_bcd(IMSI)
        ,        min             = unpack_bcd(MIN)
        ,        icc_id          = unpack_bcd(ICC_ID)
        ,        extensions      = parse_extensions(Exts)
      }.