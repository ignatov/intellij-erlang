f(Record) ->
    fun(Record=#{authp := md5}) -> Record#{authp := usmHMACMD5AuthProtocol};
        (Record=#{authp := sha}) -> Record#{authp := usmHMACSHAAuthProtocol};
        (Record) -> Record
    end.