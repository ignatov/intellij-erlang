handle_msg({join, InvitedBy, Stealth, LastN}, DeviceID, S) when is_binary(DeviceID),
                                                                    is_integer(InvitedBy),
                                                is_integer(Stealth) ->
    join(DeviceID, InvitedBy, Stealth, LastN, S).