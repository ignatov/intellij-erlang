-export([stop/0]).

stop() ->
    case 1 of
        Pid ->
            Flag = process_flag(trap_exit, true),
            receive
                {Pid} ->
                    process_flag(trap_exit, Flag),
                    ok
            end
    end.