init(Args) ->
  OpsgenueOptions = get_opsgenue_config(),
  {ok, #state{ level = lager_util:config_to_mask(Loglevel)
             , opsgenie_url = URL
             , enabled = get_conf(opsgenie_enabled, Args, true)
             }}.

%% @private
get_opsgenue_config() ->
  lists:foldl(
    fun(Param, Acc) ->
      case application:get_env(lager_opsgenie_backend, Param, undefined) of
        undefined -> Acc;
        Value -> add_param(Param, Value, Acc)
      end
    end, #{}, ?OPSGENUE_OPTIONAL_PARAMS).
