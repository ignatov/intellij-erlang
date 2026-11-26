init(Args) ->
  ApiKey = get_api_key(Args),
  URL = throw_get_conf(api_alert_endpoint, Args),
  Loglevel = throw_get_conf(level, Args),
  Formatter = get_conf(formatter, Args, lob_formatter),
  FormatterConfig = get_conf(formatter_config, Args, []),
  OpsgenueOptions = get_opsgenue_config(),
  {ok, #state{level = lager_util:config_to_mask(Loglevel),
              opsgenie_url = URL,
              api_key = ApiKey,
              opsgenue_options = OpsgenueOptions,
              formatter = Formatter,
              formatter_config = FormatterConfig,
              enabled = get_conf(opsgenie_enabled, Args, true)
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
