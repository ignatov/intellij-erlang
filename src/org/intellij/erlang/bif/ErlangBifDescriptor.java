package org.intellij.erlang.bif;

import org.jetbrains.annotations.NotNull;
import com.intellij.util.containers.MultiMap;

import java.util.Collection;
import java.util.HashSet;

public final class ErlangBifDescriptor {
  private static final MultiMap<String, ErlangBifDescriptor> bifMap = new MultiMap<String, ErlangBifDescriptor>() {
    @Override
    protected Collection<ErlangBifDescriptor> createCollection() {
      return new HashSet<ErlangBifDescriptor>();
    }
  };

  private final String myModule;
  private final String myName;
  private final int myArity;

  private ErlangBifDescriptor(@NotNull String module, @NotNull String name, int arity) {
    myModule = module;
    myName = name;
    myArity = arity;
  }

  static {
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "abs", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "adler32", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "adler32", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "adler32_combine", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "apply", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "atom_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_list", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_term", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "check_process_code", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "crc32", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "crc32", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "crc32_combine", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "date", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "delete_module", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "display", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "display_string", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "display_nl", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "element", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "erase", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "erase", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "exit", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "exit", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "external_size", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "external_size", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "fun_info", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "garbage_collect", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "garbage_collect", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_keys", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "group_leader", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "group_leader", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "halt", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "halt", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "halt", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "phash", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "phash2", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "phash2", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "hd", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "integer_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_alive", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "length", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "link", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_atom", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_binary", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_float", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_integer", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_pid", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_tuple", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "load_module", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "loaded", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "localtime", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "localtime_to_universaltime", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_ref", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5_init", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5_update", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5_final", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "module_loaded", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "function_exported", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "monitor_node", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "monitor_node", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "node", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "node", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "nodes", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "now", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "open_port", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "pid_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_info", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_info", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "ports", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "pre_loaded", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_flag", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_flag", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_info", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_info", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "processes", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "purge_module", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "put", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "register", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "registered", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "round", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "self", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "setelement", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "size", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "spawn", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "spawn_link", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "split_binary", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "statistics", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "term_to_binary", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "term_to_binary", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "throw", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "time", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "tl", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trunc", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "tuple_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "universaltime", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "universaltime_to_localtime", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "unlink", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "unregister", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "whereis", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "spawn_opt", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "setnode", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "setnode", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dist_exit", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_call", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_call", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_command", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_command", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_control", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_close", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_connect", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_set_data", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_get_data", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_pattern", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_pattern", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_info", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_delivered", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace_info", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace_print", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace_print", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "suspend_process", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "resume_process", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_display", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "bump_reductions", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "cos", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "cosh", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "sin", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "sinh", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "tan", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "tanh", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "acos", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "acosh", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "asin", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "asinh", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "atan", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "atanh", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "erf", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "erfc", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "exp", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "log", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "log10", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "sqrt", 1));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "atan2", 2));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "pow", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "start_timer", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "send_after", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "cancel_timer", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "read_timer", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_tuple", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "append_element", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_tuple", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_flag", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_info", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_monitor", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_monitor", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_monitor", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_profile", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_profile", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "ref_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "fun_to_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "monitor", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "demonitor", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "demonitor", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_process_alive", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "error", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "error", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "raise", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_stacktrace", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_builtin", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "send", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "send", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "append", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "subtract", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_atom", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_list", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_tuple", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_float", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_integer", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_number", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_pid", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_port", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_reference", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_binary", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_function", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_function", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_record", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_record", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "match_spec_test", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "all", 0));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "new", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete_all_objects", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete_object", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "first", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "is_compiled_ms", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "lookup", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "lookup_element", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "info", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "info", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "last", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_object", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_object", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_object", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "member", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "next", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "prev", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "insert", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "insert_new", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "rename", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "safe_fixtable", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "slot", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "update_counter", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_count", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_reverse", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_reverse", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_reverse", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_delete", 2));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_spec_compile", 1));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_spec_run_r", 3));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "putenv", 2));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "getenv", 0));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "getenv", 1));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "getpid", 0));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "timestamp", 0));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "try_load", 3));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "try_unload", 2));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "loaded_drivers", 0));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "info", 2));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "format_error_int", 1));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "monitor", 2));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "demonitor", 1));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "compile", 1));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "compile", 2));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "run", 2));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "run", 3));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "member", 2));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "reverse", 2));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "keymember", 3));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "keysearch", 3));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "keyfind", 3));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "disassemble", 1));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "breakpoint", 2));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "same", 2));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "flat_size", 1));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "get_internal_state", 1));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "set_internal_state", 2));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "display", 1));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "dist_ext_to_term", 2));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "instructions", 0));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "dump_monitors", 1));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "dump_links", 1));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "lock_counters", 1));
    // Since R8
    bifMap.putValue("code", new ErlangBifDescriptor("code", "get_chunk", 2));
    bifMap.putValue("code", new ErlangBifDescriptor("code", "module_md5", 1));
    bifMap.putValue("code", new ErlangBifDescriptor("code", "make_stub_module", 3));
    bifMap.putValue("code", new ErlangBifDescriptor("code", "is_module_native", 1));
    // Since R9C
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "hibernate", 3));
    bifMap.putValue("error_logger", new ErlangBifDescriptor("error_logger", "warning_map", 0));
    // Since R10B
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_module_info", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_module_info", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_boolean", 1));
    bifMap.putValue("string", new ErlangBifDescriptor("string", "to_integer", 1));
    bifMap.putValue("string", new ErlangBifDescriptor("string", "to_float", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_fun", 3));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "iolist_size", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "iolist_to_binary", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_existing_atom", 1));
    // Since R12B-0
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_bitstring", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "tuple_size", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "byte_size", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "bit_size", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_bitstring", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "bitstring_to_list", 1));
    // Since R12B-2
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "update_element", 3));
    // Since R12B-4
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "decode_packet", 3));
    // Since R12B-5
    bifMap.putValue("unicode", new ErlangBifDescriptor("unicode", "characters_to_binary", 2));
    bifMap.putValue("unicode", new ErlangBifDescriptor("unicode", "characters_to_list", 2));
    bifMap.putValue("unicode", new ErlangBifDescriptor("unicode", "bin_is_7bit", 1));
    // Since R13A
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "atom_to_binary", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_atom", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_existing_atom", 2));
    bifMap.putValue("net_kernel", new ErlangBifDescriptor("net_kernel", "dflag_unicode_io", 1));
    // Since R13B-1
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "give_away", 3));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "setopts", 2));
    // Since R13B3
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "load_nif", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "call_on_load_function", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "finish_after_on_load", 2));
    // Since R13B4
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_term", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_part", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_part", 3));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "compile_pattern", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "match", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "match", 3));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "matches", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "matches", 3));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "longest_common_prefix", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "longest_common_suffix", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "first", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "last", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "at", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "part", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "part", 3));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "bin_to_list", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "bin_to_list", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "bin_to_list", 3));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "list_to_bin", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "copy", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "copy", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "referenced_byte_size", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "encode_unsigned", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "encode_unsigned", 2));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "decode_unsigned", 1));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "decode_unsigned", 2));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "nif_error", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "nif_error", 2));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "internal_name2native", 1));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "internal_native2name", 1));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "internal_normalize_utf8", 1));
    bifMap.putValue("file", new ErlangBifDescriptor("file", "native_name_encoding", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "check_old_code", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "universaltime_to_posixtime", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "posixtime_to_universaltime", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_put_tag", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_get_tag", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_get_tag_data", 0));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_spread_tag", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_restore_tag", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_prepend_vm_tag_data", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_append_vm_tag_data", 1));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "hash", 2));
  }

  @NotNull
  public static MultiMap<String, ErlangBifDescriptor> getBifDescriptorsMap() {
    return bifMap;
  }

  @NotNull
  public String getModule() {
    return myModule;
  }

  @NotNull
  public String getName() {
    return myName;
  }

  public int getArity() {
    return myArity;
  }
}
