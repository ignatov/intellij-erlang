package org.intellij.erlang.bif;

import com.intellij.util.containers.MultiMap;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

public final class ErlangBifTable {
  private static final MultiMap<String, ErlangBifDescriptor> bifMap = new MultiMap<String, ErlangBifDescriptor>() {
    @Override
    protected Collection<ErlangBifDescriptor> createCollection() {
      return new TreeSet<ErlangBifDescriptor>();
    }
  };

  static {
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "abs", 1, "Int"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "adler32", 1, "Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "adler32", 2, "OldAdler, Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "adler32_combine", 3, "FirstAdler, SecondAdler, SecondSize"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "apply", 3, "Module, Function, Args"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "atom_to_list", 1, "Atom"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_list", 1, "Binary"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_list", 3, "Binary, Start, Stop"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_term", 1, "Binary"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "check_process_code", 2, "Pid, Module"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "crc32", 1, "Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "crc32", 2, "OldCrc, Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "crc32_combine", 3, "FirstCrc, SecondCrc, SecondSize"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "date", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "delete_module", 1, "Module"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "display", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "display_string", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "display_nl", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "element", 2, "N, Tuple"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "erase", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "erase", 1, "Key"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "exit", 1, "Reason"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "exit", 2, "Pid, Reason"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "external_size", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "external_size", 2, "Term, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float", 1, "Number"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float_to_list", 1, "Float"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float_to_list", 2, "Float, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "fun_info", 2, "Fun, Item"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "garbage_collect", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "garbage_collect", 1, "Pid"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get", 1, "Key"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_keys", 1, "Val"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "group_leader", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "group_leader", 2, "GroupLeader, Pid"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "halt", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "halt", 1, "Status"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "halt", 2, "Status, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "phash", 2, "Term, Range"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "phash2", 1, "Term, Range"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "phash2", 2, "Term, Range"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "hd", 1, "List"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "integer_to_list", 1, "Integer"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_alive", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "length", 1, "List"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "link", 1, "PidOrPort"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_atom", 1, "String"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_binary", 1, "IoList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_float", 1, "String"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_integer", 1, "String"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_pid", 1, "String"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_tuple", 1, "List"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "loaded", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "localtime", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "localtime_to_universaltime", 2, "Localtime, IsDst"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_ref", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5", 1, "Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5_init", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5_update", 2, "Context, Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "md5_final", 1, "Context"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "module_loaded", 1, "Module"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "function_exported", 3, "Module, Function, Arity"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "monitor_node", 2, "Node, Flag"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "monitor_node", 3, "Node, Flag, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "node", 1, "Arg"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "node", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "nodes", 1, "Arg"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "now", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "open_port", 2, "PortName, PortSettings"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "pid_to_list", 1, "Pid"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "ports", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "pre_loaded", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_flag", 2, "Flag :: trap_exit, Boolean"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_flag", 3, "Pid, Flag, Value"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_info", 1, "Pid"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_info", 2, "Pid, ItemList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "processes", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "purge_module", 1, "Module"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "put", 2, "Key, Val"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "register", 2, "RegName, PidOrPort"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "registered", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "round", 1, "Number"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "self", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "setelement", 3, "Index, Tuple1, Value"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "size", 1, "Item"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "spawn", 3, "Module, Function, Args"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "spawn_link", 3, "Module, Function, Args"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "split_binary", 2, "Bin, Pos"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "statistics", 1, "Item :: context_switches"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "term_to_binary", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "term_to_binary", 2, "Term, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "throw", 1, "Any"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "time", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "tl", 1, "List"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trunc", 1, "Number"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "tuple_to_list", 1, "Tuple"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "universaltime", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "universaltime_to_localtime", 1, "Universaltime"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "unlink", 1, "Id"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "unregister", 1, "RegName"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "whereis", 1, "RegName"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "spawn_opt", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "setnode", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "setnode", 3, "P1, P2, P3"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dist_exit", 3, "P1, P2, P3"));
    bifMap.putValue("erts_internal", new ErlangBifDescriptor("erts_internal", "port_info", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_call", 3, "Port, Operation, Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_command", 2, "Port, Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_command", 3, "Port, Data, OptionList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_control", 3, "Port, Operation, Data"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_close", 1, "Port"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_connect", 2, "Port, Pid"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_set_data", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_get_data", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_pattern", 2, "MFA, MatchSpec"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_pattern", 3, "MFA, MatchSpec, FlagList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace", 3, "PidSpec, How, FlagList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_info", 2, "PidOrFunc, Item"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "trace_delivered", 1, "Tracee"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace_info", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace_print", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "seq_trace_print", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "suspend_process", 2, "Suspendee, OptList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "resume_process", 1, "Suspendee"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "process_display", 2, "Pid, Type"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "bump_reductions", 1, "Reductions"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "cos", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "cosh", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "sin", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "sinh", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "tan", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "tanh", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "acos", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "acosh", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "asin", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "asinh", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "atan", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "atanh", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "erf", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "erfc", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "exp", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "log", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "log10", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "sqrt", 1, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "atan2", 2, "X"));
    bifMap.putValue("math", new ErlangBifDescriptor("math", "pow", 2, "X"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "start_timer", 3, "Time, Dest, Msg"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "send_after", 3, "Time, Dest, Msg"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "cancel_timer", 1, "TimerRef"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "read_timer", 1, "TimerRef"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_tuple", 2, "Arity, InitialValue"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "append_element", 2, "Tuple1, Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_tuple", 3, "Arity, DefaultValue, InitList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_flag", 2, "Flag :: backtrace_depth, Depth"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_info", 1, "Item :: {allocator_sizes, Alloc}"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_monitor", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_monitor", 1, "Arg"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_monitor", 2, "MonitorPid, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_profile", 2, "ProfilerPid, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "system_profile", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "ref_to_list", 1, "Ref"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "port_to_list", 1, "Port"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "fun_to_list", 1, "Fun"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "monitor", 2, "Type, Item"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "demonitor", 1, "MonitorRef"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "demonitor", 2, "MonitorRef, OptionList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_process_alive", 1, "Pid"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "error", 1, "Reason"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "error", 2, "Reason, Args"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "raise", 3, "Class, Reason, Stacktrace"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_stacktrace", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_builtin", 3, "Module, Function, Arity"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "send", 2, "Dest, Msg"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "send", 3, "Dest, Msg, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "append", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "subtract", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_atom", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_list", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_tuple", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_float", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_integer", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_number", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_pid", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_port", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_reference", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_binary", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_function", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_function", 2, "Term, Arity"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_record", 2, "Term, RecordTag"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_record", 3, "Term, RecordTag, Size"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "match_spec_test", 3, "P1, P2, P3"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "all", 0, ""));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "new", 2, "Name, Options"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete", 1, "Tab"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete", 2, "Tab, Key"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete_all_objects", 1, "Tab"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "delete_object", 2, "Tab, Object"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "first", 1, "Tab"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "is_compiled_ms", 1, "Term"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "lookup", 2, "Tab, Key"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "lookup_element", 3, "Tab, Key, Pos"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "info", 1, "Tab"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "info", 2, "Tab, Item"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "last", 1, "Tab"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match", 1, "Continuation"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match", 2, "Tab, Pattern"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match", 3, "Tab, Pattern, Limit"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_object", 1, "Continuation"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_object", 2, "Tab, Pattern"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_object", 3, "Tab, Pattern, Limit"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "member", 2, "Tab, Key"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "next", 2, "Tab, Key1"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "prev", 2, "Tab, Key1"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "insert", 2, "Tab, ObjectOrObjects"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "insert_new", 2, "Tab, ObjectOrObjects"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "rename", 2, "Tab, Name"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "safe_fixtable", 2, "Tab, Fix"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "slot", 2, "Tab, I"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "update_counter", 3, "Tab, Key, Incr"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select", 1, "Continuation"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select", 2, "Tab, MatchSpec"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select", 3, "Tab, MatchSpec, Limit"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_count", 2, "Tab, MatchSpec"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_reverse", 1, "Continuation"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_reverse", 2, "Tab, MatchSpec"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_reverse", 3, "Tab, MatchSpec, Limit"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "select_delete", 2, "Tab, MatchSpec"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_spec_compile", 1, "MatchSpec"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "match_spec_run_r", 3, "P1, P2, P3"));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "putenv", 2, "VarName, Value"));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "getenv", 0, ""));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "getenv", 1, "VarName"));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "getpid", 0, ""));
    bifMap.putValue("os", new ErlangBifDescriptor("os", "timestamp", 0, ""));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "try_load", 3, "Path, Name, OptionList"));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "try_unload", 2, "Name, OptionList"));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "loaded_drivers", 0, ""));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "info", 2, "Name, Tag"));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "format_error_int", 1, "P1"));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "monitor", 2, "Tag, Item"));
    bifMap.putValue("erl_ddll", new ErlangBifDescriptor("erl_ddll", "demonitor", 1, "MonitorRef"));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "compile", 1, "Regexp"));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "compile", 2, "Regexp, Options"));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "run", 2, "Subject, RE"));
    bifMap.putValue("re", new ErlangBifDescriptor("re", "run", 3, "Subject, RE, Options"));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "member", 2, "Elem, List"));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "reverse", 2, "List1, Tail"));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "keymember", 3, "Key, N, TupleList"));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "keysearch", 3, "Key, N, TupleList"));
    bifMap.putValue("lists", new ErlangBifDescriptor("lists", "keyfind", 3, "Key, N, TupleList"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "disassemble", 1, "P1"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "breakpoint", 2, "P1, P2"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "same", 2, "P1, P2"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "flat_size", 1, "P1"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "get_internal_state", 1, "P1"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "set_internal_state", 2, "P1, P2"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "display", 1, "P1"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "dist_ext_to_term", 2, "P1, P2"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "instructions", 0, ""));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "dump_monitors", 1, "P1"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "dump_links", 1, "P1"));
    bifMap.putValue("erts_debug", new ErlangBifDescriptor("erts_debug", "lock_counters", 1, "P1"));
    // Since R8
    bifMap.putValue("code", new ErlangBifDescriptor("code", "get_chunk", 2, "P1, P2"));
    bifMap.putValue("code", new ErlangBifDescriptor("code", "module_md5", 1, "P1"));
    bifMap.putValue("code", new ErlangBifDescriptor("code", "make_stub_module", 3, "P1, P2, P3"));
    bifMap.putValue("code", new ErlangBifDescriptor("code", "is_module_native", 1, "Module"));
    // Since R9C
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "hibernate", 3, "P1, P2, P3"));
    bifMap.putValue("error_logger", new ErlangBifDescriptor("error_logger", "warning_map", 0, ""));
    // Since R10B
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_module_info", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "get_module_info", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_boolean", 1, "Term"));
    bifMap.putValue("string", new ErlangBifDescriptor("string", "to_integer", 1, "String"));
    bifMap.putValue("string", new ErlangBifDescriptor("string", "to_float", 1, "String"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "make_fun", 3, "P1, P2, P3"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "iolist_size", 1, "Item"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "iolist_to_binary", 1, "IoListOrBinary"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_existing_atom", 1, "String"));
    // Since R12B-0
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "is_bitstring", 1, "Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "tuple_size", 1, "Tuple"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "byte_size", 1, "Bitstring"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "bit_size", 1, "Bitstring"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_bitstring", 1, "BitstringList"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "bitstring_to_list", 1, "Bitstring"));
    // Since R12B-2
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "update_element", 3, "Tab, Key, ElementSpec :: [{Pos, Value}]"));
    // Since R12B-4
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "decode_packet", 3, "Type, Bin, Options"));
    // Since R12B-5
    bifMap.putValue("unicode", new ErlangBifDescriptor("unicode", "characters_to_binary", 2, "Data, InEncoding"));
    bifMap.putValue("unicode", new ErlangBifDescriptor("unicode", "characters_to_list", 2, "Data, InEncoding"));
    bifMap.putValue("unicode", new ErlangBifDescriptor("unicode", "bin_is_7bit", 1, "P1"));
    // Since R13A
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "atom_to_binary", 2, "Atom, Encoding"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_atom", 2, "Binary, Encoding"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_existing_atom", 2, "Binary, Encoding"));
    bifMap.putValue("net_kernel", new ErlangBifDescriptor("net_kernel", "dflag_unicode_io", 1, "P1"));
    // Since R13B-1
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "give_away", 3, "Tab, Pid, GiftData"));
    bifMap.putValue("ets", new ErlangBifDescriptor("ets", "setopts", 2, "Tab, Opts"));
    // Since R13B3
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "load_nif", 2, "Path, LoadInfo"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "call_on_load_function", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "finish_after_on_load", 2, "P1, P2"));
    // Since R13B4
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_term", 2, "Binary, Opts"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_part", 2, "Subject, PosLen"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_part", 3, "Subject, Start, Length"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "compile_pattern", 1, "Pattern"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "match", 2, "Subject, Pattern"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "match", 3, "Subject, Pattern, Options"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "matches", 2, "Subject, Pattern"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "matches", 3, "Subject, Pattern, Options"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "longest_common_prefix", 1, "Binaries"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "longest_common_suffix", 1, "Binaries"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "first", 1, "Subject"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "last", 1, "Subject"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "at", 2, "Subject, Pos"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "part", 2, "Subject, PosLen"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "part", 3, "Subject, Pos, Len"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "bin_to_list", 1, "Subject"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "bin_to_list", 2, "Subject, PosLen"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "bin_to_list", 3, "Subject, Pos, Len"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "list_to_bin", 1, "ByteList"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "copy", 1, "Subject"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "copy", 2, "Subject, N"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "referenced_byte_size", 1, "Binary"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "encode_unsigned", 1, "Unsigned"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "encode_unsigned", 2, "Unsigned, Endianess"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "decode_unsigned", 1, "Subject"));
    bifMap.putValue("binary", new ErlangBifDescriptor("binary", "decode_unsigned", 2, "Subject, Endianess"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "nif_error", 1, "Reason"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "nif_error", 2, "Reason, Args"));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "internal_name2native", 1, "P1"));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "internal_native2name", 1, "P1"));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "internal_normalize_utf8", 1, "P1"));
    bifMap.putValue("prim_file", new ErlangBifDescriptor("prim_file", "is_translatable", 1, "P1"));
    bifMap.putValue("file", new ErlangBifDescriptor("file", "native_name_encoding", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "check_old_code", 1, "Module"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "universaltime_to_posixtime", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "posixtime_to_universaltime", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_put_tag", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_get_tag", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_get_tag_data", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_spread_tag", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_restore_tag", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_prepend_vm_tag_data", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "dt_append_vm_tag_data", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "prepare_loading", 2, "P1, P2"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "finish_loading", 1, "P1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "insert_element", 3, "Index, Tuple1, Term"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "delete_element", 2, "Index, Tuple1"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_integer", 1, "Binary"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_integer", 2, "Binary, Base"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "integer_to_binary", 1, "Integer"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "list_to_integer", 2, "String, Base"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float_to_binary", 1, "Float"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "float_to_binary", 2, "Float, Options"));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "binary_to_float", 1, "Binary"));
    bifMap.putValue("io", new ErlangBifDescriptor("io", "printable_range", 0, ""));
    bifMap.putValue("erlang", new ErlangBifDescriptor("erlang", "hash", 2, "Term, Range"));

    putLagerFunctionsToBifMap();
    putGeneratedInfo();
  }

  public static final String MODULE_INFO = "module_info";

  private static void putGeneratedInfo() {
    bifMap.putValue("", new ErlangBifDescriptor("", "record_info", 2, "Key, Record"));
    bifMap.putValue("", new ErlangBifDescriptor("", MODULE_INFO, 0, ""));
    bifMap.putValue("", new ErlangBifDescriptor("", MODULE_INFO, 1, "Key"));
  }

  private static void putLagerFunctionsToBifMap() {
    putLagerFunctionsToBifMap(1, "What");
    putLagerFunctionsToBifMap(2, "Str, Args");
    putLagerFunctionsToBifMap(3, "Attrs, Str, Args");
  }

  private static void putLagerFunctionsToBifMap(int arity, String params) {
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "debug",      arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "info",       arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "notice",     arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "warning",    arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "error",      arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "critical",   arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "alert",      arity, params));
    bifMap.putValue("lager", new ErlangBifDescriptor("lager", "emergency",  arity, params));
  }

  private ErlangBifTable() {
  }

  @NotNull
  public static Collection<ErlangBifDescriptor> getBifs(@NotNull String moduleName) {
    return bifMap.get(moduleName);
  }

  @NotNull
  public static List<ErlangBifDescriptor> getBifs(@NotNull String moduleName, @NotNull String functionName) {
    final List<ErlangBifDescriptor> bifDescriptors = new ArrayList<ErlangBifDescriptor>();
    for (ErlangBifDescriptor bifDescriptor : bifMap.get(moduleName)) {
      if (functionName.equals(bifDescriptor.getName())) {
        bifDescriptors.add(bifDescriptor);
      }
    }
    return bifDescriptors;
  }


  public static boolean isBif(@NotNull String moduleName, @NotNull String functionName, int arity) {
    final Collection<ErlangBifDescriptor> erlangBifDescriptors = bifMap.get(moduleName);
    for (ErlangBifDescriptor bifDescriptor : erlangBifDescriptors) {
      if (bifDescriptor.getModule().equals(moduleName) && bifDescriptor.getName().equals(functionName) &&
        bifDescriptor.getArity() == arity) {
        return true;
      }
    }
    return false;
  }
}
