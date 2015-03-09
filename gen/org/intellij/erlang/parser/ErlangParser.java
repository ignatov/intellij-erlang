// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static org.intellij.erlang.ErlangTypes.*;
import static org.intellij.erlang.parser.ErlangParserUtil.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class ErlangParser implements PsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, EXTENDS_SETS_);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    if (t == ERL_ADDITIVE_EXPRESSION) {
      r = expression(b, 0, 6);
    }
    else if (t == ERL_AFTER_CLAUSE) {
      r = after_clause(b, 0);
    }
    else if (t == ERL_AFTER_CLAUSE_BODY) {
      r = after_clause_body(b, 0);
    }
    else if (t == ERL_ANDALSO_EXPRESSION) {
      r = expression(b, 0, 3);
    }
    else if (t == ERL_ANONYMOUS_CALL_EXPRESSION) {
      r = expression(b, 0, 11);
    }
    else if (t == ERL_ARGUMENT_DEFINITION) {
      r = argument_definition(b, 0);
    }
    else if (t == ERL_ARGUMENT_DEFINITION_LIST) {
      r = argument_definition_list(b, 0);
    }
    else if (t == ERL_ARGUMENT_LIST) {
      r = argument_list(b, 0);
    }
    else if (t == ERL_ASSIGNMENT_EXPRESSION) {
      r = expression(b, 0, 0);
    }
    else if (t == ERL_ATOM) {
      r = atom(b, 0);
    }
    else if (t == ERL_ATOM_ATTRIBUTE) {
      r = atom_attribute(b, 0);
    }
    else if (t == ERL_ATOM_WITH_ARITY_EXPRESSION) {
      r = atom_with_arity_expression(b, 0);
    }
    else if (t == ERL_ATTR_VAL) {
      r = attr_val(b, 0);
    }
    else if (t == ERL_ATTRIBUTE) {
      r = attribute(b, 0);
    }
    else if (t == ERL_BEGIN_END_BODY) {
      r = begin_end_body(b, 0);
    }
    else if (t == ERL_BEGIN_END_EXPRESSION) {
      r = begin_end_expression(b, 0);
    }
    else if (t == ERL_BEHAVIOUR) {
      r = behaviour(b, 0);
    }
    else if (t == ERL_BIN_BASE_TYPE) {
      r = bin_base_type(b, 0);
    }
    else if (t == ERL_BIN_ELEMENT) {
      r = bin_element(b, 0);
    }
    else if (t == ERL_BIN_UNIT_TYPE) {
      r = bin_unit_type(b, 0);
    }
    else if (t == ERL_BINARY_EXPRESSION) {
      r = binary_expression(b, 0);
    }
    else if (t == ERL_BINARY_TYPE) {
      r = binary_type(b, 0);
    }
    else if (t == ERL_BIT_TYPE) {
      r = bit_type(b, 0);
    }
    else if (t == ERL_CALLBACK_SPEC) {
      r = callback_spec(b, 0);
    }
    else if (t == ERL_CASE_EXPRESSION) {
      r = case_expression(b, 0);
    }
    else if (t == ERL_CATCH_EXPRESSION) {
      r = catch_expression(b, 0);
    }
    else if (t == ERL_CLAUSE_BODY) {
      r = clause_body(b, 0);
    }
    else if (t == ERL_CLAUSE_GUARD) {
      r = clause_guard(b, 0);
    }
    else if (t == ERL_COLON_QUALIFIED_EXPRESSION) {
      r = expression(b, 0, 10);
    }
    else if (t == ERL_COMP_OP_EXPRESSION) {
      r = expression(b, 0, 4);
    }
    else if (t == ERL_CONFIG_CALL_EXPRESSION) {
      r = config_call_expression(b, 0);
    }
    else if (t == ERL_CONFIG_EXPRESSION) {
      r = config_expression(b, 0);
    }
    else if (t == ERL_CR_CLAUSE) {
      r = cr_clause(b, 0);
    }
    else if (t == ERL_EXPORT) {
      r = export(b, 0);
    }
    else if (t == ERL_EXPORT_FUNCTION) {
      r = export_function(b, 0);
    }
    else if (t == ERL_EXPORT_FUNCTIONS) {
      r = export_functions(b, 0);
    }
    else if (t == ERL_EXPORT_TYPE) {
      r = export_type(b, 0);
    }
    else if (t == ERL_EXPORT_TYPE_ATTRIBUTE) {
      r = export_type_attribute(b, 0);
    }
    else if (t == ERL_EXPORT_TYPES) {
      r = export_types(b, 0);
    }
    else if (t == ERL_EXPRESSION) {
      r = expression(b, 0, -1);
    }
    else if (t == ERL_FIELD_TYPE) {
      r = field_type(b, 0);
    }
    else if (t == ERL_FUN_CLAUSE) {
      r = fun_clause(b, 0);
    }
    else if (t == ERL_FUN_CLAUSES) {
      r = fun_clauses(b, 0);
    }
    else if (t == ERL_FUN_EXPRESSION) {
      r = fun_expression(b, 0);
    }
    else if (t == ERL_FUN_TYPE) {
      r = fun_type(b, 0);
    }
    else if (t == ERL_FUN_TYPE_100_T) {
      r = fun_type_100_t(b, 0);
    }
    else if (t == ERL_FUN_TYPE_ARGUMENTS) {
      r = fun_type_arguments(b, 0);
    }
    else if (t == ERL_FUN_TYPE_SIGS) {
      r = fun_type_sigs(b, 0);
    }
    else if (t == ERL_FUN_TYPE_SIGS_BRACES) {
      r = fun_type_sigs_braces(b, 0);
    }
    else if (t == ERL_FUNCTION) {
      r = function(b, 0);
    }
    else if (t == ERL_FUNCTION_CALL_EXPRESSION) {
      r = function_call_expression(b, 0);
    }
    else if (t == ERL_FUNCTION_CLAUSE) {
      r = function_clause(b, 0);
    }
    else if (t == ERL_FUNCTION_WITH_ARITY) {
      r = function_with_arity(b, 0);
    }
    else if (t == ERL_FUNCTION_WITH_ARITY_VARIABLES) {
      r = function_with_arity_variables(b, 0);
    }
    else if (t == ERL_GENERIC_FUNCTION_CALL_EXPRESSION) {
      r = generic_function_call_expression(b, 0);
    }
    else if (t == ERL_GLOBAL_FUNCTION_CALL_EXPRESSION) {
      r = global_function_call_expression(b, 0);
    }
    else if (t == ERL_GUARD) {
      r = guard(b, 0);
    }
    else if (t == ERL_IF_CLAUSE) {
      r = if_clause(b, 0);
    }
    else if (t == ERL_IF_EXPRESSION) {
      r = if_expression(b, 0);
    }
    else if (t == ERL_IMPORT_DIRECTIVE) {
      r = import_directive(b, 0);
    }
    else if (t == ERL_IMPORT_FUNCTION) {
      r = import_function(b, 0);
    }
    else if (t == ERL_IMPORT_FUNCTIONS) {
      r = import_functions(b, 0);
    }
    else if (t == ERL_INCLUDE) {
      r = include(b, 0);
    }
    else if (t == ERL_INCLUDE_LIB) {
      r = include_lib(b, 0);
    }
    else if (t == ERL_INCLUDE_STRING) {
      r = include_string(b, 0);
    }
    else if (t == ERL_INT_TYPE) {
      r = int_type(b, 0);
    }
    else if (t == ERL_LC_EXPRESSION) {
      r = lc_expression(b, 0);
    }
    else if (t == ERL_LC_EXPRS) {
      r = lc_exprs(b, 0);
    }
    else if (t == ERL_LIST_COMPREHENSION) {
      r = list_comprehension(b, 0);
    }
    else if (t == ERL_LIST_EXPRESSION) {
      r = list_expression(b, 0);
    }
    else if (t == ERL_LIST_OP_EXPRESSION) {
      r = expression(b, 0, 5);
    }
    else if (t == ERL_MACRO_CALL_ARGUMENT_LIST) {
      r = macro_call_argument_list(b, 0);
    }
    else if (t == ERL_MACRO_FORM) {
      r = macro_form(b, 0);
    }
    else if (t == ERL_MACROS) {
      r = macros(b, 0);
    }
    else if (t == ERL_MACROS_ARG) {
      r = macros_arg(b, 0);
    }
    else if (t == ERL_MACROS_BODY) {
      r = macros_body(b, 0);
    }
    else if (t == ERL_MACROS_DEFINITION) {
      r = macros_definition(b, 0);
    }
    else if (t == ERL_MACROS_NAME) {
      r = macros_name(b, 0);
    }
    else if (t == ERL_MAP_ENTRY) {
      r = map_entry(b, 0);
    }
    else if (t == ERL_MAP_ENTRY_TYPE) {
      r = map_entry_type(b, 0);
    }
    else if (t == ERL_MAP_EXPRESSION) {
      r = expression(b, 0, 11);
    }
    else if (t == ERL_MAP_TUPLE) {
      r = map_tuple(b, 0);
    }
    else if (t == ERL_MAP_TYPE) {
      r = map_type(b, 0);
    }
    else if (t == ERL_MAX_EXPRESSION) {
      r = max_expression(b, 0);
    }
    else if (t == ERL_MODEL_FIELD_LIST) {
      r = model_field_list(b, 0);
    }
    else if (t == ERL_MODULE) {
      r = module(b, 0);
    }
    else if (t == ERL_MODULE_REF) {
      r = module_ref(b, 0);
    }
    else if (t == ERL_MULTIPLICATIVE_EXPRESSION) {
      r = expression(b, 0, 7);
    }
    else if (t == ERL_OPT_BIT_TYPE_LIST) {
      r = opt_bit_type_list(b, 0);
    }
    else if (t == ERL_ORELSE_EXPRESSION) {
      r = expression(b, 0, 2);
    }
    else if (t == ERL_PARENTHESIZED_EXPRESSION) {
      r = parenthesized_expression(b, 0);
    }
    else if (t == ERL_PREFIX_EXPRESSION) {
      r = prefix_expression(b, 0);
    }
    else if (t == ERL_Q_ATOM) {
      r = q_atom(b, 0);
    }
    else if (t == ERL_Q_VAR) {
      r = q_var(b, 0);
    }
    else if (t == ERL_QUALIFIED_EXPRESSION) {
      r = qualified_expression(b, 0);
    }
    else if (t == ERL_RECEIVE_EXPRESSION) {
      r = receive_expression(b, 0);
    }
    else if (t == ERL_RECORD_DEFINITION) {
      r = record_definition(b, 0);
    }
    else if (t == ERL_RECORD_EXPRESSION) {
      r = expression(b, 0, 11);
    }
    else if (t == ERL_RECORD_FIELD) {
      r = record_field(b, 0);
    }
    else if (t == ERL_RECORD_LIKE_TYPE) {
      r = record_like_type(b, 0);
    }
    else if (t == ERL_RECORD_REF) {
      r = record_ref(b, 0);
    }
    else if (t == ERL_RECORD_TUPLE) {
      r = record_tuple(b, 0);
    }
    else if (t == ERL_RULE) {
      r = rule(b, 0);
    }
    else if (t == ERL_RULE_BODY) {
      r = rule_body(b, 0);
    }
    else if (t == ERL_RULE_CLAUSE) {
      r = rule_clause(b, 0);
    }
    else if (t == ERL_SEND_EXPRESSION) {
      r = expression(b, 0, 1);
    }
    else if (t == ERL_SPEC_FUN) {
      r = spec_fun(b, 0);
    }
    else if (t == ERL_SPECIFICATION) {
      r = specification(b, 0);
    }
    else if (t == ERL_STRING_LITERAL) {
      r = string_literal(b, 0);
    }
    else if (t == ERL_TOP_TYPE) {
      r = top_type(b, 0);
    }
    else if (t == ERL_TOP_TYPE_CLAUSE) {
      r = top_type_clause(b, 0);
    }
    else if (t == ERL_TRY_CATCH) {
      r = try_catch(b, 0);
    }
    else if (t == ERL_TRY_CLAUSE) {
      r = try_clause(b, 0);
    }
    else if (t == ERL_TRY_CLAUSES) {
      r = try_clauses(b, 0);
    }
    else if (t == ERL_TRY_EXPRESSION) {
      r = try_expression(b, 0);
    }
    else if (t == ERL_TRY_EXPRESSIONS_CLAUSE) {
      r = try_expressions_clause(b, 0);
    }
    else if (t == ERL_TUPLE_EXPRESSION) {
      r = tuple_expression(b, 0);
    }
    else if (t == ERL_TYPE) {
      r = type(b, 0);
    }
    else if (t == ERL_TYPE_DEFINITION) {
      r = type_definition(b, 0);
    }
    else if (t == ERL_TYPE_GUARD) {
      r = type_guard(b, 0);
    }
    else if (t == ERL_TYPE_REF) {
      r = type_ref(b, 0);
    }
    else if (t == ERL_TYPE_SIG) {
      r = type_sig(b, 0);
    }
    else if (t == ERL_TYPE_SIG_GUARD) {
      r = type_sig_guard(b, 0);
    }
    else if (t == ERL_TYPED_ATTR_VAL) {
      r = typed_attr_val(b, 0);
    }
    else if (t == ERL_TYPED_EXPR) {
      r = typed_expr(b, 0);
    }
    else if (t == ERL_TYPED_RECORD_FIELDS) {
      r = typed_record_fields(b, 0);
    }
    else {
      r = parse_root_(t, b, 0);
    }
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return forms(b, l + 1);
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(ERL_ADDITIVE_EXPRESSION, ERL_ANDALSO_EXPRESSION, ERL_ASSIGNMENT_EXPRESSION, ERL_COMP_OP_EXPRESSION,
      ERL_LIST_OP_EXPRESSION, ERL_MULTIPLICATIVE_EXPRESSION, ERL_ORELSE_EXPRESSION, ERL_SEND_EXPRESSION),
    create_token_set_(ERL_BINARY_TYPE, ERL_BIN_BASE_TYPE, ERL_BIN_UNIT_TYPE, ERL_BIT_TYPE,
      ERL_EXPORT_TYPE, ERL_FIELD_TYPE, ERL_FUN_TYPE, ERL_FUN_TYPE_100_T,
      ERL_INT_TYPE, ERL_MAP_ENTRY_TYPE, ERL_MAP_TYPE, ERL_RECORD_LIKE_TYPE,
      ERL_TOP_TYPE, ERL_TYPE),
    create_token_set_(ERL_ADDITIVE_EXPRESSION, ERL_ANDALSO_EXPRESSION, ERL_ANONYMOUS_CALL_EXPRESSION, ERL_ASSIGNMENT_EXPRESSION,
      ERL_ATOM_WITH_ARITY_EXPRESSION, ERL_BEGIN_END_EXPRESSION, ERL_BINARY_EXPRESSION, ERL_CASE_EXPRESSION,
      ERL_CATCH_EXPRESSION, ERL_COLON_QUALIFIED_EXPRESSION, ERL_COMP_OP_EXPRESSION, ERL_CONFIG_CALL_EXPRESSION,
      ERL_CONFIG_EXPRESSION, ERL_EXPRESSION, ERL_FUNCTION_CALL_EXPRESSION, ERL_FUN_EXPRESSION,
      ERL_GENERIC_FUNCTION_CALL_EXPRESSION, ERL_GLOBAL_FUNCTION_CALL_EXPRESSION, ERL_IF_EXPRESSION, ERL_LC_EXPRESSION,
      ERL_LIST_COMPREHENSION, ERL_LIST_EXPRESSION, ERL_LIST_OP_EXPRESSION, ERL_MAP_EXPRESSION,
      ERL_MAX_EXPRESSION, ERL_MULTIPLICATIVE_EXPRESSION, ERL_ORELSE_EXPRESSION, ERL_PARENTHESIZED_EXPRESSION,
      ERL_PREFIX_EXPRESSION, ERL_QUALIFIED_EXPRESSION, ERL_RECEIVE_EXPRESSION, ERL_RECORD_EXPRESSION,
      ERL_SEND_EXPRESSION, ERL_STRING_LITERAL, ERL_TRY_EXPRESSION, ERL_TUPLE_EXPRESSION),
  };

  /* ********************************************************** */
  // '+' |'-' | bor | bxor | bsl | bsr | or | xor
  static boolean add_op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "add_op")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_PLUS);
    if (!r) r = consumeToken(b, ERL_OP_MINUS);
    if (!r) r = consumeToken(b, ERL_BOR);
    if (!r) r = consumeToken(b, ERL_BXOR);
    if (!r) r = consumeToken(b, ERL_BSL);
    if (!r) r = consumeToken(b, ERL_BSR);
    if (!r) r = consumeToken(b, ERL_OR);
    if (!r) r = consumeToken(b, ERL_XOR);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // after after_clause_body
  public static boolean after_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "after_clause")) return false;
    if (!nextTokenIs(b, ERL_AFTER)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_AFTER);
    p = r; // pin = 1
    r = r && after_clause_body(b, l + 1);
    exit_section_(b, l, m, ERL_AFTER_CLAUSE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // <<guarded expression>> clause_body
  public static boolean after_clause_body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "after_clause_body")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<after clause body>");
    r = guarded(b, l + 1, expression_parser_);
    p = r; // pin = 1
    r = r && clause_body(b, l + 1);
    exit_section_(b, l, m, ERL_AFTER_CLAUSE_BODY, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // expression
  public static boolean argument_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_definition")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<argument definition>");
    r = expression(b, l + 1, -1);
    exit_section_(b, l, m, ERL_ARGUMENT_DEFINITION, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '(' argument_definition? (',' argument_definition)* ')'
  public static boolean argument_definition_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_definition_list")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, argument_definition_list_1(b, l + 1));
    r = p && report_error_(b, argument_definition_list_2(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_ARGUMENT_DEFINITION_LIST, r, p, null);
    return r || p;
  }

  // argument_definition?
  private static boolean argument_definition_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_definition_list_1")) return false;
    argument_definition(b, l + 1);
    return true;
  }

  // (',' argument_definition)*
  private static boolean argument_definition_list_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_definition_list_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!argument_definition_list_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "argument_definition_list_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' argument_definition
  private static boolean argument_definition_list_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_definition_list_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && argument_definition(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '(' call_exprs? ')'
  public static boolean argument_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, argument_list_1(b, l + 1));
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_ARGUMENT_LIST, r, p, null);
    return r || p;
  }

  // call_exprs?
  private static boolean argument_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1")) return false;
    call_exprs(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // atom_name | (single_quote atom_name single_quote)
  public static boolean atom(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom")) return false;
    if (!nextTokenIs(b, "<atom>", ERL_ATOM_NAME, ERL_SINGLE_QUOTE)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<atom>");
    r = consumeToken(b, ERL_ATOM_NAME);
    if (!r) r = atom_1(b, l + 1);
    exit_section_(b, l, m, ERL_ATOM, r, false, null);
    return r;
  }

  // single_quote atom_name single_quote
  private static boolean atom_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeTokens(b, 1, ERL_SINGLE_QUOTE, ERL_ATOM_NAME, ERL_SINGLE_QUOTE);
    p = r; // pin = 1
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // atom_name ['(' typed_attr_val ')' | typed_attr_val | attr_val]
  public static boolean atom_attribute(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_attribute")) return false;
    if (!nextTokenIs(b, ERL_ATOM_NAME)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_ATOM_NAME);
    r = r && atom_attribute_1(b, l + 1);
    exit_section_(b, m, ERL_ATOM_ATTRIBUTE, r);
    return r;
  }

  // ['(' typed_attr_val ')' | typed_attr_val | attr_val]
  private static boolean atom_attribute_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_attribute_1")) return false;
    atom_attribute_1_0(b, l + 1);
    return true;
  }

  // '(' typed_attr_val ')' | typed_attr_val | attr_val
  private static boolean atom_attribute_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_attribute_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atom_attribute_1_0_0(b, l + 1);
    if (!r) r = typed_attr_val(b, l + 1);
    if (!r) r = attr_val(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' typed_attr_val ')'
  private static boolean atom_attribute_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_attribute_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_LEFT);
    r = r && typed_attr_val(b, l + 1);
    r = r && consumeToken(b, ERL_PAR_RIGHT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom !'(' | integer | string_literal+ | float | char
  static boolean atomic(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atomic")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atomic_0(b, l + 1);
    if (!r) r = consumeToken(b, ERL_INTEGER);
    if (!r) r = atomic_2(b, l + 1);
    if (!r) r = consumeToken(b, ERL_FLOAT);
    if (!r) r = consumeToken(b, ERL_CHAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // q_atom !'('
  private static boolean atomic_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atomic_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom(b, l + 1);
    r = r && atomic_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !'('
  private static boolean atomic_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atomic_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, ERL_PAR_LEFT);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // string_literal+
  private static boolean atomic_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atomic_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = string_literal(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!string_literal(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "atomic_2", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' exprs ')' | exprs
  public static boolean attr_val(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_val")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<attribute value>");
    r = attr_val_0(b, l + 1);
    if (!r) r = exprs(b, l + 1);
    exit_section_(b, l, m, ERL_ATTR_VAL, r, false, null);
    return r;
  }

  // '(' exprs ')'
  private static boolean attr_val_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_val_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_LEFT);
    r = r && exprs(b, l + 1);
    r = r && consumeToken(b, ERL_PAR_RIGHT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '-' (
  //     module
  //   | export
  //   | export_type_attribute
  //   | import_directive
  //   | specification
  //   | callback_spec
  //   | behaviour
  //   | on_load
  //   | ifdef_ifndef_undef_attribute
  //   | else_atom_attribute <<enterMode "ELSE">>
  //   | <<withOn "ATOM_ATTRIBUTE" atom_attribute>>
  //   )
  public static boolean attribute(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attribute")) return false;
    if (!nextTokenIs(b, ERL_OP_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    p = r; // pin = 1
    r = r && attribute_1(b, l + 1);
    exit_section_(b, l, m, ERL_ATTRIBUTE, r, p, null);
    return r || p;
  }

  // module
  //   | export
  //   | export_type_attribute
  //   | import_directive
  //   | specification
  //   | callback_spec
  //   | behaviour
  //   | on_load
  //   | ifdef_ifndef_undef_attribute
  //   | else_atom_attribute <<enterMode "ELSE">>
  //   | <<withOn "ATOM_ATTRIBUTE" atom_attribute>>
  private static boolean attribute_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attribute_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = module(b, l + 1);
    if (!r) r = export(b, l + 1);
    if (!r) r = export_type_attribute(b, l + 1);
    if (!r) r = import_directive(b, l + 1);
    if (!r) r = specification(b, l + 1);
    if (!r) r = callback_spec(b, l + 1);
    if (!r) r = behaviour(b, l + 1);
    if (!r) r = on_load(b, l + 1);
    if (!r) r = ifdef_ifndef_undef_attribute(b, l + 1);
    if (!r) r = attribute_1_9(b, l + 1);
    if (!r) r = withOn(b, l + 1, "ATOM_ATTRIBUTE", atom_attribute_parser_);
    exit_section_(b, m, null, r);
    return r;
  }

  // else_atom_attribute <<enterMode "ELSE">>
  private static boolean attribute_1_9(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attribute_1_9")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = else_atom_attribute(b, l + 1);
    r = r && enterMode(b, l + 1, "ELSE");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // !'(' <<p1>> | '(' <<p1>> ')'
  static boolean attribute_tail(PsiBuilder b, int l, final Parser _p1) {
    if (!recursion_guard_(b, l, "attribute_tail")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = attribute_tail_0(b, l + 1, _p1);
    if (!r) r = attribute_tail_1(b, l + 1, _p1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !'(' <<p1>>
  private static boolean attribute_tail_0(PsiBuilder b, int l, final Parser _p1) {
    if (!recursion_guard_(b, l, "attribute_tail_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = attribute_tail_0_0(b, l + 1);
    r = r && _p1.parse(b, l);
    exit_section_(b, m, null, r);
    return r;
  }

  // !'('
  private static boolean attribute_tail_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attribute_tail_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, ERL_PAR_LEFT);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // '(' <<p1>> ')'
  private static boolean attribute_tail_1(PsiBuilder b, int l, final Parser _p1) {
    if (!recursion_guard_(b, l, "attribute_tail_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_LEFT);
    r = r && _p1.parse(b, l);
    r = r && consumeToken(b, ERL_PAR_RIGHT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // exprs
  public static boolean begin_end_body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "begin_end_body")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = exprs(b, l + 1);
    exit_section_(b, l, m, ERL_BEGIN_END_BODY, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // begin begin_end_body end
  public static boolean begin_end_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "begin_end_expression")) return false;
    if (!nextTokenIs(b, ERL_BEGIN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BEGIN);
    p = r; // pin = 1
    r = r && report_error_(b, begin_end_body(b, l + 1));
    r = p && consumeToken(b, ERL_END) && r;
    exit_section_(b, l, m, ERL_BEGIN_END_EXPRESSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'behaviour' '(' module_ref ')'
  public static boolean behaviour(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "behaviour")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<behaviour>");
    r = consumeToken(b, "behaviour");
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, module_ref(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_BEHAVIOUR, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // q_var ':' integer
  public static boolean bin_base_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_base_type")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_var(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    r = r && consumeToken(b, ERL_INTEGER);
    exit_section_(b, m, ERL_BIN_BASE_TYPE, r);
    return r;
  }

  /* ********************************************************** */
  // bin_base_type ',' bin_unit_type
  static boolean bin_base_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_base_type_list")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = bin_base_type(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_COMMA));
    r = p && bin_unit_type(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // generic_function_call_expression
  //   | prefix_op? bin_expression [':' bin_expression] opt_bit_type_list?
  public static boolean bin_element(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<binary element>");
    r = generic_function_call_expression(b, l + 1);
    if (!r) r = bin_element_1(b, l + 1);
    exit_section_(b, l, m, ERL_BIN_ELEMENT, r, false, null);
    return r;
  }

  // prefix_op? bin_expression [':' bin_expression] opt_bit_type_list?
  private static boolean bin_element_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = bin_element_1_0(b, l + 1);
    r = r && bin_expression(b, l + 1);
    r = r && bin_element_1_2(b, l + 1);
    r = r && bin_element_1_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // prefix_op?
  private static boolean bin_element_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_1_0")) return false;
    prefix_op(b, l + 1);
    return true;
  }

  // [':' bin_expression]
  private static boolean bin_element_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_1_2")) return false;
    bin_element_1_2_0(b, l + 1);
    return true;
  }

  // ':' bin_expression
  private static boolean bin_element_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_COLON);
    r = r && bin_expression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // opt_bit_type_list?
  private static boolean bin_element_1_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_1_3")) return false;
    opt_bit_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // bin_element (',' bin_element)*
  static boolean bin_element_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = bin_element(b, l + 1);
    p = r; // pin = 1
    r = r && bin_element_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' bin_element)*
  private static boolean bin_element_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!bin_element_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "bin_element_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' bin_element
  private static boolean bin_element_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_element_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && bin_element(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // max_expression | parenthesized_expression
  static boolean bin_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = max_expression(b, l + 1);
    if (!r) r = parenthesized_expression(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // q_var ':' q_var '*' integer
  public static boolean bin_unit_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bin_unit_type")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_var(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    r = r && q_var(b, l + 1);
    r = r && consumeToken(b, ERL_OP_AR_MUL);
    r = r && consumeToken(b, ERL_INTEGER);
    exit_section_(b, m, ERL_BIN_UNIT_TYPE, r);
    return r;
  }

  /* ********************************************************** */
  // '<<' binary_expression '||' lc_exprs '>>'
  public static boolean binary_comprehension(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_comprehension")) return false;
    if (!nextTokenIs(b, ERL_BIN_START)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BIN_START);
    r = r && binary_expression(b, l + 1);
    r = r && consumeToken(b, ERL_OR_OR);
    p = r; // pin = 3
    r = r && report_error_(b, lc_exprs(b, l + 1));
    r = p && consumeToken(b, ERL_BIN_END) && r;
    exit_section_(b, l, m, ERL_LIST_COMPREHENSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '<<' bin_element_list? '>>'
  public static boolean binary_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_expression")) return false;
    if (!nextTokenIs(b, ERL_BIN_START)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BIN_START);
    p = r; // pin = 1
    r = r && report_error_(b, binary_expression_1(b, l + 1));
    r = p && consumeToken(b, ERL_BIN_END) && r;
    exit_section_(b, l, m, ERL_BINARY_EXPRESSION, r, p, null);
    return r || p;
  }

  // bin_element_list?
  private static boolean binary_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_expression_1")) return false;
    bin_element_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '<<' [bin_base_type | bin_unit_type | bin_base_type_list] '>>'
  public static boolean binary_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_type")) return false;
    if (!nextTokenIs(b, ERL_BIN_START)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BIN_START);
    p = r; // pin = 1
    r = r && report_error_(b, binary_type_1(b, l + 1));
    r = p && consumeToken(b, ERL_BIN_END) && r;
    exit_section_(b, l, m, ERL_BINARY_TYPE, r, p, null);
    return r || p;
  }

  // [bin_base_type | bin_unit_type | bin_base_type_list]
  private static boolean binary_type_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_type_1")) return false;
    binary_type_1_0(b, l + 1);
    return true;
  }

  // bin_base_type | bin_unit_type | bin_base_type_list
  private static boolean binary_type_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "binary_type_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = bin_base_type(b, l + 1);
    if (!r) r = bin_unit_type(b, l + 1);
    if (!r) r = bin_base_type_list(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom [':' integer]
  public static boolean bit_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bit_type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<type>");
    r = q_atom(b, l + 1);
    r = r && bit_type_1(b, l + 1);
    exit_section_(b, l, m, ERL_BIT_TYPE, r, false, null);
    return r;
  }

  // [':' integer]
  private static boolean bit_type_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bit_type_1")) return false;
    bit_type_1_0(b, l + 1);
    return true;
  }

  // ':' integer
  private static boolean bit_type_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bit_type_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_COLON);
    r = r && consumeToken(b, ERL_INTEGER);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // bit_type ('-' bit_type)*
  static boolean bit_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bit_type_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = bit_type(b, l + 1);
    p = r; // pin = 1
    r = r && bit_type_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ('-' bit_type)*
  private static boolean bit_type_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bit_type_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!bit_type_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "bit_type_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '-' bit_type
  private static boolean bit_type_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "bit_type_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    p = r; // pin = 1
    r = r && bit_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // expr_with_guard (',' expr_with_guard)*
  static boolean call_exprs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exprs")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = expr_with_guard(b, l + 1);
    p = r; // pin = 1
    r = r && call_exprs_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' expr_with_guard)*
  private static boolean call_exprs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exprs_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!call_exprs_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "call_exprs_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' expr_with_guard
  private static boolean call_exprs_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "call_exprs_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && expr_with_guard(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'callback' type_spec
  public static boolean callback_spec(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "callback_spec")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<callback spec>");
    r = consumeToken(b, "callback");
    p = r; // pin = 1
    r = r && type_spec(b, l + 1);
    exit_section_(b, l, m, ERL_CALLBACK_SPEC, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // case expression of cr_clauses end
  public static boolean case_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "case_expression")) return false;
    if (!nextTokenIs(b, ERL_CASE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_CASE);
    p = r; // pin = 1
    r = r && report_error_(b, expression(b, l + 1, -1));
    r = p && report_error_(b, consumeToken(b, ERL_OF)) && r;
    r = p && report_error_(b, cr_clauses(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_END) && r;
    exit_section_(b, l, m, ERL_CASE_EXPRESSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '->' exprs
  public static boolean clause_body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "clause_body")) return false;
    if (!nextTokenIs(b, ERL_ARROW)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_ARROW);
    p = r; // pin = 1
    r = r && exprs(b, l + 1);
    exit_section_(b, l, m, ERL_CLAUSE_BODY, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // when <<guarded guard>>
  public static boolean clause_guard(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "clause_guard")) return false;
    if (!nextTokenIs(b, ERL_WHEN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_WHEN);
    p = r; // pin = 1
    r = r && guarded(b, l + 1, guard_parser_);
    exit_section_(b, l, m, ERL_CLAUSE_GUARD, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '==' | '/=' |'=<' |'<' |'>=' |'>' |'=:=' |'=/='
  static boolean comp_op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "comp_op")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_EQ_EQ);
    if (!r) r = consumeToken(b, ERL_OP_DIV_EQ);
    if (!r) r = consumeToken(b, ERL_OP_EQ_LT);
    if (!r) r = consumeToken(b, ERL_OP_LT);
    if (!r) r = consumeToken(b, ERL_OP_GT_EQ);
    if (!r) r = consumeToken(b, ERL_OP_GT);
    if (!r) r = consumeToken(b, ERL_OP_EQ_COL_EQ);
    if (!r) r = consumeToken(b, ERL_OP_EQ_DIV_EQ);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' config_exprs? ')'
  public static boolean config_argument_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_argument_list")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, config_argument_list_1(b, l + 1));
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_ARGUMENT_LIST, r, p, null);
    return r || p;
  }

  // config_exprs?
  private static boolean config_argument_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_argument_list_1")) return false;
    config_exprs(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '<<' config_exprs? '>>'
  public static boolean config_bin_list_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_bin_list_expression")) return false;
    if (!nextTokenIs(b, ERL_BIN_START)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BIN_START);
    p = r; // pin = 1
    r = r && report_error_(b, config_bin_list_expression_1(b, l + 1));
    r = p && consumeToken(b, ERL_BIN_END) && r;
    exit_section_(b, l, m, ERL_BINARY_EXPRESSION, r, p, null);
    return r || p;
  }

  // config_exprs?
  private static boolean config_bin_list_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_bin_list_expression_1")) return false;
    config_exprs(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // config_argument_list
  public static boolean config_call_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_call_expression")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _LEFT_, null);
    r = config_argument_list(b, l + 1);
    exit_section_(b, l, m, ERL_CONFIG_CALL_EXPRESSION, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // !(')' | '>>' | ']' | '}')
  static boolean config_expr_recover(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_expr_recover")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !config_expr_recover_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // ')' | '>>' | ']' | '}'
  private static boolean config_expr_recover_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_expr_recover_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_RIGHT);
    if (!r) r = consumeToken(b, ERL_BIN_END);
    if (!r) r = consumeToken(b, ERL_BRACKET_RIGHT);
    if (!r) r = consumeToken(b, ERL_CURLY_RIGHT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // config_tuple_expression
  //   | config_list_expression
  //   | config_bin_list_expression
  //   | config_qualified_or_call_expression
  //   | (prefix_op? atomic)
  //   | q_var
  public static boolean config_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<expression>");
    r = config_tuple_expression(b, l + 1);
    if (!r) r = config_list_expression(b, l + 1);
    if (!r) r = config_bin_list_expression(b, l + 1);
    if (!r) r = config_qualified_or_call_expression(b, l + 1);
    if (!r) r = config_expression_4(b, l + 1);
    if (!r) r = q_var(b, l + 1);
    exit_section_(b, l, m, ERL_CONFIG_EXPRESSION, r, false, null);
    return r;
  }

  // prefix_op? atomic
  private static boolean config_expression_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_expression_4")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = config_expression_4_0(b, l + 1);
    r = r && atomic(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // prefix_op?
  private static boolean config_expression_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_expression_4_0")) return false;
    prefix_op(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // config_expression (',' config_expression)*
  static boolean config_exprs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_exprs")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = config_expression(b, l + 1);
    p = r; // pin = 1
    r = r && config_exprs_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, config_expr_recover_parser_);
    return r || p;
  }

  // (',' config_expression)*
  private static boolean config_exprs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_exprs_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!config_exprs_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "config_exprs_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' config_expression
  private static boolean config_exprs_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_exprs_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && config_expression(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '[' config_exprs? ']'
  public static boolean config_list_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_list_expression")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, config_list_expression_1(b, l + 1));
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, ERL_LIST_EXPRESSION, r, p, null);
    return r || p;
  }

  // config_exprs?
  private static boolean config_list_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_list_expression_1")) return false;
    config_exprs(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom left_accessors?
  static boolean config_qualified_or_call_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_qualified_or_call_expression")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && config_qualified_or_call_expression_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // left_accessors?
  private static boolean config_qualified_or_call_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_qualified_or_call_expression_1")) return false;
    left_accessors(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '{' config_exprs? '}'
  public static boolean config_tuple_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_tuple_expression")) return false;
    if (!nextTokenIs(b, ERL_CURLY_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_CURLY_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, config_tuple_expression_1(b, l + 1));
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_TUPLE_EXPRESSION, r, p, null);
    return r || p;
  }

  // config_exprs?
  private static boolean config_tuple_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "config_tuple_expression_1")) return false;
    config_exprs(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // is_console empty console_expression_or_empty
  static boolean console_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "console_expression")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = is_console(b, l + 1);
    r = r && empty(b, l + 1);
    p = r; // pin = 2
    r = r && console_expression_or_empty(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // exprs period | empty
  static boolean console_expression_or_empty(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "console_expression_or_empty")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = console_expression_or_empty_0(b, l + 1);
    if (!r) r = empty(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // exprs period
  private static boolean console_expression_or_empty_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "console_expression_or_empty_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = exprs(b, l + 1);
    p = r; // pin = 1
    r = r && period(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // cr_clause_argument_definition clause_guard? clause_body
  public static boolean cr_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cr_clause")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<cr clause>");
    r = cr_clause_argument_definition(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, cr_clause_1(b, l + 1));
    r = p && clause_body(b, l + 1) && r;
    exit_section_(b, l, m, ERL_CR_CLAUSE, r, p, null);
    return r || p;
  }

  // clause_guard?
  private static boolean cr_clause_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cr_clause_1")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // <<guarded argument_definition>>
  static boolean cr_clause_argument_definition(PsiBuilder b, int l) {
    return guarded(b, l + 1, argument_definition_parser_);
  }

  /* ********************************************************** */
  // cr_clause (';' cr_clause)*
  static boolean cr_clauses(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cr_clauses")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = cr_clause(b, l + 1);
    p = r; // pin = 1
    r = r && cr_clauses_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (';' cr_clause)*
  private static boolean cr_clauses_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cr_clauses_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!cr_clauses_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "cr_clauses_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' cr_clause
  private static boolean cr_clauses_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "cr_clauses_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && cr_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // &'else' atom_name
  public static boolean else_atom_attribute(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "else_atom_attribute")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<attribute>");
    r = else_atom_attribute_0(b, l + 1);
    r = r && consumeToken(b, ERL_ATOM_NAME);
    exit_section_(b, l, m, ERL_ATOM_ATTRIBUTE, r, false, null);
    return r;
  }

  // &'else'
  private static boolean else_atom_attribute_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "else_atom_attribute_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = consumeToken(b, "else");
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // ()
  static boolean empty(PsiBuilder b, int l) {
    return true;
  }

  /* ********************************************************** */
  // 'export' <<attribute_tail export_functions>>
  public static boolean export(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<export>");
    r = consumeToken(b, "export");
    p = r; // pin = 1
    r = r && attribute_tail(b, l + 1, export_functions_parser_);
    exit_section_(b, l, m, ERL_EXPORT, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean export_function(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_function")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<export function>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_OP_AR_DIV));
    r = p && consumeToken(b, ERL_INTEGER) && r;
    exit_section_(b, l, m, ERL_EXPORT_FUNCTION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // export_function (',' export_function)*
  static boolean export_function_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_function_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = export_function(b, l + 1);
    p = r; // pin = 1
    r = r && export_function_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' export_function)*
  private static boolean export_function_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_function_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!export_function_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "export_function_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' export_function
  private static boolean export_function_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_function_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && export_function(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '[' export_function_list? ']'
  public static boolean export_functions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_functions")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, export_functions_1(b, l + 1));
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, ERL_EXPORT_FUNCTIONS, r, p, null);
    return r || p;
  }

  // export_function_list?
  private static boolean export_functions_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_functions_1")) return false;
    export_function_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean export_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_type")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<type>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_OP_AR_DIV));
    r = p && consumeToken(b, ERL_INTEGER) && r;
    exit_section_(b, l, m, ERL_EXPORT_TYPE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'export_type' <<attribute_tail export_types>>
  public static boolean export_type_attribute(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_type_attribute")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<attribute>");
    r = consumeToken(b, "export_type");
    p = r; // pin = 1
    r = r && attribute_tail(b, l + 1, export_types_parser_);
    exit_section_(b, l, m, ERL_EXPORT_TYPE_ATTRIBUTE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // export_type (',' export_type)*
  static boolean export_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_type_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = export_type(b, l + 1);
    p = r; // pin = 1
    r = r && export_type_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' export_type)*
  private static boolean export_type_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_type_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!export_type_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "export_type_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' export_type
  private static boolean export_type_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_type_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && export_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '[' export_type_list? ']'
  public static boolean export_types(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_types")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, export_types_1(b, l + 1));
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, ERL_EXPORT_TYPES, r, p, null);
    return r || p;
  }

  // export_type_list?
  private static boolean export_types_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "export_types_1")) return false;
    export_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // expression clause_guard?
  static boolean expr_with_guard(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expr_with_guard")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = expression(b, l + 1, -1);
    p = r; // pin = 1
    r = r && expr_with_guard_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // clause_guard?
  private static boolean expr_with_guard_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expr_with_guard_1")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // expression (exprs_tail)*
  static boolean exprs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = expression(b, l + 1, -1);
    r = r && exprs_1(b, l + 1);
    exit_section_(b, l, m, null, r, false, exprs_recover_parser_);
    return r;
  }

  // (exprs_tail)*
  private static boolean exprs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!exprs_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "exprs_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // (exprs_tail)
  private static boolean exprs_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exprs_tail(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // !(')' | ',' | '->' | '.' | ':-' | ';' | '}' | after | catch | end | of | atom '(')
  static boolean exprs_recover(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs_recover")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !exprs_recover_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // ')' | ',' | '->' | '.' | ':-' | ';' | '}' | after | catch | end | of | atom '('
  private static boolean exprs_recover_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs_recover_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_RIGHT);
    if (!r) r = consumeToken(b, ERL_COMMA);
    if (!r) r = consumeToken(b, ERL_ARROW);
    if (!r) r = consumeToken(b, ERL_DOT);
    if (!r) r = consumeToken(b, ":-");
    if (!r) r = consumeToken(b, ERL_SEMI);
    if (!r) r = consumeToken(b, ERL_CURLY_RIGHT);
    if (!r) r = consumeToken(b, ERL_AFTER);
    if (!r) r = consumeToken(b, ERL_CATCH);
    if (!r) r = consumeToken(b, ERL_END);
    if (!r) r = consumeToken(b, ERL_OF);
    if (!r) r = exprs_recover_0_11(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // atom '('
  private static boolean exprs_recover_0_11(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs_recover_0_11")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atom(b, l + 1);
    r = r && consumeToken(b, ERL_PAR_LEFT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ',' expression
  static boolean exprs_tail(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exprs_tail")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && expression(b, l + 1, -1);
    exit_section_(b, l, m, null, r, p, exprs_recover_parser_);
    return r || p;
  }

  /* ********************************************************** */
  // q_atom '::' top_type
  public static boolean field_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "field_type")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<type>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_COLON_COLON));
    r = p && top_type(b, l + 1) && r;
    exit_section_(b, l, m, ERL_FIELD_TYPE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // field_type (',' field_type)*
  static boolean field_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "field_type_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = field_type(b, l + 1);
    p = r; // pin = 1
    r = r && field_type_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' field_type)*
  private static boolean field_type_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "field_type_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!field_type_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "field_type_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' field_type
  private static boolean field_type_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "field_type_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && field_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // is_app_config config_expression
  //   | function
  //   | record_definition
  //   | include_lib
  //   | include
  //   | macros_definition
  //   | type_definition
  //   | attribute
  //   | macro_form // macros support
  //   | rule
  //   | !<<eofOrSpace>>
  static boolean form(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "form")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = form_0(b, l + 1);
    if (!r) r = function(b, l + 1);
    if (!r) r = record_definition(b, l + 1);
    if (!r) r = include_lib(b, l + 1);
    if (!r) r = include(b, l + 1);
    if (!r) r = macros_definition(b, l + 1);
    if (!r) r = type_definition(b, l + 1);
    if (!r) r = attribute(b, l + 1);
    if (!r) r = macro_form(b, l + 1);
    if (!r) r = rule(b, l + 1);
    if (!r) r = form_10(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // is_app_config config_expression
  private static boolean form_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "form_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = is_app_config(b, l + 1);
    r = r && config_expression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !<<eofOrSpace>>
  private static boolean form_10(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "form_10")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !eofOrSpace(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // !('+' | '-' | '<<' | '?' | '[' | '{' | atom_name | single_quote | bnot | char | float | integer | not | string | var | '.')
  static boolean form_recover(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "form_recover")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !form_recover_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // '+' | '-' | '<<' | '?' | '[' | '{' | atom_name | single_quote | bnot | char | float | integer | not | string | var | '.'
  private static boolean form_recover_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "form_recover_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_PLUS);
    if (!r) r = consumeToken(b, ERL_OP_MINUS);
    if (!r) r = consumeToken(b, ERL_BIN_START);
    if (!r) r = consumeToken(b, ERL_QMARK);
    if (!r) r = consumeToken(b, ERL_BRACKET_LEFT);
    if (!r) r = consumeToken(b, ERL_CURLY_LEFT);
    if (!r) r = consumeToken(b, ERL_ATOM_NAME);
    if (!r) r = consumeToken(b, ERL_SINGLE_QUOTE);
    if (!r) r = consumeToken(b, ERL_BNOT);
    if (!r) r = consumeToken(b, ERL_CHAR);
    if (!r) r = consumeToken(b, ERL_FLOAT);
    if (!r) r = consumeToken(b, ERL_INTEGER);
    if (!r) r = consumeToken(b, ERL_NOT);
    if (!r) r = consumeToken(b, ERL_STRING);
    if (!r) r = consumeToken(b, ERL_VAR);
    if (!r) r = consumeToken(b, ERL_DOT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // form period
  static boolean form_with_period(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "form_with_period")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = form(b, l + 1);
    p = r; // pin = 1
    r = r && period(b, l + 1);
    exit_section_(b, l, m, null, r, p, form_recover_parser_);
    return r || p;
  }

  /* ********************************************************** */
  // console_expression | form_with_period *
  static boolean forms(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "forms")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = console_expression(b, l + 1);
    if (!r) r = forms_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // form_with_period *
  private static boolean forms_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "forms_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!form_with_period(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "forms_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // fun_name? argument_definition_list clause_guard? clause_body
  public static boolean fun_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_clause")) return false;
    if (!nextTokenIs(b, "<fun clause>", ERL_PAR_LEFT, ERL_VAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<fun clause>");
    r = fun_clause_0(b, l + 1);
    r = r && argument_definition_list(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, fun_clause_2(b, l + 1));
    r = p && clause_body(b, l + 1) && r;
    exit_section_(b, l, m, ERL_FUN_CLAUSE, r, p, null);
    return r || p;
  }

  // fun_name?
  private static boolean fun_clause_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_clause_0")) return false;
    fun_name(b, l + 1);
    return true;
  }

  // clause_guard?
  private static boolean fun_clause_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_clause_2")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // fun_clause (';' fun_clause)*
  public static boolean fun_clauses(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_clauses")) return false;
    if (!nextTokenIs(b, "<fun clauses>", ERL_PAR_LEFT, ERL_VAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<fun clauses>");
    r = fun_clause(b, l + 1);
    p = r; // pin = 1
    r = r && fun_clauses_1(b, l + 1);
    exit_section_(b, l, m, ERL_FUN_CLAUSES, r, p, null);
    return r || p;
  }

  // (';' fun_clause)*
  private static boolean fun_clauses_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_clauses_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!fun_clauses_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "fun_clauses_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' fun_clause
  private static boolean fun_clauses_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_clauses_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && fun_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // fun (fun_expression_block | fun_expression_lambda)
  public static boolean fun_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression")) return false;
    if (!nextTokenIs(b, ERL_FUN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_FUN);
    p = r; // pin = 1
    r = r && fun_expression_1(b, l + 1);
    exit_section_(b, l, m, ERL_FUN_EXPRESSION, r, p, null);
    return r || p;
  }

  // fun_expression_block | fun_expression_lambda
  private static boolean fun_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fun_expression_block(b, l + 1);
    if (!r) r = fun_expression_lambda(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // fun_clauses end
  static boolean fun_expression_block(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_block")) return false;
    if (!nextTokenIs(b, "", ERL_PAR_LEFT, ERL_VAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = fun_clauses(b, l + 1);
    p = r; // pin = 1
    r = r && consumeToken(b, ERL_END);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // [(module_ref | q_var) ':'] (function_with_arity | function_with_arity_variables)
  static boolean fun_expression_lambda(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_lambda")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fun_expression_lambda_0(b, l + 1);
    r = r && fun_expression_lambda_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [(module_ref | q_var) ':']
  private static boolean fun_expression_lambda_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_lambda_0")) return false;
    fun_expression_lambda_0_0(b, l + 1);
    return true;
  }

  // (module_ref | q_var) ':'
  private static boolean fun_expression_lambda_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_lambda_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fun_expression_lambda_0_0_0(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  // module_ref | q_var
  private static boolean fun_expression_lambda_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_lambda_0_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = module_ref(b, l + 1);
    if (!r) r = q_var(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // function_with_arity | function_with_arity_variables
  private static boolean fun_expression_lambda_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_expression_lambda_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = function_with_arity(b, l + 1);
    if (!r) r = function_with_arity_variables(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // fun_name_var
  public static boolean fun_name(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_name")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fun_name_var(b, l + 1);
    exit_section_(b, m, ERL_ARGUMENT_DEFINITION, r);
    return r;
  }

  /* ********************************************************** */
  // q_var
  public static boolean fun_name_var(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_name_var")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_var(b, l + 1);
    exit_section_(b, m, ERL_MAX_EXPRESSION, r);
    return r;
  }

  /* ********************************************************** */
  // fun_type_arguments top_type_clause
  public static boolean fun_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = fun_type_arguments(b, l + 1);
    p = r; // pin = 1
    r = r && top_type_clause(b, l + 1);
    exit_section_(b, l, m, ERL_FUN_TYPE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '(' ('...' | top_type_list?) ')' top_type_clause
  public static boolean fun_type_100_t(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_100_t")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, fun_type_100_t_1(b, l + 1));
    r = p && report_error_(b, consumeToken(b, ERL_PAR_RIGHT)) && r;
    r = p && top_type_clause(b, l + 1) && r;
    exit_section_(b, l, m, ERL_FUN_TYPE_100_T, r, p, null);
    return r || p;
  }

  // '...' | top_type_list?
  private static boolean fun_type_100_t_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_100_t_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_DOT_DOT_DOT);
    if (!r) r = fun_type_100_t_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // top_type_list?
  private static boolean fun_type_100_t_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_100_t_1_1")) return false;
    top_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '(' top_type_list? ')'
  public static boolean fun_type_arguments(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_arguments")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, fun_type_arguments_1(b, l + 1));
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_FUN_TYPE_ARGUMENTS, r, p, null);
    return r || p;
  }

  // top_type_list?
  private static boolean fun_type_arguments_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_arguments_1")) return false;
    top_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // [module_ref ':'] spec_fun '::'? type_sigs_list
  public static boolean fun_type_sigs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_sigs")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<fun type sigs>");
    r = fun_type_sigs_0(b, l + 1);
    r = r && spec_fun(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, fun_type_sigs_2(b, l + 1));
    r = p && type_sigs_list(b, l + 1) && r;
    exit_section_(b, l, m, ERL_FUN_TYPE_SIGS, r, p, null);
    return r || p;
  }

  // [module_ref ':']
  private static boolean fun_type_sigs_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_sigs_0")) return false;
    fun_type_sigs_0_0(b, l + 1);
    return true;
  }

  // module_ref ':'
  private static boolean fun_type_sigs_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_sigs_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = module_ref(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  // '::'?
  private static boolean fun_type_sigs_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_sigs_2")) return false;
    consumeToken(b, ERL_COLON_COLON);
    return true;
  }

  /* ********************************************************** */
  // '(' fun_type_sigs ')'
  public static boolean fun_type_sigs_braces(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "fun_type_sigs_braces")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, fun_type_sigs(b, l + 1));
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_FUN_TYPE_SIGS_BRACES, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // function_clause (';' function_clause)*
  public static boolean function(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<function>");
    r = function_clause(b, l + 1);
    p = r; // pin = 1
    r = r && function_1(b, l + 1);
    exit_section_(b, l, m, ERL_FUNCTION, r, p, null);
    return r || p;
  }

  // (';' function_clause)*
  private static boolean function_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!function_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "function_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' function_clause
  private static boolean function_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && function_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // plain_function_clause | maybe_macro_funciton_clause
  public static boolean function_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_clause")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<function clause>");
    r = plain_function_clause(b, l + 1);
    if (!r) r = maybe_macro_funciton_clause(b, l + 1);
    exit_section_(b, l, m, ERL_FUNCTION_CLAUSE, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // q_atom argument_definition_list
  static boolean function_clause_head(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_clause_head")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom(b, l + 1);
    r = r && argument_definition_list(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean function_with_arity(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_with_arity")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<function with arity>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_OP_AR_DIV));
    r = p && consumeToken(b, ERL_INTEGER) && r;
    exit_section_(b, l, m, ERL_FUNCTION_WITH_ARITY, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // q_var '/' (integer|q_var)
  public static boolean function_with_arity_variables(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_with_arity_variables")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = q_var(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_OP_AR_DIV));
    r = p && function_with_arity_variables_2(b, l + 1) && r;
    exit_section_(b, l, m, ERL_FUNCTION_WITH_ARITY_VARIABLES, r, p, null);
    return r || p;
  }

  // integer|q_var
  private static boolean function_with_arity_variables_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_with_arity_variables_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_INTEGER);
    if (!r) r = q_var(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // exprs (';' exprs)*
  public static boolean guard(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guard")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<guard>");
    r = exprs(b, l + 1);
    p = r; // pin = 1
    r = r && guard_1(b, l + 1);
    exit_section_(b, l, m, ERL_GUARD, r, p, null);
    return r || p;
  }

  // (';' exprs)*
  private static boolean guard_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guard_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!guard_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "guard_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' exprs
  private static boolean guard_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guard_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && exprs(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // <<guarded guard>>
  static boolean guard_with_mode(PsiBuilder b, int l) {
    return guarded(b, l + 1, guard_parser_);
  }

  /* ********************************************************** */
  // <<enterMode "GUARD">> (<<p1>> <<exitMode "GUARD">>|<<exitMode "GUARD">> !())
  static boolean guarded(PsiBuilder b, int l, final Parser _p1) {
    if (!recursion_guard_(b, l, "guarded")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = enterMode(b, l + 1, "GUARD");
    r = r && guarded_1(b, l + 1, _p1);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<p1>> <<exitMode "GUARD">>|<<exitMode "GUARD">> !()
  private static boolean guarded_1(PsiBuilder b, int l, final Parser _p1) {
    if (!recursion_guard_(b, l, "guarded_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = guarded_1_0(b, l + 1, _p1);
    if (!r) r = guarded_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<p1>> <<exitMode "GUARD">>
  private static boolean guarded_1_0(PsiBuilder b, int l, final Parser _p1) {
    if (!recursion_guard_(b, l, "guarded_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = _p1.parse(b, l);
    r = r && exitMode(b, l + 1, "GUARD");
    exit_section_(b, m, null, r);
    return r;
  }

  // <<exitMode "GUARD">> !()
  private static boolean guarded_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guarded_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exitMode(b, l + 1, "GUARD");
    r = r && guarded_1_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !()
  private static boolean guarded_1_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "guarded_1_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !guarded_1_1_1_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // ()
  private static boolean guarded_1_1_1_0(PsiBuilder b, int l) {
    return true;
  }

  /* ********************************************************** */
  // guard_with_mode clause_body
  public static boolean if_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_clause")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<if clause>");
    r = guard_with_mode(b, l + 1);
    p = r; // pin = 1
    r = r && clause_body(b, l + 1);
    exit_section_(b, l, m, ERL_IF_CLAUSE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // if_clause (';' if_clause)*
  static boolean if_clauses(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_clauses")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = if_clause(b, l + 1);
    p = r; // pin = 1
    r = r && if_clauses_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (';' if_clause)*
  private static boolean if_clauses_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_clauses_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!if_clauses_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "if_clauses_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' if_clause
  private static boolean if_clauses_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_clauses_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && if_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // if if_clauses end
  public static boolean if_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_expression")) return false;
    if (!nextTokenIs(b, ERL_IF)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_IF);
    p = r; // pin = 1
    r = r && report_error_(b, if_clauses(b, l + 1));
    r = p && consumeToken(b, ERL_END) && r;
    exit_section_(b, l, m, ERL_IF_EXPRESSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // &('ifdef'|'ifndef'|'undef') atom_name '(' macros_name ')'
  public static boolean ifdef_ifndef_undef_attribute(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ifdef_ifndef_undef_attribute")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<attribute>");
    r = ifdef_ifndef_undef_attribute_0(b, l + 1);
    r = r && consumeToken(b, ERL_ATOM_NAME);
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, macros_name(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_ATOM_ATTRIBUTE, r, p, null);
    return r || p;
  }

  // &('ifdef'|'ifndef'|'undef')
  private static boolean ifdef_ifndef_undef_attribute_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ifdef_ifndef_undef_attribute_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = ifdef_ifndef_undef_attribute_0_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // 'ifdef'|'ifndef'|'undef'
  private static boolean ifdef_ifndef_undef_attribute_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "ifdef_ifndef_undef_attribute_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "ifdef");
    if (!r) r = consumeToken(b, "ifndef");
    if (!r) r = consumeToken(b, "undef");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // 'import' '(' module_ref ',' import_functions ')'
  public static boolean import_directive(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_directive")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<import directive>");
    r = consumeToken(b, "import");
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, module_ref(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, ERL_COMMA)) && r;
    r = p && report_error_(b, import_functions(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_IMPORT_DIRECTIVE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean import_function(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_function")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<import function>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_OP_AR_DIV));
    r = p && consumeToken(b, ERL_INTEGER) && r;
    exit_section_(b, l, m, ERL_IMPORT_FUNCTION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // import_function (',' import_function)*
  static boolean import_function_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_function_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = import_function(b, l + 1);
    p = r; // pin = 1
    r = r && import_function_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' import_function)*
  private static boolean import_function_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_function_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!import_function_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "import_function_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' import_function
  private static boolean import_function_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_function_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && import_function(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '[' import_function_list? ']'
  public static boolean import_functions(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_functions")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    r = r && import_functions_1(b, l + 1);
    r = r && consumeToken(b, ERL_BRACKET_RIGHT);
    exit_section_(b, m, ERL_IMPORT_FUNCTIONS, r);
    return r;
  }

  // import_function_list?
  private static boolean import_functions_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "import_functions_1")) return false;
    import_function_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '-' 'include' '(' include_string ')'
  public static boolean include(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "include")) return false;
    if (!nextTokenIs(b, ERL_OP_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    r = r && consumeToken(b, "include");
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, include_string(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_INCLUDE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '-' 'include_lib' '(' include_string ')'
  public static boolean include_lib(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "include_lib")) return false;
    if (!nextTokenIs(b, ERL_OP_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    r = r && consumeToken(b, "include_lib");
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, include_string(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_INCLUDE_LIB, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // string
  public static boolean include_string(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "include_string")) return false;
    if (!nextTokenIs(b, ERL_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_STRING);
    exit_section_(b, m, ERL_INCLUDE_STRING, r);
    return r;
  }

  /* ********************************************************** */
  // '-'? (integer | macros argument_list?)
  public static boolean int_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "int_type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<type>");
    r = int_type_0(b, l + 1);
    r = r && int_type_1(b, l + 1);
    exit_section_(b, l, m, ERL_INT_TYPE, r, false, null);
    return r;
  }

  // '-'?
  private static boolean int_type_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "int_type_0")) return false;
    consumeToken(b, ERL_OP_MINUS);
    return true;
  }

  // integer | macros argument_list?
  private static boolean int_type_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "int_type_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_INTEGER);
    if (!r) r = int_type_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // macros argument_list?
  private static boolean int_type_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "int_type_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = macros(b, l + 1);
    r = r && int_type_1_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // argument_list?
  private static boolean int_type_1_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "int_type_1_1_1")) return false;
    argument_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // &<<isApplicationLanguage>>
  static boolean is_app_config(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "is_app_config")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = isApplicationLanguage(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // &<<isConsole>>
  static boolean is_console(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "is_console")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = isConsole(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // [ lc_generator_definition ('<-' | '<=')] expression
  public static boolean lc_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<expression>");
    r = lc_expression_0(b, l + 1);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, l, m, ERL_LC_EXPRESSION, r, false, null);
    return r;
  }

  // [ lc_generator_definition ('<-' | '<=')]
  private static boolean lc_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_expression_0")) return false;
    lc_expression_0_0(b, l + 1);
    return true;
  }

  // lc_generator_definition ('<-' | '<=')
  private static boolean lc_expression_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_expression_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = lc_generator_definition(b, l + 1);
    r = r && lc_expression_0_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '<-' | '<='
  private static boolean lc_expression_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_expression_0_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_LT_MINUS);
    if (!r) r = consumeToken(b, ERL_OP_LT_EQ);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // lc_expression (',' lc_expression)*
  public static boolean lc_exprs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_exprs")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<lc exprs>");
    r = lc_expression(b, l + 1);
    p = r; // pin = 1
    r = r && lc_exprs_1(b, l + 1);
    exit_section_(b, l, m, ERL_LC_EXPRS, r, p, null);
    return r || p;
  }

  // (',' lc_expression)*
  private static boolean lc_exprs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_exprs_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!lc_exprs_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "lc_exprs_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' lc_expression
  private static boolean lc_exprs_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_exprs_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && lc_expression(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // map_match | expression
  public static boolean lc_generator_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "lc_generator_definition")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<lc generator definition>");
    r = map_match(b, l + 1);
    if (!r) r = expression(b, l + 1, -1);
    exit_section_(b, l, m, ERL_ARGUMENT_DEFINITION, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // config_call_expression | qualified_expression+
  static boolean left_accessors(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "left_accessors")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = config_call_expression(b, l + 1);
    if (!r) r = left_accessors_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // qualified_expression+
  private static boolean left_accessors_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "left_accessors_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qualified_expression(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!qualified_expression(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "left_accessors_1", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '[' expression '||' lc_exprs ']'
  public static boolean list_comprehension(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_comprehension")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    r = r && expression(b, l + 1, -1);
    r = r && consumeToken(b, ERL_OR_OR);
    p = r; // pin = 3
    r = r && report_error_(b, lc_exprs(b, l + 1));
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, ERL_LIST_COMPREHENSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '[' (']' | expression not_comprehension tail)
  public static boolean list_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_expression")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    r = r && list_expression_1(b, l + 1);
    exit_section_(b, m, ERL_LIST_EXPRESSION, r);
    return r;
  }

  // ']' | expression not_comprehension tail
  private static boolean list_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_expression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_BRACKET_RIGHT);
    if (!r) r = list_expression_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // expression not_comprehension tail
  private static boolean list_expression_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_expression_1_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = expression(b, l + 1, -1);
    r = r && not_comprehension(b, l + 1);
    p = r; // pin = not_comprehension
    r = r && tail(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '++' | '--'
  static boolean list_op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "list_op")) return false;
    if (!nextTokenIs(b, "", ERL_OP_PLUS_PLUS, ERL_OP_MINUS_MINUS)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_PLUS_PLUS);
    if (!r) r = consumeToken(b, ERL_OP_MINUS_MINUS);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // <<macroCallArgumentsList>>
  public static boolean macro_call_argument_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macro_call_argument_list")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<macro call argument list>");
    r = macroCallArgumentsList(b, l + 1);
    exit_section_(b, l, m, ERL_MACRO_CALL_ARGUMENT_LIST, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // &('?') macros &not_function_definition
  public static boolean macro_form(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macro_form")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<macro form>");
    r = macro_form_0(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, macros(b, l + 1));
    r = r && macro_form_2(b, l + 1);
    exit_section_(b, l, m, ERL_MACRO_FORM, r, p, macro_form_recover_parser_);
    return r || p;
  }

  // &('?')
  private static boolean macro_form_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macro_form_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = macro_form_0_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // ('?')
  private static boolean macro_form_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macro_form_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_QMARK);
    exit_section_(b, m, null, r);
    return r;
  }

  // &not_function_definition
  private static boolean macro_form_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macro_form_2")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = not_function_definition(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // !'.'
  static boolean macro_form_recover(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macro_form_recover")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, ERL_DOT);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '?' &<<recordMacroNameTokenSubstitutionDepth>> macros_name macro_call_argument_list?
  public static boolean macros(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros")) return false;
    if (!nextTokenIs(b, ERL_QMARK)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_QMARK);
    p = r; // pin = 1
    r = r && macros_1(b, l + 1);
    r = r && macros_name(b, l + 1);
    r = r && macros_3(b, l + 1);
    exit_section_(b, l, m, ERL_MACROS, r, p, null);
    return r || p;
  }

  // &<<recordMacroNameTokenSubstitutionDepth>>
  private static boolean macros_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = recordMacroNameTokenSubstitutionDepth(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // macro_call_argument_list?
  private static boolean macros_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_3")) return false;
    macro_call_argument_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '?''?' q_var
  public static boolean macros_arg(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_arg")) return false;
    if (!nextTokenIs(b, ERL_QMARK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_QMARK);
    r = r && consumeToken(b, ERL_QMARK);
    r = r && q_var(b, l + 1);
    exit_section_(b, m, ERL_MACROS_ARG, r);
    return r;
  }

  /* ********************************************************** */
  // <<consumeMacroBody>>
  public static boolean macros_body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_body")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<macros body>");
    r = consumeMacroBody(b, l + 1);
    exit_section_(b, l, m, ERL_MACROS_BODY, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '-' 'define' '(' macros_name argument_definition_list? ',' <<guarded macros_body>> ')'
  public static boolean macros_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_definition")) return false;
    if (!nextTokenIs(b, ERL_OP_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    r = r && consumeToken(b, "define");
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, macros_name(b, l + 1)) && r;
    r = p && report_error_(b, macros_definition_4(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, ERL_COMMA)) && r;
    r = p && report_error_(b, guarded(b, l + 1, macros_body_parser_)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_MACROS_DEFINITION, r, p, null);
    return r || p;
  }

  // argument_definition_list?
  private static boolean macros_definition_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_definition_4")) return false;
    argument_definition_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // atom | var
  public static boolean macros_name(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "macros_name")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<macros name>");
    r = atom(b, l + 1);
    if (!r) r = consumeToken(b, ERL_VAR);
    exit_section_(b, l, m, ERL_MACROS_NAME, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // expression '=>' expression
  static boolean map_assoc(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_assoc")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = expression(b, l + 1, -1);
    r = r && consumeToken(b, ERL_ASSOC);
    p = r; // pin = 2
    r = r && expression(b, l + 1, -1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '#' '{' map_assoc '||' lc_exprs '}'
  public static boolean map_comprehension(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_comprehension")) return false;
    if (!nextTokenIs(b, ERL_RADIX)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_RADIX);
    r = r && consumeToken(b, ERL_CURLY_LEFT);
    r = r && map_assoc(b, l + 1);
    r = r && consumeToken(b, ERL_OR_OR);
    p = r; // pin = 4
    r = r && report_error_(b, lc_exprs(b, l + 1));
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_LIST_COMPREHENSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // map_tuple
  public static boolean map_construct_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_construct_expression")) return false;
    if (!nextTokenIs(b, ERL_RADIX)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = map_tuple(b, l + 1);
    exit_section_(b, m, ERL_MAP_EXPRESSION, r);
    return r;
  }

  /* ********************************************************** */
  // map_entry (',' map_entry)*
  static boolean map_entries(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entries")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = map_entry(b, l + 1);
    p = r; // pin = 1
    r = r && map_entries_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' map_entry)*
  private static boolean map_entries_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entries_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!map_entries_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "map_entries_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' map_entry
  private static boolean map_entries_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entries_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && map_entry(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // expression (('=>' | ':=') expression)?
  public static boolean map_entry(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<map entry>");
    r = expression(b, l + 1, -1);
    p = r; // pin = 1
    r = r && map_entry_1(b, l + 1);
    exit_section_(b, l, m, ERL_MAP_ENTRY, r, p, tuple_recoverer_parser_);
    return r || p;
  }

  // (('=>' | ':=') expression)?
  private static boolean map_entry_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_1")) return false;
    map_entry_1_0(b, l + 1);
    return true;
  }

  // ('=>' | ':=') expression
  private static boolean map_entry_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = map_entry_1_0_0(b, l + 1);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '=>' | ':='
  private static boolean map_entry_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_ASSOC);
    if (!r) r = consumeToken(b, ERL_MATCH);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // top_type '=>' top_type
  public static boolean map_entry_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_type")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<type>");
    r = top_type(b, l + 1);
    r = r && consumeToken(b, ERL_ASSOC);
    p = r; // pin = 2
    r = r && top_type(b, l + 1);
    exit_section_(b, l, m, ERL_MAP_ENTRY_TYPE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // map_entry_type (',' map_entry_type)*
  static boolean map_entry_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_type_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = map_entry_type(b, l + 1);
    p = r; // pin = 1
    r = r && map_entry_type_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' map_entry_type)*
  private static boolean map_entry_type_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_type_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!map_entry_type_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "map_entry_type_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' map_entry_type
  private static boolean map_entry_type_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_entry_type_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && map_entry_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // expression ':=' expression
  static boolean map_match(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_match")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = expression(b, l + 1, -1);
    r = r && consumeToken(b, ERL_MATCH);
    p = r; // pin = 2
    r = r && expression(b, l + 1, -1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '#' '{' map_entries? '}'
  public static boolean map_tuple(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_tuple")) return false;
    if (!nextTokenIs(b, ERL_RADIX)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_RADIX);
    r = r && consumeToken(b, ERL_CURLY_LEFT);
    p = r; // pin = 2
    r = r && report_error_(b, map_tuple_2(b, l + 1));
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_MAP_TUPLE, r, p, null);
    return r || p;
  }

  // map_entries?
  private static boolean map_tuple_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_tuple_2")) return false;
    map_entries(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '#' '{' map_entry_type_list? '}'
  public static boolean map_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_type")) return false;
    if (!nextTokenIs(b, ERL_RADIX)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_RADIX);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_CURLY_LEFT));
    r = p && report_error_(b, map_type_2(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_MAP_TYPE, r, p, null);
    return r || p;
  }

  // map_entry_type_list?
  private static boolean map_type_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "map_type_2")) return false;
    map_entry_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // function_clause_head clause_guard? clause_body
  static boolean maybe_macro_funciton_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "maybe_macro_funciton_clause")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = function_clause_head(b, l + 1);
    r = r && maybe_macro_funciton_clause_1(b, l + 1);
    r = r && clause_body(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // clause_guard?
  private static boolean maybe_macro_funciton_clause_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "maybe_macro_funciton_clause_1")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // q_var ['::' top_type]
  public static boolean model_field(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "model_field")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = q_var(b, l + 1);
    p = r; // pin = 1
    r = r && model_field_1(b, l + 1);
    exit_section_(b, l, m, ERL_ARGUMENT_DEFINITION, r, p, null);
    return r || p;
  }

  // ['::' top_type]
  private static boolean model_field_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "model_field_1")) return false;
    model_field_1_0(b, l + 1);
    return true;
  }

  // '::' top_type
  private static boolean model_field_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "model_field_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_COLON_COLON);
    r = r && top_type(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '[' model_field (',' model_field)* ']'
  public static boolean model_field_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "model_field_list")) return false;
    if (!nextTokenIs(b, ERL_BRACKET_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, model_field(b, l + 1));
    r = p && report_error_(b, model_field_list_2(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, ERL_MODEL_FIELD_LIST, r, p, null);
    return r || p;
  }

  // (',' model_field)*
  private static boolean model_field_list_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "model_field_list_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!model_field_list_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "model_field_list_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' model_field
  private static boolean model_field_list_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "model_field_list_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && model_field(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // 'module' '(' q_atom [',' module_tail] ')'
  public static boolean module(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<module>");
    r = consumeToken(b, "module");
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, q_atom(b, l + 1)) && r;
    r = p && report_error_(b, module_3(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_MODULE, r, p, null);
    return r || p;
  }

  // [',' module_tail]
  private static boolean module_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_3")) return false;
    module_3_0(b, l + 1);
    return true;
  }

  // ',' module_tail
  private static boolean module_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_COMMA);
    r = r && module_tail(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean module_ref(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_ref")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<module ref>");
    r = q_atom(b, l + 1);
    exit_section_(b, l, m, ERL_MODULE_REF, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // model_field_list | argument_definition
  static boolean module_tail(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "module_tail")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = model_field_list(b, l + 1);
    if (!r) r = argument_definition(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '/' |'*' | div | rem  | band | and
  static boolean mult_op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "mult_op")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_AR_DIV);
    if (!r) r = consumeToken(b, ERL_OP_AR_MUL);
    if (!r) r = consumeToken(b, ERL_DIV);
    if (!r) r = consumeToken(b, ERL_REM);
    if (!r) r = consumeToken(b, ERL_BAND);
    if (!r) r = consumeToken(b, ERL_AND);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // !'||'
  static boolean not_comprehension(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "not_comprehension")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, "<expression>");
    r = !consumeToken(b, ERL_OR_OR);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // <<isModeOn "GUARD">> | !('->'|when)
  static boolean not_function_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "not_function_definition")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = isModeOn(b, l + 1, "GUARD");
    if (!r) r = not_function_definition_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !('->'|when)
  private static boolean not_function_definition_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "not_function_definition_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !not_function_definition_1_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // '->'|when
  private static boolean not_function_definition_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "not_function_definition_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_ARROW);
    if (!r) r = consumeToken(b, ERL_WHEN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // &'on_load' atom_name '(' function_with_arity ')'
  public static boolean on_load(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "on_load")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<on load>");
    r = on_load_0(b, l + 1);
    r = r && consumeToken(b, ERL_ATOM_NAME);
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, function_with_arity(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_ATOM_ATTRIBUTE, r, p, null);
    return r || p;
  }

  // &'on_load'
  private static boolean on_load_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "on_load_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = consumeToken(b, "on_load");
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '/' bit_type_list
  public static boolean opt_bit_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "opt_bit_type_list")) return false;
    if (!nextTokenIs(b, ERL_OP_AR_DIV)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_AR_DIV);
    p = r; // pin = 1
    r = r && bit_type_list(b, l + 1);
    exit_section_(b, l, m, ERL_OPT_BIT_TYPE_LIST, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '.' | <<isModeOn "ELSE">> <<exitMode "ELSE">>
  static boolean period(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "period")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _LEFT_INNER_, null);
    r = consumeToken(b, ERL_DOT);
    if (!r) r = period_1(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // <<isModeOn "ELSE">> <<exitMode "ELSE">>
  private static boolean period_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "period_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = isModeOn(b, l + 1, "ELSE");
    r = r && exitMode(b, l + 1, "ELSE");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // &(!'?') function_clause_head clause_guard? clause_body
  static boolean plain_function_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plain_function_clause")) return false;
    if (!nextTokenIs(b, "", ERL_ATOM_NAME, ERL_SINGLE_QUOTE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = plain_function_clause_0(b, l + 1);
    r = r && function_clause_head(b, l + 1);
    p = r; // pin = 2
    r = r && report_error_(b, plain_function_clause_2(b, l + 1));
    r = p && clause_body(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // &(!'?')
  private static boolean plain_function_clause_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plain_function_clause_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = plain_function_clause_0_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // !'?'
  private static boolean plain_function_clause_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plain_function_clause_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, ERL_QMARK);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // clause_guard?
  private static boolean plain_function_clause_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plain_function_clause_2")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '+' | '-' | bnot | not
  static boolean prefix_op(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "prefix_op")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_OP_PLUS);
    if (!r) r = consumeToken(b, ERL_OP_MINUS);
    if (!r) r = consumeToken(b, ERL_BNOT);
    if (!r) r = consumeToken(b, ERL_NOT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // atom | macros_arg | macros
  public static boolean q_atom(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "q_atom")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<q atom>");
    r = atom(b, l + 1);
    if (!r) r = macros_arg(b, l + 1);
    if (!r) r = macros(b, l + 1);
    exit_section_(b, l, m, ERL_Q_ATOM, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // q_atom | q_var
  static boolean q_atom_or_var(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "q_atom_or_var")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom(b, l + 1);
    if (!r) r = q_var(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // var
  public static boolean q_var(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "q_var")) return false;
    if (!nextTokenIs(b, ERL_VAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_VAR);
    exit_section_(b, m, ERL_Q_VAR, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom '.' q_atom
  static boolean qualified_atom_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qualified_atom_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = q_atom(b, l + 1);
    r = r && consumeToken(b, ERL_DOT);
    r = r && q_atom(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // receive (after_clause | cr_clauses after_clause? ) end
  public static boolean receive_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "receive_expression")) return false;
    if (!nextTokenIs(b, ERL_RECEIVE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_RECEIVE);
    p = r; // pin = 1
    r = r && report_error_(b, receive_expression_1(b, l + 1));
    r = p && consumeToken(b, ERL_END) && r;
    exit_section_(b, l, m, ERL_RECEIVE_EXPRESSION, r, p, null);
    return r || p;
  }

  // after_clause | cr_clauses after_clause?
  private static boolean receive_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "receive_expression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = after_clause(b, l + 1);
    if (!r) r = receive_expression_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // cr_clauses after_clause?
  private static boolean receive_expression_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "receive_expression_1_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = cr_clauses(b, l + 1);
    p = r; // pin = 1
    r = r && receive_expression_1_1_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // after_clause?
  private static boolean receive_expression_1_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "receive_expression_1_1_1")) return false;
    after_clause(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '-' 'record' '(' q_atom ',' typed_record_fields ')'
  public static boolean record_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_definition")) return false;
    if (!nextTokenIs(b, ERL_OP_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    r = r && consumeToken(b, "record");
    p = r; // pin = 2
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, q_atom(b, l + 1)) && r;
    r = p && report_error_(b, consumeToken(b, ERL_COMMA)) && r;
    r = p && report_error_(b, typed_record_fields(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, ERL_RECORD_DEFINITION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // (q_atom | '_') '=' (qualified_atom_expression | expression)
  public static boolean record_field(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_field")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<record field>");
    r = record_field_0(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_OP_EQ));
    r = p && record_field_2(b, l + 1) && r;
    exit_section_(b, l, m, ERL_RECORD_FIELD, r, p, tuple_recoverer_parser_);
    return r || p;
  }

  // q_atom | '_'
  private static boolean record_field_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_field_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom(b, l + 1);
    if (!r) r = consumeToken(b, "_");
    exit_section_(b, m, null, r);
    return r;
  }

  // qualified_atom_expression | expression
  private static boolean record_field_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_field_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = qualified_atom_expression(b, l + 1);
    if (!r) r = expression(b, l + 1, -1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '.' q_atom
  public static boolean record_field_ref(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_field_ref")) return false;
    if (!nextTokenIs(b, ERL_DOT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_DOT);
    p = r; // pin = 1
    r = r && q_atom(b, l + 1);
    exit_section_(b, l, m, ERL_RECORD_FIELD, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // record_field (',' record_field)*
  static boolean record_fields(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_fields")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = record_field(b, l + 1);
    p = r; // pin = 1
    r = r && record_fields_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' record_field)*
  private static boolean record_fields_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_fields_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!record_fields_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "record_fields_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' record_field
  private static boolean record_fields_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_fields_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && record_field(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '#' &(!'{')
  static boolean record_hash(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_hash")) return false;
    if (!nextTokenIs(b, ERL_RADIX)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_RADIX);
    r = r && record_hash_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &(!'{')
  private static boolean record_hash_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_hash_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = record_hash_1_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // !'{'
  private static boolean record_hash_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_hash_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, ERL_CURLY_LEFT);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // record_head_classic | macros &('{'|'.' q_atom &(!('(')))
  static boolean record_head(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head")) return false;
    if (!nextTokenIs(b, "", ERL_RADIX, ERL_QMARK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = record_head_classic(b, l + 1);
    if (!r) r = record_head_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // macros &('{'|'.' q_atom &(!('(')))
  private static boolean record_head_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = macros(b, l + 1);
    r = r && record_head_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &('{'|'.' q_atom &(!('(')))
  private static boolean record_head_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = record_head_1_1_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // '{'|'.' q_atom &(!('('))
  private static boolean record_head_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_CURLY_LEFT);
    if (!r) r = record_head_1_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '.' q_atom &(!('('))
  private static boolean record_head_1_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1_1_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_DOT);
    r = r && q_atom(b, l + 1);
    r = r && record_head_1_1_0_1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &(!('('))
  private static boolean record_head_1_1_0_1_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1_1_0_1_2")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = record_head_1_1_0_1_2_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // !('(')
  private static boolean record_head_1_1_0_1_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1_1_0_1_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !record_head_1_1_0_1_2_0_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // ('(')
  private static boolean record_head_1_1_0_1_2_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_1_1_0_1_2_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_LEFT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // record_hash record_ref
  static boolean record_head_classic(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_head_classic")) return false;
    if (!nextTokenIs(b, ERL_RADIX)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = record_hash(b, l + 1);
    p = r; // pin = 1
    r = r && record_ref(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '{' top_type_list? '}'
  public static boolean record_like_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_like_type")) return false;
    if (!nextTokenIs(b, ERL_CURLY_LEFT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_CURLY_LEFT);
    r = r && record_like_type_1(b, l + 1);
    r = r && consumeToken(b, ERL_CURLY_RIGHT);
    exit_section_(b, m, ERL_RECORD_LIKE_TYPE, r);
    return r;
  }

  // top_type_list?
  private static boolean record_like_type_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_like_type_1")) return false;
    top_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean record_ref(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_ref")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<record ref>");
    r = q_atom(b, l + 1);
    exit_section_(b, l, m, ERL_RECORD_REF, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // record_head (record_field_ref | record_tuple | ())
  static boolean record_tail(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_tail")) return false;
    if (!nextTokenIs(b, "", ERL_RADIX, ERL_QMARK)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = record_head(b, l + 1);
    p = r; // pin = 1
    r = r && record_tail_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // record_field_ref | record_tuple | ()
  private static boolean record_tail_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_tail_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = record_field_ref(b, l + 1);
    if (!r) r = record_tuple(b, l + 1);
    if (!r) r = record_tail_1_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ()
  private static boolean record_tail_1_2(PsiBuilder b, int l) {
    return true;
  }

  /* ********************************************************** */
  // '{' record_fields? '}'
  public static boolean record_tuple(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_tuple")) return false;
    if (!nextTokenIs(b, ERL_CURLY_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_CURLY_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, record_tuple_1(b, l + 1));
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_RECORD_TUPLE, r, p, null);
    return r || p;
  }

  // record_fields?
  private static boolean record_tuple_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record_tuple_1")) return false;
    record_fields(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // rule_clause (';' rule_clause)*
  public static boolean rule(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<rule>");
    r = rule_clause(b, l + 1);
    p = r; // pin = 1
    r = r && rule_1(b, l + 1);
    exit_section_(b, l, m, ERL_RULE, r, p, null);
    return r || p;
  }

  // (';' rule_clause)*
  private static boolean rule_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!rule_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "rule_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' rule_clause
  private static boolean rule_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && rule_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // ':-' lc_exprs
  public static boolean rule_body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_body")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<rule body>");
    r = consumeToken(b, ":-");
    p = r; // pin = 1
    r = r && lc_exprs(b, l + 1);
    exit_section_(b, l, m, ERL_RULE_BODY, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // q_atom argument_list clause_guard? rule_body
  public static boolean rule_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_clause")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<rule clause>");
    r = q_atom(b, l + 1);
    r = r && argument_list(b, l + 1);
    r = r && rule_clause_2(b, l + 1);
    r = r && rule_body(b, l + 1);
    exit_section_(b, l, m, ERL_RULE_CLAUSE, r, false, null);
    return r;
  }

  // clause_guard?
  private static boolean rule_clause_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_clause_2")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom ('/' integer | &('('))
  public static boolean spec_fun(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "spec_fun")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<spec fun>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && spec_fun_1(b, l + 1);
    exit_section_(b, l, m, ERL_SPEC_FUN, r, p, null);
    return r || p;
  }

  // '/' integer | &('(')
  private static boolean spec_fun_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "spec_fun_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = spec_fun_1_0(b, l + 1);
    if (!r) r = spec_fun_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '/' integer
  private static boolean spec_fun_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "spec_fun_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_AR_DIV);
    p = r; // pin = 1
    r = r && consumeToken(b, ERL_INTEGER);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // &('(')
  private static boolean spec_fun_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "spec_fun_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = spec_fun_1_1_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // ('(')
  private static boolean spec_fun_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "spec_fun_1_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_PAR_LEFT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // 'spec' type_spec
  public static boolean specification(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "specification")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<specification>");
    r = consumeToken(b, "spec");
    p = r; // pin = 1
    r = r && type_spec(b, l + 1);
    exit_section_(b, l, m, ERL_SPECIFICATION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // string
  public static boolean string_literal(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "string_literal")) return false;
    if (!nextTokenIs(b, ERL_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_STRING);
    exit_section_(b, m, ERL_STRING_LITERAL, r);
    return r;
  }

  /* ********************************************************** */
  // ']' | '|' expression ']' | ',' expression tail
  static boolean tail(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tail")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_BRACKET_RIGHT);
    if (!r) r = tail_1(b, l + 1);
    if (!r) r = tail_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '|' expression ']'
  private static boolean tail_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tail_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_OR);
    p = r; // pin = 1
    r = r && report_error_(b, expression(b, l + 1, -1));
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ',' expression tail
  private static boolean tail_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tail_2")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && report_error_(b, expression(b, l + 1, -1));
    r = p && tail(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // [q_var '::'] type_list
  public static boolean top_type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<type>");
    r = top_type_0(b, l + 1);
    r = r && type_list(b, l + 1);
    exit_section_(b, l, m, ERL_TOP_TYPE, r, false, null);
    return r;
  }

  // [q_var '::']
  private static boolean top_type_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type_0")) return false;
    top_type_0_0(b, l + 1);
    return true;
  }

  // q_var '::'
  private static boolean top_type_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_var(b, l + 1);
    r = r && consumeToken(b, ERL_COLON_COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '->' top_type
  public static boolean top_type_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type_clause")) return false;
    if (!nextTokenIs(b, ERL_ARROW)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_ARROW);
    p = r; // pin = 1
    r = r && top_type(b, l + 1);
    exit_section_(b, l, m, ERL_TOP_TYPE_CLAUSE, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // top_type (',' top_type)*
  static boolean top_type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = top_type(b, l + 1);
    p = r; // pin = 1
    r = r && top_type_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' top_type)*
  private static boolean top_type_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!top_type_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "top_type_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' top_type
  private static boolean top_type_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "top_type_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && top_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // [argument_definition ':'] argument_definition
  static boolean try_argument_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_argument_definition")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = try_argument_definition_0(b, l + 1);
    r = r && argument_definition(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [argument_definition ':']
  private static boolean try_argument_definition_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_argument_definition_0")) return false;
    try_argument_definition_0_0(b, l + 1);
    return true;
  }

  // argument_definition ':'
  private static boolean try_argument_definition_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_argument_definition_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = argument_definition(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // catch try_clauses [after try_expressions_clause] | after try_expressions_clause
  public static boolean try_catch(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_catch")) return false;
    if (!nextTokenIs(b, "<try catch>", ERL_AFTER, ERL_CATCH)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<try catch>");
    r = try_catch_0(b, l + 1);
    if (!r) r = try_catch_1(b, l + 1);
    exit_section_(b, l, m, ERL_TRY_CATCH, r, false, null);
    return r;
  }

  // catch try_clauses [after try_expressions_clause]
  private static boolean try_catch_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_catch_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_CATCH);
    p = r; // pin = catch|after
    r = r && report_error_(b, try_clauses(b, l + 1));
    r = p && try_catch_0_2(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [after try_expressions_clause]
  private static boolean try_catch_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_catch_0_2")) return false;
    try_catch_0_2_0(b, l + 1);
    return true;
  }

  // after try_expressions_clause
  private static boolean try_catch_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_catch_0_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_AFTER);
    p = r; // pin = catch|after
    r = r && try_expressions_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // after try_expressions_clause
  private static boolean try_catch_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_catch_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_AFTER);
    p = r; // pin = catch|after
    r = r && try_expressions_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // <<guarded try_argument_definition>> clause_guard? clause_body
  public static boolean try_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_clause")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<try clause>");
    r = guarded(b, l + 1, try_argument_definition_parser_);
    p = r; // pin = 1
    r = r && report_error_(b, try_clause_1(b, l + 1));
    r = p && clause_body(b, l + 1) && r;
    exit_section_(b, l, m, ERL_TRY_CLAUSE, r, p, null);
    return r || p;
  }

  // clause_guard?
  private static boolean try_clause_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_clause_1")) return false;
    clause_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // try_clause (';' try_clause)*
  public static boolean try_clauses(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_clauses")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<try clauses>");
    r = try_clause(b, l + 1);
    p = r; // pin = 1
    r = r && try_clauses_1(b, l + 1);
    exit_section_(b, l, m, ERL_TRY_CLAUSES, r, p, null);
    return r || p;
  }

  // (';' try_clause)*
  private static boolean try_clauses_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_clauses_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!try_clauses_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "try_clauses_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' try_clause
  private static boolean try_clauses_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_clauses_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && try_clause(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // try try_expressions_clause [of cr_clauses] try_catch end
  public static boolean try_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_expression")) return false;
    if (!nextTokenIs(b, ERL_TRY)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_TRY);
    p = r; // pin = 1
    r = r && report_error_(b, try_expressions_clause(b, l + 1));
    r = p && report_error_(b, try_expression_2(b, l + 1)) && r;
    r = p && report_error_(b, try_catch(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_END) && r;
    exit_section_(b, l, m, ERL_TRY_EXPRESSION, r, p, null);
    return r || p;
  }

  // [of cr_clauses]
  private static boolean try_expression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_expression_2")) return false;
    try_expression_2_0(b, l + 1);
    return true;
  }

  // of cr_clauses
  private static boolean try_expression_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_expression_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OF);
    p = r; // pin = 1
    r = r && cr_clauses(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // exprs
  public static boolean try_expressions_clause(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "try_expressions_clause")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<try expressions clause>");
    r = exprs(b, l + 1);
    exit_section_(b, l, m, ERL_TRY_EXPRESSIONS_CLAUSE, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '{' (&'}' | exprs) '}'
  public static boolean tuple_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_expression")) return false;
    if (!nextTokenIs(b, ERL_CURLY_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_CURLY_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, tuple_expression_1(b, l + 1));
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_TUPLE_EXPRESSION, r, p, null);
    return r || p;
  }

  // &'}' | exprs
  private static boolean tuple_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_expression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tuple_expression_1_0(b, l + 1);
    if (!r) r = exprs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &'}'
  private static boolean tuple_expression_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_expression_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = consumeToken(b, ERL_CURLY_RIGHT);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // !('}'|',')
  static boolean tuple_recoverer(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_recoverer")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !tuple_recoverer_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // '}'|','
  private static boolean tuple_recoverer_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tuple_recoverer_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_CURLY_RIGHT);
    if (!r) r = consumeToken(b, ERL_COMMA);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' top_type ')'
  //   | int_type ['..' int_type]
  //   | fun '(' fun_type_100_t? ')'
  //   | type_ref_with_module ['(' top_type_list? ')']
  //   | binary_type
  //   | q_var ['::' top_type]
  //   | '[' [top_type [',' '...']] ']'
  //   | record_like_type
  //   | record_hash record_ref '{' field_type_list? '}'
  //   | map_type
  public static boolean type(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<type>");
    r = type_0(b, l + 1);
    if (!r) r = type_1(b, l + 1);
    if (!r) r = type_2(b, l + 1);
    if (!r) r = type_3(b, l + 1);
    if (!r) r = binary_type(b, l + 1);
    if (!r) r = type_5(b, l + 1);
    if (!r) r = type_6(b, l + 1);
    if (!r) r = record_like_type(b, l + 1);
    if (!r) r = type_8(b, l + 1);
    if (!r) r = map_type(b, l + 1);
    exit_section_(b, l, m, ERL_TYPE, r, false, null);
    return r;
  }

  // '(' top_type ')'
  private static boolean type_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, top_type(b, l + 1));
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // int_type ['..' int_type]
  private static boolean type_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_1")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = int_type(b, l + 1);
    p = r; // pin = 1
    r = r && type_1_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ['..' int_type]
  private static boolean type_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_1_1")) return false;
    type_1_1_0(b, l + 1);
    return true;
  }

  // '..' int_type
  private static boolean type_1_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_1_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_DOT_DOT);
    p = r; // pin = 1
    r = r && int_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // fun '(' fun_type_100_t? ')'
  private static boolean type_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_2")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_FUN);
    p = r; // pin = 1
    r = r && report_error_(b, consumeToken(b, ERL_PAR_LEFT));
    r = p && report_error_(b, type_2_2(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // fun_type_100_t?
  private static boolean type_2_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_2_2")) return false;
    fun_type_100_t(b, l + 1);
    return true;
  }

  // type_ref_with_module ['(' top_type_list? ')']
  private static boolean type_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_3")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = type_ref_with_module(b, l + 1);
    p = r; // pin = 1
    r = r && type_3_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ['(' top_type_list? ')']
  private static boolean type_3_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_3_1")) return false;
    type_3_1_0(b, l + 1);
    return true;
  }

  // '(' top_type_list? ')'
  private static boolean type_3_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_3_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_PAR_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, type_3_1_0_1(b, l + 1));
    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // top_type_list?
  private static boolean type_3_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_3_1_0_1")) return false;
    top_type_list(b, l + 1);
    return true;
  }

  // q_var ['::' top_type]
  private static boolean type_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_5")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = q_var(b, l + 1);
    p = r; // pin = 1
    r = r && type_5_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ['::' top_type]
  private static boolean type_5_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_5_1")) return false;
    type_5_1_0(b, l + 1);
    return true;
  }

  // '::' top_type
  private static boolean type_5_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_5_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COLON_COLON);
    p = r; // pin = 1
    r = r && top_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // '[' [top_type [',' '...']] ']'
  private static boolean type_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_6")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_BRACKET_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, type_6_1(b, l + 1));
    r = p && consumeToken(b, ERL_BRACKET_RIGHT) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [top_type [',' '...']]
  private static boolean type_6_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_6_1")) return false;
    type_6_1_0(b, l + 1);
    return true;
  }

  // top_type [',' '...']
  private static boolean type_6_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_6_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = top_type(b, l + 1);
    p = r; // pin = 1
    r = r && type_6_1_0_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // [',' '...']
  private static boolean type_6_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_6_1_0_1")) return false;
    type_6_1_0_1_0(b, l + 1);
    return true;
  }

  // ',' '...'
  private static boolean type_6_1_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_6_1_0_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && consumeToken(b, ERL_DOT_DOT_DOT);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // record_hash record_ref '{' field_type_list? '}'
  private static boolean type_8(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_8")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = record_hash(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, record_ref(b, l + 1));
    r = p && report_error_(b, consumeToken(b, ERL_CURLY_LEFT)) && r;
    r = p && report_error_(b, type_8_3(b, l + 1)) && r;
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // field_type_list?
  private static boolean type_8_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_8_3")) return false;
    field_type_list(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom argument_definition_list '::' top_type
  static boolean type_body(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_body")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, argument_definition_list(b, l + 1));
    r = p && report_error_(b, consumeToken(b, ERL_COLON_COLON)) && r;
    r = p && top_type(b, l + 1) && r;
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '-' ('type'|'opaque') <<attribute_tail type_body>>
  public static boolean type_definition(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_definition")) return false;
    if (!nextTokenIs(b, ERL_OP_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_MINUS);
    r = r && type_definition_1(b, l + 1);
    p = r; // pin = 2
    r = r && attribute_tail(b, l + 1, type_body_parser_);
    exit_section_(b, l, m, ERL_TYPE_DEFINITION, r, p, null);
    return r || p;
  }

  // 'type'|'opaque'
  private static boolean type_definition_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_definition_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "type");
    if (!r) r = consumeToken(b, "opaque");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom '(' top_type_list ')' | top_type
  public static boolean type_guard(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_guard")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<type guard>");
    r = type_guard_0(b, l + 1);
    if (!r) r = top_type(b, l + 1);
    exit_section_(b, l, m, ERL_TYPE_GUARD, r, false, null);
    return r;
  }

  // q_atom '(' top_type_list ')'
  private static boolean type_guard_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_guard_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom(b, l + 1);
    r = r && consumeToken(b, ERL_PAR_LEFT);
    r = r && top_type_list(b, l + 1);
    r = r && consumeToken(b, ERL_PAR_RIGHT);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // type_guard (',' type_guard)*
  static boolean type_guard_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_guard_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = type_guard(b, l + 1);
    p = r; // pin = 1
    r = r && type_guard_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' type_guard)*
  private static boolean type_guard_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_guard_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!type_guard_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "type_guard_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' type_guard
  private static boolean type_guard_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_guard_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && type_guard(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // type ('|' type)*
  static boolean type_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_list")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = type(b, l + 1);
    p = r; // pin = 1
    r = r && type_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ('|' type)*
  private static boolean type_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!type_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "type_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '|' type
  private static boolean type_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_OR);
    p = r; // pin = 1
    r = r && type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean type_ref(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_ref")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<type ref>");
    r = q_atom(b, l + 1);
    exit_section_(b, l, m, ERL_TYPE_REF, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // [module_ref ':'] type_ref
  static boolean type_ref_with_module(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_ref_with_module")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = type_ref_with_module_0(b, l + 1);
    r = r && type_ref(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [module_ref ':']
  private static boolean type_ref_with_module_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_ref_with_module_0")) return false;
    type_ref_with_module_0_0(b, l + 1);
    return true;
  }

  // module_ref ':'
  private static boolean type_ref_with_module_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_ref_with_module_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = module_ref(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // fun_type type_sig_guard?
  public static boolean type_sig(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_sig")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = fun_type(b, l + 1);
    p = r; // pin = 1
    r = r && type_sig_1(b, l + 1);
    exit_section_(b, l, m, ERL_TYPE_SIG, r, p, null);
    return r || p;
  }

  // type_sig_guard?
  private static boolean type_sig_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_sig_1")) return false;
    type_sig_guard(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // when type_guard_list
  public static boolean type_sig_guard(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_sig_guard")) return false;
    if (!nextTokenIs(b, ERL_WHEN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_WHEN);
    r = r && type_guard_list(b, l + 1);
    exit_section_(b, m, ERL_TYPE_SIG_GUARD, r);
    return r;
  }

  /* ********************************************************** */
  // type_sig (';' type_sig)*
  static boolean type_sigs_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_sigs_list")) return false;
    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = type_sig(b, l + 1);
    p = r; // pin = 1
    r = r && type_sigs_list_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (';' type_sig)*
  private static boolean type_sigs_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_sigs_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!type_sigs_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "type_sigs_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ';' type_sig
  private static boolean type_sigs_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_sigs_list_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_SEMI);
    p = r; // pin = 1
    r = r && type_sig(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // fun_type_sigs_braces | fun_type_sigs
  static boolean type_spec(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "type_spec")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = fun_type_sigs_braces(b, l + 1);
    if (!r) r = fun_type_sigs(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // expression (',' typed_record_fields | '::' top_type)
  public static boolean typed_attr_val(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_attr_val")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<typed attr val>");
    r = expression(b, l + 1, -1);
    r = r && typed_attr_val_1(b, l + 1);
    exit_section_(b, l, m, ERL_TYPED_ATTR_VAL, r, false, null);
    return r;
  }

  // ',' typed_record_fields | '::' top_type
  private static boolean typed_attr_val_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_attr_val_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = typed_attr_val_1_0(b, l + 1);
    if (!r) r = typed_attr_val_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' typed_record_fields
  private static boolean typed_attr_val_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_attr_val_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_COMMA);
    r = r && typed_record_fields(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '::' top_type
  private static boolean typed_attr_val_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_attr_val_1_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, ERL_COLON_COLON);
    r = r && top_type(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // q_atom ['=' expression] ['::' top_type]
  public static boolean typed_expr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_expr")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<typed expr>");
    r = q_atom(b, l + 1);
    p = r; // pin = 1
    r = r && report_error_(b, typed_expr_1(b, l + 1));
    r = p && typed_expr_2(b, l + 1) && r;
    exit_section_(b, l, m, ERL_TYPED_EXPR, r, p, null);
    return r || p;
  }

  // ['=' expression]
  private static boolean typed_expr_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_expr_1")) return false;
    typed_expr_1_0(b, l + 1);
    return true;
  }

  // '=' expression
  private static boolean typed_expr_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_expr_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_OP_EQ);
    p = r; // pin = 1
    r = r && expression(b, l + 1, -1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // ['::' top_type]
  private static boolean typed_expr_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_expr_2")) return false;
    typed_expr_2_0(b, l + 1);
    return true;
  }

  // '::' top_type
  private static boolean typed_expr_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_expr_2_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COLON_COLON);
    p = r; // pin = 1
    r = r && top_type(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // generic_function_call_expression | typed_expr
  static boolean typed_expr_or_macros(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_expr_or_macros")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = generic_function_call_expression(b, l + 1);
    if (!r) r = typed_expr(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // typed_expr_or_macros (',' typed_expr_or_macros)*
  static boolean typed_exprs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_exprs")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = typed_expr_or_macros(b, l + 1);
    p = r; // pin = 1
    r = r && typed_exprs_1(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  // (',' typed_expr_or_macros)*
  private static boolean typed_exprs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_exprs_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!typed_exprs_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "typed_exprs_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' typed_expr_or_macros
  private static boolean typed_exprs_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_exprs_1_0")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_COMMA);
    p = r; // pin = 1
    r = r && typed_expr_or_macros(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // '{' typed_exprs? '}'
  public static boolean typed_record_fields(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_record_fields")) return false;
    if (!nextTokenIs(b, ERL_CURLY_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, ERL_CURLY_LEFT);
    p = r; // pin = 1
    r = r && report_error_(b, typed_record_fields_1(b, l + 1));
    r = p && consumeToken(b, ERL_CURLY_RIGHT) && r;
    exit_section_(b, l, m, ERL_TYPED_RECORD_FIELDS, r, p, null);
    return r || p;
  }

  // typed_exprs?
  private static boolean typed_record_fields_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "typed_record_fields_1")) return false;
    typed_exprs(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // Expression root: expression
  // Operator priority table:
  // 0: ATOM(catch_expression)
  // 1: BINARY(assignment_expression)
  // 2: BINARY(send_expression)
  // 3: BINARY(orelse_expression)
  // 4: BINARY(andalso_expression)
  // 5: BINARY(comp_op_expression)
  // 6: BINARY(list_op_expression)
  // 7: BINARY(additive_expression)
  // 8: BINARY(multiplicative_expression)
  // 9: ATOM(atom_with_arity_expression)
  // 10: PREFIX(prefix_expression)
  // 11: BINARY(colon_qualified_expression)
  // 12: ATOM(function_call_expression) ATOM(global_function_call_expression) ATOM(generic_function_call_expression) POSTFIX(anonymous_call_expression) POSTFIX(record_expression) ATOM(record2_expression) POSTFIX(map_expression) ATOM(qualified_expression)
  // 13: ATOM(max_expression)
  // 14: PREFIX(parenthesized_expression)
  public static boolean expression(PsiBuilder b, int l, int g) {
    if (!recursion_guard_(b, l, "expression")) return false;
    addVariant(b, "<expression>");
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = catch_expression(b, l + 1);
    if (!r) r = atom_with_arity_expression(b, l + 1);
    if (!r) r = prefix_expression(b, l + 1);
    if (!r) r = function_call_expression(b, l + 1);
    if (!r) r = global_function_call_expression(b, l + 1);
    if (!r) r = generic_function_call_expression(b, l + 1);
    if (!r) r = record2_expression(b, l + 1);
    if (!r) r = qualified_expression(b, l + 1);
    if (!r) r = max_expression(b, l + 1);
    if (!r) r = parenthesized_expression(b, l + 1);
    p = r;
    r = r && expression_0(b, l + 1, g);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  public static boolean expression_0(PsiBuilder b, int l, int g) {
    if (!recursion_guard_(b, l, "expression_0")) return false;
    boolean r = true;
    while (true) {
      Marker m = enter_section_(b, l, _LEFT_, null);
      if (g < 1 && consumeTokenSmart(b, ERL_OP_EQ)) {
        r = expression(b, l, 0);
        exit_section_(b, l, m, ERL_ASSIGNMENT_EXPRESSION, r, true, null);
      }
      else if (g < 2 && consumeTokenSmart(b, ERL_OP_EXL)) {
        r = expression(b, l, 1);
        exit_section_(b, l, m, ERL_SEND_EXPRESSION, r, true, null);
      }
      else if (g < 3 && consumeTokenSmart(b, ERL_ORELSE)) {
        r = expression(b, l, 3);
        exit_section_(b, l, m, ERL_ORELSE_EXPRESSION, r, true, null);
      }
      else if (g < 4 && consumeTokenSmart(b, ERL_ANDALSO)) {
        r = expression(b, l, 4);
        exit_section_(b, l, m, ERL_ANDALSO_EXPRESSION, r, true, null);
      }
      else if (g < 5 && comp_op(b, l + 1)) {
        r = expression(b, l, 5);
        exit_section_(b, l, m, ERL_COMP_OP_EXPRESSION, r, true, null);
      }
      else if (g < 6 && list_op(b, l + 1)) {
        r = expression(b, l, 5);
        exit_section_(b, l, m, ERL_LIST_OP_EXPRESSION, r, true, null);
      }
      else if (g < 7 && add_op(b, l + 1)) {
        r = expression(b, l, 7);
        exit_section_(b, l, m, ERL_ADDITIVE_EXPRESSION, r, true, null);
      }
      else if (g < 8 && multiplicative_expression_0(b, l + 1)) {
        r = expression(b, l, 8);
        exit_section_(b, l, m, ERL_MULTIPLICATIVE_EXPRESSION, r, true, null);
      }
      else if (g < 11 && consumeTokenSmart(b, ERL_COLON)) {
        r = expression(b, l, 11);
        exit_section_(b, l, m, ERL_COLON_QUALIFIED_EXPRESSION, r, true, null);
      }
      else if (g < 12 && anonymous_call_expression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, ERL_ANONYMOUS_CALL_EXPRESSION, r, true, null);
      }
      else if (g < 12 && record_tail(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, ERL_RECORD_EXPRESSION, r, true, null);
      }
      else if (g < 12 && map_tuple(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, ERL_MAP_EXPRESSION, r, true, null);
      }
      else {
        exit_section_(b, l, m, null, false, false, null);
        break;
      }
    }
    return r;
  }

  // catch expression
  public static boolean catch_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "catch_expression")) return false;
    if (!nextTokenIsFast(b, ERL_CATCH)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeTokenSmart(b, ERL_CATCH);
    p = r; // pin = 1
    r = r && expression(b, l + 1, -1);
    exit_section_(b, l, m, ERL_CATCH_EXPRESSION, r, p, null);
    return r || p;
  }

  // mult_op &(!(atom (',' | '>>')))
  private static boolean multiplicative_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiplicative_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = mult_op(b, l + 1);
    r = r && multiplicative_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &(!(atom (',' | '>>')))
  private static boolean multiplicative_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiplicative_expression_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = multiplicative_expression_0_1_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // !(atom (',' | '>>'))
  private static boolean multiplicative_expression_0_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiplicative_expression_0_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !multiplicative_expression_0_1_0_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // atom (',' | '>>')
  private static boolean multiplicative_expression_0_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiplicative_expression_0_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = atom(b, l + 1);
    r = r && multiplicative_expression_0_1_0_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // ',' | '>>'
  private static boolean multiplicative_expression_0_1_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "multiplicative_expression_0_1_0_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, ERL_COMMA);
    if (!r) r = consumeTokenSmart(b, ERL_BIN_END);
    exit_section_(b, m, null, r);
    return r;
  }

  // <<isModeOn "ATOM_ATTRIBUTE">> q_atom '/' integer
  public static boolean atom_with_arity_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "atom_with_arity_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = isModeOn(b, l + 1, "ATOM_ATTRIBUTE");
    r = r && q_atom(b, l + 1);
    r = r && consumeToken(b, ERL_OP_AR_DIV);
    r = r && consumeToken(b, ERL_INTEGER);
    exit_section_(b, l, m, ERL_ATOM_WITH_ARITY_EXPRESSION, r, false, null);
    return r;
  }

  public static boolean prefix_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "prefix_expression")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = prefix_op(b, l + 1);
    p = r;
    r = p && expression(b, l, 10);
    exit_section_(b, l, m, ERL_PREFIX_EXPRESSION, r, p, null);
    return r || p;
  }

  // q_atom argument_list &not_function_definition
  public static boolean function_call_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_call_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = q_atom(b, l + 1);
    r = r && argument_list(b, l + 1);
    r = r && function_call_expression_2(b, l + 1);
    exit_section_(b, l, m, ERL_FUNCTION_CALL_EXPRESSION, r, false, null);
    return r;
  }

  // &not_function_definition
  private static boolean function_call_expression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_call_expression_2")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = not_function_definition(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // module_ref ':' (function_call_expression)
  public static boolean global_function_call_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "global_function_call_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = module_ref(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    r = r && global_function_call_expression_2(b, l + 1);
    exit_section_(b, l, m, ERL_GLOBAL_FUNCTION_CALL_EXPRESSION, r, false, null);
    return r;
  }

  // (function_call_expression)
  private static boolean global_function_call_expression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "global_function_call_expression_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = function_call_expression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // [q_atom_or_var ':'] (q_atom_or_var | macros) argument_list &not_function_definition
  public static boolean generic_function_call_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "generic_function_call_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = generic_function_call_expression_0(b, l + 1);
    r = r && generic_function_call_expression_1(b, l + 1);
    r = r && argument_list(b, l + 1);
    r = r && generic_function_call_expression_3(b, l + 1);
    exit_section_(b, l, m, ERL_GENERIC_FUNCTION_CALL_EXPRESSION, r, false, null);
    return r;
  }

  // [q_atom_or_var ':']
  private static boolean generic_function_call_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "generic_function_call_expression_0")) return false;
    generic_function_call_expression_0_0(b, l + 1);
    return true;
  }

  // q_atom_or_var ':'
  private static boolean generic_function_call_expression_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "generic_function_call_expression_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom_or_var(b, l + 1);
    r = r && consumeToken(b, ERL_COLON);
    exit_section_(b, m, null, r);
    return r;
  }

  // q_atom_or_var | macros
  private static boolean generic_function_call_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "generic_function_call_expression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = q_atom_or_var(b, l + 1);
    if (!r) r = macros(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &not_function_definition
  private static boolean generic_function_call_expression_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "generic_function_call_expression_3")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = not_function_definition(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // argument_list &not_function_definition
  private static boolean anonymous_call_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "anonymous_call_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = argument_list(b, l + 1);
    r = r && anonymous_call_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // &not_function_definition
  private static boolean anonymous_call_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "anonymous_call_expression_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _AND_, null);
    r = not_function_definition(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // record_tail
  public static boolean record2_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "record2_expression")) return false;
    if (!nextTokenIsFast(b, ERL_RADIX, ERL_QMARK)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = record_tail(b, l + 1);
    exit_section_(b, l, m, ERL_RECORD_EXPRESSION, r, false, null);
    return r;
  }

  // q_atom '.' q_atom !'('
  public static boolean qualified_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qualified_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = q_atom(b, l + 1);
    r = r && consumeToken(b, ERL_DOT);
    r = r && q_atom(b, l + 1);
    r = r && qualified_expression_3(b, l + 1);
    exit_section_(b, l, m, ERL_QUALIFIED_EXPRESSION, r, false, null);
    return r;
  }

  // !'('
  private static boolean qualified_expression_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "qualified_expression_3")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeTokenSmart(b, ERL_PAR_LEFT);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // atomic
  //   | q_var
  //   | tuple_expression
  //   | list_expression
  //   | case_expression
  //   | if_expression
  //   | binary_comprehension
  //   | list_comprehension
  //   | map_comprehension
  //   | map_construct_expression
  //   | receive_expression
  //   | fun_expression
  //   | try_expression
  //   | binary_expression
  //   | begin_end_expression
  public static boolean max_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "max_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<expression>");
    r = atomic(b, l + 1);
    if (!r) r = q_var(b, l + 1);
    if (!r) r = tuple_expression(b, l + 1);
    if (!r) r = list_expression(b, l + 1);
    if (!r) r = case_expression(b, l + 1);
    if (!r) r = if_expression(b, l + 1);
    if (!r) r = binary_comprehension(b, l + 1);
    if (!r) r = list_comprehension(b, l + 1);
    if (!r) r = map_comprehension(b, l + 1);
    if (!r) r = map_construct_expression(b, l + 1);
    if (!r) r = receive_expression(b, l + 1);
    if (!r) r = fun_expression(b, l + 1);
    if (!r) r = try_expression(b, l + 1);
    if (!r) r = binary_expression(b, l + 1);
    if (!r) r = begin_end_expression(b, l + 1);
    exit_section_(b, l, m, ERL_MAX_EXPRESSION, r, false, null);
    return r;
  }

  public static boolean parenthesized_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_expression")) return false;
    if (!nextTokenIsFast(b, ERL_PAR_LEFT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeTokenSmart(b, ERL_PAR_LEFT);
    p = r;
    r = p && expression(b, l, -1);
    r = p && report_error_(b, consumeToken(b, ERL_PAR_RIGHT)) && r;
    exit_section_(b, l, m, ERL_PARENTHESIZED_EXPRESSION, r, p, null);
    return r || p;
  }

  final static Parser argument_definition_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return argument_definition(b, l + 1);
    }
  };
  final static Parser atom_attribute_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return atom_attribute(b, l + 1);
    }
  };
  final static Parser config_expr_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return config_expr_recover(b, l + 1);
    }
  };
  final static Parser export_functions_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return export_functions(b, l + 1);
    }
  };
  final static Parser export_types_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return export_types(b, l + 1);
    }
  };
  final static Parser expression_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return expression(b, l + 1, -1);
    }
  };
  final static Parser exprs_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return exprs_recover(b, l + 1);
    }
  };
  final static Parser form_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return form_recover(b, l + 1);
    }
  };
  final static Parser guard_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return guard(b, l + 1);
    }
  };
  final static Parser macro_form_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return macro_form_recover(b, l + 1);
    }
  };
  final static Parser macros_body_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return macros_body(b, l + 1);
    }
  };
  final static Parser try_argument_definition_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return try_argument_definition(b, l + 1);
    }
  };
  final static Parser tuple_recoverer_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return tuple_recoverer(b, l + 1);
    }
  };
  final static Parser type_body_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return type_body(b, l + 1);
    }
  };
}
