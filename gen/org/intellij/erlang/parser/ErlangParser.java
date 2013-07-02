// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.openapi.diagnostic.Logger;
import static org.intellij.erlang.ErlangTypes.*;
import static org.intellij.erlang.parser.ErlangParserUtil.*;
import com.intellij.lang.LighterASTNode;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class ErlangParser implements PsiParser {

  public static final Logger LOG_ = Logger.getInstance("org.intellij.erlang.parser.ErlangParser");

  public ASTNode parse(IElementType root_, PsiBuilder builder_) {
    int level_ = 0;
    boolean result_;
    builder_ = adapt_builder_(root_, builder_, this, EXTENDS_SETS_);
    if (root_ == ERL_ADDITIVE_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 6);
    }
    else if (root_ == ERL_AFTER_CLAUSE) {
      result_ = after_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_AFTER_CLAUSE_BODY) {
      result_ = after_clause_body(builder_, level_ + 1);
    }
    else if (root_ == ERL_ANDALSO_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 3);
    }
    else if (root_ == ERL_ANONYMOUS_CALL_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 10);
    }
    else if (root_ == ERL_ARGUMENT_DEFINITION) {
      result_ = argument_definition(builder_, level_ + 1);
    }
    else if (root_ == ERL_ARGUMENT_DEFINITION_LIST) {
      result_ = argument_definition_list(builder_, level_ + 1);
    }
    else if (root_ == ERL_ARGUMENT_LIST) {
      result_ = argument_list(builder_, level_ + 1);
    }
    else if (root_ == ERL_ASSIGNMENT_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 0);
    }
    else if (root_ == ERL_ATOM_ATTRIBUTE) {
      result_ = atom_attribute(builder_, level_ + 1);
    }
    else if (root_ == ERL_ATTR_VAL) {
      result_ = attr_val(builder_, level_ + 1);
    }
    else if (root_ == ERL_ATTRIBUTE) {
      result_ = attribute(builder_, level_ + 1);
    }
    else if (root_ == ERL_BEGIN_END_BODY) {
      result_ = begin_end_body(builder_, level_ + 1);
    }
    else if (root_ == ERL_BEGIN_END_EXPRESSION) {
      result_ = begin_end_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_BEHAVIOUR) {
      result_ = behaviour(builder_, level_ + 1);
    }
    else if (root_ == ERL_BIN_BASE_TYPE) {
      result_ = bin_base_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_BIN_ELEMENT) {
      result_ = bin_element(builder_, level_ + 1);
    }
    else if (root_ == ERL_BIN_UNIT_TYPE) {
      result_ = bin_unit_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_BINARY_EXPRESSION) {
      result_ = binary_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_BINARY_TYPE) {
      result_ = binary_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_BIT_TYPE) {
      result_ = bit_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_CALLBACK_SPEC) {
      result_ = callback_spec(builder_, level_ + 1);
    }
    else if (root_ == ERL_CASE_EXPRESSION) {
      result_ = case_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_CATCH_EXPRESSION) {
      result_ = catch_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_CLAUSE_BODY) {
      result_ = clause_body(builder_, level_ + 1);
    }
    else if (root_ == ERL_CLAUSE_GUARD) {
      result_ = clause_guard(builder_, level_ + 1);
    }
    else if (root_ == ERL_COLON_QUALIFIED_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 9);
    }
    else if (root_ == ERL_COMP_OP_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 4);
    }
    else if (root_ == ERL_CONFIG_CALL_EXPRESSION) {
      result_ = config_call_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_CONFIG_EXPRESSION) {
      result_ = config_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_CR_CLAUSE) {
      result_ = cr_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPORT) {
      result_ = export(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPORT_FUNCTION) {
      result_ = export_function(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPORT_FUNCTIONS) {
      result_ = export_functions(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPORT_TYPE) {
      result_ = export_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPORT_TYPE_ATTRIBUTE) {
      result_ = export_type_attribute(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPORT_TYPES) {
      result_ = export_types(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, -1);
    }
    else if (root_ == ERL_FIELD_TYPE) {
      result_ = field_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_CLAUSE) {
      result_ = fun_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_CLAUSES) {
      result_ = fun_clauses(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_EXPRESSION) {
      result_ = fun_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_TYPE) {
      result_ = fun_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_TYPE_100_T) {
      result_ = fun_type_100_t(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_TYPE_ARGUMENTS) {
      result_ = fun_type_arguments(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_TYPE_SIGS) {
      result_ = fun_type_sigs(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUN_TYPE_SIGS_BRACES) {
      result_ = fun_type_sigs_braces(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUNCTION) {
      result_ = function(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUNCTION_CALL_EXPRESSION) {
      result_ = function_call_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUNCTION_CLAUSE) {
      result_ = function_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUNCTION_WITH_ARITY) {
      result_ = function_with_arity(builder_, level_ + 1);
    }
    else if (root_ == ERL_FUNCTION_WITH_ARITY_VARIABLES) {
      result_ = function_with_arity_variables(builder_, level_ + 1);
    }
    else if (root_ == ERL_GENERIC_FUNCTION_CALL_EXPRESSION) {
      result_ = generic_function_call_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_GLOBAL_FUNCTION_CALL_EXPRESSION) {
      result_ = global_function_call_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_GUARD) {
      result_ = guard(builder_, level_ + 1);
    }
    else if (root_ == ERL_IF_CLAUSE) {
      result_ = if_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_IF_CLAUSES) {
      result_ = if_clauses(builder_, level_ + 1);
    }
    else if (root_ == ERL_IF_EXPRESSION) {
      result_ = if_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_IMPORT_DIRECTIVE) {
      result_ = import_directive(builder_, level_ + 1);
    }
    else if (root_ == ERL_IMPORT_FUNCTION) {
      result_ = import_function(builder_, level_ + 1);
    }
    else if (root_ == ERL_IMPORT_FUNCTIONS) {
      result_ = import_functions(builder_, level_ + 1);
    }
    else if (root_ == ERL_INCLUDE) {
      result_ = include(builder_, level_ + 1);
    }
    else if (root_ == ERL_INCLUDE_STRING) {
      result_ = include_string(builder_, level_ + 1);
    }
    else if (root_ == ERL_INT_TYPE) {
      result_ = int_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_LC_EXPRESSION) {
      result_ = lc_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_LC_EXPRS) {
      result_ = lc_exprs(builder_, level_ + 1);
    }
    else if (root_ == ERL_LIST_COMPREHENSION) {
      result_ = list_comprehension(builder_, level_ + 1);
    }
    else if (root_ == ERL_LIST_EXPRESSION) {
      result_ = list_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_LIST_OP_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 5);
    }
    else if (root_ == ERL_MACROS) {
      result_ = macros(builder_, level_ + 1);
    }
    else if (root_ == ERL_MACROS_ARG) {
      result_ = macros_arg(builder_, level_ + 1);
    }
    else if (root_ == ERL_MACROS_BODY) {
      result_ = macros_body(builder_, level_ + 1);
    }
    else if (root_ == ERL_MACROS_CALL) {
      result_ = macros_call(builder_, level_ + 1);
    }
    else if (root_ == ERL_MACROS_DEFINITION) {
      result_ = macros_definition(builder_, level_ + 1);
    }
    else if (root_ == ERL_MACROS_NAME) {
      result_ = macros_name(builder_, level_ + 1);
    }
    else if (root_ == ERL_MAX_EXPRESSION) {
      result_ = max_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_MODEL_FIELD_LIST) {
      result_ = model_field_list(builder_, level_ + 1);
    }
    else if (root_ == ERL_MODULE) {
      result_ = module(builder_, level_ + 1);
    }
    else if (root_ == ERL_MODULE_REF) {
      result_ = module_ref(builder_, level_ + 1);
    }
    else if (root_ == ERL_MULTIPLICATIVE_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 7);
    }
    else if (root_ == ERL_OPT_BIT_TYPE_LIST) {
      result_ = opt_bit_type_list(builder_, level_ + 1);
    }
    else if (root_ == ERL_ORELSE_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 2);
    }
    else if (root_ == ERL_PARENTHESIZED_EXPRESSION) {
      result_ = parenthesized_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_PREFIX_EXPRESSION) {
      result_ = prefix_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_Q_ATOM) {
      result_ = q_atom(builder_, level_ + 1);
    }
    else if (root_ == ERL_Q_VAR) {
      result_ = q_var(builder_, level_ + 1);
    }
    else if (root_ == ERL_QUALIFIED_EXPRESSION) {
      result_ = qualified_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECEIVE_EXPRESSION) {
      result_ = receive_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_DEFINITION) {
      result_ = record_definition(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 10);
    }
    else if (root_ == ERL_RECORD_FIELD) {
      result_ = record_field(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_FIELDS) {
      result_ = record_fields(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_REF) {
      result_ = record_ref(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_TUPLE) {
      result_ = record_tuple(builder_, level_ + 1);
    }
    else if (root_ == ERL_RULE) {
      result_ = rule(builder_, level_ + 1);
    }
    else if (root_ == ERL_RULE_BODY) {
      result_ = rule_body(builder_, level_ + 1);
    }
    else if (root_ == ERL_RULE_CLAUSE) {
      result_ = rule_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_SEND_EXPRESSION) {
      result_ = expression(builder_, level_ + 1, 1);
    }
    else if (root_ == ERL_SPEC_FUN) {
      result_ = spec_fun(builder_, level_ + 1);
    }
    else if (root_ == ERL_SPECIFICATION) {
      result_ = specification(builder_, level_ + 1);
    }
    else if (root_ == ERL_STRING_LITERAL) {
      result_ = string_literal(builder_, level_ + 1);
    }
    else if (root_ == ERL_TOP_TYPE) {
      result_ = top_type(builder_, level_ + 1);
    }
    else if (root_ == ERL_TOP_TYPE_100_T) {
      result_ = top_type_100_t(builder_, level_ + 1);
    }
    else if (root_ == ERL_TOP_TYPE_CLAUSE) {
      result_ = top_type_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_TRY_CATCH) {
      result_ = try_catch(builder_, level_ + 1);
    }
    else if (root_ == ERL_TRY_CLAUSE) {
      result_ = try_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_TRY_CLAUSES) {
      result_ = try_clauses(builder_, level_ + 1);
    }
    else if (root_ == ERL_TRY_EXPRESSION) {
      result_ = try_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_TRY_EXPRESSIONS_CLAUSE) {
      result_ = try_expressions_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_TUPLE_EXPRESSION) {
      result_ = tuple_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE) {
      result_ = type(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_DEFINITION) {
      result_ = type_definition(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_GUARD) {
      result_ = type_guard(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_REF) {
      result_ = type_ref(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_SIG) {
      result_ = type_sig(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_SIG_GUARD) {
      result_ = type_sig_guard(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPED_ATTR_VAL) {
      result_ = typed_attr_val(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPED_EXPR) {
      result_ = typed_expr(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPED_RECORD_FIELDS) {
      result_ = typed_record_fields(builder_, level_ + 1);
    }
    else {
      Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
      result_ = parse_root_(root_, builder_, level_);
      exit_section_(builder_, level_, marker_, root_, result_, true, TOKEN_ADVANCER);
    }
    return builder_.getTreeBuilt();
  }

  protected boolean parse_root_(final IElementType root_, final PsiBuilder builder_, final int level_) {
    return forms(builder_, level_ + 1);
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(ERL_ADDITIVE_EXPRESSION, ERL_ANDALSO_EXPRESSION, ERL_ANONYMOUS_CALL_EXPRESSION, ERL_ASSIGNMENT_EXPRESSION,
      ERL_BEGIN_END_EXPRESSION, ERL_BINARY_EXPRESSION, ERL_CASE_EXPRESSION, ERL_CATCH_EXPRESSION,
      ERL_COLON_QUALIFIED_EXPRESSION, ERL_COMP_OP_EXPRESSION, ERL_CONFIG_CALL_EXPRESSION, ERL_CONFIG_EXPRESSION,
      ERL_EXPRESSION, ERL_FUNCTION_CALL_EXPRESSION, ERL_FUN_EXPRESSION, ERL_GENERIC_FUNCTION_CALL_EXPRESSION,
      ERL_GLOBAL_FUNCTION_CALL_EXPRESSION, ERL_IF_EXPRESSION, ERL_LC_EXPRESSION, ERL_LIST_COMPREHENSION,
      ERL_LIST_EXPRESSION, ERL_LIST_OP_EXPRESSION, ERL_MAX_EXPRESSION, ERL_MULTIPLICATIVE_EXPRESSION,
      ERL_ORELSE_EXPRESSION, ERL_PARENTHESIZED_EXPRESSION, ERL_PREFIX_EXPRESSION, ERL_QUALIFIED_EXPRESSION,
      ERL_RECEIVE_EXPRESSION, ERL_RECORD_EXPRESSION, ERL_SEND_EXPRESSION, ERL_STRING_LITERAL,
      ERL_TRY_EXPRESSION, ERL_TUPLE_EXPRESSION),
  };

  /* ********************************************************** */
  // '+' |'-' | bor | bxor | bsl | bsr | or | xor
  static boolean add_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "add_op")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_BOR);
    if (!result_) result_ = consumeToken(builder_, ERL_BXOR);
    if (!result_) result_ = consumeToken(builder_, ERL_BSL);
    if (!result_) result_ = consumeToken(builder_, ERL_BSR);
    if (!result_) result_ = consumeToken(builder_, ERL_OR);
    if (!result_) result_ = consumeToken(builder_, ERL_XOR);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // after after_clause_body
  public static boolean after_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "after_clause")) return false;
    if (!nextTokenIs(builder_, ERL_AFTER)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_AFTER);
    pinned_ = result_; // pin = 1
    result_ = result_ && after_clause_body(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_AFTER_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // <<guarded expression>> clause_body
  public static boolean after_clause_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "after_clause_body")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<after clause body>");
    result_ = guarded(builder_, level_ + 1, expression_parser_);
    pinned_ = result_; // pin = 1
    result_ = result_ && clause_body(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_AFTER_CLAUSE_BODY, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // expression
  public static boolean argument_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<argument definition>");
    result_ = expression(builder_, level_ + 1, -1);
    exit_section_(builder_, level_, marker_, ERL_ARGUMENT_DEFINITION, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '(' argument_definition? (',' argument_definition)* ')'
  public static boolean argument_definition_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, argument_definition_list_1(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, argument_definition_list_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_ARGUMENT_DEFINITION_LIST, result_, pinned_, null);
    return result_ || pinned_;
  }

  // argument_definition?
  private static boolean argument_definition_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list_1")) return false;
    argument_definition(builder_, level_ + 1);
    return true;
  }

  // (',' argument_definition)*
  private static boolean argument_definition_list_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list_2")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!argument_definition_list_2_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "argument_definition_list_2");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' argument_definition
  private static boolean argument_definition_list_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && argument_definition(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '(' call_exprs? ')'
  public static boolean argument_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_list")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, argument_list_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_ARGUMENT_LIST, result_, pinned_, null);
    return result_ || pinned_;
  }

  // call_exprs?
  private static boolean argument_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_list_1")) return false;
    call_exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom ['(' typed_attr_val ')' | typed_attr_val | attr_val]
  public static boolean atom_attribute(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<attribute>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<attribute>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && atom_attribute_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_ATOM_ATTRIBUTE, result_, false, null);
    return result_;
  }

  // ['(' typed_attr_val ')' | typed_attr_val | attr_val]
  private static boolean atom_attribute_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1")) return false;
    atom_attribute_1_0(builder_, level_ + 1);
    return true;
  }

  // '(' typed_attr_val ')' | typed_attr_val | attr_val
  private static boolean atom_attribute_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = atom_attribute_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = typed_attr_val(builder_, level_ + 1);
    if (!result_) result_ = attr_val(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' typed_attr_val ')'
  private static boolean atom_attribute_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && typed_attr_val(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean atom_with_arity_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_with_arity_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OP_AR_DIV);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    exit_section_(builder_, level_, marker_, ERL_EXPRESSION, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // char | integer | float | q_atom !'(' | string_literal+
  static boolean atomic(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atomic")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_CHAR);
    if (!result_) result_ = consumeToken(builder_, ERL_INTEGER);
    if (!result_) result_ = consumeToken(builder_, ERL_FLOAT);
    if (!result_) result_ = atomic_3(builder_, level_ + 1);
    if (!result_) result_ = atomic_4(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // q_atom !'('
  private static boolean atomic_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atomic_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && atomic_3_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !'('
  private static boolean atomic_3_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atomic_3_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, ERL_PAR_LEFT);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // string_literal+
  private static boolean atomic_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atomic_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = string_literal(builder_, level_ + 1);
    int offset_ = builder_.getCurrentOffset();
    while (result_) {
      if (!string_literal(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "atomic_4");
        break;
      }
      offset_ = next_offset_;
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '(' exprs ')' | exprs
  public static boolean attr_val(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attr_val")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<attribute value>");
    result_ = attr_val_0(builder_, level_ + 1);
    if (!result_) result_ = exprs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_ATTR_VAL, result_, false, null);
    return result_;
  }

  // '(' exprs ')'
  private static boolean attr_val_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attr_val_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && exprs(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '-' (
  //   module
  //   | export
  //   | export_type_attribute
  //   | import_directive
  //   | specification
  //   | callback_spec
  //   | behaviour
  //   | atom_attribute
  //   )
  public static boolean attribute(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attribute")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    pinned_ = result_; // pin = 1
    result_ = result_ && attribute_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_ATTRIBUTE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // module
  //   | export
  //   | export_type_attribute
  //   | import_directive
  //   | specification
  //   | callback_spec
  //   | behaviour
  //   | atom_attribute
  private static boolean attribute_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attribute_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module(builder_, level_ + 1);
    if (!result_) result_ = export(builder_, level_ + 1);
    if (!result_) result_ = export_type_attribute(builder_, level_ + 1);
    if (!result_) result_ = import_directive(builder_, level_ + 1);
    if (!result_) result_ = specification(builder_, level_ + 1);
    if (!result_) result_ = callback_spec(builder_, level_ + 1);
    if (!result_) result_ = behaviour(builder_, level_ + 1);
    if (!result_) result_ = atom_attribute(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // exprs
  public static boolean begin_end_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "begin_end_body")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = exprs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_BEGIN_END_BODY, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // begin begin_end_body end
  public static boolean begin_end_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "begin_end_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BEGIN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BEGIN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, begin_end_body(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_BEGIN_END_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'behaviour' '(' module_ref ')'
  public static boolean behaviour(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "behaviour")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<behaviour>");
    result_ = consumeToken(builder_, "behaviour");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, module_ref(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_BEHAVIOUR, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_var ':' integer
  public static boolean bin_base_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_base_type")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    exit_section_(builder_, level_, marker_, ERL_BIN_BASE_TYPE, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // bin_base_type ',' bin_unit_type
  static boolean bin_base_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_base_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = bin_base_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_COMMA));
    result_ = pinned_ && bin_unit_type(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // prefix_op? expression [':' expression] opt_bit_type_list?
  public static boolean bin_element(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<binary element>");
    result_ = bin_element_0(builder_, level_ + 1);
    result_ = result_ && expression(builder_, level_ + 1, -1);
    result_ = result_ && bin_element_2(builder_, level_ + 1);
    result_ = result_ && bin_element_3(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_BIN_ELEMENT, result_, false, null);
    return result_;
  }

  // prefix_op?
  private static boolean bin_element_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_0")) return false;
    prefix_op(builder_, level_ + 1);
    return true;
  }

  // [':' expression]
  private static boolean bin_element_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_2")) return false;
    bin_element_2_0(builder_, level_ + 1);
    return true;
  }

  // ':' expression
  private static boolean bin_element_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COLON);
    result_ = result_ && expression(builder_, level_ + 1, -1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // opt_bit_type_list?
  private static boolean bin_element_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_3")) return false;
    opt_bit_type_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // bin_element (',' bin_element)*
  static boolean bin_element_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_list")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = bin_element(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && bin_element_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' bin_element)*
  private static boolean bin_element_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!bin_element_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "bin_element_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' bin_element
  private static boolean bin_element_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && bin_element(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_var ':' q_var '*' integer
  public static boolean bin_unit_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_unit_type")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    result_ = result_ && q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OP_AR_MUL);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    exit_section_(builder_, level_, marker_, ERL_BIN_UNIT_TYPE, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '<<' binary_expression '||' lc_exprs '>>'
  public static boolean binary_comprehension(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_comprehension")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BIN_START);
    result_ = result_ && binary_expression(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OR_OR);
    pinned_ = result_; // pin = 3
    result_ = result_ && report_error_(builder_, lc_exprs(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BIN_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_LIST_COMPREHENSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '<<' bin_element_list? '>>'
  public static boolean binary_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BIN_START);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, binary_expression_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BIN_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_BINARY_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // bin_element_list?
  private static boolean binary_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_expression_1")) return false;
    bin_element_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '<<' [bin_base_type | bin_unit_type | bin_base_type_list] '>>'
  public static boolean binary_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_type")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BIN_START);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, binary_type_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BIN_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_BINARY_TYPE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [bin_base_type | bin_unit_type | bin_base_type_list]
  private static boolean binary_type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_type_1")) return false;
    binary_type_1_0(builder_, level_ + 1);
    return true;
  }

  // bin_base_type | bin_unit_type | bin_base_type_list
  private static boolean binary_type_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_type_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = bin_base_type(builder_, level_ + 1);
    if (!result_) result_ = bin_unit_type(builder_, level_ + 1);
    if (!result_) result_ = bin_base_type_list(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // q_atom [':' integer]
  public static boolean bit_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && bit_type_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_BIT_TYPE, result_, false, null);
    return result_;
  }

  // [':' integer]
  private static boolean bit_type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_1")) return false;
    bit_type_1_0(builder_, level_ + 1);
    return true;
  }

  // ':' integer
  private static boolean bit_type_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COLON);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // bit_type ('-' bit_type)*
  static boolean bit_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = bit_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && bit_type_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ('-' bit_type)*
  private static boolean bit_type_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!bit_type_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "bit_type_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // '-' bit_type
  private static boolean bit_type_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    pinned_ = result_; // pin = 1
    result_ = result_ && bit_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // expr_with_guard (',' expr_with_guard)*
  static boolean call_exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "call_exprs")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = expr_with_guard(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && call_exprs_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' expr_with_guard)*
  private static boolean call_exprs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "call_exprs_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!call_exprs_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "call_exprs_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' expr_with_guard
  private static boolean call_exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "call_exprs_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && expr_with_guard(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'callback' type_spec
  public static boolean callback_spec(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "callback_spec")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<callback spec>");
    result_ = consumeToken(builder_, "callback");
    pinned_ = result_; // pin = 1
    result_ = result_ && type_spec(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_CALLBACK_SPEC, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // case expression of cr_clauses end
  public static boolean case_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "case_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CASE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CASE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, expression(builder_, level_ + 1, -1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_OF)) && result_;
    result_ = pinned_ && report_error_(builder_, cr_clauses(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_CASE_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '->' exprs
  public static boolean clause_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "clause_body")) return false;
    if (!nextTokenIs(builder_, ERL_ARROW)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_ARROW);
    pinned_ = result_; // pin = 1
    result_ = result_ && exprs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_CLAUSE_BODY, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // when <<guarded guard>>
  public static boolean clause_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "clause_guard")) return false;
    if (!nextTokenIs(builder_, ERL_WHEN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_WHEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && guarded(builder_, level_ + 1, guard_parser_);
    exit_section_(builder_, level_, marker_, ERL_CLAUSE_GUARD, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '==' | '/=' |'=<' |'<' |'>=' |'>' |'=:=' |'=/='
  static boolean comp_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comp_op")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_EQ_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_DIV_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_EQ_LT);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_LT);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_GT_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_GT);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_EQ_COL_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_EQ_DIV_EQ);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '(' config_exprs? ')'
  public static boolean config_argument_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_argument_list")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, config_argument_list_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_ARGUMENT_LIST, result_, pinned_, null);
    return result_ || pinned_;
  }

  // config_exprs?
  private static boolean config_argument_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_argument_list_1")) return false;
    config_exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '<<' config_exprs? '>>'
  public static boolean config_bin_list_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_bin_list_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BIN_START);
    result_ = result_ && config_bin_list_expression_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BIN_END);
    exit_section_(builder_, marker_, ERL_BINARY_EXPRESSION, result_);
    return result_;
  }

  // config_exprs?
  private static boolean config_bin_list_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_bin_list_expression_1")) return false;
    config_exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // config_argument_list
  public static boolean config_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_call_expression")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _LEFT_, null);
    result_ = config_argument_list(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_CONFIG_CALL_EXPRESSION, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // config_tuple_expression
  //   | config_list_expression
  //   | config_bin_list_expression
  //   | config_qualified_or_call_expression
  //   | (prefix_op? atomic)
  //   | q_var
  public static boolean config_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_expression")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _COLLAPSE_, "<expression>");
    result_ = config_tuple_expression(builder_, level_ + 1);
    if (!result_) result_ = config_list_expression(builder_, level_ + 1);
    if (!result_) result_ = config_bin_list_expression(builder_, level_ + 1);
    if (!result_) result_ = config_qualified_or_call_expression(builder_, level_ + 1);
    if (!result_) result_ = config_expression_4(builder_, level_ + 1);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_CONFIG_EXPRESSION, result_, false, null);
    return result_;
  }

  // prefix_op? atomic
  private static boolean config_expression_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_expression_4")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = config_expression_4_0(builder_, level_ + 1);
    result_ = result_ && atomic(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // prefix_op?
  private static boolean config_expression_4_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_expression_4_0")) return false;
    prefix_op(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // config_expression (',' config_expression)*
  static boolean config_exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_exprs")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = config_expression(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && config_exprs_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' config_expression)*
  private static boolean config_exprs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_exprs_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!config_exprs_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "config_exprs_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' config_expression
  private static boolean config_exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_exprs_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && config_expression(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' config_exprs? ']'
  public static boolean config_list_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_list_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && config_list_expression_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BRACKET_RIGHT);
    exit_section_(builder_, marker_, ERL_LIST_EXPRESSION, result_);
    return result_;
  }

  // config_exprs?
  private static boolean config_list_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_list_expression_1")) return false;
    config_exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom left_accessors?
  static boolean config_qualified_or_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_qualified_or_call_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && config_qualified_or_call_expression_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // left_accessors?
  private static boolean config_qualified_or_call_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_qualified_or_call_expression_1")) return false;
    left_accessors(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '{' config_exprs? '}'
  public static boolean config_tuple_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_tuple_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    result_ = result_ && config_tuple_expression_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_CURLY_RIGHT);
    exit_section_(builder_, marker_, ERL_TUPLE_EXPRESSION, result_);
    return result_;
  }

  // config_exprs?
  private static boolean config_tuple_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "config_tuple_expression_1")) return false;
    config_exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // is_console empty console_expression_or_empty
  static boolean console_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "console_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = is_console(builder_, level_ + 1);
    result_ = result_ && empty(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    result_ = result_ && console_expression_or_empty(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // exprs period | empty
  static boolean console_expression_or_empty(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "console_expression_or_empty")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = console_expression_or_empty_0(builder_, level_ + 1);
    if (!result_) result_ = empty(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // exprs period
  private static boolean console_expression_or_empty_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "console_expression_or_empty_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = exprs(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && period(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // cr_clause_argument_definition clause_guard? clause_body
  public static boolean cr_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clause")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<cr clause>");
    result_ = cr_clause_argument_definition(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, cr_clause_1(builder_, level_ + 1));
    result_ = pinned_ && clause_body(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_CR_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // clause_guard?
  private static boolean cr_clause_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clause_1")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // <<guarded argument_definition>>
  static boolean cr_clause_argument_definition(PsiBuilder builder_, int level_) {
    return guarded(builder_, level_ + 1, argument_definition_parser_);
  }

  /* ********************************************************** */
  // cr_clause (';' cr_clause)*
  static boolean cr_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clauses")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = cr_clause(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && cr_clauses_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' cr_clause)*
  private static boolean cr_clauses_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clauses_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!cr_clauses_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "cr_clauses_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' cr_clause
  private static boolean cr_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clauses_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && cr_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // ()
  static boolean empty(PsiBuilder builder_, int level_) {
    return true;
  }

  /* ********************************************************** */
  // 'export' '(' export_functions ')'
  public static boolean export(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<export>");
    result_ = consumeToken(builder_, "export");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, export_functions(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_EXPORT, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean export_function(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_function")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<export function>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<export function>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    exit_section_(builder_, level_, marker_, ERL_EXPORT_FUNCTION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // export_function (',' export_function)*
  static boolean export_function_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_function_list")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = export_function(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && export_function_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' export_function)*
  private static boolean export_function_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_function_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!export_function_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "export_function_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' export_function
  private static boolean export_function_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_function_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && export_function(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' export_function_list? ']'
  public static boolean export_functions(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, export_functions_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_EXPORT_FUNCTIONS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // export_function_list?
  private static boolean export_functions_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions_1")) return false;
    export_function_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean export_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    exit_section_(builder_, level_, marker_, ERL_EXPORT_TYPE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'export_type' '(' export_types ')'
  public static boolean export_type_attribute(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type_attribute")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<attribute>");
    result_ = consumeToken(builder_, "export_type");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, export_types(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_EXPORT_TYPE_ATTRIBUTE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // export_type (',' export_type)*
  static boolean export_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = export_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && export_type_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' export_type)*
  private static boolean export_type_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!export_type_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "export_type_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' export_type
  private static boolean export_type_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && export_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' export_type_list? ']'
  public static boolean export_types(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, export_types_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_EXPORT_TYPES, result_, pinned_, null);
    return result_ || pinned_;
  }

  // export_type_list?
  private static boolean export_types_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types_1")) return false;
    export_type_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // expression clause_guard?
  static boolean expr_with_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_with_guard")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = expression(builder_, level_ + 1, -1);
    pinned_ = result_; // pin = 1
    result_ = result_ && expr_with_guard_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // clause_guard?
  private static boolean expr_with_guard_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_with_guard_1")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // expression (exprs_tail)*
  static boolean exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = expression(builder_, level_ + 1, -1);
    result_ = result_ && exprs_1(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    exit_section_(builder_, level_, marker_, null, result_, pinned_, exprs_recover_parser_);
    return result_ || pinned_;
  }

  // (exprs_tail)*
  private static boolean exprs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!exprs_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "exprs_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (exprs_tail)
  private static boolean exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exprs_tail(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !(')' | ',' | '->' | '.' | ':-' | ';' | '}' | after | catch | end | of | atom '(')
  static boolean exprs_recover(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_recover")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !exprs_recover_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ')' | ',' | '->' | '.' | ':-' | ';' | '}' | after | catch | end | of | atom '('
  private static boolean exprs_recover_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_recover_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_COMMA);
    if (!result_) result_ = consumeToken(builder_, ERL_ARROW);
    if (!result_) result_ = consumeToken(builder_, ERL_DOT);
    if (!result_) result_ = consumeToken(builder_, ":-");
    if (!result_) result_ = consumeToken(builder_, ERL_SEMI);
    if (!result_) result_ = consumeToken(builder_, ERL_CURLY_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_AFTER);
    if (!result_) result_ = consumeToken(builder_, ERL_CATCH);
    if (!result_) result_ = consumeToken(builder_, ERL_END);
    if (!result_) result_ = consumeToken(builder_, ERL_OF);
    if (!result_) result_ = exprs_recover_0_11(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // atom '('
  private static boolean exprs_recover_0_11(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_recover_0_11")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_ATOM);
    result_ = result_ && consumeToken(builder_, ERL_PAR_LEFT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // ',' expression
  static boolean exprs_tail(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_tail")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && expression(builder_, level_ + 1, -1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, exprs_recover_parser_);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom '::' top_type
  public static boolean field_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_type")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_COLON_COLON));
    result_ = pinned_ && top_type(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_FIELD_TYPE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // field_type (',' field_type)*
  static boolean field_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = field_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && field_type_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' field_type)*
  private static boolean field_type_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_type_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!field_type_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "field_type_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' field_type
  private static boolean field_type_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_type_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && field_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // is_app_config config_expression
  //   | function
  //   | rule
  //   | record_definition
  //   | include
  //   | macros_definition
  //   | type_definition
  //   | attribute
  //   | macros_call
  static boolean form(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "form")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = form_0(builder_, level_ + 1);
    if (!result_) result_ = function(builder_, level_ + 1);
    if (!result_) result_ = rule(builder_, level_ + 1);
    if (!result_) result_ = record_definition(builder_, level_ + 1);
    if (!result_) result_ = include(builder_, level_ + 1);
    if (!result_) result_ = macros_definition(builder_, level_ + 1);
    if (!result_) result_ = type_definition(builder_, level_ + 1);
    if (!result_) result_ = attribute(builder_, level_ + 1);
    if (!result_) result_ = macros_call(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // is_app_config config_expression
  private static boolean form_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "form_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = is_app_config(builder_, level_ + 1);
    result_ = result_ && config_expression(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !('+' | '-' | '<<' | '?' | '[' | '_' | '{' | atom | bnot | char | float | integer | not | string | var)
  static boolean form_recover(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "form_recover")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !form_recover_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // '+' | '-' | '<<' | '?' | '[' | '_' | '{' | atom | bnot | char | float | integer | not | string | var
  private static boolean form_recover_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "form_recover_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_BIN_START);
    if (!result_) result_ = consumeToken(builder_, ERL_QMARK);
    if (!result_) result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    if (!result_) result_ = consumeToken(builder_, ERL_UNI_PATTERN);
    if (!result_) result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    if (!result_) result_ = consumeToken(builder_, ERL_ATOM);
    if (!result_) result_ = consumeToken(builder_, ERL_BNOT);
    if (!result_) result_ = consumeToken(builder_, ERL_CHAR);
    if (!result_) result_ = consumeToken(builder_, ERL_FLOAT);
    if (!result_) result_ = consumeToken(builder_, ERL_INTEGER);
    if (!result_) result_ = consumeToken(builder_, ERL_NOT);
    if (!result_) result_ = consumeToken(builder_, ERL_STRING);
    if (!result_) result_ = consumeToken(builder_, ERL_VAR);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // form period
  static boolean form_with_period(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "form_with_period")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = form(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && period(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, form_recover_parser_);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // console_expression | form_with_period *
  static boolean forms(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "forms")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = console_expression(builder_, level_ + 1);
    if (!result_) result_ = forms_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // form_with_period *
  private static boolean forms_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "forms_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!form_with_period(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "forms_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  /* ********************************************************** */
  // argument_definition_list clause_guard? clause_body
  public static boolean fun_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clause")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = argument_definition_list(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, fun_clause_1(builder_, level_ + 1));
    result_ = pinned_ && clause_body(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUN_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // clause_guard?
  private static boolean fun_clause_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clause_1")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // fun_clause (';' fun_clause)*
  public static boolean fun_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clauses")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = fun_clause(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && fun_clauses_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_FUN_CLAUSES, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' fun_clause)*
  private static boolean fun_clauses_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clauses_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!fun_clauses_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "fun_clauses_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' fun_clause
  private static boolean fun_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clauses_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && fun_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // fun (fun_expression_block | fun_expression_lambda)
  public static boolean fun_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression")) return false;
    if (!nextTokenIs(builder_, ERL_FUN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_FUN);
    pinned_ = result_; // pin = 1
    result_ = result_ && fun_expression_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_FUN_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // fun_expression_block | fun_expression_lambda
  private static boolean fun_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fun_expression_block(builder_, level_ + 1);
    if (!result_) result_ = fun_expression_lambda(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // fun_clauses end
  static boolean fun_expression_block(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_block")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = fun_clauses(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && consumeToken(builder_, ERL_END);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // [(module_ref | q_var) ':'] (function_with_arity | function_with_arity_variables)
  static boolean fun_expression_lambda(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_lambda")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fun_expression_lambda_0(builder_, level_ + 1);
    result_ = result_ && fun_expression_lambda_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [(module_ref | q_var) ':']
  private static boolean fun_expression_lambda_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_lambda_0")) return false;
    fun_expression_lambda_0_0(builder_, level_ + 1);
    return true;
  }

  // (module_ref | q_var) ':'
  private static boolean fun_expression_lambda_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_lambda_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fun_expression_lambda_0_0_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // module_ref | q_var
  private static boolean fun_expression_lambda_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_lambda_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_ref(builder_, level_ + 1);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // function_with_arity | function_with_arity_variables
  private static boolean fun_expression_lambda_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_lambda_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = function_with_arity(builder_, level_ + 1);
    if (!result_) result_ = function_with_arity_variables(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // fun_type_arguments top_type_clause
  public static boolean fun_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = fun_type_arguments(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_FUN_TYPE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '(' ('...' | top_type_list?) ')' top_type_clause
  public static boolean fun_type_100_t(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, fun_type_100_t_1(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_RIGHT)) && result_;
    result_ = pinned_ && top_type_clause(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUN_TYPE_100_T, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '...' | top_type_list?
  private static boolean fun_type_100_t_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_DOT_DOT_DOT);
    if (!result_) result_ = fun_type_100_t_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // top_type_list?
  private static boolean fun_type_100_t_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t_1_1")) return false;
    top_type_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '(' top_type_list? ')'
  public static boolean fun_type_arguments(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_arguments")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, fun_type_arguments_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUN_TYPE_ARGUMENTS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // top_type_list?
  private static boolean fun_type_arguments_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_arguments_1")) return false;
    top_type_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // [module_ref ':'] spec_fun '::'? type_sigs_list
  public static boolean fun_type_sigs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_sigs")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<fun type sigs>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<fun type sigs>");
    result_ = fun_type_sigs_0(builder_, level_ + 1);
    result_ = result_ && spec_fun(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, fun_type_sigs_2(builder_, level_ + 1));
    result_ = pinned_ && type_sigs_list(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUN_TYPE_SIGS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [module_ref ':']
  private static boolean fun_type_sigs_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_sigs_0")) return false;
    fun_type_sigs_0_0(builder_, level_ + 1);
    return true;
  }

  // module_ref ':'
  private static boolean fun_type_sigs_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_sigs_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_ref(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '::'?
  private static boolean fun_type_sigs_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_sigs_2")) return false;
    consumeToken(builder_, ERL_COLON_COLON);
    return true;
  }

  /* ********************************************************** */
  // '(' fun_type_sigs ')'
  public static boolean fun_type_sigs_braces(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_sigs_braces")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, fun_type_sigs(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUN_TYPE_SIGS_BRACES, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // function_clause (';' function_clause)*
  public static boolean function(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function")) return false;
    if (!nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = function_clause(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && function_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_FUNCTION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' function_clause)*
  private static boolean function_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!function_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "function_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' function_clause
  private static boolean function_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && function_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // !'?' q_atom argument_definition_list clause_guard? clause_body
  public static boolean function_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_clause")) return false;
    if (!nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = function_clause_0(builder_, level_ + 1);
    result_ = result_ && q_atom(builder_, level_ + 1);
    result_ = result_ && argument_definition_list(builder_, level_ + 1);
    pinned_ = result_; // pin = 3
    result_ = result_ && report_error_(builder_, function_clause_3(builder_, level_ + 1));
    result_ = pinned_ && clause_body(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUNCTION_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // !'?'
  private static boolean function_clause_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_clause_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, ERL_QMARK);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // clause_guard?
  private static boolean function_clause_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_clause_3")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean function_with_arity(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_with_arity")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<function with arity>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<function with arity>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUNCTION_WITH_ARITY, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_var '/' (integer|q_var)
  public static boolean function_with_arity_variables(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_with_arity_variables")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<function with arity variables>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<function with arity variables>");
    result_ = q_var(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && function_with_arity_variables_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_FUNCTION_WITH_ARITY_VARIABLES, result_, pinned_, null);
    return result_ || pinned_;
  }

  // integer|q_var
  private static boolean function_with_arity_variables_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_with_arity_variables_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_INTEGER);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // exprs (';' exprs)*
  public static boolean guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<guard>");
    result_ = exprs(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && guard_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_GUARD, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' exprs)*
  private static boolean guard_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!guard_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "guard_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' exprs
  private static boolean guard_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && exprs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // <<guarded guard>>
  static boolean guard_with_mode(PsiBuilder builder_, int level_) {
    return guarded(builder_, level_ + 1, guard_parser_);
  }

  /* ********************************************************** */
  // <<enterMode "GUARD">> (<<p1>> <<exitMode "GUARD">>|<<exitMode "GUARD">> !())
  static boolean guarded(PsiBuilder builder_, int level_, final Parser p1) {
    if (!recursion_guard_(builder_, level_, "guarded")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = enterMode(builder_, level_ + 1, "GUARD");
    result_ = result_ && guarded_1(builder_, level_ + 1, p1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // <<p1>> <<exitMode "GUARD">>|<<exitMode "GUARD">> !()
  private static boolean guarded_1(PsiBuilder builder_, int level_, final Parser p1) {
    if (!recursion_guard_(builder_, level_, "guarded_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = guarded_1_0(builder_, level_ + 1, p1);
    if (!result_) result_ = guarded_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // <<p1>> <<exitMode "GUARD">>
  private static boolean guarded_1_0(PsiBuilder builder_, int level_, final Parser p1) {
    if (!recursion_guard_(builder_, level_, "guarded_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = p1.parse(builder_, level_);
    result_ = result_ && exitMode(builder_, level_ + 1, "GUARD");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // <<exitMode "GUARD">> !()
  private static boolean guarded_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guarded_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = exitMode(builder_, level_ + 1, "GUARD");
    result_ = result_ && guarded_1_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !()
  private static boolean guarded_1_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guarded_1_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !guarded_1_1_1_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ()
  private static boolean guarded_1_1_1_0(PsiBuilder builder_, int level_) {
    return true;
  }

  /* ********************************************************** */
  // guard_with_mode clause_body
  public static boolean if_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clause")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<if clause>");
    result_ = guard_with_mode(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && clause_body(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_IF_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // if_clause (';' if_clause)*
  public static boolean if_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clauses")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<if clauses>");
    result_ = if_clause(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && if_clauses_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_IF_CLAUSES, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' if_clause)*
  private static boolean if_clauses_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clauses_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!if_clauses_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "if_clauses_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' if_clause
  private static boolean if_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clauses_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && if_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // if if_clauses end
  public static boolean if_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_expression")) return false;
    if (!nextTokenIs(builder_, ERL_IF)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_IF);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, if_clauses(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_IF_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'import' '(' module_ref ',' import_functions ')'
  public static boolean import_directive(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_directive")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<import directive>");
    result_ = consumeToken(builder_, "import");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, module_ref(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_COMMA)) && result_;
    result_ = pinned_ && report_error_(builder_, import_functions(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_IMPORT_DIRECTIVE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean import_function(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_function")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<import function>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<import function>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    exit_section_(builder_, level_, marker_, ERL_IMPORT_FUNCTION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // import_function (',' import_function)*
  static boolean import_function_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_function_list")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = import_function(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && import_function_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' import_function)*
  private static boolean import_function_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_function_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!import_function_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "import_function_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' import_function
  private static boolean import_function_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_function_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && import_function(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' import_function_list? ']'
  public static boolean import_functions(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_functions")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && import_functions_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BRACKET_RIGHT);
    exit_section_(builder_, marker_, ERL_IMPORT_FUNCTIONS, result_);
    return result_;
  }

  // import_function_list?
  private static boolean import_functions_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "import_functions_1")) return false;
    import_function_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '-' ('include' | 'include_lib') '(' include_string ')'
  public static boolean include(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && include_1(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, include_string(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_INCLUDE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // 'include' | 'include_lib'
  private static boolean include_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "include");
    if (!result_) result_ = consumeToken(builder_, "include_lib");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // string
  public static boolean include_string(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include_string")) return false;
    if (!nextTokenIs(builder_, ERL_STRING)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_STRING);
    exit_section_(builder_, marker_, ERL_INCLUDE_STRING, result_);
    return result_;
  }

  /* ********************************************************** */
  // '-'? (integer | macros argument_list?)
  public static boolean int_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = int_type_0(builder_, level_ + 1);
    result_ = result_ && int_type_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_INT_TYPE, result_, false, null);
    return result_;
  }

  // '-'?
  private static boolean int_type_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_0")) return false;
    consumeToken(builder_, ERL_OP_MINUS);
    return true;
  }

  // integer | macros argument_list?
  private static boolean int_type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_INTEGER);
    if (!result_) result_ = int_type_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // macros argument_list?
  private static boolean int_type_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = macros(builder_, level_ + 1);
    result_ = result_ && int_type_1_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // argument_list?
  private static boolean int_type_1_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_1_1_1")) return false;
    argument_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // &<<isApplicationLanguage>>
  static boolean is_app_config(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "is_app_config")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = isApplicationLanguage(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // &<<isConsole>>
  static boolean is_console(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "is_console")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = isConsole(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // [argument_definition ('<-' | '<=')] expression
  public static boolean lc_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _COLLAPSE_, "<expression>");
    result_ = lc_expression_0(builder_, level_ + 1);
    result_ = result_ && expression(builder_, level_ + 1, -1);
    pinned_ = result_; // pin = 2
    exit_section_(builder_, level_, marker_, ERL_LC_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [argument_definition ('<-' | '<=')]
  private static boolean lc_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0")) return false;
    lc_expression_0_0(builder_, level_ + 1);
    return true;
  }

  // argument_definition ('<-' | '<=')
  private static boolean lc_expression_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = argument_definition(builder_, level_ + 1);
    result_ = result_ && lc_expression_0_0_1(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '<-' | '<='
  private static boolean lc_expression_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_LT_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_LT_EQ);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // lc_expression (',' lc_expression)*
  public static boolean lc_exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_exprs")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<lc exprs>");
    result_ = lc_expression(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && lc_exprs_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_LC_EXPRS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' lc_expression)*
  private static boolean lc_exprs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_exprs_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!lc_exprs_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "lc_exprs_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' lc_expression
  private static boolean lc_exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_exprs_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && lc_expression(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // config_call_expression | qualified_expression+
  static boolean left_accessors(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "left_accessors")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = config_call_expression(builder_, level_ + 1);
    if (!result_) result_ = left_accessors_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qualified_expression+
  private static boolean left_accessors_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "left_accessors_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qualified_expression(builder_, level_ + 1);
    int offset_ = builder_.getCurrentOffset();
    while (result_) {
      if (!qualified_expression(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "left_accessors_1");
        break;
      }
      offset_ = next_offset_;
    }
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '[' atom_with_arity_expression (',' atom_with_arity_expression)* ']'
  public static boolean list_atom_with_arity_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_atom_with_arity_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && atom_with_arity_expression(builder_, level_ + 1);
    result_ = result_ && list_atom_with_arity_expression_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BRACKET_RIGHT);
    exit_section_(builder_, marker_, ERL_LIST_EXPRESSION, result_);
    return result_;
  }

  // (',' atom_with_arity_expression)*
  private static boolean list_atom_with_arity_expression_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_atom_with_arity_expression_2")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!list_atom_with_arity_expression_2_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "list_atom_with_arity_expression_2");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' atom_with_arity_expression
  private static boolean list_atom_with_arity_expression_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_atom_with_arity_expression_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && atom_with_arity_expression(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '[' expression '||' lc_exprs ']'
  public static boolean list_comprehension(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_comprehension")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && expression(builder_, level_ + 1, -1);
    result_ = result_ && consumeToken(builder_, ERL_OR_OR);
    pinned_ = result_; // pin = 3
    result_ = result_ && report_error_(builder_, lc_exprs(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_LIST_COMPREHENSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' (']' | expression not_comprehension tail)
  public static boolean list_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && list_expression_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, ERL_LIST_EXPRESSION, result_);
    return result_;
  }

  // ']' | expression not_comprehension tail
  private static boolean list_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) result_ = list_expression_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // expression not_comprehension tail
  private static boolean list_expression_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression_1_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = expression(builder_, level_ + 1, -1);
    result_ = result_ && not_comprehension(builder_, level_ + 1);
    pinned_ = result_; // pin = not_comprehension
    result_ = result_ && tail(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '++' | '--'
  static boolean list_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_op")) return false;
    if (!nextTokenIs(builder_, ERL_OP_PLUS_PLUS) && !nextTokenIs(builder_, ERL_OP_MINUS_MINUS)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_PLUS_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS_MINUS);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '?' macros_name
  public static boolean macros(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_QMARK);
    pinned_ = result_; // pin = 1
    result_ = result_ && macros_name(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_MACROS, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '?''?' macros_name
  public static boolean macros_arg(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_arg")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_QMARK);
    result_ = result_ && consumeToken(builder_, ERL_QMARK);
    result_ = result_ && macros_name(builder_, level_ + 1);
    exit_section_(builder_, marker_, ERL_MACROS_ARG, result_);
    return result_;
  }

  /* ********************************************************** */
  // expression ((',' | ';' | '->') expression)*
  public static boolean macros_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_body")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<macros body>");
    result_ = expression(builder_, level_ + 1, -1);
    pinned_ = result_; // pin = 1
    result_ = result_ && macros_body_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_MACROS_BODY, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ((',' | ';' | '->') expression)*
  private static boolean macros_body_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_body_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!macros_body_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "macros_body_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' | ';' | '->') expression
  private static boolean macros_body_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_body_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = macros_body_1_0_0(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && expression(builder_, level_ + 1, -1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ',' | ';' | '->'
  private static boolean macros_body_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_body_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COMMA);
    if (!result_) result_ = consumeToken(builder_, ERL_SEMI);
    if (!result_) result_ = consumeToken(builder_, ERL_ARROW);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // &('?') generic_function_call_expression
  public static boolean macros_call(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_call")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<macros call>");
    result_ = macros_call_0(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && generic_function_call_expression(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_MACROS_CALL, result_, pinned_, macros_call_recover_parser_);
    return result_ || pinned_;
  }

  // &('?')
  private static boolean macros_call_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_call_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = macros_call_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ('?')
  private static boolean macros_call_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_call_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_QMARK);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !'.'
  static boolean macros_call_recover(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_call_recover")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, ERL_DOT);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '-' 'define' '(' macros_name argument_definition_list? ',' <<guarded macros_body>> ')'
  public static boolean macros_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && consumeToken(builder_, "define");
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, macros_name(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, macros_definition_4(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_COMMA)) && result_;
    result_ = pinned_ && report_error_(builder_, guarded(builder_, level_ + 1, macros_body_parser_)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_MACROS_DEFINITION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // argument_definition_list?
  private static boolean macros_definition_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition_4")) return false;
    argument_definition_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // atom | var
  public static boolean macros_name(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_name")) return false;
    if (!nextTokenIs(builder_, ERL_ATOM) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<macros name>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<macros name>");
    result_ = consumeToken(builder_, ERL_ATOM);
    if (!result_) result_ = consumeToken(builder_, ERL_VAR);
    exit_section_(builder_, level_, marker_, ERL_MACROS_NAME, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // q_var ['::' top_type]
  public static boolean model_field(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "model_field")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<model field>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<model field>");
    result_ = q_var(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && model_field_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_ARGUMENT_DEFINITION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['::' top_type]
  private static boolean model_field_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "model_field_1")) return false;
    model_field_1_0(builder_, level_ + 1);
    return true;
  }

  // '::' top_type
  private static boolean model_field_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "model_field_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COLON_COLON);
    result_ = result_ && top_type(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '[' model_field (',' model_field)* ']'
  public static boolean model_field_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "model_field_list")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, model_field(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, model_field_list_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_MODEL_FIELD_LIST, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' model_field)*
  private static boolean model_field_list_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "model_field_list_2")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!model_field_list_2_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "model_field_list_2");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' model_field
  private static boolean model_field_list_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "model_field_list_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && model_field(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'module' '(' q_atom [',' module_tail] ')'
  public static boolean module(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<module>");
    result_ = consumeToken(builder_, "module");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, q_atom(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, module_3(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_MODULE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [',' module_tail]
  private static boolean module_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_3")) return false;
    module_3_0(builder_, level_ + 1);
    return true;
  }

  // ',' module_tail
  private static boolean module_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && module_tail(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean module_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_ref")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<module ref>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<module ref>");
    result_ = q_atom(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_MODULE_REF, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '.' q_atom | module_ref
  static boolean module_ref_or_dot_atom(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_ref_or_dot_atom")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_ref_or_dot_atom_0(builder_, level_ + 1);
    if (!result_) result_ = module_ref(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '.' q_atom
  private static boolean module_ref_or_dot_atom_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_ref_or_dot_atom_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_DOT);
    result_ = result_ && q_atom(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // model_field_list | argument_definition
  static boolean module_tail(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_tail")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = model_field_list(builder_, level_ + 1);
    if (!result_) result_ = argument_definition(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '/' |'*' | div | rem  | band | and
  static boolean mult_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "mult_op")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_AR_MUL);
    if (!result_) result_ = consumeToken(builder_, ERL_DIV);
    if (!result_) result_ = consumeToken(builder_, ERL_REM);
    if (!result_) result_ = consumeToken(builder_, ERL_BAND);
    if (!result_) result_ = consumeToken(builder_, ERL_AND);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // !'||'
  static boolean not_comprehension(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "not_comprehension")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, "<expression>");
    result_ = !consumeToken(builder_, ERL_OR_OR);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // <<isModeOn "GUARD">> | !('->'|when)
  static boolean not_function_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "not_function_definition")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = isModeOn(builder_, level_ + 1, "GUARD");
    if (!result_) result_ = not_function_definition_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // !('->'|when)
  private static boolean not_function_definition_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "not_function_definition_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !not_function_definition_1_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // '->'|when
  private static boolean not_function_definition_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "not_function_definition_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_ARROW);
    if (!result_) result_ = consumeToken(builder_, ERL_WHEN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '/' bit_type_list
  public static boolean opt_bit_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "opt_bit_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_OP_AR_DIV)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    pinned_ = result_; // pin = 1
    result_ = result_ && bit_type_list(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_OPT_BIT_TYPE_LIST, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '.'
  static boolean period(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "period")) return false;
    if (!nextTokenIs(builder_, ERL_DOT)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _LEFT_INNER_, null);
    result_ = consumeToken(builder_, ERL_DOT);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '+' | '-' | bnot | not
  static boolean prefix_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "prefix_op")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OP_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_BNOT);
    if (!result_) result_ = consumeToken(builder_, ERL_NOT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // macros_arg | macros | atom
  public static boolean q_atom(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "q_atom")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<q atom>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<q atom>");
    result_ = macros_arg(builder_, level_ + 1);
    if (!result_) result_ = macros(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, ERL_ATOM);
    exit_section_(builder_, level_, marker_, ERL_Q_ATOM, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // q_atom | q_var
  static boolean q_atom_or_var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "q_atom_or_var")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_atom(builder_, level_ + 1);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '_' | var
  public static boolean q_var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "q_var")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<q var>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<q var>");
    result_ = consumeToken(builder_, ERL_UNI_PATTERN);
    if (!result_) result_ = consumeToken(builder_, ERL_VAR);
    exit_section_(builder_, level_, marker_, ERL_Q_VAR, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // q_atom '.' q_atom
  static boolean qualified_atom_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_atom_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_DOT);
    result_ = result_ && q_atom(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // receive (after_clause | cr_clauses after_clause? ) end
  public static boolean receive_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression")) return false;
    if (!nextTokenIs(builder_, ERL_RECEIVE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_RECEIVE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, receive_expression_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    exit_section_(builder_, level_, marker_, ERL_RECEIVE_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // after_clause | cr_clauses after_clause?
  private static boolean receive_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = after_clause(builder_, level_ + 1);
    if (!result_) result_ = receive_expression_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // cr_clauses after_clause?
  private static boolean receive_expression_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = cr_clauses(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && receive_expression_1_1_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // after_clause?
  private static boolean receive_expression_1_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_1_1")) return false;
    after_clause(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '-' 'record' '(' q_atom ',' typed_record_fields ')'
  public static boolean record_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_definition")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && consumeToken(builder_, "record");
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, q_atom(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_COMMA)) && result_;
    result_ = pinned_ && report_error_(builder_, typed_record_fields(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_RECORD_DEFINITION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // (q_atom | '_') '=' (qualified_atom_expression | expression)
  public static boolean record_field(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<record field>");
    result_ = record_field_0(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_EQ));
    result_ = pinned_ && record_field_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_RECORD_FIELD, result_, pinned_, tuple_recoverer_parser_);
    return result_ || pinned_;
  }

  // q_atom | '_'
  private static boolean record_field_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_atom(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, ERL_UNI_PATTERN);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // qualified_atom_expression | expression
  private static boolean record_field_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = qualified_atom_expression(builder_, level_ + 1);
    if (!result_) result_ = expression(builder_, level_ + 1, -1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '.' q_atom
  public static boolean record_field_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field_ref")) return false;
    if (!nextTokenIs(builder_, ERL_DOT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_DOT);
    pinned_ = result_; // pin = 1
    result_ = result_ && q_atom(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RECORD_FIELD, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // record_field (',' record_field)*
  public static boolean record_fields(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_fields")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<record fields>");
    result_ = record_field(builder_, level_ + 1);
    result_ = result_ && record_fields_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RECORD_FIELDS, result_, false, null);
    return result_;
  }

  // (',' record_field)*
  private static boolean record_fields_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_fields_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!record_fields_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "record_fields_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' record_field
  private static boolean record_fields_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_fields_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && record_field(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // record_head_classic | macros &('{'|'.' q_atom &(!('(')))
  static boolean record_head(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head")) return false;
    if (!nextTokenIs(builder_, ERL_RADIX) && !nextTokenIs(builder_, ERL_QMARK)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = record_head_classic(builder_, level_ + 1);
    if (!result_) result_ = record_head_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // macros &('{'|'.' q_atom &(!('(')))
  private static boolean record_head_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = macros(builder_, level_ + 1);
    result_ = result_ && record_head_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &('{'|'.' q_atom &(!('(')))
  private static boolean record_head_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = record_head_1_1_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // '{'|'.' q_atom &(!('('))
  private static boolean record_head_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    if (!result_) result_ = record_head_1_1_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '.' q_atom &(!('('))
  private static boolean record_head_1_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1_1_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_DOT);
    result_ = result_ && q_atom(builder_, level_ + 1);
    result_ = result_ && record_head_1_1_0_1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &(!('('))
  private static boolean record_head_1_1_0_1_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1_1_0_1_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = record_head_1_1_0_1_2_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // !('(')
  private static boolean record_head_1_1_0_1_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1_1_0_1_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !record_head_1_1_0_1_2_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ('(')
  private static boolean record_head_1_1_0_1_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_1_1_0_1_2_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '#' record_ref
  static boolean record_head_classic(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_head_classic")) return false;
    if (!nextTokenIs(builder_, ERL_RADIX)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_RADIX);
    pinned_ = result_; // pin = 1
    result_ = result_ && record_ref(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean record_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_ref")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<record ref>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<record ref>");
    result_ = q_atom(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RECORD_REF, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // record_head (record_field_ref | record_tuple | ())
  static boolean record_tail(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_tail")) return false;
    if (!nextTokenIs(builder_, ERL_RADIX) && !nextTokenIs(builder_, ERL_QMARK)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = record_head(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && record_tail_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // record_field_ref | record_tuple | ()
  private static boolean record_tail_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_tail_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = record_field_ref(builder_, level_ + 1);
    if (!result_) result_ = record_tuple(builder_, level_ + 1);
    if (!result_) result_ = record_tail_1_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ()
  private static boolean record_tail_1_2(PsiBuilder builder_, int level_) {
    return true;
  }

  /* ********************************************************** */
  // '{' record_fields? '}'
  public static boolean record_tuple(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_tuple")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, record_tuple_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_RECORD_TUPLE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // record_fields?
  private static boolean record_tuple_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_tuple_1")) return false;
    record_fields(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // rule_clause (';' rule_clause)*
  public static boolean rule(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<rule>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<rule>");
    result_ = rule_clause(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && rule_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RULE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' rule_clause)*
  private static boolean rule_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!rule_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "rule_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' rule_clause
  private static boolean rule_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && rule_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // ':-' lc_exprs
  public static boolean rule_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_body")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<rule body>");
    result_ = consumeToken(builder_, ":-");
    pinned_ = result_; // pin = 1
    result_ = result_ && lc_exprs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RULE_BODY, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom argument_list clause_guard? rule_body
  public static boolean rule_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_clause")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<rule clause>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<rule clause>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    result_ = result_ && rule_clause_2(builder_, level_ + 1);
    result_ = result_ && rule_body(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RULE_CLAUSE, result_, false, null);
    return result_;
  }

  // clause_guard?
  private static boolean rule_clause_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_clause_2")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom ('/' integer | &('('))
  public static boolean spec_fun(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<spec fun>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<spec fun>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && spec_fun_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_SPEC_FUN, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '/' integer | &('(')
  private static boolean spec_fun_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = spec_fun_1_0(builder_, level_ + 1);
    if (!result_) result_ = spec_fun_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '/' integer
  private static boolean spec_fun_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    pinned_ = result_; // pin = 1
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // &('(')
  private static boolean spec_fun_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = spec_fun_1_1_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // ('(')
  private static boolean spec_fun_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'spec' type_spec
  public static boolean specification(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "specification")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<specification>");
    result_ = consumeToken(builder_, "spec");
    pinned_ = result_; // pin = 1
    result_ = result_ && type_spec(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_SPECIFICATION, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // string
  public static boolean string_literal(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "string_literal")) return false;
    if (!nextTokenIs(builder_, ERL_STRING)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_STRING);
    exit_section_(builder_, marker_, ERL_STRING_LITERAL, result_);
    return result_;
  }

  /* ********************************************************** */
  // ']' | '|' expression ']' | ',' expression tail
  static boolean tail(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) result_ = tail_1(builder_, level_ + 1);
    if (!result_) result_ = tail_2(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '|' expression ']'
  private static boolean tail_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_OR);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, expression(builder_, level_ + 1, -1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ',' expression tail
  private static boolean tail_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail_2")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, expression(builder_, level_ + 1, -1));
    result_ = pinned_ && tail(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // [q_var '::'] top_type_100_t
  public static boolean top_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = top_type_0(builder_, level_ + 1);
    result_ = result_ && top_type_100_t(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TOP_TYPE, result_, false, null);
    return result_;
  }

  // [q_var '::']
  private static boolean top_type_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_0")) return false;
    top_type_0_0(builder_, level_ + 1);
    return true;
  }

  // q_var '::'
  private static boolean top_type_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON_COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // type ['|' top_type_100_t]
  public static boolean top_type_100_t(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_100_t")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type_100_t_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TOP_TYPE_100_T, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['|' top_type_100_t]
  private static boolean top_type_100_t_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_100_t_1")) return false;
    top_type_100_t_1_0(builder_, level_ + 1);
    return true;
  }

  // '|' top_type_100_t
  private static boolean top_type_100_t_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_100_t_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_OR);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type_100_t(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '->' top_type
  public static boolean top_type_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_clause")) return false;
    if (!nextTokenIs(builder_, ERL_ARROW)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_ARROW);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TOP_TYPE_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // top_type (',' top_type)*
  static boolean top_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_list")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = top_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' top_type)*
  private static boolean top_type_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!top_type_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "top_type_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' top_type
  private static boolean top_type_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // [argument_definition ':'] argument_definition
  static boolean try_argument_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_argument_definition")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = try_argument_definition_0(builder_, level_ + 1);
    result_ = result_ && argument_definition(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [argument_definition ':']
  private static boolean try_argument_definition_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_argument_definition_0")) return false;
    try_argument_definition_0_0(builder_, level_ + 1);
    return true;
  }

  // argument_definition ':'
  private static boolean try_argument_definition_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_argument_definition_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = argument_definition(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // catch try_clauses [after try_expressions_clause] end | after try_expressions_clause end
  public static boolean try_catch(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch")) return false;
    if (!nextTokenIs(builder_, ERL_AFTER) && !nextTokenIs(builder_, ERL_CATCH)
        && replaceVariants(builder_, 2, "<try catch>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<try catch>");
    result_ = try_catch_0(builder_, level_ + 1);
    if (!result_) result_ = try_catch_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TRY_CATCH, result_, false, null);
    return result_;
  }

  // catch try_clauses [after try_expressions_clause] end
  private static boolean try_catch_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CATCH);
    pinned_ = result_; // pin = catch|after
    result_ = result_ && report_error_(builder_, try_clauses(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, try_catch_0_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [after try_expressions_clause]
  private static boolean try_catch_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0_2")) return false;
    try_catch_0_2_0(builder_, level_ + 1);
    return true;
  }

  // after try_expressions_clause
  private static boolean try_catch_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_AFTER);
    pinned_ = result_; // pin = catch|after
    result_ = result_ && try_expressions_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // after try_expressions_clause end
  private static boolean try_catch_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_AFTER);
    pinned_ = result_; // pin = catch|after
    result_ = result_ && report_error_(builder_, try_expressions_clause(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // <<guarded try_argument_definition>> clause_guard? clause_body
  public static boolean try_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clause")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<try clause>");
    result_ = guarded(builder_, level_ + 1, try_argument_definition_parser_);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, try_clause_1(builder_, level_ + 1));
    result_ = pinned_ && clause_body(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_TRY_CLAUSE, result_, pinned_, null);
    return result_ || pinned_;
  }

  // clause_guard?
  private static boolean try_clause_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clause_1")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // try_clause (';' try_clause)*
  public static boolean try_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clauses")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<try clauses>");
    result_ = try_clause(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && try_clauses_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TRY_CLAUSES, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' try_clause)*
  private static boolean try_clauses_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clauses_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!try_clauses_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "try_clauses_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' try_clause
  private static boolean try_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clauses_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && try_clause(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // try try_expressions_clause [of cr_clauses] try_catch
  public static boolean try_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression")) return false;
    if (!nextTokenIs(builder_, ERL_TRY)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_TRY);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, try_expressions_clause(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, try_expression_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && try_catch(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_TRY_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [of cr_clauses]
  private static boolean try_expression_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression_2")) return false;
    try_expression_2_0(builder_, level_ + 1);
    return true;
  }

  // of cr_clauses
  private static boolean try_expression_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_OF);
    result_ = result_ && cr_clauses(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // exprs
  public static boolean try_expressions_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expressions_clause")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<try expressions clause>");
    result_ = exprs(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TRY_EXPRESSIONS_CLAUSE, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // '{' (&'}' | exprs) '}'
  public static boolean tuple_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, tuple_expression_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_TUPLE_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // &'}' | exprs
  private static boolean tuple_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_expression_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = tuple_expression_1_0(builder_, level_ + 1);
    if (!result_) result_ = exprs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &'}'
  private static boolean tuple_expression_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_expression_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = consumeToken(builder_, ERL_CURLY_RIGHT);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // !('}'|',')
  static boolean tuple_recoverer(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_recoverer")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !tuple_recoverer_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // '}'|','
  private static boolean tuple_recoverer_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_recoverer_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_CURLY_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_COMMA);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // '(' top_type ')'
  //   | int_type ['..' int_type]
  //   | fun '(' fun_type_100_t? ')'
  //   | type_ref_with_module ['(' top_type_list? ')']
  //   | binary_type
  //   | q_var ['::' top_type]
  //   | '[' [top_type [',' '...']] ']'
  //   | '{' top_type_list? '}'
  //   | '#' record_ref '{' field_type_list? '}'
  public static boolean type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type>");
    result_ = type_0(builder_, level_ + 1);
    if (!result_) result_ = type_1(builder_, level_ + 1);
    if (!result_) result_ = type_2(builder_, level_ + 1);
    if (!result_) result_ = type_3(builder_, level_ + 1);
    if (!result_) result_ = binary_type(builder_, level_ + 1);
    if (!result_) result_ = type_5(builder_, level_ + 1);
    if (!result_) result_ = type_6(builder_, level_ + 1);
    if (!result_) result_ = type_7(builder_, level_ + 1);
    if (!result_) result_ = type_8(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TYPE, result_, false, null);
    return result_;
  }

  // '(' top_type ')'
  private static boolean type_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, top_type(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // int_type ['..' int_type]
  private static boolean type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = int_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_1_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['..' int_type]
  private static boolean type_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1_1")) return false;
    type_1_1_0(builder_, level_ + 1);
    return true;
  }

  // '..' int_type
  private static boolean type_1_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_DOT_DOT);
    pinned_ = result_; // pin = 1
    result_ = result_ && int_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // fun '(' fun_type_100_t? ')'
  private static boolean type_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_2")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_FUN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, type_2_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // fun_type_100_t?
  private static boolean type_2_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_2_2")) return false;
    fun_type_100_t(builder_, level_ + 1);
    return true;
  }

  // type_ref_with_module ['(' top_type_list? ')']
  private static boolean type_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = type_ref_with_module(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_3_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['(' top_type_list? ')']
  private static boolean type_3_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_1")) return false;
    type_3_1_0(builder_, level_ + 1);
    return true;
  }

  // '(' top_type_list? ')'
  private static boolean type_3_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, type_3_1_0_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // top_type_list?
  private static boolean type_3_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_1_0_1")) return false;
    top_type_list(builder_, level_ + 1);
    return true;
  }

  // q_var ['::' top_type]
  private static boolean type_5(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_5")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = q_var(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_5_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['::' top_type]
  private static boolean type_5_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_5_1")) return false;
    type_5_1_0(builder_, level_ + 1);
    return true;
  }

  // '::' top_type
  private static boolean type_5_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_5_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COLON_COLON);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '[' [top_type [',' '...']] ']'
  private static boolean type_6(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_6")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, type_6_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [top_type [',' '...']]
  private static boolean type_6_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_6_1")) return false;
    type_6_1_0(builder_, level_ + 1);
    return true;
  }

  // top_type [',' '...']
  private static boolean type_6_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_6_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = top_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_6_1_0_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // [',' '...']
  private static boolean type_6_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_6_1_0_1")) return false;
    type_6_1_0_1_0(builder_, level_ + 1);
    return true;
  }

  // ',' '...'
  private static boolean type_6_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_6_1_0_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && consumeToken(builder_, ERL_DOT_DOT_DOT);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // '{' top_type_list? '}'
  private static boolean type_7(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, type_7_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // top_type_list?
  private static boolean type_7_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7_1")) return false;
    top_type_list(builder_, level_ + 1);
    return true;
  }

  // '#' record_ref '{' field_type_list? '}'
  private static boolean type_8(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_8")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_RADIX);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, record_ref(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_CURLY_LEFT)) && result_;
    result_ = pinned_ && report_error_(builder_, type_8_3(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // field_type_list?
  private static boolean type_8_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_8_3")) return false;
    field_type_list(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // q_atom argument_definition_list '::' top_type
  static boolean type_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_body")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, argument_definition_list(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_COLON_COLON)) && result_;
    result_ = pinned_ && top_type(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '-' ('type'|'opaque') (type_body | '(' type_body ')')
  public static boolean type_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_definition")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && type_definition_1(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    result_ = result_ && type_definition_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TYPE_DEFINITION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // 'type'|'opaque'
  private static boolean type_definition_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_definition_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, "type");
    if (!result_) result_ = consumeToken(builder_, "opaque");
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // type_body | '(' type_body ')'
  private static boolean type_definition_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_definition_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = type_body(builder_, level_ + 1);
    if (!result_) result_ = type_definition_2_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '(' type_body ')'
  private static boolean type_definition_2_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_definition_2_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && type_body(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // q_atom '(' top_type_list ')' | top_type
  public static boolean type_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type guard>");
    result_ = type_guard_0(builder_, level_ + 1);
    if (!result_) result_ = top_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TYPE_GUARD, result_, false, null);
    return result_;
  }

  // q_atom '(' top_type_list ')'
  private static boolean type_guard_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && top_type_list(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // type_guard (',' type_guard)*
  static boolean type_guard_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard_list")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = type_guard(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_guard_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' type_guard)*
  private static boolean type_guard_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!type_guard_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "type_guard_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' type_guard
  private static boolean type_guard_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_guard(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean type_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_ref")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type ref>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<type ref>");
    result_ = q_atom(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TYPE_REF, result_, false, null);
    return result_;
  }

  /* ********************************************************** */
  // [module_ref ':'] type_ref
  static boolean type_ref_with_module(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_ref_with_module")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = type_ref_with_module_0(builder_, level_ + 1);
    result_ = result_ && type_ref(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // [module_ref ':']
  private static boolean type_ref_with_module_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_ref_with_module_0")) return false;
    type_ref_with_module_0_0(builder_, level_ + 1);
    return true;
  }

  // module_ref ':'
  private static boolean type_ref_with_module_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_ref_with_module_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = module_ref(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // fun_type type_sig_guard?
  public static boolean type_sig(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sig")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = fun_type(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_sig_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TYPE_SIG, result_, pinned_, null);
    return result_ || pinned_;
  }

  // type_sig_guard?
  private static boolean type_sig_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sig_1")) return false;
    type_sig_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // when type_guard_list
  public static boolean type_sig_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sig_guard")) return false;
    if (!nextTokenIs(builder_, ERL_WHEN)) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_WHEN);
    result_ = result_ && type_guard_list(builder_, level_ + 1);
    exit_section_(builder_, marker_, ERL_TYPE_SIG_GUARD, result_);
    return result_;
  }

  /* ********************************************************** */
  // type_sig (';' type_sig)*
  static boolean type_sigs_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs_list")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = type_sig(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_sigs_list_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (';' type_sig)*
  private static boolean type_sigs_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs_list_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!type_sigs_list_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "type_sigs_list_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ';' type_sig
  private static boolean type_sigs_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs_list_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_SEMI);
    pinned_ = result_; // pin = 1
    result_ = result_ && type_sig(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // fun_type_sigs_braces | fun_type_sigs
  static boolean type_spec(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_spec")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = fun_type_sigs_braces(builder_, level_ + 1);
    if (!result_) result_ = fun_type_sigs(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // expression (',' typed_record_fields | '::' top_type)
  public static boolean typed_attr_val(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<typed attr val>");
    result_ = expression(builder_, level_ + 1, -1);
    result_ = result_ && typed_attr_val_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_TYPED_ATTR_VAL, result_, false, null);
    return result_;
  }

  // ',' typed_record_fields | '::' top_type
  private static boolean typed_attr_val_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = typed_attr_val_1_0(builder_, level_ + 1);
    if (!result_) result_ = typed_attr_val_1_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ',' typed_record_fields
  private static boolean typed_attr_val_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && typed_record_fields(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // '::' top_type
  private static boolean typed_attr_val_1_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COLON_COLON);
    result_ = result_ && top_type(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // q_atom ['=' expression] ['::' top_type]
  public static boolean typed_expr(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_expr")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<typed expr>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<typed expr>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, typed_expr_1(builder_, level_ + 1));
    result_ = pinned_ && typed_expr_2(builder_, level_ + 1) && result_;
    exit_section_(builder_, level_, marker_, ERL_TYPED_EXPR, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['=' expression]
  private static boolean typed_expr_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_expr_1")) return false;
    typed_expr_1_0(builder_, level_ + 1);
    return true;
  }

  // '=' expression
  private static boolean typed_expr_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_expr_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_OP_EQ);
    pinned_ = result_; // pin = 1
    result_ = result_ && expression(builder_, level_ + 1, -1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // ['::' top_type]
  private static boolean typed_expr_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_expr_2")) return false;
    typed_expr_2_0(builder_, level_ + 1);
    return true;
  }

  // '::' top_type
  private static boolean typed_expr_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_expr_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COLON_COLON);
    pinned_ = result_; // pin = 1
    result_ = result_ && top_type(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // generic_function_call_expression | typed_expr
  static boolean typed_expr_or_macros(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_expr_or_macros")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = generic_function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = typed_expr(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  /* ********************************************************** */
  // typed_expr_or_macros (',' typed_expr_or_macros)*
  static boolean typed_exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_exprs")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = typed_expr_or_macros(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && typed_exprs_1(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  // (',' typed_expr_or_macros)*
  private static boolean typed_exprs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_exprs_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!typed_exprs_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "typed_exprs_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ',' typed_expr_or_macros
  private static boolean typed_exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_exprs_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_COMMA);
    pinned_ = result_; // pin = 1
    result_ = result_ && typed_expr_or_macros(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '{' typed_exprs? '}'
  public static boolean typed_record_fields(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_record_fields")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, typed_record_fields_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    exit_section_(builder_, level_, marker_, ERL_TYPED_RECORD_FIELDS, result_, pinned_, null);
    return result_ || pinned_;
  }

  // typed_exprs?
  private static boolean typed_record_fields_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_record_fields_1")) return false;
    typed_exprs(builder_, level_ + 1);
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
  // 9: PREFIX(prefix_expression)
  // 10: BINARY(colon_qualified_expression)
  // 11: ATOM(function_call_expression) ATOM(global_function_call_expression) ATOM(generic_function_call_expression) POSTFIX(anonymous_call_expression) POSTFIX(record_expression) ATOM(record2_expression) ATOM(qualified_expression)
  // 12: ATOM(max_expression)
  // 13: PREFIX(parenthesized_expression)
  public static boolean expression(PsiBuilder builder_, int level_, int priority_) {
    if (!recursion_guard_(builder_, level_, "expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = catch_expression(builder_, level_ + 1);
    if (!result_) result_ = prefix_expression(builder_, level_ + 1);
    if (!result_) result_ = function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = global_function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = generic_function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = record2_expression(builder_, level_ + 1);
    if (!result_) result_ = qualified_expression(builder_, level_ + 1);
    if (!result_) result_ = max_expression(builder_, level_ + 1);
    if (!result_) result_ = parenthesized_expression(builder_, level_ + 1);
    pinned_ = result_;
    result_ = result_ && expression_0(builder_, level_ + 1, priority_);
    exit_section_(builder_, level_, marker_, null, result_, pinned_, null);
    return result_ || pinned_;
  }

  public static boolean expression_0(PsiBuilder builder_, int level_, int priority_) {
    if (!recursion_guard_(builder_, level_, "expression_0")) return false;
    boolean result_ = true;
    while (true) {
      Marker left_marker_ = (Marker) builder_.getLatestDoneMarker();
      if (!invalid_left_marker_guard_(builder_, left_marker_, "expression_0")) return false;
      Marker marker_ = builder_.mark();
      if (priority_ < 1 && consumeToken(builder_, ERL_OP_EQ)) {
        result_ = report_error_(builder_, expression(builder_, level_, 0));
        marker_.drop();
        left_marker_.precede().done(ERL_ASSIGNMENT_EXPRESSION);
      }
      else if (priority_ < 2 && consumeToken(builder_, ERL_OP_EXL)) {
        result_ = report_error_(builder_, expression(builder_, level_, 1));
        marker_.drop();
        left_marker_.precede().done(ERL_SEND_EXPRESSION);
      }
      else if (priority_ < 3 && consumeToken(builder_, ERL_ORELSE)) {
        result_ = report_error_(builder_, expression(builder_, level_, 3));
        marker_.drop();
        left_marker_.precede().done(ERL_ORELSE_EXPRESSION);
      }
      else if (priority_ < 4 && consumeToken(builder_, ERL_ANDALSO)) {
        result_ = report_error_(builder_, expression(builder_, level_, 4));
        marker_.drop();
        left_marker_.precede().done(ERL_ANDALSO_EXPRESSION);
      }
      else if (priority_ < 5 && comp_op(builder_, level_ + 1)) {
        result_ = report_error_(builder_, expression(builder_, level_, 5));
        marker_.drop();
        left_marker_.precede().done(ERL_COMP_OP_EXPRESSION);
      }
      else if (priority_ < 6 && list_op(builder_, level_ + 1)) {
        result_ = report_error_(builder_, expression(builder_, level_, 6));
        marker_.drop();
        left_marker_.precede().done(ERL_LIST_OP_EXPRESSION);
      }
      else if (priority_ < 7 && add_op(builder_, level_ + 1)) {
        result_ = report_error_(builder_, expression(builder_, level_, 7));
        marker_.drop();
        left_marker_.precede().done(ERL_ADDITIVE_EXPRESSION);
      }
      else if (priority_ < 8 && multiplicative_expression_0(builder_, level_ + 1)) {
        result_ = report_error_(builder_, expression(builder_, level_, 8));
        marker_.drop();
        left_marker_.precede().done(ERL_MULTIPLICATIVE_EXPRESSION);
      }
      else if (priority_ < 10 && consumeToken(builder_, ERL_COLON)) {
        result_ = report_error_(builder_, expression(builder_, level_, 10));
        marker_.drop();
        left_marker_.precede().done(ERL_COLON_QUALIFIED_EXPRESSION);
      }
      else if (priority_ < 11 && anonymous_call_expression_0(builder_, level_ + 1)) {
        result_ = true;
        marker_.drop();
        left_marker_.precede().done(ERL_ANONYMOUS_CALL_EXPRESSION);
      }
      else if (priority_ < 11 && record_tail(builder_, level_ + 1)) {
        result_ = true;
        marker_.drop();
        left_marker_.precede().done(ERL_RECORD_EXPRESSION);
      }
      else {
        exit_section_(builder_, marker_, null, false);
        break;
      }
    }
    return result_;
  }

  // catch expression
  public static boolean catch_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "catch_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CATCH)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, null);
    result_ = consumeToken(builder_, ERL_CATCH);
    pinned_ = result_; // pin = 1
    result_ = result_ && expression(builder_, level_ + 1, -1);
    exit_section_(builder_, level_, marker_, ERL_CATCH_EXPRESSION, result_, pinned_, null);
    return result_ || pinned_;
  }

  // mult_op &(!(atom (',' | '>>')))
  private static boolean multiplicative_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "multiplicative_expression_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = mult_op(builder_, level_ + 1);
    result_ = result_ && multiplicative_expression_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &(!(atom (',' | '>>')))
  private static boolean multiplicative_expression_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "multiplicative_expression_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = multiplicative_expression_0_1_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // !(atom (',' | '>>'))
  private static boolean multiplicative_expression_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "multiplicative_expression_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !multiplicative_expression_0_1_0_0(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // atom (',' | '>>')
  private static boolean multiplicative_expression_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "multiplicative_expression_0_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_ATOM);
    result_ = result_ && multiplicative_expression_0_1_0_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // ',' | '>>'
  private static boolean multiplicative_expression_0_1_0_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "multiplicative_expression_0_1_0_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = consumeToken(builder_, ERL_COMMA);
    if (!result_) result_ = consumeToken(builder_, ERL_BIN_END);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  public static boolean prefix_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "prefix_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = prefix_op(builder_, level_ + 1);
    pinned_ = result_;
    result_ = pinned_ && expression(builder_, level_, 9) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_PREFIX_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // q_atom argument_list &not_function_definition
  public static boolean function_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_call_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    result_ = result_ && function_call_expression_2(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_FUNCTION_CALL_EXPRESSION, result_, false, null);
    return result_;
  }

  // &not_function_definition
  private static boolean function_call_expression_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_call_expression_2")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = not_function_definition(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // module_ref_or_dot_atom ':' function_call_expression
  public static boolean global_function_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "global_function_call_expression")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = module_ref_or_dot_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    result_ = result_ && function_call_expression(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_GLOBAL_FUNCTION_CALL_EXPRESSION, result_, false, null);
    return result_;
  }

  // [q_atom_or_var ':'] (q_atom_or_var | macros) argument_list &not_function_definition
  public static boolean generic_function_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = generic_function_call_expression_0(builder_, level_ + 1);
    result_ = result_ && generic_function_call_expression_1(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    result_ = result_ && generic_function_call_expression_3(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_GENERIC_FUNCTION_CALL_EXPRESSION, result_, false, null);
    return result_;
  }

  // [q_atom_or_var ':']
  private static boolean generic_function_call_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_0")) return false;
    generic_function_call_expression_0_0(builder_, level_ + 1);
    return true;
  }

  // q_atom_or_var ':'
  private static boolean generic_function_call_expression_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_atom_or_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // q_atom_or_var | macros
  private static boolean generic_function_call_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = q_atom_or_var(builder_, level_ + 1);
    if (!result_) result_ = macros(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &not_function_definition
  private static boolean generic_function_call_expression_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = not_function_definition(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // argument_list &not_function_definition
  private static boolean anonymous_call_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anonymous_call_expression_0")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_);
    result_ = argument_list(builder_, level_ + 1);
    result_ = result_ && anonymous_call_expression_0_1(builder_, level_ + 1);
    exit_section_(builder_, marker_, null, result_);
    return result_;
  }

  // &not_function_definition
  private static boolean anonymous_call_expression_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anonymous_call_expression_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _AND_, null);
    result_ = not_function_definition(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // record_tail
  public static boolean record2_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record2_expression")) return false;
    if (!nextTokenIs(builder_, ERL_RADIX) && !nextTokenIs(builder_, ERL_QMARK)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _COLLAPSE_, "<expression>");
    result_ = record_tail(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_RECORD_EXPRESSION, result_, false, null);
    return result_;
  }

  // q_atom '.' q_atom !'('
  public static boolean qualified_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NONE_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_DOT);
    result_ = result_ && q_atom(builder_, level_ + 1);
    result_ = result_ && qualified_expression_3(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_QUALIFIED_EXPRESSION, result_, false, null);
    return result_;
  }

  // !'('
  private static boolean qualified_expression_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_expression_3")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _NOT_, null);
    result_ = !consumeToken(builder_, ERL_PAR_LEFT);
    exit_section_(builder_, level_, marker_, null, result_, false, null);
    return result_;
  }

  // atomic
  //   | q_var
  //   | tuple_expression
  //   | list_atom_with_arity_expression
  //   | list_expression
  //   | case_expression
  //   | if_expression
  //   | binary_comprehension
  //   | list_comprehension
  //   | receive_expression
  //   | fun_expression
  //   | try_expression
  //   | binary_expression
  //   | begin_end_expression
  public static boolean max_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "max_expression")) return false;
    boolean result_ = false;
    Marker marker_ = enter_section_(builder_, level_, _COLLAPSE_, "<expression>");
    result_ = atomic(builder_, level_ + 1);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    if (!result_) result_ = tuple_expression(builder_, level_ + 1);
    if (!result_) result_ = list_atom_with_arity_expression(builder_, level_ + 1);
    if (!result_) result_ = list_expression(builder_, level_ + 1);
    if (!result_) result_ = case_expression(builder_, level_ + 1);
    if (!result_) result_ = if_expression(builder_, level_ + 1);
    if (!result_) result_ = binary_comprehension(builder_, level_ + 1);
    if (!result_) result_ = list_comprehension(builder_, level_ + 1);
    if (!result_) result_ = receive_expression(builder_, level_ + 1);
    if (!result_) result_ = fun_expression(builder_, level_ + 1);
    if (!result_) result_ = try_expression(builder_, level_ + 1);
    if (!result_) result_ = binary_expression(builder_, level_ + 1);
    if (!result_) result_ = begin_end_expression(builder_, level_ + 1);
    exit_section_(builder_, level_, marker_, ERL_MAX_EXPRESSION, result_, false, null);
    return result_;
  }

  public static boolean parenthesized_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenthesized_expression")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT) && replaceVariants(builder_, 1, "<expression>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_;
    result_ = pinned_ && expression(builder_, level_, -1) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_RIGHT)) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_PARENTHESIZED_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  final static Parser argument_definition_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return argument_definition(builder_, level_ + 1);
    }
  };
  final static Parser expression_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return expression(builder_, level_ + 1, -1);
    }
  };
  final static Parser exprs_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return exprs_recover(builder_, level_ + 1);
    }
  };
  final static Parser form_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return form_recover(builder_, level_ + 1);
    }
  };
  final static Parser guard_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return guard(builder_, level_ + 1);
    }
  };
  final static Parser macros_body_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return macros_body(builder_, level_ + 1);
    }
  };
  final static Parser macros_call_recover_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return macros_call_recover(builder_, level_ + 1);
    }
  };
  final static Parser try_argument_definition_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return try_argument_definition(builder_, level_ + 1);
    }
  };
  final static Parser tuple_recoverer_parser_ = new Parser() {
    public boolean parse(PsiBuilder builder_, int level_) {
      return tuple_recoverer(builder_, level_ + 1);
    }
  };
}
