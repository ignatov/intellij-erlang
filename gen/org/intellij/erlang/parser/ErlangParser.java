// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.parser;

import org.jetbrains.annotations.*;
import com.intellij.lang.LighterASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.openapi.diagnostic.Logger;
import static org.intellij.erlang.ErlangTypes.*;
import static org.intellij.erlang.parser.ErlangParserUtil.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class ErlangParser implements PsiParser {

  public static Logger LOG_ = Logger.getInstance("org.intellij.erlang.parser.ErlangParser");

  @NotNull
  public ASTNode parse(IElementType root_, PsiBuilder builder_) {
    int level_ = 0;
    boolean result_;
    builder_ = adapt_builder_(root_, builder_, this);
    if (root_ == ERL_ADDITIVE_EXPRESSION) {
      result_ = additive_expression(builder_, level_ + 1);
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
      result_ = assignment_expression(builder_, level_ + 1);
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
      result_ = colon_qualified_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_CR_CLAUSE) {
      result_ = cr_clause(builder_, level_ + 1);
    }
    else if (root_ == ERL_CR_CLAUSES) {
      result_ = cr_clauses(builder_, level_ + 1);
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
    else if (root_ == ERL_EXPR_100_A) {
      result_ = expr_100_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_150_A) {
      result_ = expr_150_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_160_A) {
      result_ = expr_160_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_200_A) {
      result_ = expr_200_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_300_A) {
      result_ = expr_300_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_400_A) {
      result_ = expr_400_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_500_A) {
      result_ = expr_500_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPR_700_A) {
      result_ = expr_700_a(builder_, level_ + 1);
    }
    else if (root_ == ERL_EXPRESSION) {
      result_ = expression(builder_, level_ + 1);
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
    else if (root_ == ERL_MACROS) {
      result_ = macros(builder_, level_ + 1);
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
    else if (root_ == ERL_MODULE) {
      result_ = module(builder_, level_ + 1);
    }
    else if (root_ == ERL_MODULE_REF) {
      result_ = module_ref(builder_, level_ + 1);
    }
    else if (root_ == ERL_MULTIPLICATIVE_EXPRESSION) {
      result_ = multiplicative_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_OPT_BIT_TYPE_LIST) {
      result_ = opt_bit_type_list(builder_, level_ + 1);
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
    else if (root_ == ERL_QUERY_EXPRESSION) {
      result_ = query_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECEIVE_EXPRESSION) {
      result_ = receive_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_DEFINITION) {
      result_ = record_definition(builder_, level_ + 1);
    }
    else if (root_ == ERL_RECORD_EXPRESSION) {
      result_ = record_expression(builder_, level_ + 1);
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
      result_ = send_expression(builder_, level_ + 1);
    }
    else if (root_ == ERL_SPEC_FUN) {
      result_ = spec_fun(builder_, level_ + 1);
    }
    else if (root_ == ERL_SPECIFICATION) {
      result_ = specification(builder_, level_ + 1);
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
    else if (root_ == ERL_TYPE_GUARD) {
      result_ = type_guard(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_SIG) {
      result_ = type_sig(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_SIG_GUARD) {
      result_ = type_sig_guard(builder_, level_ + 1);
    }
    else if (root_ == ERL_TYPE_SPEC) {
      result_ = type_spec(builder_, level_ + 1);
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
      Marker marker_ = builder_.mark();
      result_ = parse_root_(root_, builder_, level_);
      while (builder_.getTokenType() != null) {
        builder_.advanceLexer();
      }
      marker_.done(root_);
    }
    return builder_.getTreeBuilt();
  }

  protected boolean parse_root_(final IElementType root_, final PsiBuilder builder_, final int level_) {
    return forms(builder_, level_ + 1);
  }

  private static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    TokenSet.create(ERL_ADDITIVE_EXPRESSION, ERL_ASSIGNMENT_EXPRESSION, ERL_BEGIN_END_EXPRESSION, ERL_BINARY_EXPRESSION,
      ERL_CASE_EXPRESSION, ERL_CATCH_EXPRESSION, ERL_COLON_QUALIFIED_EXPRESSION, ERL_EXPRESSION,
      ERL_EXPR_100_A, ERL_EXPR_150_A, ERL_EXPR_160_A, ERL_EXPR_200_A,
      ERL_EXPR_300_A, ERL_EXPR_400_A, ERL_EXPR_500_A, ERL_EXPR_700_A,
      ERL_FUNCTION_CALL_EXPRESSION, ERL_FUN_EXPRESSION, ERL_GENERIC_FUNCTION_CALL_EXPRESSION, ERL_GLOBAL_FUNCTION_CALL_EXPRESSION,
      ERL_IF_EXPRESSION, ERL_LC_EXPRESSION, ERL_LIST_COMPREHENSION, ERL_LIST_EXPRESSION,
      ERL_MAX_EXPRESSION, ERL_MULTIPLICATIVE_EXPRESSION, ERL_PARENTHESIZED_EXPRESSION, ERL_PREFIX_EXPRESSION,
      ERL_QUALIFIED_EXPRESSION, ERL_QUERY_EXPRESSION, ERL_RECEIVE_EXPRESSION, ERL_RECORD_EXPRESSION,
      ERL_SEND_EXPRESSION, ERL_TRY_EXPRESSION, ERL_TUPLE_EXPRESSION),
  };
  public static boolean type_extends_(IElementType child_, IElementType parent_) {
    for (TokenSet set : EXTENDS_SETS_) {
      if (set.contains(child_) && set.contains(parent_)) return true;
    }
    return false;
  }

  /* ********************************************************** */
  // '+' |'-' | bor | bxor | bsl | bsr | or | xor
  static boolean add_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "add_op")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_BOR);
    if (!result_) result_ = consumeToken(builder_, ERL_BXOR);
    if (!result_) result_ = consumeToken(builder_, ERL_BSL);
    if (!result_) result_ = consumeToken(builder_, ERL_BSR);
    if (!result_) result_ = consumeToken(builder_, ERL_OR);
    if (!result_) result_ = consumeToken(builder_, ERL_XOR);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // add_op expr_500_a
  public static boolean additive_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "additive_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker left_marker_ = (Marker)builder_.getLatestDoneMarker();
    if (!invalid_left_marker_guard_(builder_, left_marker_, "additive_expression")) return false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = add_op(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && expr_500_a(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.drop();
      left_marker_.precede().done(ERL_ADDITIVE_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // (fun_expression|parenthesized_expression) argument_list
  static boolean anonymous_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anonymous_call_expression")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT) && !nextTokenIs(builder_, ERL_FUN)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = anonymous_call_expression_0(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    if (!result_ && !pinned_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (fun_expression|parenthesized_expression)
  private static boolean anonymous_call_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anonymous_call_expression_0")) return false;
    return anonymous_call_expression_0_0(builder_, level_ + 1);
  }

  // fun_expression|parenthesized_expression
  private static boolean anonymous_call_expression_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "anonymous_call_expression_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = fun_expression(builder_, level_ + 1);
    if (!result_) result_ = parenthesized_expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expression
  public static boolean argument_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<argument definition>");
    result_ = expression(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_ARGUMENT_DEFINITION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // '(' argument_definition? (',' argument_definition)* ')'
  public static boolean argument_definition_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && argument_definition_list_1(builder_, level_ + 1);
    result_ = result_ && argument_definition_list_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (result_) {
      marker_.done(ERL_ARGUMENT_DEFINITION_LIST);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
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

  // (',' argument_definition)
  private static boolean argument_definition_list_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list_2_0")) return false;
    return argument_definition_list_2_0_0(builder_, level_ + 1);
  }

  // ',' argument_definition
  private static boolean argument_definition_list_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_definition_list_2_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && argument_definition(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '(' exprs? ')'
  public static boolean argument_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_list")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && argument_list_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (result_) {
      marker_.done(ERL_ARGUMENT_LIST);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  // exprs?
  private static boolean argument_list_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "argument_list_1")) return false;
    exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '=' expr_100_a
  public static boolean assignment_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "assignment_expression")) return false;
    if (!nextTokenIs(builder_, ERL_OP_EQ)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker left_marker_ = (Marker)builder_.getLatestDoneMarker();
    if (!invalid_left_marker_guard_(builder_, left_marker_, "assignment_expression")) return false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_EQ);
    pinned_ = result_; // pin = 1
    result_ = result_ && expr_100_a(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.drop();
      left_marker_.precede().done(ERL_ASSIGNMENT_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom [('(' typed_attr_val ')') | typed_attr_val | attr_val]
  public static boolean atom_attribute(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<attribute>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<attribute>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && atom_attribute_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_ATOM_ATTRIBUTE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [('(' typed_attr_val ')') | typed_attr_val | attr_val]
  private static boolean atom_attribute_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1")) return false;
    atom_attribute_1_0(builder_, level_ + 1);
    return true;
  }

  // ('(' typed_attr_val ')') | typed_attr_val | attr_val
  private static boolean atom_attribute_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = atom_attribute_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = typed_attr_val(builder_, level_ + 1);
    if (!result_) result_ = attr_val(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('(' typed_attr_val ')')
  private static boolean atom_attribute_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1_0_0")) return false;
    return atom_attribute_1_0_0_0(builder_, level_ + 1);
  }

  // '(' typed_attr_val ')'
  private static boolean atom_attribute_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_attribute_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && typed_attr_val(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean atom_with_arity_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atom_with_arity_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OP_AR_DIV);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // char | integer | float | q_atom | string+
  static boolean atomic(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atomic")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_CHAR);
    if (!result_) result_ = consumeToken(builder_, ERL_INTEGER);
    if (!result_) result_ = consumeToken(builder_, ERL_FLOAT);
    if (!result_) result_ = q_atom(builder_, level_ + 1);
    if (!result_) result_ = atomic_4(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // string+
  private static boolean atomic_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "atomic_4")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_STRING);
    int offset_ = builder_.getCurrentOffset();
    while (result_) {
      if (!consumeToken(builder_, ERL_STRING)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "atomic_4");
        break;
      }
      offset_ = next_offset_;
    }
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // ('(' exprs ')') | exprs
  public static boolean attr_val(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attr_val")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<attribute value>");
    result_ = attr_val_0(builder_, level_ + 1);
    if (!result_) result_ = exprs(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_ATTR_VAL);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // ('(' exprs ')')
  private static boolean attr_val_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attr_val_0")) return false;
    return attr_val_0_0(builder_, level_ + 1);
  }

  // '(' exprs ')'
  private static boolean attr_val_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attr_val_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && exprs(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '-' (
  //   module
  //   | export
  //   | export_type_attribute
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
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    pinned_ = result_; // pin = 1
    result_ = result_ && attribute_1(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_ATTRIBUTE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (
  //   module
  //   | export
  //   | export_type_attribute
  //   | specification
  //   | callback_spec
  //   | behaviour
  //   | atom_attribute
  //   )
  private static boolean attribute_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attribute_1")) return false;
    return attribute_1_0(builder_, level_ + 1);
  }

  // module
  //   | export
  //   | export_type_attribute
  //   | specification
  //   | callback_spec
  //   | behaviour
  //   | atom_attribute
  private static boolean attribute_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "attribute_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = module(builder_, level_ + 1);
    if (!result_) result_ = export(builder_, level_ + 1);
    if (!result_) result_ = export_type_attribute(builder_, level_ + 1);
    if (!result_) result_ = specification(builder_, level_ + 1);
    if (!result_) result_ = callback_spec(builder_, level_ + 1);
    if (!result_) result_ = behaviour(builder_, level_ + 1);
    if (!result_) result_ = atom_attribute(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // exprs
  public static boolean begin_end_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "begin_end_body")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = exprs(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_BEGIN_END_BODY);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // begin begin_end_body end
  public static boolean begin_end_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "begin_end_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BEGIN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BEGIN);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, begin_end_body(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_BEGIN_END_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_BEGIN_END_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'behaviour' '(' q_atom ')'
  public static boolean behaviour(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "behaviour")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<behaviour>");
    result_ = consumeToken(builder_, "behaviour");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, q_atom(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_BEHAVIOUR);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_var ':' integer
  public static boolean bin_base_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_base_type")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    if (result_) {
      marker_.done(ERL_BIN_BASE_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // bin_base_type ',' bin_unit_type
  static boolean bin_base_types(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_base_types")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = bin_base_type(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COMMA);
    result_ = result_ && bin_unit_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // prefix_op? max_expression (colon_max_expression)? opt_bit_type_list?
  public static boolean bin_element(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<binary element>");
    result_ = bin_element_0(builder_, level_ + 1);
    result_ = result_ && max_expression(builder_, level_ + 1);
    result_ = result_ && bin_element_2(builder_, level_ + 1);
    result_ = result_ && bin_element_3(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_BIN_ELEMENT);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // prefix_op?
  private static boolean bin_element_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_0")) return false;
    prefix_op(builder_, level_ + 1);
    return true;
  }

  // (colon_max_expression)?
  private static boolean bin_element_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_2")) return false;
    bin_element_2_0(builder_, level_ + 1);
    return true;
  }

  // (colon_max_expression)
  private static boolean bin_element_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_element_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = colon_max_expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
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
  static boolean bin_elements(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_elements")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = bin_element(builder_, level_ + 1);
    result_ = result_ && bin_elements_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' bin_element)*
  private static boolean bin_elements_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_elements_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!bin_elements_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "bin_elements_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' bin_element)
  private static boolean bin_elements_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_elements_1_0")) return false;
    return bin_elements_1_0_0(builder_, level_ + 1);
  }

  // ',' bin_element
  private static boolean bin_elements_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_elements_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && bin_element(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_var ':' q_var '*' integer
  public static boolean bin_unit_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bin_unit_type")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    result_ = result_ && q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OP_AR_MUL);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    if (result_) {
      marker_.done(ERL_BIN_UNIT_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // '<<' binary_expression '||' lc_exprs '>>'
  public static boolean binary_comprehension(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_comprehension")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BIN_START);
    result_ = result_ && binary_expression(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OR_OR);
    pinned_ = result_; // pin = 3
    result_ = result_ && report_error_(builder_, lc_exprs(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BIN_END) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_LIST_COMPREHENSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_LIST_COMPREHENSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '<<' bin_elements? '>>'
  public static boolean binary_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BIN_START);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, binary_expression_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BIN_END) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_BINARY_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_BINARY_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // bin_elements?
  private static boolean binary_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_expression_1")) return false;
    bin_elements(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '<<' [bin_base_type | bin_unit_type | bin_base_types] '>>'
  public static boolean binary_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_type")) return false;
    if (!nextTokenIs(builder_, ERL_BIN_START)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BIN_START);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, binary_type_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BIN_END) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_BINARY_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // [bin_base_type | bin_unit_type | bin_base_types]
  private static boolean binary_type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_type_1")) return false;
    binary_type_1_0(builder_, level_ + 1);
    return true;
  }

  // bin_base_type | bin_unit_type | bin_base_types
  private static boolean binary_type_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "binary_type_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = bin_base_type(builder_, level_ + 1);
    if (!result_) result_ = bin_unit_type(builder_, level_ + 1);
    if (!result_) result_ = bin_base_types(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom [':' integer]
  public static boolean bit_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && bit_type_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_BIT_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
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
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COLON);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // bit_type ('-' bit_type)*
  static boolean bit_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = bit_type(builder_, level_ + 1);
    result_ = result_ && bit_type_list_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
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

  // ('-' bit_type)
  private static boolean bit_type_list_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_list_1_0")) return false;
    return bit_type_list_1_0_0(builder_, level_ + 1);
  }

  // '-' bit_type
  private static boolean bit_type_list_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "bit_type_list_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && bit_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // 'callback' type_spec
  public static boolean callback_spec(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "callback_spec")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<callback spec>");
    result_ = consumeToken(builder_, "callback");
    pinned_ = result_; // pin = 1
    result_ = result_ && type_spec(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_CALLBACK_SPEC);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // case expression of cr_clauses end
  public static boolean case_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "case_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CASE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_CASE);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, expression(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_OF)) && result_;
    result_ = pinned_ && report_error_(builder_, cr_clauses(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_CASE_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_CASE_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // catch expression
  public static boolean catch_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "catch_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CATCH)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_CATCH);
    pinned_ = result_; // pin = 1
    result_ = result_ && expression(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_CATCH_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_CATCH_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '->' exprs
  public static boolean clause_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "clause_body")) return false;
    if (!nextTokenIs(builder_, ERL_ARROW)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_ARROW);
    pinned_ = result_; // pin = 1
    result_ = result_ && exprs(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_CLAUSE_BODY);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // when guard
  public static boolean clause_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "clause_guard")) return false;
    if (!nextTokenIs(builder_, ERL_WHEN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_WHEN);
    pinned_ = result_; // pin = 1
    result_ = result_ && guard(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_CLAUSE_GUARD);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // ':' max_expression
  static boolean colon_max_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "colon_max_expression")) return false;
    if (!nextTokenIs(builder_, ERL_COLON)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_COLON);
    pinned_ = result_; // pin = 1
    result_ = result_ && max_expression(builder_, level_ + 1);
    if (!result_ && !pinned_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // (max_expression | qualified_reference) colon_max_expression?
  public static boolean colon_qualified_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "colon_qualified_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = colon_qualified_expression_0(builder_, level_ + 1);
    result_ = result_ && colon_qualified_expression_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_COLON_QUALIFIED_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_COLON_QUALIFIED_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (max_expression | qualified_reference)
  private static boolean colon_qualified_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "colon_qualified_expression_0")) return false;
    return colon_qualified_expression_0_0(builder_, level_ + 1);
  }

  // max_expression | qualified_reference
  private static boolean colon_qualified_expression_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "colon_qualified_expression_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = max_expression(builder_, level_ + 1);
    if (!result_) result_ = qualified_reference(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // colon_max_expression?
  private static boolean colon_qualified_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "colon_qualified_expression_1")) return false;
    colon_max_expression(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '==' | '/=' |'=<' |'<' |'>=' |'>' |'=:=' |'=/='
  static boolean comp_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "comp_op")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_EQ_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_DIV_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_EQ_LT);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_LT);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_GT_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_GT);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_EQ_COL_EQ);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_EQ_DIV_EQ);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // argument_definition clause_guard? clause_body
  public static boolean cr_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clause")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<cr clause>");
    result_ = argument_definition(builder_, level_ + 1);
    result_ = result_ && cr_clause_1(builder_, level_ + 1);
    result_ = result_ && clause_body(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_CR_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // clause_guard?
  private static boolean cr_clause_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clause_1")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // cr_clause (';' cr_clause)*
  public static boolean cr_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clauses")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<cr clauses>");
    result_ = cr_clause(builder_, level_ + 1);
    result_ = result_ && cr_clauses_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_CR_CLAUSES);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (';' cr_clause)
  private static boolean cr_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clauses_1_0")) return false;
    return cr_clauses_1_0_0(builder_, level_ + 1);
  }

  // ';' cr_clause
  private static boolean cr_clauses_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "cr_clauses_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && cr_clause(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // 'export' '(' export_functions ')'
  public static boolean export(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<export>");
    result_ = consumeToken(builder_, "export");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, export_functions(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_EXPORT);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
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
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<export function>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_EXPORT_FUNCTION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' [export_function (',' export_function)*] ']'
  public static boolean export_functions(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, export_functions_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_EXPORT_FUNCTIONS);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // [export_function (',' export_function)*]
  private static boolean export_functions_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions_1")) return false;
    export_functions_1_0(builder_, level_ + 1);
    return true;
  }

  // export_function (',' export_function)*
  private static boolean export_functions_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = export_function(builder_, level_ + 1);
    result_ = result_ && export_functions_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' export_function)*
  private static boolean export_functions_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions_1_0_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!export_functions_1_0_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "export_functions_1_0_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' export_function)
  private static boolean export_functions_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions_1_0_1_0")) return false;
    return export_functions_1_0_1_0_0(builder_, level_ + 1);
  }

  // ',' export_function
  private static boolean export_functions_1_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_functions_1_0_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && export_function(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom '/' integer
  public static boolean export_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_EXPORT_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // 'export_type' '(' export_types ')'
  public static boolean export_type_attribute(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_type_attribute")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<attribute>");
    result_ = consumeToken(builder_, "export_type");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, export_types(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_EXPORT_TYPE_ATTRIBUTE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' [export_type (',' export_type)*] ']'
  public static boolean export_types(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, export_types_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_EXPORT_TYPES);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // [export_type (',' export_type)*]
  private static boolean export_types_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types_1")) return false;
    export_types_1_0(builder_, level_ + 1);
    return true;
  }

  // export_type (',' export_type)*
  private static boolean export_types_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = export_type(builder_, level_ + 1);
    result_ = result_ && export_types_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' export_type)*
  private static boolean export_types_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types_1_0_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!export_types_1_0_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "export_types_1_0_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' export_type)
  private static boolean export_types_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types_1_0_1_0")) return false;
    return export_types_1_0_1_0_0(builder_, level_ + 1);
  }

  // ',' export_type
  private static boolean export_types_1_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "export_types_1_0_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && export_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expr_150_a [assignment_expression|send_expression]
  public static boolean expr_100_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_100_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expr_150_a(builder_, level_ + 1);
    result_ = result_ && expr_100_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_100_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_100_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [assignment_expression|send_expression]
  private static boolean expr_100_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_100_a_1")) return false;
    expr_100_a_1_0(builder_, level_ + 1);
    return true;
  }

  // assignment_expression|send_expression
  private static boolean expr_100_a_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_100_a_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = assignment_expression(builder_, level_ + 1);
    if (!result_) result_ = send_expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expr_160_a [orelse expr_150_a]
  public static boolean expr_150_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_150_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expr_160_a(builder_, level_ + 1);
    result_ = result_ && expr_150_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_150_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_150_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [orelse expr_150_a]
  private static boolean expr_150_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_150_a_1")) return false;
    expr_150_a_1_0(builder_, level_ + 1);
    return true;
  }

  // orelse expr_150_a
  private static boolean expr_150_a_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_150_a_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_ORELSE);
    result_ = result_ && expr_150_a(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expr_200_a [andalso expr_160_a]
  public static boolean expr_160_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_160_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expr_200_a(builder_, level_ + 1);
    result_ = result_ && expr_160_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_160_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_160_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [andalso expr_160_a]
  private static boolean expr_160_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_160_a_1")) return false;
    expr_160_a_1_0(builder_, level_ + 1);
    return true;
  }

  // andalso expr_160_a
  private static boolean expr_160_a_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_160_a_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_ANDALSO);
    result_ = result_ && expr_160_a(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expr_300_a [comp_op expr_300_a]
  public static boolean expr_200_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_200_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expr_300_a(builder_, level_ + 1);
    result_ = result_ && expr_200_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_200_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_200_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [comp_op expr_300_a]
  private static boolean expr_200_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_200_a_1")) return false;
    expr_200_a_1_0(builder_, level_ + 1);
    return true;
  }

  // comp_op expr_300_a
  private static boolean expr_200_a_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_200_a_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = comp_op(builder_, level_ + 1);
    result_ = result_ && expr_300_a(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expr_400_a [list_op expr_300_a]
  public static boolean expr_300_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_300_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expr_400_a(builder_, level_ + 1);
    result_ = result_ && expr_300_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_300_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_300_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [list_op expr_300_a]
  private static boolean expr_300_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_300_a_1")) return false;
    expr_300_a_1_0(builder_, level_ + 1);
    return true;
  }

  // list_op expr_300_a
  private static boolean expr_300_a_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_300_a_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = list_op(builder_, level_ + 1);
    result_ = result_ && expr_300_a(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expr_500_a additive_expression*
  public static boolean expr_400_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_400_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expr_500_a(builder_, level_ + 1);
    result_ = result_ && expr_400_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_400_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_400_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // additive_expression*
  private static boolean expr_400_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_400_a_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!additive_expression(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "expr_400_a_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  /* ********************************************************** */
  // prefix_expression multiplicative_expression*
  public static boolean expr_500_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_500_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = prefix_expression(builder_, level_ + 1);
    result_ = result_ && expr_500_a_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_500_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_500_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // multiplicative_expression*
  private static boolean expr_500_a_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_500_a_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!multiplicative_expression(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "expr_500_a_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  /* ********************************************************** */
  // global_function_call_expression | function_call_expression | generic_function_call_expression |
  //   anonymous_call_expression | record_expression+ | colon_qualified_expression
  public static boolean expr_700_a(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_700_a")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = global_function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = generic_function_call_expression(builder_, level_ + 1);
    if (!result_) result_ = anonymous_call_expression(builder_, level_ + 1);
    if (!result_) result_ = expr_700_a_4(builder_, level_ + 1);
    if (!result_) result_ = colon_qualified_expression(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPR_700_A)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPR_700_A);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // record_expression+
  private static boolean expr_700_a_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expr_700_a_4")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = record_expression(builder_, level_ + 1);
    int offset_ = builder_.getCurrentOffset();
    while (result_) {
      if (!record_expression(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "expr_700_a_4");
        break;
      }
      offset_ = next_offset_;
    }
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom_or_var&(',' | ')' | '->' | '}' | ']' | ';') | catch_expression | expr_100_a
  public static boolean expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expression")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = expression_0(builder_, level_ + 1);
    if (!result_) result_ = catch_expression(builder_, level_ + 1);
    if (!result_) result_ = expr_100_a(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // q_atom_or_var&(',' | ')' | '->' | '}' | ']' | ';')
  private static boolean expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expression_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom_or_var(builder_, level_ + 1);
    result_ = result_ && expression_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // &(',' | ')' | '->' | '}' | ']' | ';')
  private static boolean expression_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expression_0_1")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_AND_, null);
    result_ = expression_0_1_0(builder_, level_ + 1);
    marker_.rollbackTo();
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_AND_, null);
    return result_;
  }

  // (',' | ')' | '->' | '}' | ']' | ';')
  private static boolean expression_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expression_0_1_0")) return false;
    return expression_0_1_0_0(builder_, level_ + 1);
  }

  // ',' | ')' | '->' | '}' | ']' | ';'
  private static boolean expression_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "expression_0_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    if (!result_) result_ = consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_ARROW);
    if (!result_) result_ = consumeToken(builder_, ERL_CURLY_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_SEMI);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expression (',' expression)*
  static boolean exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = expression(builder_, level_ + 1);
    result_ = result_ && exprs_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' expression)*
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

  // (',' expression)
  private static boolean exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_1_0")) return false;
    return exprs_1_0_0(builder_, level_ + 1);
  }

  // ',' expression
  private static boolean exprs_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "exprs_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom '::' top_type
  public static boolean field_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_type")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<type>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON_COLON);
    result_ = result_ && top_type(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FIELD_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // field_type (',' field_type)*
  static boolean field_types(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_types")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = field_type(builder_, level_ + 1);
    result_ = result_ && field_types_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' field_type)*
  private static boolean field_types_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_types_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!field_types_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "field_types_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' field_type)
  private static boolean field_types_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_types_1_0")) return false;
    return field_types_1_0_0(builder_, level_ + 1);
  }

  // ',' field_type
  private static boolean field_types_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "field_types_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && field_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // function | rule | record_definition | include | macros_definition | attribute
  static boolean form(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "form")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_RECOVER_, null);
    result_ = function(builder_, level_ + 1);
    if (!result_) result_ = rule(builder_, level_ + 1);
    if (!result_) result_ = record_definition(builder_, level_ + 1);
    if (!result_) result_ = include(builder_, level_ + 1);
    if (!result_) result_ = macros_definition(builder_, level_ + 1);
    if (!result_) result_ = attribute(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_RECOVER_, recoverer_parser_);
    return result_;
  }

  /* ********************************************************** */
  // form ('.' form)*
  static boolean forms(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "forms")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = form(builder_, level_ + 1);
    result_ = result_ && forms_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('.' form)*
  private static boolean forms_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "forms_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!forms_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "forms_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // ('.' form)
  private static boolean forms_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "forms_1_0")) return false;
    return forms_1_0_0(builder_, level_ + 1);
  }

  // '.' form
  private static boolean forms_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "forms_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_DOT);
    result_ = result_ && form(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // argument_definition_list clause_guard? clause_body
  public static boolean fun_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clause")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = argument_definition_list(builder_, level_ + 1);
    result_ = result_ && fun_clause_1(builder_, level_ + 1);
    result_ = result_ && clause_body(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FUN_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
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
    Marker marker_ = builder_.mark();
    result_ = fun_clause(builder_, level_ + 1);
    result_ = result_ && fun_clauses_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FUN_CLAUSES);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
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

  // (';' fun_clause)
  private static boolean fun_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clauses_1_0")) return false;
    return fun_clauses_1_0_0(builder_, level_ + 1);
  }

  // ';' fun_clause
  private static boolean fun_clauses_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_clauses_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && fun_clause(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // fun ((fun_clauses end) | ([module_ref ':'] function_with_arity))
  public static boolean fun_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression")) return false;
    if (!nextTokenIs(builder_, ERL_FUN)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_FUN);
    pinned_ = result_; // pin = 1
    result_ = result_ && fun_expression_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_FUN_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_FUN_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // ((fun_clauses end) | ([module_ref ':'] function_with_arity))
  private static boolean fun_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1")) return false;
    return fun_expression_1_0(builder_, level_ + 1);
  }

  // (fun_clauses end) | ([module_ref ':'] function_with_arity)
  private static boolean fun_expression_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = fun_expression_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = fun_expression_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (fun_clauses end)
  private static boolean fun_expression_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0_0")) return false;
    return fun_expression_1_0_0_0(builder_, level_ + 1);
  }

  // fun_clauses end
  private static boolean fun_expression_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = fun_clauses(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_END);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ([module_ref ':'] function_with_arity)
  private static boolean fun_expression_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0_1")) return false;
    return fun_expression_1_0_1_0(builder_, level_ + 1);
  }

  // [module_ref ':'] function_with_arity
  private static boolean fun_expression_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = fun_expression_1_0_1_0_0(builder_, level_ + 1);
    result_ = result_ && function_with_arity(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // [module_ref ':']
  private static boolean fun_expression_1_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0_1_0_0")) return false;
    fun_expression_1_0_1_0_0_0(builder_, level_ + 1);
    return true;
  }

  // module_ref ':'
  private static boolean fun_expression_1_0_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_expression_1_0_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = module_ref(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '(' top_types? ')' top_type_clause
  public static boolean fun_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && fun_type_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    result_ = result_ && top_type_clause(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FUN_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  // top_types?
  private static boolean fun_type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_1")) return false;
    top_types(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '(' ('...' | top_types?) ')' top_type_clause
  public static boolean fun_type_100_t(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && fun_type_100_t_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    result_ = result_ && top_type_clause(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FUN_TYPE_100_T);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  // ('...' | top_types?)
  private static boolean fun_type_100_t_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t_1")) return false;
    return fun_type_100_t_1_0(builder_, level_ + 1);
  }

  // '...' | top_types?
  private static boolean fun_type_100_t_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_DOT_DOT_DOT);
    if (!result_) result_ = fun_type_100_t_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // top_types?
  private static boolean fun_type_100_t_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "fun_type_100_t_1_0_1")) return false;
    top_types(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // function_clause (';' function_clause)*
  public static boolean function(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<function>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<function>");
    result_ = function_clause(builder_, level_ + 1);
    result_ = result_ && function_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FUNCTION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (';' function_clause)
  private static boolean function_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_1_0")) return false;
    return function_1_0_0(builder_, level_ + 1);
  }

  // ';' function_clause
  private static boolean function_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && function_clause(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom argument_list
  public static boolean function_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_call_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_FUNCTION_CALL_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_FUNCTION_CALL_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom argument_definition_list clause_guard? clause_body
  public static boolean function_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_clause")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<function clause>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<function clause>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && argument_definition_list(builder_, level_ + 1);
    result_ = result_ && function_clause_2(builder_, level_ + 1);
    result_ = result_ && clause_body(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_FUNCTION_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // clause_guard?
  private static boolean function_clause_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "function_clause_2")) return false;
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
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<function with arity>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_AR_DIV));
    result_ = pinned_ && consumeToken(builder_, ERL_INTEGER) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_FUNCTION_WITH_ARITY);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // (q_atom_or_var ':')? q_atom_or_var argument_list
  public static boolean generic_function_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = generic_function_call_expression_0(builder_, level_ + 1);
    result_ = result_ && q_atom_or_var(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_GENERIC_FUNCTION_CALL_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_GENERIC_FUNCTION_CALL_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (q_atom_or_var ':')?
  private static boolean generic_function_call_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_0")) return false;
    generic_function_call_expression_0_0(builder_, level_ + 1);
    return true;
  }

  // (q_atom_or_var ':')
  private static boolean generic_function_call_expression_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_0_0")) return false;
    return generic_function_call_expression_0_0_0(builder_, level_ + 1);
  }

  // q_atom_or_var ':'
  private static boolean generic_function_call_expression_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "generic_function_call_expression_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom_or_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // module_ref ':' function_call_expression
  public static boolean global_function_call_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "global_function_call_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = module_ref(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    result_ = result_ && function_call_expression(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_GLOBAL_FUNCTION_CALL_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_GLOBAL_FUNCTION_CALL_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // exprs (';' exprs)*
  public static boolean guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<guard>");
    result_ = exprs(builder_, level_ + 1);
    result_ = result_ && guard_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_GUARD);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (';' exprs)
  private static boolean guard_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard_1_0")) return false;
    return guard_1_0_0(builder_, level_ + 1);
  }

  // ';' exprs
  private static boolean guard_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "guard_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && exprs(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // guard clause_body
  public static boolean if_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clause")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<if clause>");
    result_ = guard(builder_, level_ + 1);
    result_ = result_ && clause_body(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_IF_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // if_clause (';' if_clause)*
  public static boolean if_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clauses")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<if clauses>");
    result_ = if_clause(builder_, level_ + 1);
    result_ = result_ && if_clauses_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_IF_CLAUSES);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (';' if_clause)
  private static boolean if_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clauses_1_0")) return false;
    return if_clauses_1_0_0(builder_, level_ + 1);
  }

  // ';' if_clause
  private static boolean if_clauses_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_clauses_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && if_clause(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // if if_clauses end
  public static boolean if_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "if_expression")) return false;
    if (!nextTokenIs(builder_, ERL_IF)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_IF);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, if_clauses(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_IF_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_IF_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '-' ('include' | 'include_lib') '(' include_string ')'
  public static boolean include(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && include_1(builder_, level_ + 1);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, include_string(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_INCLUDE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // ('include' | 'include_lib')
  private static boolean include_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include_1")) return false;
    return include_1_0(builder_, level_ + 1);
  }

  // 'include' | 'include_lib'
  private static boolean include_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, "include");
    if (!result_) result_ = consumeToken(builder_, "include_lib");
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // string
  public static boolean include_string(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "include_string")) return false;
    if (!nextTokenIs(builder_, ERL_STRING)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_STRING);
    if (result_) {
      marker_.done(ERL_INCLUDE_STRING);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  /* ********************************************************** */
  // '-'? (integer | macros)
  public static boolean int_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = int_type_0(builder_, level_ + 1);
    result_ = result_ && int_type_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_INT_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // '-'?
  private static boolean int_type_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_0")) return false;
    consumeToken(builder_, ERL_OP_MINUS);
    return true;
  }

  // (integer | macros)
  private static boolean int_type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_1")) return false;
    return int_type_1_0(builder_, level_ + 1);
  }

  // integer | macros
  private static boolean int_type_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "int_type_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_INTEGER);
    if (!result_) result_ = macros(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // (argument_definition (('<-' | '<=') expression)?)
  public static boolean lc_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression")) return false;
    return lc_expression_0(builder_, level_ + 1);
  }

  // argument_definition (('<-' | '<=') expression)?
  private static boolean lc_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = argument_definition(builder_, level_ + 1);
    result_ = result_ && lc_expression_0_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_LC_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_LC_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (('<-' | '<=') expression)?
  private static boolean lc_expression_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_1")) return false;
    lc_expression_0_1_0(builder_, level_ + 1);
    return true;
  }

  // (('<-' | '<=') expression)
  private static boolean lc_expression_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_1_0")) return false;
    return lc_expression_0_1_0_0(builder_, level_ + 1);
  }

  // ('<-' | '<=') expression
  private static boolean lc_expression_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = lc_expression_0_1_0_0_0(builder_, level_ + 1);
    result_ = result_ && expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('<-' | '<=')
  private static boolean lc_expression_0_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_1_0_0_0")) return false;
    return lc_expression_0_1_0_0_0_0(builder_, level_ + 1);
  }

  // '<-' | '<='
  private static boolean lc_expression_0_1_0_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_expression_0_1_0_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_LT_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_LT_EQ);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // lc_expression (',' lc_expression)*
  public static boolean lc_exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_exprs")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<lc exprs>");
    result_ = lc_expression(builder_, level_ + 1);
    result_ = result_ && lc_exprs_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_LC_EXPRS);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (',' lc_expression)
  private static boolean lc_exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_exprs_1_0")) return false;
    return lc_exprs_1_0_0(builder_, level_ + 1);
  }

  // ',' lc_expression
  private static boolean lc_exprs_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "lc_exprs_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && lc_expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '[' atom_with_arity_expression (',' atom_with_arity_expression)* ']'
  public static boolean list_atom_with_arity_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_atom_with_arity_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && atom_with_arity_expression(builder_, level_ + 1);
    result_ = result_ && list_atom_with_arity_expression_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BRACKET_RIGHT);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_LIST_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_LIST_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
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

  // (',' atom_with_arity_expression)
  private static boolean list_atom_with_arity_expression_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_atom_with_arity_expression_2_0")) return false;
    return list_atom_with_arity_expression_2_0_0(builder_, level_ + 1);
  }

  // ',' atom_with_arity_expression
  private static boolean list_atom_with_arity_expression_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_atom_with_arity_expression_2_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && atom_with_arity_expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '[' expression '||' lc_exprs ']'
  public static boolean list_comprehension(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_comprehension")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && expression(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_OR_OR);
    pinned_ = result_; // pin = 3
    result_ = result_ && report_error_(builder_, lc_exprs(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_BRACKET_RIGHT) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_LIST_COMPREHENSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_LIST_COMPREHENSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '[' (']' | (expression tail))
  public static boolean list_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression")) return false;
    if (!nextTokenIs(builder_, ERL_BRACKET_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && list_expression_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_LIST_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_LIST_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (']' | (expression tail))
  private static boolean list_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression_1")) return false;
    return list_expression_1_0(builder_, level_ + 1);
  }

  // ']' | (expression tail)
  private static boolean list_expression_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) result_ = list_expression_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (expression tail)
  private static boolean list_expression_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression_1_0_1")) return false;
    return list_expression_1_0_1_0(builder_, level_ + 1);
  }

  // expression tail
  private static boolean list_expression_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_expression_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = expression(builder_, level_ + 1);
    result_ = result_ && tail(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '++' | '--'
  static boolean list_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "list_op")) return false;
    if (!nextTokenIs(builder_, ERL_OP_PLUS_PLUS) && !nextTokenIs(builder_, ERL_OP_MINUS_MINUS)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_PLUS_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS_MINUS);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '?' macros_name
  public static boolean macros(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_QMARK);
    pinned_ = result_; // pin = 1
    result_ = result_ && macros_name(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_MACROS);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '-' 'define' '(' macros_name ['(' q_atom_or_var (',' q_atom_or_var)* ')']',' expression ')'
  public static boolean macros_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && consumeToken(builder_, "define");
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, macros_name(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, macros_definition_4(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_COMMA)) && result_;
    result_ = pinned_ && report_error_(builder_, expression(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_MACROS_DEFINITION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // ['(' q_atom_or_var (',' q_atom_or_var)* ')']
  private static boolean macros_definition_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition_4")) return false;
    macros_definition_4_0(builder_, level_ + 1);
    return true;
  }

  // '(' q_atom_or_var (',' q_atom_or_var)* ')'
  private static boolean macros_definition_4_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition_4_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && q_atom_or_var(builder_, level_ + 1);
    result_ = result_ && macros_definition_4_0_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' q_atom_or_var)*
  private static boolean macros_definition_4_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition_4_0_2")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!macros_definition_4_0_2_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "macros_definition_4_0_2");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' q_atom_or_var)
  private static boolean macros_definition_4_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition_4_0_2_0")) return false;
    return macros_definition_4_0_2_0_0(builder_, level_ + 1);
  }

  // ',' q_atom_or_var
  private static boolean macros_definition_4_0_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_definition_4_0_2_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && q_atom_or_var(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // atom | var
  public static boolean macros_name(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "macros_name")) return false;
    if (!nextTokenIs(builder_, ERL_ATOM) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<macros name>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<macros name>");
    result_ = consumeToken(builder_, ERL_ATOM);
    if (!result_) result_ = consumeToken(builder_, ERL_VAR);
    if (result_) {
      marker_.done(ERL_MACROS_NAME);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
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
  //   | query_expression
  //   | parenthesized_expression
  //   | binary_expression
  //   | begin_end_expression
  public static boolean max_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "max_expression")) return false;
    boolean result_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
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
    if (!result_) result_ = query_expression(builder_, level_ + 1);
    if (!result_) result_ = parenthesized_expression(builder_, level_ + 1);
    if (!result_) result_ = binary_expression(builder_, level_ + 1);
    if (!result_) result_ = begin_end_expression(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_MAX_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_) {
      marker_.done(ERL_MAX_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // 'module' '(' q_atom (',' argument_definition)? ')'
  public static boolean module(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<module>");
    result_ = consumeToken(builder_, "module");
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, q_atom(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, module_3(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_MODULE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (',' argument_definition)?
  private static boolean module_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_3")) return false;
    module_3_0(builder_, level_ + 1);
    return true;
  }

  // (',' argument_definition)
  private static boolean module_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_3_0")) return false;
    return module_3_0_0(builder_, level_ + 1);
  }

  // ',' argument_definition
  private static boolean module_3_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_3_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && argument_definition(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean module_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "module_ref")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<module ref>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<module ref>");
    result_ = q_atom(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_MODULE_REF);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // '/' |'*' | div | rem  | band | and
  static boolean mult_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "mult_op")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_AR_MUL);
    if (!result_) result_ = consumeToken(builder_, ERL_DIV);
    if (!result_) result_ = consumeToken(builder_, ERL_REM);
    if (!result_) result_ = consumeToken(builder_, ERL_BAND);
    if (!result_) result_ = consumeToken(builder_, ERL_AND);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // mult_op prefix_expression
  public static boolean multiplicative_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "multiplicative_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker left_marker_ = (Marker)builder_.getLatestDoneMarker();
    if (!invalid_left_marker_guard_(builder_, left_marker_, "multiplicative_expression")) return false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = mult_op(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && prefix_expression(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.drop();
      left_marker_.precede().done(ERL_MULTIPLICATIVE_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '/' bit_type_list
  public static boolean opt_bit_type_list(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "opt_bit_type_list")) return false;
    if (!nextTokenIs(builder_, ERL_OP_AR_DIV)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    pinned_ = result_; // pin = 1
    result_ = result_ && bit_type_list(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_OPT_BIT_TYPE_LIST);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '(' expression ')'
  public static boolean parenthesized_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "parenthesized_expression")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, expression(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_PARENTHESIZED_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_PARENTHESIZED_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // prefix_op? expr_700_a
  public static boolean prefix_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "prefix_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = prefix_expression_0(builder_, level_ + 1);
    result_ = result_ && expr_700_a(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_PREFIX_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_PREFIX_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // prefix_op?
  private static boolean prefix_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "prefix_expression_0")) return false;
    prefix_op(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '+' | '-' | bnot | not
  static boolean prefix_op(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "prefix_op")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_PLUS);
    if (!result_) result_ = consumeToken(builder_, ERL_OP_MINUS);
    if (!result_) result_ = consumeToken(builder_, ERL_BNOT);
    if (!result_) result_ = consumeToken(builder_, ERL_NOT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // macros | atom
  public static boolean q_atom(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "q_atom")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<q atom>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<q atom>");
    result_ = macros(builder_, level_ + 1);
    if (!result_) result_ = consumeToken(builder_, ERL_ATOM);
    if (result_) {
      marker_.done(ERL_Q_ATOM);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // q_atom | q_var
  static boolean q_atom_or_var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "q_atom_or_var")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom(builder_, level_ + 1);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '_' | var
  public static boolean q_var(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "q_var")) return false;
    if (!nextTokenIs(builder_, ERL_UNI_PATTERN) && !nextTokenIs(builder_, ERL_VAR)
        && replaceVariants(builder_, 2, "<q var>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<q var>");
    result_ = consumeToken(builder_, ERL_UNI_PATTERN);
    if (!result_) result_ = consumeToken(builder_, ERL_VAR);
    if (result_) {
      marker_.done(ERL_Q_VAR);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // q_atom '.' q_atom
  static boolean qualified_atom_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_atom_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<expression>")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_DOT);
    result_ = result_ && q_atom(builder_, level_ + 1);
    if (!result_ && !pinned_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // '.' q_atom
  public static boolean qualified_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_expression")) return false;
    if (!nextTokenIs(builder_, ERL_DOT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker left_marker_ = (Marker)builder_.getLatestDoneMarker();
    if (!invalid_left_marker_guard_(builder_, left_marker_, "qualified_expression")) return false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_DOT);
    result_ = result_ && q_atom(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.drop();
      left_marker_.precede().done(ERL_QUALIFIED_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom qualified_expression*
  static boolean qualified_reference(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_reference")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && qualified_reference_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // qualified_expression*
  private static boolean qualified_reference_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "qualified_reference_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!qualified_expression(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "qualified_reference_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  /* ********************************************************** */
  // query list_comprehension end
  public static boolean query_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "query_expression")) return false;
    if (!nextTokenIs(builder_, ERL_QUERY)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_QUERY);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, list_comprehension(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_QUERY_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_QUERY_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // receive ((cr_clauses end) | (cr_clauses? after expression clause_body end))
  public static boolean receive_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression")) return false;
    if (!nextTokenIs(builder_, ERL_RECEIVE)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_RECEIVE);
    pinned_ = result_; // pin = 1
    result_ = result_ && receive_expression_1(builder_, level_ + 1);
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_RECEIVE_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_RECEIVE_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // ((cr_clauses end) | (cr_clauses? after expression clause_body end))
  private static boolean receive_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1")) return false;
    return receive_expression_1_0(builder_, level_ + 1);
  }

  // (cr_clauses end) | (cr_clauses? after expression clause_body end)
  private static boolean receive_expression_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = receive_expression_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = receive_expression_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (cr_clauses end)
  private static boolean receive_expression_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_0_0")) return false;
    return receive_expression_1_0_0_0(builder_, level_ + 1);
  }

  // cr_clauses end
  private static boolean receive_expression_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = cr_clauses(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_END);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (cr_clauses? after expression clause_body end)
  private static boolean receive_expression_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_0_1")) return false;
    return receive_expression_1_0_1_0(builder_, level_ + 1);
  }

  // cr_clauses? after expression clause_body end
  private static boolean receive_expression_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = receive_expression_1_0_1_0_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_AFTER);
    result_ = result_ && expression(builder_, level_ + 1);
    result_ = result_ && clause_body(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_END);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // cr_clauses?
  private static boolean receive_expression_1_0_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "receive_expression_1_0_1_0_0")) return false;
    cr_clauses(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '-' 'record' '(' q_atom ',' typed_record_fields ')'
  public static boolean record_definition(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_definition")) return false;
    if (!nextTokenIs(builder_, ERL_OP_MINUS)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_MINUS);
    result_ = result_ && consumeToken(builder_, "record");
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_PAR_LEFT));
    result_ = pinned_ && report_error_(builder_, q_atom(builder_, level_ + 1)) && result_;
    result_ = pinned_ && report_error_(builder_, consumeToken(builder_, ERL_COMMA)) && result_;
    result_ = pinned_ && report_error_(builder_, typed_record_fields(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_PAR_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_RECORD_DEFINITION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // max_expression? '#' record_ref (record_field_ref | record_tuple)
  public static boolean record_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_expression")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<expression>");
    result_ = record_expression_0(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_RADIX);
    pinned_ = result_; // pin = 2
    result_ = result_ && report_error_(builder_, record_ref(builder_, level_ + 1));
    result_ = pinned_ && record_expression_3(builder_, level_ + 1) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_RECORD_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_RECORD_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // max_expression?
  private static boolean record_expression_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_expression_0")) return false;
    max_expression(builder_, level_ + 1);
    return true;
  }

  // (record_field_ref | record_tuple)
  private static boolean record_expression_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_expression_3")) return false;
    return record_expression_3_0(builder_, level_ + 1);
  }

  // record_field_ref | record_tuple
  private static boolean record_expression_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_expression_3_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = record_field_ref(builder_, level_ + 1);
    if (!result_) result_ = record_tuple(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom '=' (qualified_atom_expression | expression)
  public static boolean record_field(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_RECOVER_, "<record field>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, consumeToken(builder_, ERL_OP_EQ));
    result_ = pinned_ && record_field_2(builder_, level_ + 1) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_RECORD_FIELD);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_RECOVER_, tuple_recoverer_parser_);
    return result_ || pinned_;
  }

  // (qualified_atom_expression | expression)
  private static boolean record_field_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field_2")) return false;
    return record_field_2_0(builder_, level_ + 1);
  }

  // qualified_atom_expression | expression
  private static boolean record_field_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = qualified_atom_expression(builder_, level_ + 1);
    if (!result_) result_ = expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '.' q_atom
  public static boolean record_field_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_field_ref")) return false;
    if (!nextTokenIs(builder_, ERL_DOT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_DOT);
    pinned_ = result_; // pin = 1
    result_ = result_ && q_atom(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_RECORD_FIELD);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // record_field (',' record_field)*
  public static boolean record_fields(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_fields")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<record fields>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<record fields>");
    result_ = record_field(builder_, level_ + 1);
    result_ = result_ && record_fields_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_RECORD_FIELDS);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
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

  // (',' record_field)
  private static boolean record_fields_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_fields_1_0")) return false;
    return record_fields_1_0_0(builder_, level_ + 1);
  }

  // ',' record_field
  private static boolean record_fields_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_fields_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && record_field(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // q_atom
  public static boolean record_ref(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_ref")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<record ref>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<record ref>");
    result_ = q_atom(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_RECORD_REF);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // '{' record_fields? '}'
  public static boolean record_tuple(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_tuple")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, record_tuple_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_RECORD_TUPLE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // record_fields?
  private static boolean record_tuple_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "record_tuple_1")) return false;
    record_fields(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // !('.')
  static boolean recoverer(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recoverer")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_NOT_, null);
    result_ = !recoverer_0(builder_, level_ + 1);
    marker_.rollbackTo();
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_NOT_, null);
    return result_;
  }

  // ('.')
  private static boolean recoverer_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "recoverer_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_DOT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // rule_clause (';' rule_clause)*
  public static boolean rule(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<rule>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<rule>");
    result_ = rule_clause(builder_, level_ + 1);
    result_ = result_ && rule_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_RULE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (';' rule_clause)
  private static boolean rule_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_1_0")) return false;
    return rule_1_0_0(builder_, level_ + 1);
  }

  // ';' rule_clause
  private static boolean rule_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && rule_clause(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // ':-' lc_exprs
  public static boolean rule_body(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_body")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<rule body>");
    result_ = consumeToken(builder_, ":-");
    pinned_ = result_; // pin = 1
    result_ = result_ && lc_exprs(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_RULE_BODY);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom argument_list clause_guard? rule_body
  public static boolean rule_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_clause")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<rule clause>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<rule clause>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && argument_list(builder_, level_ + 1);
    result_ = result_ && rule_clause_2(builder_, level_ + 1);
    result_ = result_ && rule_body(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_RULE_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // clause_guard?
  private static boolean rule_clause_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "rule_clause_2")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // '!' expr_100_a
  public static boolean send_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "send_expression")) return false;
    if (!nextTokenIs(builder_, ERL_OP_EXL)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker left_marker_ = (Marker)builder_.getLatestDoneMarker();
    if (!invalid_left_marker_guard_(builder_, left_marker_, "send_expression")) return false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_OP_EXL);
    pinned_ = result_; // pin = 1
    result_ = result_ && expr_100_a(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.drop();
      left_marker_.precede().done(ERL_SEND_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // q_atom [('/' integer '::') | (':' q_atom ['/' integer '::'])]
  public static boolean spec_fun(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)
        && replaceVariants(builder_, 2, "<spec fun>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<spec fun>");
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && spec_fun_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_SPEC_FUN);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [('/' integer '::') | (':' q_atom ['/' integer '::'])]
  private static boolean spec_fun_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1")) return false;
    spec_fun_1_0(builder_, level_ + 1);
    return true;
  }

  // ('/' integer '::') | (':' q_atom ['/' integer '::'])
  private static boolean spec_fun_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = spec_fun_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = spec_fun_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('/' integer '::')
  private static boolean spec_fun_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0_0")) return false;
    return spec_fun_1_0_0_0(builder_, level_ + 1);
  }

  // '/' integer '::'
  private static boolean spec_fun_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    result_ = result_ && consumeToken(builder_, ERL_COLON_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (':' q_atom ['/' integer '::'])
  private static boolean spec_fun_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0_1")) return false;
    return spec_fun_1_0_1_0(builder_, level_ + 1);
  }

  // ':' q_atom ['/' integer '::']
  private static boolean spec_fun_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COLON);
    result_ = result_ && q_atom(builder_, level_ + 1);
    result_ = result_ && spec_fun_1_0_1_0_2(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ['/' integer '::']
  private static boolean spec_fun_1_0_1_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0_1_0_2")) return false;
    spec_fun_1_0_1_0_2_0(builder_, level_ + 1);
    return true;
  }

  // '/' integer '::'
  private static boolean spec_fun_1_0_1_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "spec_fun_1_0_1_0_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_AR_DIV);
    result_ = result_ && consumeToken(builder_, ERL_INTEGER);
    result_ = result_ && consumeToken(builder_, ERL_COLON_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // 'spec' type_spec
  public static boolean specification(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "specification")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<specification>");
    result_ = consumeToken(builder_, "spec");
    pinned_ = result_; // pin = 1
    result_ = result_ && type_spec(builder_, level_ + 1);
    if (result_ || pinned_) {
      marker_.done(ERL_SPECIFICATION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // ']' | ('|' expression ']') | (',' expression tail)
  static boolean tail(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) result_ = tail_1(builder_, level_ + 1);
    if (!result_) result_ = tail_2(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('|' expression ']')
  private static boolean tail_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail_1")) return false;
    return tail_1_0(builder_, level_ + 1);
  }

  // '|' expression ']'
  private static boolean tail_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OR);
    result_ = result_ && expression(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' expression tail)
  private static boolean tail_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail_2")) return false;
    return tail_2_0(builder_, level_ + 1);
  }

  // ',' expression tail
  private static boolean tail_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tail_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && expression(builder_, level_ + 1);
    result_ = result_ && tail(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // [q_var '::'] top_type_100_t
  public static boolean top_type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = top_type_0(builder_, level_ + 1);
    result_ = result_ && top_type_100_t(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TOP_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
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
    Marker marker_ = builder_.mark();
    result_ = q_var(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // type ['|' top_type_100_t]
  public static boolean top_type_100_t(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_100_t")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = type(builder_, level_ + 1);
    result_ = result_ && top_type_100_t_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TOP_TYPE_100_T);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OR);
    result_ = result_ && top_type_100_t(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '->' top_type
  public static boolean top_type_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_type_clause")) return false;
    if (!nextTokenIs(builder_, ERL_ARROW)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_ARROW);
    result_ = result_ && top_type(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TOP_TYPE_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  /* ********************************************************** */
  // top_type (',' top_type)*
  static boolean top_types(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_types")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = top_type(builder_, level_ + 1);
    result_ = result_ && top_types_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' top_type)*
  private static boolean top_types_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_types_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!top_types_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "top_types_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' top_type)
  private static boolean top_types_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_types_1_0")) return false;
    return top_types_1_0_0(builder_, level_ + 1);
  }

  // ',' top_type
  private static boolean top_types_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "top_types_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && top_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // (catch try_clauses [after try_expressions_clause] end) | (after try_expressions_clause end)
  public static boolean try_catch(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch")) return false;
    if (!nextTokenIs(builder_, ERL_AFTER) && !nextTokenIs(builder_, ERL_CATCH)
        && replaceVariants(builder_, 2, "<try catch>")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<try catch>");
    result_ = try_catch_0(builder_, level_ + 1);
    if (!result_) result_ = try_catch_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TRY_CATCH);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // (catch try_clauses [after try_expressions_clause] end)
  private static boolean try_catch_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0")) return false;
    return try_catch_0_0(builder_, level_ + 1);
  }

  // catch try_clauses [after try_expressions_clause] end
  private static boolean try_catch_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_CATCH);
    pinned_ = result_; // pin = catch|after
    result_ = result_ && report_error_(builder_, try_clauses(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, try_catch_0_0_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    if (!result_ && !pinned_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // [after try_expressions_clause]
  private static boolean try_catch_0_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0_0_2")) return false;
    try_catch_0_0_2_0(builder_, level_ + 1);
    return true;
  }

  // after try_expressions_clause
  private static boolean try_catch_0_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_0_0_2_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_AFTER);
    pinned_ = result_; // pin = catch|after
    result_ = result_ && try_expressions_clause(builder_, level_ + 1);
    if (!result_ && !pinned_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (after try_expressions_clause end)
  private static boolean try_catch_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_1")) return false;
    return try_catch_1_0(builder_, level_ + 1);
  }

  // after try_expressions_clause end
  private static boolean try_catch_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_catch_1_0")) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_AFTER);
    pinned_ = result_; // pin = catch|after
    result_ = result_ && report_error_(builder_, try_expressions_clause(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_END) && result_;
    if (!result_ && !pinned_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  /* ********************************************************** */
  // [argument_definition ':'] argument_definition clause_guard? clause_body
  public static boolean try_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clause")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<try clause>");
    result_ = try_clause_0(builder_, level_ + 1);
    result_ = result_ && argument_definition(builder_, level_ + 1);
    result_ = result_ && try_clause_2(builder_, level_ + 1);
    result_ = result_ && clause_body(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TRY_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // [argument_definition ':']
  private static boolean try_clause_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clause_0")) return false;
    try_clause_0_0(builder_, level_ + 1);
    return true;
  }

  // argument_definition ':'
  private static boolean try_clause_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clause_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = argument_definition(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // clause_guard?
  private static boolean try_clause_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clause_2")) return false;
    clause_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // try_clause (';' try_clause)*
  public static boolean try_clauses(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clauses")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<try clauses>");
    result_ = try_clause(builder_, level_ + 1);
    result_ = result_ && try_clauses_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TRY_CLAUSES);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
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

  // (';' try_clause)
  private static boolean try_clauses_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clauses_1_0")) return false;
    return try_clauses_1_0_0(builder_, level_ + 1);
  }

  // ';' try_clause
  private static boolean try_clauses_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_clauses_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && try_clause(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // try try_expressions_clause (of cr_clauses)? try_catch
  public static boolean try_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression")) return false;
    if (!nextTokenIs(builder_, ERL_TRY)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_TRY);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, try_expressions_clause(builder_, level_ + 1));
    result_ = pinned_ && report_error_(builder_, try_expression_2(builder_, level_ + 1)) && result_;
    result_ = pinned_ && try_catch(builder_, level_ + 1) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_TRY_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_TRY_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // (of cr_clauses)?
  private static boolean try_expression_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression_2")) return false;
    try_expression_2_0(builder_, level_ + 1);
    return true;
  }

  // (of cr_clauses)
  private static boolean try_expression_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression_2_0")) return false;
    return try_expression_2_0_0(builder_, level_ + 1);
  }

  // of cr_clauses
  private static boolean try_expression_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expression_2_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OF);
    result_ = result_ && cr_clauses(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // exprs
  public static boolean try_expressions_clause(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "try_expressions_clause")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<try expressions clause>");
    result_ = exprs(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TRY_EXPRESSIONS_CLAUSE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  /* ********************************************************** */
  // '{' exprs? '}'
  public static boolean tuple_expression(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_expression")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    int start_ = builder_.getCurrentOffset();
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, tuple_expression_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    LighterASTNode last_ = result_? builder_.getLatestDoneMarker() : null;
    if (last_ != null && last_.getStartOffset() == start_ && type_extends_(last_.getTokenType(), ERL_TUPLE_EXPRESSION)) {
      marker_.drop();
    }
    else if (result_ || pinned_) {
      marker_.done(ERL_TUPLE_EXPRESSION);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // exprs?
  private static boolean tuple_expression_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_expression_1")) return false;
    exprs(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // !('}'|',')
  static boolean tuple_recoverer(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_recoverer")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_NOT_, null);
    result_ = !tuple_recoverer_0(builder_, level_ + 1);
    marker_.rollbackTo();
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_NOT_, null);
    return result_;
  }

  // ('}'|',')
  private static boolean tuple_recoverer_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_recoverer_0")) return false;
    return tuple_recoverer_0_0(builder_, level_ + 1);
  }

  // '}'|','
  private static boolean tuple_recoverer_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "tuple_recoverer_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_CURLY_RIGHT);
    if (!result_) result_ = consumeToken(builder_, ERL_COMMA);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // ('(' top_type ')')
  //   | (int_type ['..' int_type])
  //   | (fun '(' fun_type_100_t? ')')
  //   | [q_atom ':'] q_atom ['(' top_types? ')']
  //   | ( q_atom '(' top_types ')')
  //   | binary_type
  //   | q_var
  //   | ('[' top_type (',' '...')? ']')
  //   | ('{' top_types? '}')
  //   | ('#' q_atom '{' field_types? '}')
  public static boolean type(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type>");
    result_ = type_0(builder_, level_ + 1);
    if (!result_) result_ = type_1(builder_, level_ + 1);
    if (!result_) result_ = type_2(builder_, level_ + 1);
    if (!result_) result_ = type_3(builder_, level_ + 1);
    if (!result_) result_ = type_4(builder_, level_ + 1);
    if (!result_) result_ = binary_type(builder_, level_ + 1);
    if (!result_) result_ = q_var(builder_, level_ + 1);
    if (!result_) result_ = type_7(builder_, level_ + 1);
    if (!result_) result_ = type_8(builder_, level_ + 1);
    if (!result_) result_ = type_9(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TYPE);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // ('(' top_type ')')
  private static boolean type_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_0")) return false;
    return type_0_0(builder_, level_ + 1);
  }

  // '(' top_type ')'
  private static boolean type_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && top_type(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (int_type ['..' int_type])
  private static boolean type_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1")) return false;
    return type_1_0(builder_, level_ + 1);
  }

  // int_type ['..' int_type]
  private static boolean type_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = int_type(builder_, level_ + 1);
    result_ = result_ && type_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ['..' int_type]
  private static boolean type_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1_0_1")) return false;
    type_1_0_1_0(builder_, level_ + 1);
    return true;
  }

  // '..' int_type
  private static boolean type_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_DOT_DOT);
    result_ = result_ && int_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (fun '(' fun_type_100_t? ')')
  private static boolean type_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_2")) return false;
    return type_2_0(builder_, level_ + 1);
  }

  // fun '(' fun_type_100_t? ')'
  private static boolean type_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_FUN);
    result_ = result_ && consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && type_2_0_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // fun_type_100_t?
  private static boolean type_2_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_2_0_2")) return false;
    fun_type_100_t(builder_, level_ + 1);
    return true;
  }

  // [q_atom ':'] q_atom ['(' top_types? ')']
  private static boolean type_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = type_3_0(builder_, level_ + 1);
    result_ = result_ && q_atom(builder_, level_ + 1);
    result_ = result_ && type_3_2(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // [q_atom ':']
  private static boolean type_3_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_0")) return false;
    type_3_0_0(builder_, level_ + 1);
    return true;
  }

  // q_atom ':'
  private static boolean type_3_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_COLON);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ['(' top_types? ')']
  private static boolean type_3_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_2")) return false;
    type_3_2_0(builder_, level_ + 1);
    return true;
  }

  // '(' top_types? ')'
  private static boolean type_3_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_2_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && type_3_2_0_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // top_types?
  private static boolean type_3_2_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_3_2_0_1")) return false;
    top_types(builder_, level_ + 1);
    return true;
  }

  // ( q_atom '(' top_types ')')
  private static boolean type_4(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_4")) return false;
    return type_4_0(builder_, level_ + 1);
  }

  // q_atom '(' top_types ')'
  private static boolean type_4_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_4_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && top_types(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('[' top_type (',' '...')? ']')
  private static boolean type_7(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7")) return false;
    return type_7_0(builder_, level_ + 1);
  }

  // '[' top_type (',' '...')? ']'
  private static boolean type_7_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_BRACKET_LEFT);
    result_ = result_ && top_type(builder_, level_ + 1);
    result_ = result_ && type_7_0_2(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_BRACKET_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' '...')?
  private static boolean type_7_0_2(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7_0_2")) return false;
    type_7_0_2_0(builder_, level_ + 1);
    return true;
  }

  // (',' '...')
  private static boolean type_7_0_2_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7_0_2_0")) return false;
    return type_7_0_2_0_0(builder_, level_ + 1);
  }

  // ',' '...'
  private static boolean type_7_0_2_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_7_0_2_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && consumeToken(builder_, ERL_DOT_DOT_DOT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('{' top_types? '}')
  private static boolean type_8(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_8")) return false;
    return type_8_0(builder_, level_ + 1);
  }

  // '{' top_types? '}'
  private static boolean type_8_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_8_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    result_ = result_ && type_8_0_1(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_CURLY_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // top_types?
  private static boolean type_8_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_8_0_1")) return false;
    top_types(builder_, level_ + 1);
    return true;
  }

  // ('#' q_atom '{' field_types? '}')
  private static boolean type_9(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_9")) return false;
    return type_9_0(builder_, level_ + 1);
  }

  // '#' q_atom '{' field_types? '}'
  private static boolean type_9_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_9_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_RADIX);
    result_ = result_ && q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_CURLY_LEFT);
    result_ = result_ && type_9_0_3(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_CURLY_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // field_types?
  private static boolean type_9_0_3(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_9_0_3")) return false;
    field_types(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // (q_atom '(' top_types ')') | top_type
  public static boolean type_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type guard>");
    result_ = type_guard_0(builder_, level_ + 1);
    if (!result_) result_ = top_type(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TYPE_GUARD);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // (q_atom '(' top_types ')')
  private static boolean type_guard_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard_0")) return false;
    return type_guard_0_0(builder_, level_ + 1);
  }

  // q_atom '(' top_types ')'
  private static boolean type_guard_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guard_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = q_atom(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && top_types(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // type_guard (',' type_guard)*
  static boolean type_guards(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guards")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = type_guard(builder_, level_ + 1);
    result_ = result_ && type_guards_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' type_guard)*
  private static boolean type_guards_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guards_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!type_guards_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "type_guards_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (',' type_guard)
  private static boolean type_guards_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guards_1_0")) return false;
    return type_guards_1_0_0(builder_, level_ + 1);
  }

  // ',' type_guard
  private static boolean type_guards_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_guards_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && type_guard(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // fun_type [type_sig_guard]
  public static boolean type_sig(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sig")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = fun_type(builder_, level_ + 1);
    result_ = result_ && type_sig_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TYPE_SIG);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  // [type_sig_guard]
  private static boolean type_sig_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sig_1")) return false;
    type_sig_guard(builder_, level_ + 1);
    return true;
  }

  /* ********************************************************** */
  // when type_guards
  public static boolean type_sig_guard(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sig_guard")) return false;
    if (!nextTokenIs(builder_, ERL_WHEN)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_WHEN);
    result_ = result_ && type_guards(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TYPE_SIG_GUARD);
    }
    else {
      marker_.rollbackTo();
    }
    return result_;
  }

  /* ********************************************************** */
  // type_sig (';' type_sig)*
  static boolean type_sigs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs")) return false;
    if (!nextTokenIs(builder_, ERL_PAR_LEFT)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = type_sig(builder_, level_ + 1);
    result_ = result_ && type_sigs_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (';' type_sig)*
  private static boolean type_sigs_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs_1")) return false;
    int offset_ = builder_.getCurrentOffset();
    while (true) {
      if (!type_sigs_1_0(builder_, level_ + 1)) break;
      int next_offset_ = builder_.getCurrentOffset();
      if (offset_ == next_offset_) {
        empty_element_parsed_guard_(builder_, offset_, "type_sigs_1");
        break;
      }
      offset_ = next_offset_;
    }
    return true;
  }

  // (';' type_sig)
  private static boolean type_sigs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs_1_0")) return false;
    return type_sigs_1_0_0(builder_, level_ + 1);
  }

  // ';' type_sig
  private static boolean type_sigs_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_sigs_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_SEMI);
    result_ = result_ && type_sig(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // ('(' spec_fun type_sigs ')') | spec_fun type_sigs
  public static boolean type_spec(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_spec")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<type spec>");
    result_ = type_spec_0(builder_, level_ + 1);
    if (!result_) result_ = type_spec_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TYPE_SPEC);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // ('(' spec_fun type_sigs ')')
  private static boolean type_spec_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_spec_0")) return false;
    return type_spec_0_0(builder_, level_ + 1);
  }

  // '(' spec_fun type_sigs ')'
  private static boolean type_spec_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_spec_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_PAR_LEFT);
    result_ = result_ && spec_fun(builder_, level_ + 1);
    result_ = result_ && type_sigs(builder_, level_ + 1);
    result_ = result_ && consumeToken(builder_, ERL_PAR_RIGHT);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // spec_fun type_sigs
  private static boolean type_spec_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "type_spec_1")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = spec_fun(builder_, level_ + 1);
    result_ = result_ && type_sigs(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // expression ((',' typed_record_fields) | ('::' top_type))
  public static boolean typed_attr_val(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<typed attr val>");
    result_ = expression(builder_, level_ + 1);
    result_ = result_ && typed_attr_val_1(builder_, level_ + 1);
    if (result_) {
      marker_.done(ERL_TYPED_ATTR_VAL);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, false, _SECTION_GENERAL_, null);
    return result_;
  }

  // ((',' typed_record_fields) | ('::' top_type))
  private static boolean typed_attr_val_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1")) return false;
    return typed_attr_val_1_0(builder_, level_ + 1);
  }

  // (',' typed_record_fields) | ('::' top_type)
  private static boolean typed_attr_val_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = typed_attr_val_1_0_0(builder_, level_ + 1);
    if (!result_) result_ = typed_attr_val_1_0_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' typed_record_fields)
  private static boolean typed_attr_val_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_0_0")) return false;
    return typed_attr_val_1_0_0_0(builder_, level_ + 1);
  }

  // ',' typed_record_fields
  private static boolean typed_attr_val_1_0_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_0_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && typed_record_fields(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // ('::' top_type)
  private static boolean typed_attr_val_1_0_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_0_1")) return false;
    return typed_attr_val_1_0_1_0(builder_, level_ + 1);
  }

  // '::' top_type
  private static boolean typed_attr_val_1_0_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_attr_val_1_0_1_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COLON_COLON);
    result_ = result_ && top_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
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
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, "<typed expr>");
    result_ = q_atom(builder_, level_ + 1);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, typed_expr_1(builder_, level_ + 1));
    result_ = pinned_ && typed_expr_2(builder_, level_ + 1) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_TYPED_EXPR);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
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
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_OP_EQ);
    result_ = result_ && expression(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
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
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COLON_COLON);
    result_ = result_ && top_type(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // typed_expr (',' typed_expr)*
  static boolean typed_exprs(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_exprs")) return false;
    if (!nextTokenIs(builder_, ERL_QMARK) && !nextTokenIs(builder_, ERL_ATOM)) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = typed_expr(builder_, level_ + 1);
    result_ = result_ && typed_exprs_1(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  // (',' typed_expr)*
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

  // (',' typed_expr)
  private static boolean typed_exprs_1_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_exprs_1_0")) return false;
    return typed_exprs_1_0_0(builder_, level_ + 1);
  }

  // ',' typed_expr
  private static boolean typed_exprs_1_0_0(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_exprs_1_0_0")) return false;
    boolean result_ = false;
    Marker marker_ = builder_.mark();
    result_ = consumeToken(builder_, ERL_COMMA);
    result_ = result_ && typed_expr(builder_, level_ + 1);
    if (!result_) {
      marker_.rollbackTo();
    }
    else {
      marker_.drop();
    }
    return result_;
  }

  /* ********************************************************** */
  // '{' typed_exprs? '}'
  public static boolean typed_record_fields(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_record_fields")) return false;
    if (!nextTokenIs(builder_, ERL_CURLY_LEFT)) return false;
    boolean result_ = false;
    boolean pinned_ = false;
    Marker marker_ = builder_.mark();
    enterErrorRecordingSection(builder_, level_, _SECTION_GENERAL_, null);
    result_ = consumeToken(builder_, ERL_CURLY_LEFT);
    pinned_ = result_; // pin = 1
    result_ = result_ && report_error_(builder_, typed_record_fields_1(builder_, level_ + 1));
    result_ = pinned_ && consumeToken(builder_, ERL_CURLY_RIGHT) && result_;
    if (result_ || pinned_) {
      marker_.done(ERL_TYPED_RECORD_FIELDS);
    }
    else {
      marker_.rollbackTo();
    }
    result_ = exitErrorRecordingSection(builder_, level_, result_, pinned_, _SECTION_GENERAL_, null);
    return result_ || pinned_;
  }

  // typed_exprs?
  private static boolean typed_record_fields_1(PsiBuilder builder_, int level_) {
    if (!recursion_guard_(builder_, level_, "typed_record_fields_1")) return false;
    typed_exprs(builder_, level_ + 1);
    return true;
  }

  final static Parser recoverer_parser_ = new Parser() {
      public boolean parse(PsiBuilder builder_, int level_) {
        return recoverer(builder_, level_ + 1);
      }
    };
  final static Parser tuple_recoverer_parser_ = new Parser() {
      public boolean parse(PsiBuilder builder_, int level_) {
        return tuple_recoverer(builder_, level_ + 1);
      }
    };
}
