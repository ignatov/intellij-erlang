///*
// * Copyright 2010 Joachim Ansorg, mail@ansorg-it.com
// * File: ErlangTokenTypes.java, Class: ErlangTokenTypes
// * Last modified: 2009-10-10
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *    http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package org.intellij.erlang.parser;
//
//import com.intellij.psi.TokenType;
//import com.intellij.psi.tree.IElementType;
//import com.intellij.psi.tree.TokenSet;
//import org.intellij.erlang.psi.ErlangTokenType;
//
//public interface ErlangTokenTypes {
//  // common types
//  public static final IElementType BAD_CHARACTER = TokenType.BAD_CHARACTER;
//  public static final IElementType WHITESPACE = TokenType.WHITE_SPACE;
//  public static final TokenSet whitespace = TokenSet.create(WHITESPACE);
//
//  public static final IElementType COMMENT = new ErlangTokenType("COMMENT");
//  public static final TokenSet comment = TokenSet.create(COMMENT);
//
//  public static final IElementType LINE_FEED = new ErlangTokenType("\\n");
//
//  //separators
//  public static final IElementType PAR_LEFT = new ErlangTokenType("(");
//  public static final IElementType PAR_RIGHT = new ErlangTokenType(")");
//  public static final IElementType CURLY_LEFT = new ErlangTokenType("{");
//  public static final IElementType CURLY_RIGHT = new ErlangTokenType("}");
//  public static final IElementType BRACKET_LEFT = new ErlangTokenType("[");
//  public static final IElementType BRACKET_RIGHT = new ErlangTokenType("]");
//  public static final IElementType DOT = new ErlangTokenType(".");
//  public static final IElementType COLON = new ErlangTokenType(":");
//  public static final IElementType OR_OR = new ErlangTokenType("||");
//  public static final IElementType OR = new ErlangTokenType("|");
//  public static final IElementType SEMI = new ErlangTokenType(";");
//  public static final IElementType COMMA = new ErlangTokenType(",");
//  public static final IElementType QMARK = new ErlangTokenType("?");
//  public static final IElementType ARROW = new ErlangTokenType("->");
//  public static final IElementType RADIX = new ErlangTokenType("#");
//
//  public static final TokenSet seperators = TokenSet.create(PAR_LEFT, PAR_RIGHT, CURLY_LEFT, CURLY_RIGHT, BRACKET_LEFT, BRACKET_RIGHT, DOT, COLON,
//    OR_OR, OR, SEMI, COMMA, QMARK, ARROW, RADIX);
//  public static final TokenSet macroSeperators = TokenSet.create(DOT, COLON, OR_OR, OR, SEMI, COMMA, QMARK, ARROW, RADIX);
//
//  //operators
//  public static final IElementType OP_PLUS = new ErlangTokenType("+");
//  public static final IElementType OP_MINUS = new ErlangTokenType("-");
//  public static final IElementType OP_AR_MUL = new ErlangTokenType("*");
//  public static final IElementType OP_AR_DIV = new ErlangTokenType("/");
//  public static final IElementType OP_DIV = new ErlangTokenType("div");
//  public static final IElementType OP_REM = new ErlangTokenType("rem");
//  public static final IElementType OP_OR = new ErlangTokenType("or");
//  public static final IElementType OP_XOR = new ErlangTokenType("xor");
//  public static final IElementType OP_BOR = new ErlangTokenType("bor");
//  public static final IElementType OP_BXOR = new ErlangTokenType("bxor");
//  public static final IElementType OP_BSL = new ErlangTokenType("bsl");
//  public static final IElementType OP_BSR = new ErlangTokenType("bsr");
//  public static final IElementType OP_AND = new ErlangTokenType("and");
//  public static final IElementType OP_BAND = new ErlangTokenType("band");
//  public static final IElementType OP_EQ_EQ = new ErlangTokenType("==");
//  public static final IElementType OP_DIV_EQ = new ErlangTokenType("/=");
//  public static final IElementType OP_EQ_COL_EQ = new ErlangTokenType("=:=");
//  public static final IElementType OP_EQ_DIV_EQ = new ErlangTokenType("=/=");
//  public static final IElementType OP_LT = new ErlangTokenType("<");
//  public static final IElementType OP_EQ_LT = new ErlangTokenType("=<");
//  public static final IElementType OP_GT = new ErlangTokenType(">");
//  public static final IElementType OP_GT_EQ = new ErlangTokenType(">=");
//  public static final IElementType OP_NOT = new ErlangTokenType("not");
//  public static final IElementType OP_BNOT = new ErlangTokenType("bnot");
//  public static final IElementType OP_PLUS_PLUS = new ErlangTokenType("++");
//  public static final IElementType OP_MINUS_MINUS = new ErlangTokenType("--");
//  public static final IElementType OP_EQ = new ErlangTokenType("=");
//  public static final IElementType OP_EXL = new ErlangTokenType("!");
//  public static final IElementType OP_LT_MINUS = new ErlangTokenType("<-");
//  public static final IElementType OP_ANDALSO = new ErlangTokenType("andalso");
//  public static final IElementType OP_ORELSE = new ErlangTokenType("orelse");
//
//  public static final TokenSet operators = TokenSet.create(
//    OP_PLUS, OP_MINUS, OP_AR_MUL, OP_AR_DIV, OP_DIV, OP_REM, OP_OR, OP_XOR,
//    OP_BOR, OP_BXOR, OP_BSL, OP_BSR, OP_AND, OP_BAND, OP_EQ_EQ, OP_DIV_EQ, OP_EQ_DIV_EQ, OP_EQ_COL_EQ,
//    OP_LT, OP_EQ_LT, OP_GT, OP_GT_EQ, OP_NOT, OP_BNOT, OP_PLUS_PLUS, OP_MINUS_MINUS, OP_EQ,
//    OP_EXL, OP_LT_MINUS, OP_ANDALSO, OP_ORELSE
//  );
//
//  public static final TokenSet relationalOperators = TokenSet.create(OP_LT, OP_EQ_LT, OP_GT, OP_GT_EQ);
//  public static final TokenSet equalityOperators = TokenSet.create(OP_EQ_COL_EQ, OP_EQ_DIV_EQ, OP_EQ_EQ, OP_DIV_EQ);
//  public static final TokenSet listConcOperators = TokenSet.create(OP_PLUS_PLUS, OP_MINUS_MINUS);
//  public static final TokenSet additionOps = TokenSet.create(OP_PLUS, OP_MINUS, OP_BOR, OP_BXOR);
//  public static final TokenSet shiftOps = TokenSet.create(OP_BSL, OP_BSR);
//  public static final TokenSet multiplicationOps = TokenSet.create(OP_AR_MUL, OP_AR_DIV, OP_DIV, OP_REM, OP_BAND);
//  public static final TokenSet shortcutOperators = TokenSet.create(OP_ANDALSO, OP_ORELSE);
//  public static final TokenSet prefixOps = TokenSet.create(OP_PLUS, OP_MINUS, OP_BNOT, OP_NOT);
//  public static final TokenSet additionShiftOperators = TokenSet.create(OP_OR, OP_XOR);
//
//  //keywords
//  public static final IElementType AFTER = new ErlangTokenType("after");
//  public static final IElementType COND = new ErlangTokenType("cond");
//  public static final IElementType LET = new ErlangTokenType("let");
//  public static final IElementType WHEN = new ErlangTokenType("when");
//  public static final IElementType BEGIN = new ErlangTokenType("begin");
//  public static final IElementType END = new ErlangTokenType("end");
//  public static final IElementType OF = new ErlangTokenType("of");
//  public static final IElementType CASE = new ErlangTokenType("case");
//  public static final IElementType FUN = new ErlangTokenType("fun");
//  public static final IElementType QUERY = new ErlangTokenType("query");
//  public static final IElementType CATCH = new ErlangTokenType("catch");
//  public static final IElementType IF = new ErlangTokenType("if");
//  public static final IElementType RECEIVE = new ErlangTokenType("receive");
//  public static final IElementType TRY = new ErlangTokenType("try");
//
//  public static final TokenSet keywords = TokenSet.create(AFTER, COND, LET, WHEN, BEGIN, END, OF, CASE, FUN, QUERY, CATCH, IF, RECEIVE, TRY);
//  public static final TokenSet macroKeywords = TokenSet.create(AFTER, COND, LET, WHEN, BEGIN, OF, CASE, FUN, QUERY, CATCH, IF, RECEIVE, TRY);
//
//  //tokenLiterals
//  public static final IElementType INT = new ErlangTokenType("INT");
//  public static final IElementType FLOAT = new ErlangTokenType("FLOAT");
//  public static final IElementType CHAR = new ErlangTokenType("CHAR");
//  public static final IElementType STRING = new ErlangTokenType("STRING");
//  public static final IElementType ATOM = new ErlangTokenType("ATOM");
//
//  public static final TokenSet tokenLiterals = TokenSet.create(INT, FLOAT, CHAR, STRING);
//  public static final TokenSet strings = TokenSet.create(STRING);
//
//  //misc
//  public static final IElementType VAR = new ErlangTokenType("VAR");
//  public static final IElementType UNI_PATTERN = new ErlangTokenType("_ (universal pattern)");
//  public static final IElementType FULL_STOP = new ErlangTokenType("(full stop)");
//
//  public static final IElementType BIN_START = new ErlangTokenType("<< (bin start)");
//  public static final IElementType BIN_END = new ErlangTokenType(">> (bin end)");
//
//  public static final TokenSet macroNames = TokenSet.create(ATOM, VAR);
//}
