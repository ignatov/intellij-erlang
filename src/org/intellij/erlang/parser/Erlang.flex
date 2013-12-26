package org.intellij.erlang.parser;
import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import static org.intellij.erlang.ErlangTypes.*;
import static org.intellij.erlang.ErlangParserDefinition.*;

%%

%{
  public _ErlangLexer() {
    this((java.io.Reader)null);
  }
%}

%class _ErlangLexer
%implements FlexLexer
%unicode
%public

%function advance
%type IElementType

%table

/* This hex range is the same as octal \O00 - \O37 */
ControlCharacter = [\000 - \037]

ShebangLine = "#!"[^\r\n]*
ModuleDocCommentLine = "%%%"[^\r\n]*
FunctionDocCommentLine = "%%"[^\r\n]*
CommentLine = "%"[^\r\n]*

ModuleDocComment = {ModuleDocCommentLine} {ModuleDocCommentLine}*
FunctionDocComment = {FunctionDocCommentLine} {FunctionDocCommentLine}*
Comment = {CommentLine} {CommentLine}*
Whitespace = ([ \t\n] | {ControlCharacter})+
ErlangUppercase = [A-Z]
ErlangLowercase = [a-z]
ErlangLetter = {ErlangUppercase} | {ErlangLowercase}
ErlangDigit = [0-9]
InputCharacter = [^\n]

DecimalLiteral = [0-9]+
ExplicitRadixLiteral = [0-9]{1,2} "#" [0-9a-fA-F]+

IntegerLiteral = {DecimalLiteral} | {ExplicitRadixLiteral}

ExponentPart = [Ee] [+-]? {DecimalLiteral}
FloatLiteral = {DecimalLiteral} "." {DecimalLiteral} {ExponentPart}?


OctalEscape = \\ [0-7]{1,3}
ControlName = [@A-Z\[\\\]\^_] /* this is the octal range \100 - \137 */ 
ControlEscape = \\ \^ {ControlName}
EscapeSequence = \\\" | "\\b" | "\\d" | "\\e" | "\\f" | "\\n" | "\\r" | "\\s" | "\\t" | "\\v" | "\\'" | "\\\\" | "\\[" | "\\{" | "\\]" | "\\}" | "\\`" | "\\$" | "\\=" | "\\%" | "\\," | "\\." | "\\_" | {ControlEscape} | {OctalEscape}

CharLiteralChar = {InputCharacter} | {EscapeSequence}
CharLiteral = \$ {CharLiteralChar} | \$

/* Without the \\\" at the start the lexer won't find it, for unknown reasons */
ESC = "\\" ( [^] )
CHAR = {ESC} | [^\'\"\\]
STRING_BAD1 = \" ({CHAR} | \') *
StringLiteral = {STRING_BAD1} \"

NameChar = {ErlangLetter} | {ErlangDigit} | @ | _
NameChars = {NameChar}+

QuotedCharacter = \\' | {EscapeSequence}  | [^'\\] /* [a-zA-Z0-9#_.@,;:!?/&%$+*~\^-] */ 
AtomLiteral = ({ErlangLowercase} {NameChar}*) | "''" | (' {QuotedCharacter}+ ')

Variable = (_ {NameChars}) | ({ErlangUppercase} {NameChars}?)

UniversalPattern = _

%%
 {ShebangLine}                 { return getTokenStart() == 0 ? ERL_SHEBANG : com.intellij.psi.TokenType.ERROR_ELEMENT; }
 {ModuleDocComment}            { return ERL_MODULE_DOC_COMMENT; }
 {FunctionDocComment}          { return ERL_FUNCTION_DOC_COMMENT; }
 {Comment}                     { return ERL_COMMENT; }
 {Whitespace}                  { return com.intellij.psi.TokenType.WHITE_SPACE; }

 "after"                       { return ERL_AFTER; }
 "when"                        { return ERL_WHEN; }
 "begin"                       { return ERL_BEGIN; }
 "end"                         { return ERL_END; }
 "of"                          { return ERL_OF; }
 "case"                        { return ERL_CASE; }
 "fun"                         { return ERL_FUN; }
 "try"                         { return ERL_TRY; }
 "catch"                       { return ERL_CATCH; }
 "if"                          { return ERL_IF; }
 "receive"                     { return ERL_RECEIVE; }

 "<<"                           { return ERL_BIN_START; }
 ">>"                           { return ERL_BIN_END; }
 "+"                            { return ERL_OP_PLUS; }
 "-"                            { return ERL_OP_MINUS; }
 "*"                            { return ERL_OP_AR_MUL; }
 "/"                            { return ERL_OP_AR_DIV; }
 "div"                          { return ERL_DIV; }
 "rem"                          { return ERL_REM; }
 "or"                           { return ERL_OR; }
 "xor"                          { return ERL_XOR; }
 "bor"                          { return ERL_BOR; }
 "bxor"                         { return ERL_BXOR; }
 "bsl"                          { return ERL_BSL; }
 "bsr"                          { return ERL_BSR; }
 "and"                          { return ERL_AND; }
 "band"                         { return ERL_BAND; }
 "=="                           { return ERL_OP_EQ_EQ; }
 "/="                           { return ERL_OP_DIV_EQ; }
 "=:="                          { return ERL_OP_EQ_COL_EQ; }
 "=/="                          { return ERL_OP_EQ_DIV_EQ; }
 "<"                            { return ERL_OP_LT; }
 "=<"                           { return ERL_OP_EQ_LT; }
 ">"                            { return ERL_OP_GT; }
 ">="                           { return ERL_OP_GT_EQ; }
 "not"                          { return ERL_NOT; }
 "bnot"                         { return ERL_BNOT; }
 "++"                           { return ERL_OP_PLUS_PLUS; }
 "--"                           { return ERL_OP_MINUS_MINUS; }
 "="                            { return ERL_OP_EQ; }
 "!"                            { return ERL_OP_EXL; }
 "<-"                           { return ERL_OP_LT_MINUS; }
 "<="                           { return ERL_OP_LT_EQ; }
 "andalso"                      { return ERL_ANDALSO; }
 "orelse"                       { return ERL_ORELSE; }

 {IntegerLiteral}              { return ERL_INTEGER; }
 {FloatLiteral}                { return ERL_FLOAT; }
 {UniversalPattern}            { return ERL_UNI_PATTERN; }

 {CharLiteral}                 { return ERL_CHAR; }
 {StringLiteral}               { return ERL_STRING; }
 {AtomLiteral}                 { return ERL_ATOM; }
 {Variable}                    { return ERL_VAR; }

  "("                           { return ERL_PAR_LEFT; }
  ")"                           { return ERL_PAR_RIGHT; }
  "{"                           { return ERL_CURLY_LEFT; }
  "}"                           { return ERL_CURLY_RIGHT; }
  "["                           { return ERL_BRACKET_LEFT; }
  "]"                           { return ERL_BRACKET_RIGHT; }
  "."                           { return ERL_DOT; }
  ".."                          { return ERL_DOT_DOT; }
  "..."                         { return ERL_DOT_DOT_DOT; }
  ":"                           { return ERL_COLON; }
  "::"                          { return ERL_COLON_COLON; }
  "||"                          { return ERL_OR_OR; }
  "|"                           { return ERL_OP_OR; }
  ";"                           { return ERL_SEMI; }
  ","                           { return ERL_COMMA; }
  "?"                           { return ERL_QMARK; }
  "->"                          { return ERL_ARROW; }
  "#"                           { return ERL_RADIX; }

 .                              { return com.intellij.psi.TokenType.BAD_CHARACTER; }