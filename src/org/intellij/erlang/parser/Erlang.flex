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
WhitespaceChar = [ \t\n] | {ControlCharacter}
Whitespace = {WhitespaceChar}+
ErlangUppercase = [A-Z]
ErlangLowercase = [a-z]
ErlangLetter = {ErlangUppercase} | {ErlangLowercase}
ErlangDigit = [0-9]
InputCharacter = [^\n]

DecimalLiteral = [0-9]+
ExplicitRadixLiteral = [0-9]{1,2} "#" [0-9a-zA-Z]+

IntegerLiteral = {DecimalLiteral} | {ExplicitRadixLiteral}

ExponentPart = [Ee] [+-]? {DecimalLiteral}
FloatLiteral = {DecimalLiteral} "." {DecimalLiteral} {ExponentPart}?


OctalEscape = \\ [0-7]{1,3}
ControlName = [@A-Z\[\\\]\^_] /* this is the octal range \100 - \137 */ 
ControlEscape = \\ \^ {ControlName}
EscapeSequence = \\\" | "\\b" | "\\d" | "\\e" | "\\f" | "\\n" | "\\r" | "\\s" | "\\t" | "\\v" | "\\'" | "\\\\" | "\\[" | "\\{" | "\\]" | "\\}" | "\\`" | "\\$" | "\\=" | "\\%" | "\\," | "\\." | "\\_" | {ControlEscape} | {OctalEscape}

CharLiteralChar = {InputCharacter} | {EscapeSequence} | (\\ {WhitespaceChar})
CharLiteral = \$ {CharLiteralChar} | \$

/* Without the \\\" at the start the lexer won't find it, for unknown reasons */
ESC = "\\" ( [^] )
CHAR = {ESC} | [^\'\"\\]
STRING_BAD1 = \" ({CHAR} | \') *
StringLiteral = {STRING_BAD1} \"

NameChar = {ErlangLetter} | {ErlangDigit} | @ | _
NameChars = {NameChar}*

QuotedCharacter = \\' | {EscapeSequence}  | [^'\\] /* [a-zA-Z0-9#_.@,;:!?/&%$+*~\^-] */
QuotedAtomName = {QuotedCharacter}+
AtomName = ({ErlangLowercase} {NameChar}*)
EmptyAtom = ''

Variable = (_ {NameChars}) | ({ErlangUppercase} {NameChars})

%state IN_QUOTES

%%
<YYINITIAL> {ShebangLine}                 { return getTokenStart() == 0 ? ERL_SHEBANG : com.intellij.psi.TokenType.ERROR_ELEMENT; }
<YYINITIAL> {ModuleDocComment}            { return ERL_MODULE_DOC_COMMENT; }
<YYINITIAL> {FunctionDocComment}          { return ERL_FUNCTION_DOC_COMMENT; }
<YYINITIAL> {Comment}                     { return ERL_COMMENT; }
<YYINITIAL> {Whitespace}                  { return com.intellij.psi.TokenType.WHITE_SPACE; }

<YYINITIAL> "after"                       { return ERL_AFTER; }
<YYINITIAL> "when"                        { return ERL_WHEN; }
<YYINITIAL> "begin"                       { return ERL_BEGIN; }
<YYINITIAL> "end"                         { return ERL_END; }
<YYINITIAL> "of"                          { return ERL_OF; }
<YYINITIAL> "case"                        { return ERL_CASE; }
<YYINITIAL> "fun"                         { return ERL_FUN; }
<YYINITIAL> "try"                         { return ERL_TRY; }
<YYINITIAL> "catch"                       { return ERL_CATCH; }
<YYINITIAL> "if"                          { return ERL_IF; }
<YYINITIAL> "receive"                     { return ERL_RECEIVE; }

<YYINITIAL> ":="                          { return ERL_MATCH; }
<YYINITIAL> "=>"                          { return ERL_ASSOC; }

<YYINITIAL> "<<"                           { return ERL_BIN_START; }
<YYINITIAL> ">>"                           { return ERL_BIN_END; }
<YYINITIAL> "+"                            { return ERL_OP_PLUS; }
<YYINITIAL> "-"                            { return ERL_OP_MINUS; }
<YYINITIAL> "*"                            { return ERL_OP_AR_MUL; }
<YYINITIAL> "/"                            { return ERL_OP_AR_DIV; }
<YYINITIAL> "div"                          { return ERL_DIV; }
<YYINITIAL> "rem"                          { return ERL_REM; }
<YYINITIAL> "or"                           { return ERL_OR; }
<YYINITIAL> "xor"                          { return ERL_XOR; }
<YYINITIAL> "bor"                          { return ERL_BOR; }
<YYINITIAL> "bxor"                         { return ERL_BXOR; }
<YYINITIAL> "bsl"                          { return ERL_BSL; }
<YYINITIAL> "bsr"                          { return ERL_BSR; }
<YYINITIAL> "and"                          { return ERL_AND; }
<YYINITIAL> "band"                         { return ERL_BAND; }
<YYINITIAL> "=="                           { return ERL_OP_EQ_EQ; }
<YYINITIAL> "/="                           { return ERL_OP_DIV_EQ; }
<YYINITIAL> "=:="                          { return ERL_OP_EQ_COL_EQ; }
<YYINITIAL> "=/="                          { return ERL_OP_EQ_DIV_EQ; }
<YYINITIAL> "<"                            { return ERL_OP_LT; }
<YYINITIAL> "=<"                           { return ERL_OP_EQ_LT; }
<YYINITIAL> ">"                            { return ERL_OP_GT; }
<YYINITIAL> ">="                           { return ERL_OP_GT_EQ; }
<YYINITIAL> "not"                          { return ERL_NOT; }
<YYINITIAL> "bnot"                         { return ERL_BNOT; }
<YYINITIAL> "++"                           { return ERL_OP_PLUS_PLUS; }
<YYINITIAL> "--"                           { return ERL_OP_MINUS_MINUS; }
<YYINITIAL> "="                            { return ERL_OP_EQ; }
<YYINITIAL> "!"                            { return ERL_OP_EXL; }
<YYINITIAL> "<-"                           { return ERL_OP_LT_MINUS; }
<YYINITIAL> "<="                           { return ERL_OP_LT_EQ; }
<YYINITIAL> "andalso"                      { return ERL_ANDALSO; }
<YYINITIAL> "orelse"                       { return ERL_ORELSE; }

<YYINITIAL> {IntegerLiteral}              { return ERL_INTEGER; }
<YYINITIAL> {FloatLiteral}                { return ERL_FLOAT; }

<YYINITIAL> {CharLiteral}                 { return ERL_CHAR; }
<YYINITIAL> {StringLiteral}               { return ERL_STRING; }

<YYINITIAL> {AtomName} | {EmptyAtom}      { return ERL_ATOM_NAME; }
<YYINITIAL> '                             { yybegin(IN_QUOTES); return ERL_SINGLE_QUOTE; }
<IN_QUOTES> {QuotedAtomName}              { return ERL_ATOM_NAME; }
<IN_QUOTES> '                             { yybegin(YYINITIAL); return ERL_SINGLE_QUOTE; }

<YYINITIAL> {Variable}                    { return ERL_VAR; }

<YYINITIAL>  "("                           { return ERL_PAR_LEFT; }
<YYINITIAL>  ")"                           { return ERL_PAR_RIGHT; }
<YYINITIAL>  "{"                           { return ERL_CURLY_LEFT; }
<YYINITIAL>  "}"                           { return ERL_CURLY_RIGHT; }
<YYINITIAL>  "["                           { return ERL_BRACKET_LEFT; }
<YYINITIAL>  "]"                           { return ERL_BRACKET_RIGHT; }
<YYINITIAL>  "."                           { return ERL_DOT; }
<YYINITIAL>  ".."                          { return ERL_DOT_DOT; }
<YYINITIAL>  "..."                         { return ERL_DOT_DOT_DOT; }
<YYINITIAL>  ":"                           { return ERL_COLON; }
<YYINITIAL>  "::"                          { return ERL_COLON_COLON; }
<YYINITIAL>  "||"                          { return ERL_OR_OR; }
<YYINITIAL>  "|"                           { return ERL_OP_OR; }
<YYINITIAL>  ";"                           { return ERL_SEMI; }
<YYINITIAL>  ","                           { return ERL_COMMA; }
<YYINITIAL>  "?"                           { return ERL_QMARK; }
<YYINITIAL>  "->"                          { return ERL_ARROW; }
<YYINITIAL>  "#"                           { return ERL_RADIX; }

<YYINITIAL> .                              { return com.intellij.psi.TokenType.BAD_CHARACTER; }