package org.intellij.erlang.lexer;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import static org.intellij.erlang.ErlangParserDefinition.*;
import static org.intellij.erlang.lexer.ErlangInterimTokenTypes.*;

%%

%{
  public _ErlangFormsLexer() {
    this((java.io.Reader)null);
  }
%}

%class _ErlangFormsLexer
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

WhitespaceChar = [ \t\n] | {ControlCharacter}
Whitespace = {WhitespaceChar}+

/* Without the \\\" at the start the lexer won't find it, for unknown reasons */
ESC = "\\" ( [^] )
CHAR = {ESC} | [^\'\"\\]
STRING_BAD1 = \" ({CHAR} | \') *
StringLiteral = {STRING_BAD1} \"

OctalEscape = \\ [0-7]{1,3}
ControlName = [@A-Z\[\\\]\^_] /* this is the octal range \100 - \137 */
ControlEscape = \\ \^ {ControlName}
EscapeSequence = \\\" | "\\b" | "\\d" | "\\e" | "\\f" | "\\n" | "\\r" | "\\s" | "\\t" | "\\v" | "\\'" | "\\\\" | "\\[" | "\\{" | "\\]" | "\\}" | "\\`" | "\\$" | "\\=" | "\\%" | "\\," | "\\." | "\\_" | {ControlEscape} | {OctalEscape}
QuotedAtomLiteral = '(\\' | {EscapeSequence}  | [^'\\])+'

CharLiteral = \$.

FormToken = {StringLiteral} | {QuotedAtomLiteral} | {CharLiteral} | .

%state FORM

%%
<YYINITIAL>       {ShebangLine}                 { return getTokenStart() == 0 ? ERL_SHEBANG : com.intellij.psi.TokenType.ERROR_ELEMENT; }

<YYINITIAL, FORM> {ModuleDocCommentLine}        { return ERL_MODULE_DOC_COMMENT; }
<YYINITIAL, FORM> {FunctionDocCommentLine}      { return ERL_FUNCTION_DOC_COMMENT; }
<YYINITIAL, FORM> {CommentLine}                 { return ERL_COMMENT; }
<YYINITIAL, FORM> {Whitespace}                  { return com.intellij.psi.TokenType.WHITE_SPACE; }
<YYINITIAL, FORM> {FormToken}                   { yybegin(FORM); return ANY; }

<FORM>            "."/ ({WhitespaceChar} | "%") { yybegin(YYINITIAL); return ANY; }
<FORM>            <<EOF>>                       { yybegin(YYINITIAL); return null; }