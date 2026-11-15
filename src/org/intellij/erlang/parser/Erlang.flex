package org.intellij.erlang.parser;

import com.intellij.lexer.FlexLexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.Nullable;
import static org.intellij.erlang.ErlangParserDefinition.*;
import static org.intellij.erlang.ErlangTypes.*;

%%

%{
    // Used by the tokenizer to request language features to be enabled
    public Project project = null;

    public _ErlangLexer(@Nullable Project project_) {
      this((java.io.Reader)null);
      this.project = project_;
    }

    // OTP 26 and later end a string or sigil after a \^ that is not followed by a control name; ErlangAnnotator reports it
    private IElementType endAtBadControlEscape(IElementType type) {
      CharSequence text = yytext();
      if (text.length() < 2 || text.charAt(0) == '"' && text.charAt(1) == '"' || text.charAt(0) == '~' && (text.charAt(1) == 'B' || text.charAt(1) == 'S')) return type;
      for (int i = 0; i < text.length() - 1; i++) {
        if (text.charAt(i) != '\\') continue;
        if (text.charAt(i + 1) == '^' && (i + 2 >= text.length() || !isControlName(text.charAt(i + 2)))) {
          if (ErlangSdkType.getRelease(project).erlangControlEscapeNeedsControlName()) yypushback(text.length() - i - 2);
          return type;
        }
        i += text.charAt(i + 1) == '^' ? 2 : 1;
      }
      return type;
    }

    private static boolean isControlName(char c) {
      return c >= '@' && c <= '_' || c >= 'a' && c <= 'z' || c == '?';
    }
%}

%class _ErlangLexer
%implements FlexLexer
%unicode
%public

%function advance
%type IElementType

/* This hex range is the same as octal \O00 - \O37 */
ControlCharacter = [\000 - \037]

ShebangLine = "#!"[^\r\n]*
ModuleDocCommentLine = "%%%"[^\r\n]*
FunctionDocCommentLine = "%%"[^\r\n]*
CommentLine = "%"[^\r\n]*

ModuleDocComment = {ModuleDocCommentLine} {ModuleDocCommentLine}*
FunctionDocComment = {FunctionDocCommentLine} {FunctionDocCommentLine}*
Comment = {CommentLine} {CommentLine}*
WhitespaceChar = [ \t\n\xa0] | {ControlCharacter}
Whitespace = {WhitespaceChar}+
ErlangUppercase = [A-Z]
ErlangLowercase = [a-z]
ErlangLetter = {ErlangUppercase} | {ErlangLowercase}
ErlangDigit = [0-9]
InputCharacter = [^\n]

DecimalLiteral = [0-9]+ ( _[0-9]+ )*
ExplicitRadixLiteral = {DecimalLiteral} "#" {BasedDigits}

IntegerLiteral = {DecimalLiteral} | {ExplicitRadixLiteral}

ExponentPart = [Ee] [+-]? {DecimalLiteral}
FloatLiteral = {DecimalLiteral} "." {DecimalLiteral} {ExponentPart}?

/* Based float literals (OTP 28+, EEP 75): base#digits.digits with an optional #e exponent */
BasedDigits = [0-9a-zA-Z]+ ( _[0-9a-zA-Z]+ )*
BasedFloatLiteral = [0-9]{1,2} "#" {BasedDigits} "." {BasedDigits} ("#" {ExponentPart})?

OctalEscape = \\ [0-7]{1,3}
ControlName = [@A-Z\[\\\]\^_a-z?] /* \100 - \137, \141 - \172 and ? (\^? is DEL) */
ControlEscape = \\ \^ {ControlName}
/* Any other character after a backslash stands for itself (or for its control code, as \n does); \x and \^ have their own forms */
GenericEscape = \\ [^x\^]
HexEscape = \\ x ( [0-9a-fA-F]{2} | \{ [0-9a-fA-F]+ \} )
EscapeSequence = {GenericEscape} | {ControlEscape} | {OctalEscape} | {HexEscape}

CharLiteralChar = {InputCharacter} | {EscapeSequence} | (\\ {WhitespaceChar})
CharLiteral = \$ {CharLiteralChar} | \$

/* Without the \\\" at the start the lexer won't find it, for unknown reasons */
/* \^ takes the next character too, so "\^\" ends after the second backslash; endAtBadControlEscape ends the literal earlier for OTP 26+ */
ESC = "\\" ( \^ [^] | [^\^] )
/* Only a control name follows \^ here, so the BadCaret rules below end at the first \^ that has none */
STRICT_ESC = "\\" ( \^ {ControlName} | [^\^] )
/* ~B and ~S sigils are verbatim, so \^ does not take the next character there */
VERBATIM_ESC = "\\" [^]
CHAR = {ESC} | [^\'\"\\]
STRING_BAD1 = \" ({CHAR} | \') *
/* A \^ before a character that is not a control name ends an unterminated literal there, as in erl_scan of OTP 26+ */
BadCaret = \\ \^
NotControlName = [^@A-Z\[\\\]\^_a-z?]
StringBadCaret = \" ({STRICT_ESC} | [^\'\"\\] | \')* {BadCaret}
TripleQuotedString = \"\"\" ([^\"] | \"[^\"] | \"\"[^\"])* \"\"\"
StringLiteral = {TripleQuotedString} | {STRING_BAD1} \"

NameChar = {ErlangLetter} | {ErlangDigit} | @ | _
NameChars = {NameChar}*

QuotedCharacter = \\' | {EscapeSequence}  | [^'\\] /* [a-zA-Z0-9#_.@,;:!?/&%$+*~\^-] */
QuotedAtomName = {QuotedCharacter}+
AtomName = ({ErlangLowercase} {NameChar}*)
EmptyAtom = ''

Variable = (_ {NameChars}) | ({ErlangUppercase} {NameChars})

/* Sigil support (OTP 27+) */
/* Sigil with double-quoted string delimiter */
SigilDoubleQuoteString = "~" [bs]? \" ([^\\\"] | {ESC})* \" | "~" [BS] \" ([^\\\"] | {VERBATIM_ESC})* \"
/* Sigil with triple-quoted string delimiter */
SigilTripleQuoteString = "~" [bBsS]? \"\"\" ([^\"] | \"[^\"] | \"\"[^\"])* \"\"\"
/* Sigil with single-quote delimiter */
SigilSingleQuoteString = "~" [bs]? \' ([^\\\'] | {ESC})* \' | "~" [BS] \' ([^\\\'] | {VERBATIM_ESC})* \'
/* Sigil with parentheses delimiter */
SigilParenString = "~" [bs]? \( ([^\\\)] | {ESC})* \) | "~" [BS] \( ([^\\\)] | {VERBATIM_ESC})* \)
/* Sigil with brackets delimiter */
SigilBracketString = "~" [bs]? \[ ([^\\\]] | {ESC})* \] | "~" [BS] \[ ([^\\\]] | {VERBATIM_ESC})* \]
/* Sigil with braces delimiter; the } of a \x{...} escape does not end it */
SigilBraceString = "~" [bs]? \{ ([^\\\}] | {ESC} | {HexEscape})* \} | "~" [BS] \{ ([^\\\}] | {VERBATIM_ESC})* \}
/* Sigil with angle brackets delimiter */
SigilAngleString = "~" [bs]? \< ([^\\\>] | {ESC})* \> | "~" [BS] \< ([^\\\>] | {VERBATIM_ESC})* \>
/* Sigil with slash delimiter */
SigilSlashString = "~" [bs]? \/ ([^\\\\/] | {ESC})* \/ | "~" [BS] \/ ([^\\\\/] | {VERBATIM_ESC})* \/
/* Sigil with pipe delimiter */
SigilPipeString = "~" [bs]? \| ([^\\\|] | {ESC})* \| | "~" [BS] \| ([^\\\|] | {VERBATIM_ESC})* \|
/* Sigil with backtick delimiter */
SigilBacktickString = "~" [bs]? \` ([^\\\`] | {ESC})* \` | "~" [BS] \` ([^\\\`] | {VERBATIM_ESC})* \`
/* Sigil with hash delimiter */
SigilHashString = "~" [bs]? "#" ([^\\#] | {ESC})* "#" | "~" [BS] "#" ([^\\#] | {VERBATIM_ESC})* "#"

SigilDoubleQuoteBadCaret = "~" [bs]? \" ([^\\\"] | {STRICT_ESC})* {BadCaret}
SigilSingleQuoteBadCaret = "~" [bs]? \' ([^\\\'] | {STRICT_ESC})* {BadCaret}
SigilParenBadCaret = "~" [bs]? \( ([^\\\)] | {STRICT_ESC})* {BadCaret}
SigilBracketBadCaret = "~" [bs]? \[ ([^\\\]] | {STRICT_ESC})* {BadCaret}
SigilBraceBadCaret = "~" [bs]? \{ ([^\\\}] | {STRICT_ESC} | {HexEscape})* {BadCaret}
SigilAngleBadCaret = "~" [bs]? \< ([^\\\>] | {STRICT_ESC})* {BadCaret}
SigilSlashBadCaret = "~" [bs]? \/ ([^\\\\/] | {STRICT_ESC})* {BadCaret}
SigilPipeBadCaret = "~" [bs]? \| ([^\\\|] | {STRICT_ESC})* {BadCaret}
SigilBacktickBadCaret = "~" [bs]? \` ([^\\\`] | {STRICT_ESC})* {BadCaret}
SigilHashBadCaret = "~" [bs]? "#" ([^\\#] | {STRICT_ESC})* {BadCaret}

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
<YYINITIAL> "maybe"                       { boolean isSupported = ErlangSdkType.getRelease(this.project).erlangFeatureMaybe(); return isSupported ? ERL_MAYBE : ERL_ATOM_NAME; }
<YYINITIAL> "else"                        { return ERL_ELSE; }

<YYINITIAL> ":="                          { return ERL_MATCH; }
<YYINITIAL> "=>"                          { return ERL_ASSOC; }

<YYINITIAL> "<<"                          { return ERL_BIN_START; }
<YYINITIAL> ">>"                          { return ERL_BIN_END; }
<YYINITIAL> "+"                           { return ERL_OP_PLUS; }
<YYINITIAL> "-"                           { return ERL_OP_MINUS; }
<YYINITIAL> "*"                           { return ERL_OP_AR_MUL; }
<YYINITIAL> "/"                           { return ERL_OP_AR_DIV; }
<YYINITIAL> "div"                         { return ERL_DIV; }
<YYINITIAL> "rem"                         { return ERL_REM; }
<YYINITIAL> "or"                          { return ERL_OR; }
<YYINITIAL> "xor"                         { return ERL_XOR; }
<YYINITIAL> "bor"                         { return ERL_BOR; }
<YYINITIAL> "bxor"                        { return ERL_BXOR; }
<YYINITIAL> "bsl"                         { return ERL_BSL; }
<YYINITIAL> "bsr"                         { return ERL_BSR; }
<YYINITIAL> "and"                         { return ERL_AND; }
<YYINITIAL> "band"                        { return ERL_BAND; }
<YYINITIAL> "=="                          { return ERL_OP_EQ_EQ; }
<YYINITIAL> "/="                          { return ERL_OP_DIV_EQ; }
<YYINITIAL> "=:="                         { return ERL_OP_EQ_COL_EQ; }
<YYINITIAL> "=/="                         { return ERL_OP_EQ_DIV_EQ; }
<YYINITIAL> "<"                           { return ERL_OP_LT; }
<YYINITIAL> "=<"                          { return ERL_OP_EQ_LT; }
<YYINITIAL> ">"                           { return ERL_OP_GT; }
<YYINITIAL> ">="                          { return ERL_OP_GT_EQ; }
<YYINITIAL> "not"                         { return ERL_NOT; }
<YYINITIAL> "bnot"                        { return ERL_BNOT; }
<YYINITIAL> "++"                          { return ERL_OP_PLUS_PLUS; }
<YYINITIAL> "--"                          { return ERL_OP_MINUS_MINUS; }
<YYINITIAL> "="                           { return ERL_OP_EQ; }
<YYINITIAL> "?="                          { return ERL_OP_MAYBE_EQ; }
<YYINITIAL> "!"                           { return ERL_OP_EXL; }
<YYINITIAL> "<-"                          { return ERL_OP_LT_MINUS; }
<YYINITIAL> "<="                          { return ERL_OP_LT_EQ; }
<YYINITIAL> "<:-"                         { return ERL_OP_LT_COLON_MINUS; }
<YYINITIAL> "<:="                         { return ERL_OP_LT_COLON_EQ; }
<YYINITIAL> "andalso"                     { return ERL_ANDALSO; }
<YYINITIAL> "orelse"                      { return ERL_ORELSE; }
<YYINITIAL> "&&"                          { return ERL_OP_AND_AND; }

<YYINITIAL> {IntegerLiteral}              { return ERL_INTEGER; }
<YYINITIAL> {FloatLiteral}                { return ERL_FLOAT; }
<YYINITIAL> {BasedFloatLiteral}           { return ERL_FLOAT; }

<YYINITIAL> {CharLiteral}                 { return ERL_CHAR; }
<YYINITIAL> {StringLiteral}               { return endAtBadControlEscape(ERL_STRING); }
<YYINITIAL> {TripleQuotedString}          { return ERL_TRIPLE_QUOTED_STRING; }

<YYINITIAL> {SigilTripleQuoteString}      { return ERL_SIGIL_STRING; }
<YYINITIAL> {SigilDoubleQuoteString}      { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilSingleQuoteString}      { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilParenString}            { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilBracketString}          { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilBraceString}            { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilAngleString}            { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilSlashString}            { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilPipeString}             { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilBacktickString}         { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {SigilHashString}             { return endAtBadControlEscape(ERL_SIGIL_STRING); }
<YYINITIAL> {StringBadCaret} / {NotControlName} { return ERL_STRING; }
<YYINITIAL> ({SigilDoubleQuoteBadCaret} | {SigilSingleQuoteBadCaret} | {SigilParenBadCaret} | {SigilBracketBadCaret} | {SigilBraceBadCaret} | {SigilAngleBadCaret} | {SigilSlashBadCaret} | {SigilPipeBadCaret} | {SigilBacktickBadCaret} | {SigilHashBadCaret}) / {NotControlName} { return ERL_SIGIL_STRING; }

<YYINITIAL> {AtomName} | {EmptyAtom}      { return ERL_ATOM_NAME; }
<YYINITIAL> '                             { yybegin(IN_QUOTES); return ERL_SINGLE_QUOTE; }
<IN_QUOTES> {QuotedAtomName}              { return ERL_ATOM_NAME; }
<IN_QUOTES> '                             { yybegin(YYINITIAL); return ERL_SINGLE_QUOTE; }

<YYINITIAL> {Variable}                    { return ERL_VAR; }

<YYINITIAL>  "("                          { return ERL_PAR_LEFT; }
<YYINITIAL>  ")"                          { return ERL_PAR_RIGHT; }
<YYINITIAL>  "{"                          { return ERL_CURLY_LEFT; }
<YYINITIAL>  "}"                          { return ERL_CURLY_RIGHT; }
<YYINITIAL>  "["                          { return ERL_BRACKET_LEFT; }
<YYINITIAL>  "]"                          { return ERL_BRACKET_RIGHT; }
<YYINITIAL>  "."                          { return ERL_DOT; }
<YYINITIAL>  ".."                         { return ERL_DOT_DOT; }
<YYINITIAL>  "..."                        { return ERL_DOT_DOT_DOT; }
<YYINITIAL>  ":"                          { return ERL_COLON; }
<YYINITIAL>  "::"                         { return ERL_COLON_COLON; }
<YYINITIAL>  "||"                         { return ERL_OR_OR; }
<YYINITIAL>  "|"                          { return ERL_OP_OR; }
<YYINITIAL>  ";"                          { return ERL_SEMI; }
<YYINITIAL>  ","                          { return ERL_COMMA; }
<YYINITIAL>  "?"                          { return ERL_QMARK; }
<YYINITIAL>  "->"                         { return ERL_ARROW; }
<YYINITIAL>  "#"                          { return ERL_RADIX; }

[^]                                       { yybegin(YYINITIAL); return com.intellij.psi.TokenType.BAD_CHARACTER; }