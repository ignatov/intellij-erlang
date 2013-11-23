/*
 * Copyright 2012-2013 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.lexer;

import com.intellij.lexer.Lexer;
import com.intellij.lexer.LexerPosition;
import com.intellij.lexer.LookAheadLexer;
import com.intellij.openapi.util.Condition;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Stack;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangLexer;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author savenko
 */
public class ErlangMacroSubstitutingLexer extends LookAheadLexer {
  private final ErlangMacroContext myMacroContext;

  public ErlangMacroSubstitutingLexer() {
    this(new ErlangFormsLexer());
  }

  public ErlangMacroSubstitutingLexer(Lexer baseLexer) {
    this(baseLexer, new ErlangMacroContext());
  }

  public ErlangMacroSubstitutingLexer(Lexer baseLexer, ErlangMacroContext context) {
    super(baseLexer);
    myMacroContext = context;
  }

  @Override
  protected void lookAhead(Lexer baseLexer) {
    if (baseLexer.getTokenType() == ErlangInterimTokenTypes.FORM) {
      formLookAhead(baseLexer.getBufferSequence(), baseLexer.getTokenStart(), baseLexer.getTokenEnd());
      baseLexer.advance();
    }
    else {
      super.lookAhead(baseLexer);
    }
  }

  private void formLookAhead(CharSequence formBuffer, int formStartIdx, int formEndIdx) {
    ErlangLexer lexer = new ErlangLexer();
    lexer.start(formBuffer, formStartIdx, formEndIdx);

    if (lexer.getTokenType() == ErlangTypes.ERL_OP_MINUS) {
      addTokenAndWhitespaceFrom(lexer);
      String attributeName = lexer.getTokenText();
      addTokenAndWhitespaceFrom(lexer);
      if ("define".equals(attributeName)) {
        macroDefinitionLookAhead(lexer);
      }
      else if ("include".equals(attributeName)) {
        includeLookAhead(lexer);
      }
      else if ("include_lib".equals(attributeName)) {
        includeLibLookAhead(lexer);
      }
      else if ("undef".equals(attributeName)) {
        undefLookAhead(lexer);
      }
    }
    //consume remaining tokens
    addAllTokensWithMacroSubstitution(lexer);
  }

  private void macroDefinitionLookAhead(ErlangLexer lexer) {
    if (lexer.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return;
    addTokenAndWhitespaceFrom(lexer);
    ErlangMacroBuilder macroBuilder = new ErlangMacroBuilder();

    //TODO handle quoted atom_names!
    //macro name and arguments list
    if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME && lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
    macroBuilder.setName(lexer.getTokenText());
    addTokenAndWhitespaceFrom(lexer);
    if (lexer.getTokenType() == ErlangTypes.ERL_PAR_LEFT) {
      macroBuilder.setHasParameters(true);
      addTokenAndWhitespaceFrom(lexer);
      if (lexer.getTokenType() == ErlangTypes.ERL_PAR_RIGHT) {
        addTokenAndWhitespaceFrom(lexer);
      }
      else {
        if (lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
        macroBuilder.addParameter(lexer.getTokenText());
        addTokenAndWhitespaceFrom(lexer);
        while (lexer.getTokenType() != ErlangTypes.ERL_PAR_RIGHT) {
          if (lexer.getTokenType() != ErlangTypes.ERL_COMMA) return;
          addTokenAndWhitespaceFrom(lexer);
          if (lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
          macroBuilder.addParameter(lexer.getTokenText());
          addTokenAndWhitespaceFrom(lexer);
        }
        addTokenAndWhitespaceFrom(lexer);
      }
    }

    if (lexer.getTokenType() != ErlangTypes.ERL_COMMA) return;
    int lastTokenEnd = lexer.getTokenEnd();
    addTokenFrom(lexer);

    //macro body
    addToken(lastTokenEnd, ErlangInterimTokenTypes.ERL_MACRO_BODY_BEGIN);
    addWhitespaceAndCommentsFrom(lexer);
    int tokensAfterLastRightParenthesis = 0; //includes the ')' token itself
    LexerPosition lexerRightParenthesisPosition = null;
    while (lexer.getTokenType() != null && lexer.getBufferEnd() != lexer.getTokenEnd()) {
      if (lexer.getTokenType() == ErlangTypes.ERL_PAR_RIGHT) {
        lexerRightParenthesisPosition = lexer.getCurrentPosition();
        tokensAfterLastRightParenthesis = 0;
      }
      if (lexerRightParenthesisPosition != null) {
        tokensAfterLastRightParenthesis++;
      }
      macroBuilder.addBodyToken(lexer.getTokenType(), lexer.getTokenText());
      addTokenFrom(lexer);
      lastTokenEnd = lexer.getTokenEnd();
    }
    if (lexer.getTokenType() == ErlangTypes.ERL_DOT && lexerRightParenthesisPosition != null) {
      lexer.restore(lexerRightParenthesisPosition);
      resetCacheSize(getCacheSize() - tokensAfterLastRightParenthesis);
      macroBuilder.dropBodyTokens(tokensAfterLastRightParenthesis);
      lastTokenEnd = lexer.getTokenStart();
    }
    addToken(lastTokenEnd, ErlangInterimTokenTypes.ERL_MACRO_BODY_END);
    addAllTokensFrom(lexer);
    ErlangMacro macro = macroBuilder.build();
    if (macro != null) {
      myMacroContext.defineMacro(macro);
    }
  }

  private void includeLookAhead(Lexer lexer) {
    //TODO implement
  }

  private void includeLibLookAhead(Lexer lexer) {
    //TODO implement
  }

  private void undefLookAhead(Lexer lexer) {
    //TODO implement
  }

  private void addAllTokensFrom(Lexer lexer) {
    while (lexer.getTokenType() != null) {
      addTokenFrom(lexer);
    }
  }

  private void addTokenAndWhitespaceFrom(Lexer lexer) {
    addTokenFrom(lexer);
    addWhitespaceAndCommentsFrom(lexer);
  }

  private void addWhitespaceAndCommentsFrom(Lexer lexer) {
    while (ErlangParserDefinition.WS.contains(lexer.getTokenType()) ||
      ErlangParserDefinition.COMMENTS.contains(lexer.getTokenType())) {
      addTokenFrom(lexer);
    }
  }

  private void addTokenFrom(Lexer lexer) {
    if (lexer.getTokenType() != null) {
      addToken(lexer.getTokenEnd(), lexer.getTokenType());
      lexer.advance();
    }
  }

  //TODO support quotes
  private void addAllTokensWithMacroSubstitution(Lexer baseLexer) {
    new MacroSubstitutionWorker(baseLexer).doWork();
  }

  private final class MacroSubstitutionWorker {
    private final Lexer myBaseLexer;
    private final Stack<Lexer> myLexersStack = ContainerUtil.newStack();
    private MacroCallParsingState myMacroCallParsingState = MacroCallParsingState.NONE;
    private MacroCallBuilder myMacroCallBuilder = new MacroCallBuilder();
    private int myOpenParenthesesCount = 0;
    private MacroSubstitutionWorkerPosition myMacroNameEndPosition;

    public MacroSubstitutionWorker(Lexer baseLexer) {
      myBaseLexer = baseLexer;
      myLexersStack.push(baseLexer);
    }

    public void doWork() {
      while (!myLexersStack.isEmpty()) {
        Lexer lexer = myLexersStack.peek();
        // macro call arguments should contain all whitespace symbols so that ?MACRO(X = <<0:8>>) works fine
        if (myMacroCallParsingState != MacroCallParsingState.ARGUMENTS_LIST) {
          addNotForeignWhitespaceAndCommentsFrom(lexer);
        }
        IElementType tokenType = lexer.getTokenType();
        String tokenText = lexer.getTokenText();
        if (tokenType == null) {
          if (myMacroCallParsingState == MacroCallParsingState.MACRO_NAME) {
            processMacroCall();
          }
          else {
            if (myMacroCallParsingState != MacroCallParsingState.QMARK) {
              resetMacroCallParsing();
            }
            myLexersStack.pop();
            continue;
          }
        }
        switch (myMacroCallParsingState) {
          case NONE: {
            if (tokenType == ErlangTypes.ERL_QMARK) {
              myMacroCallParsingState = MacroCallParsingState.QMARK;
            }
            break;
          }
          case QMARK: {
            if (tokenType == ErlangTypes.ERL_VAR || tokenType == ErlangTypes.ERL_ATOM) {
              myMacroCallParsingState = MacroCallParsingState.MACRO_NAME;
              myMacroCallBuilder.setMacroName(tokenText);
              myMacroNameEndPosition = new MacroSubstitutionWorkerPosition();
            }
            else {
              resetMacroCallParsing();
            }
            break;
          }
          case MACRO_NAME: {
            if (tokenType == ErlangTypes.ERL_PAR_LEFT) {
              myMacroCallParsingState = MacroCallParsingState.ARGUMENTS_LIST;
              myMacroCallBuilder.setCanHaveArguments();
              myOpenParenthesesCount = 1;
            }
            else { // no arguments specified: the macro call has a form ?MACRO
              processMacroCall();
            }
            break;
          }
          case ARGUMENTS_LIST: {
            if (myOpenParenthesesCount == 1 && tokenType == ErlangTypes.ERL_COMMA) {
              myMacroCallBuilder.completeMacroArgument();
            }
            else if (myOpenParenthesesCount == 1 && tokenType == ErlangTypes.ERL_PAR_RIGHT) {
              myMacroCallBuilder.completeMacroArgument();
              processMacroCall();
            }
            else {
              //TODO handle ERL_DOT - stop trying to build a macro call and continue lexing.
              if (tokenType == ErlangTypes.ERL_PAR_RIGHT) {
                myOpenParenthesesCount--;
              }
              else if (tokenType == ErlangTypes.ERL_PAR_LEFT) {
                myOpenParenthesesCount++;
              }
              assert tokenType != null;
              myMacroCallBuilder.appendMacroArgument(tokenText, tokenType);
            }
            break;
          }
        }
        addMayBeForeignTokenFrom(lexer);
      }
    }

    private void processMacroCall() {
      MacroCall macroCall = myMacroCallBuilder.build();
      if (macroCall == null) {
        //TODO report ill-formed macro call
        return;
      }
      // this code handles calls like ?MACRO(arg1, arg2) where arguments may not be a MACRO's arguments
      ErlangMacro macro = null;
      List<String> macroArguments = macroCall.getPossibleArgumentsList();
      if (macroArguments != null) {
        macro = myMacroContext.getParameterizedMacro(macroCall.getName(), macroArguments.size());
      }
      if (macro == null) {
        macro = myMacroContext.getParameterlessMacro(macroCall.getName());
        macroArguments = null;
        myMacroNameEndPosition.restore();
      }
      if (macro != null) {
        String substitution = macro.substitute(macroArguments);
        ErlangLexer substitutionLexer = new ErlangLexer();
        substitutionLexer.start(substitution);
        myLexersStack.push(substitutionLexer);
      }
      myMacroNameEndPosition = null;
      resetMacroCallParsing();
    }

    public void resetMacroCallParsing() {
      myMacroCallBuilder.reset();
      myOpenParenthesesCount = 0;
      myMacroCallParsingState = MacroCallParsingState.NONE;
    }

    private void addNotForeignWhitespaceAndCommentsFrom(Lexer lexer) {
      if (lexer != myBaseLexer) {
        //drop whitespace tokens from macro body.
        while (ErlangParserDefinition.WS.contains(lexer.getTokenType()) ||
          ErlangParserDefinition.COMMENTS.contains(lexer.getTokenType())) {
          lexer.advance();
        }
      }
      addWhitespaceAndCommentsFrom(lexer);
    }

    private void addMayBeForeignTokenFrom(Lexer lexer) {
      IElementType tokenType = lexer.getTokenType();
      int tokenEndOffset = myBaseLexer.getTokenEnd();
      assert tokenType != null;
      if (myBaseLexer != lexer) {
        tokenEndOffset = myBaseLexer.getTokenStart();
        tokenType = new ErlangForeignLeafType(tokenType, lexer.getTokenText());
      }
      addToken(tokenEndOffset, tokenType);
      lexer.advance();
    }

    private class MacroSubstitutionWorkerPosition {
      private final LexerPosition myBaseLexerPosition = myBaseLexer.getCurrentPosition();
      private final LexerPosition myTopmostLexerPosition = myLexersStack.peek().getCurrentPosition();
      private final int mySubstitutingLexerCacheSize = ErlangMacroSubstitutingLexer.this.getCacheSize();

      public void restore() {
        myBaseLexer.restore(myBaseLexerPosition);
        myLexersStack.peek().restore(myTopmostLexerPosition);
        ErlangMacroSubstitutingLexer.this.resetCacheSize(mySubstitutingLexerCacheSize);
        myMacroNameEndPosition = null;
      }
    }
  }

  private static enum MacroCallParsingState {
    NONE, QMARK, MACRO_NAME, ARGUMENTS_LIST
  }

  private static final class MacroCallBuilder {
    private String myMacroName;
    private StringBuilder myMacroArgumentBuilder = new StringBuilder();
    private boolean myMacroArgumentHasNonWhitespaceTokens;
    private List<MacroCallArgument> myArguments;
    private boolean myCanHaveArguments;

    public MacroCallBuilder() {
      reset();
    }

    public void setMacroName(String name) {
      myMacroName = name;
    }

    public void appendMacroArgument(String tokenText, IElementType tokenType) {
      if (!ErlangParserDefinition.COMMENTS.contains(tokenType) &&
        !ErlangParserDefinition.WS.contains(tokenType)) {
        myMacroArgumentHasNonWhitespaceTokens = true;
      }
      myMacroArgumentBuilder.append(tokenText);
    }

    public void completeMacroArgument() {
      if (myMacroArgumentBuilder.length() != 0) {
        myArguments.add(new MacroCallArgument(myMacroArgumentBuilder.toString(), myMacroArgumentHasNonWhitespaceTokens));
        resetMacroArgumentBuilder();
      }
    }

    public void setCanHaveArguments() {
      myCanHaveArguments = true;
    }

    public void reset() {
      myMacroName = null;
      resetMacroArgumentBuilder();
      myArguments = ContainerUtil.newArrayList();
      myCanHaveArguments = false;
    }

    @Nullable
    public MacroCall build() {
      String macroName = myMacroName;
      List<String> argumentsList = null;
      if (myCanHaveArguments) {
        if (myArguments.size() == 1) {
          MacroCallArgument argument = myArguments.get(0);
          argumentsList = ContainerUtil.createMaybeSingletonList(argument.hasNonWhitespaceTokens() ? argument.getText() : null);
        }
        else {
          MacroCallArgument whitespaceOnlyArgument = ContainerUtil.find(myArguments, new Condition<MacroCallArgument>() {
            @Override
            public boolean value(MacroCallArgument macroCallArgument) {
              return !macroCallArgument.hasNonWhitespaceTokens();
            }
          });
          if (whitespaceOnlyArgument != null) {
            reset();
            return null;
          }
          argumentsList = ContainerUtil.map(myArguments, new Function<MacroCallArgument, String>() {
            @Override
            public String fun(MacroCallArgument macroCallArgument) {
              return macroCallArgument.getText();
            }
          });
        }
      }
      reset();
      return new MacroCall(macroName, argumentsList);
    }

    private void resetMacroArgumentBuilder() {
      myMacroArgumentBuilder.setLength(0);
      myMacroArgumentHasNonWhitespaceTokens = false;
    }
  }

  private static final class MacroCallArgument {
    private final String myText;
    private final boolean myHasNonWhitespaceTokens;

    private MacroCallArgument(String text, boolean hasNonWhitespaceTokens) {
      myText = text;
      myHasNonWhitespaceTokens = hasNonWhitespaceTokens;
    }

    public String getText() {
      return myText;
    }

    public boolean hasNonWhitespaceTokens() {
      return myHasNonWhitespaceTokens;
    }
  }

  private static final class MacroCall {
    private final String myName;
    private final List<String> myPossibleArgumentsList;

    private MacroCall(String name, @Nullable List<String> possibleArgumentsList) {
      myName = name;
      myPossibleArgumentsList = possibleArgumentsList;
    }

    public String getName() {
      return myName;
    }

    @Nullable
    public List<String> getPossibleArgumentsList() {
      return myPossibleArgumentsList;
    }
  }
}
