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

import com.intellij.lang.BracePair;
import com.intellij.lexer.Lexer;
import com.intellij.lexer.LexerPosition;
import com.intellij.lexer.LookAheadLexer;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Stack;
import org.intellij.erlang.ErlangBraceMatcher;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.context.ErlangCompileContext;
import org.intellij.erlang.context.ErlangPathResolver;
import org.intellij.erlang.parser.ErlangLexer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.List;

public class ErlangMacroSubstitutingLexer extends LookAheadLexer {
  private static final int MAX_INCLUSION_STACK_DEPTH = 100;
  private static final BracePair[] BRACE_PAIRS = new ErlangBraceMatcher().getPairs();

  private final ErlangMacroContext myMacroContext;
  private final ErlangCompileContext myCompileContext;
  private final Stack<ConditionalBranchType> myConditionalBranchesStack = ContainerUtil.newStack();
  private final Stack<VirtualFile> myIncludeOwnersStack; //TODO replace with an immutable stack

  public ErlangMacroSubstitutingLexer(@NotNull ErlangCompileContext compileContext, @Nullable VirtualFile file) {
    this(new ErlangFormsLexer(), new ErlangMacroContext(), compileContext, file != null ? ContainerUtil.newStack(file) : ContainerUtil.<VirtualFile>newStack());
  }

  private ErlangMacroSubstitutingLexer(Lexer baseLexer, ErlangMacroContext context, ErlangCompileContext compileContext, Stack<VirtualFile> includeOwnersStack) {
    super(baseLexer);
    myMacroContext = context;
    myCompileContext = compileContext;
    myIncludeOwnersStack = includeOwnersStack;
  }

  @Override
  protected void lookAhead(Lexer baseLexer) {
    if (baseLexer.getTokenType() == ErlangInterimTokenTypes.FORM) {
      ConditionalBranchType branchType = interpretConditionalDirective(baseLexer.getBufferSequence(), baseLexer.getTokenStart(), baseLexer.getTokenEnd());
      formLookAhead(baseLexer.getBufferSequence(), baseLexer.getTokenStart(), baseLexer.getTokenEnd(), branchType);
      baseLexer.advance();
    }
    else {
      super.lookAhead(baseLexer);
    }
  }

  private void formLookAhead(CharSequence formBuffer, int formStartIdx, int formEndIdx, ConditionalBranchType branchType) {
    if (branchType == ConditionalBranchType.HAS_INACTIVE_PARENT || branchType == ConditionalBranchType.INACTIVE) {
      addToken(formEndIdx, ErlangParserDefinition.ERL_DISABLED_FORM);
      return;
    }

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

  private ConditionalBranchType interpretConditionalDirective(CharSequence formBuffer, int formStartIdx, int formEndIdx) {
    ErlangLexer lexer = new ErlangLexer();
    lexer.start(formBuffer, formStartIdx, formEndIdx);

    if (lexer.getTokenType() != ErlangTypes.ERL_OP_MINUS) return currentBranchType();
    skipTokenAndWhitespace(lexer);

    String attributeName = lexer.getTokenText();
    if ("ifdef".equals(attributeName) || "ifndef".equals(attributeName)) { //TODO upon entering an ifdef or an ifndef block you can decide whether some macro is defined or not
                                                                           //TODO be sure to clean this information up upon leaving if(n)def-endif block
      if (!myConditionalBranchesStack.isEmpty() &&
        (myConditionalBranchesStack.peek() == ConditionalBranchType.HAS_INACTIVE_PARENT ||
          myConditionalBranchesStack.peek() == ConditionalBranchType.INACTIVE)) {
        myConditionalBranchesStack.push(ConditionalBranchType.HAS_INACTIVE_PARENT);
        return ConditionalBranchType.HAS_INACTIVE_PARENT;
      }

      skipTokenAndWhitespace(lexer);
      if (lexer.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return currentBranchType();
      skipTokenAndWhitespace(lexer);
      String macroName = skipMacroNameTokens(lexer);
      if (macroName == null || lexer.getTokenType() != ErlangTypes.ERL_PAR_RIGHT) return currentBranchType();

      ErlangMacroDefinitionState macroDefinitionState = myMacroContext.getMacroDefinitionState(macroName);
      if (macroDefinitionState == ErlangMacroDefinitionState.FREE && myCompileContext.macroDefinitions.containsKey(macroName)) {
        macroDefinitionState =  ErlangMacroDefinitionState.DEFINED;
      }

      boolean ifndef = "ifndef".equals(attributeName);
      ConditionalBranchType newBranchType = macroDefinitionState == ErlangMacroDefinitionState.DEFINED ?
        (ifndef ? ConditionalBranchType.INACTIVE : ConditionalBranchType.ACTIVE) :
        (ifndef ? ConditionalBranchType.ACTIVE : ConditionalBranchType.INACTIVE);
      myConditionalBranchesStack.push(newBranchType);
      return ConditionalBranchType.ACTIVE;
    }
    else if ("else".equals(attributeName)) {
      ConditionalBranchType branchType = myConditionalBranchesStack.tryPop();
      ConditionalBranchType newBranchType = branchType == ConditionalBranchType.ACTIVE ? ConditionalBranchType.INACTIVE :
        branchType == ConditionalBranchType.INACTIVE ? ConditionalBranchType.ACTIVE : branchType;
      if (newBranchType != null) {
        ConditionalBranchType containingBranchType = currentBranchType();
        myConditionalBranchesStack.push(newBranchType);
        if (containingBranchType == ConditionalBranchType.ACTIVE || containingBranchType == ConditionalBranchType.FREE) {
          return ConditionalBranchType.ACTIVE;
        }
      }
    }
    else if ("endif".equals(attributeName)) {
      myConditionalBranchesStack.tryPop();
    }

    return currentBranchType();
  }

  private ConditionalBranchType currentBranchType() {
    return myConditionalBranchesStack.isEmpty() ? ConditionalBranchType.ACTIVE : myConditionalBranchesStack.peek();
  }

  private void macroDefinitionLookAhead(ErlangLexer lexer) {
    if (lexer.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return;
    addTokenAndWhitespaceFrom(lexer);
    ErlangMacroBuilder macroBuilder = new ErlangMacroBuilder();

    //macro name and arguments list
    boolean macroNameIsQuoted = lexer.getTokenType() == ErlangTypes.ERL_SINGLE_QUOTE;
    if (macroNameIsQuoted) {
      addTokenFrom(lexer);
      if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME) return;
      macroBuilder.setName(lexer.getTokenText());
      addTokenFrom(lexer);
      if (lexer.getTokenType() != ErlangTypes.ERL_SINGLE_QUOTE) return;
      addTokenAndWhitespaceFrom(lexer);
    }
    else {
      if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME && lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
      macroBuilder.setName(lexer.getTokenText());
      addTokenAndWhitespaceFrom(lexer);
    }
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
    processInclusion(lexer, false);
  }

  private void includeLibLookAhead(Lexer lexer) {
    processInclusion(lexer, true);
  }

  private void undefLookAhead(Lexer lexer) {
    if (lexer.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return;
    addTokenAndWhitespaceFrom(lexer);
    String macroName = consumeMacroNameAndAddTokens(lexer);
    if (macroName != null) {
      myMacroContext.undefineMacro(macroName);
    }
  }

  private void processInclusion(Lexer lexer, boolean isIncludeLib) {
    if (lexer.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return;
    addTokenAndWhitespaceFrom(lexer);
    if (lexer.getTokenType() != ErlangTypes.ERL_STRING) return;

    String includeString = StringUtil.unquoteString(lexer.getTokenText());
    VirtualFile inclusion = isIncludeLib ?
      ErlangPathResolver.resolveIncludeLib(myCompileContext.project, myIncludeOwnersStack, includeString) :
      ErlangPathResolver.resolveInclude(myCompileContext.project, myIncludeOwnersStack, includeString);
    if (inclusion != null && inclusion.isValid() && !inclusion.isDirectory()) {
      myIncludeOwnersStack.push(inclusion);

      try {
        if (myIncludeOwnersStack.size() < MAX_INCLUSION_STACK_DEPTH) {
          String text = VfsUtilCore.loadText(inclusion);
          ErlangMacroSubstitutingLexer inclusionLexer = new ErlangMacroSubstitutingLexer(new ErlangFormsLexer(), myMacroContext, myCompileContext, myIncludeOwnersStack);
          inclusionLexer.start(text);
          while (inclusionLexer.getTokenType() != null) {
            inclusionLexer.advance();
          }
        }
        else {
          System.err.println("Max inclusion stack depth reached. Skipping.");
        }
      } catch (IOException e) {
        //TODO report error
        e.printStackTrace();
      }
      finally {
        myIncludeOwnersStack.pop();
      }
    } else if (!myIncludeOwnersStack.isEmpty()) {
      //TODO report error
      System.err.println("Unresolved inclusion: " + includeString + " in " + myIncludeOwnersStack.peek());
    }

    addAllTokensFrom(lexer);
  }

  @Nullable
  private String consumeMacroNameAndAddTokens(Lexer lexer) {
    String macroName;
    boolean macroNameIsQuoted = lexer.getTokenType() == ErlangTypes.ERL_SINGLE_QUOTE;
    if (macroNameIsQuoted) {
      addTokenFrom(lexer);
      if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME) return null;
      macroName = lexer.getTokenText();
      addTokenFrom(lexer);
      if (lexer.getTokenType() != ErlangTypes.ERL_SINGLE_QUOTE) return null;
    }
    else {
      if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME && lexer.getTokenType() != ErlangTypes.ERL_VAR) return null;
      macroName = lexer.getTokenText();
    }
    addTokenAndWhitespaceFrom(lexer); // eat up the a single quote, atom_name or var
    return macroName;
  }

  @Nullable
  private static String skipMacroNameTokens(Lexer lexer) {
    String macroName;
    boolean macroNameIsQuoted = lexer.getTokenType() == ErlangTypes.ERL_SINGLE_QUOTE;
    if (macroNameIsQuoted) {
      lexer.advance();
      if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME) return null;
      macroName = lexer.getTokenText();
      lexer.advance();
      if (lexer.getTokenType() != ErlangTypes.ERL_SINGLE_QUOTE) return null;
    }
    else {
      if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME && lexer.getTokenType() != ErlangTypes.ERL_VAR) return null;
      macroName = lexer.getTokenText();
    }
    skipTokenAndWhitespace(lexer); // eat up the a single quote, atom_name or var
    return macroName;
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

  private static void skipTokenAndWhitespace(Lexer lexer) {
    lexer.advance();
    skipWhitespace(lexer);
  }

  private static void skipWhitespace(Lexer lexer) {
    while (ErlangParserDefinition.WS.contains(lexer.getTokenType()) ||
      ErlangParserDefinition.COMMENTS.contains(lexer.getTokenType())) {
      lexer.advance();
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
    private final Stack<BracePair> myBracePairsStack = ContainerUtil.newStack();
    private MacroCallParsingState myMacroCallParsingState = MacroCallParsingState.NONE;
    private MacroCallBuilder myMacroCallBuilder = new MacroCallBuilder();
    private MacroSubstitutionWorkerPosition myMacroNameEndPosition;

    public MacroSubstitutionWorker(Lexer baseLexer) {
      myBaseLexer = baseLexer;
      myLexersStack.push(baseLexer);
    }

    public void doWork() {
      while (!myLexersStack.isEmpty()) {
        int substitutionDepth = myLexersStack.size();
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
          else if (myMacroCallParsingState != MacroCallParsingState.QMARK) {
            resetMacroCallParsing();
          }
          if (lexer == myLexersStack.peek()) {
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
            String macroName = null;
            if (tokenType == ErlangTypes.ERL_VAR || tokenType == ErlangTypes.ERL_ATOM_NAME) {
              macroName = tokenText;
            }
            else if (tokenType == ErlangTypes.ERL_SINGLE_QUOTE) {
              // we will not switch to a different lexer while parsing these 3 tokens, so it's safe to advance the lexer.
              addMayBeForeignTokenFrom(lexer, substitutionDepth);
              if (lexer.getTokenType() != null && lexer.getTokenType() == ErlangTypes.ERL_ATOM_NAME) {
                String nameAfterQuote = lexer.getTokenText();
                addMayBeForeignTokenFrom(lexer, substitutionDepth);
                if (lexer.getTokenType() != null && lexer.getTokenType() == ErlangTypes.ERL_SINGLE_QUOTE) {
                  macroName = nameAfterQuote;
                  // the closing quote will be added in loop's end
                }
              }
            }

            if (macroName != null) {
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
              myBracePairsStack.push(getBracePairForLeftBrace(ErlangTypes.ERL_PAR_LEFT));
            }
            else { // no arguments specified: the macro call has a form ?MACRO
              processMacroCall();
            }
            break;
          }
          case ARGUMENTS_LIST: {
            int bracesStackSize = myBracePairsStack.size();
            IElementType rightBraceType = myBracePairsStack.peek().getRightBraceType();
            if (bracesStackSize == 1 && tokenType == ErlangTypes.ERL_COMMA) {
              myMacroCallBuilder.completeMacroArgument();
            }
            else {
              if (bracesStackSize == 1 && tokenType == rightBraceType) {
                myMacroCallBuilder.completeMacroArgument();
                processMacroCall();
              }
              else {
                //TODO handle ERL_DOT - stop trying to build a macro call and continue lexing.
                if (tokenType == rightBraceType) {
                  myBracePairsStack.pop();
                }
                else {
                  BracePair bracePair = getBracePairForLeftBrace(tokenType);
                  if (bracePair != null) {
                    myBracePairsStack.push(bracePair);
                  }
                }
                assert tokenType != null;
                myMacroCallBuilder.appendMacroArgument(tokenText, tokenType);
              }
            }
            break;
          }
        }
        addMayBeForeignTokenFrom(lexer, substitutionDepth);
      }
    }

    @Nullable
    private BracePair getBracePairForLeftBrace(@Nullable IElementType leftBraceType) {
      for (BracePair bracePair : BRACE_PAIRS) {
        if (leftBraceType == bracePair.getLeftBraceType()) {
          return bracePair;
        }
      }
      return null;
    }

    private void processMacroCall() {
      MacroCall macroCall = myMacroCallBuilder.build();
      if (macroCall == null) {
        //TODO report ill-formed macro call
        return;
      }

      String substitution = null;
      ErlangMacroDefinitionState macroDefinitionState = myMacroContext.getMacroDefinitionState(macroCall.getName());
      if (ErlangMacroDefinitionState.DEFINED == macroDefinitionState) {
        // this code handles calls like ?MACRO(arg1, arg2) where arguments may not be a MACRO's arguments
        ErlangMacro macro = null;
        List<String> macroArguments = macroCall.getPossibleArgumentsList();
        if (macroArguments != null) {
          macro = myMacroContext.getParameterizedMacro(macroCall.getName(), macroArguments.size());
        }
        if (macro == null) {
          macro = myMacroContext.getParameterlessMacro(macroCall.getName());
          if (macro != null) {
            macroArguments = null;
            myMacroNameEndPosition.restore();
          }
        }
        if (macro != null) {
          substitution = macro.substitute(macroArguments);
        }
      }

      // If no substitution was produced at this point, it's either a free macro or there was no macro definition with appropriate arity.
      // We can now try too look a definition up in compile context.
      if (substitution == null && (ErlangMacroDefinitionState.DEFINED == macroDefinitionState || ErlangMacroDefinitionState.FREE == macroDefinitionState)) {
        String macroBody = myCompileContext.macroDefinitions.get(macroCall.getName());
        if (macroBody != null) {
          myMacroNameEndPosition.restore();
          substitution = macroBody;
        }
      }

      if (substitution != null) {
        ErlangLexer substitutionLexer = new ErlangLexer();
        substitutionLexer.start(substitution);
        myLexersStack.push(substitutionLexer);
      }

      myMacroNameEndPosition = null;
      resetMacroCallParsing();
    }

    public void resetMacroCallParsing() {
      myMacroCallBuilder.reset();
      myBracePairsStack.clear();
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

    private void addMayBeForeignTokenFrom(Lexer lexer, int substitutionDepth) {
      IElementType tokenType = lexer.getTokenType();
      int tokenEndOffset = myBaseLexer.getTokenEnd();
      assert tokenType != null;
      if (myBaseLexer != lexer) {
        tokenEndOffset = myBaseLexer.getTokenStart();
        tokenType = new ErlangForeignLeafType(tokenType, lexer.getTokenText(), substitutionDepth);
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

  private enum MacroCallParsingState {
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

  private enum ConditionalBranchType {
    ACTIVE, INACTIVE, HAS_INACTIVE_PARENT, FREE
  }
}
