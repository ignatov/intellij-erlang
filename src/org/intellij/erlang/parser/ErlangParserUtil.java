/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.impl.source.resolve.FileContextUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import gnu.trove.TObjectLongHashMap;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.lexer.ErlangForeignLeafType;
import org.intellij.erlang.lexer.ErlangInterimTokenTypes;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangParserUtil extends GeneratedParserUtilBase {
  public static boolean isApplicationLanguage(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    PsiFile file = builder_.getUserDataUnprotected(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return isApplicationConfigFileType(file);
  }

  public static boolean isConsole(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    PsiFile file = builder_.getUserDataUnprotected(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return isConsole(file);
  }

  public static boolean isConsole(@NotNull PsiFile file) {
    return file.getOriginalFile().getUserData(ErlangPsiImplUtil.ERLANG_CONSOLE) != null;
  }

  public static boolean isApplicationConfigFileType(@NotNull PsiFile file) {
    FileType fileType = file.getViewProvider().getVirtualFile().getFileType();
    return fileType == ErlangFileType.APP || fileType == ErlangFileType.TERMS ||
      ApplicationManager.getApplication().isUnitTestMode() && (fileType.getDefaultExtension().equals("app") || fileType.getDefaultExtension().equals("config"));
  }

  private static final Key<TObjectLongHashMap<String>> MODES_KEY = Key.create("MODES_KEY");

  private static TObjectLongHashMap<String> getParsingModes(PsiBuilder builder_) {
    TObjectLongHashMap<String> flags = builder_.getUserDataUnprotected(MODES_KEY);
    if (flags == null) builder_.putUserDataUnprotected(MODES_KEY, flags = new TObjectLongHashMap<String>());
    return flags;
  }

  public static boolean isModeOn(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    return getParsingModes(builder_).get(mode) > 0;
  }

  public static boolean isModeOff(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    return getParsingModes(builder_).get(mode) == 0;
  }

  public static boolean withOn(PsiBuilder builder_, int level_, String mode, Parser parser) {
    return withImpl(builder_, level_, mode, true, parser, parser);
  }

  public static boolean withCleared(PsiBuilder builder_, int level_, String mode, Parser whenOn, Parser whenOff) {
    return withImpl(builder_, level_, mode, false, whenOn, whenOff);
  }

  private static boolean withImpl(PsiBuilder builder_, int level_, String mode, boolean onOff, Parser whenOn, Parser whenOff) {
    TObjectLongHashMap<String> map = getParsingModes(builder_);
    long prev = map.get(mode);
    boolean change = ((prev & 1) == 0) == onOff;
    if (change) map.put(mode, prev << 1 | (onOff? 1 : 0));
    boolean result = (change ? whenOn : whenOff).parse(builder_, level_);
    if (change) map.put(mode, prev);
    return result;
  }

  public static boolean enterMode(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    TObjectLongHashMap<String> flags = getParsingModes(builder_);
    if (!flags.increment(mode)) flags.put(mode, 1);
    return true;
  }

  public static boolean exitMode(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    TObjectLongHashMap<String> flags = getParsingModes(builder_);
    long count = flags.get(mode);
    if (count == 1) flags.remove(mode);
    else if (count > 1) flags.put(mode, count -1);
    else builder_.error("Could not exit inactive '" + mode + "' mode at offset " + builder_.getCurrentOffset());
    return true;
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static PsiBuilder adapt_builder_(IElementType root, PsiBuilder builder, PsiParser parser, TokenSet[] tokenSets) {
    //TODO copy implementation from GPUB, use ErrorState.initState to init state (available since 133.162)
    PsiBuilder result = GeneratedParserUtilBase.adapt_builder_(root, builder, parser, tokenSets);
    ErrorState errorState = ErrorState.get(result);
    return new Builder(builder, errorState, parser);
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean eofOrSpace(PsiBuilder builder_, int level_) {
    if (builder_.eof()) return true;
    IElementType one = builder_.rawLookup(1);
    IElementType two = builder_.rawLookup(2);
    if (one == TokenType.WHITE_SPACE && (two == ErlangTypes.ERL_DOT || two == null) || one == null && builder_.getTokenType() == ErlangTypes.ERL_DOT) {
      builder_.remapCurrentToken(TokenType.ERROR_ELEMENT);
      return true;
    }
    return false;
  }

  public static boolean consumeMacroBody(PsiBuilder builder_, int level_) {
    if (builder_.getTokenType() != ErlangInterimTokenTypes.ERL_MACRO_BODY_BEGIN) return false;
    PsiBuilder.Marker beforeMacroBody = builder_.mark();
    builder_.advanceLexer();
    while (builder_.getTokenType() != ErlangInterimTokenTypes.ERL_MACRO_BODY_END
      && builder_.getTokenType() != null) {
      if (!consumeToken(builder_, builder_.getTokenType())) {
        beforeMacroBody.rollbackTo();
        return false;
      }
    }
    beforeMacroBody.drop();
    builder_.advanceLexer();
    return true;
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean recursion_guard_(PsiBuilder builder_, int level_, String funcName_) {
    if (!GeneratedParserUtilBase.recursion_guard_(builder_, level_, funcName_)) {
      return false;
    }
    if (!funcName_.equals("macros") && !funcName_.startsWith("macros_call") && nextTokenIsFast(builder_, ErlangTypes.ERL_QMARK)) {
      PsiBuilder.Marker beforeMacroCallParsed = builder_.mark();
      boolean macroCallParsed = ErlangParser.macros(builder_, level_);
      if (!macroCallParsed || !(((Builder)builder_).getWrappedTokenType() instanceof ErlangForeignLeafType)) {
        beforeMacroCallParsed.rollbackTo();
      }
      else {
        beforeMacroCallParsed.drop();
      }
    }
    return true;
  }

  @SuppressWarnings("ClassNameSameAsAncestorName")
  public static class Builder extends GeneratedParserUtilBase.Builder {
    public Builder(PsiBuilder builder, GeneratedParserUtilBase.ErrorState state, PsiParser parser) {
      super(builder, state, parser);
    }

    @Nullable
    @Override
    public IElementType getTokenType() {
      IElementType tokenType = super.getTokenType();
      while (tokenType instanceof ErlangForeignLeafType) {
        tokenType = ((ErlangForeignLeafType) tokenType).getDelegate();
      }
      return tokenType;
    }

    @Nullable
    public IElementType getWrappedTokenType() {
      return super.getTokenType();
    }
  }
}