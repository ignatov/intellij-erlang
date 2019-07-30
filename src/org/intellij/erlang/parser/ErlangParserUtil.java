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

import com.intellij.lang.LighterASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.impl.source.resolve.FileContextUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.ArrayUtil;
import com.intellij.util.indexing.IndexingDataKeys;
import gnu.trove.TObjectLongHashMap;
import org.intellij.erlang.ErlangBraceMatcher;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangParserUtil extends GeneratedParserUtilBase {
  public static boolean isApplicationLanguage(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    PsiFile file = builder_.getUserData(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return file.getFileType() == ErlangFileType.APP;
  }

  public static boolean isConfigLanguage(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    PsiFile file = builder_.getUserData(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return file.getFileType() == ErlangFileType.TERMS;
  }

  public static boolean isConsole(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    PsiFile file = builder_.getUserData(FileContextUtil.CONTAINING_FILE_KEY);
    assert file != null;
    return isConsole(file);
  }

  public static boolean isConsole(@NotNull PsiFile file) {
    return file.getOriginalFile().getUserData(ErlangPsiImplUtil.ERLANG_CONSOLE) != null;
  }

  public static boolean isApplicationConfigFileType(@NotNull PsiFile file) {
    FileType fileType = file.getFileType();
    return fileType == ErlangFileType.APP || fileType == ErlangFileType.TERMS;
  }

  private static final Key<TObjectLongHashMap<String>> MODES_KEY = Key.create("MODES_KEY");

  private static TObjectLongHashMap<String> getParsingModes(PsiBuilder builder_) {
    TObjectLongHashMap<String> flags = builder_.getUserData(MODES_KEY);
    if (flags == null) builder_.putUserData(MODES_KEY, flags = new TObjectLongHashMap<>());
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
    ErrorState state = new ErrorState();
    ErrorState.initState(state, builder, root, ArrayUtil.mergeArrays(tokenSets, ErlangParser.EXTENDS_SETS_));
    PsiFile file = builder.getUserData(FileContextUtil.CONTAINING_FILE_KEY);
    VirtualFile data = file != null ? file.getUserData(IndexingDataKeys.VIRTUAL_FILE) : null;
    PsiBuilder result = new MyBuilder(builder, state, parser, data != null);
    ErrorState errorState = ErrorState.get(result);
    errorState.altMode = true;
    errorState.braces = ErlangBraceMatcher.PAIRS;
    return result;
  }

  private static class MyBuilder extends Builder {
    private final boolean indexing;

    private MyBuilder(PsiBuilder builder, ErrorState state, PsiParser parser, boolean indexing) {
      super(builder, state, parser);
      this.indexing = indexing;
    }
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean consumeToken(PsiBuilder builder, IElementType token) {
    boolean indexing = builder instanceof MyBuilder && ((MyBuilder)builder).indexing;
    if (!indexing) return GeneratedParserUtilBase.consumeToken(builder, token);

    if (nextTokenIsFast(builder, token)) {
      builder.advanceLexer();
      return true;
    }
    return false;
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean nextTokenIs(PsiBuilder builder, IElementType token) {
    boolean indexing = builder instanceof MyBuilder && ((MyBuilder)builder).indexing;
    return indexing ? nextTokenIsFast(builder, token) : GeneratedParserUtilBase.nextTokenIs(builder, token);
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean nextTokenIs(PsiBuilder builder, String frameName, IElementType... tokens) {
    boolean indexing = builder instanceof MyBuilder && ((MyBuilder)builder).indexing;
    return indexing ? nextTokenIsFast(builder, tokens) : GeneratedParserUtilBase.nextTokenIs(builder, frameName, tokens);
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static void addVariant(PsiBuilder builder, String text) {
    boolean indexing = builder instanceof MyBuilder && ((MyBuilder)builder).indexing;
    if (indexing) return;
    GeneratedParserUtilBase.addVariant(builder, text);
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

  public static boolean consumeMacroBody(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level_) {
    PsiBuilder.Marker m = builder_.mark();
    while (!builder_.eof()) {
      IElementType one = builder_.rawLookup(0);
      IElementType two = builder_.rawLookup(1);
      if (one == ErlangTypes.ERL_PAR_RIGHT && two == ErlangTypes.ERL_DOT) {
        m.drop();
        return true;
      }
      builder_.advanceLexer();
    }
    m.rollbackTo();
    return false;
  }

  public static boolean isInCompletion(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    return ErrorState.get(builder_).completionState != null;
  }

  private static final Key<Boolean> IS_COMPREHENSION_KEY = Key.create("Erlang.IS_COMPREHENSION");

  public static boolean markComprehension(PsiBuilder builder, @SuppressWarnings("UnusedParameters") int level) {
    IS_COMPREHENSION_KEY.set(builder, Boolean.TRUE);
    return true;
  }

  public static boolean maybeComprehension(PsiBuilder builder, int level, Parser parser) {
    Boolean previousIsComprehensionValue = IS_COMPREHENSION_KEY.get(builder);
    IS_COMPREHENSION_KEY.set(builder, null);
    boolean result = parser.parse(builder, level + 1);
    if (result && Boolean.TRUE.equals(IS_COMPREHENSION_KEY.get(builder))) {
      LighterASTNode latestDoneNode = builder.getLatestDoneMarker();
      if (latestDoneNode != null && latestDoneNode.getTokenType() == ErlangTypes.ERL_LIST_EXPRESSION) {
        PsiBuilder.Marker latestDoneMarker = (PsiBuilder.Marker) latestDoneNode;
        PsiBuilder.Marker newDoneMarker = latestDoneMarker.precede();
        latestDoneMarker.drop();
        newDoneMarker.done(ErlangTypes.ERL_LIST_COMPREHENSION);
      }
    }
    IS_COMPREHENSION_KEY.set(builder, previousIsComprehensionValue);
    return result;
  }
}