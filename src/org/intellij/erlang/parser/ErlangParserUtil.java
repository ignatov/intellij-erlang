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
import com.intellij.lang.SyntaxTreeBuilder;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.impl.source.resolve.FileContextUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.ArrayUtil;
import com.intellij.util.indexing.IndexingDataKeys;
import it.unimi.dsi.fastutil.objects.Object2LongOpenHashMap;
import org.intellij.erlang.ErlangBraceMatcher;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.Map;
import java.util.TreeMap;
import java.util.function.BiConsumer;

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

  private static final Key<Object2LongOpenHashMap<String>> MODES_KEY = Key.create("MODES_KEY");

  private static Object2LongOpenHashMap<String> getParsingModes(PsiBuilder builder_) {
    Object2LongOpenHashMap<String> flags = builder_.getUserData(MODES_KEY);
    if (flags == null) builder_.putUserData(MODES_KEY, flags = new Object2LongOpenHashMap<>());
    return flags;
  }

  public static boolean isModeOn(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    return getParsingModes(builder_).getLong(mode) > 0;
  }

  public static boolean withOn(PsiBuilder builder_, int level_, String mode, Parser parser) {
    return withImpl(builder_, level_, mode, true, parser, parser);
  }

  private static boolean withImpl(PsiBuilder builder_, int level_, String mode, boolean onOff, Parser whenOn, Parser whenOff) {
    Object2LongOpenHashMap<String> map = getParsingModes(builder_);
    long prev = map.getLong(mode);
    boolean change = ((prev & 1) == 0) == onOff;
    if (change) map.put(mode, prev << 1 | (onOff? 1 : 0));
    boolean result = (change ? whenOn : whenOff).parse(builder_, level_);
    if (change) map.put(mode, prev);
    return result;
  }

  public static boolean enterMode(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    Object2LongOpenHashMap<String> flags = getParsingModes(builder_);
    flags.addTo(mode, 1);
    return true;
  }

  public static boolean exitMode(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level, String mode) {
    Object2LongOpenHashMap<String> flags = getParsingModes(builder_);
    long count = flags.getLong(mode);
    if (count == 1) flags.removeLong(mode);
    else if (count > 1) flags.put(mode, count -1);
    else builder_.error("Could not exit inactive '" + mode + "' mode at offset " + builder_.getCurrentOffset());
    return true;
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static PsiBuilder adapt_builder_(IElementType root, PsiBuilder builder, PsiParser parser, TokenSet[] tokenSets) {
    ErrorState state = new ErrorState();
    ErrorState.initState(state, builder, root, ArrayUtil.mergeArrays(tokenSets, ErlangParser.EXTENDS_SETS_));
    FastParse fastParse = builder.getUserData(FAST_PARSE_KEY);
    BiConsumer<PsiFile, Boolean> listener = ourParseListener;
    if (listener != null) listener.accept(builder.getUserData(FileContextUtil.CONTAINING_FILE_KEY), fastParse != null);
    PsiBuilder result = new MyBuilder(builder, state, parser, fastParse);
    ErrorState errorState = ErrorState.get(result);
    errorState.altMode = true;
    errorState.braces = ErlangBraceMatcher.PAIRS;
    return result;
  }

  /**
   * State of a fast parse for the index. It records no expected-token variants, only the position of the furthest
   * token check of each frame ({@link Frame#lastVariantAt}), which is what the error recovery reads, and never less
   * than a full parse would record there. On valid code it builds the tree of a full parse. A pinned section that
   * fails marks an error, which stays unless an enclosing alternative is rolled back. When the tree has an error the
   * variants it lacks can change the tree, so {@link #parseLight} parses the file again in full.
   */
  private static final class FastParse {
    private final TreeMap<Integer, IElementType> remappedTokens = new TreeMap<>();
  }

  private static final Key<FastParse> FAST_PARSE_KEY = Key.create("Erlang.FAST_PARSE");

  private static volatile @Nullable BiConsumer<PsiFile, Boolean> ourParseListener;

  /**
   * Calls {@code listener} each time the generated parser starts, with the file being parsed and whether the parse
   * runs in the fast mode, until {@code parentDisposable} is disposed.
   */
  @TestOnly
  public static void setParseListener(@NotNull BiConsumer<PsiFile, Boolean> listener, @NotNull Disposable parentDisposable) {
    ourParseListener = listener;
    Disposer.register(parentDisposable, () -> ourParseListener = null);
  }

  public static void parseLight(IElementType root, PsiBuilder builder, BiConsumer<IElementType, PsiBuilder> parser) {
    PsiFile file = builder.getUserData(FileContextUtil.CONTAINING_FILE_KEY);
    VirtualFile data = file != null ? file.getUserData(IndexingDataKeys.VIRTUAL_FILE) : null;
    if (data == null) {
      parser.accept(root, builder);
      return;
    }
    FastParse fastParse = new FastParse();
    builder.putUserData(FAST_PARSE_KEY, fastParse);
    try {
      parser.accept(root, builder);
    }
    finally {
      builder.putUserData(FAST_PARSE_KEY, null);
    }
    if (!hasErrors(builder)) return;
    // the root marker is the first production; a marker made before the parse would move it past leading comments
    ((PsiBuilder.Marker)builder.getProductions().get(0)).precede().rollbackTo();
    restoreRemappedTokens(builder, fastParse);
    builder.putUserData(MODES_KEY, null);
    builder.putUserData(IS_COMPREHENSION_KEY, null);
    parser.accept(root, builder);
  }

  // a rollback keeps the token types that eofOrSpace remapped
  private static void restoreRemappedTokens(PsiBuilder builder, FastParse fastParse) {
    if (fastParse.remappedTokens.isEmpty()) return;
    PsiBuilder.Marker start = builder.mark();
    for (Map.Entry<Integer, IElementType> token : fastParse.remappedTokens.entrySet()) {
      while (!builder.eof() && builder.rawTokenIndex() < token.getKey()) builder.advanceLexer(); // eof() skips whitespaces
      if (builder.rawTokenIndex() == token.getKey()) builder.remapCurrentToken(token.getValue());
    }
    start.rollbackTo();
  }

  private static boolean hasErrors(PsiBuilder builder) {
    for (SyntaxTreeBuilder.Production production : builder.getProductions()) {
      if (production.getTokenType() == TokenType.ERROR_ELEMENT) return true;
    }
    return false;
  }

  private static class MyBuilder extends Builder {
    private final @Nullable FastParse fastParse;

    private MyBuilder(PsiBuilder builder, ErrorState state, PsiParser parser, @Nullable FastParse fastParse) {
      super(builder, state, parser);
      this.fastParse = fastParse;
    }
  }

  private static @Nullable FastParse fastParse(PsiBuilder builder) {
    return builder instanceof MyBuilder ? ((MyBuilder)builder).fastParse : null;
  }

  private static void markTokenCheck(PsiBuilder builder) {
    builder.eof(); // skip whitespaces
    Frame frame = ErrorState.get(builder).currentFrame;
    int position = builder.rawTokenIndex();
    if (frame != null && frame.lastVariantAt < position) frame.lastVariantAt = position;
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean consumeToken(PsiBuilder builder, IElementType token) {
    if (fastParse(builder) == null) return GeneratedParserUtilBase.consumeToken(builder, token);

    markTokenCheck(builder);
    if (nextTokenIsFast(builder, token)) {
      builder.advanceLexer();
      return true;
    }
    return false;
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean nextTokenIs(PsiBuilder builder, IElementType token) {
    if (fastParse(builder) == null) return GeneratedParserUtilBase.nextTokenIs(builder, token);
    markTokenCheck(builder);
    return nextTokenIsFast(builder, token);
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static boolean nextTokenIs(PsiBuilder builder, String frameName, IElementType... tokens) {
    if (fastParse(builder) == null) return GeneratedParserUtilBase.nextTokenIs(builder, frameName, tokens);
    markTokenCheck(builder);
    return nextTokenIsFast(builder, tokens);
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static void addVariant(PsiBuilder builder, String text) {
    if (fastParse(builder) == null) {
      GeneratedParserUtilBase.addVariant(builder, text);
      return;
    }
    markTokenCheck(builder);
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static void exit_section_(PsiBuilder builder, int level, PsiBuilder.Marker marker,
                                   boolean result, boolean pinned, @Nullable Parser eatMore) {
    exit_section_(builder, level, marker, null, result, pinned, eatMore);
  }

  @SuppressWarnings("MethodOverridesStaticMethodOfSuperclass")
  public static void exit_section_(PsiBuilder builder, int level, PsiBuilder.Marker marker, @Nullable IElementType elementType,
                                   boolean result, boolean pinned, @Nullable Parser eatMore) {
    FastParse fastParse = fastParse(builder);
    Frame frame = ErrorState.get(builder).currentFrame;
    if (fastParse != null && frame != null) {
      // a full parse reports a syntax error here from its variants, unless an enclosing alternative is rolled back
      if (!result && pinned) builder.error("fast parse error");
      // a full parse sets this for a predicate that consumed tokens and recorded no variants
      int position = builder.rawTokenIndex();
      if ((frame.modifiers & (_AND_ | _NOT_)) != 0 && frame.position < position && frame.lastVariantAt < position) {
        frame.lastVariantAt = position;
      }
    }
    GeneratedParserUtilBase.exit_section_(builder, level, marker, elementType, result, pinned, eatMore);
  }

  // a '.' that starts a form, after another '.' or at the start of the file, is a syntax error for erlc
  public static boolean strayPeriodOr(PsiBuilder builder_, int level_, Parser form) {
    if (!isStrayPeriod(builder_)) return form.parse(builder_, level_);
    PsiBuilder.Marker marker = builder_.mark();
    builder_.advanceLexer();
    marker.error("Unexpected '.'");
    return true;
  }

  private static boolean isStrayPeriod(PsiBuilder builder_) {
    if (builder_.getTokenType() != ErlangTypes.ERL_DOT) return false;
    for (int i = -1; ; i--) {
      IElementType type = builder_.rawLookup(i);
      if (type == null) return true;
      if (type != TokenType.WHITE_SPACE && !ErlangParserDefinition.COMMENTS.contains(type)) return type == ErlangTypes.ERL_DOT;
    }
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean eofOrSpace(PsiBuilder builder_, int level_) {
    if (builder_.eof()) return true;
    IElementType one = builder_.rawLookup(1);
    IElementType two = builder_.rawLookup(2);
    if (one == TokenType.WHITE_SPACE && (two == ErlangTypes.ERL_DOT || two == null) || one == null && builder_.getTokenType() == ErlangTypes.ERL_DOT) {
      FastParse fastParse = fastParse(builder_);
      if (fastParse != null) fastParse.remappedTokens.putIfAbsent(builder_.rawTokenIndex(), builder_.getTokenType());
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

  /**
   * Parses with the given parser, but fails and rolls back if the parser reported a syntax error,
   * so that the next alternative gets a chance (an incomplete typed field in a macro body, #933).
   */
  public static boolean withoutErrors(PsiBuilder builder_, int level_, Parser parser) {
    Frame frame = ErrorState.get(builder_).currentFrame;
    if (frame == null) return parser.parse(builder_, level_ + 1);
    int errorReportedAt = frame.errorReportedAt;
    frame.errorReportedAt = -1;
    PsiBuilder.Marker m = builder_.mark();
    boolean result = parser.parse(builder_, level_ + 1);
    boolean errorReported = frame.errorReportedAt != -1;
    frame.errorReportedAt = errorReportedAt;
    if (result && !errorReported) {
      m.drop();
      return true;
    }
    m.rollbackTo();
    return false;
  }

  /**
   * Tells whether the parser is inside a macro body or the arguments of a macro call. There a token sequence such as
   * {@code ()} need not be an expression on its own: erlc parses it only after the expansion (#1647).
   */
  public static boolean isMacroFragment(PsiBuilder builder, @SuppressWarnings("UnusedParameters") int level) {
    for (Frame frame = ErrorState.get(builder).currentFrame; frame != null; frame = frame.parentFrame) {
      if (frame.elementType == ErlangTypes.ERL_MACROS_BODY) return true;
      if (frame.elementType == ErlangTypes.ERL_ARGUMENT_LIST && followsMacroName(builder, frame.position)) return true;
    }
    return false;
  }

  private static boolean followsMacroName(PsiBuilder builder, int position) {
    int i = skipBack(builder, position - 1);
    if (rawTokenAt(builder, i) == ErlangTypes.ERL_SINGLE_QUOTE) i = skipBack(builder, i - 1);
    IElementType name = rawTokenAt(builder, i);
    if (name != ErlangTypes.ERL_ATOM_NAME && name != ErlangTypes.ERL_VAR) return false;
    i = skipBack(builder, i - 1);
    if (rawTokenAt(builder, i) == ErlangTypes.ERL_SINGLE_QUOTE) i = skipBack(builder, i - 1);
    return rawTokenAt(builder, i) == ErlangTypes.ERL_QMARK;
  }

  private static int skipBack(PsiBuilder builder, int index) {
    while (index >= 0) {
      IElementType type = rawTokenAt(builder, index);
      if (type != TokenType.WHITE_SPACE && !ErlangParserDefinition.COMMENTS.contains(type)) break;
      index--;
    }
    return index;
  }

  private static @Nullable IElementType rawTokenAt(PsiBuilder builder, int index) {
    return index < 0 ? null : builder.rawLookup(index - builder.rawTokenIndex());
  }

  public static boolean isInCompletion(PsiBuilder builder_, @SuppressWarnings("UnusedParameters") int level) {
    return ErrorState.get(builder_).completionState != null;
  }

  // Tells whether the binary that starts before the current token has '||' at its top level before its '>>',
  // so that binary_comprehension does not parse the first element of every binary speculatively.
  public static boolean isBinaryComprehension(PsiBuilder builder, @SuppressWarnings("UnusedParameters") int level) {
    PsiBuilder.Marker m = builder.mark();
    try {
      int depth = 0;
      for (IElementType type = builder.getTokenType(); type != null; builder.advanceLexer(), type = builder.getTokenType()) {
        if (type == ErlangTypes.ERL_BIN_START || type == ErlangTypes.ERL_PAR_LEFT ||
            type == ErlangTypes.ERL_BRACKET_LEFT || type == ErlangTypes.ERL_CURLY_LEFT) {
          depth++;
        }
        else if (type == ErlangTypes.ERL_BIN_END || type == ErlangTypes.ERL_PAR_RIGHT ||
                 type == ErlangTypes.ERL_BRACKET_RIGHT || type == ErlangTypes.ERL_CURLY_RIGHT) {
          if (depth == 0) return false;
          depth--;
        }
        else if (type == ErlangTypes.ERL_OR_OR && depth == 0) {
          return true;
        }
      }
      return false;
    }
    finally {
      m.rollbackTo();
    }
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