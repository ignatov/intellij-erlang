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

package org.intellij.erlang.documentation;

import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

final class ErlangMacroDocProvider implements ElementDocProvider {
  @NotNull
  private final ErlangMacrosDefinition myMacrosDefinition;
  @Nullable
  private final PsiElement myContext;

  public ErlangMacroDocProvider(@NotNull ErlangMacrosDefinition macrosDefinition, @Nullable PsiElement context) {
    myMacrosDefinition = macrosDefinition;
    myContext = context;
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @NotNull
  @Override
  public String getDocText() {
    StringBuilder result = new StringBuilder();
    for (ErlangMacrosDefinition definition : getDefinitionsForUse()) {
      result.append(ErlangDocUtil.wrapInPreTag(getDocText(definition)));
    }
    return result.toString();
  }

  @NotNull
  private static String getDocText(@NotNull ErlangMacrosDefinition definition) {
    String text = StringUtil.escapeXmlEntities(definition.getText());
    List<PsiComment> comments = collectLeadingComments(definition);
    if (!comments.isEmpty()) {
      List<String> lines = ContainerUtil.map(comments, c -> StringUtil.escapeXmlEntities(c.getText().replaceFirst("^%+", "")));
      text += "<br/><br/><b>Comment:</b><br/>" + StringUtil.join(lines, "<br/>");
    }
    PsiFile file = definition.getContainingFile();
    if (file != null) {
      text += "<br/><br/><b>Defined in:</b> " + StringUtil.escapeXmlEntities(file.getName());
    }
    return text;
  }

  /**
   * The resolved definition is the first one with the macro's name. When the macro is documented at a use of that name,
   * pick the definitions that the preprocessor can expand there: walk the forms before the use in order, entering
   * the included files where they are included, and follow {@code -define}, {@code -undef} and the conditionals on
   * the macro's name. A definition under a condition that cannot be decided here is one of several alternatives.
   */
  @NotNull
  private List<ErlangMacrosDefinition> getDefinitionsForUse() {
    ErlangMacros use = myContext != null ? PsiTreeUtil.getParentOfType(myContext, ErlangMacros.class, false) : null;
    ErlangMacrosName useName = use != null ? use.getMacrosName() : null;
    String name = myMacrosDefinition.getName();
    if (useName == null || !name.equals(ErlangPsiImplUtil.getNameIdentifier(useName).getText()) ||
        !(use.getContainingFile() instanceof ErlangFile useFile)) {
      return List.of(myMacrosDefinition);
    }
    boolean inMacroBody = PsiTreeUtil.getParentOfType(use, ErlangMacrosDefinition.class) != null;
    MacroWalker walker = new MacroWalker(name, inMacroBody ? -1 : use.getTextRange().getStartOffset(), useFile);
    walker.walk(useFile);
    int useArity = getUseArity(use);
    Map<Integer, Set<ErlangMacrosDefinition>> definitions = walker.myDefinitions;
    Set<ErlangMacrosDefinition> result = definitions.getOrDefault(useArity, Set.of());
    if (result.isEmpty()) result = definitions.getOrDefault(NO_ARGUMENTS, Set.of());
    return result.isEmpty() ? List.of(myMacrosDefinition) : new ArrayList<>(result);
  }

  private static final int NO_ARGUMENTS = -1;

  /**
   * The number of arguments after the use, counted on tokens as the preprocessor does.
   */
  private static int getUseArity(@NotNull ErlangMacros use) {
    PsiElement token = nextToken(PsiTreeUtil.getDeepestLast(use));
    if (token == null || !"(".equals(token.getText())) return NO_ARGUMENTS;
    token = nextToken(token);
    if (token != null && ")".equals(token.getText())) return 0;
    int depth = 0;
    int arity = 1;
    for (; token != null; token = nextToken(token)) {
      String text = token.getText();
      switch (text) {
        case "(", "[", "{", "<<", "begin", "case", "if", "receive", "try" -> depth++;
        case "maybe" -> {
          if (token.getNode().getElementType() == ErlangTypes.ERL_MAYBE) depth++;
        }
        case "fun" -> {
          PsiElement next = nextToken(token);
          if (next != null && "(".equals(next.getText())) depth++;
        }
        case "]", "}", ">>", "end" -> depth--;
        case ")" -> {
          if (depth == 0) return arity;
          depth--;
        }
        case "," -> {
          if (depth == 0) arity++;
        }
        case "." -> {
          if (depth == 0) return NO_ARGUMENTS;
        }
        default -> {
        }
      }
    }
    return NO_ARGUMENTS;
  }

  @Nullable
  private static PsiElement nextToken(@NotNull PsiElement leaf) {
    PsiElement next = PsiTreeUtil.nextLeaf(leaf);
    while (next != null && (next instanceof PsiWhiteSpace || next instanceof PsiComment || next.getTextLength() == 0)) {
      next = PsiTreeUtil.nextLeaf(next);
    }
    return next;
  }

  private static int getArity(@NotNull ErlangMacrosDefinition definition) {
    ErlangArgumentDefinitionList arguments = definition.getArgumentDefinitionList();
    return arguments != null ? arguments.getArgumentDefinitionList().size() : NO_ARGUMENTS;
  }

  /**
   * Follows the definitions of one macro name through the forms, as the preprocessor does. A condition is
   * {@code TRUE}, {@code FALSE} or {@code null} when it cannot be decided (a condition on another name or an expression).
   */
  private static final class MacroWalker {
    private final String myName;
    private final int myStopOffset;
    private final ErlangFile myUseFile;
    private final Set<ErlangFile> myVisiting = new HashSet<>();
    private final Deque<Boolean[]> myConditions = new ArrayDeque<>();
    /** The possible definitions per arity, and the arities that are surely defined by one of them. */
    private final Map<Integer, Set<ErlangMacrosDefinition>> myDefinitions = new HashMap<>();
    private final Set<Integer> mySurelyDefined = new HashSet<>();
    private boolean myStopped;

    private MacroWalker(@NotNull String name, int stopOffset, @NotNull ErlangFile useFile) {
      myName = name;
      myStopOffset = stopOffset;
      myUseFile = useFile;
      myVisiting.add(useFile);
    }

    private void walk(@NotNull ErlangFile file) {
      for (PsiElement child = file.getFirstChild(); child != null && !myStopped; child = child.getNextSibling()) {
        ProgressManager.checkCanceled();
        if (file == myUseFile && myStopOffset >= 0 && child.getTextRange().getEndOffset() > myStopOffset) {
          myStopped = true;
        }
        else if (child instanceof ErlangMacrosDefinition definition) {
          if (myName.equals(definition.getName())) define(definition);
        }
        else if (child instanceof ErlangInclude || child instanceof ErlangIncludeLib) {
          include(child, file);
        }
        else {
          directive(child);
        }
      }
    }

    private void define(@NotNull ErlangMacrosDefinition definition) {
      Boolean active = isActive();
      if (active == Boolean.FALSE) return;
      int arity = getArity(definition);
      Set<ErlangMacrosDefinition> definitions = myDefinitions.computeIfAbsent(arity, k -> new LinkedHashSet<>());
      if (active == Boolean.TRUE || myStopOffset < 0) {
        if (myStopOffset >= 0) definitions.clear();
        mySurelyDefined.add(arity);
      }
      definitions.add(definition);
    }

    private void include(@NotNull PsiElement include, @NotNull ErlangFile file) {
      Boolean active = isActive();
      if (active == Boolean.FALSE) return;
      List<ErlangFile> files = include instanceof ErlangInclude i
                               ? ErlangPsiImplUtil.getDirectlyIncludedFiles(i, file)
                               : ErlangPsiImplUtil.getDirectlyIncludedFiles((ErlangIncludeLib)include, file);
      for (ErlangFile included : files) {
        if (!myVisiting.add(included)) continue;
        myConditions.push(new Boolean[]{active, active});
        walk(included);
        myConditions.pop();
        myVisiting.remove(included);
        if (myStopped) return;
      }
    }

    private void directive(@NotNull PsiElement form) {
      PsiElement minus = PsiTreeUtil.getDeepestFirst(form);
      PsiElement directive = "-".equals(minus.getText()) ? nextToken(minus) : null;
      if (directive == null) return;
      ErlangMacrosName macrosName = PsiTreeUtil.findChildOfType(form, ErlangMacrosName.class);
      boolean ours = macrosName != null && myName.equals(ErlangPsiImplUtil.getNameIdentifier(macrosName).getText());
      switch (directive.getText()) {
        case "ifdef" -> pushCondition(ours ? isDefined() : null);
        case "ifndef" -> pushCondition(ours ? not(isDefined()) : null);
        case "if" -> pushCondition(null);
        case "elif" -> {
          Boolean[] condition = myConditions.peek();
          if (condition != null) {
            condition[0] = condition[1] == Boolean.TRUE ? Boolean.FALSE : null;
            condition[1] = condition[1] == Boolean.TRUE ? Boolean.TRUE : null;
          }
        }
        case "else" -> {
          Boolean[] condition = myConditions.peek();
          if (condition != null) {
            condition[0] = not(condition[1]);
            condition[1] = Boolean.TRUE;
          }
        }
        case "endif" -> {
          if (!myConditions.isEmpty()) myConditions.pop();
        }
        case "undef" -> {
          Boolean active = isActive();
          if (!ours || active == Boolean.FALSE || myStopOffset < 0) return;
          if (active == Boolean.TRUE) myDefinitions.clear();
          mySurelyDefined.clear();
        }
        default -> {
        }
      }
    }

    private void pushCondition(@Nullable Boolean value) {
      myConditions.push(new Boolean[]{value, value});
    }

    @Nullable
    private Boolean isActive() {
      Boolean result = Boolean.TRUE;
      for (Boolean[] condition : myConditions) {
        if (condition[0] == Boolean.FALSE) return Boolean.FALSE;
        if (condition[0] == null) result = null;
      }
      return result;
    }

    @Nullable
    private Boolean isDefined() {
      if (!mySurelyDefined.isEmpty()) return Boolean.TRUE;
      return ContainerUtil.all(myDefinitions.values(), Set::isEmpty) ? Boolean.FALSE : null;
    }

    @Nullable
    private static Boolean not(@Nullable Boolean value) {
      return value == null ? null : !value;
    }
  }

  /**
   * The {@code %%} comments on the lines directly above the definition, without blank lines in between.
   */
  @NotNull
  private static List<PsiComment> collectLeadingComments(@NotNull PsiElement element) {
    List<PsiComment> result = new ArrayList<>();
    PsiElement current = element;
    while (current.getPrevSibling() instanceof PsiWhiteSpace whiteSpace &&
           StringUtil.countNewLines(whiteSpace.getText()) == 1 &&
           whiteSpace.getPrevSibling() instanceof PsiComment comment &&
           comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT &&
           startsLine(comment)) {
      result.add(comment);
      current = comment;
    }
    return ContainerUtil.reverse(result);
  }

  private static boolean startsLine(@NotNull PsiElement comment) {
    PsiElement before = comment.getPrevSibling();
    return before == null || before instanceof PsiWhiteSpace && before.textContains('\n');
  }
}
