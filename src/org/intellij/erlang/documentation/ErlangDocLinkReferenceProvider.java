/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ObjectUtils;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * References on the targets of EDoc {@code @see Reference} tags and {@code {@link Reference}} macros:
 * {@code Module}, {@code Function/Arity}, {@code Module:Function/Arity} and the same with a {@code //App/} prefix.
 */
public class ErlangDocLinkReferenceProvider extends PsiReferenceProvider {
  private static final String ATOM = "[a-z][a-zA-Z0-9_@]*";
  private static final Pattern ATOM_PATTERN = Pattern.compile(ATOM);
  private static final Pattern REFERENCE = Pattern.compile(
    "(?:^%+[ \\t]*@see|\\{@link)[ \\t]+" +
    "(//" + ATOM + "/)?(?:(" + ATOM + ")[ \\t]*:[ \\t]*)?(" + ATOM + ")(?:[ \\t]*/[ \\t]*(\\d+))?" +
    "(?=[ \\t]*(?:\\.(?:[ \\t]|$)|}|$))");

  @Override
  public PsiReference @NotNull [] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
    if (!(element instanceof PsiComment comment)) return PsiReference.EMPTY_ARRAY;
    List<PsiReference> references = new ArrayList<>();
    Matcher reference = REFERENCE.matcher(comment.getText());
    while (reference.find()) {
      String moduleName = reference.group(2);
      String arity = reference.group(4);
      if (arity != null) {
        if (moduleName == null && reference.group(1) != null) continue;
        if (moduleName != null) {
          references.add(new ModuleReference(comment, range(reference, 2)));
        }
        references.add(new FunctionReference(comment, range(reference, 3), moduleName, StringUtil.parseInt(arity, -1)));
      }
      else if (moduleName == null) {
        references.add(new ModuleReference(comment, range(reference, 3)));
      }
    }
    return references.toArray(PsiReference.EMPTY_ARRAY);
  }

  @NotNull
  private static TextRange range(@NotNull Matcher matcher, int group) {
    return new TextRange(matcher.start(group), matcher.end(group));
  }

  private abstract static class DocLinkReference extends PsiReferenceBase<PsiComment> {
    private DocLinkReference(@NotNull PsiComment comment, @NotNull TextRange range) {
      super(comment, range, true);
    }

    @Override
    public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
      // a name that needs quotes would not be a link anymore, so keep the old one
      return ATOM_PATTERN.matcher(newElementName).matches() ? super.handleElementRename(newElementName) : myElement;
    }
  }

  private static final class ModuleReference extends DocLinkReference {
    private ModuleReference(@NotNull PsiComment comment, @NotNull TextRange range) {
      super(comment, range);
    }

    @Nullable
    @Override
    public PsiElement resolve() {
      Project project = myElement.getProject();
      return ContainerUtil.getFirstItem(ErlangModuleIndex.getModulesByName(project, getValue(), GlobalSearchScope.allScope(project)));
    }
  }

  private static final class FunctionReference extends DocLinkReference {
    @Nullable
    private final String myModuleName;
    private final int myArity;

    private FunctionReference(@NotNull PsiComment comment, @NotNull TextRange range, @Nullable String moduleName, int arity) {
      super(comment, range);
      myModuleName = moduleName;
      myArity = arity;
    }

    @Nullable
    @Override
    public PsiElement resolve() {
      if (myModuleName == null) {
        ErlangFile file = ObjectUtils.tryCast(myElement.getContainingFile(), ErlangFile.class);
        return file != null ? file.getFunction(getValue(), myArity) : null;
      }
      Project project = myElement.getProject();
      for (ErlangFile file : ErlangModuleIndex.getFilesByName(project, myModuleName, GlobalSearchScope.allScope(project))) {
        ErlangFunction function = file.getFunction(getValue(), myArity);
        if (function != null) return function;
      }
      return null;
    }
  }
}
