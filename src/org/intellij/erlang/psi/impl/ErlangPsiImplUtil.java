package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.completion.BasicInsertHandler;
import com.intellij.codeInsight.completion.InsertionContext;
import com.intellij.codeInsight.completion.util.ParenthesesInsertHandler;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.PsiManagerEx;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.PathUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.*;

public class ErlangPsiImplUtil {
  private ErlangPsiImplUtil() {
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangQVar o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processor.execute(o, state);
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangQVar o) {
    if (inDefinition(o)) return null;
    return new ErlangVariableReferenceImpl(o, TextRange.from(0, o.getTextLength()));
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangFunctionCallExpression o) {
    PsiElement parent = o.getParent();
    ErlangModuleRef moduleReference = null;
    if (parent instanceof ErlangGlobalFunctionCallExpression) {
      moduleReference = ((ErlangGlobalFunctionCallExpression) parent).getModuleRef();
    }
    ErlangQAtom moduleAtom = moduleReference == null ? null : moduleReference.getQAtom();
    ErlangQAtom nameAtom = o.getQAtom();

    return new ErlangFunctionReferenceImpl<ErlangQAtom>(
      nameAtom, moduleAtom, TextRange.from(0, nameAtom.getTextLength()),
      nameAtom.getText(), o.getArgumentList().getExpressionList().size());
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangExportFunction o) {
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl<ErlangQAtom>(o.getQAtom(), null, TextRange.from(0, o.getQAtom().getTextLength()),
      o.getQAtom().getText(), StringUtil.parseInt(arity == null ? "" : arity.getText(), -1));
  }

  public static boolean inDefinition(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class) != null;
  }

  public static boolean inAtomAttribute(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangAtomAttribute.class) != null;
  }

  public static boolean inSpecification(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangSpecification.class) != null;
  }

  public static boolean isLeftPartOfAssignment(@NotNull PsiElement psiElement) {
    ErlangAssignmentExpression assignmentExpression = PsiTreeUtil.getParentOfType(psiElement, ErlangAssignmentExpression.class);
    if (assignmentExpression == null) return false;
    return PsiTreeUtil.isAncestor(assignmentExpression.getLeft(), psiElement, false);
  }

  public static boolean isMacros(ErlangQVar o) {
    return o.getName().startsWith("?");
  }

  public static boolean isForceSkipped(ErlangQVar o) {
    return o.getName().startsWith("_");
  }

  @NotNull
  static List<LookupElement> getFunctionLookupElements(@NotNull PsiFile containingFile, final boolean withArity) {
    if (containingFile instanceof ErlangFile) {
      return ContainerUtil.map(((ErlangFile) containingFile).getFunctions(), new Function<ErlangFunction, LookupElement>() {
        @Override
        public LookupElement fun(@NotNull final ErlangFunction function) {
          return LookupElementBuilder.create(function)
            .setIcon(ErlangIcons.FUNCTION).setTailText("/" + function.getArity()).
              setInsertHandler(
                withArity ?
                  new BasicInsertHandler<LookupElement>() {
                    @Override
                    public void handleInsert(InsertionContext context, LookupElement item) {
                      final Editor editor = context.getEditor();
                      final Document document = editor.getDocument();
                      context.commitDocument();
                      document.insertString(context.getTailOffset(), "/" + function.getArity());
                      editor.getCaretModel().moveToOffset(context.getTailOffset());
                    }
                  } :
                  new ParenthesesInsertHandler<LookupElement>() {
                    @Override
                    protected boolean placeCaretInsideParentheses(InsertionContext context, LookupElement item) {
                      return function.getArity() > 0;
                    }
                  }
              );
        }
      });
    }
    return Collections.emptyList();
  }

  @NotNull
  public static List<LookupElement> getRecordLookupElements(@NotNull PsiFile containingFile) {
    if (containingFile instanceof ErlangFile) {
      List<ErlangRecordDefinition> concat = ContainerUtil.concat(((ErlangFile) containingFile).getRecords(), getErlangRecordFromIncludes(containingFile, true, ""));
      return ContainerUtil.map(
        concat,
        new Function<ErlangRecordDefinition, LookupElement>() {
          @Override
          public LookupElement fun(@NotNull ErlangRecordDefinition rd) {
            return LookupElementBuilder.create(rd).setIcon(ErlangIcons.RECORD);
          }
        });
    }
    return Collections.emptyList();
  }

  @NotNull
  public static String getName(@NotNull ErlangFunction o) {
    return o.getAtomName().getAtom().getText();
  }

  @NotNull
  public static String getName(@NotNull ErlangQVar o) {
    return o.getText();
  }

  public static int getArity(@NotNull ErlangFunction o) {
    return o.getFunctionClauseList().get(0).getArgumentDefinitionList().size();
  }

  @NotNull
  public static String getName(@NotNull ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    if (atom == null) return "";
    return atom.getText();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    return atom != null ? atom : o;
  }

  public static int getTextOffset(@NotNull ErlangRecordDefinition o) {
    if (o.getNameIdentifier() == o) return 0;//o.getNode().getTextOffset();
    return o.getNameIdentifier().getTextOffset();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangQVar o) {
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangFunction o) {
    return o.getAtomName();
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangRecordExpression o) { // todo: hack?
    ErlangQAtom atom = o.getAtomName();
    if (atom == null) return null;
    atom.setReference(new ErlangRecordReferenceImpl<ErlangQAtom>(atom, TextRange.from(0, atom.getTextLength()), atom.getText()));
    return null;
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangModuleRef o) {
    ErlangQAtom atom = o.getQAtom();
    return new ErlangModuleReferenceImpl<ErlangQAtom>(atom,
      TextRange.from(0, atom.getTextLength()), atom.getText() + ".erl");
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangFunction o, @NotNull String newName) {
    for (ErlangFunctionClause clause : o.getFunctionClauseList()) {
      clause.getQAtom().getAtom().replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
    }
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangQVar o, @NotNull String newName) {
    o.replace(ErlangElementFactory.createQVarFromText(o.getProject(), newName));
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangRecordDefinition o, @NotNull String newName) {
    ErlangQAtom atom = o.getQAtom();
    if (atom != null) {
      atom.getAtom().replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
    }
    return o;
  }

  @NotNull
  public static String getName(@NotNull ErlangModule o) {
    ErlangQAtom atom = o.getQAtom();
    return atom == null ? "" : atom.getText();
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangModule o, String newName) {
    VirtualFile virtualFile = o.getContainingFile().getVirtualFile();
    if (virtualFile != null) {
      try {
        String ext = FileUtil.getExtension(virtualFile.getName());
        virtualFile.rename(o, newName + "." + ext);

        ErlangQAtom atom = o.getQAtom();
        if (atom != null) {
          atom.getAtom().replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
        }
      } catch (IOException ignored) {
      }
    }
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangModule o) {
    ErlangQAtom atom = o.getQAtom();
    return atom == null ? o : atom;
  }

  public static int getTextOffset(@NotNull ErlangModule o) {
    return o.getNameIdentifier().getTextOffset();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangFunctionCallExpression o) {
    return o.getQAtom();
  }

  public static int getTextOffset(@NotNull ErlangFunctionCallExpression o) {
    return o.getQAtom().getTextOffset();
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangListComprehension o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processDeclarationRecursive(o, processor, state);
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangModule o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processDeclarationRecursive(o, processor, state);
  }

  private static boolean processDeclarationRecursive(ErlangCompositeElement o, PsiScopeProcessor processor, ResolveState state) {
    Queue<ErlangCompositeElement> queue = new LinkedList<ErlangCompositeElement>();
    queue.add(o);
    while (!queue.isEmpty()) {
      ErlangCompositeElement top = queue.remove();
      if (!processor.execute(top, state)) return false;
      queue.addAll(PsiTreeUtil.getChildrenOfTypeAsList(top, ErlangCompositeElement.class));
    }
    return true;
  }

  @Nullable
  public static ErlangModule getModule(PsiFile file) {
    if (file instanceof ErlangFile) {
      List<ErlangAttribute> attributes = PsiTreeUtil.getChildrenOfTypeAsList(file, ErlangAttribute.class);
      for (ErlangAttribute attribute : attributes) {
        ErlangModule module = attribute.getModule();
        if (module != null) {
          return module;
        }
      }
    }
    return null;
  }

  static boolean isInModule(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangModule.class) != null;
  }

  @NotNull
  static List<ErlangRecordDefinition> getErlangRecordFromIncludes(@NotNull PsiFile containingFile, boolean forCompletion, String name) {
    List<ErlangInclude> includes = ((ErlangFile) containingFile).getIncludes();

    List<ErlangRecordDefinition> fromIncludes = new ArrayList<ErlangRecordDefinition>();
    for (ErlangInclude include : includes) {
      PsiElement string = include.getString();
      if (string != null) {
        String includeFilePath = string.getText().replaceAll("\"", "");
        fromIncludes.addAll(justAppend(containingFile, includeFilePath, forCompletion, name));
        fromIncludes.addAll(findByWildCard(containingFile, includeFilePath, forCompletion, name));
      }
    }
    return fromIncludes;
  }

  @NotNull
  private static Collection<ErlangRecordDefinition> findByWildCard(@NotNull PsiFile containingFile, @NotNull final String includeFilePath, boolean forCompletion, @NotNull String name) {
    List<String> split = StringUtil.split(includeFilePath, "/");
    String last = ContainerUtil.iterateAndGetLastItem(split);
    if (last != null) {
      PsiFile[] filesByName = FilenameIndex.getFilesByName(containingFile.getProject(), last, GlobalSearchScope.projectScope(containingFile.getProject()));
      List<PsiFile> filter = ContainerUtil.filter(filesByName, new Condition<PsiFile>() {
        @Override
        public boolean value(PsiFile psiFile) {
          VirtualFile virtualFile = psiFile.getVirtualFile();
          if (virtualFile == null) return false;
          String canonicalPath = virtualFile.getCanonicalPath();
          if (canonicalPath == null) return false;
          return canonicalPath.endsWith(includeFilePath);
        }
      });

      for (PsiFile file : filter) {
        if (file instanceof ErlangFile) {
          if (!forCompletion) {
            ErlangRecordDefinition recordFromIncludeFile = ((ErlangFile) file).getRecord(name);
            return recordFromIncludeFile == null ? ContainerUtil.<ErlangRecordDefinition>emptyList() : ContainerUtil.list(recordFromIncludeFile);
          } else {
            return ((ErlangFile) file).getRecords();
          }
        }
      }
    }
    return ContainerUtil.emptyList();
  }

  @NotNull
  public static List<ErlangRecordDefinition> justAppend(@NotNull PsiFile containingFile, @NotNull String includeFilePath, boolean forCompletion, @NotNull String name) {
    VirtualFile virtualFile = containingFile.getOriginalFile().getVirtualFile();
    VirtualFile parent = virtualFile != null ? virtualFile.getParent() : null;
    if (parent == null) return ContainerUtil.emptyList();
    String localPath = PathUtil.getLocalPath(parent);
    String globalPath = localPath + "/" + includeFilePath;

    VirtualFile fileByUrl = LocalFileSystem.getInstance().findFileByPath(globalPath);
    if (fileByUrl == null) return ContainerUtil.emptyList();
    PsiFile file = ((PsiManagerEx) PsiManager.getInstance(containingFile.getProject())).getFileManager().findFile(fileByUrl);
    if (file instanceof ErlangFile) {
      if (!forCompletion) {
        ErlangRecordDefinition recordFromIncludeFile = ((ErlangFile) file).getRecord(name);
        return recordFromIncludeFile == null ? ContainerUtil.<ErlangRecordDefinition>emptyList() : ContainerUtil.list(recordFromIncludeFile);
      } else {
        return ((ErlangFile) file).getRecords();
      }
    }
    return ContainerUtil.emptyList();
  }
}
