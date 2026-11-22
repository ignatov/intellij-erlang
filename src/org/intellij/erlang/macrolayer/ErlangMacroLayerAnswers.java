package org.intellij.erlang.macrolayer;

import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import java.util.*;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Copy;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Outcome;
import org.intellij.erlang.macrolayer.ErlangMacroLayer.Table;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangModuleReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangQAtomBasedReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangRecordReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangTypeReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The layer's answers to references, asked only when master's path has none: a reference in a form that needs the
 * copy resolves at the same place in the copy, and the target is mapped back to a real element; a record, field, type
 * or function that a macro makes in another form; a variable bound by a macro body or argument (a binding in the copy
 * that the main tree does not see binds itself); a record named by a macro ({@code -record(?REC, ...)}); optionally a
 * remote call to a function a macro makes in another module ({@code erlang.macro.layer.remote}).
 * <p>
 * Owns nothing: answers are kept in the copy ({@link ErlangMacroLayer.Copy#answers()}, dropped on any PSI change).
 * Threading: under a read action, cancellable; on the UI thread only a copy that is there is used.
 */
final class ErlangMacroLayerAnswers {
  private ErlangMacroLayerAnswers() {
  }



  static boolean isRemoteCall(@NotNull PsiElement element) {
    return element.getParent() instanceof ErlangGlobalFunctionCallExpression;
  }



  static @Nullable PsiReference correspondingReference(@NotNull Copy copy, @NotNull PsiReference ref, int occurrence) {
    return correspondingReference(copy, ref, occurrence, false);
  }

  /** {@code verbatimForm}: the reference may also lie in a form that is not expanded (its copy text is the source text). */
  static @Nullable PsiReference correspondingReference(@NotNull Copy copy, @NotNull PsiReference ref, int occurrence,
                                                               boolean verbatimForm) {
    PsiElement element = ref.getElement();
    TextRange abs = ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset());
    int[] offsets = copy.toCopyOffsets(abs);
    if (occurrence >= offsets.length || copy.copyFile == null) return null;
    int e = offsets[occurrence];
    Form form = copy.expansion.formAtExpanded(e);
    boolean usable = copy.isClean(form) || verbatimForm && form != null && form.status != FormStatus.EXPANDED;
    if (!usable) return null;
    String refText = abs.subSequence(copy.expansion.sourceText).toString();
    PsiReference candidate = copy.copyFile.findReferenceAt(e);
    if (candidate == null || candidate.getClass() != ref.getClass()) return null;
    TextRange cabs = candidate.getRangeInElement().shiftRight(candidate.getElement().getTextRange().getStartOffset());
    if (cabs.getStartOffset() != e || !refText.contentEquals(cabs.subSequence(copy.expansion.text))) return null;
    return candidate;
  }

  /** Hook: master's resolve found nothing. Returns a real element or null (master's answer). */
  public static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref) {
    if (!ErlangMacroLayerStats.on()) return resolveFallback(ref, null);
    Outcome o = new Outcome();
    long t0 = ErlangMacroLayerStats.enter();
    String entry = ref instanceof ErlangVariableReferenceImpl ? "variable(resolve)" : "resolve:" + ref.getClass().getSimpleName();
    try {
      return resolveFallback(ref, o);
    }
    finally {
      ErlangMacroLayerStats.exit(entry, o.value, o.file, t0);
    }
  }

  static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref, @Nullable Outcome o) {
    PsiElement element = ref.getElement();
    ErlangFile file = ErlangMacroLayer.layerFile(element);
    if (file == null) {
      ErlangMacroLayer.mark(o, "gate:not-layer-file");
      return null;
    }
    ErlangMacroLayer.markFile(o, file);
    if (ref instanceof ErlangVariableReferenceImpl v) {
      ResolveResult[] rr = multiResolveFallback(v, o);
      return rr.length > 0 ? rr[0].getElement() : null;
    }
    if (ref instanceof ErlangFunctionReference fr && isRemoteCall(element)) {
      ErlangMacroLayer.mark(o, ErlangMacroLayer.isRemoteEnabled() ? "remote" : "gate:remote-off");
      return ErlangMacroLayer.isRemoteEnabled() ? remoteFallback(file, element, fr) : null;
    }
    if (!ErlangMacroLayer.isGateWide() && isMacroNameReference(ref, element)) {
      ErlangMacroLayer.mark(o, "gate:macro-name");
      return null;
    }
    if (ref instanceof ErlangRecordReferenceImpl rr) {
      PsiElement named = recordNamedByMacro(file, rr.getCanonicalText());
      if (named != null) {
        ErlangMacroLayer.mark(o, "record-named-by-macro");
        return named;
      }
    }
    Table t = ErlangMacroLayer.table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) {
      ErlangMacroLayer.mark(o, t == null ? "gate:no-table" : t.refusal != null ? "gate:refused" : "gate:no-names");
      return null;
    }
    if (!ErlangMacroLayer.isGateWide() && !ErlangMacroLayer.anyFormNeedsCopy(file, t)) {
      ErlangMacroLayer.mark(o, "gate:file-needs-no-copy");
      return null;
    }
    PsiElement form = ErlangMacroLayer.topForm(element);
    boolean formHasUse = ErlangMacroLayer.formUsesMacro(t, form);
    if (formHasUse && !ErlangMacroLayer.formNeedsCopy(t, form) && !ErlangMacroLayer.hasTopLevelMacroForm(file, t)) {
      // The form's macros need no substitution, and no top-level form can make a name: the copy has no answer
      ErlangMacroLayer.mark(o, "gate:plain-form");
      return null;
    }
    if (!formHasUse) {
      if (!ErlangMacroLayer.hasTopLevelMacroForm(file, t)) {
        ErlangMacroLayer.mark(o, "gate:form-no-use");
        return null;
      }
      // A record, a record field or a local type may be made by a macro in another form
      if (isMadeNameReference(ref)) return madeNameFallback(file, ref, element);
      // a local function may be made by a macro in another form: the per-file table
      if (!(ref instanceof ErlangFunctionReference fr)) {
        ErlangMacroLayer.mark(o, "gate:form-no-use");
        return null;
      }
      ErlangMacroLayer.FALLBACK_ASKED.incrementAndGet();
      ErlangMacroLayerLog.query("function-table", file.getName());
      Copy copy = ErlangMacroLayer.getCopy(file, "function-table");
      if (copy == null) {
        ErlangMacroLayer.mark(o, "function-table:no-copy");
        return null;
      }
      PsiElement real = ErlangMacroLayer.exit(copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity()), file);
      if (real != null) ErlangMacroLayer.FALLBACK_ANSWERED.incrementAndGet();
      ErlangMacroLayer.mark(o, real != null ? "function-table:answered" : "function-table:none");
      return real;
    }
    ErlangMacroLayer.FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("resolve", file.getName());
    Copy copy = ErlangMacroLayer.getCopy(file, "resolve");
    if (copy == null) {
      ErlangMacroLayer.mark(o, "asked:no-copy");
      return null;
    }
    String answerKey = "r" + element.getTextRange().getStartOffset() + ref.getClass().getSimpleName();
    Object cached = copy.answers().get(answerKey);
    if (cached != null) {
      ErlangMacroLayer.mark(o, cached == NO_ANSWER ? "asked:cached-none" : "asked:cached-answer");
      return cached == NO_ANSWER ? null : (PsiElement)cached;
    }
    long q0 = System.nanoTime();
    PsiElement answer = resolveInCopy(copy, ref, element, file, t);
    ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    copy.answers().put(answerKey, answer == null ? NO_ANSWER : answer);
    ErlangMacroLayer.mark(o, answer == null ? "asked:computed-none" : "asked:computed-answer");
    return answer;
  }

  static final Object NO_ANSWER = new Object();

  /**
   * {@code -record(?REC, {...})} with {@code -define(REC, item).}: the record that {@code #item{}} means (the main
   * tree names it "?REC"). Only a macro without parameters whose body is one atom, and only the file's own records.
   */
  static @Nullable PsiElement recordNamedByMacro(@NotNull ErlangFile file, @NotNull String name) {
    if (name.isEmpty() || name.startsWith("?")) return null;
    for (ErlangRecordDefinition r : file.getRecords()) {
      ProgressManager.checkCanceled();
      ErlangQAtom q = r.getQAtom();
      ErlangMacros m = q == null ? null : q.getMacros();
      ErlangMacrosName macroName = m == null ? null : m.getMacrosName();
      if (macroName == null) continue;
      ErlangMacrosDefinition d = ErlangMacroResolve.resolve(file, StringUtil.unquoteString(macroName.getText(), '\''), ErlangMacroResolve.NO_ARGUMENTS);
      ErlangMacrosBody body = d == null || d.getArgumentDefinitionList() != null ? null : d.getMacrosBody();
      if (body == null) continue;
      String atom = body.getText().trim();
      if (!atom.matches("[a-z][A-Za-z0-9_@]*|'[^']*'")) continue;
      if (name.equals(StringUtil.unquoteString(atom, '\'')) || name.equals(atom)) return r;
    }
    return null;
  }

  /**
   * The reference's own name is a macro use ({@code ?NAME(...)}, {@code fun ?F/1}, {@code #?R{}}, ...).
   * The copy replaces that name with the macro's body, so no reference of the copy stands at the name: the copy can
   * never answer for it.
   */
  static boolean isMacroNameReference(@NotNull PsiReference ref, @NotNull PsiElement element) {
    TextRange range = ref.getRangeInElement();
    if (range.isEmpty()) return false;
    PsiElement leaf = element.findElementAt(range.getStartOffset());
    return leaf != null && leaf.getNode().getElementType() == ErlangTypes.ERL_QMARK;
  }

  /** A record, record field or local type reference: the names a macro can make besides functions. */
  static boolean isMadeNameReference(@NotNull PsiReference ref) {
    if (!(ref instanceof ErlangQAtomBasedReferenceImpl) || ref instanceof ErlangModuleReferenceImpl) return false;
    if (ref instanceof ErlangTypeReferenceImpl) {
      // remote types (m:t()) are not made in this file
      return PsiTreeUtil.getPrevSiblingOfType(ref.getElement(), ErlangModuleRef.class) == null;
    }
    return true;
  }

  /**
   * A record, field or type reference in a form without a macro use, in a file with a top-level macro form.
   * The form is copied verbatim into the copy, so the same reference there sees the definitions the expansions made.
   */
  static @Nullable PsiElement madeNameFallback(@NotNull ErlangFile file, @NotNull PsiReference ref, @NotNull PsiElement element) {
    ErlangMacroLayer.FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("made-name", file.getName());
    Copy copy = ErlangMacroLayer.getCopy(file, "made-name");
    if (copy == null || copy.copyFile == null) return null;
    String answerKey = "m" + element.getTextRange().getStartOffset() + ref.getClass().getName();
    Object cached = copy.answers().get(answerKey);
    if (cached != null) return cached == NO_ANSWER ? null : (PsiElement)cached;
    PsiElement answer = null;
    if (ref instanceof ErlangRecordReferenceImpl rr) answer = ErlangMacroLayer.exit(copy.recordsMadeByMacros().get(rr.getCanonicalText()), file);
    else if (ref instanceof ErlangTypeReferenceImpl tr) answer = ErlangMacroLayer.exit(copy.typesMadeByMacros().get(tr.getCanonicalText()), file);
    else {
      // a field: resolve the verbatim copy of the reference (its record is made by a macro)
      PsiReference c = correspondingReference(copy, ref, 0, true);
      if (c != null) answer = ErlangMacroLayer.exit(copy.toReal(c.resolve()), file);
    }
    if (answer != null) ErlangMacroLayer.FALLBACK_ANSWERED.incrementAndGet();
    copy.answers().put(answerKey, answer == null ? NO_ANSWER : answer);
    return answer;
  }

  static @Nullable PsiElement resolveInCopy(@NotNull Copy copy, @NotNull PsiReference ref, @NotNull PsiElement element,
                                                    @NotNull ErlangFile file, @NotNull Table t) {
    int copies = copy.toCopyOffsets(ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset())).length;
    for (int i = 0; i < copies; i++) {
      PsiReference c = correspondingReference(copy, ref, i);
      if (c == null) continue;
      PsiElement real = ErlangMacroLayer.exit(copy.toReal(c.resolve()), file);
      if (real != null) {
        ErlangMacroLayer.FALLBACK_ANSWERED.incrementAndGet();
        return real;
      }
    }
    if (ref instanceof ErlangFunctionReference fr && ErlangMacroLayer.hasTopLevelMacroForm(file, t)) {
      PsiElement real = ErlangMacroLayer.exit(copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity()), file);
      if (real != null) ErlangMacroLayer.FALLBACK_ANSWERED.incrementAndGet();
      return real;
    }
    return null;
  }

  /** Hook for poly-variant resolve (variables): master's multiResolve found nothing. */
  public static @NotNull ResolveResult @NotNull [] multiResolveFallback(@NotNull PsiPolyVariantReference ref) {
    if (!ErlangMacroLayerStats.on()) return multiResolveFallback(ref, null);
    Outcome o = new Outcome();
    long t0 = ErlangMacroLayerStats.enter();
    try {
      return multiResolveFallback(ref, o);
    }
    finally {
      ErlangMacroLayerStats.exit("variable(multiResolve)", o.value, o.file, t0);
    }
  }

  static @NotNull ResolveResult @NotNull [] multiResolveFallback(@NotNull PsiPolyVariantReference ref, @Nullable Outcome o) {
    PsiElement element = ref.getElement();
    ErlangFile file = ErlangMacroLayer.layerFile(element);
    if (file == null) {
      ErlangMacroLayer.mark(o, "gate:not-layer-file");
      return ResolveResult.EMPTY_ARRAY;
    }
    ErlangMacroLayer.markFile(o, file);
    Table t = ErlangMacroLayer.table(file);
    if (t == null || t.refusal != null || t.names.isEmpty()) {
      ErlangMacroLayer.mark(o, t == null ? "gate:no-table" : t.refusal != null ? "gate:refused" : "gate:no-names");
      return ResolveResult.EMPTY_ARRAY;
    }
    if (!ErlangMacroLayer.isGateWide() && !ErlangMacroLayer.anyFormNeedsCopy(file, t)) {
      ErlangMacroLayer.mark(o, "gate:file-needs-no-copy");
      return ResolveResult.EMPTY_ARRAY;
    }
    if ("_".equals(element.getText())) {
      ErlangMacroLayer.mark(o, "gate:underscore");
      return ResolveResult.EMPTY_ARRAY;
    }
    PsiElement form = ErlangMacroLayer.topForm(element);
    if (form == null || !ErlangMacroLayer.formUsesMacro(t, form)) {
      ErlangMacroLayer.mark(o, "gate:form-no-use");
      return ResolveResult.EMPTY_ARRAY;
    }
    if (!ErlangMacroLayer.formNeedsCopy(t, form)) {
      ErlangMacroLayer.mark(o, "gate:plain-form");
      return ResolveResult.EMPTY_ARRAY;
    }
    // the copy can bind the variable only through a macro body that names it, a macro argument, or a broken form;
    // the narrow gate: only a free variable of a body (not a parameter), and only the arguments of a use that
    // needs substitution (a plain use puts its arguments in value slots: their bindings are master's); only a
    // free variable of a body of a macro that this form uses (directly or through the macros of that body)
    boolean wide = ErlangMacroLayer.isGateWide();
    String name = element.getText();
    boolean body = wide ? t.bodyVars.contains(name) : ErlangMacroLayer.formFreeBodyVars(t, form).contains(name);
    boolean arg = !body && (wide ? ErlangMacroLayer.argumentVars(t, form) : ErlangMacroLayer.substitutedArgumentVars(t, form)).contains(name);
    boolean err = !body && !arg && ErlangMacroLayer.formHasError(t, form);
    if (!body && !arg && !err) {
      ErlangMacroLayer.mark(o, "gate:var-not-candidate");
      return ResolveResult.EMPTY_ARRAY;
    }
    String why = body ? "body-var" : arg ? "arg-var" : "form-error";
    ErlangMacroLayer.FALLBACK_ASKED.incrementAndGet();
    ErlangMacroLayerLog.query("variable", file.getName());
    Copy copy = ErlangMacroLayer.getCopy(file, "variable");
    if (copy == null) {
      ErlangMacroLayer.mark(o, "asked(" + why + "):no-copy");
      return ResolveResult.EMPTY_ARRAY;
    }
    String answerKey = "v" + element.getTextRange().getStartOffset();
    Object cached = copy.answers().get(answerKey);
    if (cached instanceof ResolveResult[] rr) {
      ErlangMacroLayer.mark(o, "asked(" + why + "):cached-" + (rr.length > 0 ? "answer" : "none"));
      return rr;
    }
    long q0 = System.nanoTime();
    ResolveResult[] answer = multiResolveInCopy(copy, ref, element, file);
    ErlangMacroLayerLog.queryNanos(System.nanoTime() - q0);
    copy.answers().put(answerKey, answer);
    ErlangMacroLayer.mark(o, "asked(" + why + "):computed-" + (answer.length > 0 ? "answer" : "none"));
    return answer;
  }

  static @NotNull ResolveResult @NotNull [] multiResolveInCopy(@NotNull Copy copy, @NotNull PsiPolyVariantReference ref,
                                                                      @NotNull PsiElement element, @NotNull ErlangFile file) {
    int copies = copy.toCopyOffsets(ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset())).length;
    LinkedHashSet<PsiElement> result = new LinkedHashSet<>();
    for (int i = 0; i < copies; i++) {
      PsiReference c = correspondingReference(copy, ref, i);
      if (!(c instanceof PsiPolyVariantReference pc)) continue;
      ResolveResult[] rrs = pc.multiResolve(false);
      for (ResolveResult rr : rrs) {
        PsiElement real = ErlangMacroLayer.exit(copy.toReal(rr.getElement()), file);
        if (real != null) result.add(real);
      }
      // In the copy the variable binds itself (a parameter of a function that a top-level macro use makes, the left
      // side of a match), while the main tree does not see a binding there: the real variable is a binding, not an
      // unbound one (where the main tree sees the binding, master's answer, none, stays)
      PsiElement v = c.getElement();
      if (rrs.length == 0 && isBinding(v) && !isBinding(element)) {
        PsiElement real = ErlangMacroLayer.exit(copy.toReal(v), file);
        if (real != null) result.add(real);
      }
    }
    if (result.isEmpty()) return ResolveResult.EMPTY_ARRAY;
    ErlangMacroLayer.FALLBACK_ANSWERED.incrementAndGet();
    return PsiElementResolveResult.createResults(result);
  }

  static boolean isBinding(@NotNull PsiElement v) {
    return v instanceof ErlangQVar &&
           (ErlangPsiImplUtil.inArgumentDefinition(v) && !ErlangPsiImplUtil.inArgumentList(v) || ErlangPsiImplUtil.inLeftPartOfAssignment(v));
  }

  /** Remote call m:f() where f is made by a macro in m: the per-file table of m's file. Off by default. */
  static @Nullable PsiElement remoteFallback(@NotNull ErlangFile file, @NotNull PsiElement element, @NotNull ErlangFunctionReference fr) {
    if (!(element.getParent() instanceof ErlangGlobalFunctionCallExpression g)) return null;
    ErlangQAtom moduleAtom = g.getModuleRef().getQAtom();
    String moduleName = ErlangPsiImplUtil.getName(moduleAtom);
    for (ErlangFile target : ErlangModuleIndex.getFilesByName(file.getProject(), moduleName, GlobalSearchScope.allScope(file.getProject()))) {
      ErlangMacroLayerLog.query("remote", file.getName());
      Copy copy = ErlangMacroLayer.getCopy(target, "remote");
      if (copy == null) continue;
      PsiElement real = copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity());
      if (real != null) {
        ErlangMacroLayer.REMOTE_ANSWERED.incrementAndGet();
        return ErlangMacroLayer.exit(real, file);
      }
    }
    return null;
  }
}
