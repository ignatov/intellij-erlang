package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Spike 3: macro expansion as a separate layer, used only as an oracle.
 * <p>
 * The main tree, the stubs and the indices of a file are not touched. On demand, the layer expands the same-file
 * macros of the file's PSI text ({@link ErlangMacroExpander}), parses the expanded text as one non-physical Erlang
 * file whose original file is the real file (the "copy"), and caches it on the real file. A caller asks the copy only
 * when master's path has no answer, and every answer is mapped back to an element of a real file.
 * <p>
 * Switch: system property {@code erlang.macro.layer}, else the environment variable {@code ERLANG_MACRO_LAYER}:
 * {@code off} or {@code layer} (default). Form cap: {@code erlang.macro.layer.cap} / {@code ERLANG_MACRO_LAYER_CAP},
 * default 2,000 significant tokens per expanded form. Remote fallback ({@code m:f()} to a function made by a macro
 * in m): {@code erlang.macro.layer.remote} / {@code ERLANG_MACRO_LAYER_REMOTE} = {@code on}, default off.
 */
public final class ErlangMacroLayer {
  public enum Mode {OFF, LAYER}

  /** Marks a copy and points to its real file. */
  public static final Key<PsiFile> COPY_OF = Key.create("erlang.macro.layer.copy.of");
  private static final Key<com.intellij.psi.util.CachedValue<Copy>> COPY_KEY = Key.create("erlang.macro.layer.copy");

  public static final AtomicInteger EXPANSIONS_BUILT = new AtomicInteger();
  public static final AtomicInteger COPIES_PARSED = new AtomicInteger();
  public static final AtomicInteger FALLBACK_ASKED = new AtomicInteger();
  public static final AtomicInteger FALLBACK_ANSWERED = new AtomicInteger();
  public static final AtomicInteger ERRORS_HIDDEN = new AtomicInteger();
  public static final AtomicInteger GUARD_VIOLATIONS = new AtomicInteger();
  public static final AtomicInteger REMOTE_ANSWERED = new AtomicInteger();

  private static final Mode DEFAULT_MODE = readMode();
  public static final int FORM_CAP = readInt("erlang.macro.layer.cap", "ERLANG_MACRO_LAYER_CAP", 2000);
  private static final boolean REMOTE_DEFAULT = "on".equals(read("erlang.macro.layer.remote", "ERLANG_MACRO_LAYER_REMOTE"));
  private static volatile @Nullable Mode ourTestMode;
  private static volatile @Nullable Boolean ourTestRemote;

  private ErlangMacroLayer() {
  }

  private static @Nullable String read(String property, String env) {
    String value = System.getProperty(property);
    if (value == null || value.isEmpty()) value = System.getenv(env);
    return value == null || value.isEmpty() ? null : value.trim().toLowerCase(Locale.ROOT);
  }

  private static int readInt(String property, String env, int def) {
    String v = read(property, env);
    try {
      return v == null ? def : Integer.parseInt(v);
    }
    catch (NumberFormatException e) {
      return def;
    }
  }

  private static Mode readMode() {
    String v = read("erlang.macro.layer", "ERLANG_MACRO_LAYER");
    return "off".equals(v) || "none".equals(v) || "false".equals(v) ? Mode.OFF : Mode.LAYER;
  }

  public static @NotNull Mode getMode() {
    Mode m = ourTestMode;
    return m != null ? m : DEFAULT_MODE;
  }

  public static boolean isEnabled() {
    return getMode() == Mode.LAYER;
  }

  public static boolean isRemoteEnabled() {
    Boolean b = ourTestRemote;
    return b != null ? b : REMOTE_DEFAULT;
  }

  @TestOnly
  public static void setModeForTests(@NotNull Mode mode, @NotNull Disposable parent) {
    Mode previous = ourTestMode;
    ourTestMode = mode;
    Disposer.register(parent, () -> ourTestMode = previous);
  }

  @TestOnly
  public static void setRemoteForTests(boolean on, @NotNull Disposable parent) {
    Boolean previous = ourTestRemote;
    ourTestRemote = on;
    Disposer.register(parent, () -> ourTestRemote = previous);
  }

  // ------------------------------------------------------------------------------------------------ the copy

  /** The expanded copy of a real file. */
  public static final class Copy {
    public final @NotNull PsiFile realFile;
    public final @NotNull Expansion expansion;
    /** Null when no form is expanded: then the copy is not parsed at all. */
    public final @Nullable ErlangFile copyFile;
    /** Forms (by index) whose expanded text has an error element in the copy. */
    private final BitSet myBroken = new BitSet();
    public final long buildNanos;
    private volatile Map<String, PsiElement> myFunctionsMadeByMacros;

    Copy(@NotNull PsiFile realFile, @NotNull Expansion expansion, @Nullable ErlangFile copyFile, long buildNanos) {
      this.realFile = realFile;
      this.expansion = expansion;
      this.copyFile = copyFile;
      this.buildNanos = buildNanos;
      if (copyFile != null) {
        for (PsiErrorElement e : PsiTreeUtil.collectElementsOfType(copyFile, PsiErrorElement.class)) {
          Form f = expansion.formAtExpanded(e.getTextRange().getStartOffset());
          if (f == null) f = previousForm(e.getTextRange().getStartOffset());
          if (f != null) myBroken.set(expansion.forms.indexOf(f));
        }
      }
    }

    private @Nullable Form previousForm(int expOffset) {
      Form best = null;
      for (Form f : expansion.forms) {
        if (f.expEnd <= expOffset) best = f;
        else break;
      }
      return best;
    }

    /** The form is expanded and its expanded text parses without errors. */
    public boolean isClean(@Nullable Form f) {
      return f != null && f.status == FormStatus.EXPANDED && copyFile != null && !myBroken.get(expansion.forms.indexOf(f));
    }

    public boolean isBroken(@NotNull Form f) {
      return myBroken.get(expansion.forms.indexOf(f));
    }

    /** A target in the copy may be used when it lies in a form that is not expanded, or in a clean expanded form. */
    boolean isUsableTarget(int expOffset) {
      Form f = expansion.formAtExpanded(expOffset);
      if (f == null) return true;
      return f.status != FormStatus.EXPANDED || isClean(f);
    }

    /** Functions whose name exists only in an expansion, by "name/arity", with real targets (the source of the name). */
    public @NotNull Map<String, PsiElement> functionsMadeByMacros() {
      Map<String, PsiElement> map = myFunctionsMadeByMacros;
      if (map != null) return map;
      map = new LinkedHashMap<>();
      if (copyFile != null) {
        for (ErlangFunction fn : copyFile.getFunctions()) {
          PsiElement id = fn.getNameIdentifier();
          int at = id.getTextRange().getStartOffset();
          if (expansion.kindAt(at) == ErlangMacroExpander.SRC_VERBATIM) continue;
          if (!isUsableTarget(at)) continue;
          PsiElement real = toReal(fn);
          if (real != null) map.putIfAbsent(fn.getName() + "/" + fn.getArity(), real);
        }
      }
      myFunctionsMadeByMacros = map;
      return map;
    }

    /** The copy elements that correspond to a real range: its verbatim copy, else every copy of an argument token. */
    int @NotNull [] toCopyOffsets(@NotNull TextRange real) {
      int e = expansion.toExpanded(real.getStartOffset());
      if (e >= 0) return new int[]{e};
      return expansion.argumentCopies(real.getStartOffset());
    }

    /** Maps an element of the copy to a real element, or returns an element of another file as it is. */
    public @Nullable PsiElement toReal(@Nullable PsiElement target) {
      if (target == null || !target.isValid()) return null;
      PsiFile file = target.getContainingFile();
      if (file == null) return null;
      if (file != copyFile) return file.getUserData(COPY_OF) == null ? target : null;
      if (!isUsableTarget(target.getTextRange().getStartOffset())) return null;
      PsiElement real = mapElement(target);
      if (real != null) return real;
      if (target instanceof PsiNameIdentifierOwner owner) {
        PsiElement id = owner.getNameIdentifier();
        if (id != null) return mapElement(id);
      }
      return null;
    }

    private @Nullable PsiElement mapElement(@NotNull PsiElement copyElement) {
      TextRange r = copyElement.getTextRange();
      TextRange src = expansion.toSource(r.getStartOffset(), r.getEndOffset());
      if (src == null || src.isEmpty()) return null;
      return findReal(realFile, src, copyElement.getClass());
    }
  }

  /** The element of the real file with exactly this range; of the given class if one exists, else the topmost. */
  static @Nullable PsiElement findReal(@NotNull PsiFile file, @NotNull TextRange range, @NotNull Class<?> preferred) {
    PsiElement e = file.findElementAt(range.getStartOffset());
    PsiElement topmost = null;
    while (e != null && !(e instanceof PsiFile)) {
      TextRange er = e.getTextRange();
      if (!range.contains(er)) break;
      if (er.equals(range)) {
        if (preferred.isInstance(e)) return e;
        topmost = e;
      }
      e = e.getParent();
    }
    return topmost;
  }

  /** The real Erlang file of an element, or null for copies, consoles and non-Erlang files. */
  static @Nullable ErlangFile realFile(@Nullable PsiElement element) {
    if (element == null) return null;
    PsiFile file = element.getContainingFile();
    if (!(file instanceof ErlangFile ef)) return null;
    if (file.getUserData(COPY_OF) != null) return null;
    if (file.getOriginalFile().getUserData(ErlangPsiImplUtil.ERLANG_CONSOLE) != null) return null;
    return ef;
  }

  /** The cached copy of a real file; built lazily from the PSI text. */
  public static @Nullable Copy getCopy(@NotNull PsiFile file) {
    if (!isEnabled() || !(file instanceof ErlangFile) || file.getUserData(COPY_OF) != null) return null;
    Project project = file.getProject();
    return CachedValuesManager.getManager(project).getCachedValue(file, COPY_KEY, () -> {
      Copy copy = build(file);
      return CachedValueProvider.Result.create(copy, file, PsiModificationTracker.getInstance(project));
    }, false);
  }

  /** Builds the copy without the cache (also used by measurement tests). */
  public static @NotNull Copy build(@NotNull PsiFile file) {
    long t0 = System.nanoTime();
    EXPANSIONS_BUILT.incrementAndGet();
    CharSequence text = file.getViewProvider().getContents();
    if (file.getTextLength() != text.length() || !file.textMatches(text)) text = file.getText();
    Expansion expansion = ErlangMacroExpander.expand(text, new ErlangLexer(file.getProject()), FORM_CAP);
    ErlangFile copyFile = null;
    if (expansion.hasExpansions()) {
      COPIES_PARSED.incrementAndGet();
      PsiFile parsed = PsiFileFactory.getInstance(file.getProject())
        .createFileFromText(file.getName(), ErlangLanguage.INSTANCE, expansion.text, false, false);
      if (parsed instanceof ErlangFile ef) {
        parsed.putUserData(COPY_OF, file);
        if (parsed instanceof PsiFileImpl impl) impl.setOriginalFile(file);
        copyFile = ef;
        parsed.getNode().getFirstChildNode(); // parse now, so that the build time is honest
      }
    }
    return new Copy(file, expansion, copyFile, System.nanoTime() - t0);
  }

  // ------------------------------------------------------------------------------------------------ exits

  /** The I2 guard: nothing from a copy leaves the layer. */
  static @Nullable PsiElement exit(@Nullable PsiElement e, @NotNull PsiFile asking) {
    if (e == null) return null;
    PsiFile f = e.getContainingFile();
    boolean ok = e.isValid() && f != null && f.getUserData(COPY_OF) == null && (!asking.isPhysical() || e.isPhysical());
    if (ok) return e;
    GUARD_VIOLATIONS.incrementAndGet();
    if (ApplicationManager.getApplication().isUnitTestMode()) {
      throw new AssertionError("macro layer: an element of an expanded copy left the layer: " + e + " in " + f);
    }
    return null;
  }

  /** Cheap gate: the top-level form of the element contains a '?' and the file defines a macro. */
  private static boolean mayNeedExpansion(@NotNull ErlangFile file, @NotNull PsiElement element) {
    PsiElement form = element;
    while (form.getParent() != null && !(form.getParent() instanceof PsiFile)) form = form.getParent();
    if (form.getNode() == null || form.getNode().findLeafElementAt(0) == null) return false;
    if (!form.textContains('?')) return false;
    return !file.getMacroses().isEmpty();
  }

  // ------------------------------------------------------------------------------------------------ resolve

  private static boolean isRemoteCall(@NotNull PsiElement element) {
    return element.getParent() instanceof ErlangGlobalFunctionCallExpression;
  }

  /** Some top-level form of the main tree starts with '?' or is broken: only such forms can make a function. */
  private static boolean hasTopLevelMacroForm(@NotNull ErlangFile file) {
    if (file.getMacroses().isEmpty()) return false;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
      if (c instanceof PsiErrorElement) return true;
      PsiElement leaf = PsiTreeUtil.firstChild(c);
      if (leaf != null && "?".equals(leaf.getText())) return true;
    }
    return false;
  }

  private static @Nullable PsiReference correspondingReference(@NotNull Copy copy, @NotNull PsiReference ref, int occurrence) {
    PsiElement element = ref.getElement();
    TextRange abs = ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset());
    int[] offsets = copy.toCopyOffsets(abs);
    if (occurrence >= offsets.length || copy.copyFile == null) return null;
    int e = offsets[occurrence];
    if (!copy.isClean(copy.expansion.formAtExpanded(e))) return null;
    String refText = abs.substring(copy.realFile.getText());
    PsiReference candidate = copy.copyFile.findReferenceAt(e);
    if (candidate == null || candidate.getClass() != ref.getClass()) return null;
    TextRange cabs = candidate.getRangeInElement().shiftRight(candidate.getElement().getTextRange().getStartOffset());
    if (cabs.getStartOffset() != e || !refText.equals(cabs.substring(copy.copyFile.getText()))) return null;
    return candidate;
  }

  /** Hook: master's resolve found nothing. Returns a real element or null (master's answer). */
  public static @Nullable PsiElement resolveFallback(@NotNull PsiReference ref) {
    if (!isEnabled()) return null;
    PsiElement element = ref.getElement();
    ErlangFile file = realFile(element);
    if (file == null) return null;
    if (ref instanceof ErlangFunctionReference fr && isRemoteEnabled()) {
      PsiElement remote = remoteFallback(file, element, fr);
      if (remote != null) return remote;
    }
    boolean formHasUse = mayNeedExpansion(file, element);
    if (!formHasUse) {
      // a local function may be made by a macro in another form: the per-file table
      if (!(ref instanceof ErlangFunctionReference fr) || isRemoteCall(element) || !hasTopLevelMacroForm(file)) return null;
      FALLBACK_ASKED.incrementAndGet();
      Copy copy = getCopy(file);
      if (copy == null) return null;
      PsiElement real = exit(copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity()), file);
      if (real != null) FALLBACK_ANSWERED.incrementAndGet();
      return real;
    }
    FALLBACK_ASKED.incrementAndGet();
    Copy copy = getCopy(file);
    if (copy == null) return null;
    int copies = copy.toCopyOffsets(ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset())).length;
    for (int i = 0; i < copies; i++) {
      PsiReference c = correspondingReference(copy, ref, i);
      if (c == null) continue;
      PsiElement real = exit(copy.toReal(c.resolve()), file);
      if (real != null) {
        FALLBACK_ANSWERED.incrementAndGet();
        return real;
      }
    }
    return null;
  }

  /** Hook for poly-variant resolve (variables): master's multiResolve found nothing. */
  public static @NotNull ResolveResult @NotNull [] multiResolveFallback(@NotNull PsiPolyVariantReference ref) {
    if (!isEnabled()) return ResolveResult.EMPTY_ARRAY;
    PsiElement element = ref.getElement();
    ErlangFile file = realFile(element);
    if (file == null || !mayNeedExpansion(file, element)) return ResolveResult.EMPTY_ARRAY;
    FALLBACK_ASKED.incrementAndGet();
    Copy copy = getCopy(file);
    if (copy == null) return ResolveResult.EMPTY_ARRAY;
    int copies = copy.toCopyOffsets(ref.getRangeInElement().shiftRight(element.getTextRange().getStartOffset())).length;
    LinkedHashSet<PsiElement> result = new LinkedHashSet<>();
    for (int i = 0; i < copies; i++) {
      PsiReference c = correspondingReference(copy, ref, i);
      if (!(c instanceof PsiPolyVariantReference pc)) continue;
      for (ResolveResult rr : pc.multiResolve(false)) {
        PsiElement real = exit(copy.toReal(rr.getElement()), file);
        if (real != null) result.add(real);
      }
    }
    if (result.isEmpty()) return ResolveResult.EMPTY_ARRAY;
    FALLBACK_ANSWERED.incrementAndGet();
    return PsiElementResolveResult.createResults(result);
  }

  /** Remote call m:f() where f is made by a macro in m: the per-file table of m's file. Off by default. */
  private static @Nullable PsiElement remoteFallback(@NotNull ErlangFile file, @NotNull PsiElement element, @NotNull ErlangFunctionReference fr) {
    if (!(element.getParent() instanceof ErlangGlobalFunctionCallExpression g)) return null;
    ErlangQAtom moduleAtom = g.getModuleRef().getQAtom();
    String moduleName = ErlangPsiImplUtil.getName(moduleAtom);
    for (ErlangFile target : ErlangModuleIndex.getFilesByName(file.getProject(), moduleName, GlobalSearchScope.allScope(file.getProject()))) {
      Copy copy = getCopy(target);
      if (copy == null) continue;
      PsiElement real = copy.functionsMadeByMacros().get(fr.getName() + "/" + fr.getArity());
      if (real != null) {
        REMOTE_ANSWERED.incrementAndGet();
        return exit(real, file);
      }
    }
    return null;
  }

  // ------------------------------------------------------------------------------------------------ syntax errors

  /** Hook of the error filter: the error is in an expanded form whose expansion parses without errors. */
  public static boolean isErrorExplainedByExpansion(@NotNull PsiErrorElement error) {
    if (!isEnabled()) return false;
    ErlangFile file = realFile(error);
    if (file == null || file.getMacroses().isEmpty()) return false;
    Copy copy = getCopy(file);
    if (copy == null || copy.copyFile == null) return false;
    int offset = error.getTextRange().getStartOffset();
    Form f = copy.expansion.formAtSource(offset);
    if (f == null) {
      for (Form g : copy.expansion.forms) {
        if (g.srcEnd <= offset) f = g;
        else break;
      }
      if (f != null && !copy.expansion.sourceText.subSequence(f.srcEnd, offset).toString().isBlank()) f = null;
    }
    boolean hide = copy.isClean(f);
    if (hide) ERRORS_HIDDEN.incrementAndGet();
    return hide;
  }
}
