package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

/**
 * M2: the switch of the macro layer, the Registry key {@code erlang.macro.layer}, is read at call time, and with it off
 * the plugin behaves as master: on the owner's samples ({@code testData/macroLayerOff/samples}, his main.erl among
 * them) the highlighting infos with every Erlang inspection on, the reference at every {@code ?} and every atom, and the
 * completion variants after every {@code ?} and inside the first calls of a file are the ones master 2a6160fc gives
 * ({@code testData/macroLayerOff/expected}, written by this same class on master with {@code S6_OFF_WRITE}). No table and
 * no copy is built. The class compiles and runs on master: it reaches the layer only by reflection.
 */
public class ErlangMacroLayerOffTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String KEY = "erlang.macro.layer";
  private static final Path BASE = Paths.get("testData/macroLayerOff");
  private static final String LAYER = "org.intellij.erlang.macrolayer.ErlangMacroLayer";
  private boolean myAutocomplete;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    myAutocomplete = CodeInsightSettings.getInstance().AUTOCOMPLETE_ON_CODE_COMPLETION;
    CodeInsightSettings.getInstance().AUTOCOMPLETE_ON_CODE_COMPLETION = false;
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      CodeInsightSettings.getInstance().AUTOCOMPLETE_ON_CODE_COMPLETION = myAutocomplete;
    }
    finally {
      super.tearDown();
    }
  }

  private static boolean hasLayer() {
    try {
      Class.forName(LAYER);
      return true;
    }
    catch (ClassNotFoundException e) {
      return false;
    }
  }

  private static boolean layerEnabled() throws Exception {
    return (Boolean)Class.forName(LAYER).getMethod("isEnabled").invoke(null);
  }

  private static AtomicInteger counter(String name) throws Exception {
    return (AtomicInteger)Class.forName(LAYER).getField(name).get(null);
  }

  public void testTheRegistryKeyIsReadAtCallTime() throws Exception {
    if (!hasLayer()) return;
    assertTrue("on by default", layerEnabled());
    Registry.get(KEY).setValue(false, getTestRootDisposable());
    assertFalse("off as soon as the key is off", layerEnabled());
    Registry.get(KEY).setValue(true);
    assertTrue("on again as soon as the key is on", layerEnabled());
  }

  public void testOffEqualsMaster() throws Exception {
    String write = System.getenv("S6_OFF_WRITE");
    if (hasLayer()) {
      assertNull("the golden files are written on master only", write);
      // the same getMode() path as the Registry key (see testTheRegistryKeyIsReadAtCallTime)
      Class<?> layer = Class.forName(LAYER);
      @SuppressWarnings({"unchecked", "rawtypes"})
      Object off = Enum.valueOf((Class<Enum>)Class.forName(LAYER + "$Mode"), "OFF");
      layer.getMethod("setModeForTests", off.getClass(), com.intellij.openapi.Disposable.class).invoke(null, off, getTestRootDisposable());
      assertFalse(layerEnabled());
      counter("TABLES_BUILT").set(0);
      counter("EXPANSIONS_BUILT").set(0);
    }
    Path samples = BASE.resolve("samples");
    List<String> erl = new ArrayList<>();
    try (Stream<Path> s = Files.walk(samples)) {
      for (Path p : s.filter(Files::isRegularFile).sorted().toList()) {
        String rel = samples.relativize(p).toString().replace('\\', '/');
        myFixture.addFileToProject(rel, read(p));
        if (rel.endsWith(".erl")) erl.add(rel);
      }
    }
    enableAll();
    // the differences from master that stay with the layer off (ErlangMacroResolve: by arity and conditional branch):
    // "file TAB master's line TAB the branch's line", lines starting with # are reasons
    List<String[]> allowed = new ArrayList<>();
    for (String line : read(BASE.resolve("allowed-differences.txt")).split("\n")) {
      if (!line.isBlank() && !line.startsWith("#")) allowed.add(line.split("\t"));
    }
    List<String> differ = new ArrayList<>();
    for (String rel : erl) {
      String dump = dump(rel);
      Path expected = (write != null ? Paths.get(write) : BASE.resolve("expected")).resolve(rel + ".txt");
      if (write != null) {
        Files.createDirectories(expected.getParent());
        Files.writeString(expected, dump);
        continue;
      }
      String want = read(expected);
      for (String[] d : allowed) {
        if (d[0].equals(rel)) want = want.replace(d[1] + "\n", d[2] + "\n");
      }
      if (!want.equals(dump)) {
        differ.add(rel);
        System.out.println("S6-OFF DIFF " + rel + "\n--- master\n" + want + "\n--- branch, layer off\n" + dump);
      }
    }
    if (hasLayer()) {
      assertEquals("no table built with the layer off", 0, counter("TABLES_BUILT").get());
      assertEquals("no copy built with the layer off", 0, counter("EXPANSIONS_BUILT").get());
    }
    assertEquals(List.of(), differ);
  }

  private String dump(String rel) {
    PsiFile f = myFixture.getPsiManager().findFile(myFixture.findFileInTempDir(rel));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    String text = f.getText();
    StringBuilder sb = new StringBuilder();
    List<String> infos = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      infos.add(at(text, h.getStartOffset()) + "-" + at(text, h.getEndOffset()) + " " + h.getSeverity() + " " + h.getDescription() +
                " " + (h.forcedTextAttributesKey != null ? h.forcedTextAttributesKey : h.type.getAttributesKey()).getExternalName());
    }
    Collections.sort(infos);
    for (String i : infos) sb.append("HIGHLIGHT ").append(i).append('\n');
    List<Integer> calls = new ArrayList<>();
    for (int i = 0; i < text.length(); i++) {
      PsiElement leaf = f.findElementAt(i);
      if (leaf == null || leaf.getTextRange().getStartOffset() != i) continue;
      IElementType type = leaf.getNode().getElementType();
      if (type != ErlangTypes.ERL_QMARK && type != ErlangTypes.ERL_ATOM_NAME) continue;
      sb.append("REF ").append(at(text, i)).append(' ').append(leaf.getText()).append(" -> ").append(references(f, i + (type == ErlangTypes.ERL_QMARK ? 1 : 0))).append('\n');
      if (type == ErlangTypes.ERL_ATOM_NAME && calls.size() < 3 && i + leaf.getTextLength() < text.length() &&
          text.charAt(i + leaf.getTextLength()) == '(' && i > 0 && text.charAt(i - 1) != '\n' && text.charAt(i - 1) != '-') {
        calls.add(i + 1);
      }
    }
    for (int i = 0; i < text.length(); i++) {
      if (text.charAt(i) == '?' && !inComment(text, i)) calls.add(i + 1);
    }
    for (int offset : calls) sb.append("COMPLETE ").append(at(text, offset)).append(" -> ").append(complete(offset, text)).append('\n');
    return sb.toString();
  }

  private static boolean inComment(String text, int offset) {
    int start = text.lastIndexOf('\n', offset) + 1;
    return text.substring(start, offset).contains("%");
  }

  private static String at(String text, int offset) {
    int line = StringUtil.offsetToLineNumber(text, offset);
    return (line + 1) + ":" + (offset - StringUtil.lineColToOffset(text, line, 0) + 1);
  }

  private static String references(PsiFile f, int offset) {
    PsiReference ref = f.findReferenceAt(offset);
    if (ref == null) return "none";
    List<PsiReference> refs = ref instanceof PsiMultiReference m ? List.of(m.getReferences()) : List.of(ref);
    List<String> r = new ArrayList<>();
    for (PsiReference one : refs) {
      PsiElement t = one.resolve();
      if (t == null && one instanceof PsiPolyVariantReference p) {
        ResolveResult[] results = p.multiResolve(false);
        if (results.length > 0) t = results[0].getElement();
      }
      r.add(one.getClass().getSimpleName() + ":" + target(t));
    }
    Collections.sort(r);
    return String.join(", ", r);
  }

  private static String target(PsiElement t) {
    if (t == null) return "unresolved";
    PsiFile file = t.getContainingFile();
    if (t instanceof PsiFile || file == null) return String.valueOf(t instanceof PsiFile pf ? pf.getName() : t);
    String text = file.getText();
    String first = t.getText().split("\n", 2)[0];
    return file.getName() + ":" + at(text, t.getTextRange().getStartOffset()) + ":" + StringUtil.first(first, 40, true);
  }

  private String complete(int offset, String text) {
    Document d = myFixture.getEditor().getDocument();
    myFixture.getEditor().getCaretModel().moveToOffset(offset);
    LookupElement[] items = myFixture.complete(CompletionType.BASIC);
    List<String> names = new ArrayList<>();
    if (items != null) for (LookupElement e : items) names.add(e.getLookupString());
    LookupManager.hideActiveLookup(getProject());
    if (!d.getText().equals(text)) {
      names.add("(inserted)");
      WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(text));
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    }
    Collections.sort(names);
    String all = String.join(" ", names);
    return names.size() + " " + Integer.toHexString(all.hashCode()) + " " + StringUtil.first(all, 300, true);
  }

  private void enableAll() {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  private static String read(Path p) throws IOException {
    return new String(Files.readAllBytes(p), StandardCharsets.UTF_8).replace("\r\n", "\n");
  }
}
