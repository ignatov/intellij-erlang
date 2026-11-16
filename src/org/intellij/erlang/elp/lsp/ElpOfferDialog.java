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

package org.intellij.erlang.elp.lsp;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.util.text.HtmlChunk;
import com.intellij.ui.components.BrowserLink;
import com.intellij.ui.components.JBLabel;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBFont;
import com.intellij.util.ui.JBUI;
import org.intellij.erlang.elp.distribution.ElpAvailability;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.TestOnly;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The offer to try ELP: what ELP is, what stays with the plugin and what "Try ELP" does, because the click is the user's
 * consent. "Not now", Esc and the close button of the window all end in {@link #doCancelAction()}; with "Don't ask again"
 * selected they mean {@link ElpOffer.Choice#NEVER}.
 */
final class ElpOfferDialog extends DialogWrapper {
  static final String TITLE = "Try ELP for Erlang";
  static final String ABOUT = "About ELP";
  static final String DOCS_URL = "https://whatsapp.github.io/erlang-language-platform/";
  private static final int WIDTH = 520;
  private static final int BULLET_INDENT = 22;
  private static final String BULLET_MARK = "\u2022";

  enum Kind {PARAGRAPH, HEADING, BULLET}

  record Block(@NotNull Kind kind, @NotNull String text) {
  }

  private final List<Block> myContent;
  private boolean myDontAsk;

  ElpOfferDialog(@NotNull Project project, @NotNull List<Block> content) {
    super(project);
    myContent = content;
    setTitle(TITLE);
    setOKButtonText(ElpOffer.TRY);
    setCancelButtonText(ElpOffer.NOT_NOW);
    // Qualified: inside a DialogWrapper the simple name is the deprecated DialogWrapper.DoNotAskOption.
    setDoNotAskOption(new com.intellij.openapi.ui.DoNotAskOption.Adapter() {
      @Override
      public void rememberChoice(boolean isSelected, int exitCode) {
        myDontAsk = isSelected;
      }

      @Override
      public boolean shouldSaveOptionsOnCancel() {
        return true;
      }

      @NotNull
      @Override
      public String getDoNotShowMessage() {
        return ElpOffer.NEVER;
      }
    });
    init();
  }

  /** The text of the dialog; the download line only when "Try ELP" downloads the server. */
  @NotNull
  static List<Block> content(@NotNull ElpAvailability availability) {
    List<Block> blocks = new ArrayList<>();
    blocks.add(new Block(Kind.PARAGRAPH,
                         "ELP (Erlang Language Platform) is the open-source Erlang language server that WhatsApp develops. " +
                         "It reads the project the way rebar3 builds it, so it knows the dependencies, the include paths and the macros."));
    blocks.add(new Block(Kind.HEADING, "Why try it"));
    blocks.add(new Block(Kind.BULLET, "Errors and warnings follow macros, -ifdef and includes of the real build."));
    blocks.add(new Block(Kind.BULLET, "Go to definition, find usages and rename come from the same engine."));
    blocks.add(new Block(Kind.BULLET, "It stays fast on large projects."));
    blocks.add(new Block(Kind.HEADING, "What stays with the plugin"));
    blocks.add(new Block(Kind.BULLET, "Completion, formatting, refactorings, run and debug."));
    blocks.add(new Block(Kind.HEADING, "What Try ELP does"));
    ElpRelease.Asset asset = availability.choice().asset();
    if (availability.kind() == ElpAvailability.Kind.DOWNLOADABLE && asset != null) {
      blocks.add(new Block(Kind.BULLET, "Downloads ELP " + availability.release().version() + " (" + asset.sizeInMegabytes() +
                                        ") from GitHub and checks its checksum."));
    }
    blocks.add(new Block(Kind.BULLET,
                         "When a project is opened, ELP runs rebar3 to fetch and build its dependencies and writes to _build."));
    blocks.add(new Block(Kind.PARAGRAPH, "You can switch back at any time in Settings | Other Settings | Erlang External Tools."));
    return blocks;
  }

  /** The content as plain text, one block per line, for tests and the log. */
  @NotNull
  static String text(@NotNull List<Block> content) {
    return content.stream().map(b -> b.kind() == Kind.BULLET ? BULLET_MARK + " " + b.text() : b.text()).collect(Collectors.joining("\n"));
  }

  /** What the user chose; read after the dialog is closed. */
  @NotNull
  ElpOffer.Choice choice() {
    return choice(getExitCode(), myDontAsk);
  }

  @NotNull
  static ElpOffer.Choice choice(int exitCode, boolean dontAsk) {
    if (exitCode == OK_EXIT_CODE) return ElpOffer.Choice.TRY;
    return dontAsk ? ElpOffer.Choice.NEVER : ElpOffer.Choice.NOT_NOW;
  }

  @NotNull
  @Override
  protected JComponent createCenterPanel() {
    // FormBuilder and the panels scale the gaps themselves.
    FormBuilder form = FormBuilder.createFormBuilder();
    Kind previous = null;
    for (Block block : myContent) {
      int gap = previous == null ? 0 : block.kind() == Kind.BULLET && previous != Kind.PARAGRAPH ? 4 : 12;
      form.addComponent(component(block), gap);
      previous = block.kind();
    }
    form.addComponent(JBUI.Panels.simplePanel().addToLeft(new BrowserLink(ABOUT, DOCS_URL)), 12);
    return form.getPanel();
  }

  @NotNull
  private static JComponent component(@NotNull Block block) {
    return switch (block.kind()) {
      case HEADING -> new JBLabel(block.text()).withFont(JBFont.label().asBold());
      case PARAGRAPH -> label(block.text(), WIDTH);
      case BULLET -> {
        JBLabel bullet = new JBLabel(BULLET_MARK);
        bullet.setVerticalAlignment(SwingConstants.TOP);
        yield JBUI.Panels.simplePanel(6, 0)
          .addToLeft(bullet)
          .addToCenter(label(block.text(), WIDTH - BULLET_INDENT))
          .withBorder(JBUI.Borders.emptyLeft(8));
      }
    };
  }

  /** A label that wraps at {@code width}; the width attribute is in pixels, as in the platform's own popup hints. */
  @NotNull
  private static JBLabel label(@NotNull String text, int width) {
    JBLabel label = new JBLabel(HtmlChunk.text(text).wrapWith(HtmlChunk.div().attr("width", JBUI.scale(width)))
                                  .wrapWith(HtmlChunk.html()).toString());
    label.setVerticalAlignment(SwingConstants.TOP);
    return label;
  }

  @TestOnly
  @NotNull
  List<Block> content() {
    return myContent;
  }

  /** The buttons, "Try ELP" first. */
  @TestOnly
  @NotNull
  List<Action> buttons() {
    return List.of(getOKAction(), getCancelAction());
  }

  @TestOnly
  @NotNull
  JCheckBox dontAskCheckBox() {
    return myCheckBoxDoNotShowDialog;
  }
}
