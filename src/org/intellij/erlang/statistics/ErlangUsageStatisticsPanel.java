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

package org.intellij.erlang.statistics;

import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.TitledSeparator;
import com.intellij.ui.components.ActionLink;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBUI;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;

/**
 * The "Usage Statistics" section of the Erlang External Tools settings page.
 */
public final class ErlangUsageStatisticsPanel extends JPanel {
  public static final String CHECKBOX_TEXT = "Send anonymous usage statistics";
  private final JBCheckBox myCheckBox = new JBCheckBox(CHECKBOX_TEXT);
  // the value shown by the last reset; a second settings dialog that did not touch the check box changes nothing
  private boolean myShownValue;

  public ErlangUsageStatisticsPanel() {
    super(new BorderLayout());
    JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(8), 0));
    JButton show = new JButton("Show what is sent");
    show.addActionListener(e -> new PreviewDialog(this, ErlangUsageStatistics.getInstance().getCollector().previewToday()).show());
    actions.add(show);
    actions.add(new ActionLink("Privacy policy", (ActionListener)e -> BrowserUtil.browse(ErlangUsageStatistics.PRIVACY_URL)));
    JPanel form = FormBuilder.createFormBuilder()
      .addComponent(new TitledSeparator("Usage Statistics"))
      .addComponent(myCheckBox)
      .addComponent(new JBLabel("Once a day: counts of the features used, the IDE, the OS and the Erlang/OTP release. " +
                                "Never code, names or paths."))
      .addComponent(actions)
      .getPanel();
    add(form, BorderLayout.CENTER);
    reset();
  }

  public boolean isModified() {
    return myCheckBox.isSelected() != myShownValue;
  }

  public void apply() {
    if (!isModified()) return;
    ErlangUsageStatistics.getInstance().setEnabled(myCheckBox.isSelected());
    myShownValue = myCheckBox.isSelected();
  }

  public void reset() {
    myShownValue = isEnabled(ErlangUsageStatistics.getInstance());
    myCheckBox.setSelected(myShownValue);
  }

  @TestOnly
  void setSelectedForTests(boolean selected) {
    myCheckBox.setSelected(selected);
  }

  private static boolean isEnabled(@NotNull ErlangUsageStatistics statistics) {
    return statistics.getCollector().getConsent() == ErlangUsageCollector.Consent.YES;
  }

  private static final class PreviewDialog extends DialogWrapper {
    private final String myJson;

    PreviewDialog(@NotNull Component parent, @NotNull String json) {
      super(parent, false);
      myJson = json;
      setTitle("What Is Sent");
      init();
    }

    @Override
    protected @Nullable JComponent createCenterPanel() {
      JBTextArea text = new JBTextArea(prettyPrint(myJson), 24, 72);
      text.setEditable(false);
      text.setFont(JBUI.Fonts.create(Font.MONOSPACED, text.getFont().getSize()));
      JPanel panel = new JPanel(new BorderLayout(0, JBUI.scale(8)));
      panel.add(new JBLabel("The summary of today so far. If the statistics are on, it is sent tomorrow."), BorderLayout.NORTH);
      panel.add(new JBScrollPane(text), BorderLayout.CENTER);
      return panel;
    }

    @Override
    protected Action @NotNull [] createActions() {
      return new Action[]{getOKAction()};
    }
  }

  @NotNull
  static String prettyPrint(@NotNull String json) {
    StringBuilder result = new StringBuilder();
    int indent = 0;
    boolean inString = false;
    for (int i = 0; i < json.length(); i++) {
      char c = json.charAt(i);
      if (inString) {
        result.append(c);
        if (c == '\\' && i + 1 < json.length()) result.append(json.charAt(++i));
        else if (c == '"') inString = false;
        continue;
      }
      switch (c) {
        case '"' -> {
          inString = true;
          result.append(c);
        }
        case '{', '[' -> {
          indent++;
          result.append(c).append('\n').append("  ".repeat(indent));
        }
        case '}', ']' -> {
          indent--;
          result.append('\n').append("  ".repeat(Math.max(indent, 0))).append(c);
        }
        case ',' -> result.append(c).append('\n').append("  ".repeat(indent));
        case ':' -> result.append(": ");
        default -> result.append(c);
      }
    }
    return result.toString();
  }
}
