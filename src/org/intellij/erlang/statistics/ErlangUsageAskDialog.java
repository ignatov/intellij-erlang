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

import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.icons.AllIcons;
import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.components.ActionLink;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.NamedColorUtil;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.Arrays;
import java.util.List;

/**
 * A small dialog in the middle of the IDE window that asks whether the plugin may send usage statistics.
 * "Ask me later", Esc and closing the window all mean later.
 */
public final class ErlangUsageAskDialog extends DialogWrapper {
  public static final String TITLE = "Help improve the Erlang plugin";
  public static final String YES = "Yes, send statistics";
  public static final String NO = "No, thanks";
  public static final String LATER = "Ask me later";
  public static final String NEVER_SENT = "It never sends code, file names or paths.";
  private static final int NO_EXIT_CODE = NEXT_USER_EXIT_CODE;
  // the buttons do not react this long after the dialog appears, so a key press already on its way does not answer
  static final int GUARD_MS = 1500;
  public static final String EXAMPLE_TITLE = "Example of the data that is sent";
  public static final String REAL_SUMMARY = "The real summary of today is in Settings | Erlang External Tools.";
  private static final int TEXT_WIDTH = 440;
  private static final int EXAMPLE_ROWS = 13;

  public static final ErlangUsageAskUi UI = new ErlangUsageAskUi() {
    @Override
    public boolean canAskNow(@NotNull Project project) {
      if (project.isDisposed() || DumbService.isDumb(project)) return false;
      if (!ModalityState.current().equals(ModalityState.nonModal())) return false;
      if (JBPopupFactory.getInstance().isPopupActive() || LookupManager.getInstance(project).getActiveLookup() != null) return false;
      JFrame frame = WindowManager.getInstance().getFrame(project);
      return frame != null && frame.isActive();
    }

    @Override
    public @NotNull ErlangUsageAskUi.Choice ask(@NotNull Project project, boolean reminder) {
      ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(project, reminder);
      // closes the dialog if the project closes; after the dialog closes, nothing keeps it
      Disposer.register(project, dialog.getDisposable());
      dialog.show();
      return choiceOf(dialog.getExitCode());
    }
  };

  private final boolean myReminder;
  private JComponent myExample;
  private JComponent myContent;
  private ActionLink myToggle;
  private Dimension myCollapsedSize;
  private final Action myNoAction = new DialogWrapperExitAction(NO, NO_EXIT_CODE) {
    @Override
    protected void doAction(ActionEvent e) {
      if (!myGuarded) super.doAction(e);
    }
  };
  private volatile boolean myGuarded = true;

  ErlangUsageAskDialog(@Nullable Project project, boolean reminder) {
    super(project, false);
    myReminder = reminder;
    setTitle(TITLE);
    setOKButtonText(YES);
    setCancelButtonText(LATER);
    setResizable(false);
    init();
    startGuard(myContent);
  }

  @NotNull
  static ErlangUsageAskUi.Choice choiceOf(int exitCode) {
    if (exitCode == OK_EXIT_CODE) return ErlangUsageAskUi.Choice.YES;
    if (exitCode == NO_EXIT_CODE) return ErlangUsageAskUi.Choice.NO;
    return ErlangUsageAskUi.Choice.LATER;
  }

  @Override
  protected Action @NotNull [] createActions() {
    return new Action[]{getOKAction(), myNoAction, getCancelAction()};
  }

  // "Yes, send statistics" is the default button, but not in the first moment after the dialog appears
  @Override
  protected void doOKAction() {
    if (!myGuarded) super.doOKAction();
  }

  // the buttons are disabled until the guard time after the dialog is shown is over; Esc still means later
  private void startGuard(@NotNull JComponent shown) {
    setButtonsEnabled(false);
    shown.addHierarchyListener(new HierarchyListener() {
      @Override
      public void hierarchyChanged(HierarchyEvent e) {
        if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) == 0 || !shown.isShowing()) return;
        shown.removeHierarchyListener(this);
        Timer timer = new Timer(GUARD_MS, event -> endGuard());
        timer.setRepeats(false);
        Disposer.register(getDisposable(), timer::stop);
        timer.start();
      }
    });
  }

  private void endGuard() {
    if (isDisposed()) return;
    myGuarded = false;
    setButtonsEnabled(true);
  }

  private void setButtonsEnabled(boolean enabled) {
    getOKAction().setEnabled(enabled);
    myNoAction.setEnabled(enabled);
    JButton later = getButton(getCancelAction());
    if (later != null) later.setEnabled(enabled);
  }

  @TestOnly
  void endGuardForTests() {
    endGuard();
  }

  @TestOnly
  boolean isGuardedForTests() {
    return myGuarded;
  }

  @TestOnly
  void pressYesForTests() {
    doOKAction();
  }

  @TestOnly
  @NotNull
  List<Action> getActionsForTests() {
    return List.of(createActions());
  }

  @TestOnly
  void toggleExampleForTests() {
    toggleExample();
  }

  @TestOnly
  @NotNull
  Dimension getContentPreferredSizeForTests() {
    return myContent.getPreferredSize();
  }

  @TestOnly
  @NotNull
  List<String> getButtonTextsForTests() {
    return Arrays.stream(createActions()).map(action -> (String)action.getValue(Action.NAME)).toList();
  }

  @TestOnly
  boolean isExampleShownForTests() {
    return myExample.isVisible();
  }

  @TestOnly
  void clickNoForTests() {
    myNoAction.actionPerformed(new ActionEvent(myContent, ActionEvent.ACTION_PERFORMED, NO));
  }

  @Override
  protected @NotNull JComponent createCenterPanel() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    myContent = panel;
    String text = myReminder ? ErlangUsageStatistics.REMINDER_TEXT : ErlangUsageStatistics.TEXT;
    panel.add(left(new JBLabel(wrapped(text))));
    panel.add(Box.createVerticalStrut(JBUI.scale(8)));
    JBLabel neverSent = new JBLabel(wrapped(NEVER_SENT), UIUtil.ComponentStyle.SMALL);
    neverSent.setForeground(NamedColorUtil.getInactiveTextColor());
    panel.add(left(neverSent));
    panel.add(Box.createVerticalStrut(JBUI.scale(8)));
    JPanel links = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
    links.add(new ActionLink("Privacy policy", (ActionListener)e -> BrowserUtil.browse(ErlangUsageStatistics.PRIVACY_URL)));
    panel.add(left(links));
    panel.add(Box.createVerticalStrut(JBUI.scale(8)));
    panel.add(left(createExample()));
    return panel;
  }

  // a collapsed section with an example summary; the dialog grows when it opens and shrinks when it closes
  @NotNull
  private JComponent createExample() {
    JBTextArea json = new JBTextArea(ErlangUsageStatisticsPanel.prettyPrint(ErlangUsagePayload.example()), EXAMPLE_ROWS, 0);
    json.setEditable(false);
    json.setFont(JBUI.Fonts.create(Font.MONOSPACED, json.getFont().getSize()));
    json.setCaretPosition(0);
    JPanel content = new JPanel(new BorderLayout(0, JBUI.scale(4)));
    content.add(new JBScrollPane(json), BorderLayout.CENTER);
    JBLabel where = new JBLabel(wrapped(REAL_SUMMARY), UIUtil.ComponentStyle.SMALL);
    where.setForeground(NamedColorUtil.getInactiveTextColor());
    content.add(where, BorderLayout.SOUTH);
    content.setVisible(false);
    myExample = content;
    ActionLink toggle = new ActionLink(EXAMPLE_TITLE);
    toggle.setIcon(AllIcons.General.ArrowRight, false);
    myToggle = toggle;
    toggle.addActionListener(e -> toggleExample());
    JPanel holder = new JPanel(new BorderLayout(0, JBUI.scale(4)));
    holder.add(toggle, BorderLayout.NORTH);
    holder.add(content, BorderLayout.CENTER);
    return holder;
  }

  // grows the dialog to show the example, and gives it back its first size when the example is hidden again
  private void toggleExample() {
    boolean expand = !myExample.isVisible();
    Window window = SwingUtilities.getWindowAncestor(myContent);
    if (expand && window != null) myCollapsedSize = window.getSize();
    myExample.setVisible(expand);
    myToggle.setIcon(expand ? AllIcons.General.ArrowDown : AllIcons.General.ArrowRight, false);
    myContent.revalidate();
    if (!expand && window != null && myCollapsedSize != null) {
      window.setSize(myCollapsedSize);
      window.validate();
    }
    else {
      pack();
    }
  }

  @Override
  public @Nullable JComponent getPreferredFocusedComponent() {
    return getButton(getOKAction());
  }

  // HTML with a fixed width, so that the text wraps into a compact block
  @NotNull
  static String wrapped(@NotNull String text) {
    return "<html><div style='width:" + JBUI.scale(TEXT_WIDTH) + "px'>" + text + "</div></html>";
  }

  @NotNull
  private static JComponent left(@NotNull JComponent component) {
    component.setAlignmentX(Component.LEFT_ALIGNMENT);
    return component;
  }
}
