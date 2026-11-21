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
import com.intellij.ui.ColorUtil;
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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Arrays;
import java.util.List;

/**
 * A small dialog in the middle of the IDE window that asks whether the plugin may send usage statistics.
 * "Yes, send statistics" is the default button. For the first {@link #GUARD_MS} ms after the window is shown, the
 * buttons are disabled, so that a key press that was already on its way does not answer. "Ask me later", Esc and
 * closing the window all mean later.
 */
public final class ErlangUsageAskDialog extends DialogWrapper {
  // never exactly "Yes" or "No", and no mnemonics: DialogWrapper binds the bare Y and N keys to such buttons
  public static final String TITLE = "Help improve the Erlang plugin";
  public static final String YES = "Yes, send statistics";
  public static final String NO = "No, thanks";
  public static final String LATER = "Ask me later";
  public static final String NEVER_SENT = "It never sends code, file names or paths.";
  public static final String EXAMPLE_TITLE = "Example of the data that is sent";
  public static final String SENT_TOGETHER =
    "Sent together with: the date, a random installation id, the plugin version, the IDE, the OS and the country.";
  public static final String REAL_SUMMARY = "The real summary of today is in Settings | Erlang External Tools.";
  static final int GUARD_MS = 500;
  // own exit codes; the OK exit code, which the hidden OK action may give, means later
  private static final int YES_EXIT_CODE = NEXT_USER_EXIT_CODE;
  private static final int NO_EXIT_CODE = NEXT_USER_EXIT_CODE + 1;
  private static final int TEXT_WIDTH = 440;
  // the text inside the box: without the border, the insets, the icon and its gap
  private static final int BOX_TEXT_WIDTH = TEXT_WIDTH - 50;
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
    public @NotNull ErlangUsageAskUi.Choice ask(@NotNull Project project, boolean reminder, @NotNull Runnable beforeShow) {
      ErlangUsageAskDialog dialog = new ErlangUsageAskDialog(project, reminder);
      // closes the dialog if the project closes; after the dialog closes, nothing keeps it
      Disposer.register(project, dialog.getDisposable());
      beforeShow.run();
      dialog.show();
      return choiceOf(dialog.getExitCode());
    }
  };

  private final boolean myReminder;
  private final Action myYesAction = new GuardedExitAction(YES, YES_EXIT_CODE);
  private final Action myNoAction = new GuardedExitAction(NO, NO_EXIT_CODE);
  private JComponent myContent;
  private JPanel myExampleHolder;
  private JComponent myExample;
  private ActionLink myToggle;
  private JBTextArea myExampleText;
  private Dimension myCollapsedSize;
  private boolean myGuarded = true;
  private Timer myGuardTimer;

  ErlangUsageAskDialog(@Nullable Project project, boolean reminder) {
    super(project, false);
    myReminder = reminder;
    myYesAction.putValue(DEFAULT_ACTION, Boolean.TRUE);
    myYesAction.putValue(FOCUSED_ACTION, Boolean.TRUE);
    setTitle(TITLE);
    setCancelButtonText(LATER);
    setResizable(false);
    init();
    setButtonsEnabled(false);
    Window window = getWindow();
    if (window != null) {
      window.addWindowListener(new WindowAdapter() {
        @Override
        public void windowOpened(WindowEvent e) {
          if (myCollapsedSize == null) myCollapsedSize = e.getWindow().getSize();
          startGuard();
        }

        @Override
        public void windowActivated(WindowEvent e) {
          startGuard();
        }
      });
    }
  }

  // an exit action that does nothing while the guard is on, also when its button was pressed before and released after
  private final class GuardedExitAction extends DialogWrapperExitAction {
    GuardedExitAction(@NotNull String name, int exitCode) {
      super(name, exitCode);
    }

    @Override
    protected void doAction(ActionEvent e) {
      if (!myGuarded) super.doAction(e);
    }
  }

  @NotNull
  static ErlangUsageAskUi.Choice choiceOf(int exitCode) {
    if (exitCode == YES_EXIT_CODE) return ErlangUsageAskUi.Choice.YES;
    if (exitCode == NO_EXIT_CODE) return ErlangUsageAskUi.Choice.NO;
    return ErlangUsageAskUi.Choice.LATER;
  }

  // on macOS the default action goes to the right end; elsewhere this order is kept
  @Override
  protected Action @NotNull [] createActions() {
    return new Action[]{myYesAction, myNoAction};
  }

  // "Ask me later" stands apart, at the left end
  @Override
  protected Action @NotNull [] createLeftSideActions() {
    return new Action[]{getCancelAction()};
  }

  // the OK action is not shown; Ctrl+Enter, which runs it, does nothing
  @Override
  protected void doOKAction() {
  }

  // the guard starts once, when the window is shown or activated; a Swing timer ends it and stops with the dialog
  private void startGuard() {
    if (myGuardTimer != null || isDisposed()) return;
    myGuardTimer = new Timer(GUARD_MS, e -> endGuard());
    myGuardTimer.setRepeats(false);
    Timer timer = myGuardTimer;
    Disposer.register(getDisposable(), timer::stop);
    timer.start();
  }

  private void endGuard() {
    Window window = getWindow();
    endGuard(window != null ? window.getMostRecentFocusOwner() : null);
  }

  // true when the focus goes to Yes
  private boolean endGuard(@Nullable Component focusOwner) {
    if (isDisposed()) return false;
    myGuarded = false;
    setButtonsEnabled(true);
    // a disabled button could not take the focus when the dialog opened; a control of the content that the user moved
    // to keeps it
    boolean toYes = focusOwner == null || focusOwner == myContent || !SwingUtilities.isDescendingFrom(focusOwner, myContent);
    JButton yes = getButton(myYesAction);
    if (toYes && yes != null) yes.requestFocusInWindow();
    return toYes;
  }

  // disabled buttons never arm, so a press during the guard cannot click them; Esc and the close button still work
  private void setButtonsEnabled(boolean enabled) {
    myYesAction.setEnabled(enabled);
    myNoAction.setEnabled(enabled);
    JButton later = getButton(getCancelAction());
    if (later != null) later.setEnabled(enabled);
  }

  @Override
  public @Nullable JComponent getPreferredFocusedComponent() {
    // during the guard the buttons are disabled; the content takes the focus until then
    return myGuarded ? myContent : getButton(myYesAction);
  }

  @Override
  protected @NotNull JComponent createCenterPanel() {
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.setFocusable(true);
    myContent = panel;
    // the question first and large, then who asks and why
    JBLabel question = new JBLabel(wrapped(ErlangUsageStatistics.QUESTION));
    question.setFont(JBUI.Fonts.label().biggerOn(3).asBold());
    panel.add(left(question));
    panel.add(Box.createVerticalStrut(JBUI.scale(6)));
    panel.add(left(new JBLabel(wrapped(myReminder ? ErlangUsageStatistics.REMINDER_TEXT : ErlangUsageStatistics.TEXT))));
    panel.add(Box.createVerticalStrut(JBUI.scale(14)));
    panel.add(left(createPromiseBox()));
    panel.add(Box.createVerticalStrut(JBUI.scale(8)));
    return panel;
  }

  // a rounded, slightly lighter box: what is never sent, the privacy policy and the example
  @NotNull
  private JComponent createPromiseBox() {
    JPanel promise = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
    promise.setOpaque(false);
    promise.add(new JBLabel(NEVER_SENT));
    promise.add(Box.createHorizontalStrut(JBUI.scale(6)));
    promise.add(new ActionLink("Privacy policy", (ActionListener)e -> BrowserUtil.browse(ErlangUsageStatistics.PRIVACY_URL)));
    JPanel lines = new JPanel();
    lines.setOpaque(false);
    lines.setLayout(new BoxLayout(lines, BoxLayout.Y_AXIS));
    lines.add(left(promise));
    lines.add(left((JComponent)Box.createVerticalStrut(JBUI.scale(6))));
    lines.add(left(createExample()));
    JBLabel icon = new JBLabel(AllIcons.General.InspectionsOK);
    icon.setVerticalAlignment(SwingConstants.TOP);
    icon.setBorder(JBUI.Borders.emptyTop(1));
    JPanel box = new JPanel(new BorderLayout(JBUI.scale(10), 0)) {
      @Override
      protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D)g.create();
        try {
          g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
          // the same in a light and in a dark theme: the background of the dialog with a little of the text colour
          g2.setColor(ColorUtil.mix(UIUtil.getPanelBackground(), UIUtil.getLabelForeground(), 0.07));
          int arc = JBUI.scale(12);
          g2.fillRoundRect(0, 0, getWidth(), getHeight(), arc, arc);
        }
        finally {
          g2.dispose();
        }
      }
    };
    box.setOpaque(false);
    box.add(icon, BorderLayout.WEST);
    box.add(lines, BorderLayout.CENTER);
    box.setBorder(JBUI.Borders.empty(10, 12));
    return box;
  }

  @NotNull
  static String exampleText() {
    return ErlangUsageStatisticsPanel.prettyPrint(ErlangUsagePayload.exampleEventProperties()) + "\n\n" + SENT_TOGETHER;
  }

  // a collapsed section with an example summary; the dialog grows when it opens and gets its first size back when it closes
  @NotNull
  private JComponent createExample() {
    myExampleText = new JBTextArea(exampleText(), EXAMPLE_ROWS, 0);
    myExampleText.setEditable(false);
    myExampleText.setLineWrap(true);
    myExampleText.setWrapStyleWord(true);
    myExampleText.setFont(JBUI.Fonts.create(Font.MONOSPACED, myExampleText.getFont().getSize()));
    myExampleText.setCaretPosition(0);
    JPanel content = new JPanel(new BorderLayout(0, JBUI.scale(6)));
    // no frame: the example is a darker field inside the box, with its own margins
    myExampleText.setBackground(UIUtil.getPanelBackground());
    myExampleText.setBorder(JBUI.Borders.empty(6, 8));
    JBScrollPane scroll = new JBScrollPane(myExampleText);
    scroll.setBorder(JBUI.Borders.empty());
    content.add(scroll, BorderLayout.CENTER);
    JBLabel where = new JBLabel(wrapped(REAL_SUMMARY, BOX_TEXT_WIDTH), UIUtil.ComponentStyle.SMALL);
    where.setForeground(NamedColorUtil.getInactiveTextColor());
    content.add(where, BorderLayout.SOUTH);
    content.setBorder(JBUI.Borders.empty(6, 2, 0, 0));
    myExample = content;
    myToggle = new ActionLink(EXAMPLE_TITLE);
    myToggle.setIcon(AllIcons.General.ArrowRight, true);
    myToggle.addActionListener(e -> toggleExample());
    // the example is not in the panel while it is closed, so the closed layout is exactly the first one
    myExampleHolder = new JPanel(new BorderLayout());
    myExampleHolder.setOpaque(false);
    content.setOpaque(false);
    myExampleHolder.add(myToggle, BorderLayout.NORTH);
    return myExampleHolder;
  }

  private void toggleExample() {
    boolean expand = myExample.getParent() == null;
    Window window = SwingUtilities.getWindowAncestor(myContent);
    if (expand && window != null && myCollapsedSize == null) myCollapsedSize = window.getSize();
    if (expand) myExampleHolder.add(myExample, BorderLayout.CENTER);
    else myExampleHolder.remove(myExample);
    myToggle.setIcon(expand ? AllIcons.General.ArrowDown : AllIcons.General.ArrowRight, true);
    // BoxLayout keeps the sizes of its children until its container is invalidated itself, and a container that is
    // already invalid does not pass the call on to its parent
    for (Container c = myExampleHolder; c != null; c = c == myContent ? null : c.getParent()) {
      c.invalidate();
    }
    myContent.revalidate();
    if (window == null) return;
    if (expand) {
      pack();
    }
    else if (myCollapsedSize != null) {
      // the first size, also if the window was given a larger minimum size while the example was open
      Dimension size = new Dimension(myCollapsedSize);
      window.setMinimumSize(size);
      window.setSize(size);
      window.validate();
      // once more after the pending layout, in case it changed the size again
      SwingUtilities.invokeLater(() -> {
        if (myExample.getParent() == null && !isDisposed() && !window.getSize().equals(size)) window.setSize(size);
      });
    }
  }

  // HTML with a fixed width, so that the text wraps into a compact block
  @NotNull
  static String wrapped(@NotNull String text) {
    return wrapped(text, TEXT_WIDTH);
  }

  @NotNull
  private static String wrapped(@NotNull String text, int width) {
    return "<html><div style='width:" + JBUI.scale(width) + "px'>" + text + "</div></html>";
  }

  @NotNull
  private static JComponent left(@NotNull JComponent component) {
    component.setAlignmentX(Component.LEFT_ALIGNMENT);
    return component;
  }

  // tests

  @TestOnly
  @NotNull
  List<Action> getActionsForTests() {
    return List.of(createActions());
  }

  @TestOnly
  @NotNull
  List<String> getMacOrderForTests() {
    List<Action> right = new java.util.ArrayList<>(List.of(createActions()));
    sortActionsOnMac(right);
    List<Action> actions = new java.util.ArrayList<>(List.of(createLeftSideActions()));
    actions.addAll(right);
    return actions.stream().map(action -> (String)action.getValue(Action.NAME)).toList();
  }

  @TestOnly
  @NotNull
  List<String> getButtonTextsForTests() {
    return java.util.stream.Stream.concat(Arrays.stream(createActions()), Arrays.stream(createLeftSideActions())).map(action -> (String)action.getValue(Action.NAME)).toList();
  }

  @TestOnly
  @Nullable
  JButton getButtonForTests(@NotNull Action action) {
    return getButton(action);
  }

  @TestOnly
  void startGuardForTests() {
    startGuard();
  }

  @TestOnly
  boolean isGuardTimerRunningForTests() {
    return myGuardTimer != null && myGuardTimer.isRunning();
  }

  @TestOnly
  void endGuardForTests() {
    endGuard();
  }

  @TestOnly
  boolean endGuardForTests(@Nullable Component focusOwner) {
    return endGuard(focusOwner);
  }

  @TestOnly
  JComponent getContentForTests() {
    return myContent;
  }

  @TestOnly
  JComponent getToggleForTests() {
    return myToggle;
  }

  @TestOnly
  boolean isGuardedForTests() {
    return myGuarded;
  }

  @TestOnly
  void pressYesForTests() {
    myYesAction.actionPerformed(new ActionEvent(myContent, ActionEvent.ACTION_PERFORMED, YES));
  }

  @TestOnly
  void clickNoForTests() {
    myNoAction.actionPerformed(new ActionEvent(myContent, ActionEvent.ACTION_PERFORMED, NO));
  }

  @TestOnly
  boolean isExampleShownForTests() {
    return myExample.getParent() != null;
  }

  @TestOnly
  void toggleExampleForTests() {
    toggleExample();
  }

  @TestOnly
  @NotNull
  String getExampleTextForTests() {
    return myExampleText.getText();
  }

  @TestOnly
  @NotNull
  Dimension getContentPreferredSizeForTests() {
    return myContent.getPreferredSize();
  }
}
