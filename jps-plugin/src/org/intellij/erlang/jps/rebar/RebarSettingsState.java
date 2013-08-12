package org.intellij.erlang.jps.rebar;

import com.intellij.util.xmlb.annotations.Tag;
import org.jetbrains.annotations.NotNull;

/**
 * @author savenko
 */
public class RebarSettingsState {
  public RebarSettingsState() {
    myRebarPath = "";
  }

  public RebarSettingsState(RebarSettingsState state) {
    myRebarPath = new String(state.myRebarPath);
  }

  @Tag("rebarPath")
  @NotNull
  public String myRebarPath;

  @Override
  public String toString() {
    return "RebarSettingsState(rebarPath='" + myRebarPath + "')";
  }
}
