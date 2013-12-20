/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.console;

import com.intellij.openapi.actionSystem.ActionPromoter;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.actions.EnterAction;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class ErlangConsoleActionPromoter implements ActionPromoter {
  private static final Comparator<AnAction> COMPARATOR = new Comparator<AnAction>() {
    @Override
    public int compare(@NotNull AnAction o1, @NotNull AnAction o2) {
      return notEnter(o1) ? notEnter(o2) ? 0 : -1 : notEnter(o2) ? 1 : 0;
    }

    private boolean notEnter(AnAction o) {
      return !(o instanceof EnterAction);
    }
  };

  @Override
  public List<AnAction> promote(List<AnAction> actions, DataContext context) {
    ArrayList<AnAction> result = ContainerUtil.newArrayList(actions);
    ContainerUtil.sort(result, COMPARATOR);
    return result;
  }
}
