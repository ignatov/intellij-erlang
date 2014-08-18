/*
 * Copyright 2012-2014 Sergey Ignatov
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
package org.intellij.erlang.jps.execution;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.execution.ParametersListUtil;
import gnu.trove.THashMap;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ParametersList implements Cloneable {
  private static final Logger LOG = Logger.getInstance("#com.intellij.execution.configurations.ParametersList");

  private static final Pattern PROPERTY_PATTERN = Pattern.compile("-D(\\S+?)=(.+)");

  private List<String> myParameters = new ArrayList<String>();
  private List<ParamsGroup> myGroups = new ArrayList<ParamsGroup>();

  public boolean hasParameter(@NonNls String param) {
    return myParameters.contains(param);
  }

  public boolean hasProperty(@NonNls String name) {
    return getPropertyValue(name) != null;
  }

  @Nullable
  public String getPropertyValue(@NotNull @NonNls String name) {
    String prefix = "-D" + name + "=";
    for (String parameter : myParameters) {
      if (parameter.startsWith(prefix)) {
        return parameter.substring(prefix.length());
      }
    }
    return null;
  }

  @NotNull
  public Map<String, String> getProperties() {
    Map<String, String> result = new THashMap<String, String>();
    for (String parameter : myParameters) {
      Matcher matcher = PROPERTY_PATTERN.matcher(parameter);
      if (matcher.matches()) {
        result.put(matcher.group(1), matcher.group(2));
      }
    }
    return result;
  }

  @NotNull
  public String getParametersString() {
    return join(getList());
  }

  @NotNull
  public String[] getArray() {
    return ArrayUtil.toStringArray(getList());
  }

  @NotNull
  public List<String> getList() {
    if (myGroups.isEmpty()) {
      return Collections.unmodifiableList(myParameters);
    }

    List<String> params = new ArrayList<String>();
    params.addAll(myParameters);
    for (ParamsGroup group : myGroups) {
      params.addAll(group.getParameters());
    }
    return Collections.unmodifiableList(params);
  }

  public void clearAll() {
    myParameters.clear();
    myGroups.clear();
  }

  public void prepend(@NonNls String parameter) {
    addAt(0, parameter);
  }

  public void prependAll(@NonNls String... parameter) {
    addAll(parameter);
    Collections.rotate(myParameters, parameter.length);
  }

  public void addParametersString(String parameters) {
    if (parameters != null) {
      String[] split = parse(parameters);
      for (String param : split) {
        add(param);
      }
    }
  }

  public void add(@NonNls String parameter) {
    myParameters.add(expandMacros(parameter));
  }

  public ParamsGroup addParamsGroup(@NotNull String groupId) {
    return addParamsGroup(new ParamsGroup(groupId));
  }

  public ParamsGroup addParamsGroup(@NotNull ParamsGroup group) {
    myGroups.add(group);
    return group;
  }

  public ParamsGroup addParamsGroupAt(int index, @NotNull ParamsGroup group) {
    myGroups.add(index, group);
    return group;
  }

  public ParamsGroup addParamsGroupAt(int index, @NotNull String groupId) {
    ParamsGroup group = new ParamsGroup(groupId);
    myGroups.add(index, group);
    return group;
  }

  public int getParamsGroupsCount() {
    return myGroups.size();
  }

  public List<String> getParameters() {
    return Collections.unmodifiableList(myParameters);
  }

  public List<ParamsGroup> getParamsGroups() {
    return Collections.unmodifiableList(myGroups);
  }

  public ParamsGroup getParamsGroupAt(int index) {
    return myGroups.get(index);
  }

  @Nullable
  public ParamsGroup getParamsGroup(@NotNull String name) {
    for (ParamsGroup group : myGroups) {
      if (name.equals(group.getId())) return group;
    }
    return null;
  }

  public ParamsGroup removeParamsGroup(int index) {
    return myGroups.remove(index);
  }

  public void addAt(int index, @NotNull String parameter) {
    myParameters.add(index, expandMacros(parameter));
  }

  public void defineProperty(@NonNls String propertyName, @NonNls String propertyValue) {
    addProperty(propertyName, propertyValue);
  }

  public void addProperty(@NonNls String propertyName) {
    myParameters.add("-D" + propertyName);
  }

  public void addProperty(@NonNls String propertyName, @NonNls String propertyValue) {
    myParameters.add("-D" + propertyName + "=" + propertyValue);
  }

  public void replaceOrAppend(@NonNls String parameterPrefix, @NonNls String replacement) {
    replaceOrAdd(parameterPrefix, replacement, myParameters.size());
  }

  private void replaceOrAdd(@NonNls String parameterPrefix, @NonNls String replacement, int position) {
    for (ListIterator<String> iterator = myParameters.listIterator(); iterator.hasNext(); ) {
      String param = iterator.next();
      if (param.startsWith(parameterPrefix)) {
        if (replacement != null && replacement.isEmpty()) {
          iterator.remove();
        }
        else {
          iterator.set(replacement);
        }
        return;
      }
    }
    if (replacement != null && !replacement.isEmpty()) {
      myParameters.add(position, replacement);
    }
  }

  public void replaceOrPrepend(@NonNls String parameter, @NonNls String replacement) {
    replaceOrAdd(parameter, replacement, 0);
  }

  public void set(int ind, @NonNls String value) {
    myParameters.set(ind, value);
  }

  public String get(int ind) {
    return myParameters.get(ind);
  }

  public void add(@NonNls String name, @NonNls String value) {
    add(name);
    add(value);
  }

  public void addAll(String... parameters) {
    addAll(Arrays.asList(parameters));
  }

  public void addAll(List<String> parameters) {
    // Don't use myParameters.addAll(parameters) , it does not call expandMacros(parameter)
    for (String parameter : parameters) {
      add(parameter);
    }
  }

  @Override
  public ParametersList clone() {
    try {
      ParametersList clone = (ParametersList)super.clone();
      clone.myParameters = new ArrayList<String>(myParameters);
      clone.myGroups = new ArrayList<ParamsGroup>(myGroups.size() + 1);
      for (ParamsGroup group : myGroups) {
        clone.myGroups.add(group.clone());
      }
      return clone;
    }
    catch (CloneNotSupportedException e) {
      LOG.error(e);
      return null;
    }
  }

  /**
   * @see com.intellij.util.execution.ParametersListUtil#join(java.util.List)
   */
  @NotNull
  public static String join(@NotNull List<String> parameters) {
    return ParametersListUtil.join(parameters);
  }

  /**
   * @see com.intellij.util.execution.ParametersListUtil#join(java.util.List)
   */
  @NotNull
  public static String join(String... parameters) {
    return ParametersListUtil.join(parameters);
  }

  /**
   * @see com.intellij.util.execution.ParametersListUtil#parseToArray(String)
   */
  @NotNull
  public static String[] parse(@NotNull String string) {
    return ParametersListUtil.parseToArray(string);
  }

  public String expandMacros(String text) {
    Map<String, String> macroMap = getMacroMap();
    Set<String> set = macroMap.keySet();
    for (String from : set) {
      String to = macroMap.get(from);
      text = StringUtil.replace(text, from, to, true);
    }
    return text;
  }

  private Map<String, String> getMacroMap() {
    return ContainerUtil.newLinkedHashMap();
  }

  @Override
  public String toString() {
    return myParameters.toString();
  }

}
