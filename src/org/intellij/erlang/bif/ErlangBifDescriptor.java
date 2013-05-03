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

package org.intellij.erlang.bif;

import org.jetbrains.annotations.NotNull;

public final class ErlangBifDescriptor implements Comparable<ErlangBifDescriptor> {
  @NotNull private final String myModule;
  @NotNull private final String myName;
  private final int myArity;
  @NotNull private final String myParams;

  public ErlangBifDescriptor(@NotNull String module, @NotNull String name, int arity, @NotNull String params) {
    myModule = module;
    myName = name;
    myArity = arity;
    myParams = params;
  }

  @NotNull
  public String getModule() {
    return myModule;
  }

  @NotNull
  public String getName() {
    return myName;
  }

  public int getArity() {
    return myArity;
  }

  @NotNull
  public String getParams() {
    return myParams;
  }

  @Override
  public String toString() {
    return myModule + ":" + myName + "/" + myArity;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    ErlangBifDescriptor that = (ErlangBifDescriptor) o;

    if (myArity != that.myArity) return false;
    if (!myModule.equals(that.myModule)) return false;
    if (!myName.equals(that.myName)) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = myModule.hashCode();
    result = 31 * result + myName.hashCode();
    result = 31 * result + myArity;
    return result;
  }

  @Override
  public int compareTo(@NotNull ErlangBifDescriptor that) {
    int result = myModule.compareTo(that.myModule);
    if (result == 0) {
      result = myName.compareTo(that.myName);
      if (result == 0) {
        result = Integer.signum(myArity - that.myArity);
      }
    }
    return result;
  }
}
