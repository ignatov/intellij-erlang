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

package org.intellij.erlang.runconfig;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ModuleBasedConfiguration;
import com.intellij.execution.configurations.RuntimeConfigurationError;
import com.intellij.execution.configurations.RuntimeConfigurationException;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public abstract class ErlangRunConfigurationBase<RunningState extends ErlangRunningState> extends ModuleBasedConfiguration<ErlangModuleBasedConfiguration>
  implements RunConfigurationWithSuppressedDefaultRunAction {
  private ErlangDebugOptions myDebugOptions = new ErlangDebugOptions();
  private String myWorkDirectory;

  public ErlangRunConfigurationBase(String name, ErlangModuleBasedConfiguration configurationModule, ConfigurationFactory factory) {
    super(name, configurationModule, factory);
  }

  @NotNull
  public ErlangDebugOptions getDebugOptions() {
    return myDebugOptions;
  }

  public void setDebugOptions(@NotNull ErlangDebugOptions debugOptions) {
    myDebugOptions = debugOptions;
  }

  @Nullable
  public String getWorkDirectory() {
    return myWorkDirectory;
  }

  public void setWorkDirectory(@Nullable String workDirectory) {
    myWorkDirectory = workDirectory;
  }

  @Override
  public Collection<Module> getValidModules() {
    Module[] modules = ModuleManager.getInstance(getProject()).getModules();
    return Arrays.asList(modules);
  }

  @Override
  public void writeExternal(Element element) throws WriteExternalException {
    super.writeExternal(element);
    XmlSerializer.serializeInto(this, element);
  }

  @Override
  public void checkConfiguration() throws RuntimeConfigurationException {
    ErlangModuleBasedConfiguration configurationModule = getConfigurationModule();
    configurationModule.checkForWarning();
    checkWorkDirectory();
  }

  private void checkWorkDirectory() throws RuntimeConfigurationError {
    if (StringUtil.isNotEmpty(myWorkDirectory)) {
      File dir = new File(myWorkDirectory);
      try {
        if (!dir.isDirectory()) {
          throw new RuntimeConfigurationError("Incorrect path to working directory.");
        }
      }
      catch (SecurityException e) {
        throw new RuntimeConfigurationError("Access denied to working directory");
      }
    }
  }

  @Override
  public void checkSettingsBeforeRun() {
    ErlangDebuggableRunConfigurationProducer.updateDebugOptions(this);
  }

  @Override
  public void readExternal(Element element) throws InvalidDataException {
    super.readExternal(element);
    XmlSerializer.deserializeInto(this, element);
  }

  @Nullable
  @Override
  public final RunningState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment environment) {
    ErlangModuleBasedConfiguration configuration = getConfigurationModule();
    Module module = configuration.getModule();
    return module != null ? newRunningState(environment, module) : null;
  }

  public abstract boolean isUseTestCodePath();

  protected abstract RunningState newRunningState(ExecutionEnvironment env, Module module);

  public static final class ErlangDebugOptions implements Serializable {
    private boolean myAutoUpdateModulesNotToInterpret = true;
    private Set<String> myModulesNotToInterpret = new HashSet<>();

    public boolean isAutoUpdateModulesNotToInterpret() {
      return myAutoUpdateModulesNotToInterpret;
    }

    public void setAutoUpdateModulesNotToInterpret(boolean autoUpdateModulesNotToInterpret) {
      myAutoUpdateModulesNotToInterpret = autoUpdateModulesNotToInterpret;
    }

    @NotNull
    public Set<String> getModulesNotToInterpret() {
      return myModulesNotToInterpret;
    }

    public void setModulesNotToInterpret(@NotNull Set<String> modulesNotToInterpret) {
      myModulesNotToInterpret = modulesNotToInterpret;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;

      ErlangDebugOptions that = (ErlangDebugOptions) o;

      if (myAutoUpdateModulesNotToInterpret != that.myAutoUpdateModulesNotToInterpret) return false;
      if (!myModulesNotToInterpret.equals(that.myModulesNotToInterpret)) return false;

      return true;
    }

    @Override
    public int hashCode() {
      int result = (myAutoUpdateModulesNotToInterpret ? 1 : 0);
      result = 31 * result + myModulesNotToInterpret.hashCode();
      return result;
    }
  }
}
