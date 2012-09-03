/*
 * Copyright 2012 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
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
package org.intellij.erlang.template;

import com.intellij.codeInsight.template.impl.DefaultLiveTemplatesProvider;
import org.apache.commons.lang.ArrayUtils;

/**
 * @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
 */
public class ErlangLiveTemplateProvider implements DefaultLiveTemplatesProvider {
  public static final String[] TEMPLATE_FILES = {"liveTemplates/erlang"};

  @Override
  public String[] getDefaultLiveTemplateFiles() {
    return TEMPLATE_FILES;
  }

  @Override
  public String[] getHiddenLiveTemplateFiles() {
    return ArrayUtils.EMPTY_STRING_ARRAY;
  }
}
