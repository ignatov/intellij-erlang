/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.ide.fileTemplates.DefaultCreateFromTemplateHandler;
import com.intellij.ide.fileTemplates.FileTemplate;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.Map;

public class ErlangCreateFromTemplateHandler extends DefaultCreateFromTemplateHandler {
  @Override
  public boolean handlesTemplate(FileTemplate template) {
    return template.isTemplateOfType(ErlangFileType.MODULE) ||
      template.isTemplateOfType(ErlangFileType.HEADER) ||
      template.isTemplateOfType(ErlangFileType.TERMS) ||
      template.isTemplateOfType(ErlangFileType.APP);
  }

  @Override
  public void prepareProperties(Map<String, Object> props) {
    String name = String.valueOf(props.get(FileTemplate.ATTRIBUTE_NAME));
    String nameAtom = ObjectUtils.notNull(ErlangPsiImplUtil.toAtomName(name), name);
    props.put(FileTemplate.ATTRIBUTE_NAME + "_ATOM", nameAtom);
  }
}
