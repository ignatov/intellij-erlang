/*
 * Copyright 2012-2023 Sergey Ignatov
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

package org.intellij.erlang.types;

import com.intellij.lang.ParserDefinition;
import org.intellij.erlang.parser.ErlangParserTestBase;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.junit.jupiter.api.Test;

class ErlTypeFactoryTest extends ErlangParserTestBase {
  protected ErlTypeFactoryTest(String dataPath, String fileExt, ParserDefinition... definitions) {
    super(dataPath, fileExt, definitions);
  }

  @Test
  void fromVariable() {
    var qvar = ErlangElementFactory.createQVarFromText(this.getProject(), "A");
    var type = ErlTypeFactory.fromVariable((ErlangQVar) qvar);
  }
}