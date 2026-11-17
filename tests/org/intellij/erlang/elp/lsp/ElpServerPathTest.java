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

package org.intellij.erlang.elp.lsp;

import com.intellij.execution.ExecutionException;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpMode;

/**
 * A path to {@code elp} that does not exist is stored as typed; starting the server then fails with an error that the platform's
 * LSP client reports, rather than hanging, and the client never runs, so the plugin keeps answering (see {@code ElpEngineSwitchTest}).
 */
public class ElpServerPathTest extends ElpRecordedProjectTestBase {
  public void testMissingBinaryFailsToStart() {
    ElpLspClientDescriptor descriptor =
      new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration("/no/such/dir/elp", ElpMode.ELP));
    try {
      descriptor.startServerProcess();
      fail("a server started from a path that does not exist");
    }
    catch (ExecutionException e) {
      assertTrue(e.getMessage(), e.getMessage().contains("/no/such/dir/elp"));
    }
  }
}
