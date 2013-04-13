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

package org.intellij.erlang.utils;

import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.*;

public final class ExtProcessUtil {
  private ExtProcessUtil() {
  }

  @NotNull
  public static String restrictedTimeExec(@NotNull String cmd, int timeout) {
    try {
      final Process cmdRunner = Runtime.getRuntime().exec(cmd);
      ExecutorService singleTreadExecutor = Executors.newSingleThreadExecutor();
      Future<String> cmdRunnerFuture = singleTreadExecutor.submit(new Callable<String>() {
        @Override
        public String call() throws Exception {
          cmdRunner.waitFor();
          BufferedReader outReader = new BufferedReader(new InputStreamReader(cmdRunner.getInputStream()));
          try {
            String firstLine = outReader.readLine();
            return firstLine == null ? "" : firstLine;
          } finally {
            outReader.close();
          }
        }
      });
      try {
        return cmdRunnerFuture.get(timeout, TimeUnit.MILLISECONDS);
      } catch (Exception e) { // Suppress
      }
      cmdRunnerFuture.cancel(true);
      singleTreadExecutor.shutdown();
    } catch (IOException e) { // Suppress
    }
    return "";
  }
}
