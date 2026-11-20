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

package org.intellij.erlang.statistics;

import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.io.HttpRequests;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URLConnection;

/**
 * Sends the events to the HTTP V2 API of Amplitude with the IDE's HTTP client, so the IDE proxy settings apply.
 */
public final class ErlangAmplitudeSender implements ErlangUsageSender {
  // The data center and the key of the Amplitude project; change both together. The key is public by Amplitude's rules.
  // The project is in the EU data center; a key of a US project needs https://api2.amplitude.com/2/httpapi instead.
  public static final String URL = "https://api.eu.amplitude.com/2/httpapi";
  public static final String API_KEY = "cf7d5b9ddda8fc71a93ba88fcea2372d";
  private static final int TIMEOUT_MS = 5_000;

  @Override
  @RequiresBackgroundThread
  public int send(@NotNull String json) throws IOException {
    return HttpRequests.post(URL, "application/json")
      .accept("*/*")
      .connectTimeout(TIMEOUT_MS)
      .readTimeout(TIMEOUT_MS)
      .throwStatusCodeException(false)
      .connect(request -> {
        request.write(json);
        URLConnection connection = request.getConnection();
        return connection instanceof HttpURLConnection http ? http.getResponseCode() : -1;
      });
  }
}
