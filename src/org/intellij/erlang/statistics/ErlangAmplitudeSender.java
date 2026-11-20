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
  // The project's key was accepted only by the US data center; the EU one answered "Invalid API key".
  public static final String URL = "https://api2.amplitude.com/2/httpapi";
  private static final int TIMEOUT_MS = 5_000;

  @Override
  @RequiresBackgroundThread
  public boolean send(@NotNull String json) throws IOException {
    return status(json) == HttpURLConnection.HTTP_OK;
  }

  // the HTTP status of the answer
  @RequiresBackgroundThread
  public int status(@NotNull String json) throws IOException {
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
