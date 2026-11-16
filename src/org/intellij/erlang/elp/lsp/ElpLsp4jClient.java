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

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.platform.lsp.api.Lsp4jClient;
import com.intellij.platform.lsp.api.LspServerNotificationsHandler;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

/**
 * The client side of ELP's connection: receives ELP's own {@code elp/status} notification, and watches the loading progress and
 * the messages for {@link ElpLoadState}. The platform registers the JSON-RPC methods of this class next to the standard ones.
 * <p>
 * ELP's messages: an Info or Log {@code window/showMessage} ("ELP version: ..." at every start) goes to the IDE log instead of a
 * balloon; an Error one goes to {@link ElpLoadState} and is shown by the plugin once per session, see
 * {@link ElpLspClientDescriptor}; a Warning is left to the platform.
 */
final class ElpLsp4jClient extends Lsp4jClient {
  private static final Logger LOG = Logger.getInstance(ElpLsp4jClient.class);

  /** The parameters of {@code elp/status}: {@code Loading}, {@code Running} or {@code ShuttingDown}. */
  static final class StatusParams {
    String status;
  }

  private final ElpLoadState myLoadState;

  ElpLsp4jClient(@NotNull LspServerNotificationsHandler handler, @NotNull ElpLoadState loadState) {
    super(intercept(handler, loadState));
    myLoadState = loadState;
  }

  @JsonNotification("elp/status")
  public void elpStatus(@Nullable StatusParams params) {
    String status = params != null ? params.status : null;
    LOG.info("ELP status: " + status);
    myLoadState.status(status, System.currentTimeMillis());
  }

  /**
   * @return a handler that passes everything to {@code handler} except the messages that the plugin handles itself
   */
  @NotNull
  static LspServerNotificationsHandler intercept(@NotNull LspServerNotificationsHandler handler, @NotNull ElpLoadState loadState) {
    return (LspServerNotificationsHandler)Proxy.newProxyInstance(
      LspServerNotificationsHandler.class.getClassLoader(), new Class<?>[]{LspServerNotificationsHandler.class}, (proxy, method, args) -> {
        Object argument = args != null && args.length == 1 ? args[0] : null;
        switch (method.getName()) {
          case "showMessage" -> {
            if (argument instanceof MessageParams params && !passShowMessage(params, loadState)) return null;
          }
          case "logMessage" -> {
            if (argument instanceof MessageParams params) loadState.message(typeOf(params), String.valueOf(params.getMessage()), false);
          }
          case "notifyProgress" -> {
            if (argument instanceof ProgressParams params) progress(params, loadState);
          }
          default -> {
          }
        }
        try {
          return method.invoke(handler, args);
        }
        catch (InvocationTargetException e) {
          throw e.getCause();
        }
      });
  }

  /**
   * @return whether the platform shows the message as it does by default
   */
  static boolean passShowMessage(@NotNull MessageParams params, @NotNull ElpLoadState loadState) {
    int type = typeOf(params);
    String message = String.valueOf(params.getMessage());
    if (type == 2) return true;
    loadState.message(type, message, true);
    if (type != 1) LOG.info("ELP: " + message);
    return false;
  }

  private static void progress(@NotNull ProgressParams params, @NotNull ElpLoadState loadState) {
    if (params.getToken() == null || params.getValue() == null) return;
    String token = String.valueOf(params.getToken().get());
    WorkDoneProgressNotification notification = params.getValue().isLeft() ? params.getValue().getLeft() : null;
    String kind = notification != null && notification.getKind() != null ? notification.getKind().name() : null;
    loadState.progress(token, kind, System.currentTimeMillis());
  }

  private static int typeOf(@NotNull MessageParams params) {
    MessageType type = params.getType();
    return type != null ? type.getValue() : MessageType.Log.getValue();
  }
}
