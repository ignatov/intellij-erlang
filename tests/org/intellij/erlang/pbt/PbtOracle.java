package org.intellij.erlang.pbt;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * The erlc oracle: one long-lived {@code erl -noshell} process that runs {@code testData/pbt/pbt_oracle.erl}. It is
 * started once per run and asked about one file per line. It is started again only when it dies or does not answer
 * in time; the report counts the restarts.
 */
final class PbtOracle implements AutoCloseable {
  /** The modules whose errors are syntax errors: the scanner, the parser and the preprocessor. */
  static final List<String> SYNTAX_MODULES = List.of("erl_scan", "erl_parse", "epp");

  record Item(String module, int line, int column, String tag, @Nullable String name, String message, boolean error) {
    boolean syntax() {
      return error && SYNTAX_MODULES.contains(module);
    }
  }

  record Result(String status, List<Item> items) {
    boolean hasSyntaxError() {
      for (Item i : items) if (i.syntax()) return true;
      return false;
    }

    @Nullable Item firstSyntaxError() {
      for (Item i : items) if (i.syntax()) return i;
      return null;
    }

    Result without(String tag) {
      List<Item> kept = new ArrayList<>();
      for (Item i : items) if (!i.tag.equals(tag)) kept.add(i);
      return new Result(status, kept);
    }
  }

  private final String myErl;
  private final Path mySource;
  private final long myTimeoutMs;
  private Process myProcess;
  private Writer myIn;
  private BlockingQueue<String> myLines;
  private String myOtpRelease = "unknown";
  int restarts;
  int calls;
  long millis;

  PbtOracle(String erl, Path source, long timeoutMs) throws IOException {
    myErl = erl;
    mySource = source.toAbsolutePath();
    myTimeoutMs = timeoutMs;
    start();
  }

  String otpRelease() {
    return myOtpRelease;
  }

  private void start() throws IOException {
    String src = mySource.toString().replace("\\", "\\\\").replace("\"", "\\\"");
    String eval = "{ok, M, B} = compile:file(\"" + src + "\", [binary, return_errors]), " +
                  "{module, M} = code:load_binary(M, \"" + src + "\", B), pbt_oracle:main().";
    ProcessBuilder pb = new ProcessBuilder(myErl, "-noshell", "-eval", eval);
    pb.redirectError(ProcessBuilder.Redirect.DISCARD);
    myProcess = pb.start();
    myIn = new OutputStreamWriter(myProcess.getOutputStream(), StandardCharsets.UTF_8);
    BlockingQueue<String> lines = new LinkedBlockingQueue<>();
    myLines = lines;
    Process process = myProcess;
    Thread reader = new Thread(() -> {
      try (BufferedReader r = new BufferedReader(new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
        String line;
        while ((line = r.readLine()) != null) lines.add(line);
      }
      catch (IOException ignored) {
      }
      lines.add("");
    }, "pbt-oracle-reader");
    reader.setDaemon(true);
    reader.start();
    try {
      String hello = poll(60_000);
      if (hello == null || hello.isEmpty()) throw new IOException("the oracle did not start: " + myErl);
      JsonObject o = JsonParser.parseString(hello).getAsJsonObject();
      myOtpRelease = o.get("otp").getAsString();
    }
    catch (IOException | RuntimeException e) {
      close();
      throw e;
    }
  }

  private @Nullable String poll(long ms) {
    try {
      return myLines.poll(ms, TimeUnit.MILLISECONDS);
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return null;
    }
  }

  /** Writes the text to {@code file} and asks erlc about it. */
  Result check(Path file, String text) throws IOException {
    Files.writeString(file, text, StandardCharsets.UTF_8);
    long start = System.currentTimeMillis();
    try {
      for (int attempt = 0; attempt < 2; attempt++) {
        try {
          myIn.write(file.toAbsolutePath() + "\n");
          myIn.flush();
          String line = poll(myTimeoutMs);
          if (line != null && !line.isEmpty()) {
            Result result = parse(line);
            // a crash of the oracle is not an answer of erlc: ask again on a fresh process
            if (!result.status().equals("crash")) {
              calls++;
              return result;
            }
          }
        }
        catch (IOException ignored) {
        }
        restart();
      }
      throw new IOException("the oracle gave no answer twice for " + file);
    }
    finally {
      millis += System.currentTimeMillis() - start;
    }
  }

  private void restart() throws IOException {
    restarts++;
    close();
    start();
  }

  static Result parse(String line) {
    JsonObject o = JsonParser.parseString(line).getAsJsonObject();
    List<Item> items = new ArrayList<>();
    add(items, o.getAsJsonArray("errors"), true);
    add(items, o.getAsJsonArray("warnings"), false);
    return new Result(o.get("status").getAsString(), items);
  }

  private static void add(List<Item> out, JsonArray array, boolean error) {
    if (array == null) return;
    for (JsonElement e : array) {
      JsonObject i = e.getAsJsonObject();
      JsonElement name = i.get("name");
      out.add(new Item(i.get("module").getAsString(), i.get("line").getAsInt(), i.get("column").getAsInt(),
                       i.get("tag").getAsString(), name == null ? null : name.getAsString(), i.get("message").getAsString(), error));
    }
  }

  @Override
  public void close() {
    if (myProcess == null) return;
    try {
      myIn.close();
    }
    catch (IOException ignored) {
    }
    try {
      if (!myProcess.waitFor(2, TimeUnit.SECONDS)) myProcess.destroyForcibly();
    }
    catch (InterruptedException e) {
      myProcess.destroyForcibly();
      Thread.currentThread().interrupt();
    }
    myProcess = null;
  }
}
