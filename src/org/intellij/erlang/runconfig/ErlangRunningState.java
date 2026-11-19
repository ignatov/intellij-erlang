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

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.ObjectUtils;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.parser.ErlangLexer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import static org.intellij.erlang.ErlangTypes.ERL_BIN_END;
import static org.intellij.erlang.ErlangTypes.ERL_BIN_START;
import static org.intellij.erlang.ErlangTypes.ERL_BRACKET_LEFT;
import static org.intellij.erlang.ErlangTypes.ERL_BRACKET_RIGHT;
import static org.intellij.erlang.ErlangTypes.ERL_CURLY_LEFT;
import static org.intellij.erlang.ErlangTypes.ERL_CURLY_RIGHT;
import static org.intellij.erlang.ErlangTypes.ERL_PAR_LEFT;
import static org.intellij.erlang.ErlangTypes.ERL_PAR_RIGHT;

public abstract class ErlangRunningState extends CommandLineState {
  private final Module myModule;

  protected ErlangRunningState(ExecutionEnvironment env, Module module) {
    super(env);
    myModule = module;
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    GeneralCommandLine commandLine = getCommand();
    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }

  @VisibleForTesting
  GeneralCommandLine getCommand() throws ExecutionException {
    GeneralCommandLine commandLine = new GeneralCommandLine();
    setExePath(commandLine);
    setWorkDirectory(commandLine);
    setEnvironment(commandLine);
    setCodePath(commandLine);
    setErlangFlags(commandLine);
    setEntryPoint(commandLine);
    setStopErlang(commandLine);
    setNoShellMode(commandLine);
    setExtraArguments(commandLine);
    TextConsoleBuilder consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(myModule.getProject());
    setConsoleBuilder(consoleBuilder);
    return commandLine;
  }

  protected List<String> getCodePath() throws ExecutionException {
    return ErlangConsoleUtil.getCodePath(myModule, useTestCodePath());
  }

  public Module getModule() {
    return myModule;
  }

  protected abstract boolean useTestCodePath();

  protected abstract boolean isNoShellMode();

  protected abstract boolean isStopErlang();

  protected List<String> getErlFlags() {
    return ContainerUtil.emptyList();
  }

  protected abstract ErlangEntryPoint getEntryPoint() throws ExecutionException;

  @Nullable
  protected abstract String getWorkDirectory();

  @NotNull
  protected abstract EnvironmentVariablesData getEnvData();

  public ErlangEntryPoint getDebugEntryPoint() throws ExecutionException {
    return getEntryPoint();
  }

  public final void setCodePath(GeneralCommandLine commandLine) throws ExecutionException {
    commandLine.addParameters(getCodePath());
  }

  public final void setExePath(GeneralCommandLine commandLine) throws ExecutionException {
    Sdk sdk = ModuleRootManager.getInstance(myModule).getSdk();
    String homePath = sdk != null ? sdk.getHomePath() : null;
    if (homePath == null) {
      throw new ExecutionException("Invalid module SDK.");
    }
    commandLine.setExePath(JpsErlangSdkType.getByteCodeInterpreterExecutable(homePath).getAbsolutePath());
  }

  public final void setWorkDirectory(GeneralCommandLine commandLine) {
    String workDirectory = getWorkDirectory();
    commandLine.withWorkDirectory(StringUtil.isEmpty(workDirectory) ? ObjectUtils.notNull(myModule.getProject().getBasePath(), "") : workDirectory);
  }

  public final void setEnvironment(GeneralCommandLine commandLine) {
    getEnvData().configureCommandLine(commandLine, true);
  }

  public final void setStopErlang(GeneralCommandLine commandLine) {
    if (isStopErlang()) {
      commandLine.addParameters("-s", "init", "stop");
    }
  }

  public final void setNoShellMode(GeneralCommandLine commandLine) {
    if (isNoShellMode()) commandLine.addParameters("-noshell");
  }

  private void setEntryPoint(GeneralCommandLine commandLine) throws ExecutionException {
    ErlangEntryPoint entryPoint = getEntryPoint();
    commandLine.addParameters("-eval",
      toAtom(entryPoint.getModuleName()) + ":" + toAtom(entryPoint.getFunctionName()) +
        "(" + StringUtil.join(entryPoint.getArgsList(), ", ") + ").");
  }

  private static final Pattern PLAIN_ATOM = Pattern.compile("[a-z][a-zA-Z_@0-9]*");
  private static final Set<String> RESERVED_WORDS = Set.of(
    "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond", "div",
    "else", "end", "fun", "if", "let", "maybe", "not", "of", "or", "orelse", "receive", "rem", "try", "when", "xor");

  // the atom with the given name as Erlang source: quoted and escaped unless it is a plain atom
  @NotNull
  public static String toAtom(@NotNull String name) {
    if (PLAIN_ATOM.matcher(name).matches() && !RESERVED_WORDS.contains(name)) return name;
    StringBuilder sb = new StringBuilder("'");
    for (int i = 0; i < name.length(); i++) {
      char c = name.charAt(i);
      switch (c) {
        case '\\' -> sb.append("\\\\");
        case '\'' -> sb.append("\\'");
        case '\n' -> sb.append("\\n");
        case '\r' -> sb.append("\\r");
        case '\t' -> sb.append("\\t");
        default -> {
          if (c < ' ' || c == 127) sb.append("\\x{").append(Integer.toHexString(c)).append('}');
          else sb.append(c);
        }
      }
    }
    return sb.append('\'').toString();
  }

  // the name of the atom written as the given Erlang source, plain or in quotes with escapes
  @NotNull
  public static String atomName(@NotNull String atom) {
    if (atom.length() < 2 || !atom.startsWith("'") || !atom.endsWith("'")) return atom;
    return unescape(atom.substring(1, atom.length() - 1));
  }

  @NotNull
  private static String unescape(@NotNull String text) {
    StringBuilder sb = new StringBuilder();
    int length = text.length();
    int i = 0;
    while (i < length) {
      char c = text.charAt(i++);
      if (c != '\\' || i >= length) {
        sb.append(c);
        continue;
      }
      char e = text.charAt(i++);
      switch (e) {
        case 'b' -> sb.append('\b');
        case 'd' -> sb.append((char) 127);
        case 'e' -> sb.append((char) 27);
        case 'f' -> sb.append('\f');
        case 'n' -> sb.append('\n');
        case 'r' -> sb.append('\r');
        case 's' -> sb.append(' ');
        case 't' -> sb.append('\t');
        case 'v' -> sb.append((char) 11);
        case '^' -> {
          if (i < length) sb.append((char) (text.charAt(i++) & 31));
        }
        case 'x' -> {
          int end;
          int start;
          if (i < length && text.charAt(i) == '{') {
            start = i + 1;
            end = text.indexOf('}', start);
            if (end < 0) end = length;
            i = Math.min(end + 1, length);
          }
          else {
            start = i;
            end = Math.min(i + 2, length);
            i = end;
          }
          try {
            sb.appendCodePoint(Integer.parseInt(text.substring(start, end), 16));
          }
          catch (IllegalArgumentException ignored) {
            sb.append(text, start, end);
          }
        }
        default -> {
          if (e >= '0' && e <= '7') {
            int code = e - '0';
            for (int n = 0; n < 2 && i < length && text.charAt(i) >= '0' && text.charAt(i) <= '7'; n++) {
              code = code * 8 + text.charAt(i++) - '0';
            }
            sb.append((char) code);
          }
          else {
            sb.append(e);
          }
        }
      }
    }
    return sb.toString();
  }

  public final void setErlangFlags(GeneralCommandLine commandLine) {
    List<String> flags = getErlFlags();
    int extra = flags.indexOf("-extra");
    commandLine.addParameters(extra < 0 ? flags : flags.subList(0, extra));
  }

  // everything after -extra is plain arguments, so it must stay at the end of the command line
  public final void setExtraArguments(GeneralCommandLine commandLine) {
    List<String> flags = getErlFlags();
    int extra = flags.indexOf("-extra");
    if (extra >= 0) commandLine.addParameters(flags.subList(extra, flags.size()));
  }

  @NotNull
  public abstract ConsoleView createConsoleView(Executor executor);

  public static class ErlangEntryPoint {
    private final String myModuleName;
    private final String myFunctionName;
    private final List<String> myArgsList;

    public ErlangEntryPoint(String moduleName, String functionName, List<String> args) {
      myModuleName = moduleName;
      myFunctionName = functionName;
      myArgsList = args;
    }

    public String getModuleName() {
      return myModuleName;
    }

    public String getFunctionName() {
      return myFunctionName;
    }

    public List<String> getArgsList() {
      return myArgsList;
    }

    @Nullable
    public static ErlangEntryPoint fromModuleAndFunction(@NotNull String moduleAndFunction, @NotNull String params) {
      List<String> split = splitAtoms(moduleAndFunction);
      if (split == null || split.size() != 2) return null;
      String module = split.get(0);
      String function = split.get(1);

      List<String> args = parseArguments(params);
      
      return new ErlangEntryPoint(module, function, args);
    }

    // the names are atoms, plain or in quotes; a quoted one may contain spaces and is returned without the quotes and escapes
    @Nullable
    private static List<String> splitAtoms(@NotNull String moduleAndFunction) {
      List<String> atoms = new SmartList<>();
      int length = moduleAndFunction.length();
      int i = 0;
      while (i < length) {
        char c = moduleAndFunction.charAt(i);
        if (Character.isWhitespace(c)) {
          i++;
        }
        else if (c == '\'') {
          int start = ++i;
          while (i < length && moduleAndFunction.charAt(i) != '\'') {
            i += moduleAndFunction.charAt(i) == '\\' ? 2 : 1;
          }
          if (i >= length) return null;
          atoms.add(unescape(moduleAndFunction.substring(start, i++)));
          if (i < length && !Character.isWhitespace(moduleAndFunction.charAt(i))) return null;
        }
        else {
          int start = i;
          while (i < length && !Character.isWhitespace(moduleAndFunction.charAt(i))) i++;
          atoms.add(moduleAndFunction.substring(start, i));
        }
      }
      return atoms;
    }

    @NotNull
    private static List<String> parseArguments(@NotNull String params) {
      List<String> args = new SmartList<>();
      Lexer lexer = new ErlangLexer(null);
      lexer.start(params);

      int argumentStart = -1;
      int nestingLevel = 0;
      while (lexer.getTokenType() != null) {
        IElementType tokenType = lexer.getTokenType();
        if (tokenType == TokenType.WHITE_SPACE && nestingLevel == 0) {
          if (argumentStart >= 0) {
            args.add(params.substring(argumentStart, lexer.getTokenStart()));
            argumentStart = -1;
          }
        }
        else {
          if (argumentStart < 0) {
            argumentStart = lexer.getTokenStart();
          }
          if (isOpeningDelimiter(tokenType)) {
            nestingLevel++;
          }
          else if (isClosingDelimiter(tokenType) && nestingLevel > 0) {
            nestingLevel--;
          }
        }
        lexer.advance();
      }

      if (argumentStart >= 0) {
        args.add(params.substring(argumentStart));
      }
      return args;
    }

    private static boolean isOpeningDelimiter(@NotNull IElementType tokenType) {
      return tokenType == ERL_PAR_LEFT || tokenType == ERL_CURLY_LEFT ||
             tokenType == ERL_BRACKET_LEFT || tokenType == ERL_BIN_START;
    }

    private static boolean isClosingDelimiter(@NotNull IElementType tokenType) {
      return tokenType == ERL_PAR_RIGHT || tokenType == ERL_CURLY_RIGHT ||
             tokenType == ERL_BRACKET_RIGHT || tokenType == ERL_BIN_END;
    }
  }
}
