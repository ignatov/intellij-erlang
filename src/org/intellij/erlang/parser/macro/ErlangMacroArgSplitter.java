package org.intellij.erlang.parser.macro;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayDeque;

import static org.intellij.erlang.ErlangTypes.*;

/**
 * The arguments of a macro use, split token by token as epp's {@code macro_arg/3} does (OTP 29.1): after the {@code (}
 * of the use, a {@code ,} or a {@code )} ends an argument only when nothing is open. {@code ( [ { <<} open until their
 * own closer; {@code begin if case receive try maybe cond} and {@code fun Name (} open until {@code end}. A
 * {@code fun (} may be a fun with clauses or a fun type: it opens until {@code end} once {@code ->} or {@code when}
 * follows its parameter list, and is closed by the next {@code ,} or {@code )} before that ({@code fun()},
 * {@code fun((a) -> b)}). A closer that is not the expected one is part of the argument; strings, characters and
 * quoted atoms are single tokens, comments are not tokens.
 * <p>
 * Feed the tokens after the {@code (} one by one, without whitespace and comments.
 */
public final class ErlangMacroArgSplitter {
  /** What a token is for the argument list. */
  public enum Step {
    /** A token of the current argument. */
    ARGUMENT,
    /** A {@code ,} that ends the current argument. */
    SEPARATOR,
    /** The {@code )} that ends the argument list. */
    CLOSE
  }

  private static final Object FUN_END = new Object();
  private static final TokenSet END_OPENERS = TokenSet.create(ERL_BEGIN, ERL_IF, ERL_CASE, ERL_MAYBE, ERL_RECEIVE, ERL_TRY);

  private final ArrayDeque<Object> myExpected = new ArrayDeque<>();
  /** 1: the previous token is {@code fun}; 2: the previous tokens are {@code fun Var}. */
  private int myFun;
  private IElementType myPrevious;

  /** The step of the next significant token. After {@link Step#CLOSE} the splitter is done. */
  public @NotNull Step next(@NotNull IElementType type, @NotNull CharSequence text) {
    int fun = myFun;
    myFun = 0;
    if (fun == 1 && type == ERL_PAR_LEFT) myExpected.push(FUN_END);
    else if (fun == 2 && type == ERL_PAR_LEFT) myExpected.push(ERL_END);
    Step r = step(type, text);
    if (fun == 1 && type == ERL_VAR) myFun = 2;
    myPrevious = type;
    return r;
  }

  private @NotNull Step step(@NotNull IElementType type, @NotNull CharSequence text) {
    if (myExpected.isEmpty() && type == ERL_COMMA) return Step.SEPARATOR;
    if (myExpected.isEmpty() && type == ERL_PAR_RIGHT) return Step.CLOSE;
    if (type == ERL_PAR_LEFT) myExpected.push(ERL_PAR_RIGHT);
    else if (type == ERL_BIN_START) myExpected.push(ERL_BIN_END);
    else if (type == ERL_BRACKET_LEFT) myExpected.push(ERL_BRACKET_RIGHT);
    else if (type == ERL_CURLY_LEFT) myExpected.push(ERL_CURLY_RIGHT);
    else if (END_OPENERS.contains(type) || isCond(type, text)) myExpected.push(ERL_END);
    else if (type == ERL_FUN) myFun = 1;
    else if (myExpected.peek() == FUN_END) {
      if (type == ERL_WHEN || type == ERL_ARROW) {
        // a fun with clauses: fun () when ... -> ... end
        myExpected.pop();
        myExpected.push(ERL_END);
      }
      else if (type == ERL_COMMA || type == ERL_PAR_RIGHT) {
        // the end of a fun type: fun(), fun((a) -> b)
        myExpected.pop();
        return step(type, text);
      }
    }
    else if (!myExpected.isEmpty() && myExpected.peek() == type) myExpected.pop();
    return Step.ARGUMENT;
  }

  /** {@code cond} is a reserved word of erl_scan; the plugin's lexer reads it as an atom ({@code 'cond'} is an atom). */
  private boolean isCond(@NotNull IElementType type, @NotNull CharSequence text) {
    return type == ERL_ATOM_NAME && myPrevious != ERL_SINGLE_QUOTE && "cond".contentEquals(text);
  }
}
