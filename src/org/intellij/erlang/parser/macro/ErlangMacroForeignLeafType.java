package org.intellij.erlang.parser.macro;

import com.intellij.lang.ASTNode;
import com.intellij.lang.ForeignLeafType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A token produced by a macro substitution. It has no text in the file. It records its source: a token of an argument
 * of the use it belongs to (argument index and significant token index in the argument), a token of the body of a
 * same-file {@code -define} (macro name, arity and significant token index in the body), or nothing (made by the
 * substitution itself, as {@code ??Arg}). The source is given relative to the use and to the definition, not as a file
 * offset, so that a leaf that the tree merge keeps after an edit still finds its source.
 */
public class ErlangMacroForeignLeafType extends ForeignLeafType {
  public static final int SOURCE_NONE = 0;
  public static final int SOURCE_ARGUMENT = 1;
  public static final int SOURCE_BODY = 2;

  private final String myMacroName;
  private final int myDepth;
  private final int mySourceKind;
  private final @Nullable String myDefinitionName;
  private final int myArgumentOrArity;
  private final int myTokenIndex;

  public ErlangMacroForeignLeafType(@NotNull IElementType delegate, @NotNull CharSequence value, @NotNull String macroName, int depth) {
    this(delegate, value, macroName, depth, SOURCE_NONE, null, 0, 0);
  }

  public ErlangMacroForeignLeafType(@NotNull IElementType delegate, @NotNull CharSequence value, @NotNull String macroName, int depth,
                                    int sourceKind, @Nullable String definitionName, int argumentOrArity, int tokenIndex) {
    super(delegate, value);
    myMacroName = macroName;
    myDepth = depth;
    mySourceKind = sourceKind;
    myDefinitionName = definitionName;
    myArgumentOrArity = argumentOrArity;
    myTokenIndex = tokenIndex;
  }

  @Override
  public @NotNull ASTNode createLeafNode(@NotNull CharSequence leafText) {
    return new ErlangMacroForeignLeafElement(this, getText());
  }

  public @NotNull String getMacroName() {
    return myMacroName;
  }

  public int getDepth() {
    return myDepth;
  }

  public int getSourceKind() {
    return mySourceKind;
  }

  /** For a body source: the name of the macro whose {@code -define} holds the token. */
  public @Nullable String getDefinitionName() {
    return myDefinitionName;
  }

  /** For an argument source: the argument index. For a body source: the arity of the definition (-1: no parentheses). */
  public int getArgumentOrArity() {
    return myArgumentOrArity;
  }

  /** The index of the source token among the significant tokens of the argument or of the body. */
  public int getTokenIndex() {
    return myTokenIndex;
  }
}
