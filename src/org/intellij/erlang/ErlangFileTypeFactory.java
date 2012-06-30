package org.intellij.erlang;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangFileTypeFactory extends FileTypeFactory {
  @Override
  public void createFileTypes(@NotNull FileTypeConsumer fileTypeConsumer) {
    fileTypeConsumer.consume(ErlangFileType.INSTANCE);
    fileTypeConsumer.consume(ErlangFileType.HRL);
  }
}
