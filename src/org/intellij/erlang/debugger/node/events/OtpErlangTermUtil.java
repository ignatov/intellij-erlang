package org.intellij.erlang.debugger.node.events;

import com.ericsson.otp.erlang.*;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class OtpErlangTermUtil {
  private OtpErlangTermUtil() {
  }

  @Nullable
  public static Integer getIntegerValue(@Nullable OtpErlangObject integerObject) {
    OtpErlangLong otpLong = integerObject instanceof OtpErlangLong ? (OtpErlangLong) integerObject : null;
    try {
      return otpLong != null ? otpLong.intValue() : null;
    } catch (OtpErlangRangeException e) {
      return null;
    }
  }

  @Nullable
  public static OtpErlangList getListValue(@Nullable OtpErlangObject listObject) {
    if (listObject instanceof OtpErlangList) {
      return (OtpErlangList)listObject;
    }
    if (listObject instanceof OtpErlangString) {
      OtpErlangString string = (OtpErlangString) listObject;
      return new OtpErlangList(string.stringValue());
    }
    return null;
  }

  @Nullable
  public static OtpErlangTuple getTupleValue(@Nullable OtpErlangObject tupleObject) {
    return tupleObject instanceof OtpErlangTuple ? (OtpErlangTuple) tupleObject : null;
  }

  @Nullable
  public static OtpErlangPid getPidValue(@Nullable OtpErlangObject pidObject) {
    return pidObject instanceof OtpErlangPid ? (OtpErlangPid) pidObject : null;
  }

  @Nullable
  public static String getAtomText(@Nullable OtpErlangObject atomObject) {
    OtpErlangAtom atom = atomObject instanceof OtpErlangAtom ? (OtpErlangAtom) atomObject : null;
    return atom != null ? atom.atomValue() : null;
  }

  @Nullable
  public static OtpErlangObject elementAt(@Nullable OtpErlangTuple tuple, int idx) {
    return tuple != null ? tuple.elementAt(idx) : null;
  }

  public static boolean isOkAtom(@Nullable OtpErlangObject okObject) {
    return isAtom("ok", okObject);
  }

  public static boolean isErrorAtom(@Nullable OtpErlangObject errorObject) {
    return isAtom("error", errorObject);
  }

  public static boolean isAtom(@NotNull String expectedAtom, @Nullable OtpErlangObject atomObject) {
    return StringUtil.equals(expectedAtom, getAtomText(atomObject));
  }

  @NotNull
  public static String toString(@Nullable OtpErlangObject object) {
    return null != object ? object.toString() : "";
  }
}
