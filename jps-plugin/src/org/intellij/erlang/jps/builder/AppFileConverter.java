package org.intellij.erlang.jps.builder;

import java.io.*;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import com.intellij.openapi.util.io.FileUtil;
import com.metadave.etp.ETP;
import com.metadave.etp.rep.*;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;

/**
 * Takes original xxx.app.src file decodes it using ETP library, and adds modules and registred sections
 * if they are not already present.
 */
public class AppFileConverter {
  private File appConfigSrc;
  private ETPTuple appConfigterms;
  private CompileContext context;
  public AppFileConverter(CompileContext context, File appConfigSrc, File outputDir) {
    this.context=context;
    this.appConfigSrc=appConfigSrc;
    try {
      appConfigterms = (ETPTuple)ETP.parse(new FileInputStream(appConfigSrc));
      if(!"application".equals(((ETPAtom)(appConfigterms.getValue(0))).getValue()))
        throw new ParseException("Unexpected file format - missing application",0);
      List<ETPTerm<?>> list=((ETPList)(appConfigterms.getValue(2))).getValue();
      HashSet<String> s = new HashSet<>();
      for(ETPTerm<?> etpTerm : list) {
        String name = ((ETPAtom)(((ETPTuple)etpTerm).getValue(0))).getValue();
        s.add(name);
      }
      if(!s.contains("registered")) {
        list.add(new ETPTuple(new ETPAtom("registered"),new ETPList(new ArrayList<ETPTerm<?>>())));
      }
      if(!s.contains("modules")) {
        String[] names = outputDir.list(new FilenameFilter() {
          @Override
          public boolean accept(File dir, String name) {
            return name.endsWith("beam");
          }
        });
        ArrayList<ETPTerm<?>> modules = new ArrayList<ETPTerm<?>>();
        for(int i=0;i<names.length;i++) {
          modules.add(new ETPAtom(names[i].substring(0,names[i].length()-5)));
        }
        list.add(new ETPTuple(new ETPAtom("modules"),new ETPList(modules)));
      }
    }
    catch (Exception e) {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      e.printStackTrace(pw);
      context.processMessage(new CompilerMessage(ErlangBuilder.NAME, BuildMessage.Kind.INFO, "Failed to parse "+appConfigSrc+" app src file : "+e.getLocalizedMessage()+","+sw.toString()));
      appConfigterms = null;
    }
  }

  public void writeToFile(File appConfigDst) throws IOException {
    if(appConfigterms!=null)
      try(FileWriter fw=new FileWriter(appConfigDst)) {
        fw.write(appConfigterms.toString());
        fw.write(".");
      } catch(Exception e) {
        context.processMessage(new CompilerMessage(ErlangBuilder.NAME, BuildMessage.Kind.INFO, "Failed to write "+appConfigDst+" : "+e.getLocalizedMessage()));
        appConfigterms=null;
      }
    if(appConfigterms==null) {
      FileUtil.copy(appConfigSrc, appConfigDst);
    }
  }
}
