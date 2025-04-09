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

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.ConfigurationFromContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformCoreDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testFramework.JavaModuleTestCase;
import com.intellij.testFramework.MapDataContext;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationProducer;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationProducer;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangDebuggableRunConfigurationProducerTest extends JavaModuleTestCase {
  @Override
  public @NotNull ErlangModuleType getModuleType() {
    return ErlangModuleType.getInstance();
  }

  @Override
  @NotNull
  public String getTestDirectoryName() {
    return "testData/runconfig/debuggableConfigurationProducer/";
  }


  public void testNoNifs() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    doTestDebugOptionsForSource(noNifs);
    doTestDebugOptionsForTests(noNifs);
  }

  public void testNifsInTests() throws Exception {
    Module nifsInTests = createErlangModule("nifs_in_tests");
    doTestDebugOptionsForSource(nifsInTests);
    doTestDebugOptionsForTests(nifsInTests, "nifs_in_tests_test");
  }

  public void testNifsInSource() throws Exception {
    Module nifsInSource = createErlangModule("nifs_in_source");
    doTestDebugOptionsForSource(nifsInSource, "nifs_in_source");
    doTestDebugOptionsForTests(nifsInSource, "nifs_in_source");
  }

  public void testNoNifsDependentOnNifsInTests() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    Module nifsInTests = createErlangModule("nifs_in_tests");
    addDependency(noNifs, nifsInTests);

    doTestDebugOptionsForSource(noNifs);
    doTestDebugOptionsForTests(noNifs, "nifs_in_tests_test");
  }

  /*
  File accessed outside allowed roots: file:///private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NifsInTests_2vURNGEwo8Rw75fRvyoFqXC27Sj/2vURNLf2svmJ6xMuSjS8Pn8sE5C/test/nifs_in_tests_test.erl;
Allowed roots: [/Users/ignatov/.gradle, /private/etc, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOM94iRnes9IkqLavNwyELBm/test, /Users/ignatov/src/intellij-erlang/build/idea-sandbox/IC-2025.1/config-test, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOKMrrFpW4poFw7wqfstdld4/test, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOM94iRnes9IkqLavNwyELBm/src, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOK3PtvnVXYVIHKUAtnukUkU/test, /var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOM94iRnes9IkqLavNwyELBm, /etc, /Users/ignatov/.gradle/caches/8.10/transforms/76a4016573b19ea80bee0b757b63e081/transformed/ideaIC-251.23774.318-aarch64/jbr/Contents/Home, /Users/ignatov/.gradle/caches/8.10/transforms/76a4016573b19ea80bee0b757b63e081/transformed/ideaIC-251.23774.318-aarch64, /Users/ignatov, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv, /usr/lib/jvm, /Users/ignatov/.gradle/caches/8.10/transforms/76a4016573b19ea80bee0b757b63e081/transformed/ideaIC-251.23774.318-aarch64/java/mockJDK-1.7, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOKMrrFpW4poFw7wqfstdld4/src, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOK3PtvnVXYVIHKUAtnukUkU, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOK3PtvnVXYVIHKUAtnukUkU/src, /Users/ignatov/src/intellij-erlang/build, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOKMrrFpW4poFw7wqfstdld4]
com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess$VfsRootAccessNotAllowedError: File accessed outside allowed roots: file:///private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NifsInTests_2vURNGEwo8Rw75fRvyoFqXC27Sj/2vURNLf2svmJ6xMuSjS8Pn8sE5C/test/nifs_in_tests_test.erl;
Allowed roots: [/Users/ignatov/.gradle, /private/etc, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOM94iRnes9IkqLavNwyELBm/test, /Users/ignatov/src/intellij-erlang/build/idea-sandbox/IC-2025.1/config-test, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOKMrrFpW4poFw7wqfstdld4/test, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOM94iRnes9IkqLavNwyELBm/src, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOK3PtvnVXYVIHKUAtnukUkU/test, /var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOM94iRnes9IkqLavNwyELBm, /etc, /Users/ignatov/.gradle/caches/8.10/transforms/76a4016573b19ea80bee0b757b63e081/transformed/ideaIC-251.23774.318-aarch64/jbr/Contents/Home, /Users/ignatov/.gradle/caches/8.10/transforms/76a4016573b19ea80bee0b757b63e081/transformed/ideaIC-251.23774.318-aarch64, /Users/ignatov, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv, /usr/lib/jvm, /Users/ignatov/.gradle/caches/8.10/transforms/76a4016573b19ea80bee0b757b63e081/transformed/ideaIC-251.23774.318-aarch64/java/mockJDK-1.7, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOKMrrFpW4poFw7wqfstdld4/src, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOK3PtvnVXYVIHKUAtnukUkU, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOK3PtvnVXYVIHKUAtnukUkU/src, /Users/ignatov/src/intellij-erlang/build, /private/var/folders/2f/1q6x13kn6f5dzbh5t85lv9k00000gq/T/unitTest_NoNifsDependentOnNifsInSource_2vUYOMdUmNEhuQnFnDHQQNeBIbv/2vUYOKMrrFpW4poFw7wqfstdld4]
	at com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess.assertAccessInTests(VfsRootAccess.java:121)
	at com.intellij.openapi.vfs.newvfs.impl.VirtualDirectoryImpl.findInPersistence(VirtualDirectoryImpl.java:202)
	at com.intellij.openapi.vfs.newvfs.impl.VirtualDirectoryImpl.doFindChild(VirtualDirectoryImpl.java:138)
	at com.intellij.openapi.vfs.newvfs.impl.VirtualDirectoryImpl.findChild(VirtualDirectoryImpl.java:84)
	at com.intellij.openapi.vfs.newvfs.impl.VirtualDirectoryImpl.doFindChildById(VirtualDirectoryImpl.java:588)
	at com.intellij.openapi.vfs.newvfs.persistent.PersistentFSImpl$ParentFinder.findChild(PersistentFSImpl.java:1981)
	at com.intellij.openapi.vfs.newvfs.persistent.PersistentFSImpl$ParentFinder.findDescendantByIdPath(PersistentFSImpl.java:1973)
	at com.intellij.openapi.vfs.newvfs.persistent.PersistentFSImpl$ParentFinder.find(PersistentFSImpl.java:2062)
	at com.intellij.openapi.vfs.newvfs.persistent.PersistentFSImpl.findFileById(PersistentFSImpl.java:1773)
	at com.intellij.openapi.vfs.newvfs.persistent.PersistentFSImpl.findFileById(PersistentFSImpl.java:71)
	at com.intellij.util.indexing.FileBasedIndexImpl.findFileById(FileBasedIndexImpl.java:1212)
	at com.intellij.util.indexing.FileBasedIndexEx.processVirtualFiles(FileBasedIndexEx.java:638)
	at com.intellij.util.indexing.FileBasedIndexEx.processFilesContainingAllKeysInPhysicalFiles(FileBasedIndexEx.java:507)
	at com.intellij.util.indexing.FileBasedIndexEx.processFilesContainingAllKeys(FileBasedIndexEx.java:516)
	at com.intellij.util.indexing.FileBasedIndexImpl.processFilesContainingAllKeys(FileBasedIndexImpl.java:1207)
	at com.intellij.psi.impl.cache.impl.IndexCacheManagerImpl.processVirtualFilesWithAllWords(IndexCacheManagerImpl.java:85)
	at com.intellij.psi.impl.cache.impl.IndexCacheManagerImpl.collectVirtualFilesWithWord(IndexCacheManagerImpl.java:101)
	at com.intellij.psi.impl.cache.impl.IndexCacheManagerImpl.processFilesWithWord(IndexCacheManagerImpl.java:108)
	at com.intellij.psi.impl.search.PsiSearchHelperImpl.processAllFilesWithWord(PsiSearchHelperImpl.java:788)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducer.getErlangModulesWithCallsToLoadNIF(ErlangDebuggableRunConfigurationProducer.java:111)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducer.createDefaultDebugOptions(ErlangDebuggableRunConfigurationProducer.java:89)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducer.setupDebugOptions(ErlangDebuggableRunConfigurationProducer.java:73)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducer.setupConfigurationFromContext(ErlangDebuggableRunConfigurationProducer.java:53)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducer.setupConfigurationFromContext(ErlangDebuggableRunConfigurationProducer.java:43)
	at com.intellij.execution.actions.RunConfigurationProducer.createConfigurationFromContext(RunConfigurationProducer.java:99)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducerTest.doTestDebugOptions(ErlangDebuggableRunConfigurationProducerTest.java:132)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducerTest.doTestDebugOptionsForSource(ErlangDebuggableRunConfigurationProducerTest.java:113)
	at org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducerTest.testNoNifsDependentOnNifsInSource(ErlangDebuggableRunConfigurationProducerTest.java:95)
	at java.base/java.lang.reflect.Method.invoke(Method.java:580)
	at junit.framework.TestCase.runTest(TestCase.java:177)
	at com.intellij.testFramework.UsefulTestCase.lambda$runBare$12(UsefulTestCase.java:501)
	at com.intellij.testFramework.UsefulTestCase.runTestRunnable(UsefulTestCase.java:426)
	at com.intellij.testFramework.HeavyPlatformTestCase.runTestRunnable(HeavyPlatformTestCase.java:655)
	at com.intellij.testFramework.HeavyPlatformTestCase.lambda$runBareImpl$25(HeavyPlatformTestCase.java:587)
	at com.intellij.testFramework.TestLoggerKt$recordErrorsLoggedInTheCurrentThreadAndReportThemAsFailures$2.invoke(testLogger.kt:99)
	at com.intellij.testFramework.TestLoggerKt$recordErrorsLoggedInTheCurrentThreadAndReportThemAsFailures$2.invoke(testLogger.kt:99)
	at com.intellij.testFramework.TestLoggerKt.recordErrorsLoggedInTheCurrentThreadAndReportThemAsFailures(testLogger.kt:81)
	at com.intellij.testFramework.TestLoggerKt.recordErrorsLoggedInTheCurrentThreadAndReportThemAsFailures(testLogger.kt:99)
	at com.intellij.testFramework.UsefulTestCase.lambda$wrapTestRunnable$15(UsefulTestCase.java:533)
	at com.intellij.testFramework.EdtTestUtil.lambda$runInEdtAndWait$5(EdtTestUtil.java:92)
	at com.intellij.openapi.application.TransactionGuardImpl.runWithWritingAllowed(TransactionGuardImpl.java:240)
	at com.intellij.openapi.application.TransactionGuardImpl.access$100(TransactionGuardImpl.java:25)
	at com.intellij.openapi.application.TransactionGuardImpl$1.run(TransactionGuardImpl.java:202)
	at com.intellij.openapi.application.impl.AnyThreadWriteThreadingSupport.runIntendedWriteActionOnCurrentThread$lambda$7(AnyThreadWriteThreadingSupport.kt:319)
	at com.intellij.openapi.application.impl.AnyThreadWriteThreadingSupport.runWriteIntentReadAction$lambda$6(AnyThreadWriteThreadingSupport.kt:274)
	at com.intellij.openapi.application.impl.AnyThreadWriteThreadingSupport.runWithTemporaryThreadLocal(AnyThreadWriteThreadingSupport.kt:204)
	at com.intellij.openapi.application.impl.AnyThreadWriteThreadingSupport.runWriteIntentReadAction(AnyThreadWriteThreadingSupport.kt:274)
	at com.intellij.openapi.application.impl.AnyThreadWriteThreadingSupport.runWriteIntentReadAction(AnyThreadWriteThreadingSupport.kt:222)
	at com.intellij.openapi.application.impl.AnyThreadWriteThreadingSupport.runIntendedWriteActionOnCurrentThread(AnyThreadWriteThreadingSupport.kt:318)
	at com.intellij.openapi.application.impl.ApplicationImpl.runIntendedWriteActionOnCurrentThread(ApplicationImpl.java:928)
	at com.intellij.openapi.application.impl.ApplicationImpl$4.run(ApplicationImpl.java:501)
	at com.intellij.openapi.application.impl.AppImplKt.rethrowExceptions$lambda$2(appImpl.kt:66)
	at com.intellij.util.concurrency.ChildContext$runInChildContext$1.invoke(propagation.kt:102)
	at com.intellij.util.concurrency.ChildContext$runInChildContext$1.invoke(propagation.kt:102)
	at com.intellij.util.concurrency.ChildContext.runInChildContext(propagation.kt:108)
	at com.intellij.util.concurrency.ChildContext.runInChildContext(propagation.kt:102)
	at com.intellij.util.concurrency.ContextRunnable.run(ContextRunnable.java:27)
	at com.intellij.openapi.application.impl.AppImplKt.rethrowExceptions$lambda$3(appImpl.kt:77)
	at com.intellij.openapi.application.impl.LaterInvocator$1.run(LaterInvocator.java:105)
	at com.intellij.openapi.application.impl.FlushQueue.runNextEvent(FlushQueue.java:117)
	at com.intellij.openapi.application.impl.FlushQueue.flushNow(FlushQueue.java:43)
	at java.desktop/java.awt.event.InvocationEvent.dispatch(InvocationEvent.java:318)
	at java.desktop/java.awt.EventQueue.dispatchEventImpl(EventQueue.java:781)
	at java.desktop/java.awt.EventQueue$4.run(EventQueue.java:728)
	at java.desktop/java.awt.EventQueue$4.run(EventQueue.java:722)
	at java.base/java.security.AccessController.doPrivileged(AccessController.java:400)
	at java.base/java.security.ProtectionDomain$JavaSecurityAccessImpl.doIntersectionPrivilege(ProtectionDomain.java:87)
	at java.desktop/java.awt.EventQueue.dispatchEvent(EventQueue.java:750)
	at com.intellij.ide.IdeEventQueue.defaultDispatchEvent(IdeEventQueue.kt:585)
	at com.intellij.ide.IdeEventQueue._dispatchEvent(IdeEventQueue.kt:482)
	at com.intellij.ide.IdeEventQueue.dispatchEvent$lambda$12$lambda$11$lambda$10$lambda$9(IdeEventQueue.kt:307)
	at com.intellij.openapi.progress.impl.CoreProgressManager.computePrioritized(CoreProgressManager.java:864)
	at com.intellij.ide.IdeEventQueue.dispatchEvent$lambda$12$lambda$11$lambda$10(IdeEventQueue.kt:306)
	at com.intellij.ide.IdeEventQueueKt.performActivity$lambda$3(IdeEventQueue.kt:958)
	at com.intellij.openapi.application.TransactionGuardImpl.performActivity(TransactionGuardImpl.java:117)
	at com.intellij.ide.IdeEventQueueKt.performActivity(IdeEventQueue.kt:958)
	at com.intellij.ide.IdeEventQueue.dispatchEvent$lambda$12(IdeEventQueue.kt:301)
	at com.intellij.ide.IdeEventQueue.dispatchEvent(IdeEventQueue.kt:341)
	at java.desktop/java.awt.EventDispatchThread.pumpOneEventForFilters(EventDispatchThread.java:207)
	at java.desktop/java.awt.EventDispatchThread.pumpEventsForFilter(EventDispatchThread.java:128)
	at java.desktop/java.awt.EventDispatchThread.pumpEventsForHierarchy(EventDispatchThread.java:117)
	at java.desktop/java.awt.EventDispatchThread.pumpEvents(EventDispatchThread.java:113)
	at java.desktop/java.awt.EventDispatchThread.pumpEvents(EventDispatchThread.java:105)
	at java.desktop/java.awt.EventDispatchThread.run(EventDispatchThread.java:92)
   */
//  @Ignore("Can't figure out how files from nifs_in_tests are present in the persistent fs")

//  public void testNoNifsDependentOnNifsInSource() throws Exception {
//    Module noNifs = createErlangModule("no_nifs");
//    Module nifsInSource = createErlangModule("nifs_in_source");
//    Module nifsInTests = createErlangModule("nifs_in_tests");
//    addDependency(noNifs, nifsInSource);
//
//    doTestDebugOptionsForSource(noNifs, "nifs_in_source");
//    doTestDebugOptionsForTests(noNifs, "nifs_in_source");
//  }

  public void testDependentNifsOfDependentModulesAreNotExcluded() throws Exception {
    Module noNifs = createErlangModule("no_nifs");
    Module nifsInSource = createErlangModule("nifs_in_source");
    Module nifsInTests = createErlangModule("nifs_in_tests");

    addDependency(nifsInSource, noNifs);
    addDependency(nifsInTests, noNifs);

    doTestDebugOptionsForSource(noNifs);
    doTestDebugOptionsForTests(noNifs);
  }


  private void doTestDebugOptionsForSource(Module module, String... expectedModulesNotToInterpret) {
    doTestDebugOptions(ErlangApplicationRunConfigurationProducer.class, module, false, expectedModulesNotToInterpret);
  }

  private void doTestDebugOptionsForTests(Module module, String... expectedModulesNotToInterpret) {
    doTestDebugOptions(ErlangUnitRunConfigurationProducer.class, module, true, expectedModulesNotToInterpret);
  }

  private void doTestDebugOptions(Class<? extends ErlangDebuggableRunConfigurationProducer<?>> producerClass,
                                  Module module, boolean useTestSource, String... expectedModulesNotToInterpret) {
    PsiElement elementToProduceFor = getElementToProduceFor(module, useTestSource);
    MapDataContext dataContext = new MapDataContext();
    dataContext.put(CommonDataKeys.PROJECT, myProject);
    dataContext.put(PlatformCoreDataKeys.MODULE, ModuleUtilCore.findModuleForPsiElement(elementToProduceFor));
    dataContext.put(CommonDataKeys.PSI_ELEMENT, elementToProduceFor);
    dataContext.put(Location.DATA_KEY, PsiLocation.fromPsiElement(elementToProduceFor));

    ConfigurationContext configurationContext = ConfigurationContext.getFromContext(dataContext, null);
    RunConfigurationProducer<?> producer = RunConfigurationProducer.getInstance(producerClass);
    System.out.println(producer);
    ConfigurationFromContext confFromCtx = producer.createConfigurationFromContext(configurationContext);
    assertNotNull(confFromCtx);

    RunConfiguration configuration = confFromCtx.getConfiguration();
    assertNotNull(configuration);
    assertInstanceOf(configuration, ErlangRunConfigurationBase.class);

    ErlangRunConfigurationBase<?> c = (ErlangRunConfigurationBase<?>) configuration;
    ErlangRunConfigurationBase.ErlangDebugOptions debugOptions = c.getDebugOptions();
    assertTrue(debugOptions.isAutoUpdateModulesNotToInterpret());
    assertSameElements(debugOptions.getModulesNotToInterpret(), expectedModulesNotToInterpret);
  }

  // We're producing for function named the same as it's containing module.
  // The containing module has the same name as it's IntelliJ's module and _test suffix if we're producing in test source.
  private PsiElement getElementToProduceFor(Module module, boolean useTestSource) {
    String erlangModuleName = module.getName() + (useTestSource ? "_test" : "");
    GlobalSearchScope scope = GlobalSearchScope.moduleScope(module);
    ErlangFile erlangModule = ErlangModulesUtil.getErlangModuleFile(myProject, erlangModuleName, scope);
    ErlangFunction function = erlangModule != null ? erlangModule.getFunction(erlangModuleName, 0) : null;
    assertNotNull(function);
    return function;
  }

  @NotNull
  private Module createErlangModule(String moduleName) throws Exception {
    String dirInTestData = getTestDirectoryName() + moduleName;
    Module module = createModuleFromTestData(dirInTestData, moduleName, getModuleType(), false);
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    VirtualFile contentRoot = ArrayUtil.getFirstElement(rootManager.getContentRoots());
    assertNotNull(contentRoot);

    VirtualFile sourceRoot = contentRoot.findChild("src");
    if (sourceRoot != null) {
      PsiTestUtil.addSourceRoot(module, sourceRoot, false);
    }

    VirtualFile testRoot = contentRoot.findChild("test");
    if (testRoot != null) {
      PsiTestUtil.addSourceRoot(module, testRoot, true);
    }

    return module;
  }

  private static void addDependency(final Module to, final Module what) throws Exception {
    final Ref<Exception> ex = Ref.create();
    ApplicationManager.getApplication().runWriteAction(() -> {
      ModifiableRootModel modifiableModel = ModuleRootManager.getInstance(to).getModifiableModel();
      try {
        modifiableModel.addModuleOrderEntry(what);
        modifiableModel.commit();
      }
      catch (Exception e) {
        modifiableModel.dispose();
        ex.set(e);
      }
    });

    if (!ex.isNull()) {
      throw ex.get();
    }
  }
}
