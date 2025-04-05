/*
 * Copyright 2012-2025 Sergey Ignatov
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

package org.intellij.erlang.hints;

import com.intellij.codeInsight.hints.InlayInfo;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Collection;
import java.util.List;

public class ErlangInlayHintsProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
    
    public void testFunctionWithMultipleOverloads() {
        // Test with multiple function clauses/overloads
        String code = """
            -module(test).

            %% Function with multiple clauses
            foo(1) -> bar();
            foo(2) -> bar();
            foo(M) when M > 0 -> 1;
            foo(N) -> 0.

            %% Function that calls foo
            test() -> foo(5).
            """;

        myFixture.configureByText("test.erl", code);

        // Find the function call
        ErlangFunctionCallExpression call = findFunctionCall("foo");
        assertNotNull("Function call 'foo' not found", call);

        // Find the function definition
        ErlangFunction function = resolveFunction(call);
        assertNotNull("Function definition for 'foo' not found", function);

        // Check that the function has multiple clauses
        List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
        assertEquals("Function should have 4 clauses", 4, clauses.size());

        // Test parameter hints with showAllClauseParams = false
        ErlangInlayHintsProvider.SHOW_ALL_PARAMS_OPTION.set(false);
        ErlangInlayHintsProvider provider = new ErlangInlayHintsProvider();
        List<InlayInfo> hints = provider.getParameterHints(call);
        
        assertEquals("Should generate 1 parameter hint", 1, hints.size());
        // Provider takes first clause, so hint shows the parameter name from the first clause
        assertEquals("Hint should be from first clause (1)", "1", hints.get(0).getText());
        
        // Test parameter hints with showAllClauseParams = true
        ErlangInlayHintsProvider.SHOW_ALL_PARAMS_OPTION.set(true);
        hints = provider.getParameterHints(call);
        
        assertEquals("Should generate 1 parameter hint", 1, hints.size());
        // Should show all parameter names from all clauses
        assertEquals("Hint should show all parameter names", "1|2|M|N", hints.get(0).getText());
    }

    public void testProviderExistence() {
        // Test that the provider is properly registered
        ErlangInlayHintsProvider provider = new ErlangInlayHintsProvider();
        assertNotNull(provider);
        
        // Test that the provider's interface methods work
        assertNotNull(provider.getInlayPresentation("test"));
        assertNotNull(provider.getDefaultBlackList());
        
        // Test that the provider provides options
        assertFalse(provider.getSupportedOptions().isEmpty());
        assertTrue(provider.getSupportedOptions().contains(ErlangInlayHintsProvider.SHOW_ALL_PARAMS_OPTION));
    }

    public void testFunctionArgumentHints() {
        // Test that function argument hints work correctly
        String code = """
            -module(test).

            %% Function with named parameters
            add(First, Second) -> First + Second.

            %% Function that calls add
            calculate() -> add(1, 2).
            """;

        myFixture.configureByText("test.erl", code);

        // Find the function call
        ErlangFunctionCallExpression call = findFunctionCall("add");
        assertNotNull("Function call 'add' not found", call);

        // Find the function definition
        ErlangFunction function = resolveFunction(call);
        assertNotNull("Function definition for 'add' not found", function);

        // Check that the function has the expected parameters
        ErlangFunctionClause clause = function.getFunctionClauseList().get(0);
        ErlangArgumentDefinitionList argDefList = clause.getArgumentDefinitionList();
        List<ErlangArgumentDefinition> argDefs = argDefList.getArgumentDefinitionList();
        assertEquals("Function should have 2 parameters", 2, argDefs.size());

        // Check parameter names
        assertEquals("First parameter should be named 'First'", "First", 
                     argDefs.get(0).getExpression().getText());
        assertEquals("Second parameter should be named 'Second'", "Second", 
                     argDefs.get(1).getExpression().getText());

        // Check arguments
        ErlangArgumentList argList = call.getArgumentList();
        List<ErlangExpression> args = argList.getExpressionList();
        assertEquals("Function call should have 2 arguments", 2, args.size());

        // Check argument values
        assertEquals("First argument should be '1'", "1", args.get(0).getText());
        assertEquals("Second argument should be '2'", "2", args.get(1).getText());
        
        // Test that the provider generates correct hints for this function call
        ErlangInlayHintsProvider provider = new ErlangInlayHintsProvider();
        List<InlayInfo> hints = provider.getParameterHints(call);
        
        assertEquals("Should generate 2 parameter hints", 2, hints.size());
        assertEquals("First hint should be at position of first argument", 
                    args.get(0).getTextRange().getStartOffset(), hints.get(0).getOffset());
        assertEquals("Second hint should be at position of second argument", 
                    args.get(1).getTextRange().getStartOffset(), hints.get(1).getOffset());
        assertEquals("First hint should have text 'First'", "First", hints.get(0).getText());
        assertEquals("Second hint should have text 'Second'", "Second", hints.get(1).getText());
    }

    public void testMultipleFunctionClauses() {
        // Test that the correct function clause is used for hints
        String code = """
            -module(test).

            %% Function with multiple clauses
            process(0) -> zero;
            process(N) -> non_zero.

            %% Function that calls process
            test() -> process(1).
            """;

        myFixture.configureByText("test.erl", code);

        // Find the function call
        ErlangFunctionCallExpression call = findFunctionCall("process");
        assertNotNull("Function call 'process' not found", call);

        // Find the function definition
        ErlangFunction function = resolveFunction(call);
        assertNotNull("Function definition for 'process' not found", function);

        // Check that the function has multiple clauses
        List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
        assertEquals("Function should have 2 clauses", 2, clauses.size());

        // Check that the second clause has the expected parameter
        ErlangFunctionClause secondClause = clauses.get(1);
        ErlangArgumentDefinitionList argDefList = secondClause.getArgumentDefinitionList();
        List<ErlangArgumentDefinition> argDefs = argDefList.getArgumentDefinitionList();
        assertEquals("Second clause should have 1 parameter", 1, argDefs.size());
        assertEquals("Parameter should be named 'N'", "N", 
                     argDefs.get(0).getExpression().getText());
                     
        // Test with showAllParams = false
        ErlangInlayHintsProvider.SHOW_ALL_PARAMS_OPTION.set(false);
        ErlangInlayHintsProvider provider = new ErlangInlayHintsProvider();
        List<InlayInfo> hints = provider.getParameterHints(call);
        
        assertEquals("Should generate 1 parameter hint", 1, hints.size());
        assertEquals("Hint should have text '0'", "0", hints.get(0).getText());
        
        // Test with showAllParams = true
        ErlangInlayHintsProvider.SHOW_ALL_PARAMS_OPTION.set(true);
        hints = provider.getParameterHints(call);
        
        assertEquals("Should generate 1 parameter hint", 1, hints.size());
        assertEquals("Hint should show all parameter names", "0|N", hints.get(0).getText());
    }

    private ErlangFunctionCallExpression findFunctionCall(String functionName) {
        Collection<ErlangFunctionCallExpression> calls = 
            PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangFunctionCallExpression.class);

        for (ErlangFunctionCallExpression call : calls) {
            if (call.getText().startsWith(functionName)) {
                return call;
            }
        }

        return null;
    }

    private ErlangFunction resolveFunction(ErlangFunctionCallExpression call) {
        PsiReference reference = call.getReference();
        if (reference == null) return null;

        PsiElement resolved = reference.resolve();
        if (resolved instanceof ErlangFunction) {
            return (ErlangFunction) resolved;
        }

        return null;
    }
}