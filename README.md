# Erlang plugin
Erlang Plugin turns [IntelliJ IDEA](http://www.jetbrains.com/idea/) to a convenient [Erlang](http://www.erlang.org/) IDE.

# Pre-release builds
You can manually set up the plugin latest version from the [intellij-erlang-builds](https://www.dropbox.com/sh/4sbgxzjto5fa21b/1cpM3Nb-yF) folder.

Note for **Mac OS X** users: you need to download the entire zip file, not a several jars (see [Apple discussion forum](https://discussions.apple.com/thread/1483114)). 

## Contributing
If you would like to help, you may check out issues [for collaboration](https://github.com/ignatov/intellij-erlang/issues?labels=for+collaboration&state=open).
Before sending a pull request please check that the **ErlangTestCase** is successfully passed. Moreover, new tests will be gladly accepted.

## Getting started with plugin development
1. [Download](http://www.jetbrains.com/idea/download/) the latest 13 IntelliJ IDEA EAP build and install it
1. Checkout plugin repo and open the project
1. Setup [IDEA SDK](http://confluence.jetbrains.net/display/IDEADEV/Getting+Started+with+Plugin+Development#GettingStartedwithPluginDevelopment-anchor2):
select the IDEA installation directory as SDK root
1. **Manually add ```$IDEA_SDK_PATH$/plugins/copyright/lib/copyright.jar``` to IDEA SDK classpath**
1. Setup the latest version of [Grammar-Kit](https://github.com/JetBrains/Grammar-Kit) plugin
1. Run the **Erlang** run configuration

For further information please see [the official plugin development page](http://confluence.jetbrains.net/display/IDEADEV/PluginDevelopment).
Also you can read some [tips and tricks](http://tomaszdziurko.pl/2011/09/developing-plugin-intellij-idea-some-tips-and-links/).
For all development questions and proposals you can mail to our [Google Group](https://groups.google.com/d/forum/intellij-erlang-dev).
Happy hacking!

## Donations
If you would like to make a donation you can use [PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=C344TC8DDC5UN).

## Change log

### Version 0.5.x (Work in progress)
* Enhanced debugger
* Preprocessing support

### Version 0.5.2
* Go to module action (Ctrl+N, Cmd+N for Mac OS X)
* Better typing for some cases
* Enhanced formatter
* Type aware completion (experimental)
* Record reference for the second argument of ```is_record/2```
* Better built-in file templates (thanks to Volodymyr Kyrychenko)
* Bugfixes

### Version 0.5.1
* Erlang/OTP R16B02 support
* Enhanced introduce variable refactoring
* Improved code formatting in some cases
* Enhanced include paths resolution
* Enhanced Rebar support: deps, include paths, parse_transforms parsing from rebar.config
* Bugfixes

### Version 0.5
* Rerun failed tests action
* Native reporter for eunit tests
* Completion for include and include_lib attributes
* Better completion for attributes
* Performance improvements
* Introduce variable refactoring
* Introduce macros quickfix
* Better auto indentation for some cases
* Debugger (experimental)
* Introduce function refactoring (experimental)
* RubyMine and other "small IDEs" compatibility
* Go to super action for callback implementations
* Better EUnit support: run single test and rerun failed tests action
* Enhanced native compilation: parse_transform and behaviour detection
* Comma-first style support
* Enhanced color schema settings
* Rebar-based compilation
* Bugfixes

### Version 0.4.5
* Comment based injection
* Dialyzer integration (experimental)
* Introduce variable and function quick fixes
* Faster module completion
* Bugfixes

### Version 0.4.4
* Bugfixes

### Version 0.4.3
* Bugfixes

### Version 0.4.2
* IntelliJ IDEA 12.1 compatibility
* Erlang console run configuration (experimental)
* Settings for Emacs executable path and Emacs-based formatter action a little bit reworked
* Behaviour line marker provider
* Implement all callbacks quickfix
* Better resolve for variables
* Bugfixes

### Version 0.4.1
* External **Shift+F1** and local **Ctrl+Q** help for OTP types (except for built-in)
* Links in local OTP documentation to other OTP modules, functions, and types were fixed
* Function parameter help **Ctrl+P** for BIF was fixed
* Unresolved function inspection added
* Better autodetection for Erlang SDK under Mac OS X
* Better local function completion (#182)
* Support for R16A and R16B
* **Experimental** Emacs-based formatter action **Ctrl+Alt+Shift+E**
* Smart typed handler for function and case clauses
* Highlighting settings for function declarations, types and specs (thanks to Marcus Nilsson)
* Bugfixes

### Version 0.4
* Rebar run configurations (thanks to Maxim Vladimirsky)
* Rebar-based Eunit integration
* Eunit integration improvements
* Erlang types support: completion (includes built-in types), rename and find usages for user defined types, export type quick fix and intention
* External **Shift+F1** and local **Ctrl+Q** documentation for OTP functions and modules (thanks to Maxim Vladimirsky)
* Function parameter info context help **Ctrl+P**
* Upgraded structure view (shows exported and inner functions)
* Go to action for Erlang symbols: functions, records, types and macroses **Ctrl+Alt+Shift+N**
* Improved BIFs completion (includes BIFs from OTP modules, for example **lists:member/2**
* Special highlighting type for atoms and macroses (thanks to Sergey Evstifeev)
* Format inspection (for io:format(), io:fwrite() and io_lib:format() functions calls)
* Inplace rename (without popup dialog) for all entities
* Head mismatch inspection and quick fix
* Formatter improvements
* Folding builder for functions
* Custom Erlang spelling dictionary for well-known words
* Support for **export_all** compile directive
* Safe delete functions improvement
* Export function and type intentions
* Better Darcula support
* Parser improvements, also includes support for Nitrogen web framework
* Bugfixes

### Version 0.3.5
* Extract variable refactoring
* New compiler mode (thanks to Nikolay Chashnikov)
* Rebar importer (thanks to Maxim Vladimirsky)
* IntelliJ IDEA 12 compatibility

### Version 0.3.4
* Eunit test runner
* Tests/sources switcher **Ctrl+Shift+T**
* Copyright plugin support (thanks to Maxim Vladimirsky)
* Recursive icon on the gutter
* Better completion
* Bugfixes

### Version 0.3.3
* Rebuild with Java 1.6

### Version 0.3.2
* New quick-fixes: export function, introduce record, introduce record field
* Multi-reference for functions: ability to go to the function definition with mismatched arity
* Better parsing, highlighting, completion and formatting
* Bugfixes

### Version 0.3.1           
* New fast parsing
* Highlighting for .config and .rebar files
* Live templates (thanks to Volodymyr Kyrychenko)
* New icons (thanks to Volodymyr Kyrychenko)
* Formatter improvements
* Bugfixes

### Version 0.3:
* Better completion
* Record fields support
* Formatter improvements
* New file templates for 'New File' action
* Bugfixes

### Version 0.2.5:
* Compilation and run
* Resolve, completion, rename and find usages for macroses

### Version 0.2:
* Erlang SDK
* Autocompletion for BIFs
* Creating project from scratch or existing sources
* Color setting page
* New inspections: unused functions and variables, duplicated functions
* Quick fix for incorrect module name
* 'New File From Template' action
* Bugfixes

### Version 0.1:
* Syntax and errors highlighting
* References resolving
* Code completion for functions, records and variables
* Keyword code completion
* Rename refactoring for modules, functions, records and variables
* Safe delete refactoring
* Structure view
* Find usages
* Code commenting/uncommenting
* Brace matching
