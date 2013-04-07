# Erlang plugin
Erlang Plugin turns [IntelliJ IDEA](http://www.jetbrains.com/idea/) to a convenient [Erlang](http://www.erlang.org/) IDE.

# Pre-realise builds
I you would like to have a new features you can manually set up the latest version of plugin from the [intellij-erlang-builds](https://www.dropbox.com/sh/4sbgxzjto5fa21b/1cpM3Nb-yF) folder.

## Contributing
If you want to help but doesn't know how, you may check out issues [for collaboration](https://github.com/ignatov/intellij-erlang/issues?labels=for+collaboration&state=open).
Before sending a pull request please check that the **ErlangTestCase** is successfully passed. Moreover, new tests will be gladly accepted.

## Getting started with plugin development
1. [Download](http://www.jetbrains.com/idea/download/) the latest 12.1 IntelliJ IDEA build and install it
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

### Version 0.4.2
* IntelliJ IDEA 12.1 compatibility
* Erlang console run configuration
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
