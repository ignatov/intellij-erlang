# Erlang plugin
Erlang language support for IntelliJ Platform (e.g. IntelliJ IDEA).
## Work in progress
You can manually set up the latest version of plugin from the [Erlang.zip](https://github.com/ignatov/intellij-erlang/raw/master/bin/Erlang.zip) file.

## Contributing
If you want to help but doesn't know how, you may check out issues [for collaboration](https://github.com/ignatov/intellij-erlang/issues?labels=for+collaboration&state=open).
Before sending a pull request please check that the **Erlang TestCase** is successfully passed. Moreover, new tests will be gladly accepted.

## Getting started with plugin development
1. [Download](http://www.jetbrains.com/idea/download/) the 11.1.4 IntelliJ IDEA build and install it
1. Checkout plugin repo and open the project
1. Setup [IDEA SDK](http://confluence.jetbrains.net/display/IDEADEV/Getting+Started+with+Plugin+Development#GettingStartedwithPluginDevelopment-anchor2):
select the IDEA installation directory as SDK root
1. **Manually add ```$IDEA_SDK_PATH$/plugins/copyright/lib/copyright.jar``` to IDEA SDK classpath**
1. Setup the latest version of [Grammar-Kit](https://github.com/JetBrains/Grammar-Kit) plugin
1. Run the **Erlang** run configuration

For further information please see [official plugin development page](http://confluence.jetbrains.net/display/IDEADEV/PluginDevelopment).
Also you can read some [tips and tricks](http://tomaszdziurko.pl/2011/09/developing-plugin-intellij-idea-some-tips-and-links/). Happy hacking!

## Compilation and running
You need to specify module name, function name and input parameters in run configuration setting 
or you might move the cursor on the function and press **Ctrl + Shift + F10**.

![Compilation and run](https://github.com/ignatov/intellij-erlang/raw/master/images/hello-run-configuration.png)

## Donations
If you would like to make a donation you can use [PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=C344TC8DDC5UN).

## Change log

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
