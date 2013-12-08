# Erlang plugin
Erlang Plugin turns [IntelliJ IDEA](http://www.jetbrains.com/idea/) to a convenient [Erlang](http://www.erlang.org/) IDE.

# Pre-release builds
You can manually set up the plugin latest version from the [intellij-erlang-builds](https://www.dropbox.com/sh/4sbgxzjto5fa21b/1cpM3Nb-yF) folder.

Note for **Mac OS X** users: you need to download the entire zip file, not a several jars (see [Apple discussion forum](https://discussions.apple.com/thread/1483114)). 

## Contributing
If you would like to help, you may check out issues [for collaboration](https://github.com/ignatov/intellij-erlang/issues?labels=for+collaboration&state=open).
Before sending a pull request please check that the **ErlangTestCase** is successfully passed. Moreover, new tests will be gladly accepted.

## Getting started with plugin development
1. [Download](http://www.jetbrains.com/idea/download/) the latest 13 IntelliJ IDEA build and install it
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

## Recent changes

### Version 0.5.3
* Faster completion
* IDEA 13 compatibility
* Bugfixes

### Work in progress
* Enhanced debugger
* Remote debugger
* Preprocessing support
