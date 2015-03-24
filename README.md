## Erlang plugin
[Erlang Plugin](http://plugins.jetbrains.com/plugin/?pluginId=7083) turns [IntelliJ IDEA](http://www.jetbrains.com/idea/) 
and other IntelliJ-based products 
([RubyMine](http://www.jetbrains.com/ruby/), 
[PyCharm](http://www.jetbrains.com/pycharm/), 
[WebStorm](http://www.jetbrains.com/webstorm/), etc.) to a convenient [Erlang](http://www.erlang.org/) IDE.

## Pre-release builds
You can manually set up the latest plugin version from the [releases page](https://github.com/ignatov/intellij-erlang/releases) or from the [intellij-erlang-builds](https://www.dropbox.com/sh/4sbgxzjto5fa21b/1cpM3Nb-yF) folder.

Note for **Mac OS X** users: you need to download the entire zip file, not a several jars (see [Apple discussion forum](https://discussions.apple.com/thread/1483114)).

## Recent changes
### Version 0.5.11
* New inspection: multiple function exports
* Fixed false-positive error highlighting (<a href="https://github.com/ignatov/intellij-erlang/issues/499">#499</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/505">#505</a>)
* Reworked Erlang/OTP version detection (<a href="https://github.com/ignatov/intellij-erlang/issues/514">#514</a>)
* Improved "Export Function" intention action (<a href="https://github.com/ignatov/intellij-erlang/issues/440">#440</a>)
* Improved debugger (<a href="https://github.com/ignatov/intellij-erlang/issues/501">#501</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/546">#546</a>)
* New completion variant: e.g. ```ilfo``` completes to ```io_lib:format()```
* Improved specification resolution
* Bugfixes

### Work in progress
* Preprocessing support

## Grammar-Kit version
[Download](https://github.com/JetBrains/Grammar-Kit/releases/tag/1.2.0.2) the right version of Grammar-Kit.
