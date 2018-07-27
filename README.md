## Erlang plugin
[![Build Status](https://teamcity.jetbrains.com/app/rest/builds/buildType:(id:IntellijIdeaPlugins_IntellijErlang_Tests)/statusIcon.svg?guest=1)](https://teamcity.jetbrains.com/viewType.html?buildTypeId=IntellijIdeaPlugins_IntellijErlang_Tests&guest=1)
[![Build Status](https://travis-ci.org/ignatov/intellij-erlang.svg?branch=master)](https://travis-ci.org/ignatov/intellij-erlang)

[Erlang Plugin](http://plugins.jetbrains.com/plugin/?pluginId=7083) turns [IntelliJ IDEA](http://www.jetbrains.com/idea/) 
and other IntelliJ-based products 
([RubyMine](http://www.jetbrains.com/ruby/), 
[PyCharm](http://www.jetbrains.com/pycharm/), 
[WebStorm](http://www.jetbrains.com/webstorm/), etc.) to a convenient [Erlang](http://www.erlang.org/) IDE.

## Pre-release builds
Download [the latest successful plugin](https://teamcity.jetbrains.com/repository/download/IntellijIdeaPlugins_IntellijErlang_Tests/.lastSuccessful/intellij-erlang-0.11.{build.number}.zip?guest=1), hit 'Install plugin from disk...' at 'Plugins' page in 'Settings' of your IDE, and point it to the downloaded archive.

Note for **Mac OS X** users: you need to download the entire zip file, not a several jars (see [Apple discussion forum](https://discussions.apple.com/thread/1483114)).

## Recent changes
### The latest version
* Improved build system - incremental compilation (<a href="https://github.com/ignatov/intellij-erlang/issues/657">#657</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/609">#609</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/513">#513</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/475">#475</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/428">#428</a>)
* Enhanced .app files support: reference resolution and autocomplete
* Erlang 18 compatibility: optional callbacks support (<a href="https://github.com/ignatov/intellij-erlang/issues/644">#644</a>), eunit compatibility (<a href="https://github.com/ignatov/intellij-erlang/issues/629">#629</a>)
* Enhanced -behavior attribute support: autocomplete, conflicts inspection (<a href="https://github.com/ignatov/intellij-erlang/issues/640">#640</a>)
* Spellchecking
* Illegal pattern/guard inspection (<a href="https://github.com/ignatov/intellij-erlang/issues/620">#620</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/622">#622</a>)
* Code editing/indentation/formatting improvements (<a href="https://github.com/ignatov/intellij-erlang/issues/593">#593</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/574">#574</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/594">#594</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/582">#582</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/589">#589</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/587">#587</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/582">#582</a>)
* Working directory for run configurations (<a href="https://github.com/ignatov/intellij-erlang/issues/572">#572</a>)
* Rebar on Windows (<a href="https://github.com/ignatov/intellij-erlang/issues/432">#432</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/469">#469</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/496">#496</a>)
* Source files are no longer copied to output upon build (<a href="https://github.com/ignatov/intellij-erlang/issues/608">#608</a>)
* Maps syntax in Erlang Term Files (<a href="https://github.com/ignatov/intellij-erlang/issues/588">#588</a>)
* Various bugfixes (<a href="https://github.com/ignatov/intellij-erlang/issues/632">#632</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/559">#559</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/605">#605</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/614">#614</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/612">#612</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/562">#562</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/603">#603</a>, <a href="https://github.com/ignatov/intellij-erlang/issues/576">#576</a>)

### Work in progress
* Preprocessing support

## Grammar-Kit version
[Download](https://github.com/JetBrains/Grammar-Kit/releases/tag/1.2.0.2) the right version of Grammar-Kit.
