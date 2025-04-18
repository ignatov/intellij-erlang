## Erlang plugin
[![Build Status](https://teamcity.jetbrains.com/app/rest/builds/buildType:(id:IntellijIdeaPlugins_IntellijErlang_Tests)/statusIcon.svg?guest=1)](https://teamcity.jetbrains.com/viewType.html?buildTypeId=IntellijIdeaPlugins_IntellijErlang_Tests&guest=1)
[![Build Status](https://travis-ci.org/ignatov/intellij-erlang.svg?branch=master)](https://travis-ci.org/ignatov/intellij-erlang)

[Erlang Plugin](http://plugins.jetbrains.com/plugin/?pluginId=7083) turns [IntelliJ IDEA](http://www.jetbrains.com/idea/) 
and other IntelliJ-based products 
([RubyMine](http://www.jetbrains.com/ruby/), 
[PyCharm](http://www.jetbrains.com/pycharm/), 
[WebStorm](http://www.jetbrains.com/webstorm/), etc.) to a convenient [Erlang](http://www.erlang.org/) IDE.

## Requirements
- IntelliJ IDEA 2022.1 or later (Community or Ultimate)
- Java 17 or later

## For Developers
If you're developing or modifying the plugin, please see the [Development Guide](docs/DEVELOPMENT.md) for information about Java version compatibility requirements.

## Pre-release builds
Download [the latest successful plugin](https://teamcity.jetbrains.com/repository/download/IntellijIdeaPlugins_IntellijErlang_Tests/.lastSuccessful/intellij-erlang-0.11.{build.number}.zip?guest=1), hit 'Install plugin from disk...' at 'Plugins' page in 'Settings' of your IDE, and point it to the downloaded archive.

Note for **Mac OS X** users: you need to download the entire zip file, not a several jars (see [Apple discussion forum](https://discussions.apple.com/thread/1483114)).
