# Erlang Plugin for IntelliJ IDEA

[![Build Status](https://teamcity.jetbrains.com/app/rest/builds/buildType:(id:IntellijIdeaPlugins_IntellijErlang_Tests)/statusIcon.svg?guest=1)](https://teamcity.jetbrains.com/viewType.html?buildTypeId=IntellijIdeaPlugins_IntellijErlang_Tests&guest=1)
[![Build Status](https://travis-ci.org/ignatov/intellij-erlang.svg?branch=master)](https://travis-ci.org/ignatov/intellij-erlang)
[![JetBrains Plugin](https://img.shields.io/jetbrains/plugin/v/7083-erlang.svg)](https://plugins.jetbrains.com/plugin/7083-erlang)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

The [Erlang Plugin](http://plugins.jetbrains.com/plugin/?pluginId=7083) transforms [IntelliJ IDEA](http://www.jetbrains.com/idea/) and other IntelliJ-based IDEs into a powerful development environment for [Erlang](http://www.erlang.org/).

## Features

- **Syntax Highlighting**: Full Erlang syntax highlighting with customizable color schemes
- **Code Navigation**: Go to definition, find usages, and structure view
- **Code Completion**: Smart code completion for functions, records, variables, and more
- **Refactoring**: Rename, extract variable, introduce function, and more
- **Debugging**: Built-in debugger support (experimental)
- **Testing**: EUnit test runner with native reporter
- **Build Tools**: Rebar and Rebar3 integration
- **Type System**: Support for Erlang types, specs, and Dialyzer integration
- **Documentation**: Quick documentation lookup for OTP functions and modules

## Installation

### From JetBrains Marketplace

1. Open IntelliJ IDEA
2. Go to **File** → **Settings** → **Plugins**
3. Search for "Erlang"
4. Click **Install**
5. Restart the IDE

### From Disk

For the latest development builds:

1. Download [the latest successful plugin](https://teamcity.jetbrains.com/repository/download/IntellijIdeaPlugins_IntellijErlang_Tests/.lastSuccessful/intellij-erlang-0.11.{build.number}.zip?guest=1)
2. Go to **File** → **Settings** → **Plugins**
3. Click the gear icon ⚙️ → **Install Plugin from Disk...**
4. Select the downloaded ZIP file
5. Restart the IDE

> **Note for macOS users**: Download the entire ZIP file, not individual JAR files. See [this discussion](https://discussions.apple.com/thread/1483114) for more details.

## Requirements

- **IntelliJ IDEA**: 2022.1 or later (Community or Ultimate Edition)
- **Java**: 17 or later
- **Erlang/OTP**: Any recent version (for running Erlang code)

### Supported IDEs

- IntelliJ IDEA
- PyCharm
- WebStorm
- RubyMine
- PhpStorm
- CLion
- Other JetBrains IDEs

## Quick Start

1. Install the plugin
2. Create a new Erlang project or open an existing one
3. Configure the Erlang SDK in **File** → **Project Structure** → **SDKs**
4. Start coding!

## Documentation

- **[Development Guide](docs/DEVELOPMENT.md)**: Information for plugin developers
- **[Contributing Guidelines](CONTRIBUTING.md)**: How to contribute to the project
- **[Changelog](CHANGELOG.md)**: Version history and changes
- **[Code of Conduct](CODE_OF_CONDUCT.md)**: Community guidelines
- **[Security Policy](SECURITY.md)**: How to report security vulnerabilities

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details on:

- How to submit bug reports
- How to suggest new features
- How to submit pull requests
- Development setup and workflows

## Support

- **Issues**: [GitHub Issues](https://github.com/ignatov/intellij-erlang/issues)
- **Discussions**: [Google Group](https://groups.google.com/d/forum/intellij-erlang-dev)
- **Plugin Page**: [JetBrains Marketplace](https://plugins.jetbrains.com/plugin/7083-erlang)

## License

This project is licensed under the Apache License 2.0 - see the [LICENCE](LICENCE) file for details.

## Authors

See [AUTHORS.md](AUTHORS.md) for the list of contributors.

## Acknowledgments

- Thanks to all [contributors](https://github.com/ignatov/intellij-erlang/graphs/contributors)
- Built with the [IntelliJ Platform SDK](https://plugins.jetbrains.com/docs/intellij/welcome.html)
- Erlang language by [Ericsson](https://www.erlang.org/)

