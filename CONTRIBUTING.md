# Contributing to IntelliJ Erlang Plugin

Thank you for considering contributing to the IntelliJ Erlang Plugin! This document provides guidelines and instructions for contributing.

## Code of Conduct

This project adheres to a [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check the [issue tracker](https://github.com/ignatov/intellij-erlang/issues) to avoid duplicates.

When creating a bug report, please use the bug report template and include:
- A clear and descriptive title
- Detailed steps to reproduce the issue
- Expected vs. actual behavior
- Screenshots if applicable
- Your environment (OS, IDE version, plugin version, Erlang/OTP version)
- Relevant logs from `idea.log`

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. Use the feature request template and include:
- A clear and descriptive title
- A detailed description of the proposed enhancement
- Explanation of why this enhancement would be useful
- Examples of how it would work

### Your First Code Contribution

Unsure where to begin? Look for issues labeled:
- `good first issue` - Good for newcomers
- `help wanted` - Extra attention needed
- `documentation` - Improvements to documentation

### Pull Requests

1. **Fork the repository** and create your branch from `master`
2. **Make your changes**:
   - Follow the existing code style
   - Add tests if applicable
   - Update documentation as needed
3. **Test your changes**:
   - Ensure all existing tests pass
   - Add new tests for new functionality
   - Test manually in the IDE
4. **Commit your changes**:
   - Use clear and meaningful commit messages
   - Reference relevant issues in commits
5. **Submit a pull request**:
   - Fill out the PR template completely
   - Link to related issues
   - Include screenshots for UI changes

## Development Setup

### Prerequisites

- **IntelliJ IDEA**: 2022.1 or later
- **Java**: 17 or later (Java 21 recommended)
- **Erlang/OTP**: Any recent version (for testing)
- **Git**: For version control

### Setting Up Your Development Environment

1. **Clone the repository**:
   ```bash
   git clone https://github.com/ignatov/intellij-erlang.git
   cd intellij-erlang
   ```

2. **Import the project**:
   - Open IntelliJ IDEA
   - Select **File** → **Open**
   - Navigate to the cloned repository
   - Select the `build.gradle` file
   - Choose **Open as Project**

3. **Wait for Gradle sync** to complete

4. **Run the plugin**:
   - Use the **Gradle** tool window
   - Navigate to **Tasks** → **intellij** → **runIde**
   - Double-click to run

### Known Issues

⚠️ **Build System**: The project currently has issues with the Gradle build configuration. See [KNOWN_ISSUES.md](KNOWN_ISSUES.md) for details.

## Development Workflow

### Testing

```bash
# Run all tests
./gradlew test

# Run specific test suite
./gradlew test --tests "org.intellij.erlang.*"

# Run tests with compilation checks
./gradlew testCompilation
```

### Building

```bash
# Build the plugin
./gradlew buildPlugin

# The plugin ZIP will be in build/distributions/
```

### Debugging

1. Set breakpoints in your code
2. Run **Gradle** → **Tasks** → **intellij** → **runIde** in debug mode
3. The IDE will open with the plugin loaded and debugging enabled

## Code Style

- Follow the existing code style in the project
- Use meaningful variable and method names
- Add comments for complex logic
- Don't include author or timestamp comments
- Keep methods focused and concise

## Commit Message Guidelines

- Use the present tense ("Add feature" not "Added feature")
- Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
- Limit the first line to 72 characters
- Reference issues and pull requests when relevant
- Consider starting the commit message with an applicable prefix:
  - `feat:` - New feature
  - `fix:` - Bug fix
  - `docs:` - Documentation changes
  - `style:` - Code style changes (formatting, etc.)
  - `refactor:` - Code refactoring
  - `test:` - Adding or updating tests
  - `chore:` - Maintenance tasks

## Resources

### Documentation

- [Development Guide](docs/DEVELOPMENT.md) - Detailed development information
- [IntelliJ Platform SDK](https://plugins.jetbrains.com/docs/intellij/welcome.html) - Official SDK documentation
- [Gradle IntelliJ Plugin](https://plugins.jetbrains.com/docs/intellij/tools-gradle-intellij-plugin.html) - Build tool documentation

### Community

- [Google Group](https://groups.google.com/d/forum/intellij-erlang-dev) - Development discussions
- [GitHub Issues](https://github.com/ignatov/intellij-erlang/issues) - Bug reports and feature requests
- [JetBrains Plugin Page](https://plugins.jetbrains.com/plugin/7083-erlang) - Plugin marketplace

### Tips and Tricks

- [Plugin Development Tips](http://tomaszdziurko.pl/2011/09/developing-plugin-intellij-idea-some-tips-and-links/)
- [IntelliJ Platform Explorer](https://plugins.jetbrains.com/intellij-platform-explorer) - Explore platform APIs

## Questions?

If you have questions about contributing:
1. Check the [Development Guide](docs/DEVELOPMENT.md)
2. Search existing [GitHub issues](https://github.com/ignatov/intellij-erlang/issues)
3. Ask on the [Google Group](https://groups.google.com/d/forum/intellij-erlang-dev)
4. Open a new issue with the `question` label

## License

By contributing, you agree that your contributions will be licensed under the same [Apache License 2.0](LICENCE) that covers the project.

---

Happy hacking! 🚀
