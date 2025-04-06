# IntelliJ Erlang Plugin Development Guide

## Official IntelliJ Platform SDK Documentation

This project follows the IntelliJ Platform Plugin SDK guidelines. For comprehensive information on plugin development, refer to:

- [IntelliJ Platform SDK Documentation](https://plugins.jetbrains.com/docs/intellij/welcome.html)
- [IntelliJ Plugin Development Quick Start Guide](https://plugins.jetbrains.com/docs/intellij/plugins-quick-start.html)
- [Gradle IntelliJ Plugin Documentation](https://plugins.jetbrains.com/docs/intellij/tools-gradle-intellij-plugin.html)

## Standard Gradle Commands

The plugin uses the Gradle build system with the Gradle IntelliJ Plugin. Here are the common commands:

```bash
# Build the plugin distribution (zip file)
./gradlew buildPlugin

# Run the tests
./gradlew test

# Run an IDE instance with the plugin installed
./gradlew runIde

# Clean build directory
./gradlew clean

# Verify plugin compatibility with specified IDE version
./gradlew verifyPlugin
```

## Java Version Compatibility Requirements

The IntelliJ Erlang plugin has specific Java version requirements for different components:

### Main Plugin
- Built with Java 17
- Targets Java 17 bytecode (class file version 61.0)
- Compatible with the latest IntelliJ platform

### JPS (JetBrains Project System) Module
- Built with Java 11 compatibility
- Targets Java 11 bytecode (class file version 55.0)
- Required for backward compatibility with older IDEs and Java runtimes

## Why Two Different Java Versions?

The JPS module is loaded by the IntelliJ build system, which may run on various Java versions depending on the IDE version. To ensure maximum compatibility, the JPS module targets Java 11 bytecode.

The main plugin can target the latest Java version supported by the current IntelliJ platform.

## Common Issues

### UnsupportedClassVersionError

If you encounter an error like:

```
java.lang.UnsupportedClassVersionError: org/intellij/erlang/jps/model/JpsErlangModelSerializerExtension has been compiled by a more recent version of the Java Runtime (class file version 61.0), this version of the Java Runtime only recognizes class file versions up to 55.0
```

This indicates a Java version mismatch between the compiled classes and the runtime environment. The solution is to:

1. Ensure the JPS module is compiled with Java 11 compatibility (already configured in build.gradle)
2. Build the plugin using Gradle with the correct JDK version

## Building the Plugin

To build the plugin correctly:

```bash
./gradlew clean build
```

This will:
1. Build the main plugin with Java 17 compatibility
2. Build the JPS module with Java 11 compatibility
3. Package everything together correctly

## Verifying Java Compatibility

The plugin includes Gradle tasks to verify that the JPS module is compiled with Java 11 compatibility. These verifications are automatically run as part of the standard build process, including the `buildPlugin` task.

If you want to run the verification tasks manually:

```bash
# Verify only the JPS module's Java 11 compatibility
./gradlew :jps-plugin:verifyJpsJava11Compatibility

# Verify the compiled class files
./gradlew :jps-plugin:verifyJpsClassFilesJava11Compatible

# Verify the JAR file
./gradlew :jps-plugin:verifyJpsJarJava11Compatible
```

These tasks check the bytecode version of each class file to ensure it's compatible with Java 11 (class file version 55.0 or lower).

### Integration with Build Pipeline

The verification tasks are integrated into the main build pipeline in the following ways:

1. The `check` task depends on JPS verification, ensuring all verification happens during testing
2. The `buildPlugin` task depends on JPS verification, making sure no plugin can be built with incompatible JPS classes
3. The `prepareSandbox` task depends on JPS verification, ensuring sandbox deployments also verify compatibility

This setup ensures that Java compatibility issues will be caught early in the development process, preventing incompatible builds from being released.

### Continuous Integration Check

For CI environments, you can use this command to quickly check JPS module compatibility:

```bash
# Fast check for CI environments
./gradlew :jps-plugin:compileJava :jps-plugin:verifyJpsClassFilesJava11Compatible
```

This will compile the JPS module and then verify the Java 11 compatibility of the resulting class files.

## Related Issues

This setup addresses the following issues:
- #957: Java version compatibility error with JpsErlangModelSerializerExtension
- #976: IntelliJ 2022.1 class version compatibility error
- #1022: Unsupported class version error
- #1054: Intellij Erlang plugin run configuration not working

## Getting Started with Plugin Development

To start contributing to the Erlang plugin:

1. Fork the repository on GitHub
2. Clone your fork locally
3. Import the project as a Gradle project in IntelliJ IDEA
4. Run `./gradlew runIde` to test your changes in a development instance

### Project Structure

- `src/` - Main plugin source code
- `jps-plugin/` - JPS module for build system integration
- `resources/` - Plugin resources (icons, templates, etc.)
- `testData/` - Test data files
- `tests/` - Test source code

### Development Workflow

1. Create a branch for your feature or fix
2. Make your changes
3. Run tests with `./gradlew test`
4. Test the plugin with `./gradlew runIde`
5. Submit a pull request

### Common Development Tasks

- **Adding a new inspection**: Create a class that extends `org.intellij.erlang.inspection.ErlangInspection`
- **Adding a new intention action**: Create a class that implements `org.intellij.erlang.intention.ErlangIntention`
- **Adding new file templates**: Add template files to `resources/fileTemplates/`
- **Modifying the parser**: Update `grammars/erlang.bnf` and run the grammar generator

## Debugging

To debug the plugin:

### Method 1: Using Gradle command line
1. Run `./gradlew runIde --debug-jvm`
2. Connect to the JVM using remote debugging in IntelliJ IDEA
3. Set breakpoints in your code

### Method 2: Using IntelliJ directly (recommended)
1. Open the Gradle tool window in IntelliJ IDEA
2. Navigate to Tasks → intellij → runIde
3. Right-click on runIde and select "Debug 'intellij-erlang [runIde]'"
4. Set breakpoints in your code

This second method is more convenient as it allows you to simply click the debug button on an existing run configuration or create a permanent run configuration with debugging enabled. The debugger will attach automatically, and you can start debugging right away without manual connection steps.