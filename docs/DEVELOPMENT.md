# IntelliJ Erlang Plugin Development Guide

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