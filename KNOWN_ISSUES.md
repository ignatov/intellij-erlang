# Known Issues

## Build System

### IntelliJ Platform Gradle Plugin Dependency Resolution

**Status**: Open  
**Severity**: High  
**Affects**: All builds using Gradle

**Description**:
The project currently uses `org.jetbrains.intellij.platform` plugin version 2.5.0, which has issues resolving the IntelliJ IDEA Community dependency. The dependency `idea:ideaIC:251.23774.318` (or any other version format tried) fails to resolve from the configured repositories.

**Error Message**:
```
[org.jetbrains.intellij.platform] Configuration 'intellijPlatformDependency' has some resolution errors.
No IntelliJ Platform dependency found.
```

**Investigation Findings**:
1. The issue exists in both plugin version 2.5.0 and 2.1.0
2. The problem occurs even with minimal build configurations
3. Repository configuration appears correct with `defaultRepositories()`
4. Various version formats attempted: `251.23774.318`, `2024.3`, etc.
5. The dependency coordinates `idea:ideaIC:*` consistently fail to resolve

**Possible Causes**:
- Bug in the IntelliJ Platform Gradle Plugin 2.x series
- Incorrect repository configuration or credentials required
- Version string format mismatch between plugin and repository
- Network/firewall issues preventing artifact download

**Workarounds**:
None currently identified. The build system needs further investigation.

**Next Steps**:
1. Check if plugin version 1.x series works (older stable version)
2. Review IntelliJ Platform Gradle Plugin documentation for version 2.5.0
3. Test with explicit repository URLs instead of `defaultRepositories()`
4. Contact JetBrains support or check plugin issue tracker
5. Consider alternative build approaches if plugin is fundamentally broken

**Impact**:
- Unable to build the plugin from source
- Cannot run tests
- Cannot create distributable plugin artifacts
- Blocks all development work

**References**:
- [IntelliJ Platform Gradle Plugin Documentation](https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin.html)
- [Gradle IntelliJ Plugin GitHub](https://github.com/JetBrains/intellij-platform-gradle-plugin)

## Other Issues

### Travis CI Configuration
**Status**: Informational  
**Note**: The project includes a `.travis.yml` configuration file. Travis CI has changed its pricing model and may no longer be accessible for this project. The new GitHub Actions workflow (`.github/workflows/ci.yml`) provides an alternative CI solution.

**Recommendation**: Consider removing `.travis.yml` if Travis CI is no longer in use.
