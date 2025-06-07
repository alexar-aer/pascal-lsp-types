# Contributing to Pascal LSP 🚀

First off, **thank you** for considering contributing to Pascal LSP! It's people like you that make this project a great tool for the Pascal community. 

This guide will help you get started with contributing to the project, whether you're fixing bugs, adding features, improving documentation, or helping with testing.

## 📋 Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Environment](#development-environment)
- [How to Contribute](#how-to-contribute)
- [Coding Standards](#coding-standards)
- [Testing Guidelines](#testing-guidelines)
- [Documentation](#documentation)
- [Pull Request Process](#pull-request-process)
- [Release Process](#release-process)
- [Community](#community)

## 🤝 Code of Conduct

This project and everyone participating in it is governed by our commitment to creating a welcoming and inclusive environment. By participating, you are expected to uphold these values:

- **Be respectful** and inclusive of different viewpoints and experiences
- **Be collaborative** and help others learn and grow
- **Be patient** with newcomers and those learning
- **Give constructive feedback** and accept it gracefully
- **Focus on what's best** for the community and the project

## 🚀 Getting Started

### Prerequisites

Before you begin, ensure you have the following installed:

- **FreePascal 3.2+** or **Delphi 10.4+**
- **Git** for version control
- **Node.js 16+** (for VS Code extension development)
- **VS Code** (recommended for testing)

### Quick Setup

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/mmfbr/pascal-lsp-types.git
   cd pascal-lsp
   ```
3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/originalowner/pascal-lsp-types.git
   ```
4. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

## 🛠️ Development Environment

### Project Structure
```
pascal-lsp/
├── src/                          # Core Pascal source files
│   ├── LSPCodeGenerator.pas      # Generates LSP types from metaModel
│   ├── BasicLSPServer.pas        # Main LSP server implementation
│   ├── LSPTypes.pas              # Generated LSP types (auto-created)
│   └── utils/                    # Utility units
├── vscode-extension/             # VS Code extension
│   ├── src/extension.ts          # Extension entry point
│   ├── package.json              # Extension manifest
│   └── language-configuration.json
├── tests/                        # Test files and scripts
│   ├── unit/                     # Unit tests
│   ├── integration/              # Integration tests
│   └── examples/                 # Test Pascal files
├── docs/                         # Documentation
└── scripts/                      # Build and utility scripts
```

### Building the Project

1. **Generate LSP Types**:
   ```bash
   cd src
   fpc -Mobjfpc LSPCodeGenerator.pas
   ./LSPCodeGenerator
   ```

2. **Build LSP Server**:
   ```bash
   fpc -Mobjfpc BasicLSPServer.pas
   ```

3. **Build VS Code Extension**:
   ```bash
   cd vscode-extension
   npm install
   npm run compile
   ```

4. **Run Tests**:
   ```bash
   # Unit tests
   cd tests/unit
   fpc -Mobjfpc TestRunner.pas && ./TestRunner
   
   # Integration tests
   cd tests/integration
   node test-server.js
   ```

## 🤔 How to Contribute

### 🐛 Reporting Bugs

**Before creating a bug report**, please check existing issues to avoid duplicates.

When creating a bug report, include:

- **Clear title** describing the issue
- **Steps to reproduce** the problem
- **Expected behavior** vs **actual behavior**
- **Pascal code sample** that demonstrates the issue
- **Environment details**:
  - OS (Windows/Linux/macOS)
  - Pascal compiler (FreePascal version or Delphi version)
  - VS Code version (if relevant)
  - Extension version

**Bug Report Template**:
```markdown
## Bug Description
A clear description of what the bug is.

## Steps to Reproduce
1. Open file `example.pas`
2. Place cursor on line 10
3. Press Ctrl+Space
4. See error

## Expected Behavior
Should show completion suggestions for Pascal keywords.

## Actual Behavior
Shows no suggestions and displays error in output.

## Environment
- OS: Windows 11
- FreePascal: 3.2.2
- VS Code: 1.85.0
- Extension: 1.0.0

## Code Sample
```pascal
program TestCase;
begin
  // Cursor here, Ctrl+Space should show completions
end.
```
```

### ✨ Suggesting Features

**Before suggesting a feature**, check if it's already planned in our [Roadmap](README.md#roadmap).

Feature requests should include:

- **Clear description** of the proposed feature
- **Use case** - why would this be useful?
- **Proposed implementation** (if you have ideas)
- **Priority level** (nice-to-have vs essential)

### 💻 Contributing Code

#### Types of Contributions Welcome

1. **Core LSP Features**:
   - Document parsing and analysis
   - Hover information improvements
   - Code completion enhancements
   - Go-to-definition implementation
   - Find references functionality
   - Diagnostics (syntax/semantic errors)

2. **Server Improvements**:
   - Performance optimizations
   - Memory usage improvements
   - Better error handling
   - Additional LSP methods

3. **VS Code Extension**:
   - Configuration options
   - Commands and shortcuts
   - UI improvements
   - Testing and debugging features

4. **Documentation**:
   - Code comments
   - User guides
   - API documentation
   - Examples and tutorials

5. **Testing**:
   - Unit tests for Pascal code
   - Integration tests for LSP protocol
   - VS Code extension tests
   - Performance benchmarks

## 📏 Coding Standards

### Pascal Code Style

#### Naming Conventions
```pascal
// Types: PascalCase with T prefix
type
  TMyClass = class
  TMyRecord = record
  TMyEnum = (meValue1, meValue2);

// Constants: PascalCase with C prefix
const
  CMaxBufferSize = 1024;
  CDefaultTimeout = 5000;

// Variables: PascalCase
var
  MyVariable: Integer;
  SomeText: string;
  IsEnabled: Boolean;

// Functions/Procedures: PascalCase
function CalculateSum(const AValue1, AValue2: Integer): Integer;
procedure ProcessDocument(const ADocument: TDocument);

// Parameters: PascalCase with A prefix
function DoSomething(const AInput: string; ACount: Integer): Boolean;
```

#### Code Formatting
```pascal
// Use consistent indentation (2 spaces recommended)
if Condition then
begin
  DoSomething;
  if AnotherCondition then
    DoSomethingElse;
end;

// Align assignments when appropriate
MyVar := 123;
LongerVariableName := 456;
ShortVar := 789;

// Use meaningful variable names
var
  DocumentContent: string;  // Good
  s: string;               // Avoid
  
// Comment complex logic
// Parse JSON-RPC message according to LSP specification
JsonData := GetJSON(MessageContent);
if JsonData is TJSONObject then
begin
  // Process the request/response/notification
  ProcessLSPMessage(TJSONObject(JsonData));
end;
```

#### Error Handling
```pascal
// Always handle exceptions appropriately
try
  ProcessDocument(Document);
except
  on E: EParserError do
  begin
    WriteLn(StdErr, 'Parser error: ', E.Message);
    SendLSPError(RequestId, -32700, 'Parse error');
  end;
  on E: Exception do
  begin
    WriteLn(StdErr, 'Unexpected error: ', E.Message);
    SendLSPError(RequestId, -32603, 'Internal error');
  end;
end;
```

### TypeScript/JavaScript Code Style

For VS Code extension development:

```typescript
// Use consistent formatting (Prettier recommended)
export function activate(context: vscode.ExtensionContext): void {
    const client = createLanguageClient();
    
    client.start().then(() => {
        console.log('Pascal LSP client started');
    }).catch((error) => {
        console.error('Failed to start client:', error);
    });
}

// Use meaningful names and document complex functions
/**
 * Creates and configures the Language Server Protocol client
 */
function createLanguageClient(): LanguageClient {
    // Implementation
}
```

### Documentation Standards

- **All public functions** must have documentation comments
- **Complex algorithms** should be explained
- **Usage examples** for non-trivial functions
- **LSP protocol references** where applicable

```pascal
{**
 * Handles LSP textDocument/hover requests
 * 
 * @param AId Request ID for JSON-RPC response
 * @param AParams Request parameters containing document URI and position
 * 
 * Implements LSP 3.17 specification:
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
 *}
procedure HandleTextDocumentHover(const AId: TJSONData; const AParams: TJSONObject);
```

## 🧪 Testing Guidelines

### Unit Tests

Create unit tests for all new functionality:

```pascal
// tests/unit/TestLSPTypes.pas
unit TestLSPTypes;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, LSPTypes;

type
  TTestLSPTypes = class(TTestCase)
  published
    procedure TestPositionCreation;
    procedure TestRangeValidation;
  end;

implementation

procedure TTestLSPTypes.TestPositionCreation;
var
  Position: TPosition;
begin
  Position.Line := 10;
  Position.Character := 5;
  
  AssertEquals('Line should be 10', 10, Position.Line);
  AssertEquals('Character should be 5', 5, Position.Character);
end;

procedure TTestLSPTypes.TestRangeValidation;
var
  Range: TRange;
begin
  Range.Start.Line := 0;
  Range.Start.Character := 0;
  Range.End_.Line := 1;
  Range.End_.Character := 10;
  
  AssertTrue('Range should be valid', IsValidRange(Range));
end;

initialization
  RegisterTest(TTestLSPTypes);
end.
```

### Integration Tests

Test LSP protocol communication:

```javascript
// tests/integration/test-hover.js
const { testLSPServer } = require('./utils/lsp-test-utils');

describe('Hover functionality', () => {
    test('should provide hover information for Pascal keywords', async () => {
        const response = await testLSPServer({
            method: 'textDocument/hover',
            params: {
                textDocument: { uri: 'file:///test.pas' },
                position: { line: 0, character: 5 }
            }
        });
        
        expect(response.result).toBeDefined();
        expect(response.result.contents).toContain('Pascal');
    });
});
```

### Manual Testing

Before submitting a PR, manually test:

1. **Generator**: Run `LSPCodeGenerator` and verify `LSPTypes.pas` is created correctly
2. **Server**: Test basic LSP communication (initialize, hover, completion)
3. **VS Code**: Install extension and test with real Pascal files
4. **Cross-platform**: Test on Windows, Linux, and macOS if possible

## 📚 Documentation

### Contributing to Documentation

- **README.md**: Project overview and quick start
- **API documentation**: In-code comments for public APIs
- **User guides**: Step-by-step tutorials in `/docs`
- **Examples**: Working code samples in `/examples`

### Writing Guidelines

- **Clear and concise** language
- **Code examples** for all features
- **Screenshots** for UI features (VS Code extension)
- **Cross-references** to LSP specification when relevant

## 🔄 Pull Request Process

### Before Submitting

1. **Update from upstream**:
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

2. **Run all tests**:
   ```bash
   # Unit tests
   cd tests && make test
   
   # Integration tests
   cd tests/integration && npm test
   
   # Manual testing
   ./scripts/test-manual.sh
   ```

3. **Update documentation** if needed

4. **Check code style**:
   ```bash
   # Pascal code (manual review)
   # TypeScript code
   cd vscode-extension && npm run lint
   ```

### PR Guidelines

- **Descriptive title**: Summarize what the PR does
- **Detailed description**: Explain why the change is needed
- **Link issues**: Reference related issues with "Fixes #123"
- **Test coverage**: Describe how you tested the changes
- **Breaking changes**: Clearly mark any breaking changes

**PR Template**:
```markdown
## Description
Brief description of what this PR does.

## Related Issues
Fixes #123
Related to #456

## Changes Made
- Added hover support for user-defined types
- Improved error handling in JSON parsing
- Updated VS Code extension to show diagnostics

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Manual testing completed
- [ ] Cross-platform testing (if applicable)

## Breaking Changes
None / [Description of breaking changes]

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] Tests added for new functionality
```

### Review Process

1. **Automated checks** must pass (CI/CD)
2. **Code review** by at least one maintainer
3. **Testing** on different platforms (if relevant)
4. **Documentation** review
5. **Final approval** and merge

## 🚀 Release Process

### Versioning

We use [Semantic Versioning](https://semver.org/):

- **MAJOR** version: Breaking changes
- **MINOR** version: New features (backward compatible)
- **PATCH** version: Bug fixes (backward compatible)

### Release Steps

1. **Update version numbers**:
   - VS Code extension `package.json`
   - Pascal code version constants
   - Documentation references

2. **Update CHANGELOG.md**:
   - List all changes since last release
   - Categorize: Added, Changed, Deprecated, Removed, Fixed, Security

3. **Create release PR**:
   - Title: "Release v1.2.3"
   - Include changelog in description

4. **Tag and release**:
   ```bash
   git tag v1.2.3
   git push origin v1.2.3
   ```

5. **Publish packages**:
   - VS Code Marketplace (if applicable)
   - GitHub Releases with binaries

## 🌍 Community

### Getting Help

- **GitHub Discussions**: For questions and general discussion
- **GitHub Issues**: For bug reports and feature requests
- **Discord/Slack**: [Link to community chat if available]

### Recognition

Contributors are recognized in:

- **README.md** contributors section
- **Release notes** for significant contributions
- **Hall of Fame** for major contributors

### Mentoring

New contributors are welcome! Don't hesitate to ask for help:

- **Good first issues**: Look for `good-first-issue` label
- **Mentor available**: Issues marked with `mentor-available`
- **Pair programming**: Available for complex features

## 🎉 Thank You!

Your contributions make Pascal LSP better for everyone. Whether you're fixing a small typo or implementing a major feature, every contribution is valuable and appreciated.

**Happy coding!** 🚀

---

## 📞 Contact

- **Project Maintainer**: [Your GitHub Profile](https://github.com/mmfbr)
- **Email**: [your.email@example.com] (for security issues)
- **Discord**: [Community Discord Server]

---

*This document is a living guide and may be updated as the project evolves. Last updated: [Current Date]*