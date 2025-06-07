# Pascal LSP Types 🚀

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)
[![Pascal](https://img.shields.io/badge/Language-Pascal-blue.svg)](https://www.freepascal.org/)
[![LSP](https://img.shields.io/badge/Protocol-LSP%203.17-orange.svg)](https://microsoft.github.io/language-server-protocol/)
[![FreePascal](https://img.shields.io/badge/Compiler-FreePascal-red.svg)](https://www.freepascal.org/)
[![Delphi](https://img.shields.io/badge/Compiler-Delphi-red.svg)](https://www.embarcadero.com/products/delphi)

> **Language Server Protocol (LSP) implementation for Pascal/Delphi** - Bringing modern IDE features to Pascal development!

## ✨ Features

### 🎯 **Code Generator**
- 🔄 **Auto-generates LSP types** from official Microsoft metaModel.json
- 📚 **Preserves documentation** - All comments and descriptions maintained
- 🔮 **Future-proof** - Automatically supports new LSP versions
- 🎨 **Clean Pascal code** - Generates readable, well-structured types

### 🖥️ **LSP Server**
- ✅ **Document Synchronization** - Real-time file tracking
- 🔍 **Hover Information** - Context-aware help on symbols
- ⚡ **Code Completion** - Smart autocomplete for Pascal keywords
- 🌐 **JSON-RPC Communication** - Standard LSP protocol support
- 🔧 **Extensible Architecture** - Easy to add new features

### 🎨 **VS Code Integration**
- 📁 **File Support** - `.pas`, `.pp`, `.inc` files
- 🎛️ **Configuration** - Customizable server settings
- 🔄 **Server Management** - Start/stop/restart commands
- 📊 **Output Channel** - Debug and logging information

## 🚀 Quick Start

### Prerequisites
- **FreePascal** 3.2+ or **Delphi** 10.4+
- **Node.js** 16+ (for VS Code extension)
- **VS Code** 1.60+ (optional, for editor integration)

### 1. Generate LSP Types
```bash
# Clone the repository
git clone https://github.com/mmfbr/pascal-lsp-types.git
cd pascal-lsp

# Compile and run the generator
fpc -Mobjfpc src/LSPCodeGenerator.pas
./LSPCodeGenerator

# This creates LSPTypes.pas with all LSP 3.17 types
```

### 2. Build LSP Server
```bash
# Compile the server
fpc -Mobjfpc src/BasicLSPServer.pas

# Test the server
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ./BasicLSPServer
```

### 3. Install VS Code Extension
```bash
cd vscode-extension
npm install
npm run compile

# Install locally
code --install-extension .
```

## 📖 Usage Examples

### Basic Pascal File
```pascal
program HelloLSP;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  // Hover over TPerson to see documentation
  TPerson = record
    Name: string;
    Age: Integer;
  end;

var
  Person: TPerson;

begin
  // Type "beg" and see completion suggesting "begin"
  Person.Name := 'Pascal Developer';
  Person.Age := 30;
  
  WriteLn('Hello from ', Person.Name);
  // Hover over Person to see type information
end.
```

### LSP Server Output
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "capabilities": {
      "textDocumentSync": 2,
      "hoverProvider": true,
      "completionProvider": {
        "triggerCharacters": [".", ":"]
      }
    },
    "serverInfo": {
      "name": "Basic Pascal LSP",
      "version": "1.0.0"
    }
  }
}
```

## 🏗️ Architecture

```
pascal-lsp/
├── src/
│   ├── LSPCodeGenerator.pas     # Generates types from metaModel.json
│   ├── BasicLSPServer.pas       # Core LSP server implementation
│   └── LSPTypes.pas            # Generated LSP types (auto-created)
├── vscode-extension/
│   ├── src/extension.ts        # VS Code client
│   ├── package.json           # Extension manifest
│   └── language-configuration.json
├── examples/
│   ├── test.pas              # Example Pascal files
│   └── server-test.js        # Server testing script
└── docs/
    ├── CONTRIBUTING.md
    └── API.md
```

## 🔧 Configuration

### VS Code Settings
```json
{
  "pascalLSP.serverPath": "./BasicLSPServer",
  "pascalLSP.trace.server": "verbose"
}
```

### Server Capabilities
```pascal
// Current LSP 3.17 support
capabilities := TJSONObject.Create([
  'textDocumentSync', 2,           // ✅ Incremental sync
  'hoverProvider', True,           // ✅ Hover information  
  'completionProvider', TJSONObject.Create([
    'triggerCharacters', TJSONArray.Create(['.', ':'])
  ])
]);
```

## 🛠️ Development

### Adding New Features
1. **Extend the server**: Add handlers in `BasicLSPServer.pas`
2. **Use generated types**: All LSP types available in `LSPTypes.pas`
3. **Test thoroughly**: Use the provided test scripts

### Example: Adding Go-to-Definition
```pascal
procedure TLSPServer.HandleTextDocumentDefinition(const AId: TJSONData; const AParams: TJSONObject);
var
  Location: TJSONObject;
begin
  // Your implementation here
  Location := TJSONObject.Create;
  Location.Add('uri', 'file:///path/to/definition.pas');
  Location.Add('range', CreateRange(lineNum, colNum, lineNum, colNum + length));
  
  SendResponse(AId, Location);
end;
```

### Regenerating Types
```bash
# When LSP specification updates
./LSPCodeGenerator  # Downloads latest metaModel.json
# LSPTypes.pas is automatically updated with new types
```

## 🗺️ Roadmap

### 🎯 **Phase 1: Foundation** (✅ Complete)
- [x] Type generation from metaModel.json
- [x] Basic LSP server structure
- [x] VS Code extension
- [x] Document synchronization
- [x] Hover provider
- [x] Basic completion

### 🚧 **Phase 2: Core Features** (In Progress)
- [ ] **Real Pascal parsing** (using FPC parser or custom)
- [ ] **Go to Definition** 
- [ ] **Find References**
- [ ] **Document Symbols** (outline view)
- [ ] **Diagnostics** (syntax errors, warnings)
- [ ] **Code Formatting**

### 🔮 **Phase 3: Advanced Features** (Planned)
- [ ] **Semantic highlighting**
- [ ] **Rename symbol**
- [ ] **Code actions** (quick fixes)
- [ ] **Signature help** (function parameters)
- [ ] **Workspace symbols** (project-wide search)
- [ ] **Call hierarchy**
- [ ] **Type hierarchy**

### 🎨 **Phase 4: IDE Integration** (Future)
- [ ] **Lazarus integration**
- [ ] **Delphi IDE plugin**
- [ ] **Vim/Neovim support**
- [ ] **Emacs support**
- [ ] **Web-based editor**

## 🤝 Contributing

We welcome contributions! Here's how you can help:

### 🐛 **Bug Reports**
- Use GitHub Issues
- Include Pascal code samples
- Describe expected vs actual behavior

### ✨ **Feature Requests**
- Check existing issues first
- Describe the use case
- Consider implementation complexity

### 💻 **Code Contributions**
1. **Fork** the repository
2. **Create** a feature branch: `git checkout -b feature/amazing-feature`
3. **Commit** your changes: `git commit -m 'Add amazing feature'`
4. **Push** to the branch: `git push origin feature/amazing-feature`
5. **Open** a Pull Request

### 📝 **Development Setup**
```bash
# Clone your fork
git clone https://github.com/mmfbr/pascal-lsp-types.git
cd pascal-lsp

# Install dependencies
cd vscode-extension && npm install

# Build everything
make build  # or manually compile Pascal files

# Run tests
make test
```

## 📚 Documentation

- **[LSP Specification](https://microsoft.github.io/language-server-protocol/)** - Official protocol docs
- **[FreePascal Manual](https://www.freepascal.org/docs.html)** - Pascal language reference
- **[VS Code Extensions](https://code.visualstudio.com/api)** - Extension development guide
- **[Contributing Guide](docs/CONTRIBUTING.md)** - Detailed contribution guidelines

## 🌟 Examples & Tutorials

### Basic Server Usage
```bash
# Start server manually
./BasicLSPServer

# Send initialize request
curl -X POST -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' 
```

### Custom Type Generation
```pascal
// After running LSPCodeGenerator, use generated types:
var
  Position: TPosition;
  Range: TRange;
  
begin
  Position.Line := 10;
  Position.Character := 5;
  
  Range.Start := Position;
  Range.End_ := CreatePosition(10, 15);
end;
```

## 🏆 Credits

### Inspiration
- **[Anders Hejlsberg](https://github.com/ahejlsberg)** - Creator of Pascal, Delphi, C#, and TypeScript
- **[Microsoft LSP Team](https://github.com/microsoft/language-server-protocol)** - LSP specification and tools
- **[FreePascal Team](https://www.freepascal.org/)** - Amazing Pascal compiler

### Contributors
- **[mmfbr](https://github.com/mmfbr)** - Project creator and maintainer
- **[Community Contributors](https://github.com/mmfbr/pascal-lsp-types/contributors)** - Thank you! 🙏

## 📄 License

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

```
MIT License

Copyright (c) 2024 Pascal LSP Contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## 🔗 Links

- **[GitHub Repository](https://github.com/mmfbr/pascal-lsp-types)** - Source code
- **[Issues](https://github.com/mmfbr/pascal-lsp-types/issues)** - Bug reports and feature requests
- **[Discussions](https://github.com/mmfbr/pascal-lsp-types/discussions)** - Community chat
- **[Wiki](https://github.com/mmfbr/pascal-lsp-types/wiki)** - Additional documentation
- **[Releases](https://github.com/mmfbr/pascal-lsp-types/releases)** - Download stable versions

---

<div align="center">

**Made with ❤️ for the Pascal community**

[⭐ Star this project](https://github.com/mmfbr/pascal-lsp-types) | [🐛 Report Bug](https://github.com/mmfbr/pascal-lsp-types/issues) | [✨ Request Feature](https://github.com/mmfbr/pascal-lsp-types/issues)

</div>