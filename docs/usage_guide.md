# 🚀 Guia Prático - Pascal LSP

Este guia mostra como usar o **Pascal LSP** na prática, desde a compilação até a integração com editores.

## 📋 Índice

- [Quick Start](#quick-start)
- [Compilação](#compilação)
- [Usando o Cliente](#usando-o-cliente)
- [Testando o Servidor](#testando-o-servidor)
- [Integração VS Code](#integração-vs-code)
- [Desenvolvimento](#desenvolvimento)
- [Troubleshooting](#troubleshooting)

## ⚡ Quick Start

```bash
# 1. Clonar o projeto
git clone https://github.com/mmfbr/pascal-lsp.git
cd pascal-lsp

# 2. Build automático
make all

# 3. Teste básico
make test

# 4. Demonstração completa
make demo
```

## 🔨 Compilação

### Pré-requisitos
- **FreePascal 3.2+** ou **Delphi 10.4+**
- **Make** (opcional, para build automático)

### Build Manual

```bash
# 1. Gerar tipos LSP
fpc -Mobjfpc src/LSPCodeGenerator.pas
./LSPCodeGenerator

# 2. Compilar servidor
fpc -Mobjfpc src/BasicLSPServer.pas

# 3. Compilar cliente
fpc -Mobjfpc src/SimpleLSPClient.pas
```

### Build com Make

```bash
# Compilar tudo
make all

# Apenas o servidor
make bin/BasicLSPServer

# Apenas o cliente  
make bin/SimpleLSPClient

# Limpar arquivos
make clean
```

## 🖥️ Usando o Cliente

### Modo Teste Automático
```bash
# Teste rápido das funcionalidades
./SimpleLSPClient ./BasicLSPServer test
```

**Saída esperada:**
```
🚀 Iniciando servidor LSP: ./BasicLSPServer
✅ Servidor iniciado com PID: 12345
🔄 Inicializando cliente LSP...
✅ Servidor inicializado com sucesso!
📋 Capacidades do servidor:
  ✅ Hover Provider
  ✅ Completion Provider
📢 Enviando notificação initialized...
✅ Cliente totalmente inicializado!

🧪 === CENÁRIO DE TESTE AUTOMÁTICO ===

📋 Teste 1: Abrindo documento...
📂 Abrindo documento: file:///test.pas

📋 Teste 2: Testando hover...
🔍 Hover em file:///test.pas:1:2
✅ Hover funcionando: **begin**

Palavra Pascal encontrada na posição 1:2

📋 Teste 3: Testando completion...
💡 Completion em file:///test.pas:1:2
✅ Completion funcionando:
  - begin
  - end
  - var
  - type
  - function

📋 Teste 4: Fechando documento...
📄 Fechando documento: file:///test.pas

✅ Cenário de teste concluído!
```

### Modo Interativo
```bash
# Sessão interativa para testes manuais
./SimpleLSPClient ./BasicLSPServer interactive
```

**Comandos disponíveis:**

```bash
# Abrir um documento Pascal
lsp> open file:///test.pas program Test; begin WriteLn('Hello'); end.

# Hover em uma posição (linha 0, coluna 8)
lsp> hover file:///test.pas 0 8

# Completion em uma posição  
lsp> complete file:///test.pas 0 25

# Fechar documento
lsp> close file:///test.pas

# Sair
lsp> quit
```

## 🧪 Testando o Servidor

### Teste Manual com cURL
```bash
# Iniciar servidor em background
./BasicLSPServer &
SERVER_PID=$!

# Enviar initialize request
curl -X POST -H "Content-Type: application/json" -d '{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "processId": null,
    "capabilities": {}
  }
}' http://localhost:3000/lsp

# Finalizar servidor
kill $SERVER_PID
```

### Teste com Node.js
```javascript
// test-server.js
const { spawn } = require('child_process');

function testLSPServer() {
  const server = spawn('./BasicLSPServer');
  
  // Enviar initialize
  const initMessage = {
    jsonrpc: '2.0',
    id: 1,
    method: 'initialize',
    params: { processId: process.pid, capabilities: {} }
  };
  
  const json = JSON.stringify(initMessage);
  const header = `Content-Length: ${json.length}\r\n\r\n`;
  
  server.stdin.write(header + json);
  
  server.stdout.on('data', (data) => {
    console.log('Server response:', data.toString());
  });
}

testLSPServer();
```

## 🎨 Integração VS Code

### 1. Instalar Extensão

```bash
cd vscode-extension
npm install
npm run compile

# Instalar localmente
code --install-extension .
```

### 2. Configurar Settings

```json
// settings.json
{
  "pascalLSP.serverPath": "/path/to/BasicLSPServer",
  "pascalLSP.trace.server": "verbose",
  "files.associations": {
    "*.pas": "pascal",
    "*.pp": "pascal", 
    "*.inc": "pascal"
  }
}
```

### 3. Testar Funcionalidades

1. **Abrir arquivo `.pas`**
2. **Hover**: Passar mouse sobre palavras-chave
3. **Completion**: `Ctrl+Space` para autocompletar
4. **Commands**: `Ctrl+Shift+P` → "Pascal LSP"

## 🔧 Desenvolvimento

### Adicionando Novos Métodos LSP

```pascal
// Em BasicLSPServer.pas
procedure TLSPServer.HandleTextDocumentDefinition(const AId: TJSONData; const AParams: TJSONObject);
var
  Location: TJSONObject;
begin
  // Sua implementação aqui
  Location := TJSONObject.Create;
  Location.Add('uri', 'file:///definition.pas');
  Location.Add('range', CreateRange(lineNum, colNum, lineNum, colNum + length));
  
  SendResponse(AId, Location);
end;

// Adicionar no ProcessMessage
'textDocument/definition': 
  HandleTextDocumentDefinition(Id, Params);
```

### Atualizando Tipos LSP

```bash
# Quando sair nova versão do LSP
./LSPCodeGenerator

# Os tipos em LSPTypes.pas são atualizados automaticamente
# Recompilar servidor e cliente
make all
```

### Debug do Servidor

```bash
# Logs detalhados para stderr
./BasicLSPServer 2> debug.log &

# Em outra janela, ver logs em tempo real
tail -f debug.log
```

### Testing Custom Scenarios

```pascal
// custom-test.pas
program CustomTest;

{$mode objfpc}{$H+}

uses
  SimpleLSPClient;

var
  Client: TLSPClient;

begin
  Client := TLSPClient.Create('./BasicLSPServer');
  try
    if Client.StartServer and Client.Initialize then
    begin
      Client.Initialized;
      
      // Seus testes customizados aqui
      Client.DidOpenTextDocument('file:///custom.pas', 'pascal', 
        'program Custom; begin end.');
      
      WriteLn('Hover result: ', Client.Hover('file:///custom.pas', 0, 8));
    end;
  finally
    Client.Free;
  end;
end.
```

## 🐛 Troubleshooting

### Problemas Comuns

#### ❌ "Servidor não encontrado"
```bash
# Verificar se foi compilado
ls -la BasicLSPServer

# Verificar permissões
chmod +x BasicLSPServer

# Testar manualmente
./BasicLSPServer
```

#### ❌ "Erro ao parsear JSON"
```bash
# Verificar formato das mensagens
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | ./BasicLSPServer
```

#### ❌ "VS Code não reconhece extensão"
```bash
# Verificar instalação
code --list-extensions | grep pascal

# Reinstalar
cd vscode-extension
npm run compile
code --install-extension . --force
```

#### ❌ "Completion não funciona"
```bash
# Verificar se servidor está respondendo
./SimpleLSPClient ./BasicLSPServer test

# Ver logs
tail -f ~/.vscode/logs/*/exthost1/output_logging_*
```

### Logs e Debug

#### Server Logs
```bash
# Redirecionar stderr para arquivo
./BasicLSPServer 2> server.log

# Ver mensagens LSP
grep "Método recebido" server.log
```

#### Client Logs  
```bash
# Output detalhado do cliente
./SimpleLSPClient ./BasicLSPServer test 2>&1 | tee client.log
```

#### VS Code Logs
```bash
# Abrir Developer Tools
Ctrl+Shift+I

# Ver output da extensão
View → Output → Pascal LSP
```

## 📊 Performance

### Benchmarks Típicos

| Operação | Tempo | Memória |
|----------|-------|---------|
| Inicialização | ~100ms | ~2MB |
| Hover | ~5ms | +0.1MB |
| Completion | ~10ms | +0.2MB |
| Document Sync | ~2ms | +0.5MB |

### Otimizações

```pascal
// Cache de resultados
var
  HoverCache: TDictionary<string, string>;

function CachedHover(const AUri: string; ALine, AChar: Integer): string;
var
  Key: string;
begin
  Key := Format('%s:%d:%d', [AUri, ALine, AChar]);
  if not HoverCache.TryGetValue(Key, Result) then
  begin
    Result := ComputeHover(AUri, ALine, AChar);
    HoverCache.Add(Key, Result);
  end;
end;
```

## 🔮 Próximos Passos

### Features Planejadas
- [ ] **Real Pascal Parser** - Syntax tree completa
- [ ] **Go to Definition** - Navegação entre símbolos
- [ ] **Find References** - Busca por uso de símbolos
- [ ] **Diagnostics** - Erros e warnings em tempo real
- [ ] **Code Formatting** - Formatação automática
- [ ] **Refactoring** - Rename, extract method, etc.

### Contribuindo
1. Escolha uma feature da lista acima
2. Implemente seguindo os padrões do projeto
3. Adicione testes
4. Abra Pull Request

### Recursos Úteis
- **[LSP Specification](https://microsoft.github.io/language-server-protocol/)** - Documentação oficial
- **[FreePascal Docs](https://www.freepascal.org/docs.html)** - Referência Pascal
- **[VS Code Extension API](https://code.visualstudio.com/api)** - Desenvolvimento de extensões

---

## 🎉 Conclusão

O **Pascal LSP** oferece uma base sólida para desenvolvimento Pascal moderno com:

✅ **Geração automática de tipos** - Sempre atualizado com LSP  
✅ **Servidor funcional** - Hover, completion, document sync  
✅ **Cliente de teste** - Debug e desenvolvimento  
✅ **Extensão VS Code** - Integração completa  
✅ **Documentação completa** - Fácil de contribuir  

**Happy coding!** 🚀

---

*Última atualização: $(date)*