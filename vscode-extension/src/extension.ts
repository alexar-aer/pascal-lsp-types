// src/extension.ts
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  console.log('Ativando extensão Pascal LSP...');

  // Configuração do servidor
  const config = vscode.workspace.getConfiguration('pascalLSP');
  const serverPath = config.get<string>('serverPath') || './BasicLSPServer';

  // Opções do servidor LSP
  const serverOptions: ServerOptions = {
    run: { 
      command: serverPath,
      transport: TransportKind.stdio 
    },
    debug: { 
      command: serverPath,
      transport: TransportKind.stdio,
      args: ['--debug']
    }
  };

  // Opções do cliente
  const clientOptions: LanguageClientOptions = {
    // Registrar para arquivos Pascal
    documentSelector: [
      { scheme: 'file', language: 'pascal' },
      { scheme: 'file', pattern: '**/*.pas' },
      { scheme: 'file', pattern: '**/*.pp' },
      { scheme: 'file', pattern: '**/*.inc' }
    ],
    synchronize: {
      // Notificar o servidor sobre mudanças em arquivos Pascal
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{pas,pp,inc}')
    },
    outputChannel: vscode.window.createOutputChannel('Pascal LSP')
  };

  // Criar e iniciar o cliente
  client = new LanguageClient(
    'pascalLSP',
    'Pascal Language Server',
    serverOptions,
    clientOptions
  );

  // Iniciar o cliente (que iniciará o servidor)
  client.start().then(() => {
    console.log('Cliente Pascal LSP iniciado com sucesso!');
    
    // Registrar comandos adicionais
    registerCommands(context);
  }).catch((error) => {
    console.error('Erro ao iniciar Pascal LSP:', error);
    vscode.window.showErrorMessage(`Erro ao iniciar Pascal LSP: ${error.message}`);
  });
}

function registerCommands(context: vscode.ExtensionContext) {
  // Comando para reiniciar o servidor
  const restartCommand = vscode.commands.registerCommand('pascalLSP.restart', async () => {
    vscode.window.showInformationMessage('Reiniciando Pascal LSP...');
    
    try {
      await client.stop();
      await client.start();
      vscode.window.showInformationMessage('Pascal LSP reiniciado com sucesso!');
    } catch (error) {
      vscode.window.showErrorMessage(`Erro ao reiniciar: ${error.message}`);
    }
  });

  // Comando para mostrar informações do servidor
  const infoCommand = vscode.commands.registerCommand('pascalLSP.info', () => {
    const info = `
**Pascal LSP Server**

**Status:** ${client.isRunning() ? '🟢 Ativo' : '🔴 Inativo'}
**Versão:** 1.0.0
**Funcionalidades:**
- ✅ Hover (informações ao passar o mouse)
- ✅ Completion (autocompletar)
- ✅ Document Sync (sincronização de arquivos)
- 🚧 Go to Definition (em desenvolvimento)
- 🚧 Find References (em desenvolvimento)

**Arquivos suportados:** .pas, .pp, .inc
    `;

    vscode.window.showInformationMessage(info, { modal: true });
  });

  context.subscriptions.push(restartCommand, infoCommand);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  
  console.log('Desativando Pascal LSP...');
  return client.stop();
}

// === CONFIGURAÇÃO DE LINGUAGEM ===
// language-configuration.json
const languageConfig = {
  "comments": {
    "lineComment": "//",
    "blockComment": ["{", "}"]
  },
  "brackets": [
    ["(", ")"],
    ["[", "]"],
    ["begin", "end"]
  ],
  "autoClosingPairs": [
    {"open": "(", "close": ")"},
    {"open": "[", "close": "]"},
    {"open": "{", "close": "}"},
    {"open": "'", "close": "'", "notIn": ["string", "comment"]},
    {"open": "\"", "close": "\"", "notIn": ["string"]},
    {"open": "begin", "close": "end"}
  ],
  "surroundingPairs": [
    ["(", ")"],
    ["[", "]"],
    ["{", "}"],
    ["'", "'"],
    ["\"", "\""]
  ],
  "wordPattern": "\\b[A-Za-z_][A-Za-z0-9_]*\\b"
};

// === SCRIPT DE TESTE ===
// test-server.js (Node.js para testar o servidor)
const testScript = `
const { spawn } = require('child_process');

console.log('=== Testando Servidor Pascal LSP ===\\n');

// Iniciar o servidor
const server = spawn('./BasicLSPServer', [], {
  stdio: ['pipe', 'pipe', 'inherit']
});

// Função para enviar mensagem LSP
function sendLSPMessage(message) {
  const json = JSON.stringify(message);
  const header = \`Content-Length: \${json.length}\\r\\n\\r\\n\`;
  server.stdin.write(header + json);
}

// Teste 1: Initialize
console.log('📤 Enviando initialize...');
sendLSPMessage({
  jsonrpc: '2.0',
  id: 1,
  method: 'initialize',
  params: {
    processId: process.pid,
    clientInfo: { name: 'Test Client', version: '1.0' },
    capabilities: {}
  }
});

// Aguardar resposta
server.stdout.on('data', (data) => {
  const lines = data.toString().split('\\n');
  
  for (const line of lines) {
    if (line.startsWith('Content-Length:')) {
      console.log('📥 Resposta recebida do servidor');
    } else if (line.trim() && !line.includes('Content-Length')) {
      try {
        const response = JSON.parse(line);
        console.log('✅ JSON válido:', JSON.stringify(response, null, 2));
        
        // Se foi initialize, enviar initialized
        if (response.id === 1 && response.result) {
          console.log('\\n📤 Enviando initialized...');
          sendLSPMessage({
            jsonrpc: '2.0',
            method: 'initialized',
            params: {}
          });
          
          // Teste hover após 1 segundo
          setTimeout(() => {
            console.log('\\n📤 Testando hover...');
            sendLSPMessage({
              jsonrpc: '2.0',
              id: 2,
              method: 'textDocument/hover',
              params: {
                textDocument: { uri: 'file:///test.pas' },
                position: { line: 0, character: 5 }
              }
            });
          }, 1000);
        }
        
      } catch (e) {
        console.log('📄 Conteúdo:', line);
      }
    }
  }
});

// Finalizar após 5 segundos
setTimeout(() => {
  console.log('\\n📤 Enviando shutdown...');
  sendLSPMessage({
    jsonrpc: '2.0',
    id: 999,
    method: 'shutdown'
  });
  
  setTimeout(() => {
    sendLSPMessage({
      jsonrpc: '2.0',
      method: 'exit'
    });
    console.log('\\n✅ Teste concluído!');
  }, 500);
}, 5000);
`;

// === INSTRUÇÕES DE SETUP ===
const setupInstructions = \`
# Guia de Setup Completo

## 1. Compilar o Servidor
\\\`\\\`\\\`bash
fpc -Mobjfpc BasicLSPServer.pas
chmod +x BasicLSPServer
\\\`\\\`\\\`

## 2. Testar o Servidor
\\\`\\\`\\\`bash
node test-server.js
\\\`\\\`\\\`

## 3. Configurar VS Code

### 3.1 Criar extensão:
\\\`\\\`\\\`bash
mkdir pascal-lsp-extension
cd pascal-lsp-extension
npm init -y
npm install vscode-languageclient
\\\`\\\`\\\`

### 3.2 Adicionar arquivos:
- package.json (configuração da extensão)
- src/extension.ts (código da extensão)
- language-configuration.json

### 3.3 Compilar:
\\\`\\\`\\\`bash
npx tsc src/extension.ts --outDir out --target es2017 --module commonjs
\\\`\\\`\\\`

### 3.4 Instalar extensão:
\\\`\\\`\\\`bash
# Via symlink (desenvolvimento)
ln -s $(pwd) ~/.vscode/extensions/pascal-lsp

# Ou via VSIX
npm install -g vsce
vsce package
code --install-extension pascal-lsp-*.vsix
\\\`\\\`\\\`

## 4. Usar no VS Code

1. Abrir arquivo .pas
2. Ver hover e completion funcionando
3. Comandos disponíveis:
   - \\\`Pascal LSP: Restart\\\` 
   - \\\`Pascal LSP: Info\\\`

## 5. Personalizar

- Adicionar mais palavras-chave em \\\`HandleTextDocumentCompletion\\\`
- Implementar \\\`textDocument/definition\\\`
- Adicionar validação de sintaxe
- Integrar com compilador Pascal
\\\`;
