#!/bin/bash
# demo.sh - Script de demonstração do Pascal LSP

echo "🚀 === Demonstração Pascal LSP ==="
echo ""

# Cores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Função para logging
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Verificar se os executáveis existem
check_prerequisites() {
    log_info "Verificando pré-requisitos..."
    
    if [ ! -f "./BasicLSPServer" ]; then
        log_error "BasicLSPServer não encontrado. Compile primeiro:"
        echo "  fpc -Mobjfpc src/BasicLSPServer.pas"
        exit 1
    fi
    
    if [ ! -f "./SimpleLSPClient" ]; then
        log_error "SimpleLSPClient não encontrado. Compile primeiro:"
        echo "  fpc -Mobjfpc src/SimpleLSPClient.pas"
        exit 1
    fi
    
    log_success "Pré-requisitos OK"
}

# Demonstração 1: Teste automático básico
demo_basic() {
    log_info "=== DEMO 1: Teste Básico ==="
    echo ""
    
    log_info "Executando teste automático..."
    ./SimpleLSPClient ./BasicLSPServer test
    
    echo ""
    log_success "Demo básico concluído"
}

# Demonstração 2: Teste com arquivo Pascal real
demo_real_file() {
    log_info "=== DEMO 2: Arquivo Pascal Real ==="
    echo ""
    
    # Criar arquivo de teste
    cat > example.pas << 'EOF'
program ExampleProgram;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  TPerson = record
    Name: string;
    Age: Integer;
  end;
  
  TCalculator = class
  private
    FValue: Double;
  public
    constructor Create;
    function Add(const AValue: Double): Double;
    property Value: Double read FValue;
  end;

constructor TCalculator.Create;
begin
  inherited Create;
  FValue := 0;
end;

function TCalculator.Add(const AValue: Double): Double;
begin
  FValue := FValue + AValue;
  Result := FValue;
end;

var
  Person: TPerson;
  Calc: TCalculator;

begin
  WriteLn('Hello Pascal LSP!');
  
  Person.Name := 'João';
  Person.Age := 30;
  
  Calc := TCalculator.Create;
  try
    Calc.Add(10);
    WriteLn('Result: ', Calc.Value:0:2);
  finally
    Calc.Free;
  end;
end.
EOF

    log_info "Arquivo example.pas criado"
    
    # Testar LSP com arquivo real
    log_info "Testando LSP features..."
    
    # Criar script de comandos para o cliente
    cat > commands.txt << 'EOF'
open file:///$(pwd)/example.pas $(cat example.pas)
hover file:///$(pwd)/example.pas 8 2
complete file:///$(pwd)/example.pas 25 5
close file:///$(pwd)/example.pas
quit
EOF

    log_info "Executando comandos LSP..."
    ./SimpleLSPClient ./BasicLSPServer interactive < commands.txt
    
    # Cleanup
    rm -f example.pas commands.txt
    
    echo ""
    log_success "Demo com arquivo real concluído"
}

# Demonstração 3: Teste de performance
demo_performance() {
    log_info "=== DEMO 3: Teste de Performance ==="
    echo ""
    
    log_info "Medindo tempo de inicialização..."
    
    time_start=$(date +%s.%N)
    ./SimpleLSPClient ./BasicLSPServer test > /dev/null 2>&1
    time_end=$(date +%s.%N)
    
    duration=$(echo "$time_end - $time_start" | bc)
    log_success "Tempo total: ${duration}s"
    
    echo ""
    log_success "Demo de performance concluído"
}

# Demonstração 4: Teste interativo guiado
demo_interactive() {
    log_info "=== DEMO 4: Sessão Interativa ==="
    echo ""
    
    log_info "Iniciando sessão interativa..."
    log_warning "Comandos disponíveis:"
    echo "  open <uri> <texto>     - Abrir documento"
    echo "  hover <uri> <line> <char> - Hover em posição"  
    echo "  complete <uri> <line> <char> - Completion em posição"
    echo "  close <uri>            - Fechar documento"
    echo "  quit                   - Sair"
    echo ""
    echo "Exemplo:"
    echo "  open file:///test.pas program Test; begin WriteLn('Hello'); end."
    echo "  hover file:///test.pas 0 8"
    echo "  complete file:///test.pas 0 25"
    echo ""
    
    ./SimpleLSPClient ./BasicLSPServer interactive
    
    echo ""
    log_success "Sessão interativa finalizada"
}

# Demonstração 5: Comparação com VS Code
demo_vscode_comparison() {
    log_info "=== DEMO 5: Comparação VS Code ==="
    echo ""
    
    log_info "Testando as mesmas funcionalidades que o VS Code usaria..."
    
    # Simular sequência de VS Code
    cat > vscode_simulation.txt << 'EOF'
open file:///workspace/main.pas program Main; {$mode objfpc} uses SysUtils; type TTest = record Name: string; end; var Test: TTest; begin Test.Name := 'Pascal'; WriteLn(Test.Name); end.
hover file:///workspace/main.pas 0 38
complete file:///workspace/main.pas 0 80
hover file:///workspace/main.pas 0 55
close file:///workspace/main.pas
quit
EOF

    log_info "Simulando workflow do VS Code..."
    ./SimpleLSPClient ./BasicLSPServer interactive < vscode_simulation.txt
    
    rm -f vscode_simulation.txt
    
    echo ""
    log_success "Comparação com VS Code concluída"
}

# Menu principal
show_menu() {
    echo ""
    log_info "Escolha uma demonstração:"
    echo "  1) Teste básico automático"
    echo "  2) Teste com arquivo Pascal real" 
    echo "  3) Teste de performance"
    echo "  4) Sessão interativa"
    echo "  5) Simulação VS Code"
    echo "  6) Executar todos os testes"
    echo "  0) Sair"
    echo ""
}

# Função principal
main() {
    check_prerequisites
    
    if [ "$1" = "all" ]; then
        demo_basic
        demo_real_file
        demo_performance
        demo_vscode_comparison
        log_success "Todas as demonstrações concluídas!"
        exit 0
    fi
    
    while true; do
        show_menu
        read -p "Opção: " choice
        
        case $choice in
            1) demo_basic ;;
            2) demo_real_file ;;
            3) demo_performance ;;
            4) demo_interactive ;;
            5) demo_vscode_comparison ;;
            6) 
                demo_basic
                demo_real_file
                demo_performance
                demo_vscode_comparison
                log_success "Todas as demonstrações concluídas!"
                ;;
            0) 
                log_info "Até logo!"
                exit 0
                ;;
            *) 
                log_error "Opção inválida: $choice"
                ;;
        esac
    done
}

# Executar
main "$@"

# === ARQUIVO MAKEFILE ===
# Makefile para build automático

.PHONY: all clean test demo install

# Compilador e flags
FPC = fpc
FPCFLAGS = -Mobjfpc -O2

# Diretórios
SRCDIR = src
BUILDDIR = build
BINDIR = bin

# Executáveis
GENERATOR = $(BINDIR)/LSPCodeGenerator
SERVER = $(BINDIR)/BasicLSPServer  
CLIENT = $(BINDIR)/SimpleLSPClient

all: $(GENERATOR) $(SERVER) $(CLIENT)

# Gerar tipos LSP
$(SRCDIR)/LSPTypes.pas: $(GENERATOR)
	@echo "🔄 Gerando tipos LSP..."
	$(GENERATOR)
	@mv LSPTypes.pas $(SRCDIR)/

# Compilar gerador
$(GENERATOR): $(SRCDIR)/LSPCodeGenerator.pas | $(BINDIR)
	@echo "🔨 Compilando gerador..."
	$(FPC) $(FPCFLAGS) -o$@ $<

# Compilar servidor
$(SERVER): $(SRCDIR)/BasicLSPServer.pas $(SRCDIR)/LSPTypes.pas | $(BINDIR)
	@echo "🔨 Compilando servidor LSP..."
	$(FPC) $(FPCFLAGS) -o$@ $<

# Compilar cliente
$(CLIENT): $(SRCDIR)/SimpleLSPClient.pas | $(BINDIR)
	@echo "🔨 Compilando cliente LSP..."
	$(FPC) $(FPCFLAGS) -o$@ $<

# Criar diretórios
$(BINDIR):
	@mkdir -p $(BINDIR)

# Executar testes
test: $(SERVER) $(CLIENT)
	@echo "🧪 Executando testes..."
	$(CLIENT) $(SERVER) test

# Demonstração interativa
demo: $(SERVER) $(CLIENT)
	@echo "🎮 Iniciando demonstração..."
	./demo.sh

# Instalar no sistema (opcional)
install: all
	@echo "📦 Instalando no sistema..."
	sudo cp $(SERVER) /usr/local/bin/
	sudo cp $(CLIENT) /usr/local/bin/
	@echo "✅ Instalação concluída!"

# Limpeza
clean:
	@echo "🧹 Limpando arquivos..."
	rm -rf $(BINDIR) $(BUILDDIR)
	rm -f $(SRCDIR)/*.o $(SRCDIR)/*.ppu
	rm -f *.o *.ppu
	@echo "✅ Limpeza concluída!"

# Ajuda
help:
	@echo "🚀 Pascal LSP - Sistema de Build"
	@echo ""
	@echo "Comandos disponíveis:"
	@echo "  make all      - Compilar tudo"
	@echo "  make test     - Executar testes"
	@echo "  make demo     - Demonstração interativa"
	@echo "  make install  - Instalar no sistema"
	@echo "  make clean    - Limpar arquivos"
	@echo "  make help     - Mostrar esta ajuda"
	@echo ""
	@echo "Primeiro uso:"
	@echo "  1. make all"
	@echo "  2. make test"
	@echo "  3. make demo"

# === SCRIPT DE QUICK START ===
# quickstart.sh

#!/bin/bash

echo "🚀 Pascal LSP - Quick Start"
echo ""

# Verificar FreePascal
if ! command -v fpc &> /dev/null; then
    echo "❌ FreePascal não encontrado. Instale com:"
    echo "  Ubuntu/Debian: sudo apt install fpc"
    echo "  CentOS/RHEL: sudo yum install fpc"
    echo "  Arch: sudo pacman -S fpc"
    echo "  macOS: brew install fpc"
    exit 1
fi

echo "✅ FreePascal encontrado: $(fpc -iV)"

# Build completo
echo ""
echo "🔨 Compilando projeto..."
make all

if [ $? -eq 0 ]; then
    echo ""
    echo "✅ Build concluído com sucesso!"
    echo ""
    echo "🧪 Executando teste básico..."
    make test
    
    echo ""
    echo "🎉 Quick start concluído!"
    echo ""
    echo "Próximos passos:"
    echo "  - make demo     # Demonstração completa"
    echo "  - make install  # Instalar no sistema"
    echo "  - Configurar VS Code com a extensão"
    echo ""
else
    echo "❌ Erro no build. Verifique as mensagens acima."
    exit 1
fi