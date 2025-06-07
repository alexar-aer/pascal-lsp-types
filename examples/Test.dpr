program TestPascalLSP;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  // Teste de hover em tipos
  TPerson = record
    Name: string;
    Age: Integer;
  end;

  // Teste de completion em classes
  TCalculator = class
  private
    FValue: Double;
  public
    constructor Create;
    function Add(AValue: Double): Double;
    function Multiply(AValue: Double): Double;
    property Value: Double read FValue write FValue;
  end;

// === IMPLEMENTAÇÃO ===
constructor TCalculator.Create;
begin
  inherited Create;
  FValue := 0;
end;

function TCalculator.Add(AValue: Double): Double;
begin
  FValue := FValue + AValue;
  Result := FValue;
end;

function TCalculator.Multiply(AValue: Double): Double;
begin
  FValue := FValue * AValue;
  Result := FValue;
end;

// === PROGRAMA PRINCIPAL ===
var
  Person: TPerson;
  Calc: TCalculator;
  ResultValue: Double;

begin
  WriteLn('=== Teste Pascal LSP ===');
  
  // Teste hover aqui - passar mouse sobre 'Person'
  Person.Name := 'João';
  Person.Age := 30;
  
  WriteLn('Nome: ', Person.Name);
  WriteLn('Idade: ', Person.Age);
  
  // Teste completion aqui - digitar 'Calc.' e ver sugestões
  Calc := TCalculator.Create;
  try
    ResultValue := Calc.Add(10);
    ResultValue := Calc.Multiply(2);
    
    WriteLn('Resultado: ', ResultValue:0:2);
  finally
    Calc.Free;
  end;
  
  // Teste completion de palavras-chave - digitar 'beg' e ver 'begin'
  WriteLn('Teste concluído!');
end.

// === MELHORIAS SUGERIDAS PARA O SERVIDOR ===

(*
unit LSPServerEnhanced;

// 1. ADICIONAR PARSING REAL DE PASCAL
uses
  PasTree, PasResolve, FPPascalParser; // Usar parser real

type
  TEnhancedLSPServer = class(TLSPServer)
  private
    FParser: TFPPascalParser;
    FSymbolTable: TSymbolTable;
    
    procedure ParseDocument(const AUri, AText: string);
    function FindSymbolAtPosition(const AUri: string; ALine, AChar: Integer): TSymbol;
    function GetSymbolCompletions(const AUri: string; ALine, AChar: Integer): TArray<TCompletionItem>;
    
  protected
    // Override métodos para usar parser real
    procedure HandleTextDocumentDidChange(const AParams: TJSONObject); override;
    procedure HandleTextDocumentHover(const AId: TJSONData; const AParams: TJSONObject); override;
    procedure HandleTextDocumentCompletion(const AId: TJSONData; const AParams: TJSONObject); override;
    
    // Novos métodos
    procedure HandleTextDocumentDefinition(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentReferences(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentDocumentSymbol(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentDiagnostics(const AUri: string);
    
  end;

// 2. IMPLEMENTAR DIAGNÓSTICOS (ERROS/WARNINGS)
procedure TEnhancedLSPServer.HandleTextDocumentDiagnostics(const AUri: string);
var
  Diagnostics: TJSONArray;
  Diagnostic: TJSONObject;
  Text: string;
  Errors: TArray<TParseError>;
  Error: TParseError;
begin
  if not FDocuments.TryGetValue(AUri, Text) then Exit;
  
  // Compilar/validar código
  Errors := FParser.ValidateCode(Text);
  Diagnostics := TJSONArray.Create;
  
  for Error in Errors do
  begin
    Diagnostic := TJSONObject.Create;
    Diagnostic.Add('range', CreateRange(Error.Line, Error.Column, Error.Line, Error.Column + Error.Length));
    Diagnostic.Add('severity', IfThen(Error.IsWarning, 2, 1)); // Error=1, Warning=2
    Diagnostic.Add('source', 'pascal-lsp');
    Diagnostic.Add('message', Error.Message);
    Diagnostics.Add(Diagnostic);
  end;
  
  // Enviar diagnósticos
  SendNotification('textDocument/publishDiagnostics', 
    TJSONObject.Create(['uri', AUri, 'diagnostics', Diagnostics]));
end;

// 3. IMPLEMENTAR GO TO DEFINITION
procedure TEnhancedLSPServer.HandleTextDocumentDefinition(const AId: TJSONData; const AParams: TJSONObject);
var
  Symbol: TSymbol;
  TextDocument, Position: TJSONObject;
  Uri: string;
  Line, Character: Integer;
  Location: TJSONObject;
begin
  TextDocument := TJSONObject(AParams.Find('textDocument'));
  Position := TJSONObject(AParams.Find('position'));
  
  if Assigned(TextDocument) and Assigned(Position) then
  begin
    Uri := TextDocument.Get('uri', '');
    Line := Position.Get('line', 0);
    Character := Position.Get('character', 0);
    
    Symbol := FindSymbolAtPosition(Uri, Line, Character);
    if Assigned(Symbol) and Assigned(Symbol.Declaration) then
    begin
      Location := TJSONObject.Create;
      Location.Add('uri', Symbol.Declaration.FileName);
      Location.Add('range', CreateRange(
        Symbol.Declaration.Line, Symbol.Declaration.Column,
        Symbol.Declaration.Line, Symbol.Declaration.Column + Length(Symbol.Name)
      ));
      
      SendResponse(AId, Location);
      Exit;
    end;
  end;
  
  SendResponse(AId, TJSONNull.Create);
end;

// 4. IMPLEMENTAR SÍMBOLOS DO DOCUMENTO
procedure TEnhancedLSPServer.HandleTextDocumentDocumentSymbol(const AId: TJSONData; const AParams: TJSONObject);
var
  Symbols: TJSONArray;
  Symbol: TJSONObject;
  ParsedSymbols: TArray<TDocumentSymbol>;
  DocSymbol: TDocumentSymbol;
begin
  // Extrair símbolos do documento (functions, procedures, types, etc.)
  ParsedSymbols := FParser.ExtractSymbols(AUri);
  Symbols := TJSONArray.Create;
  
  for DocSymbol in ParsedSymbols do
  begin
    Symbol := TJSONObject.Create;
    Symbol.Add('name', DocSymbol.Name);
    Symbol.Add('kind', Integer(DocSymbol.Kind)); // Function=12, Class=5, etc.
    Symbol.Add('range', CreateRange(DocSymbol.StartLine, DocSymbol.StartCol, DocSymbol.EndLine, DocSymbol.EndCol));
    Symbol.Add('selectionRange', CreateRange(DocSymbol.NameLine, DocSymbol.NameCol, DocSymbol.NameLine, DocSymbol.NameCol + Length(DocSymbol.Name)));
    
    if DocSymbol.Documentation <> '' then
      Symbol.Add('detail', DocSymbol.Documentation);
      
    Symbols.Add(Symbol);
  end;
  
  SendResponse(AId, Symbols);
end;

// 5. IMPLEMENTAR FORMATAÇÃO
procedure TEnhancedLSPServer.HandleTextDocumentFormatting(const AId: TJSONData; const AParams: TJSONObject);
var
  TextDocument: TJSONObject;
  Options: TJSONObject;
  Uri, Text, FormattedText: string;
  TextEdits: TJSONArray;
  Edit: TJSONObject;
  TabSize: Integer;
  InsertSpaces: Boolean;
begin
  TextDocument := TJSONObject(AParams.Find('textDocument'));
  Options := TJSONObject(AParams.Find('options'));
  
  if Assigned(TextDocument) and FDocuments.TryGetValue(TextDocument.Get('uri', ''), Text) then
  begin
    TabSize := Options.Get('tabSize', 2);
    InsertSpaces := Options.Get('insertSpaces', True);
    
    // Formatar código Pascal
    FormattedText := FormatPascalCode(Text, TabSize, InsertSpaces);
    
    if FormattedText <> Text then
    begin
      TextEdits := TJSONArray.Create;
      Edit := TJSONObject.Create;
      
      // Substituir todo o documento
      Edit.Add('range', CreateRange(0, 0, GetLineCount(Text), 0));
      Edit.Add('newText', FormattedText);
      TextEdits.Add(Edit);
      
      SendResponse(AId, TextEdits);
    end
    else
      SendResponse(AId, TJSONArray.Create); // Sem mudanças
  end;
end;

*)

// === ROADMAP DE FUNCIONALIDADES ===

(*
FUNCIONALIDADES BÁSICAS (✅ Implementadas):
- ✅ Initialize/Shutdown
- ✅ Document Sync (open/change/close)  
- ✅ Hover básico
- ✅ Completion básico (palavras-chave)

PRÓXIMAS FUNCIONALIDADES:
- 🚧 Parsing real de Pascal
- 🚧 Go to Definition
- 🚧 Find References  
- 🚧 Document Symbols (outline)
- 🚧 Diagnostics (erros/warnings)
- 🚧 Code Formatting
- 🚧 Rename Symbol
- 🚧 Code Actions (quick fixes)
- 🚧 Signature Help (parâmetros de função)

FUNCIONALIDADES AVANÇADAS:
- 📋 Workspace Symbols (busca global)
- 📋 Call Hierarchy
- 📋 Type Hierarchy  
- 📋 Folding Ranges
- 📋 Selection Ranges
- 📋 Semantic Tokens (syntax highlighting)
- 📋 Inlay Hints (tipos implícitos)
- 📋 Code Lens (referências inline)

INTEGRAÇÕES:
- 🔧 Compilador FPC/Delphi
- 🔧 Debugger (DAP)
- 🔧 Unit Testing
- 🔧 Code Coverage
- 🔧 Static Analysis
- 🔧 Package Manager
*)

// === EXEMPLO DE USO NO VS CODE ===

(*
1. Abrir test.pas no VS Code
2. Hover sobre 'TPerson' → Ver documentação
3. Digitar 'Calc.' → Ver completion com métodos
4. Ctrl+Click em 'TCalculator' → Go to definition (futuro)
5. Ctrl+Shift+O → Ver símbolos do documento (futuro)
6. F2 em variável → Rename (futuro)
*)

end.