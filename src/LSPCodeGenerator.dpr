program LSPCodeGenerator;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, opensslsockets;

type
  { TLSPGenerator - Classe principal para gerar código LSP }
  TLSPGenerator = class
  private
    FMetaModel: TJSONObject;
    FOutput: TStringList;
    
    function DownloadMetaModel: string;
    function LoadMetaModel(const AFileName: string): Boolean;
    function MapLSPTypeToPascal(const ATypeNode: TJSONObject): string;
    function SanitizePascalIdentifier(const AName: string): string;
    function CapitalizeFirst(const AStr: string): string;
    function FormatDocumentation(const ADoc: string; const AIndent: string = '  '): string;
    
    procedure GenerateStructures;
    procedure GenerateEnumerations;
    procedure GenerateTypeAliases;
    procedure GenerateRequests;
    procedure GenerateNotifications;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    function Generate(const AMetaModelFile: string = ''): Boolean;
    procedure SaveToFile(const AFileName: string);
    property Output: TStringList read FOutput;
  end;

implementation

{ TLSPGenerator }

constructor TLSPGenerator.Create;
begin
  inherited Create;
  FOutput := TStringList.Create;
  FMetaModel := nil;
end;

destructor TLSPGenerator.Destroy;
begin
  FOutput.Free;
  if Assigned(FMetaModel) then
    FMetaModel.Free;
  inherited Destroy;
end;

function TLSPGenerator.DownloadMetaModel: string;
const
  MetaModelURL = 'https://raw.githubusercontent.com/microsoft/language-server-protocol/gh-pages/_specifications/lsp/metaModel/metaModel.json';
var
  HttpClient: TFPHTTPClient;
begin
  Result := '';
  HttpClient := TFPHTTPClient.Create(nil);
  try
    try
      WriteLn('Baixando metaModel.json...');
      Result := HttpClient.Get(MetaModelURL);
      WriteLn('Download concluído!');
    except
      on E: Exception do
      begin
        WriteLn('Erro ao baixar metaModel: ', E.Message);
        WriteLn('Baixe manualmente de: ', MetaModelURL);
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

function TLSPGenerator.LoadMetaModel(const AFileName: string): Boolean;
var
  JsonStr: string;
  JsonData: TJSONData;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := False;
  
  // Se não foi fornecido arquivo, baixar da internet
  if AFileName = '' then
  begin
    JsonStr := DownloadMetaModel;
    if JsonStr = '' then Exit;
  end
  else if FileExists(AFileName) then
  begin
    // Carregar de arquivo local
    FileStream := TFileStream.Create(AFileName, fmOpenRead);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(FileStream, 0);
        JsonStr := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  end
  else
  begin
    WriteLn('Arquivo não encontrado: ', AFileName);
    Exit;
  end;
  
  // Parsear JSON
  try
    JsonData := GetJSON(JsonStr);
    if JsonData is TJSONObject then
    begin
      FMetaModel := TJSONObject(JsonData);
      Result := True;
      WriteLn('MetaModel carregado com sucesso!');
    end
    else
    begin
      JsonData.Free;
      WriteLn('Erro: JSON inválido');
    end;
  except
    on E: Exception do
      WriteLn('Erro ao parsear JSON: ', E.Message);
  end;
end;

function TLSPGenerator.MapLSPTypeToPascal(const ATypeNode: TJSONObject): string;
var
  Kind, Name: string;
  Element: TJSONObject;
begin
  Kind := ATypeNode.Get('kind', '');
  
  case Kind of
    'base':
      begin
        Name := ATypeNode.Get('name', '');
        case Name of
          'string': Result := 'string';
          'integer': Result := 'Integer';
          'uinteger': Result := 'Cardinal';
          'decimal': Result := 'Double';
          'boolean': Result := 'Boolean';
          'null': Result := 'Pointer';
          else Result := 'Variant';
        end;
      end;
    'reference':
      begin
        Name := ATypeNode.Get('name', '');
        Result := 'T' + SanitizePascalIdentifier(Name);
      end;
    'array':
      begin
        Element := TJSONObject(ATypeNode.Get('element'));
        if Assigned(Element) then
          Result := 'TArray<' + MapLSPTypeToPascal(Element) + '>'
        else
          Result := 'TArray<Variant>';
      end;
    'map':
      begin
        // Maps geralmente viram dicionários ou registros variant
        Result := 'TDictionary<string, Variant>';
      end;
    'and', 'or':
      begin
        // Union types - usar Variant por simplicidade
        Result := 'Variant';
      end;
    'tuple':
      begin
        // Tuples - usar array por simplicidade
        Result := 'TArray<Variant>';
      end;
    'literal':
      begin
        // Literais - usar o tipo base
        Result := 'Variant';
      end;
    else
      Result := 'Variant';
  end;
end;

function TLSPGenerator.SanitizePascalIdentifier(const AName: string): string;
var
  i: Integer;
begin
  Result := AName;
  
  // Substituir caracteres inválidos
  for i := 1 to Length(Result) do
  begin
    if not (Result[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Result[i] := '_';
  end;
  
  // Garantir que não comece com número
  if (Length(Result) > 0) and (Result[1] in ['0'..'9']) then
    Result := '_' + Result;
    
  // Evitar palavras reservadas
  if LowerCase(Result) in ['begin', 'end', 'var', 'type', 'class', 'record', 'interface'] then
    Result := Result + '_';
end;

function TLSPGenerator.CapitalizeFirst(const AStr: string): string;
begin
  if Length(AStr) > 0 then
    Result := UpperCase(AStr[1]) + Copy(AStr, 2, Length(AStr))
  else
    Result := AStr;
end;

function TLSPGenerator.FormatDocumentation(const ADoc: string; const AIndent: string): string;
var
  Lines: TStringList;
  i: Integer;
begin
  if ADoc = '' then
  begin
    Result := '';
    Exit;
  end;
  
  Lines := TStringList.Create;
  try
    Lines.Text := ADoc;
    Result := '';
    
    for i := 0 to Lines.Count - 1 do
    begin
      if Lines[i].Trim <> '' then
        Result := Result + AIndent + '// ' + Lines[i].Trim + sLineBreak;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TLSPGenerator.GenerateStructures;
var
  Structures: TJSONArray;
  Structure: TJSONObject;
  Properties: TJSONArray;
  Prop: TJSONObject;
  i, j: Integer;
  StructName, PropName, PropType, Documentation: string;
  Extends: TJSONArray;
  ExtendName: string;
begin
  Structures := TJSONArray(FMetaModel.Find('structures'));
  if not Assigned(Structures) then Exit;
  
  FOutput.Add('  // === ESTRUTURAS ===');
  FOutput.Add('');
  
  for i := 0 to Structures.Count - 1 do
  begin
    Structure := TJSONObject(Structures[i]);
    StructName := Structure.Get('name', '');
    Documentation := Structure.Get('documentation', '');
    
    // Adicionar documentação
    if Documentation <> '' then
      FOutput.Add(FormatDocumentation(Documentation));
    
    // Verificar herança
    Extends := TJSONArray(Structure.Find('extends'));
    if Assigned(Extends) and (Extends.Count > 0) then
    begin
      ExtendName := TJSONObject(Extends[0]).Get('name', '');
      FOutput.Add('  T' + SanitizePascalIdentifier(StructName) + ' = record(T' + SanitizePascalIdentifier(ExtendName) + ')');
    end
    else
      FOutput.Add('  T' + SanitizePascalIdentifier(StructName) + ' = record');
    
    // Processar propriedades
    Properties := TJSONArray(Structure.Find('properties'));
    if Assigned(Properties) then
    begin
      for j := 0 to Properties.Count - 1 do
      begin
        Prop := TJSONObject(Properties[j]);
        PropName := Prop.Get('name', '');
        PropType := MapLSPTypeToPascal(TJSONObject(Prop.Find('type')));
        Documentation := Prop.Get('documentation', '');
        
        if Documentation <> '' then
          FOutput.Add(FormatDocumentation(Documentation, '    '));
          
        FOutput.Add('    ' + CapitalizeFirst(SanitizePascalIdentifier(PropName)) + ': ' + PropType + ';');
      end;
    end;
    
    FOutput.Add('  end;');
    FOutput.Add('');
  end;
end;

procedure TLSPGenerator.GenerateEnumerations;
var
  Enumerations: TJSONArray;
  Enumeration: TJSONObject;
  Values: TJSONArray;
  Value: TJSONObject;
  i, j: Integer;
  EnumName, ValueName, Documentation: string;
  IntValue: Integer;
begin
  Enumerations := TJSONArray(FMetaModel.Find('enumerations'));
  if not Assigned(Enumerations) then Exit;
  
  FOutput.Add('  // === ENUMERAÇÕES ===');
  FOutput.Add('');
  
  for i := 0 to Enumerations.Count - 1 do
  begin
    Enumeration := TJSONObject(Enumerations[i]);
    EnumName := Enumeration.Get('name', '');
    Documentation := Enumeration.Get('documentation', '');
    
    if Documentation <> '' then
      FOutput.Add(FormatDocumentation(Documentation));
    
    FOutput.Add('  T' + SanitizePascalIdentifier(EnumName) + ' = (');
    
    Values := TJSONArray(Enumeration.Find('values'));
    if Assigned(Values) then
    begin
      for j := 0 to Values.Count - 1 do
      begin
        Value := TJSONObject(Values[j]);
        ValueName := Value.Get('name', '');
        IntValue := Value.Get('value', j);
        Documentation := Value.Get('documentation', '');
        
        if Documentation <> '' then
          FOutput.Add(FormatDocumentation(Documentation, '    '));
          
        ValueName := SanitizePascalIdentifier(ValueName);
        FOutput.Add('    ' + ValueName + ' = ' + InttoStr(IntValue) + 
          IfThen(j < Values.Count - 1, ',', ''));
      end;
    end;
    
    FOutput.Add('  );');
    FOutput.Add('');
  end;
end;

procedure TLSPGenerator.GenerateTypeAliases;
var
  TypeAliases: TJSONArray;
  TypeAlias: TJSONObject;
  i: Integer;
  AliasName, Documentation, AliasType: string;
begin
  TypeAliases := TJSONArray(FMetaModel.Find('typeAliases'));
  if not Assigned(TypeAliases) then Exit;
  
  FOutput.Add('  // === TYPE ALIASES ===');
  FOutput.Add('');
  
  for i := 0 to TypeAliases.Count - 1 do
  begin
    TypeAlias := TJSONObject(TypeAliases[i]);
    AliasName := TypeAlias.Get('name', '');
    Documentation := TypeAlias.Get('documentation', '');
    AliasType := MapLSPTypeToPascal(TJSONObject(TypeAlias.Find('type')));
    
    if Documentation <> '' then
      FOutput.Add(FormatDocumentation(Documentation));
      
    FOutput.Add('  T' + SanitizePascalIdentifier(AliasName) + ' = ' + AliasType + ';');
    FOutput.Add('');
  end;
end;

procedure TLSPGenerator.GenerateRequests;
var
  Requests: TJSONArray;
  Request: TJSONObject;
  i: Integer;
  Method, Documentation: string;
begin
  Requests := TJSONArray(FMetaModel.Find('requests'));
  if not Assigned(Requests) then Exit;
  
  FOutput.Add('  // === MÉTODOS LSP (REQUESTS) ===');
  FOutput.Add('  // Use estes métodos para implementar o servidor/cliente');
  FOutput.Add('');
  
  for i := 0 to Requests.Count - 1 do
  begin
    Request := TJSONObject(Requests[i]);
    Method := Request.Get('method', '');
    Documentation := Request.Get('documentation', '');
    
    FOutput.Add('  // Método: ' + Method);
    if Documentation <> '' then
      FOutput.Add(FormatDocumentation(Documentation));
    FOutput.Add('  // TODO: Implementar handler para ' + Method);
    FOutput.Add('');
  end;
end;

procedure TLSPGenerator.GenerateNotifications;
var
  Notifications: TJSONArray;
  Notification: TJSONObject;
  i: Integer;
  Method, Documentation: string;
begin
  Notifications := TJSONArray(FMetaModel.Find('notifications'));
  if not Assigned(Notifications) then Exit;
  
  FOutput.Add('  // === NOTIFICAÇÕES LSP ===');
  FOutput.Add('  // Use estas notificações para comunicação unidirecional');
  FOutput.Add('');
  
  for i := 0 to Notifications.Count - 1 do
  begin
    Notification := TJSONObject(Notifications[i]);
    Method := Notification.Get('method', '');
    Documentation := Notification.Get('documentation', '');
    
    FOutput.Add('  // Notificação: ' + Method);
    if Documentation <> '' then
      FOutput.Add(FormatDocumentation(Documentation));
    FOutput.Add('  // TODO: Implementar handler para ' + Method);
    FOutput.Add('');
  end;
end;

function TLSPGenerator.Generate(const AMetaModelFile: string): Boolean;
begin
  Result := False;
  
  // Carregar metaModel
  if not LoadMetaModel(AMetaModelFile) then
    Exit;
  
  // Limpar output
  FOutput.Clear;
  
  // Gerar cabeçalho da unit
  FOutput.Add('unit LSPTypes;');
  FOutput.Add('');
  FOutput.Add('{$mode objfpc}{$H+}');
  FOutput.Add('// Gerado automaticamente a partir do metaModel.json do LSP 3.17');
  FOutput.Add('// https://github.com/microsoft/language-server-protocol');
  FOutput.Add('');
  FOutput.Add('interface');
  FOutput.Add('');
  FOutput.Add('uses');
  FOutput.Add('  Classes, SysUtils, Generics.Collections;');
  FOutput.Add('');
  FOutput.Add('type');
  FOutput.Add('');
  
  // Gerar todos os tipos
  GenerateEnumerations;
  GenerateTypeAliases;
  GenerateStructures;
  GenerateRequests;
  GenerateNotifications;
  
  // Finalizar unit
  FOutput.Add('implementation');
  FOutput.Add('');
  FOutput.Add('end.');
  
  Result := True;
  WriteLn('Geração concluída! ', FOutput.Count, ' linhas geradas.');
end;

procedure TLSPGenerator.SaveToFile(const AFileName: string);
begin
  try
    FOutput.SaveToFile(AFileName);
    WriteLn('Arquivo salvo: ', AFileName);
  except
    on E: Exception do
      WriteLn('Erro ao salvar arquivo: ', E.Message);
  end;
end;

// === PROGRAMA PRINCIPAL ===
var
  Generator: TLSPGenerator;
  MetaModelFile, OutputFile: string;
begin
  WriteLn('=== Gerador de Código LSP para Pascal ===');
  WriteLn('Versão 1.0 - LSP 3.17');
  WriteLn('');
  
  // Verificar parâmetros
  if ParamCount >= 1 then
    MetaModelFile := ParamStr(1)
  else
    MetaModelFile := ''; // Baixar da internet
    
  if ParamCount >= 2 then
    OutputFile := ParamStr(2)
  else
    OutputFile := 'LSPTypes.pas';
  
  Generator := TLSPGenerator.Create;
  try
    WriteLn('Iniciando geração...');
    
    if Generator.Generate(MetaModelFile) then
    begin
      Generator.SaveToFile(OutputFile);
      WriteLn('');
      WriteLn('✅ Sucesso! Arquivo gerado: ', OutputFile);
      WriteLn('');
      WriteLn('Uso:');
      WriteLn('  1. Adicione LSPTypes.pas ao seu projeto');
      WriteLn('  2. Implemente os handlers dos métodos LSP');
      WriteLn('  3. Use os tipos T* para comunicação LSP');
    end
    else
    begin
      WriteLn('❌ Erro na geração');
    end;
  finally
    Generator.Free;
  end;
  
  WriteLn('');
  WriteLn('Pressione ENTER para sair...');
  ReadLn;
end.