program BasicLSPServer;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpjson, jsonparser, 
  Generics.Collections, StrUtils;

// Incluir os tipos gerados (normalmente seria: LSPTypes in 'LSPTypes.pas')
{$I LSPTypes.pas} // Simular inclusão dos tipos gerados

type
  { TLSPServer - Servidor LSP básico }
  TLSPServer = class
  private
    FDocuments: TDictionary<string, string>; // URI -> conteúdo
    FRunning: Boolean;
    FInitialized: Boolean;
    
    // Comunicação JSON-RPC
    procedure SendResponse(const AId: TJSONData; const AResult: TJSONData);
    procedure SendError(const AId: TJSONData; const ACode: Integer; const AMessage: string);
    procedure SendNotification(const AMethod: string; const AParams: TJSONData);
    procedure SendMessage(const AMessage: TJSONObject);
    
    // Utilitários
    function ReadMessage: TJSONObject;
    function CreatePosition(ALine, ACharacter: Integer): TJSONObject;
    function CreateRange(AStartLine, AStartChar, AEndLine, AEndChar: Integer): TJSONObject;
    function GetWordAtPosition(const AText: string; ALine, ACharacter: Integer): string;
    
    // Handlers LSP
    procedure HandleInitialize(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleInitialized(const AParams: TJSONObject);
    procedure HandleShutdown(const AId: TJSONData);
    procedure HandleExit;
    procedure HandleTextDocumentDidOpen(const AParams: TJSONObject);
    procedure HandleTextDocumentDidChange(const AParams: TJSONObject);
    procedure HandleTextDocumentDidClose(const AParams: TJSONObject);
    procedure HandleTextDocumentHover(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentCompletion(const AId: TJSONData; const AParams: TJSONObject);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Run;
    procedure ProcessMessage(const AMessage: TJSONObject);
  end;

{ TLSPServer }

constructor TLSPServer.Create;
begin
  inherited Create;
  FDocuments := TDictionary<string, string>.Create;
  FRunning := True;
  FInitialized := False;
end;

destructor TLSPServer.Destroy;
begin
  FDocuments.Free;
  inherited Destroy;
end;

procedure TLSPServer.SendMessage(const AMessage: TJSONObject);
var
  JsonStr: string;
  ContentLength: Integer;
begin
  JsonStr := AMessage.AsJSON;
  ContentLength := Length(JsonStr);
  
  // Protocolo LSP: Content-Length header + JSON
  WriteLn('Content-Length: ', ContentLength);
  WriteLn('');
  Write(JsonStr);
  Flush(Output);
end;

procedure TLSPServer.SendResponse(const AId: TJSONData; const AResult: TJSONData);
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('jsonrpc', '2.0');
    Response.Add('id', AId.Clone);
    if Assigned(AResult) then
      Response.Add('result', AResult.Clone)
    else
      Response.Add('result', TJSONNull.Create);
    
    SendMessage(Response);
  finally
    Response.Free;
  end;
end;

procedure TLSPServer.SendError(const AId: TJSONData; const ACode: Integer; const AMessage: string);
var
  Response, Error: TJSONObject;
begin
  Response := TJSONObject.Create;
  Error := TJSONObject.Create;
  try
    Error.Add('code', ACode);
    Error.Add('message', AMessage);
    
    Response.Add('jsonrpc', '2.0');
    Response.Add('id', AId.Clone);
    Response.Add('error', Error);
    
    SendMessage(Response);
  finally
    Response.Free;
  end;
end;

procedure TLSPServer.SendNotification(const AMethod: string; const AParams: TJSONData);
var
  Notification: TJSONObject;
begin
  Notification := TJSONObject.Create;
  try
    Notification.Add('jsonrpc', '2.0');
    Notification.Add('method', AMethod);
    if Assigned(AParams) then
      Notification.Add('params', AParams.Clone);
    
    SendMessage(Notification);
  finally
    Notification.Free;
  end;
end;

function TLSPServer.ReadMessage: TJSONObject;
var
  Line: string;
  ContentLength: Integer;
  JsonStr: string;
  JsonData: TJSONData;
begin
  Result := nil;
  ContentLength := 0;
  
  // Ler headers
  repeat
    ReadLn(Line);
    if StartsText('Content-Length:', Line) then
    begin
      ContentLength := StrToIntDef(Copy(Line, 16, Length(Line)), 0);
    end;
  until Line = '';
  
  if ContentLength > 0 then
  begin
    // Ler JSON
    SetLength(JsonStr, ContentLength);
    BlockRead(Input, JsonStr[1], ContentLength);
    
    try
      JsonData := GetJSON(JsonStr);
      if JsonData is TJSONObject then
        Result := TJSONObject(JsonData)
      else
        JsonData.Free;
    except
      on E: Exception do
        WriteLn(StdErr, 'Erro ao parsear JSON: ', E.Message);
    end;
  end;
end;

function TLSPServer.CreatePosition(ALine, ACharacter: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('line', ALine);
  Result.Add('character', ACharacter);
end;

function TLSPServer.CreateRange(AStartLine, AStartChar, AEndLine, AEndChar: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('start', CreatePosition(AStartLine, AStartChar));
  Result.Add('end', CreatePosition(AEndLine, AEndChar));
end;

function TLSPServer.GetWordAtPosition(const AText: string; ALine, ACharacter: Integer): string;
var
  Lines: TStringList;
  CurrentLine: string;
  StartPos, EndPos: Integer;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    if (ALine < 0) or (ALine >= Lines.Count) then Exit;
    
    CurrentLine := Lines[ALine];
    if (ACharacter < 0) or (ACharacter > Length(CurrentLine)) then Exit;
    
    // Encontrar início da palavra
    StartPos := ACharacter;
    while (StartPos > 1) and (CurrentLine[StartPos - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Dec(StartPos);
    
    // Encontrar fim da palavra  
    EndPos := ACharacter;
    while (EndPos <= Length(CurrentLine)) and (CurrentLine[EndPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
      Inc(EndPos);
    
    if EndPos > StartPos then
      Result := Copy(CurrentLine, StartPos, EndPos - StartPos);
  finally
    Lines.Free;
  end;
end;

procedure TLSPServer.HandleInitialize(const AId: TJSONData; const AParams: TJSONObject);
var
  Result, Capabilities, TextDocumentSync: TJSONObject;
begin
  WriteLn(StdErr, 'Inicializando servidor LSP...');
  
  Result := TJSONObject.Create;
  Capabilities := TJSONObject.Create;
  TextDocumentSync := TJSONObject.Create;
  
  try
    // Configurar capacidades do servidor
    TextDocumentSync.Add('openClose', True);
    TextDocumentSync.Add('change', 2); // Incremental
    
    Capabilities.Add('textDocumentSync', TextDocumentSync);
    Capabilities.Add('hoverProvider', True);
    Capabilities.Add('completionProvider', TJSONObject.Create(['triggerCharacters', TJSONArray.Create(['.', ':'])]));
    
    Result.Add('capabilities', Capabilities);
    Result.Add('serverInfo', TJSONObject.Create(['name', 'Basic Pascal LSP', 'version', '1.0.0']));
    
    SendResponse(AId, Result);
  finally
    Result.Free;
  end;
end;

procedure TLSPServer.HandleInitialized(const AParams: TJSONObject);
begin
  WriteLn(StdErr, 'Servidor inicializado!');
  FInitialized := True;
end;

procedure TLSPServer.HandleShutdown(const AId: TJSONData);
begin
  WriteLn(StdErr, 'Finalizando servidor...');
  SendResponse(AId, TJSONNull.Create);
  FRunning := False;
end;

procedure TLSPServer.HandleExit;
begin
  WriteLn(StdErr, 'Saindo...');
  FRunning := False;
end;

procedure TLSPServer.HandleTextDocumentDidOpen(const AParams: TJSONObject);
var
  TextDocument: TJSONObject;
  Uri, Text: string;
begin
  TextDocument := TJSONObject(AParams.Find('textDocument'));
  if Assigned(TextDocument) then
  begin
    Uri := TextDocument.Get('uri', '');
    Text := TextDocument.Get('text', '');
    
    FDocuments.AddOrSetValue(Uri, Text);
    WriteLn(StdErr, 'Documento aberto: ', Uri);
  end;
end;

procedure TLSPServer.HandleTextDocumentDidChange(const AParams: TJSONObject);
var
  TextDocument: TJSONObject;
  ContentChanges: TJSONArray;
  Change: TJSONObject;
  Uri, Text: string;
  i: Integer;
begin
  TextDocument := TJSONObject(AParams.Find('textDocument'));
  ContentChanges := TJSONArray(AParams.Find('contentChanges'));
  
  if Assigned(TextDocument) and Assigned(ContentChanges) then
  begin
    Uri := TextDocument.Get('uri', '');
    
    // Para simplicidade, assumir mudanças completas (full sync)
    for i := 0 to ContentChanges.Count - 1 do
    begin
      Change := TJSONObject(ContentChanges[i]);
      Text := Change.Get('text', '');
      
      FDocuments.AddOrSetValue(Uri, Text);
      WriteLn(StdErr, 'Documento alterado: ', Uri);
    end;
  end;
end;

procedure TLSPServer.HandleTextDocumentDidClose(const AParams: TJSONObject);
var
  TextDocument: TJSONObject;
  Uri: string;
begin
  TextDocument := TJSONObject(AParams.Find('textDocument'));
  if Assigned(TextDocument) then
  begin
    Uri := TextDocument.Get('uri', '');
    FDocuments.Remove(Uri);
    WriteLn(StdErr, 'Documento fechado: ', Uri);
  end;
end;

procedure TLSPServer.HandleTextDocumentHover(const AId: TJSONData; const AParams: TJSONObject);
var
  TextDocument, Position: TJSONObject;
  Uri, Text, Word: string;
  Line, Character: Integer;
  Result, Contents: TJSONObject;
begin
  TextDocument := TJSONObject(AParams.Find('textDocument'));
  Position := TJSONObject(AParams.Find('position'));
  
  if Assigned(TextDocument) and Assigned(Position) then
  begin
    Uri := TextDocument.Get('uri', '');
    Line := Position.Get('line', 0);
    Character := Position.Get('character', 0);
    
    if FDocuments.TryGetValue(Uri, Text) then
    begin
      Word := GetWordAtPosition(Text, Line, Character);
      
      if Word <> '' then
      begin
        // Simular informação de hover
        Result := TJSONObject.Create;
        Contents := TJSONObject.Create;
        
        Contents.Add('kind', 'markdown');
        Contents.Add('value', Format('**%s**%s%sPalavra Pascal encontrada na posição %d:%d', 
          [Word, sLineBreak, sLineBreak, Line, Character]));
        
        Result.Add('contents', Contents);
        Result.Add('range', CreateRange(Line, Character, Line, Character + Length(Word)));
        
        SendResponse(AId, Result);
        Exit;
      end;
    end;
  end;
  
  // Sem resultado
  SendResponse(AId, TJSONNull.Create);
end;

procedure TLSPServer.HandleTextDocumentCompletion(const AId: TJSONData; const AParams: TJSONObject);
var
  CompletionList, Items: TJSONArray;
  Item: TJSONObject;
  Keywords: array[0..9] of string = (
    'begin', 'end', 'var', 'type', 'function', 'procedure', 
    'class', 'record', 'interface', 'implementation'
  );
  i: Integer;
begin
  WriteLn(StdErr, 'Solicitação de completion...');
  
  // Criar lista de completion com palavras-chave Pascal
  Items := TJSONArray.Create;
  
  for i := Low(Keywords) to High(Keywords) do
  begin
    Item := TJSONObject.Create;
    Item.Add('label', Keywords[i]);
    Item.Add('kind', 14); // Keyword
    Item.Add('detail', 'Palavra-chave Pascal');
    Item.Add('insertText', Keywords[i]);
    Items.Add(Item);
  end;
  
  CompletionList := TJSONArray.Create;
  CompletionList.Add(TJSONObject.Create(['isIncomplete', False, 'items', Items]));
  
  SendResponse(AId, Items);
end;

procedure TLSPServer.ProcessMessage(const AMessage: TJSONObject);
var
  Method: string;
  Id: TJSONData;
  Params: TJSONObject;
begin
  Method := AMessage.Get('method', '');
  Id := AMessage.Find('id');
  Params := TJSONObject(AMessage.Find('params'));
  
  WriteLn(StdErr, 'Método recebido: ', Method);
  
  // Dispatch para handlers específicos
  case Method of
    'initialize': 
      HandleInitialize(Id, Params);
    'initialized': 
      HandleInitialized(Params);
    'shutdown': 
      HandleShutdown(Id);
    'exit': 
      HandleExit;
    'textDocument/didOpen': 
      HandleTextDocumentDidOpen(Params);
    'textDocument/didChange': 
      HandleTextDocumentDidChange(Params);
    'textDocument/didClose': 
      HandleTextDocumentDidClose(Params);
    'textDocument/hover': 
      HandleTextDocumentHover(Id, Params);
    'textDocument/completion': 
      HandleTextDocumentCompletion(Id, Params);
    else
      if Assigned(Id) then
        SendError(Id, -32601, 'Método não implementado: ' + Method);
  end;
end;

procedure TLSPServer.Run;
var
  Message: TJSONObject;
begin
  WriteLn(StdErr, '=== Servidor LSP Pascal ===');
  WriteLn(StdErr, 'Aguardando conexões...');
  
  while FRunning do
  begin
    try
      Message := ReadMessage;
      if Assigned(Message) then
      begin
        try
          ProcessMessage(Message);
        finally
          Message.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        WriteLn(StdErr, 'Erro no servidor: ', E.Message);
        if not FRunning then Break;
      end;
    end;
  end;
  
  WriteLn(StdErr, 'Servidor finalizado.');
end;

// === PROGRAMA PRINCIPAL ===
var
  Server: TLSPServer;
begin
  Server := TLSPServer.Create;
  try
    Server.Run;
  finally
    Server.Free;
  end;
end.