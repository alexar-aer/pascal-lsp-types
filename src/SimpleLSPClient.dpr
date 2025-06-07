program SimpleLSPClient;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process, fpjson, jsonparser;

type
  { TLSPClient - Cliente LSP simples para testar servidores }
  TLSPClient = class
  private
    FServerProcess: TProcess;
    FRequestId: Integer;
    FInitialized: Boolean;
    FServerPath: string;
    
    function GetNextRequestId: Integer;
    function SendRequest(const AMethod: string; const AParams: TJSONObject = nil): TJSONObject;
    procedure SendNotification(const AMethod: string; const AParams: TJSONObject = nil);
    function ReadServerResponse: TJSONObject;
    procedure SendMessage(const AMessage: TJSONObject);
    function CreatePosition(ALine, ACharacter: Integer): TJSONObject;
    function CreateTextDocumentIdentifier(const AUri: string): TJSONObject;
    
  public
    constructor Create(const AServerPath: string = './BasicLSPServer');
    destructor Destroy; override;
    
    function StartServer: Boolean;
    procedure StopServer;
    
    // Métodos LSP principais
    function Initialize: Boolean;
    procedure Initialized;
    function Shutdown: Boolean;
    procedure Exit;
    
    // Document Lifecycle
    procedure DidOpenTextDocument(const AUri, ALanguageId, AText: string);
    procedure DidChangeTextDocument(const AUri, AText: string; AVersion: Integer = 1);
    procedure DidCloseTextDocument(const AUri: string);
    
    // Language Features
    function Hover(const AUri: string; ALine, ACharacter: Integer): string;
    function Completion(const AUri: string; ALine, ACharacter: Integer): TStringList;
    function GotoDefinition(const AUri: string; ALine, ACharacter: Integer): string;
    
    // Utilidades
    procedure RunInteractiveSession;
    procedure RunTestScenario;
    
    property Initialized: Boolean read FInitialized;
  end;

{ TLSPClient }

constructor TLSPClient.Create(const AServerPath: string);
begin
  inherited Create;
  FServerPath := AServerPath;
  FRequestId := 0;
  FInitialized := False;
  FServerProcess := nil;
end;

destructor TLSPClient.Destroy;
begin
  StopServer;
  inherited Destroy;
end;

function TLSPClient.GetNextRequestId: Integer;
begin
  Inc(FRequestId);
  Result := FRequestId;
end;

function TLSPClient.StartServer: Boolean;
begin
  Result := False;
  
  if not FileExists(FServerPath) then
  begin
    WriteLn('❌ Servidor não encontrado: ', FServerPath);
    Exit;
  end;
  
  WriteLn('🚀 Iniciando servidor LSP: ', FServerPath);
  
  FServerProcess := TProcess.Create(nil);
  try
    FServerProcess.Executable := FServerPath;
    FServerProcess.Options := [poUsePipes, poStderrToOutPut];
    FServerProcess.Execute;
    
    WriteLn('✅ Servidor iniciado com PID: ', FServerProcess.ProcessID);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('❌ Erro ao iniciar servidor: ', E.Message);
      FreeAndNil(FServerProcess);
    end;
  end;
end;

procedure TLSPClient.StopServer;
begin
  if Assigned(FServerProcess) then
  begin
    WriteLn('🛑 Parando servidor...');
    try
      if FInitialized then
      begin
        Shutdown;
        Exit;
      end;
      FServerProcess.Terminate(0);
    finally
      FreeAndNil(FServerProcess);
    end;
  end;
end;

procedure TLSPClient.SendMessage(const AMessage: TJSONObject);
var
  JsonStr: string;
  ContentLength: Integer;
  Header: string;
begin
  if not Assigned(FServerProcess) then
    raise Exception.Create('Servidor não iniciado');
    
  JsonStr := AMessage.AsJSON;
  ContentLength := Length(JsonStr);
  
  Header := Format('Content-Length: %d'#13#10#13#10, [ContentLength]);
  
  // Enviar header + JSON para o servidor
  FServerProcess.Input.Write(Header[1], Length(Header));
  FServerProcess.Input.Write(JsonStr[1], Length(JsonStr));
  FServerProcess.Input.Flush;
  
  WriteLn('📤 Enviado: ', AMessage.Get('method', 'response'));
end;

function TLSPClient.ReadServerResponse: TJSONObject;
var
  Line: string;
  ContentLength: Integer;
  JsonStr: string;
  JsonData: TJSONData;
  Buffer: array[0..1023] of Char;
  BytesRead: Integer;
begin
  Result := nil;
  ContentLength := 0;
  
  // Ler headers
  repeat
    Line := '';
    repeat
      BytesRead := FServerProcess.Output.Read(Buffer, 1);
      if BytesRead > 0 then
      begin
        if Buffer[0] = #13 then Continue;
        if Buffer[0] = #10 then Break;
        Line := Line + Buffer[0];
      end;
    until BytesRead = 0;
    
    if StartsText('Content-Length:', Line) then
    begin
      ContentLength := StrToIntDef(Copy(Line, 16, Length(Line)), 0);
    end;
  until Line = '';
  
  if ContentLength > 0 then
  begin
    // Ler JSON
    SetLength(JsonStr, ContentLength);
    BytesRead := FServerProcess.Output.Read(JsonStr[1], ContentLength);
    
    if BytesRead = ContentLength then
    begin
      try
        JsonData := GetJSON(JsonStr);
        if JsonData is TJSONObject then
        begin
          Result := TJSONObject(JsonData);
          WriteLn('📥 Recebido: ', Result.Get('method', 'response'));
        end
        else
          JsonData.Free;
      except
        on E: Exception do
          WriteLn('❌ Erro ao parsear resposta: ', E.Message);
      end;
    end;
  end;
end;

function TLSPClient.SendRequest(const AMethod: string; const AParams: TJSONObject): TJSONObject;
var
  Request: TJSONObject;
  RequestId: Integer;
begin
  RequestId := GetNextRequestId;
  
  Request := TJSONObject.Create;
  try
    Request.Add('jsonrpc', '2.0');
    Request.Add('id', RequestId);
    Request.Add('method', AMethod);
    if Assigned(AParams) then
      Request.Add('params', AParams.Clone);
    
    SendMessage(Request);
    Result := ReadServerResponse;
  finally
    Request.Free;
  end;
end;

procedure TLSPClient.SendNotification(const AMethod: string; const AParams: TJSONObject);
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

function TLSPClient.CreatePosition(ALine, ACharacter: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('line', ALine);
  Result.Add('character', ACharacter);
end;

function TLSPClient.CreateTextDocumentIdentifier(const AUri: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('uri', AUri);
end;

function TLSPClient.Initialize: Boolean;
var
  Params, Response: TJSONObject;
  Capabilities: TJSONObject;
begin
  Result := False;
  
  WriteLn('🔄 Inicializando cliente LSP...');
  
  Params := TJSONObject.Create;
  try
    Params.Add('processId', GetProcessID);
    Params.Add('clientInfo', TJSONObject.Create([
      'name', 'Simple Pascal LSP Client',
      'version', '1.0.0'
    ]));
    
    // Capacidades básicas do cliente
    Capabilities := TJSONObject.Create;
    Capabilities.Add('textDocument', TJSONObject.Create([
      'hover', TJSONObject.Create(['contentFormat', TJSONArray.Create(['markdown', 'plaintext'])]),
      'completion', TJSONObject.Create(['completionItem', TJSONObject.Create(['snippetSupport', False])])
    ]));
    Params.Add('capabilities', Capabilities);
    
    Response := SendRequest('initialize', Params);
    if Assigned(Response) then
    begin
      try
        if Response.Find('result') <> nil then
        begin
          WriteLn('✅ Servidor inicializado com sucesso!');
          
          // Mostrar capacidades do servidor
          Capabilities := TJSONObject(TJSONObject(Response.Find('result')).Find('capabilities'));
          if Assigned(Capabilities) then
          begin
            WriteLn('📋 Capacidades do servidor:');
            if Capabilities.Get('hoverProvider', False) then
              WriteLn('  ✅ Hover Provider');
            if Capabilities.Find('completionProvider') <> nil then
              WriteLn('  ✅ Completion Provider');
            if Capabilities.Get('definitionProvider', False) then
              WriteLn('  ✅ Definition Provider');
          end;
          
          Result := True;
        end
        else
        begin
          WriteLn('❌ Erro na inicialização: ', Response.Get('error', 'Erro desconhecido'));
        end;
      finally
        Response.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

procedure TLSPClient.Initialized;
var
  Params: TJSONObject;
begin
  WriteLn('📢 Enviando notificação initialized...');
  
  Params := TJSONObject.Create;
  try
    SendNotification('initialized', Params);
    FInitialized := True;
    WriteLn('✅ Cliente totalmente inicializado!');
  finally
    Params.Free;
  end;
end;

function TLSPClient.Shutdown: Boolean;
var
  Response: TJSONObject;
begin
  Result := False;
  
  WriteLn('🔄 Enviando shutdown...');
  
  Response := SendRequest('shutdown', nil);
  if Assigned(Response) then
  begin
    try
      Result := Response.Find('result') <> nil;
      if Result then
        WriteLn('✅ Shutdown confirmado');
    finally
      Response.Free;
    end;
  end;
end;

procedure TLSPClient.Exit;
begin
  WriteLn('🚪 Enviando exit...');
  SendNotification('exit', nil);
  FInitialized := False;
end;

procedure TLSPClient.DidOpenTextDocument(const AUri, ALanguageId, AText: string);
var
  Params, TextDocument: TJSONObject;
begin
  WriteLn('📂 Abrindo documento: ', AUri);
  
  TextDocument := TJSONObject.Create;
  TextDocument.Add('uri', AUri);
  TextDocument.Add('languageId', ALanguageId);
  TextDocument.Add('version', 1);
  TextDocument.Add('text', AText);
  
  Params := TJSONObject.Create;
  Params.Add('textDocument', TextDocument);
  
  try
    SendNotification('textDocument/didOpen', Params);
  finally
    Params.Free;
  end;
end;

procedure TLSPClient.DidChangeTextDocument(const AUri, AText: string; AVersion: Integer);
var
  Params, TextDocument, ContentChange: TJSONObject;
  ContentChanges: TJSONArray;
begin
  WriteLn('📝 Alterando documento: ', AUri);
  
  TextDocument := TJSONObject.Create;
  TextDocument.Add('uri', AUri);
  TextDocument.Add('version', AVersion);
  
  ContentChange := TJSONObject.Create;
  ContentChange.Add('text', AText);
  
  ContentChanges := TJSONArray.Create;
  ContentChanges.Add(ContentChange);
  
  Params := TJSONObject.Create;
  Params.Add('textDocument', TextDocument);
  Params.Add('contentChanges', ContentChanges);
  
  try
    SendNotification('textDocument/didChange', Params);
  finally
    Params.Free;
  end;
end;

procedure TLSPClient.DidCloseTextDocument(const AUri: string);
var
  Params: TJSONObject;
begin
  WriteLn('📄 Fechando documento: ', AUri);
  
  Params := TJSONObject.Create;
  Params.Add('textDocument', CreateTextDocumentIdentifier(AUri));
  
  try
    SendNotification('textDocument/didClose', Params);
  finally
    Params.Free;
  end;
end;

function TLSPClient.Hover(const AUri: string; ALine, ACharacter: Integer): string;
var
  Params, Response: TJSONObject;
  Contents: TJSONData;
begin
  Result := '';
  
  WriteLn(Format('🔍 Hover em %s:%d:%d', [AUri, ALine, ACharacter]));
  
  Params := TJSONObject.Create;
  try
    Params.Add('textDocument', CreateTextDocumentIdentifier(AUri));
    Params.Add('position', CreatePosition(ALine, ACharacter));
    
    Response := SendRequest('textDocument/hover', Params);
    if Assigned(Response) then
    begin
      try
        Contents := TJSONObject(Response.Find('result'));
        if Assigned(Contents) and (Contents is TJSONObject) then
        begin
          Contents := TJSONObject(Contents).Find('contents');
          if Assigned(Contents) then
          begin
            if Contents is TJSONObject then
              Result := TJSONObject(Contents).Get('value', '')
            else if Contents is TJSONString then
              Result := Contents.AsString;
          end;
        end;
      finally
        Response.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TLSPClient.Completion(const AUri: string; ALine, ACharacter: Integer): TStringList;
var
  Params, Response: TJSONObject;
  Items: TJSONArray;
  Item: TJSONObject;
  i: Integer;
begin
  Result := TStringList.Create;
  
  WriteLn(Format('💡 Completion em %s:%d:%d', [AUri, ALine, ACharacter]));
  
  Params := TJSONObject.Create;
  try
    Params.Add('textDocument', CreateTextDocumentIdentifier(AUri));
    Params.Add('position', CreatePosition(ALine, ACharacter));
    
    Response := SendRequest('textDocument/completion', Params);
    if Assigned(Response) then
    begin
      try
        Items := TJSONArray(Response.Find('result'));
        if Assigned(Items) then
        begin
          for i := 0 to Items.Count - 1 do
          begin
            Item := TJSONObject(Items[i]);
            Result.Add(Item.Get('label', ''));
          end;
        end;
      finally
        Response.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TLSPClient.GotoDefinition(const AUri: string; ALine, ACharacter: Integer): string;
var
  Params, Response: TJSONObject;
  Location: TJSONData;
begin
  Result := '';
  
  WriteLn(Format('🎯 Go to definition em %s:%d:%d', [AUri, ALine, ACharacter]));
  
  Params := TJSONObject.Create;
  try
    Params.Add('textDocument', CreateTextDocumentIdentifier(AUri));
    Params.Add('position', CreatePosition(ALine, ACharacter));
    
    Response := SendRequest('textDocument/definition', Params);
    if Assigned(Response) then
    begin
      try
        Location := Response.Find('result');
        if Assigned(Location) and (Location is TJSONObject) then
        begin
          Result := TJSONObject(Location).Get('uri', '');
        end;
      finally
        Response.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

procedure TLSPClient.RunInteractiveSession;
var
  Command, Uri, Text: string;
  Line, Character: Integer;
  HoverResult: string;
  CompletionResults: TStringList;
  i: Integer;
begin
  WriteLn('');
  WriteLn('🎮 === SESSÃO INTERATIVA LSP ===');
  WriteLn('Comandos disponíveis:');
  WriteLn('  open <uri> <texto>     - Abrir documento');
  WriteLn('  hover <uri> <line> <char> - Hover em posição');
  WriteLn('  complete <uri> <line> <char> - Completion em posição');
  WriteLn('  close <uri>            - Fechar documento');
  WriteLn('  quit                   - Sair');
  WriteLn('');
  
  repeat
    Write('lsp> ');
    ReadLn(Command);
    
    if Command = 'quit' then
      Break
    else if StartsText('open ', Command) then
    begin
      // Parsing simples: open file:///test.pas program TestCode; begin end.
      Uri := Copy(Command, 6, Pos(' ', Command + ' ', 6) - 6);
      Text := Copy(Command, 6 + Length(Uri) + 1, Length(Command));
      DidOpenTextDocument(Uri, 'pascal', Text);
    end
    else if StartsText('hover ', Command) then
    begin
      // Parsing simples: hover file:///test.pas 0 5
      sscanf(PChar(Command), 'hover %s %d %d', [@Uri, @Line, @Character]);
      HoverResult := Hover(Uri, Line, Character);
      if HoverResult <> '' then
        WriteLn('📄 Hover: ', HoverResult)
      else
        WriteLn('❌ Sem informação de hover');
    end
    else if StartsText('complete ', Command) then
    begin
      sscanf(PChar(Command), 'complete %s %d %d', [@Uri, @Line, @Character]);
      CompletionResults := Completion(Uri, Line, Character);
      try
        if CompletionResults.Count > 0 then
        begin
          WriteLn('💡 Completions:');
          for i := 0 to CompletionResults.Count - 1 do
            WriteLn('  - ', CompletionResults[i]);
        end
        else
          WriteLn('❌ Sem completions disponíveis');
      finally
        CompletionResults.Free;
      end;
    end
    else if StartsText('close ', Command) then
    begin
      Uri := Copy(Command, 7, Length(Command));
      DidCloseTextDocument(Uri);
    end
    else if Command <> '' then
    begin
      WriteLn('❌ Comando não reconhecido: ', Command);
    end;
  until False;
end;

procedure TLSPClient.RunTestScenario;
const
  TestUri = 'file:///test.pas';
  TestCode = 'program TestLSP;'#10'begin'#10'  WriteLn(''Hello'');'#10'end.';
var
  HoverResult: string;
  CompletionResults: TStringList;
  i: Integer;
begin
  WriteLn('');
  WriteLn('🧪 === CENÁRIO DE TESTE AUTOMÁTICO ===');
  
  // Teste 1: Abrir documento
  WriteLn('');
  WriteLn('📋 Teste 1: Abrindo documento...');
  DidOpenTextDocument(TestUri, 'pascal', TestCode);
  
  // Teste 2: Hover
  WriteLn('');
  WriteLn('📋 Teste 2: Testando hover...');
  HoverResult := Hover(TestUri, 1, 2);
  if HoverResult <> '' then
    WriteLn('✅ Hover funcionando: ', HoverResult)
  else
    WriteLn('⚠️  Hover vazio (normal se não há símbolo na posição)');
  
  // Teste 3: Completion
  WriteLn('');
  WriteLn('📋 Teste 3: Testando completion...');
  CompletionResults := Completion(TestUri, 1, 2);
  try
    if CompletionResults.Count > 0 then
    begin
      WriteLn('✅ Completion funcionando:');
      for i := 0 to Min(CompletionResults.Count - 1, 4) do // Mostrar só os primeiros 5
        WriteLn('  - ', CompletionResults[i]);
      if CompletionResults.Count > 5 then
        WriteLn('  ... e mais ', CompletionResults.Count - 5, ' itens');
    end
    else
      WriteLn('⚠️  Completion vazio');
  finally
    CompletionResults.Free;
  end;
  
  // Teste 4: Fechar documento
  WriteLn('');
  WriteLn('📋 Teste 4: Fechando documento...');
  DidCloseTextDocument(TestUri);
  
  WriteLn('');
  WriteLn('✅ Cenário de teste concluído!');
end;

// === PROGRAMA PRINCIPAL ===
var
  Client: TLSPClient;
  ServerPath: string;
  Mode: string;
begin
  WriteLn('');
  WriteLn('🚀 === Cliente LSP Simples ===');
  WriteLn('Versão 1.0 - Teste e demonstração');
  WriteLn('');
  
  // Verificar parâmetros
  if ParamCount >= 1 then
    ServerPath := ParamStr(1)
  else
    ServerPath := './BasicLSPServer';
    
  if ParamCount >= 2 then
    Mode := ParamStr(2)
  else
    Mode := 'interactive';
  
  Client := TLSPClient.Create(ServerPath);
  try
    // Iniciar servidor
    if not Client.StartServer then
    begin
      WriteLn('❌ Falha ao iniciar servidor');
      Exit;
    end;
    
    try
      // Inicializar protocolo LSP
      if Client.Initialize then
      begin
        Client.Initialized;
        
        // Executar modo selecionado
        case LowerCase(Mode) of
          'test': Client.RunTestScenario;
          'interactive': Client.RunInteractiveSession;
          else
          begin
            WriteLn('❌ Modo inválido: ', Mode);
            WriteLn('Modos disponíveis: test, interactive');
          end;
        end;
      end;
    finally
      Client.StopServer;
    end;
  finally
    Client.Free;
  end;
  
  WriteLn('');
  WriteLn('👋 Cliente finalizado. Obrigado!');
end.