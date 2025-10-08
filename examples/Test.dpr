program TestPascalLSP;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type
  // Тестирование подсказки при наведении курсора на типы
  TPerson = record
    Name: string;
    Age: Integer;
  end;

  // Тестирование автодополнения в классах
  TCalculator = class
  private
    FValue: Double;
  public
    constructor Create;
    function Add(AValue: Double): Double;
    function Multiply(AValue: Double): Double;
    property Value: Double read FValue write FValue;
  end;

// === РЕАЛИЗАЦИЯ ===
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

// === ГЛАВНАЯ ПРОГРАММА ===
var
  Person: TPerson;
  Calc: TCalculator;
  ResultValue: Double;

begin
  WriteLn('=== Тест Pascal LSP ===');
  
  // Тест подсказки при наведении - наведите мышь на 'Person'
  Person.Name := 'João';
  Person.Age := 30;
  
  WriteLn('Имя: ', Person.Name);
  WriteLn('Возраст: ', Person.Age);
  
  // Тест автодополнения - введите 'Calc.' и посмотрите предложения
  Calc := TCalculator.Create;
  try
    ResultValue := Calc.Add(10);
    ResultValue := Calc.Multiply(2);
    
    WriteLn('Результат: ', ResultValue:0:2);
  finally
    Calc.Free;
  end;
  
  // Тест автодополнения ключевых слов - введите 'beg' и посмотрите 'begin'
  WriteLn('Тест завершён!');
end.

// === ПРЕДЛОЖЕННЫЕ УЛУЧШЕНИЯ ДЛЯ СЕРВЕРА ===

(*
unit LSPServerEnhanced;

// 1. ДОБАВИТЬ РЕАЛЬНЫЙ РАЗБОР PASCAL
uses
  PasTree, PasResolve, FPPascalParser; // Использовать реальный парсер

type
  /// <summary>
  /// Улучшенный LSP-сервер для языка Pascal
  /// </summary>
  TEnhancedLSPServer = class(TLSPServer)
  private
    FParser: TFPPascalParser;
    FSymbolTable: TSymbolTable;
    
    /// <summary>
    /// Разбирает содержимое документа и обновляет внутреннее представление
    /// </summary>
    /// <param name="AUri">URI документа</param>
    /// <param name="AText">Текст документа</param>
    procedure ParseDocument(const AUri, AText: string);
    
    /// <summary>
    /// Находит символ по заданной позиции в документе
    /// </summary>
    /// <param name="AUri">URI документа</param>
    /// <param name="ALine">Номер строки</param>
    /// <param name="AChar">Позиция символа в строке</param>
    /// <returns>Найденный символ или nil</returns>
    function FindSymbolAtPosition(const AUri: string; ALine, AChar: Integer): TSymbol;
    
    /// <summary>
    /// Возвращает список вариантов автодополнения для заданной позиции
    /// </summary>
    /// <param name="AUri">URI документа</param>
    /// <param name="ALine">Номер строки</param>
    /// <param name="AChar">Позиция символа в строке</param>
    /// <returns>Массив элементов автодополнения</returns>
    function GetSymbolCompletions(const AUri: string; ALine, AChar: Integer): TArray<TCompletionItem>;
    
  protected
    // Переопределить методы для использования реального парсера
    procedure HandleTextDocumentDidChange(const AParams: TJSONObject); override;
    procedure HandleTextDocumentHover(const AId: TJSONData; const AParams: TJSONObject); override;
    procedure HandleTextDocumentCompletion(const AId: TJSONData; const AParams: TJSONObject); override;
    
    // Новые методы
    procedure HandleTextDocumentDefinition(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentReferences(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentDocumentSymbol(const AId: TJSONData; const AParams: TJSONObject);
    procedure HandleTextDocumentDiagnostics(const AUri: string);
    
  end;

// 2. РЕАЛИЗОВАТЬ ДИАГНОСТИКУ (ОШИБКИ/ПРЕДУПРЕЖДЕНИЯ)
/// <summary>
/// Обрабатывает диагностику текстового документа, выявляя ошибки и предупреждения
/// </summary>
/// <param name="AUri">URI документа для диагностики</param>
procedure TEnhancedLSPServer.HandleTextDocumentDiagnostics(const AUri: string);
var
  Diagnostics: TJSONArray;
  Diagnostic: TJSONObject;
  Text: string;
  Errors: TArray<TParseError>;
  Error: TParseError;
begin
  if not FDocuments.TryGetValue(AUri, Text) then Exit;
  
  // Компилировать/проверять код
  Errors := FParser.ValidateCode(Text);
  Diagnostics := TJSONArray.Create;
  
  for Error in Errors do
  begin
    Diagnostic := TJSONObject.Create;
    Diagnostic.Add('range', CreateRange(Error.Line, Error.Column, Error.Line, Error.Column + Error.Length));
    Diagnostic.Add('severity', IfThen(Error.IsWarning, 2, 1)); // Ошибка=1, Предупреждение=2
    Diagnostic.Add('source', 'pascal-lsp');
    Diagnostic.Add('message', Error.Message);
    Diagnostics.Add(Diagnostic);
  end;
  
  // Отправить диагностику
  SendNotification('textDocument/publishDiagnostics', 
    TJSONObject.Create(['uri', AUri, 'diagnostics', Diagnostics]));
end;

// 3. РЕАЛИЗОВАТЬ ПЕРЕХОД К ОПРЕДЕЛЕНИЮ
/// <summary>
/// Обрабатывает запрос на переход к определению символа
/// </summary>
/// <param name="AId">Идентификатор запроса</param>
/// <param name="AParams">Параметры запроса</param>
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

// 4. РЕАЛИЗОВАТЬ СИМВОЛЫ ДОКУМЕНТА
/// <summary>
/// Обрабатывает запрос на получение символов документа (функции, процедуры, типы и т.д.)
/// </summary>
/// <param name="AId">Идентификатор запроса</param>
/// <param name="AParams">Параметры запроса</param>
procedure TEnhancedLSPServer.HandleTextDocumentDocumentSymbol(const AId: TJSONData; const AParams: TJSONObject);
var
  Symbols: TJSONArray;
  Symbol: TJSONObject;
  ParsedSymbols: TArray<TDocumentSymbol>;
  DocSymbol: TDocumentSymbol;
begin
  // Извлечь символы из документа (functions, procedures, types, etc.)
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

// 5. РЕАЛИЗОВАТЬ ФОРМАТИРОВАНИЕ
/// <summary>
/// Обрабатывает запрос на форматирование текстового документа
/// </summary>
/// <param name="AId">Идентификатор запроса</param>
/// <param name="AParams">Параметры запроса</param>
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
    
    // Форматировать код Pascal
    FormattedText := FormatPascalCode(Text, TabSize, InsertSpaces);
    
    if FormattedText <> Text then
    begin
      TextEdits := TJSONArray.Create;
      Edit := TJSONObject.Create;
      
      // Заменить весь документ
      Edit.Add('range', CreateRange(0, 0, GetLineCount(Text), 0));
      Edit.Add('newText', FormattedText);
      TextEdits.Add(Edit);
      
      SendResponse(AId, TextEdits);
    end
    else
      SendResponse(AId, TJSONArray.Create); // Без изменений
  end;
end;

*)

//TODO: === ПЛАН РАЗРАБОТКИ ФУНКЦИОНАЛОВ ===
(*
БАЗОВЫЕ ФУНКЦИИ (✅ Реализовано):
- ✅ Initialize/Shutdown
- ✅ Синхронизация документов (открытие/изменение/закрытие)  
- ✅ Базовая подсказка при наведении
- ✅ Базовое автодополнение (ключевые слова)

СЛЕДУЮЩИЕ ФУНКЦИИ (🚧 Задачи):
- 🚧 Реальный разбор Pascal
- 🚧 Переход к определению
- 🚧 Найти ссылки  
- 🚧 Символы документа (структура)
- 🚧 Диагностика (ошибки/предупреждения)
- 🚧 Форматирование кода
- 🚧 Переименование символа
- 🚧 Действия с кодом (быстрые исправления)
- 🚧 Справка по сигнатуре (параметры функции)

ПРОДВИНУТЫЕ ФУНКЦИИ (📋 Пожелания Features):
- 📋 Символы рабочей области (глобальный поиск)
- 📋 Иерархия вызовов
- 📋 Иерархия типов  
- 📋 Диапазоны сворачивания
- 📋 Диапазоны выделения
- 📋 Семантические токены (подсветка синтаксиса)
- 📋 Встроенные подсказки (неявные типы)
- 📋 Информация в строке кода (inline references)

ИНТЕГРАЦИИ (🔧 Технический долг):
- 🔧 Компилятор FPC/Delphi
- 🔧 Отладчик (DAP)
- 🔧 Модульное тестирование
- 🔧 Покрытие кода
- 🔧 Статический анализ
- 🔧 Менеджер пакетов
*)

// === ПРИМЕР ИСПОЛЬЗОВАНИЯ В VS CODE ===

(*
1. Открыть test.pas в VS Code
2. Навести курсор на 'TPerson' → Посмотреть документацию
3. Ввести 'Calc.' → Посмотреть автодополнение с методами
4. Ctrl+Click на 'TCalculator' → Перейти к определению (в будущем)
5. Ctrl+Shift+O → Посмотреть символы документа (в будущем)
6. F2 на переменной → Переименовать (в будущем)
*)

end.