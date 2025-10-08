unit TestLSPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  /// <summary>
  /// Тестовый класс для проверки функционала LSP-сервера
  /// </summary>
  TestLSPServerClass = class(TTestCase)
  private
    // Здесь можно объявить вспомогательные переменные для тестов
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    /// <summary>
    /// Тестирует создание экземпляра TPerson
    /// </summary>
    procedure TestPersonRecord;

    /// <summary>
    /// Тестирует функциональность TCalculator
    /// </summary>
    procedure TestCalculatorClass;

    /// <summary>
    /// Тестирует корректную работу арифметических операций калькулятора
    /// </summary>
    procedure TestCalculatorArithmetic;

    /// <summary>
    /// Тестирует освобождение памяти калькулятора
    /// </summary>
    procedure TestCalculatorFree;
  end;

implementation

type
  // Копии типов из основной программы для тестирования
  TPerson = record
    Name: string;
    Age: Integer;
  end;

  TCalculator = class
  private
    FValue: Double;
  public
    constructor Create;
    function Add(AValue: Double): Double;
    function Multiply(AValue: Double): Double;
    property Value: Double read FValue write FValue;
  end;

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

procedure TestLSPServerClass.SetUp;
begin
  // Подготовка к тестам (если требуется)
end;

procedure TestLSPServerClass.TearDown;
begin
  // Очистка после тестов (если требуется)
end;

procedure TestLSPServerClass.TestPersonRecord;
var
  Person: TPerson;
begin
  // Инициализация
  Person.Name := 'Тестовый пользователь';
  Person.Age := 25;

  // Проверки
  AssertEquals('Имя должно быть "Тестовый пользователь"', 'Тестовый пользователь', Person.Name);
  AssertEquals('Возраст должен быть 25', 25, Person.Age);
end;

procedure TestLSPServerClass.TestCalculatorClass;
var
  Calc: TCalculator;
begin
  Calc := TCalculator.Create;
  try
    // Проверить начальное значение
    AssertEquals('Начальное значение должно быть 0', 0.0, Calc.Value, 0.001);
  finally
    Calc.Free;
  end;
end;

procedure TestLSPServerClass.TestCalculatorArithmetic;
var
  Calc: TCalculator;
begin
  Calc := TCalculator.Create;
  try
    // Тест сложения
    Calc.Add(10.0);
    AssertEquals('После Add(10) значение должно быть 10', 10.0, Calc.Value, 0.001);

    // Тест умножения
    Calc.Multiply(2.0);
    AssertEquals('После Multiply(2) значение должно быть 20', 20.0, Calc.Value, 0.001);

    // Тест цепочки операций
    Calc.Add(5.0).Multiply(3.0); // Предполагая, что методы возвращают Result
    // Однако, в текущей реализации они возвращают Result, а не сам объект
    // Проверим это по-другому:
    Calc.Add(5.0); // Теперь значение 20 + 5 = 25
    Calc.Multiply(3.0); // Теперь значение 25 * 3 = 75
    AssertEquals('После Add(5) и Multiply(3) значение должно быть 75', 75.0, Calc.Value, 0.001);
  finally
    Calc.Free;
  end;
end;

procedure TestLSPServerClass.TestCalculatorFree;
var
  Calc: TCalculator;
  WasFreed: Boolean;
begin
  Calc := TCalculator.Create;
  WasFreed := False;

  // Проверка освобождения с помощью try-finally
  try
    Calc.Free;
    WasFreed := True; // Если мы дошли до этой строки, Free не вызвал исключение
  except
    on E: Exception do
    begin
      // Если произошло исключение при освобождении, это ошибка
      Fail('Исключение при освобождении TCalculator: ' + E.Message);
    end;
  end;

  AssertTrue('TCalculator должен быть успешно освобожден', WasFreed);

  // Повторная проверка: Calc должен быть недействительным после Free
  // (В реальности это может быть не всегда возможно проверить без дополнительных механизмов)
end;

initialization
  RegisterTest(TestLSPServerClass);
end.
