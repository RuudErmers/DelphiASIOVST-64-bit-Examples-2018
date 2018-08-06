unit TestDAV_Complex;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Windows, Classes, DAV_Complex;
type
  // Test methods for class complex functions

  TestComplexFunctions = class(TTestCase)
  published
    procedure TestComplexPolar32;
    procedure TestComplexPolar64;
    procedure TestComplexSign32;
    procedure TestComplexSign64;
    procedure TestComplexConjugate32;
    procedure TestComplexConjugate64;
    procedure TestComplexInvert32;
    procedure TestComplexInvert64;
    procedure TestComplexMagnitude32;
    procedure TestComplexMagnitude64;
    procedure TestComplexArgument32;
    procedure TestComplexArgument64;
    procedure TestComplexAdd32;
    procedure TestComplexAdd64;
    procedure TestComplexAddInplace32;
    procedure TestComplexAddInplace64;
    procedure TestComplexSubtract32;
    procedure TestComplexSubtract64;
    procedure TestComplexSubtractInplace32;
    procedure TestComplexSubtractInplace64;
    procedure TestComplexMultiply32;
    procedure TestComplexMultiply64;
    procedure TestComplexMultiplyInplace32;
    procedure TestComplexMultiplyInplace64;
    procedure TestComplexDivide32;
    procedure TestComplexDivide64;
    procedure TestComplexDivideInplace32;
    procedure TestComplexDivideInplace64;
    procedure TestComplexReciprocal32;
    procedure TestComplexReciprocal64;
    procedure TestComplexReciprocalInplace32;
    procedure TestComplexReciprocalInplace64;
    procedure TestComplexSqr32;
    procedure TestComplexSqr64;
    procedure TestComplexSqrt32;
    procedure TestComplexSqrt64;
    procedure TestComplexLog1032;
    procedure TestComplexLog1064;
    procedure TestComplexExp32;
    procedure TestComplexExp64;
    procedure TestComplexLn32;
    procedure TestComplexLn64;
    procedure TestComplexSin32;
    procedure TestComplexSin64;
    procedure TestComplexCos32;
    procedure TestComplexCos64;
    procedure TestComplexTan32;
    procedure TestComplexTan64;
    procedure TestComplexSinh32;
    procedure TestComplexSinh64;
    procedure TestComplexCosh32;
    procedure TestComplexCosh64;
    procedure TestComplexTanh32;
    procedure TestComplexTanh64;
    procedure TestComplexArcSin32;
    procedure TestComplexArcSin64;
    procedure TestComplexArcCos32;
    procedure TestComplexArcCos64;
    procedure TestComplexArcTan32;
    procedure TestComplexArcTan64;
    procedure TestComplexArcTanh32;
    procedure TestComplexArcTanh64;
  end;

implementation

uses
  Math, DAV_Common;

const
  CEpsilon32 = 1E-5;
  CEpsilon64 = 1E-9;

var
  Cos1Single : Single;

procedure TestComplexFunctions.TestComplexPolar32;
var
  Result : TComplex32;
begin
 // real case
 Result := ComplexPolar(1, 0);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexPolar64;
var
  Mag    : Double;
  Result : TComplex64;
begin
 // real case
 Mag := 1;
 Result := ComplexPolar(Mag, 0);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexSign32;
var
  Input  : TComplex32;
  Result : Single;
begin
 // real cases
 Input.Re := 1;
 Input.Im := 0;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := -1;
 Input.Im := 0;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);

 // imaginary cases
 Input.Re := 0;
 Input.Im := 1;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := 0;
 Input.Im := -1;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);

 // complex cases
 Input.Re := 1;
 Input.Im := 1;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := -1;
 Input.Im := 1;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);

 Input.Re := 1;
 Input.Im := -1;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := -1;
 Input.Im := -1;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);
end;

procedure TestComplexFunctions.TestComplexSign64;
var
  Input  : TComplex64;
  Result : Double;
begin
 // real cases
 Input.Re := 1;
 Input.Im := 0;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := -1;
 Input.Im := 0;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);

 // imaginary cases
 Input.Re := 0;
 Input.Im := 1;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := 0;
 Input.Im := -1;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);

 // complex cases
 Input.Re := 1;
 Input.Im := 1;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := -1;
 Input.Im := 1;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);

 Input.Re := 1;
 Input.Im := -1;
 Result := ComplexSign(Input);
 CheckEquals(Result, 1);

 Input.Re := -1;
 Input.Im := -1;
 Result := ComplexSign(Input);
 CheckEquals(Result, -1);
end;

procedure TestComplexFunctions.TestComplexConjugate32;
var
  Result : TComplex32;
begin
 Result.Re := 1;
 Result.Im := 2;
 Result := ComplexConjugate(Result.Re, Result.Im);
 CheckEquals(Result.Re,  1);
 CheckEquals(Result.Im, -2);

 Result := ComplexConjugate(Result);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 2);
end;

procedure TestComplexFunctions.TestComplexConjugate64;
var
  Result : TComplex64;
begin
 Result.Re := 1;
 Result.Im := 2;
 Result := ComplexConjugate(Result.Re, Result.Im);
 CheckEquals(Result.Re,  1);
 CheckEquals(Result.Im, -2);

 Result := ComplexConjugate(Result);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 2);
end;

procedure TestComplexFunctions.TestComplexInvert32;
var
  Result : TComplex32;
begin
 Result.Re := 1;
 Result.Im := 2;
 Result := ComplexInvert(Result.Re, Result.Im);
 CheckEquals(Result.Re, -1);
 CheckEquals(Result.Im, -2);

 Result := ComplexInvert(Result);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 2);
end;

procedure TestComplexFunctions.TestComplexInvert64;
var
  Result : TComplex64;
begin
 Result.Re := 1;
 Result.Im := 2;
 Result := ComplexInvert(Result.Re, Result.Im);
 CheckEquals(Result.Re, -1);
 CheckEquals(Result.Im, -2);

 Result := ComplexInvert(Result);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 2);
end;

procedure TestComplexFunctions.TestComplexMagnitude32;
var
  Value  : TComplex32;
  Result : Single;
begin
 // real case
 Value.Re := -2;
 Value.Im :=  0;
 Result := ComplexMagnitude(Value);
 CheckEquals(Result, 2);

 Result := ComplexMagnitude(Value.Re, Value.Im);
 CheckEquals(Result, 2);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -2;
 Result := ComplexMagnitude(Value);
 CheckEquals(Result, 2);

 Result := ComplexMagnitude(Value.Re, Value.Im);
 CheckEquals(Result, 2);
end;

procedure TestComplexFunctions.TestComplexMagnitude64;
var
  Value  : TComplex64;
  Result : Double;
begin
 // real case
 Value.Re := -2;
 Value.Im :=  0;
 Result := ComplexMagnitude(Value);
 CheckEquals(Result, 2);

 Result := ComplexMagnitude(Value.Re, Value.Im);
 CheckEquals(Result, 2);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -2;
 Result := ComplexMagnitude(Value);
 CheckEquals(Result, 2);

 Result := ComplexMagnitude(Value.Re, Value.Im);
 CheckEquals(Result, 2);
end;

procedure TestComplexFunctions.TestComplexArgument32;
var
  Value  : TComplex32;
  Result : Single;
begin
 // real case
 Value.Re := -2;
 Value.Im :=  0;
 Result := ComplexArgument(Value);
 CheckTrue(abs(Result - Pi) < CEpsilon32);

 Result := ComplexArgument(Value.Re, Value.Im);
 CheckTrue(abs(Result - Pi) < CEpsilon32);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -2;
 Result := ComplexArgument(Value);
 CheckTrue(abs(Result + 0.5 * Pi) < CEpsilon32);

 Result := ComplexArgument(Value.Re, Value.Im);
 CheckTrue(abs(Result + 0.5 * Pi) < CEpsilon32);
end;

procedure TestComplexFunctions.TestComplexArgument64;
var
  Value  : TComplex64;
  Result : Double;
begin
 // real case
 Value.Re := -2;
 Value.Im :=  0;
 Result := ComplexArgument(Value);
 CheckTrue(abs(Result - Pi) < CEpsilon64);

 Result := ComplexArgument(Value.Re, Value.Im);
 CheckTrue(abs(Result - Pi) < CEpsilon64);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -2;
 Result := ComplexArgument(Value);
 CheckTrue(abs(Result + 0.5 * Pi) < CEpsilon64);

 Result := ComplexArgument(Value.Re, Value.Im);
 CheckTrue(abs(Result + 0.5 * Pi) < CEpsilon64);
end;

procedure TestComplexFunctions.TestComplexAdd32;
var
  A, B   : TComplex32;
  Result : TComplex32;
begin
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexAdd(A, B);
 CheckEquals(Result.Re, 3);
 CheckEquals(Result.Im, 3);

 Result := ComplexAdd(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 3);
 CheckEquals(Result.Im, 3);
end;

procedure TestComplexFunctions.TestComplexAdd64;
var
  A, B   : TComplex64;
  Result : TComplex64;
begin
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexAdd(A, B);
 CheckEquals(Result.Re, 3);
 CheckEquals(Result.Im, 3);

 Result := ComplexAdd(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 3);
 CheckEquals(Result.Im, 3);
end;

procedure TestComplexFunctions.TestComplexAddInplace32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 Result.Re := 1;
 Result.Im := 1;
 Value.Re := 2;
 Value.Im := 2;

 ComplexAddInplace(Result, Value);
 CheckEquals(Result.Re, 3);
 CheckEquals(Result.Im, 3);

 ComplexAddInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, 5);
 CheckEquals(Result.Im, 5);
end;

procedure TestComplexFunctions.TestComplexAddInplace64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 Result.Re := 1;
 Result.Im := 1;
 Value.Re := 2;
 Value.Im := 2;

 ComplexAddInplace(Result, Value);
 CheckEquals(Result.Re, 3);
 CheckEquals(Result.Im, 3);

 ComplexAddInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, 5);
 CheckEquals(Result.Im, 5);
end;

procedure TestComplexFunctions.TestComplexSubtract32;
var
  A, B   : TComplex32;
  Result : TComplex32;
begin
 A.Re := 5;
 A.Im := 3;
 B.Re := 4;
 B.Im := 2;

 Result := ComplexSubtract(A, B);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 1);

 Result := ComplexSubtract(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 1);
end;

procedure TestComplexFunctions.TestComplexSubtract64;
var
  A, B   : TComplex64;
  Result : TComplex64;
begin
 A.Re := 5;
 A.Im := 3;
 B.Re := 4;
 B.Im := 2;

 Result := ComplexSubtract(A, B);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 1);

 Result := ComplexSubtract(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 1);
end;

procedure TestComplexFunctions.TestComplexSubtractInplace32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 Result.Re := 2;
 Result.Im := 3;
 Value.Re := 1;
 Value.Im := 2;

 ComplexSubtractInplace(Result, Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 1);

 ComplexSubtractInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, 0);
 CheckEquals(Result.Im, -1);
end;

procedure TestComplexFunctions.TestComplexSubtractInplace64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 Result.Re := 2;
 Result.Im := 3;
 Value.Re := 1;
 Value.Im := 2;

 ComplexSubtractInplace(Result, Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 1);

 ComplexSubtractInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, 0);
 CheckEquals(Result.Im, -1);
end;

procedure TestComplexFunctions.TestComplexMultiply32;
var
  A, B   : TComplex32;
  Result : TComplex32;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexMultiply(A, B);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im :=  2;

 Result := ComplexMultiply(A, B);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 1;

 Result := ComplexMultiply(A, B);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 3);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 3);
end;

procedure TestComplexFunctions.TestComplexMultiply64;
var
  A, B   : TComplex64;
  Result : TComplex64;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexMultiply(A, B);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im :=  2;

 Result := ComplexMultiply(A, B);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 1;

 Result := ComplexMultiply(A, B);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 3);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 3);
end;

procedure TestComplexFunctions.TestComplexMultiplyInplace32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := -1;
 Value.Im :=  0;
 Result.Re := -2;
 Result.Im :=  0;

 ComplexMultiplyInplace(Result, Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 ComplexMultiplyInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -1;
 Result.Re :=  0;
 Result.Im :=  2;

 ComplexMultiplyInplace(Result, Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 ComplexMultiplyInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re,  0);
 CheckEquals(Result.Im, -2);

 // complex case
 Value.Re := 1;
 Value.Im := 1;
 Result.Re := 2;
 Result.Im := 1;

 ComplexMultiplyInplace(Result, Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 3);

 ComplexMultiplyInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im,  4);
end;

procedure TestComplexFunctions.TestComplexMultiplyInplace64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := -1;
 Value.Im :=  0;
 Result.Re := -2;
 Result.Im :=  0;

 ComplexMultiplyInplace(Result, Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 ComplexMultiplyInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -1;
 Result.Re :=  0;
 Result.Im :=  2;

 ComplexMultiplyInplace(Result, Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 ComplexMultiplyInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re,  0);
 CheckEquals(Result.Im, -2);

 // complex case
 Value.Re := 1;
 Value.Im := 1;
 Result.Re := 2;
 Result.Im := 1;

 ComplexMultiplyInplace(Result, Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 3);

 ComplexMultiplyInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im,  4);
end;

procedure TestComplexFunctions.TestComplexDivide32;
var
  A, B   : TComplex32;
  Result : TComplex32;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexDivide(A, B);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im := -2;

 Result := ComplexDivide(A, B);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexDivide(A, B);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexDivide64;
var
  A, B   : TComplex64;
  Result : TComplex64;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexDivide(A, B);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im := -2;

 Result := ComplexDivide(A, B);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexDivide(A, B);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexDivideInplace32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := -2;
 Value.Im :=  0;
 Result.Re := -1;
 Result.Im :=  0;

 ComplexDivideInplace(Result, Value);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 ComplexDivideInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, -0.25);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -2;
 Result.Re :=  0;
 Result.Im := -1;

 ComplexDivideInplace(Result, Value);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 ComplexDivideInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, 0);
 CheckEquals(Result.Im, 0.25);

 // complex case
 Value.Re := 2;
 Value.Im := 2;
 Result.Re := 1;
 Result.Im := 1;

 ComplexDivideInplace(Result, Value);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 ComplexDivideInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re,  0.125);
 CheckEquals(Result.Im, -0.125);
end;

procedure TestComplexFunctions.TestComplexDivideInplace64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := -2;
 Value.Im :=  0;
 Result.Re := -1;
 Result.Im :=  0;

 ComplexDivideInplace(Result, Value);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 ComplexDivideInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, -0.25);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Value.Re :=  0;
 Value.Im := -2;
 Result.Re :=  0;
 Result.Im := -1;

 ComplexDivideInplace(Result, Value);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 ComplexDivideInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re, 0);
 CheckEquals(Result.Im, 0.25);

 // complex case
 Value.Re := 2;
 Value.Im := 2;
 Result.Re := 1;
 Result.Im := 1;

 ComplexDivideInplace(Result, Value);
 CheckEquals(Result.Re, 0.5);
 CheckEquals(Result.Im, 0);

 ComplexDivideInplace(Result.Re, Result.Im, Value.Re, Value.Im);
 CheckEquals(Result.Re,  0.125);
 CheckEquals(Result.Im, -0.125);
end;

procedure TestComplexFunctions.TestComplexReciprocal32;
var
  Result : TComplex32;
begin
 // real case
 Result.Re := -2;
 Result.Im :=  0;

 Result := ComplexReciprocal(Result);
 CheckEquals(Result.Re, -0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexReciprocal(Result.Re, Result.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Result.Re :=  0;
 Result.Im := -2;

 Result := ComplexReciprocal(Result);
 CheckEquals(Result.Re,  0  );
 CheckEquals(Result.Im, -0.5);

 Result := ComplexReciprocal(Result.Re, Result.Im);
 CheckEquals(Result.Re,  0);
 CheckEquals(Result.Im, -2);

 // complex case
 Result.Re :=  2;
 Result.Im := -2;

 Result := ComplexReciprocal(Result);
 CheckEquals(Result.Re,  0.25);
 CheckEquals(Result.Im, -0.25);

 Result := ComplexReciprocal(Result.Re, Result.Im);
 CheckEquals(Result.Re,  2);
 CheckEquals(Result.Im, -2);
end;

procedure TestComplexFunctions.TestComplexReciprocal64;
var
  Result : TComplex64;
begin
 // real case
 Result.Re := -2;
 Result.Im :=  0;

 Result := ComplexReciprocal(Result);
 CheckEquals(Result.Re, -0.5);
 CheckEquals(Result.Im, 0);

 Result := ComplexReciprocal(Result.Re, Result.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Result.Re :=  0;
 Result.Im := -2;

 Result := ComplexReciprocal(Result);
 CheckEquals(Result.Re,  0  );
 CheckEquals(Result.Im, -0.5);

 Result := ComplexReciprocal(Result.Re, Result.Im);
 CheckEquals(Result.Re,  0);
 CheckEquals(Result.Im, -2);

 // complex case
 Result.Re :=  2;
 Result.Im := -2;

 Result := ComplexReciprocal(Result);
 CheckEquals(Result.Re,  0.25);
 CheckEquals(Result.Im, -0.25);

 Result := ComplexReciprocal(Result.Re, Result.Im);
 CheckEquals(Result.Re,  2);
 CheckEquals(Result.Im, -2);
end;

procedure TestComplexFunctions.TestComplexReciprocalInplace32;
var
  Result : TComplex32;
begin
 // real case
 Result.Re := -2;
 Result.Im :=  0;

 ComplexReciprocalInplace(Result);
 CheckEquals(Result.Re, -0.5);
 CheckEquals(Result.Im, 0);

 ComplexReciprocalInplace(Result.Re, Result.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Result.Re :=  0;
 Result.Im := -2;

 ComplexReciprocalInplace(Result);
 CheckEquals(Result.Re,  0  );
 CheckEquals(Result.Im, -0.5);

 ComplexReciprocalInplace(Result.Re, Result.Im);
 CheckEquals(Result.Re,  0);
 CheckEquals(Result.Im, -2);

 // complex case
 Result.Re :=  2;
 Result.Im := -2;

 ComplexReciprocalInplace(Result);
 CheckEquals(Result.Re,  0.25);
 CheckEquals(Result.Im, -0.25);

 ComplexReciprocalInplace(Result.Re, Result.Im);
 CheckEquals(Result.Re,  2);
 CheckEquals(Result.Im, -2);
end;

procedure TestComplexFunctions.TestComplexReciprocalInplace64;
var
  Result : TComplex64;
begin
 // real case
 Result.Re := -2;
 Result.Im :=  0;

 ComplexReciprocalInplace(Result);
 CheckEquals(Result.Re, -0.5);
 CheckEquals(Result.Im, 0);

 ComplexReciprocalInplace(Result.Re, Result.Im);
 CheckEquals(Result.Re, -2);
 CheckEquals(Result.Im, 0);

 // imaginary case
 Result.Re :=  0;
 Result.Im := -2;

 ComplexReciprocalInplace(Result);
 CheckEquals(Result.Re,  0  );
 CheckEquals(Result.Im, -0.5);

 ComplexReciprocalInplace(Result.Re, Result.Im);
 CheckEquals(Result.Re,  0);
 CheckEquals(Result.Im, -2);

 // complex case
 Result.Re :=  2;
 Result.Im := -2;

 ComplexReciprocalInplace(Result);
 CheckEquals(Result.Re,  0.25);
 CheckEquals(Result.Im, -0.25);

 ComplexReciprocalInplace(Result.Re, Result.Im);
 CheckEquals(Result.Re,  2);
 CheckEquals(Result.Im, -2);
end;

procedure TestComplexFunctions.TestComplexSqr32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 2;
 Value.Im := 0;

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqr64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 2;
 Value.Im := 0;

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqrt32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 4;
 Value.Im := 0;

 Result := ComplexSqrt(Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 Result := ComplexSqrt(Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqrt64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 4;
 Value.Im := 0;

 Result := ComplexSqrt(Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

 Result := ComplexSqrt(Value);
 CheckEquals(Result.Re, 2);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexLog1032;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 10;
 Value.Im := 0;

 Result := ComplexLog10(Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);

 Result := ComplexLog10(Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexLog1064;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 10;
 Value.Im := 0;

 Result := ComplexLog10(Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);

 Result := ComplexLog10(Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexExp32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 // complex case
 Value.Re := 1;
 Value.Im := Pi;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon32);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon32);
 CheckTrue(Result.Im < CEpsilon32);
end;

procedure TestComplexFunctions.TestComplexExp64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 // complex case
 Value.Re := 1;
 Value.Im := Pi;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
end;

procedure TestComplexFunctions.TestComplexLn32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := exp(1);
 Value.Im := 0;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re - 1) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re - 1) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexLn64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := exp(1);
 Value.Im := 0;

 Result := ComplexLn(Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);

 Result := ComplexLn(Value);
 CheckEquals(Result.Re, 1);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSin32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSin64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSinh32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexSinh(Value);
 CheckTrue(abs(Result.Re - sinh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexSinh(Value);
 CheckTrue(abs(Result.Re - sinh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexSinh64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexSinh(Value);
 CheckTrue(abs(Result.Re - sinh(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 Result := ComplexSinh(Value);
 CheckTrue(abs(Result.Re - sinh(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexCos32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexCos(Value);
 CheckEquals(Result.Re, Cos1Single);
 CheckEquals(Result.Im, 0);

 Result := ComplexCos(Value);
 CheckEquals(Result.Re, Cos1Single);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexCos64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re - Cos(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re - Cos(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexCosh32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexCosh(Value);
 CheckTrue(abs(Result.Re - Cosh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexCosh(Value);
 CheckTrue(abs(Result.Re - Cosh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexCosh64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexCosh(Value);
 CheckTrue(abs(Result.Re - Cosh(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 Result := ComplexCosh(Value);
 CheckTrue(abs(Result.Re - Cosh(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexTan32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTan64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTanh32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexTanh(Value.Re, Value.Im);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTanh64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexArcCos32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcCos(Value);
 CheckTrue(abs(Result.Re - ArcCos(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcCos64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcCos(Value);
 CheckTrue(abs(Result.Re - ArcCos(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcSin32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcSin(Value);
 CheckTrue(abs(Result.Re - ArcSin(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcSin64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcSin(Value);
 CheckTrue(abs(Result.Re - ArcSin(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcTan32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcTan(Value);
 CheckTrue(abs(Result.Re - ArcTan(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcTan64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - ArcTan(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcTanh32;
var
  Value  : TComplex32;
  Result : TComplex32;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - ArcTanh(1)) < CEpsilon32);
 CheckEquals(Result.Im, 0);
end;

procedure TestComplexFunctions.TestComplexArcTanh64;
var
  Value  : TComplex64;
  Result : TComplex64;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - ArcTanh(1)) < CEpsilon64);
 CheckEquals(Result.Im, 0);
end;

initialization
  Cos1Single := Cos(1);
  RegisterTest(TestComplexFunctions.Suite);

end.
