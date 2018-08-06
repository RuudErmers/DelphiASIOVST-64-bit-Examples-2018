unit TestGuiFixedPoint;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Windows, Controls, Types, Classes, SysUtils, Messages,
  Graphics, DAV_GuiCommon, DAV_FixedPoint;

type
  TCustomTestFixedPoint = class(TTestCase)
  protected
    procedure TestAdd; virtual; abstract;
    procedure TestSub; virtual; abstract;
    procedure TestMul; virtual; abstract;
    procedure TestDiv; virtual; abstract;
    procedure TestSqr; virtual; abstract;
    procedure TestFloor; virtual; abstract;
    procedure TestCeil; virtual; abstract;
    procedure TestRound; virtual; abstract;
    procedure TestRoundHalfUp; virtual; abstract;
    procedure TestReciprocal; virtual; abstract;
    procedure TestSqrt; virtual; abstract;
    procedure TestConvert; virtual; abstract;
  end;

  TTestFixedPoint8Dot24 = class(TCustomTestFixedPoint)
  published
    procedure TestAdd; override;
    procedure TestSub; override;
    procedure TestMul; override;
    procedure TestDiv; override;
    procedure TestSqr; override;
    procedure TestFloor; override;
    procedure TestCeil; override;
    procedure TestRound; override;
    procedure TestRoundHalfUp; override;
    procedure TestReciprocal; override;
    procedure TestConvert; override;
  end;

  TTestFixedPoint16Dot16 = class(TCustomTestFixedPoint)
  published
    procedure TestAdd; override;
    procedure TestSub; override;
    procedure TestMul; override;
    procedure TestDiv; override;
    procedure TestSqr; override;
    procedure TestFloor; override;
    procedure TestCeil; override;
    procedure TestRound; override;
    procedure TestRoundHalfUp; override;
    procedure TestReciprocal; override;
    procedure TestSqrt; override;
    procedure TestConvert; override;
  end;

  TTestFixedPoint24Dot8 = class(TCustomTestFixedPoint)
  published
    procedure TestAdd; override;
    procedure TestSub; override;
    procedure TestMul; override;
    procedure TestDiv; override;
    procedure TestSqr; override;
    procedure TestFloor; override;
    procedure TestCeil; override;
    procedure TestRound; override;
    procedure TestRoundHalfUp; override;
    procedure TestReciprocal; override;
    procedure TestSqrt; override;
    procedure TestConvert; override;
  end;

implementation

uses
  DAV_Math, Math;

{ TTestFixedPoint8Dot24 }

procedure TTestFixedPoint8Dot24.TestAdd;
var
  A, B, C : TFixed8Dot24;
  Index   : Integer;
begin
 inherited;
 A := CFixed8Dot24Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed8Dot24(Index);
   C := FixedAdd(A, B);
   CheckEquals(A.Fixed + B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint8Dot24.TestSub;
var
  A, B, C : TFixed8Dot24;
  Index   : Integer;
begin
 inherited;
 A := CFixed8Dot24Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed8Dot24(Index);
   C := FixedSub(A, B);
   CheckEquals(A.Fixed - B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint8Dot24.TestMul;
var
  A, B, C : TFixed8Dot24;
  Index   : Integer;
begin
 inherited;
 A := CFixed8Dot24Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed8Dot24(Index);
   C := FixedMul(A, B);
   CheckEquals(Round(A.Fixed * CFixed8Dot24ToFloat * B.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint8Dot24.TestDiv;
var
  A, B, C : TFixed8Dot24;
  Index   : Integer;
  Floats  : array [0..2] of Single;
begin
 inherited;
 A := CFixed8Dot24Half;
 for Index := 0 to $F do
  begin
   B := ConvertToFixed8Dot24(Index);
   C := FixedDiv(B, A);
   CheckEquals(Round(B.Fixed / A.Fixed * CFixed8Dot24One.Fixed), C.Fixed);
  end;

 Floats[0] := 0.1;
 Floats[1] := 0.7;
 for Index := 0 to $1F do
  begin
   A := ConvertToFixed8Dot24(Floats[0]);
   B := ConvertToFixed8Dot24(Floats[1]);
   C := FixedDiv(A, B);

   // advance
   Floats[2] := Floats[0] + 0.1;
   Floats[0] := Floats[0] / Floats[1];
   Floats[1] := -Floats[2];

   CheckEquals(Floats[0], ConvertFromFixed8Dot24(C), 1E-5);
  end;
end;

procedure TTestFixedPoint8Dot24.TestSqr;
var
  A, B  : TFixed8Dot24;
  Index : Integer;
begin
 inherited;
 for Index := 0 to $B do
  begin
   A := ConvertToFixed8Dot24(Index);
   B := FixedSqr(A);
   CheckEquals(Round((A.Fixed / $1000000) * A.Fixed), B.Fixed);
  end;
end;

procedure TTestFixedPoint8Dot24.TestFloor;
var
  Value  : TFixed8Dot24;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := -$7FFFFE to $7FFFFE do
  begin
   Value.Fixed := Index;
   Result := FixedFloor(Value);
   CheckEquals(Value.Int, Result);
  end;
end;

procedure TTestFixedPoint8Dot24.TestCeil;
var
  Value  : TFixed8Dot24;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := -$7FFFFE to $7FFFFE do
  begin
   Value.Fixed := Index;
   Result := FixedCeil(Value);
   CheckEquals(Ceil(Value.Fixed * CFixed8Dot24ToFloat), Result);
  end;
end;

procedure TTestFixedPoint8Dot24.TestConvert;
var
  Value    : TFixed8Dot24;
  V16Dot16 : TFixed16Dot16;
  V24Dot8  : TFixed24Dot8;
  Index    : Integer;
begin
 inherited;

 for Index := -$7FFFFF to $7FFFFF do
  begin
   Value.Fixed := Index shl 8;
   V16Dot16 := ConvertToFixed16Dot16(Value);
   CheckEquals(Value.AsSingle, V16Dot16.AsSingle);
   Value := ConvertFromFixed16Dot16(V16Dot16);
   CheckEquals(Value.AsSingle, V16Dot16.AsSingle);
  end;

 for Index := -$7FFF to $7FFF do
  begin
   Value.Fixed := Index shl 16;
   V24Dot8 := ConvertToFixed24Dot8(Value);
   CheckEquals(Value.AsSingle, V24Dot8.AsSingle);
   Value := ConvertFromFixed24Dot8(V24Dot8);
   CheckEquals(Value.AsSingle, V24Dot8.AsSingle);
  end;
end;

procedure TTestFixedPoint8Dot24.TestRoundHalfUp;
var
  Value    : TFixed8Dot24;
  Result   : Integer;
  IntIndex : ShortInt;
begin
 inherited;

 for IntIndex := -$7F to $7E do
   begin
    Value.Fixed := 0;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := $7FFFFF;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := $800000;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := $FFFFFF;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed8Dot24ToFloat), Result);
   end;
end;

procedure TTestFixedPoint8Dot24.TestRound;
var
  Value    : TFixed8Dot24;
  Result   : Integer;
  IntIndex : ShortInt;
begin
 inherited;

 for IntIndex := -$7F to $7E do
   begin
    Value.Fixed := 0;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := Random($FFFFFF);
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := $7FFFFF;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := $800000;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed8Dot24ToFloat), Result);

    Value.Fixed := $FFFFFF;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed8Dot24ToFloat), Result);
   end;
end;

procedure TTestFixedPoint8Dot24.TestReciprocal;
var
  A, B  : TFixed24Dot8;
  Index : Integer;
begin
 inherited;

 for Index := 1 to $7F do
  begin
   A := ConvertToFixed24Dot8(Index);
   B := FixedReciprocal(A);
   CheckEquals(Round(65536 / A.Fixed - 0.5), B.Fixed);
  end;
end;


{ TTestFixedPoint16Dot16 }

procedure TTestFixedPoint16Dot16.TestAdd;
var
  A, B, C : TFixed16Dot16;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed16Dot16(Index);
   C := FixedAdd(A, B);
   CheckEquals(A.Fixed + B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestSub;
var
  A, B, C : TFixed16Dot16;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed16Dot16(Index);
   C := FixedSub(A, B);
   CheckEquals(A.Fixed - B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestMul;
var
  A, B, C : TFixed16Dot16;
  Index   : Integer;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed16Dot16(Index);
   C := FixedMul(A, B);
   CheckEquals(Round(A.Fixed * CFixed16Dot16ToFloat * B.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestDiv;
var
  A, B, C : TFixed16Dot16;
  Index   : Integer;
  Floats  : array [0..2] of Single;
begin
 inherited;
 A := CFixed16Dot16Half;
 for Index := 0 to $FFF do
  begin
   B := ConvertToFixed16Dot16(Index);
   C := FixedDiv(B, A);
   CheckEquals(Round(B.Fixed / A.Fixed * CFixed16Dot16One.Fixed), C.Fixed);
  end;

 Floats[0] := 0.1;
 Floats[1] := 0.7;
 for Index := 0 to $1F do
  begin
   A := ConvertToFixed16Dot16(Floats[0]);
   B := ConvertToFixed16Dot16(Floats[1]);
   C := FixedDiv(A, B);

   // advance
   Floats[2] := Floats[0] + 0.1;
   Floats[0] := Floats[0] / Floats[1];
   Floats[1] := -Floats[2];

   CheckEquals(Floats[0], ConvertFromFixed16Dot16(C), 1E-3);
  end;
end;

procedure TTestFixedPoint16Dot16.TestSqr;
var
  A, B  : TFixed16Dot16;
  Index : Integer;
begin
 inherited;
 for Index := 0 to $B5 do
  begin
   A := ConvertToFixed16Dot16(Index);
   B := FixedSqr(A);
   CheckEquals(Round((A.Fixed / $10000) * A.Fixed), B.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestFloor;
var
  Value  : TFixed16Dot16;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := -$7FFFFE to $7FFFFE do
  begin
   Value.Fixed := Index;
   Result := FixedFloor(Value);
   CheckEquals(Value.Int, Result);
  end;
end;

procedure TTestFixedPoint16Dot16.TestCeil;
var
  Value  : TFixed16Dot16;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := -$7FFFFE to $7FFFFE do
  begin
   Value.Fixed := Index;
   Result := FixedCeil(Value);
   CheckEquals(Ceil(Value.Fixed * CFixed16Dot16ToFloat), Result);
  end;
end;

procedure TTestFixedPoint16Dot16.TestConvert;
var
  Value   : TFixed16Dot16;
  V8Dot24 : TFixed8Dot24;
  V24Dot8 : TFixed24Dot8;
  Index   : Integer;
begin
 inherited;

 for Index := -$7FFFFF to $7FFFFF do
  begin
   Value.Fixed := Index;
   V8Dot24 := ConvertToFixed8Dot24(Value);
   CheckEquals(Value.AsSingle, V8Dot24.AsSingle);
   Value := ConvertFromFixed8Dot24(V8Dot24);
   CheckEquals(Value.AsSingle, V8Dot24.AsSingle);
  end;

 for Index := -$7FFFFF to $7FFFFF do
  begin
   Value.Fixed := Index shl 8;
   V24Dot8 := ConvertToFixed24Dot8(Value);
   CheckEquals(Value.AsSingle, V24Dot8.AsSingle);
   Value := ConvertFromFixed24Dot8(V24Dot8);
   CheckEquals(Value.AsSingle, V24Dot8.AsSingle);
  end;
end;

procedure TTestFixedPoint16Dot16.TestRound;
var
  Value    : TFixed16Dot16;
  Result   : Integer;
  IntIndex : SmallInt;
begin
  inherited;

  for IntIndex := -$7FFF to $7FFE do
   begin
    Value.Fixed := 0;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Frac := Random($FFFF);
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Fixed := $7FFF;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Fixed := $8000;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Fixed := $FFFF;
    Value.Int := IntIndex;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed16Dot16ToFloat), Result);
   end;
end;

procedure TTestFixedPoint16Dot16.TestRoundHalfUp;
var
  Value    : TFixed16Dot16;
  Result   : Integer;
  IntIndex : SmallInt;
begin
 inherited;

 for IntIndex := -$7FFF to $7FFE do
   begin
    Value.Fixed := 0;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Fixed := $7FFF;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Fixed := $8000;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed16Dot16ToFloat), Result);

    Value.Fixed := $FFFF;
    Value.Int := IntIndex;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed16Dot16ToFloat), Result);
   end;
end;

procedure TTestFixedPoint16Dot16.TestReciprocal;
var
  A, B  : TFixed16Dot16;
  Index : Integer;
begin
 inherited;
 for Index := 1 to $7FFF do
  begin
   A := ConvertToFixed16Dot16(Index);
   B := FixedReciprocal(A);
   CheckEquals(Round(4294967296 / A.Fixed - 0.5), B.Fixed);
  end;
end;

procedure TTestFixedPoint16Dot16.TestSqrt;
var
  A, B  : TFixed16Dot16;
  Index : Integer;
const
  CFixed16Dot16OneAsSingle : Single = 65536;
begin
 inherited;

 for Index := 0 to $7FFF do
  begin
   A := ConvertToFixed16Dot16(Index);
   B := FixedSqrtLowResolution(A);
   CheckEquals(Round(Sqrt(A.Fixed * CFixed16Dot16OneAsSingle) - 0.5) and $FFFFFF00,
     B.Fixed and $FFFFFF00);
  end;

 for Index := 0 to $3FFF do
  begin
   A := ConvertToFixed16Dot16(Index);
   B := FixedSqrtHighResolution(A);
   CheckEquals(Round(Sqrt(A.Fixed * CFixed16Dot16OneAsSingle) - 0.5), B.Fixed);
  end;
end;


{ TTestFixedPoint24Dot8 }

procedure TTestFixedPoint24Dot8.TestAdd;
var
  A, B, C : TFixed24Dot8;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed24Dot8(Index);
   C := FixedAdd(A, B);
   CheckEquals(A.Fixed + B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestSub;
var
  A, B, C : TFixed24Dot8;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed24Dot8(Index);
   C := FixedSub(A, B);
   CheckEquals(A.Fixed - B.Fixed, C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestMul;
var
  A, B, C : TFixed24Dot8;
  Index   : Integer;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed24Dot8(Index);
   C := FixedMul(A, B);
   CheckEquals(Round(A.Fixed * CFixed24Dot8ToFloat * B.Fixed), C.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestDiv;
var
  A, B, C : TFixed24Dot8;
  Index   : Integer;
  Floats  : array [0..2] of Single;
begin
 inherited;
 A := CFixed24Dot8Half;
 for Index := 0 to $FFFF do
  begin
   B := ConvertToFixed24Dot8(Index);
   C := FixedDiv(B, A);
   CheckEquals(Round(B.Fixed / A.Fixed * CFixed24Dot8One.Fixed), C.Fixed);
  end;

 Floats[0] := 0.1;
 Floats[1] := 0.7;
 for Index := 0 to $1F do
  begin
   A := ConvertToFixed24Dot8(Floats[0]);
   B := ConvertToFixed24Dot8(Floats[1]);
   C := FixedDiv(A, B);

   // advance
   Floats[2] := Floats[0] + 0.1;
   Floats[0] := Floats[0] / Floats[1];
   Floats[1] := -Floats[2];

   CheckEquals(Floats[0], ConvertFromFixed24Dot8(C), 1E-1);
  end;
end;

procedure TTestFixedPoint24Dot8.TestSqr;
var
  A, B  : TFixed24Dot8;
  Index : Integer;
begin
 inherited;
 for Index := 0 to $FF do
  begin
   A := ConvertToFixed24Dot8(Index);
   B := FixedSqr(A);
   CheckEquals(Round((A.Fixed * CFixed24Dot8ToFloat) * A.Fixed), B.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestFloor;
var
  Value  : TFixed24Dot8;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := -$7FFFFFE to $7FFFFFE do
  begin
   Value.Fixed := Index;
   Result := FixedFloor(Value);
   CheckEquals(Floor(Value.Fixed * CFixed24Dot8ToFloat), Result);
  end;
end;

procedure TTestFixedPoint24Dot8.TestCeil;
var
  Value  : TFixed24Dot8;
  Result : Integer;
  Index  : Integer;
begin
 inherited;
 for Index := -$7FFFFFE to $7FFFFFE do
  begin
   Value.Fixed := Index;
   Result := FixedCeil(Value);
   CheckEquals(Ceil(Value.Fixed * CFixed24Dot8ToFloat), Result);
  end;
end;

procedure TTestFixedPoint24Dot8.TestConvert;
var
  Value    : TFixed24Dot8;
  V8Dot24  : TFixed8Dot24;
  V16Dot16 : TFixed16Dot16;
  Index    : Integer;
begin
 inherited;

 for Index := -$7FFF to $7FFF do
  begin
   Value.Fixed := Index;
   V8Dot24 := ConvertToFixed8Dot24(Value);
   CheckEquals(Value.AsSingle, V8Dot24.AsSingle);
   Value := ConvertFromFixed8Dot24(V8Dot24);
   CheckEquals(Value.AsSingle, V8Dot24.AsSingle);
  end;

 for Index := -$7FFFFE to $7FFFFE do
  begin
   Value.Fixed := Index;
   V16Dot16 := ConvertToFixed16Dot16(Value);
   CheckEquals(Value.AsSingle, V16Dot16.AsSingle);
   Value := ConvertFromFixed16Dot16(V16Dot16);
   CheckEquals(Value.AsSingle, V16Dot16.AsSingle);
  end;
end;

procedure TTestFixedPoint24Dot8.TestRound;
var
  Value    : TFixed24Dot8;
  Result   : Integer;
  IntIndex : Integer;
begin
 inherited;

 for IntIndex := -$7FFFFE to $7FFFFE do
   begin
    Value.Fixed := IntIndex shl 8;
    Value.Frac := 0;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := Random($FF);
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := $7F;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := $80;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := $FF;
    Result := FixedRound(Value);
    CheckEquals(Round(Value.Fixed * CFixed24Dot8ToFloat), Result);
   end;
end;

procedure TTestFixedPoint24Dot8.TestRoundHalfUp;
var
  Value    : TFixed24Dot8;
  Result   : Integer;
  IntIndex : Integer;
begin
 inherited;

 for IntIndex := -$7FFFFE to $7FFFFE do
   begin
    Value.Fixed := IntIndex shl 8;
    Value.Frac := 0;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := $7F;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := $80;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed24Dot8ToFloat), Result);

    Value.Frac := $FF;
    Result := FixedRoundHalfUp(Value);
    CheckEquals(RoundHalfUp(Value.Fixed * CFixed24Dot8ToFloat), Result);
   end;
end;

procedure TTestFixedPoint24Dot8.TestReciprocal;
var
  A, B  : TFixed24Dot8;
  Index : Integer;
begin
 inherited;
 for Index := 1 to $7F do
  begin
   A := ConvertToFixed24Dot8(Index);
   B := FixedReciprocal(A);
   CheckEquals(Round(65536 / A.Fixed - 0.5), B.Fixed);
  end;
end;

procedure TTestFixedPoint24Dot8.TestSqrt;
var
  A, B, C : TFixed24Dot8;
  Index   : Integer;
const
  CFixed24Dot8OneAsSingle : Single = 256;
begin
 inherited;

 for Index := 0 to $7FFF do
  begin
   A := ConvertToFixed24Dot8(Index);
   B := FixedSqrt(A);
   C.Fixed := Round(Sqrt(A.Fixed * CFixed24Dot8OneAsSingle) - 0.5);
   CheckEquals(C.Fixed, B.Fixed);
  end;
end;


initialization
  RegisterTest(TTestFixedPoint8Dot24.Suite);
  RegisterTest(TTestFixedPoint16Dot16.Suite);
  RegisterTest(TTestFixedPoint24Dot8.Suite);

end.
