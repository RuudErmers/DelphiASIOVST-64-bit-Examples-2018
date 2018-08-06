unit DAV_BufferMathPascal;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types;

{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDSingleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1, Input2, Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure AddArrays(const Input1: TDAVSingleDynArray; const Input2: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TDAVSingleDynArray; const amount: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1: TDAVSingleDynArray; const Input2: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVSingleDynArray; const factor1, factor2: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2: TDAVSingleDynArray; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVSingleDynArray; const factor2: single; const summand, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVSingleDynArray; const summand: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVSingleDynArray; const factor2, summand: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVSingleDynArray; const summand2: single; const factor, Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVSingleDynArray; const factor: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVSingleDynArray; const summand2, factor: single; const Output: TDAVSingleDynArray; const dim2: integer); overload;

procedure GetPeaks(const Input: TDAVSingleDynArray; var Outputmin, Outputmax: Single; const dim2: integer); overload;
procedure GetSums(const Input: TDAVSingleDynArray; var Outputmin, Outputmax: Single; const dim2: integer); overload;

{TYPE: TDAVArrayOfSingleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const Input1, Input2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const Input1: TDAVArrayOfSingleDynArray; const Input2:single;
                    const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TDAVArrayOfSingleDynArray; const amount:single;
                    const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const Input1: TDAVArrayOfSingleDynArray; const Input2:single;
                    const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2: single;
                       const summand, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVArrayOfSingleDynArray; const summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2, summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2: single;
                       const factor, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVArrayOfSingleDynArray; const factor: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2, factor: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfSingleDynArray; const factor1, factor2: single; Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;


procedure ClearArrays(const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const Input, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

// when Output has no dimensions use this:
procedure CreateArrayCopy(const Input: TDAVArrayOfSingleDynArray; out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure GetPeaks(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer); overload;

{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

{TYPE: TDAVDoubleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1, Input2, Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure AddArrays(const Input1: TDAVDoubleDynArray; const Input2: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TDAVDoubleDynArray; const amount: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const Input1: TDAVDoubleDynArray; const Input2: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVDoubleDynArray; const factor1, factor2: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2: TDAVDoubleDynArray; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVDoubleDynArray; const factor2: Double; const summand, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVDoubleDynArray; const summand: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVDoubleDynArray; const factor2, summand: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVDoubleDynArray; const summand2: Double; const factor, Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVDoubleDynArray; const factor: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVDoubleDynArray; const summand2, factor: Double; const Output: TDAVDoubleDynArray; const dim2: integer); overload;

procedure GetPeaks(const Input: TDAVDoubleDynArray; var Outputmin, Outputmax: Double; const dim2: integer); overload;
procedure GetSums(const Input: TDAVDoubleDynArray; var Outputmin, Outputmax: Double; const dim2: integer); overload;


{TYPE: TDAVArrayOfDoubleDynArray}
procedure AddArrays(const Input1, Input2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const Input1, Input2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const Input1: TDAVArrayOfDoubleDynArray; const Input2:Double;
                    const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TDAVArrayOfDoubleDynArray; const amount:Double;
                    const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const Input1: TDAVArrayOfDoubleDynArray; const Input2:Double;
                    const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2: Double;
                       const summand, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TDAVArrayOfDoubleDynArray; const summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2, summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2: Double;
                       const factor, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TDAVArrayOfDoubleDynArray; const factor: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2, factor: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfDoubleDynArray; const factor1, factor2: Double; Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure ClearArrays(const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const Input, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

// when Output has no dimensions use this:
procedure CreateArrayCopy(const Input: TDAVArrayOfDoubleDynArray; out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure SetDimensions(var Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);

procedure GetPeaks(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer); overload;

implementation


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}
procedure AddArrays(const Input1, Input2, Output: TDAVSingleDynArray; const dim2: integer);
var
  j: integer;
begin
  for j := 0 to dim2 - 1 do
    Output[j] := Input1[j] + Input2[j];
end;

procedure SubArrays(const from, amount, Output: TDAVSingleDynArray; const dim2: integer);
var
  Input1 : TDAVSingleDynArray absolute from;
  Input2 : TDAVSingleDynArray absolute amount;
  outp   : TDAVSingleDynArray absolute Output;
  j      : integer;
begin
  for j := 0 to dim2 - 1 do
    outp[j] := Input1[j] - Input2[j];
end;

procedure MulArrays(const Input1, Input2,Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] * Input2[j];
end;

procedure AddArrays(const Input1: TDAVSingleDynArray; const Input2: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] + Input2;
end;

procedure SubArrays(const from:   TDAVSingleDynArray; const amount: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := from[j] - amount;
end;

procedure MulArrays(const Input1: TDAVSingleDynArray; const Input2: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] * Input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2[j] + summand[j];
end;

procedure MulAddArrays(const factor1: TDAVSingleDynArray; const factor2: single; const summand, Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2 + summand[j];
end;

procedure MulAddArrays(const factor1, factor2: TDAVSingleDynArray; const summand: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2[j] + summand;
end;

procedure MulAddArrays(const factor1: TDAVSingleDynArray; const factor2, summand: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2[j]) * factor[j];
end;

procedure AddMulArrays(const summand1: TDAVSingleDynArray; const summand2: single; const factor, Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2) * factor[j];
end;

procedure AddMulArrays(const summand1, summand2: TDAVSingleDynArray; const factor: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2[j]) * factor;
end;

procedure AddMulArrays(const summand1: TDAVSingleDynArray; const summand2, factor: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const Input1, Input2: TDAVSingleDynArray; const factor1, factor2: single; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j]*factor1 + Input2[j]*factor2;
end;



procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2: TDAVSingleDynArray; const Output: TDAVSingleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j]*envelope1[j] + Input2[j]*envelope2[j];
end;





procedure AddArrays(const Input1,Input2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(Input1[i], Input2[i], Output[i], dim2);
end;

procedure SubArrays(const from, amount, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var
  i : Integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount[i], Output[i], dim2);
end;

procedure MulArrays(const Input1, Input2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var
  i : Integer;
begin
  for i := 0 to dim1 - 1 do MulArrays(Input1[i], Input2[i], Output[i], dim2);
end;





procedure AddArrays(const Input1: TDAVArrayOfSingleDynArray; const Input2: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do AddArrays(Input1[i], Input2, Output[i], dim2);
end;


procedure SubArrays(const from: TDAVArrayOfSingleDynArray; const amount: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount, Output[i], dim2);
end;

procedure MulArrays(const Input1: TDAVArrayOfSingleDynArray; const Input2: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(Input1[i], Input2, Output[i], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2: single;
                       const summand, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TDAVArrayOfSingleDynArray; const summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand, Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfSingleDynArray; const factor2, summand: single;
                       const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand, Output[i], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2: single; const factor, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TDAVArrayOfSingleDynArray; const factor: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor, Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfSingleDynArray; const summand2, factor: single; const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor, Output[i], dim2);
end;



procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfSingleDynArray; const factor1, factor2: single; Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(Input1[i], Input2[i], factor1, factor2, Output[i], dim2);
end;




procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(Input1[i], Input2[i], envelope1[i], envelope2[i], Output[i], dim2);
end;




procedure ClearArrays(const Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(Output[i,0], dim2 * SizeOf(Single),0);
end;




procedure CopyArrays(const Input, Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(Input[i,0], Output[i,0], dim2 * SizeOf(Single));
end;


procedure CreateArrayCopy(const Input: TDAVArrayOfSingleDynArray; out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  CopyArrays(Input, Output, dim1, dim2);
end;

procedure CreateEmptyArray(out Output: TDAVArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  ClearArrays(Output, dim1, dim2);
end;








{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

procedure AddArrays(const Input1, Input2, Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] + Input2[j];
end;

procedure SubArrays(const from, amount, Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := from[j] - amount[j];
end;

procedure MulArrays(const Input1, Input2,Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] * Input2[j];
end;

procedure AddArrays(const Input1: TDAVDoubleDynArray; const Input2: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] + Input2;
end;

procedure SubArrays(const from:   TDAVDoubleDynArray; const amount: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := from[j] - amount;
end;

procedure MulArrays(const Input1: TDAVDoubleDynArray; const Input2: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j] * Input2;
end;


procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2[j] + summand[j];
end;

procedure MulAddArrays(const factor1: TDAVDoubleDynArray; const factor2: Double; const summand, Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2 + summand[j];
end;

procedure MulAddArrays(const factor1, factor2: TDAVDoubleDynArray; const summand: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2[j] + summand;
end;

procedure MulAddArrays(const factor1: TDAVDoubleDynArray; const factor2, summand: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := factor1[j] * factor2 + summand;
end;

procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2[j]) * factor[j];
end;

procedure AddMulArrays(const summand1: TDAVDoubleDynArray; const summand2: Double; const factor, Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2) * factor[j];
end;

procedure AddMulArrays(const summand1, summand2: TDAVDoubleDynArray; const factor: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2[j]) * factor;
end;

procedure AddMulArrays(const summand1: TDAVDoubleDynArray; const summand2, factor: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := (summand1[j] + summand2) * factor;
end;




procedure AddScaledArrays(const Input1, Input2: TDAVDoubleDynArray; const factor1, factor2: Double; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j]*factor1 + Input2[j]*factor2;
end;



procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2: TDAVDoubleDynArray; const Output: TDAVDoubleDynArray; const dim2: integer);
var j: integer;
begin
  for j:=0 to dim2-1 do
    Output[j] := Input1[j]*envelope1[j] + Input2[j]*envelope2[j];
end;





procedure AddArrays(const Input1,Input2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(Input1[i], Input2[i], Output[i], dim2);
end;

procedure SubArrays(const from, amount, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount[i], Output[i], dim2);
end;

procedure MulArrays(const Input1, Input2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(Input1[i], Input2[i], Output[i], dim2);
end;





procedure AddArrays(const Input1: TDAVArrayOfDoubleDynArray; const Input2: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddArrays(Input1[i], Input2, Output[i], dim2);
end;


procedure SubArrays(const from: TDAVArrayOfDoubleDynArray; const amount: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do SubArrays(from[i], amount, Output[i], dim2);
end;

procedure MulArrays(const Input1: TDAVArrayOfDoubleDynArray; const Input2: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulArrays(Input1[i], Input2, Output[i], dim2);
end;





procedure MulAddArrays(const factor1, factor2, summand, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2: Double;
                       const summand, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand[i], Output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TDAVArrayOfDoubleDynArray; const summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2[i], summand, Output[i], dim2);
end;

procedure MulAddArrays(const factor1: TDAVArrayOfDoubleDynArray; const factor2, summand: Double;
                       const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(factor1[i], factor2, summand, Output[i], dim2);
end;




procedure AddMulArrays(const summand1, summand2, factor, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2: Double; const factor, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor[i], Output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TDAVArrayOfDoubleDynArray; const factor: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2[i], factor, Output[i], dim2);
end;

procedure AddMulArrays(const summand1: TDAVArrayOfDoubleDynArray; const summand2, factor: Double; const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(summand1[i], summand2, factor, Output[i], dim2);
end;



procedure AddScaledArrays(const Input1, Input2: TDAVArrayOfDoubleDynArray; const factor1, factor2: Double; Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(Input1[i], Input2[i], factor1, factor2, Output[i], dim2);
end;




procedure AddModulatedArrays(const Input1, Input2, envelope1, envelope2, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(Input1[i], Input2[i], envelope1[i], envelope2[i], Output[i], dim2);
end;




procedure ClearArrays(const Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do Fillchar(Output[i,0], dim2 * SizeOf(Double),0);
end;




procedure CopyArrays(const Input, Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do move(Input[i,0], Output[i,0], dim2 * SizeOf(Double))
end;

procedure CreateArrayCopy(const Input: TDAVArrayOfDoubleDynArray; out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  CopyArrays(Input, Output, dim1, dim2);
end;

procedure CreateEmptyArray(out Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(Output, dim1, dim2);
  ClearArrays(Output, dim1, dim2);
end;   

procedure SetDimensions(var Output: TDAVArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  setlength(Output, dim1, dim2);
end;

procedure GetPeaks(const Input: TDAVSingleDynArray; var Outputmin, Outputmax: Single; const dim2: integer);
var j: integer;
begin
  Outputmin := Input[0];
  Outputmax := Input[0];
  for j:=1 to dim2-1 do
  begin
    if      Outputmin>Input[j] then Outputmin := Input[j]
    else if Outputmax<Input[j] then Outputmax := Input[j];
  end;
end;

procedure GetSums(const Input: TDAVSingleDynArray; var Outputmin, Outputmax: Single; const dim2: integer);
var j: integer;
begin
  Outputmin := 0;
  Outputmax := 0;
  for j:=1 to dim2-1 do
  begin
    if Input[j]<0 then Outputmin := Outputmin + Input[j]
    else               Outputmax := Outputmax + Input[j];
  end;
end;

procedure GetPeaks(const Input: TDAVDoubleDynArray; var Outputmin, Outputmax: Double; const dim2: integer);
var j: integer;
begin
  Outputmin := Input[0];
  Outputmax := Input[0];
  for j:=1 to dim2-1 do
  begin
    if      Outputmin>Input[j] then Outputmin := Input[j]
    else if Outputmax<Input[j] then Outputmax := Input[j];
  end;
end;

procedure GetSums(const Input: TDAVDoubleDynArray; var Outputmin, Outputmax: Double; const dim2: integer);
var j: integer;
begin
  Outputmin := 0;
  Outputmax := 0;
  for j:=1 to dim2-1 do
  begin
    if Input[j]<0 then Outputmin := Outputmin + Input[j]
    else               Outputmax := Outputmax + Input[j];
  end;
end;

procedure GetPeaks(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetPeaks(Input[i], Outputmin[i], Outputmax[i], dim2);
end;

procedure GetSums(const Input: TDAVArrayOfSingleDynArray; const Outputmin, Outputmax: TDAVSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i:=0 to dim1-1 do
    GetSums(Input[i], Outputmin[i], Outputmax[i], dim2);
end;

procedure GetPeaks(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i:=0 to dim1-1 do
    GetPeaks(Input[i], Outputmin[i], Outputmax[i], dim2);
end;

procedure GetSums(const Input: TDAVArrayOfDoubleDynArray; const Outputmin, Outputmax: TDAVDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do
    GetSums(Input[i], Outputmin[i], Outputmax[i], dim2);
end;


end.
