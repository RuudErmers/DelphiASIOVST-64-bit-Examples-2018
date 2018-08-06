unit DAV_DspWaveshaper;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2004-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TCustomChebyshevWaveshaper = class(TDspPersistent, IDspProcessor32,
    IDspProcessor64)
  private
    function GetCoefficients(Index: Integer): Double;
    procedure SetOrder(Value: Integer);
  protected
    FChebyshevCoeffs : PDAVDoubleFixedArray;
    FGains           : PDAVDoubleFixedArray;
    FOrder           : Integer;
    FGainCount       : Integer;
    FCoeffCount      : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AllocateChebyshevCoefficients; virtual;
    procedure AllocateGains; virtual;
    procedure OrderChanged; virtual;
    procedure RecalculateHarmonics; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); virtual; abstract;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); virtual; abstract;
    function ProcessSample32(Input: Single): Single; virtual; abstract;
    function ProcessSample64(Input: Double): Double; virtual; abstract;

    property Coefficients[Harmonic: Integer]: Double read GetCoefficients;

    property Order: Integer read FOrder write SetOrder;
  end;

  TChebyshevWaveshaper = class(TCustomChebyshevWaveshaper)
  private
    FBypassDC: Boolean;
    function GetGain(Harmonic: Integer): Double;
    function GetInverted(Harmonic: Integer): Boolean;
    function GetLevel(Harmonic: Integer): Double;
    procedure SetGain(Harmonic: Integer; const Value: Double);
    procedure SetLevel(Harmonic: Integer; const Value: Double);
    procedure SetInverted(Harmonic: Integer; const Value: Boolean);
    procedure SetBypassDC(const Value: Boolean);
  protected
    procedure RecalculateHarmonics; override;
    procedure OrderChanged; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;

    property Gain[Harmonic: Integer]: Double read GetGain write SetGain;
    property Level[Harmonic: Integer]: Double read GetLevel write SetLevel;
    property Inverted[Harmonic: Integer]: Boolean read GetInverted write SetInverted;

    property BypassDC: Boolean read FBypassDC write SetBypassDC;
  end;

  TSymmetricChebyshevWaveshaper = class(TCustomChebyshevWaveshaper)
  private
    function GetGain(Harmonic: Integer): Double;
    function GetInverted(Harmonic: Integer): Boolean;
    function GetLevel(Harmonic: Integer): Double;
    procedure SetGain(Harmonic: Integer; const Value: Double);
    procedure SetLevel(Harmonic: Integer; const Value: Double);
    procedure SetInverted(Harmonic: Integer; const Value: Boolean);
  protected
    procedure RecalculateHarmonics; override;
    procedure OrderChanged; override;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); override;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;

    property Gain[Harmonic: Integer]: Double read GetGain write SetGain;
    property Level[Harmonic: Integer]: Double read GetLevel write SetLevel;
    property Inverted[Harmonic: Integer]: Boolean read GetInverted write SetInverted;
  end;

  TChebyshevWaveshaperSquare = class(TChebyshevWaveshaper)
  protected
    procedure OrderChanged; override;
  end;

  TChebyshevWaveshaperSquareShape = class(TChebyshevWaveshaper)
  private
    FShape: Double;
    procedure SetShape(const Value: Double);
  protected
    procedure OrderChanged; override;
  published
    property Shape: Double read FShape write SetShape;
  end;

function Waveshaper1(Input, Parameter: Single): Single; overload;
function Waveshaper1(Input, Parameter: Double): Double; overload;
function Waveshaper2(Input, Parameter: Single): Single; overload;
function Waveshaper2(Input, Parameter: Double): Double; overload;
function Waveshaper3(Input, Parameter: Single): Single; overload;
function Waveshaper3(Input, Parameter: Double): Double; overload;
function Waveshaper4(Input, Parameter: Single): Single; overload;
function Waveshaper4(Input, Parameter: Double): Double; overload;
function Waveshaper5(Input, Parameter: Single): Single; overload;
function Waveshaper5(Input, Parameter: Double): Double; overload;
function Waveshaper6(Input: Single): Single; overload;
function Waveshaper6(Input: Double): Double; overload;
function Waveshaper7(Input, Parameter: Single): Single; overload;
function Waveshaper7(Input, Parameter: Double): Double; overload;
function Waveshaper8(Input, Parameter: Single): Single; overload;
function Waveshaper8(Input, Parameter: Double): Double; overload;
function Saturate(Input, Limit: Single): Single; overload;
function Saturate(Input, Limit: Double): Double; overload;
function Saturate2(Input, Limit: Single): Single; overload;
function Saturate2(Input, Limit: Double): Double; overload;
function SoftSat(Input, Parameter: Single): Single; overload;
function SoftSat(Input, Parameter: Double): Double; overload;

implementation

uses
  SysUtils, Math, DAV_Common, DAV_Math;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrOnlyEvenHarmonics = 'Only even harmonics are allowed!';

function Waveshaper1(Input, Parameter :Single): Single;
begin
 if Abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * DAV_Math.Tanh(( Input - Parameter) / (1 - Parameter))
     else Result := -(Parameter + (1 - Parameter) * DAV_Math.Tanh((-Input - Parameter) / (1 - Parameter)));
   end;
end;

function Waveshaper1(Input, Parameter :Double): Double;
begin
 if Abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * DAV_Math.Tanh(( Input - Parameter) / (1 - Parameter))
     else Result := -(Parameter + (1 - Parameter) * DAV_Math.Tanh((-Input - Parameter) / (1 - Parameter)));
   end;
end;

function Waveshaper2(Input, Parameter: Single): Single;
begin
 if Abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * Sigmoid( (Input - Parameter) / ((1 - Parameter) * 1.5))
     else Result := -(Parameter + (1 - Parameter) * Sigmoid((-Input - Parameter) / ((1 - Parameter) * 1.5)));
   end;
end;

function Waveshaper2(Input, Parameter: Double): Double;
begin
 if Abs(Input) < Parameter
  then Result := Input
  else
   begin
    if Input > 0
     then Result :=   Parameter + (1 - Parameter) * Sigmoid( (Input - Parameter) / ((1 - Parameter) * 1.5))
     else Result := -(Parameter + (1 - Parameter) * Sigmoid((-Input - Parameter) / ((1 - Parameter) * 1.5)));
   end;
end;

function Waveshaper3(Input, Parameter: Single): Single;
begin
 Result := Input * (Abs(Input) + Parameter) / (Input * Input + (Parameter - 1) * Abs(Input) + 1);
end;

function Waveshaper3(Input, Parameter: Double): Double;
begin
 Result := Input * (Abs(Input) + Parameter) / (Input * Input + (Parameter - 1) * Abs(Input) + 1);
end;

function Waveshaper4(Input, Parameter: Single): Single;
begin
 Result := sign(Input) * power(ArcTan(Power(Abs(Input), Parameter)), (1 / Parameter));
end;

function Waveshaper4(Input, Parameter: Double): Double;
begin
 Result := sign(Input) * power(arctan(power(Abs(Input), Parameter)), (1 / Parameter));
end;

function Waveshaper5(Input, Parameter: Single): Single;
begin
 Parameter := 2 * Parameter / (1 - Parameter);
 Result := (1 + Parameter) * Input / (1 + Parameter * Abs(Input));
end;

function Waveshaper5(Input, Parameter: Double): Double;
begin
 Parameter := 2 * Parameter / (1 - Parameter);
 Result := (1 + Parameter) * Input / (1 + Parameter * Abs(Input));
end;

function Waveshaper6(Input: Single): Single;
var
  Parameter, b : Single;
begin
 Input := Input * 0.686306;
 Parameter := 1 + Exp(sqrt(Abs(Input)) * -0.75);
 b := Exp(Input);
 Result := (b - Exp(-Input * Parameter)) * b / (b * b + 1);
end;

function Waveshaper6(Input: Double): Double;
var
  Parameter, b : Double;
begin
 Input := Input * 0.686306;
 Parameter := 1 + Exp(sqrt(Abs(Input)) * -0.75);
 b := Exp(Input);
 Result := (b - Exp(-Input * Parameter)) * b / (b * b + 1);
end;

function Waveshaper7(Input, Parameter: Single): Single;
begin
 Result := Sign(Input) * Exp(ln(Abs(Input)) * Parameter);
end;

function Waveshaper7(Input, Parameter: Double): Double;
begin
 Result := Sign(Input) * Exp(ln(Abs(Input)) * Parameter);
end;

function Waveshaper8(Input, Parameter: Single): Single;
begin
 Result := Sign(Input) * Exp(ln(Parameter) * Abs(Input));
end;

function Waveshaper8(Input, Parameter: Double): Double;
begin
 Result := Sign(Input) * Exp(ln(Parameter) * Abs(Input));
end;

function Saturate(Input, Limit: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := 0.5 * (Abs(Input + Limit) - Abs(Input - Limit));
{$ELSE}
{$IFDEF CPUx86_64}
const
  CMostSignificantBit : Cardinal = $80000000;
{$ELSE}
const
  CGrdDiv : Double = 0.5;
{$ENDIF}
asm
{$IFDEF CPUx86_64}
    MOVSS   XMM2, CMostSignificantBit // XMM2 = CMostSignificantBit
    MINSS   XMM0, XMM1                // XMM0 = Min(Input, Limit)
    XORSS   XMM1, XMM2                // XMM1 = -Limit
    MAXSS   XMM0, XMM1                // XMM0 = Max(XMM0, -Limit)
{$ELSE}
    FLD     Input.Single
    FADD    Limit
    FABS
    FLD     Input.Single
    FSUB    Limit
    FABS
    FSUBP
    FMUL    CGrdDiv;
{$ENDIF}
{$ENDIF}
end;

function Saturate(Input, Limit: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result := 0.5 * (Abs(Input + Limit) - Abs(Input - Limit));
{$ELSE}
{$IFDEF CPUx86_64}
const
  CMostSignificantBit : Cardinal = $80000000;
{$ELSE}
const
  CGrdDiv : Double = 0.5;
{$ENDIF}
asm
{$IFDEF CPUx86_64}
    MOVSS   XMM2, CMostSignificantBit // XMM2 = CMostSignificantBit
    MINSS   XMM0, XMM1                // XMM0 = Min(Input, Limit)
    XORSS   XMM1, XMM2                // XMM1 = -Limit
    MAXSS   XMM0, XMM1                // XMM0 = Max(XMM0, -Limit)
{$ELSE}
    FLD     Input.Double
    FADD    Limit.Double
    FABS
    FLD     Input.Double
    FSUB    Limit.Double
    FABS
    FSUBP
    FMUL    CGrdDiv;
{$ENDIF}
{$ENDIF}
end;

function Saturate2(Input, Limit: Single): Single;
begin
 if Input > Limit
  then Result := Limit
  else
   if Input < -Limit
    then Result := -Limit
    else Result := Input;
end;

function Saturate2(Input, Limit: Double): Double;
begin
 if Input > Limit
  then Result := Limit
  else
   if Input < -Limit
    then Result := -Limit
    else Result := Input;
end;

function SoftSat(Input, Parameter: Single): Single;
var
  b, c : Single;
begin
 b := Abs(Input);
 if b < Parameter then Result := Input else
 if b > 1 then Result := sign(Input) * (Parameter + 1) * 0.5 else
  begin
   c := ((Input - Parameter) / (1 - Parameter));
   Result := Parameter + (Input - Parameter) / (1 + c * c);
  end;
end;

function SoftSat(Input, Parameter: Double): Double;
var
  b, c : Double;
begin
 b := Abs(Input);
 if b < Parameter then Result := Input else
 if b > 1 then Result := sign(Input) * (Parameter + 1) * 0.5 else
  begin
   c := ((Input - Parameter) / (1 - Parameter));
   Result := Parameter + (Input - Parameter) / (1 + c * c);
  end;
end;


function ChebyshevPolynomial(Order, Power: Integer): Integer;
begin
 if (Power < 0) or (Order < Power) then Result := 0 else
  case Order of
    0 : if (Power = 0) then Result := 1 else Result := 0;
    1 : if (Power = 1) then Result := 1 else Result := 0;
    2 : case Power of
         0 : Result := -1;
         2 : Result :=  2;
         else Result := 0;
        end;
    3 : case Power of
         1 : Result := -3;
         3 : Result :=  4;
         else Result := 0;
        end;
    4 : case Power of
         0 : Result :=  1;
         2 : Result := -8;
         4 : Result :=  8;
         else Result := 0;
        end;
    5 : case Power of
         1 : Result :=   5;
         3 : Result := -20;
         5 : Result :=  16;
         else Result := 0;
        end;
    6 : case Power of
         0 : Result :=  -1;
         2 : Result :=  18;
         4 : Result := -48;
         6 : Result :=  32;
         else Result := 0;
        end;
    7 : case Power of
         1 : Result :=  -7;
         3 : Result :=  56;
         5 : Result := -112;
         7 : Result :=  64;
         else Result := 0;
        end;
    8 : case Power of
         0 : Result :=    1;
         2 : Result :=  -32;
         4 : Result :=  160;
         6 : Result := -256;
         8 : Result :=  128;
         else Result := 0;
        end;
   else Result := 2 * ChebyshevPolynomial(Order - 1, Power - 1) - ChebyshevPolynomial(Order - 2, Power);
  end;
end;


{ TCustomChebyshevWaveshaper }

constructor TCustomChebyshevWaveshaper.Create;
begin
 inherited;
end;

destructor TCustomChebyshevWaveshaper.Destroy;
begin
  FreeMem(FChebyshevCoeffs);
  FreeMem(FGains);
  inherited;
end;

procedure TCustomChebyshevWaveshaper.AllocateChebyshevCoefficients;
begin
  ReallocMem(FChebyshevCoeffs, FCoeffCount * SizeOf(Double));
end;

procedure TCustomChebyshevWaveshaper.AllocateGains;
begin
  ReallocMem(FGains, FGainCount * SizeOf(Double));
  FillChar(FGains^, FGainCount * SizeOf(Double), 0);
end;

procedure TCustomChebyshevWaveshaper.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChebyshevWaveshaper then
  with TCustomChebyshevWaveshaper(Dest) do
   begin
    Order            := Self.Order;
    FChebyshevCoeffs := Self.FChebyshevCoeffs;
    FGains           := Self.FGains;
   end
 else inherited;
end;

function TCustomChebyshevWaveshaper.GetCoefficients(Index: Integer): Double;
begin
 if (Index < 0) or (Index >= FCoeffCount)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else Result := FChebyshevCoeffs^[Index];
end;

procedure TCustomChebyshevWaveshaper.OrderChanged;
begin
 AllocateGains;
 AllocateChebyshevCoefficients;
 RecalculateHarmonics;
 Changed;
end;

procedure TCustomChebyshevWaveshaper.SetOrder(Value: Integer);
begin
 Value := EnsureRange(Value, 1, 24);
 if Value <> Order then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;


{ TChebyshevWaveshaper }

constructor TChebyshevWaveshaper.Create;
begin
 inherited;
 Order := 1;
 Gain[0] := 1;
end;

procedure TChebyshevWaveshaper.OrderChanged;
begin
  FCoeffCount := FOrder + 1;
  FGainCount := FOrder;
  inherited;
end;

procedure TChebyshevWaveshaper.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
  Term   : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   Temp := FChebyshevCoeffs^[Order];
   for Term := Order - 1 downto 0
    do Temp := Temp * Data[Sample] + FChebyshevCoeffs^[Term];
   Data[Sample] := Temp;
  end;
end;

procedure TChebyshevWaveshaper.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
  Term   : Integer;
  Temp   : Double;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   Temp := FChebyshevCoeffs^[Order];
   for Term := Order - 1 downto 0
    do Temp := Temp * Data[Sample] + FChebyshevCoeffs^[Term];
   Data[Sample] := Temp;
  end;
end;

function TChebyshevWaveshaper.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  Term : Integer;
begin
  Result := FChebyshevCoeffs^[FCoeffCount - 1];
  for Term := FCoeffCount - 2 downto 0
   do Result := Result * Input + FChebyshevCoeffs^[Term];
{$ELSE}
asm
    MOV     ECX, [Self.FCoeffCount]
    MOV     EDX, [Self.FChebyshevCoeffs]

    FLD     [EDX + ECX * 8].Double      // Result, Input
    SUB     ECX, 1
    JNG     @Done
    FLD     Input.Single                // Input, Result
    FXCH                                // Result, Input

@Start:
    FMUL    ST(0), ST(1)                // Result * Input, Input
    FADD    [EDX + ECX * 8].Double      // NewResult, Input
    SUB     ECX, 1
    JNS     @Start
    FSTP    ST(1)

@Done:
{$ENDIF}
end;

function TChebyshevWaveshaper.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  Term : Integer;
begin
  Result := FChebyshevCoeffs^[FCoeffCount - 1];
  for Term := FCoeffCount - 2 downto 0
   do Result := Result * Input + FChebyshevCoeffs^[Term];
{$ELSE}
asm
    MOV     ECX, [Self.FCoeffCount]
    MOV     EDX, [Self.FChebyshevCoeffs]

    FLD     [EDX + ECX * 8].Double      // Result, Input
    SUB     ECX, 1
    JNG     @Done
    FLD     Input.Double                // Input, Result
    FXCH                                // Result, Input

@Start:
    FMUL    ST(0), ST(1)                // Result * Input, Input
    FADD    [EDX + ECX * 8].Double      // NewResult, Input
    SUB     ECX, 1
    JNS     @Start
    FSTP    ST(1)

@Done:
{$ENDIF}
end;

procedure TChebyshevWaveshaper.RecalculateHarmonics;
var
  Input, Index : Integer;
begin
 for Index := 0 to Order do
  begin
   FChebyshevCoeffs^[Index] := FGains[0] * ChebyshevPolynomial(1, Index);
   for Input := 1 to Order - 1 do
    if FGains[Input] <> 0
     then FChebyshevCoeffs^[Index] := FChebyshevCoeffs^[Index] + FGains[Input] * ChebyshevPolynomial(1 + Input, Index);
  end;
 if FBypassDC
  then FChebyshevCoeffs^[0] := 0;
end;

function TChebyshevWaveshaper.GetGain(Harmonic: Integer): Double;
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Harmonic])
  else Result := FGains[Harmonic];
end;

function TChebyshevWaveshaper.GetInverted(Harmonic: Integer): Boolean;
begin
 Result := Gain[Harmonic] < 0;
end;

function TChebyshevWaveshaper.GetLevel(Harmonic: Integer): Double;
begin
 Result := Amp_to_dB(Abs(Gain[Harmonic]));
end;

procedure TChebyshevWaveshaper.SetBypassDC(const Value: Boolean);
begin
 if FBypassDC <> Value then
  begin
   FBypassDC := Value;
   RecalculateHarmonics;
  end;
end;

procedure TChebyshevWaveshaper.SetGain(Harmonic: Integer; const Value: Double);
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Harmonic])
  else
   begin
    FGains[Harmonic] := Value;
    RecalculateHarmonics;
   end;
end;

procedure TChebyshevWaveshaper.SetInverted(Harmonic: Integer;
  const Value: Boolean);
begin
 if Value
  then Gain[Harmonic] := -Abs(Gain[Harmonic])
  else Gain[Harmonic] :=  Abs(Gain[Harmonic]);
end;

procedure TChebyshevWaveshaper.SetLevel(Harmonic: Integer; const Value: Double);
begin
 if FGains[Harmonic] < 0
  then Gain[Harmonic] := -dB_to_Amp(Value)
  else Gain[Harmonic] :=  dB_to_Amp(Value);
end;


{ TSymmetricChebyshevWaveshaper }

constructor TSymmetricChebyshevWaveshaper.Create;
begin
 inherited;
 Order := 1;
 Gain[0] := 1;
end;

procedure TSymmetricChebyshevWaveshaper.OrderChanged;
begin
  FGainCount := (FOrder + 1) div 2;
  FCoeffCount := FGainCount;
  inherited;
end;

procedure TSymmetricChebyshevWaveshaper.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
  Term   : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   Temp := FChebyshevCoeffs^[FCoeffCount - 1];
   for Term := FCoeffCount - 2 downto 0
    do Temp := Temp * Sqr(Data[Sample]) + FChebyshevCoeffs^[Term];
   Data[Sample] := Temp;
  end;
end;

procedure TSymmetricChebyshevWaveshaper.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
  Term   : Integer;
  Temp   : Double;
begin
 for Sample := 0 to SampleCount - 1 do
  begin
   Temp := FChebyshevCoeffs^[FCoeffCount - 1];
   for Term := FCoeffCount - 2 downto 0
    do Temp := Temp * Sqr(Data[Sample]) + FChebyshevCoeffs^[Term];
   Data[Sample] := Temp;
  end;
end;

function TSymmetricChebyshevWaveshaper.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  Term : Integer;
begin
  Result := FChebyshevCoeffs^[FCoeffCount - 1];
  for Term := FCoeffCount - 2 downto 0
   do Result := Result * Sqr(Input) + FChebyshevCoeffs^[Term];
  Result := Input * Result;
{$ELSE}
asm
    MOV     ECX, [Self.FCoeffCount]
    MOV     EDX, [Self.FChebyshevCoeffs]

    FLD     Input.Single            // Input
    FLD     [EDX + ECX * 8].Double  // Result, Input
    SUB     ECX, 1
    JNG     @Done
    FLD     Input.Single            // Input, Result, Input
    FMUL    Input.Single            // Input^2, Result, Input
    FXCH                            // Result, Input^2, Input

@Start:
    FMUL    ST(0), ST(1)            // Result * Input, Input
    FADD    [EDX + ECX * 8].Double  // NewResult, Input
    SUB     ECX, 1
    JNS     @Start
    FSTP    ST(1)

@Done:
    FMULP                           // NewResult * Input
{$ENDIF}
end;

function TSymmetricChebyshevWaveshaper.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  Term : Integer;
begin
  Result := FChebyshevCoeffs^[FCoeffCount - 1];
  for Term := FCoeffCount - 2 downto 0
   do Result := Result * Sqr(Input) + FChebyshevCoeffs^[Term];
  Result := Input * Result;
{$ELSE}
asm
    MOV     ECX, [Self.FCoeffCount]
    MOV     EDX, [Self.FChebyshevCoeffs]

    FLD     Input.Double            // Input
    FLD     [EDX + ECX * 8].Double  // Result, Input
    SUB     ECX, 1
    JNG     @Done
    FLD     Input.Double            // Input, Result, Input
    FMUL    Input.Double            // Input^2, Result, Input
    FXCH                            // Result, Input^2, Input

@Start:
    FMUL    ST(0), ST(1)            // Result * Input, Input
    FADD    [EDX + ECX * 8].Double  // NewResult, Input
    SUB     ECX, 1
    JNS     @Start
    FSTP    ST(1)

@Done:
    FMULP                           // NewResult * Input
{$ENDIF}
end;

procedure TSymmetricChebyshevWaveshaper.RecalculateHarmonics;
var
  Input, Index : Integer;
  Value        : Double;
begin
 for Index := 0 to FCoeffCount - 1 do
  begin
   Value := FGains[0] * ChebyshevPolynomial(1, 2 * Index + 1);
   for Input := 1 to FCoeffCount - 1 do
    if FGains[Input] <> 0
     then Value := Value + FGains[Input] * ChebyshevPolynomial(2 * Input + 1, 2 * Index + 1);
   FChebyshevCoeffs^[Index] := Value;
  end;
end;

procedure TSymmetricChebyshevWaveshaper.SetGain(Harmonic: Integer;
  const Value: Double);
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Harmonic])
  else
 if Harmonic mod 2 <> 0
  then raise Exception.Create(RCStrOnlyEvenHarmonics)
  else
   begin
    FGains[Harmonic div 2] := Value;
    RecalculateHarmonics;
   end;
end;

procedure TSymmetricChebyshevWaveshaper.SetInverted(Harmonic: Integer;
  const Value: Boolean);
begin
 if Value
  then Gain[Harmonic] := -Abs(Gain[Harmonic])
  else Gain[Harmonic] :=  Abs(Gain[Harmonic]);
end;

procedure TSymmetricChebyshevWaveshaper.SetLevel(Harmonic: Integer;
  const Value: Double);
begin
 if FGains[Harmonic] < 0
  then Gain[Harmonic] := -dB_to_Amp(Value)
  else Gain[Harmonic] :=  dB_to_Amp(Value);
end;

function TSymmetricChebyshevWaveshaper.GetGain(Harmonic: Integer): Double;
begin
 if (Harmonic < 0) or (Harmonic >= Order)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Harmonic])
  else
 if Harmonic mod 2 <> 0
  then raise Exception.Create(RCStrOnlyEvenHarmonics)
  else Result := FGains[Harmonic div 2];
end;

function TSymmetricChebyshevWaveshaper.GetInverted(Harmonic: Integer): Boolean;
begin
 Result := Gain[Harmonic] < 0;
end;

function TSymmetricChebyshevWaveshaper.GetLevel(Harmonic: Integer): Double;
begin
 Result := Amp_to_dB(Abs(Gain[Harmonic]));
end;


{ TChebyshevWaveshaperSquare }

procedure TChebyshevWaveshaperSquare.OrderChanged;
var
  Index : Integer;
begin
 for Index := 0 to Order - 1 do
  case Index mod 4 of
   0: FGains[Index] := -1 / (Index + 1);
   2: FGains[Index] :=  1 / (Index + 1);
   else FGains[Index] := 0;
  end;
 inherited;
end;


{ TChebyshevWaveshaperSquareShape }

procedure TChebyshevWaveshaperSquareShape.OrderChanged;
var
  Index : Integer;
begin
 for Index := 0 to Order - 1 do
  case Index mod 4 of
   0: FGains[Index] := -1 / Power(Index + 1, FShape);
   2: FGains[Index] :=  1 / Power(Index + 1, FShape);
   else FGains[Index] := 0;
  end;
 inherited;
end;

procedure TChebyshevWaveshaperSquareShape.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   OrderChanged;
  end;
end;


initialization
  RegisterDspProcessor64(TChebyshevWaveshaper);

end.
