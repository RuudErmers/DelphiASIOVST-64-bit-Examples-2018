unit LunchBoxInputFilter;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

type
  TInputFilter = class(TObject)
  private
    function GetFrequency: Double;
    function GetGain: Double;
    function GetQ: Double;
    function GetSampleRate: Double;
    procedure SetDownsamplePower(Value: Integer);
    procedure SetOrder(const Value: Integer);
  protected
    FFrequency        : Double;
    FGain, FQ         : Double;
    FSampleRate       : Double;
    FSRR              : Double;
    FSinW0, FW0       : Double;
    FGainSpeed        : Double;
    FRipple           : TDAV2DoubleArray;
    FDownsamplePow    : Integer;
    FDownsampleFactor : Integer;
    FOrder            : Integer;
    FAB               : array [0..127] of Double;
    FD64              : array [0.. 63] of Double;
    procedure SetW0; virtual;
    procedure SetFrequency(const Value: Double); virtual;
    procedure SetGain(const Value: Double); virtual;
    procedure SetQ(const Value: Double); virtual;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetRipple; virtual;

    property GainSpeed: Double read FGainSpeed;
    property SinW0: Double read FSinW0;
    property W0: Double read FW0;
  protected
    procedure CalcCoefficients; virtual; abstract;
  public
    constructor Create; virtual;

    function ProcessSample(const Value: Double): Double; virtual; abstract;
    procedure SetFilterValues(const Frequency, Gain, Q : Single); virtual;
    procedure ResetState; virtual; abstract;
    function Magnitude(Frequency: Single): Single; virtual;
    function MagnitudeLog10(Frequency: Single): Single; virtual;
    procedure Reset; virtual;

    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property SampleRateReciprocal: Double read FSRR;
    property Frequency: Double read GetFrequency write SetFrequency;
    property Gain: Double read GetGain write SetGain;
    property Bandwidth: Double read GetQ write SetQ;
    property DownsampleAmount: Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFactor: Integer read FDownsampleFactor;
    property Order: Integer read FOrder write SetOrder;
  end;

  TInputFilterLP = class(TInputFilter)
  public
    constructor Create; override;

    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const Value:Double):Double; override;
    function Magnitude(Frequency: Single): Single; override;
    function MagnitudeLog10(Frequency: Single): Single; override;
  end;

  TInputFilterHP = class(TInputFilter)
  public
    constructor Create; override;

    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const Value:Double):Double; override;
    function Magnitude(Frequency: Single): Single; override;
    function MagnitudeLog10(Frequency: Single): Single; override;
  end;

implementation

uses Math, SysUtils;

constructor TInputFilter.Create;
begin
 FDownsamplePow := 0;
 FDownsampleFactor := 1;
 FFrequency := 0;
 FGain := 0; FQ := 1;
 FOrder := 10;
 SampleRate := 44100;
end;

function TInputFilter.GetFrequency: Double;
begin
 Result := FFrequency;
end;

function TInputFilter.GetGain: Double;
begin
 Result := FGain;
end;

function TInputFilter.GetQ: Double;
begin
 Result := FQ;
end;

procedure TInputFilter.Reset;
begin
 FGain := 0;
 CalcCoefficients;
end;

procedure TInputFilter.SetSampleRate(const Value: Double);
begin
 if Value = 0 then Exit;
 if Value <> FSampleRate then
  begin
   FSampleRate := Value;
   FSRR := 1 / FSampleRate;
  end;
end;

function TInputFilter.GetSampleRate: Double;
begin
 Result := FSampleRate;
end;

procedure TInputFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFactor := Round(IntPower(2, FDownsamplePow));
   SetW0;
  end;
end;

procedure TInputFilter.SetW0;
begin
 FW0 := 2 * Pi * FSRR * (FFrequency * FDownsampleFactor);
 FSinW0 := Sin(FW0);
 if FW0 > 3.1 then FW0 := 3.1;
end;

procedure TInputFilter.SetGain(const Value: Double);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FGain := Value;
 FGainSpeed := Exp(FGain*ln10_0025);
end;

procedure TInputFilter.SetOrder(const Value: Integer);
begin
 FOrder := Value;
 CalcCoefficients;
end;

procedure TInputFilter.SetFrequency(const Value: Double);
begin
 if FFrequency <> Value then
  begin
   SetW0;
   SetRipple;
  end;
end;

procedure TInputFilter.SetQ(const Value: Double);
begin
 if Value <> FQ then
  begin
   FQ := Value;
   SetRipple;
  end;
end;

procedure TInputFilter.SetRipple;
var
  t : Double;
begin
 t := ArcSinh(10 * FQ) * 0.1;
 FRipple[1] := Sinh(t);
 FRipple[0] := Sqr(Cosh(t));
end;

procedure TInputFilter.SetFilterValues(const Frequency, Gain, Q: Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency := Frequency; FGain := Gain; FQ := Q;
 FGainSpeed := Exp((FGain*ln10_0025));
 SetW0;
 SetRipple;
end;

function TInputFilter.Magnitude(Frequency: Single): Single;
begin
 Result := 1;
end;

function TInputFilter.MagnitudeLog10(Frequency: Single): Single;
begin
 Result := 20 * Log10(Magnitude(Frequency));
end;


{ TInputFilterLP }

constructor TInputFilterLP.Create;
begin
 inherited Create;
 FGainSpeed := 1;
end;

procedure TInputFilterLP.CalcCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
begin
 K := tan(FW0 * 0.5); K2 := K*K;
 for i := (FOrder div 2)-1 downto 0 do
  begin
   t  := Cos( ((i*2+1)*Pi*0.05) );
   t1 := 1/(FRipple[0]-(t*t));
   t2 := K*t1*FRipple[1]*(2*t);
   t :=      1/(t2+K2+t1);
   FAB[4 * i    ] := K2 * t;
   FAB[4 * i + 1] := 2 * FAB[4 * i];
   FAB[4 * i + 2] := 2 * (t1 - K2) * t;
   FAB[4 * i + 3] :=     (t2 - K2 - t1) * t;
  end;
 FAB[0] := FAB[0]*FGainSpeed;
 FAB[1] := FAB[1]*FGainSpeed;
{$ELSE}
const
  CHalf : Double = 0.5;
asm
    FLD1
    FILD    [Self.FOrder]                    // FOrder, 1
    FADD    ST(0), ST(0)                     // 2 * FOrder, 1
    FDIVP                                    // 1 / 2 * FOrder

    FLD     [Self.FW0]                       // FW0, 0.5 * FOrder
    FMUL    CHalf                            // FW0 / 2, FOrder / 2
    FSINCOS                                  // sin(FW0/2), Cos(FW0/2), FOrder / 2
    FDIVP                                    // K = tan(FW0*0.5), FOrder / 2
    FLD     ST(0)                            // K, K, FOrder / 2
    FMUL    ST(0), ST(0)                     // K², K, FOrder / 2
    FXCH                                     // K, K², FOrder / 2

    MOV     ECX, [Self.FOrder]               // ECX = order
    SHR     ECX, 1                           // ECX = order div 2
@OrderLoop:
    MOV     EDX, ECX                         // EDX = IMUL
    IMUL    EDX, 2                           // EDX = 2*IMUL
    DEC     EDX                              // EDX = 2*IMUL+1
    MOV     [ESP-4], EDX                     // EDX to stack
    DEC     EDX                              // EDX = 2*IMUL
    SHL     EDX, 1                           // EDX = 4*IMUL
    FILD    [ESP-4].Integer                  // EDX in ST(0) = 2*IMUL+1, K, K², FOrder / 2
    FLDPI                                    // Pi, 2*IMUL+1, K, K², FOrder / 2
    FMULP                                    // Pi * (2*IMUL+1), K, K², FOrder / 2
    FMUL    ST(0), ST(3)                     // Pi * (2*IMUL+1)/(2*Order), K, K², FOrder / 2
    FCOS                                     // Cos((IMUL*2+1)*Pi/(2*Order)) = t, K, K², FOrder / 2
    FLD     ST(0)                            // t, t, K, K², FOrder / 2
    FMUL    ST(0), ST(0)                     // t²,t, K, K², FOrder / 2
    FLD     [Self.FRipple].Double            // FRipple[0], t²,t, K, K², FOrder / 2
    FSUBRP                                   // FRipple[0] - t²,t, K, K², FOrder / 2
    FLD1                                     // 1, FRipple[0] - t²,t, K, K², FOrder / 2
    FDIVRP                                   // 1 / (FRipple[0] - t²) = t1, t, K, K², FOrder / 2
    FXCH                                     // t, t1, K, K², FOrder / 2
    FADD    ST(0), ST(0)                     // 2*t, t1, K, K², FOrder / 2
    FMUL    ST(0), ST(1)                     // 2*t * t1, t1, K, K², FOrder / 2
    FMUL    [Self.FRipple + 8].Double        // FRipple[1]*2*t*t1, t1, K, K², FOrder / 2
    FMUL    ST(0), ST(2)                     // K*FRipple[1]*2*t*t1 = t2, t1, K, K², FOrder / 2
    FLD     ST(0)                            // t2, t2, t1, K, K², FOrder / 2
    FADD    ST(0), ST(2)                     // t1+t2, t2, t1, K, K², FOrder / 2
    FADD    ST(0), ST(4)                     // t1+t2+K², t2, t1, K, K², FOrder / 2
    FLD1                                     // 1, t1+t2+K², t2, t1, K, K², FOrder / 2
    FDIVRP                                   // (1/t1+t2+K²)=t, t2, t1, K, K², FOrder / 2
    FLD     ST(0)                            // t, t, t2, t1, K, K², FOrder / 2
    FMUL    ST(0), ST(5)                     // t*K²=fA[2*IMUL], t, t2, t1, K, K², FOrder / 2
    FST     [Self.FAB + 8 * EDX].Double      // store to fA[2*IMUL], FOrder / 2
    FADD    ST(0), ST(0)                     // 2*fA[2*IMUL], t, t2, t1, K, K², FOrder / 2
    FSTP    [Self.FAB + 8 * EDX + 8].Double  // store to fA[2*IMUL+1], FOrder / 2

    FLD     ST(2)                            // t1, t, t2, t1, K, K², FOrder / 2
    FSUB    ST(0), ST(5)                     // t1-K², t, t2, t1, K, K², FOrder / 2
    FADD    ST(0), ST(0)                     // 2*(t1-K²), t, t2, t1, K, K², FOrder / 2
    FMUL    ST(0), ST(1)                     // 2*(t1-K²)*t, t, t2, t1, K, K², FOrder / 2
    FSTP    [Self.FAB + 8 * EDX + 16].Double // store to fB[2*IMUL], FOrder / 2
    FXCH                                     // t2, t, t1, K, K², FOrder / 2
    FSUBRP  ST(2), ST(0)                     // t, t2-t1, K, K², FOrder / 2
    FXCH                                     // t2-t1, t, K, K², FOrder / 2
    FSUB    ST(0), ST(3)                     // t2-t1-K², t, K, K², FOrder / 2
    FMULP                                    // (t2-t1-K²) * t, K, K², FOrder / 2
    FSTP    [Self.FAB + 8 * EDX + 24].Double // store to fB[2*IMUL+1], FOrder / 2
    LOOP    @OrderLoop
    FSTP    ST(0)                            // K², FOrder / 2
    FSTP    ST(0)                            // FOrder / 2
    FSTP    ST(0)                            // stack free!

    FLD     [Self.FAB].Double                // load fA[0]
    FMUL    [Self.FGainSpeed].Double         // apply FGainSpeed
    FSTP    [Self.FAB].Double                // store fA[0]
    FLD     [Self.FAB + 8].Double            // load fA[1]
    FMUL    [Self.FGainSpeed].Double         // apply FGainSpeed
    FSTP    [Self.FAB + 8].Double            // store fA[1]
{$ENDIF}
end;

function TInputFilterLP.Magnitude(Frequency: Single): Single;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * Cos(Frequency * Pi * FSRR);
 a := Sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * FAB[4 * i] * FAB[4 * i] * a /
    (1 + Sqr(FAB[4 * i + 2]) + Sqr(FAB[4 * i + 3]) + 2 * FAB[4 * i + 3] +
    cw * ((FAB[4 * i + 2] - cw) * FAB[4 * i + 3] - FAB[4 * i + 2]));
 Result := Sqrt(Result);
end;

function TInputFilterLP.MagnitudeLog10(Frequency: Single): Single;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * Cos(Frequency * Pi * FSRR);
 a := Sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * FAB[4 * i] * FAB[4 * i] * a /
    (1 + Sqr(FAB[4 * i + 2]) + Sqr(FAB[4 * i + 3]) + 2 * FAB[4 * i + 3] +
    cw*((FAB[4 * i + 2] - cw) * FAB[4 * i + 3] - FAB[4 * i + 2]));
 Result := 10 * Log10(Result);
end;

function TInputFilterLP.ProcessSample(const Value: Double): Double;
{$IFDEF PUREPASCAL}
var
  y,x : Double;
  i   : Integer;
begin
 Result := Value;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result := FAB[4 * i] * x + FD64[2 * i];
   FD64[2 * i    ] := FAB[4 * i + 1] * x + FAB[4 * i + 2] * Result + FD64[2 * i + 1];
   FD64[2 * i + 1] := FAB[4 * i    ] * x + FAB[4 * i + 3] * Result;
  end;
{$ELSE}
asm
    MOV     ECX, [Self.FOrder]
    SHL     ECX, 1
    FLD     Value.Double
@FilterLoop:
    SUB     ECX, 4
    FLD     ST(0)
    FMUL    [Self.FAB + ECX * 8].Double
    FADD    [Self.FD64 + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [Self.FAB + ECX * 8 + 16].Double
    FADD    [Self.FD64 + ECX * 4 + 8].Double
    FLD     ST(3)
    FMUL    [Self.FAB + ECX * 8 + 8].Double
    FADDP
    FSTP    [Self.FD64 + ECX * 4].Double
    FMUL    [Self.FAB + ECX * 8 + 24].Double
    FXCH
    FXCH    ST(2)
    FMUL    [Self.FAB + ECX * 8].Double
    FADDP
    FSTP    [Self.FD64 + ECX * 4 + 8].Double
    JNZ     @FilterLoop
{$ENDIF}
end;

procedure TInputFilterLP.ResetState;
begin
 FillChar(FD64[0], 10 * SizeOf(Double), 0);
end;


{ TInputFilterHP }

constructor TInputFilterHP.Create;
begin
 inherited Create;
 FGainSpeed := 1;
end;

procedure TInputFilterHP.CalcCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
begin
 K := Tan(FW0 * 0.5); K2 := K * K;
 for i := (FOrder div 2) - 1 downto 0 do
  begin
   t  := Sqr(Sin(((i * 2 + 1) * Pi / (4 * FOrder))));
   t1 := 1 / (FRipple[0]+4*t-4*Sqr(t)-1);
   t2 := 2 * K * t1 * FRipple[1] * (1 - 2 * t);
   FAB[4 * i    ] := 1/(t2+1+t1*K2);
   FAB[4 * i + 1] := -2*FAB[4*i];
   FAB[4 * i + 2] := 2*(   1-t1*K2)*FAB[4*i];
   FAB[4 * i + 3] :=   (t2-1-t1*K2)*FAB[4*i];
  end;
 FAB[0] := FAB[0] * FGainSpeed;
 FAB[1] := FAB[1] * FGainSpeed;
{$ELSE}
const
  CHalf : Double = 0.5;
asm
    FLD1
    FILD    [Self.FOrder]                   // FOrder, 1
    FADD    ST(0),ST(0)                     // 2*FOrder, 1
    FADD    ST(0),ST(0)                     // 4*FOrder, 1
    FDIVP                                   // FOrder / 2

    FLD     [Self.FW0]                      // FW0, FOrder / 2
    FMUL    CHalf                           // FW0/2, FOrder / 2
    FSINCOS                                 // sin(FW0/2), Cos(FW0/2), FOrder / 2
    FDIVP                                   // K = tan(FW0*0.5), FOrder / 2
    FLD     ST(0)                           // K, K, FOrder / 2
    FMUL    ST(0), ST(0)                    // K², K, FOrder / 2
    FXCH                                    // K, K², FOrder / 2

    MOV     ECX, [Self.FOrder]              // ECX = order
@OrderLoop:
    MOV     EDX, ECX                        // EDX = 2*IMUL
    DEC     EDX                             // EDX = 2*IMUL+1
    MOV     [ESP - 4], EDX                  // EDX to stack
    DEC     EDX                             // EDX = 2*IMUL
    SHL     EDX, 1                          // EDX = 4*IMUL
    FILD    [ESP - 4].Integer               // EDX in ST(0) = 2*IMUL+1, K, K²
    FLDPI                                   // Pi, 2*IMUL+1, K, K², FOrder / 2
    FMULP                                   // Pi * (2*IMUL+1), K, K², FOrder / 2
    FMUL    ST(0), ST(3)                    // Pi * (2*IMUL+1) / (4*Order), K, K², FOrder / 2
    FSIN                                    // sin((IMUL*2+1)*Pi/(2*Order)) = t, K, K², FOrder / 2
    FMUL    ST(0), ST(0)                    // Sqr(sin((IMUL*2+1)*Pi*0.025)), K, K², FOrder / 2

    FLD     ST(0)                           // t, t, K, K², FOrder / 2
    FMUL    ST(0), ST(0)                    // t²,t, K, K², FOrder / 2
    FLD     ST(1)                           // t, t²,t, K, K², FOrder / 2
    FSUBRP                                  // t-t²,t, K, K², FOrder / 2
    FADD    ST(0), ST(0)                    // 2*(t-t²),t, K, K², FOrder / 2
    FADD    ST(0), ST(0)                    // 4*(t-t²),t, K, K², FOrder / 2
    FLD     [Self.FRipple].Double           // FRipple[0], 4*t²,t, K, K², FOrder / 2
    FADDP                                   // FRipple[0] - 4*t²,t, K, K², FOrder / 2
    FLD1                                    // 1, FRipple[0]+4*t-4*t²,t, K, K², FOrder / 2
    FSUB    ST(1), ST(0)                    // 1, FRipple[0]+4*t-4*t²-1,t, K, K², FOrder / 2
    FDIVRP                                  // 1 / (FRipple[0]+4*t-4*t²-1) = t1, t, K, K², FOrder / 2

    FXCH                                    // t, t1, K, K², FOrder / 2
    FADD    ST(0), ST(0)                    // 2*t, t1, K, K², FOrder / 2
    FLD1                                    // 1, 2*t, t1, K, K², FOrder / 2
    FSUBRP                                  // 1 - 2*t, t1, K, K², FOrder / 2
    FMUL    [Self.FRipple + 8].Double       // FRipple[1]*(1-2*t), t1, K, K², FOrder / 2
    FMUL    ST(0), ST(2)                    // K*FRipple[1]*(1-2*t), t1, K, K², FOrder / 2
    FMUL    ST(0), ST(1)                    // t1*K*FRipple[1]*(1-2*t), t1, K, K², FOrder / 2
    FADD    ST(0), ST(0)                    // t2=2*t1*K*FRipple[1]*(1-2*t), t1, K, K², FOrder / 2
    FLD     ST(1)                           // t1, t2, t1, K, K², FOrder / 2
    FMUL    ST(0), ST(4)                    // t1*K², t2, t1, K, K², FOrder / 2
    FLD1                                    // 1, t1*K², t2, t1, K, K², FOrder / 2
    FADDP                                   // 1+t1*K², t2, t1, K, K², FOrder / 2
    FADD    ST(0), ST(1)                    // 1+t1*K²+t2, t2, t1, K, K², FOrder / 2
    FLD1                                    // 1, 1+t1*K²+t2, t2, t1, K, K², FOrder / 2
    FDIVRP                                  // 1/(1+t1*K²+t2)=A[2*IMUL], t2, t1, K, K², FOrder / 2
    FST     [Self.FAB + 8 * EDX].Double     // store to fA[2*IMUL]

    FLD     ST(0)                           // A[2*IMUL], A[2*IMUL], t2, t1, K, K², FOrder / 2
    FADD    ST(0), ST(0)                    // 2*A[2*IMUL], A[2*IMUL], t2, t1, K, K², FOrder / 2
    FCHS                                    // -2*A[2*IMUL], A[2*IMUL], t2, t1, K, K², FOrder / 2
    FSTP    [Self.FAB + 8 * EDX + 8].Double // store to fA[2*IMUL+1]

    FLD     ST(2)                           // t1, A[2*IMUL], t2, t1, K, K², FOrder / 2
    FMUL    ST(0),ST(5)                     // t1*K², A[2*IMUL], t2, t1, K, K², FOrder / 2
    FLD1                                    // 1, t1*K², A[2*IMUL], t2, t1, K, K², FOrder / 2
    FSUBRP                                  // 1-t1*K², A[2*IMUL], t2, t1, K, K², FOrder / 2
    FADD    ST(0),ST(0)                     // 2*(1-t1*K²), A[2*IMUL], t2, t1, K, K², FOrder / 2
    FMUL    ST(0),ST(1)                     // 2*(1-t1*K²)*A[2*IMUL], A[2*IMUL], t2, t1, K, K², FOrder / 2
    FSTP    [Self.FAB+8*EDX+16].Double      // store to fB[2*IMUL]

    FXCH    ST(2)                           // t1, t2, A[2*IMUL], K, K², FOrder / 2
    FMUL    ST(0), ST(4)                    // t1*K², t2, A[2*IMUL], K, K², FOrder / 2
    FLD1                                    // 1, t1*K², t2, A[2*IMUL], K, K², FOrder / 2
    FADDP                                   // 1 + t1*K², t2, A[2*IMUL], K, K², FOrder / 2
    FSUBP                                   // t2 - (1 + t1*K²), A[2*IMUL], K, K², FOrder / 2
    FMULP                                   // (t2 - (1 + t1*K²)) * A[2*IMUL], K, K², FOrder / 2
    FSTP    [Self.FAB+8*EDX+24].Double      // store to fB[2*IMUL+1], FOrder / 2
    SUB     ECX, 2
    JNZ     @OrderLoop
    FSTP    ST(0)                           // K², FOrder / 2
    FSTP    ST(0)                           // FOrder / 2
    FSTP    ST(0)                           // stack free!

    FLD     [Self.FAB].Double               // load fA[0]
    FMUL    [Self.FGainSpeed].Double        // apply FGainSpeed
    FSTP    [Self.FAB].Double               // store fA[0]
    FLD     [Self.FAB+8].Double             // load fA[1]
    FMUL    [Self.FGainSpeed].Double        // apply FGainSpeed
    FSTP    [Self.FAB+8].Double             // store fA[1]
{$ENDIF}
end;

function TInputFilterHP.Magnitude(Frequency: Single): Single;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * Cos(Frequency * Pi * FSRR);
 a := Sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * FAB[4 * i] * FAB[4 * i] * a /
    (1 + Sqr(FAB[4*i+2])+Sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 Result := Sqrt(Result);
end;

function TInputFilterHP.MagnitudeLog10(Frequency: Single): Single;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2 * Cos(Frequency * Pi * FSRR);
 a := Sqr(cw - 2);
 Result := 1;

 for i := 0 to (FOrder div 2) - 1
  do Result := Result * FAB[4 * i] * FAB[4 * i] * a /
    (1 + Sqr(FAB[4*i+2])+Sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 Result := 10 * Log10(Result);
end;

function TInputFilterHP.ProcessSample(const Value: Double): Double;
{$IFDEF PUREPASCAL}
var
  y, x : Double;
  i    : Integer;
begin
 Result := Value;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x := Result;
   Result          := FAB[4 * i    ] * x + FD64[2 * i];
   FD64[2 * i    ] := FAB[4 * i + 1] * x + FAB[4 * i + 2] * Result + FD64[2 * i + 1];
   FD64[2 * i + 1] := FAB[4 * i    ] * x + FAB[4 * i + 3] * Result;
  end;
{$ELSE}
asm
    MOV     ECX, [Self.FOrder]
    SHL     ECX, 1
    FLD     Value.Double;
@FilterLoop:
    SUB     ECX, 4
    FLD     ST(0)
    FMUL    [Self.FAB + ECX * 8].Double
    FADD    [Self.FD64 + ECX * 4].Double
    FLD     ST(0)
    FLD     ST(0)
    FMUL    [Self.FAB + ECX * 8 + 16].Double
    FADD    [Self.FD64 + ECX * 4 + 8].Double
    FLD     ST(3)
    FMUL    [Self.FAB + ECX * 8 + 8].Double
    FADDP
    FSTP    [Self.FD64 + ECX * 4].Double
    FMUL    [Self.FAB + ECX * 8 + 24].Double
    FXCH
    FXCH    ST(2)
    FMUL    [Self.FAB + ECX * 8].Double
    FADDP
    FSTP    [Self.FD64 + ECX * 4 + 8].Double
    JNZ     @FilterLoop
{$ENDIF}
end;

procedure TInputFilterHP.ResetState;
begin
 FillChar(FD64[0], 10 * SizeOf(Double), 0);
end;

end.
