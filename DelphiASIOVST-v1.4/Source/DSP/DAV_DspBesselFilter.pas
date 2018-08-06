unit DAV_DspBesselFilter;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF CPUX64}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, DAV_Classes, DAV_Common, DAV_DspFilter;

type
  TCustomBesselFilter = class(TCustomOrderFilter)
  private
    procedure SetDownsamplePower(Value: Integer);
  protected
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FOrder          : Integer;
    FA              : array [0..63] of Double;
    FB              : array [0..63] of Double;
    FState          : array [0..127] of Double;
    FStateStack     : array of array [0..127] of Double;
    procedure CalculateW0; override;
    class function GetMaxOrder: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    procedure ResetStates; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure ResetStatesInt64; override;
    procedure Complex(const Frequency: Double; out Real: Double; out Imaginary: Double); override;
    procedure Complex(const Frequency: Double; out Real: Single; out Imaginary: Single); override;
    function Imaginary(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
  end;

  TBesselLowpassFilter = class(TCustomBesselFilter)
  public
    procedure CalculateCoefficients; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
  end;
  TBesselHighCut = TBesselLowpassFilter;

  TBesselHighpassFilter = class(TCustomBesselFilter)
  public
    procedure CalculateCoefficients; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
  end;
  TBesselLowCut = TBesselHighpassFilter;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, Dialogs, SysUtils, DAV_Complex, DAV_Math;

constructor TCustomBesselFilter.Create(const Order: Integer = 0);
begin
 inherited Create(Order);
 FDownsamplePow := 0;
 FDownsampleFak := 1;
 CalculateCoefficients;
end;

class function TCustomBesselFilter.GetMaxOrder: Cardinal;
begin
 Result := 64;
end;

procedure TCustomBesselFilter.Reset;
begin
 Gain := 0;
end;

procedure TCustomBesselFilter.ResetStates;
begin
 FillChar(FState[0], FOrder * SizeOf(Double), 0);
end;

procedure TCustomBesselFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TCustomBesselFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if FDownsamplePow <> Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := Round(IntPower(2, FDownsamplePow));
   CalculateW0;
  end;
end;

procedure TCustomBesselFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBesselFilter then
  with TCustomBesselFilter(Dest) do
   begin
    inherited;
    FDownsamplePow  := Self.FDownsamplePow;
    FDownsampleFak  := Self.FDownsampleFak;
    FOrder          := Self.FOrder;
    FA              := Self.FA;
    FB              := Self.FB;
    FState          := Self.FState;
    FStateStack     := Self.FStateStack;
   end
 else inherited;
end;

procedure TCustomBesselFilter.CalculateW0;
begin
 FW0 := 2 * Pi * fSRR * (FFrequency * FDownsampleFak);
 GetSinCos(FW0, FExpW0.Im, FExpW0.Re);
 if FW0 > 3.1 then FW0 := 3.1;
end;

procedure TCustomBesselFilter.SetFilterValues(const AFrequency, AGain : Single);
begin
 FFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := dB_to_Amp(FGain_dB);
 CalculateW0;
end;

function TCustomBesselFilter.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Result, Temp);
end;

function TCustomBesselFilter.Imaginary(const Frequency: Double): Double;
var Temp : Double;
begin
 Complex(Frequency, Temp, Result);
end;

procedure TCustomBesselFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
(*var cw, Divider  : Double;
    cmplx        : TComplex64;
    i            : Integer;*)
begin
(*
 if FOrder = 0 then
  begin
   Real := 1;
   Imaginary := 1;
  end
 else
  begin
   cw := cos(2 * Frequency * pi * fSRR);
   Divider   := 1 / ( sqr(fAB[3]) - 2 * fAB[3] + sqr(fAB[2]) + 1
                      + 2 * cw * (fAB[2] * (fAB[3] + 1) + 2 * cw * fAB[3]));
   Real      := (fAB[0] + fAB[1] * fAB[2] + fAB[0] * fAB[3]
                + cw * (fAB[1] * (1 + fAB[3]) + fAB[2] * 2 * fAB[0])
                + (2 * sqr(cw) - 1) * fAB[0] * (fAB[3] + 1)) * Divider;
   Imaginary := (fAB[1] * (1 - fAB[3])
                + 2 * cw * fAB[0] * (1 - fAB[3])) * sqrt(1 - sqr(cw)) * Divider;
   for i := 1 to (FOrder div 2) - 1 do
    begin
     Divider   := 1 / ( sqr(fAB[4*i+3]) - 2 * fAB[4*i+3] + sqr(fAB[4*i+2]) + 1
                + 2 * cw * (fAB[4*i+2] * (fAB[4*i+3] + 1) + 2 * cw * fAB[4*i+3]));
     cmplx.Re  := (fAB[4*i+0] + fAB[4*i+1] * fAB[4*i+2] + fAB[4*i+0] * fAB[4*i+3]
                 + cw * (fAB[4*i+1] * (1 + fAB[4*i+3]) + fAB[4*i+2] * 2 * fAB[4*i+0])
                 + (2*sqr(cw)-1) * fAB[4*i+0] * (fAB[4*i+3] + 1)) * Divider;
     cmplx.Im := (fAB[4*i+1] * (1 - fAB[4*i+3])
                 + 2 * cw * (fAB[4*i+0] - fAB[4*i+0] * fAB[4*i+3])) * sqrt(1 - sqr(cw)) * Divider;
{$IFNDEF FPC}
     ComplexMultiplyInplace(Real, Imaginary, cmplx.Re, cmplx.Im);
{$ENDIF}
    end;
  end;
*)
end;

procedure TCustomBesselFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  cmplx : TComplex64;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Real := cmplx.Re;
 Imaginary := cmplx.Im;
end;

function TCustomBesselFilter.MagnitudeSquared(const Frequency: Double): Double;
begin
 Result := 1;
end;

function TCustomBesselFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 20 * Log10(MagnitudeSquared(Frequency));
end;

function TCustomBesselFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplex64;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TCustomBesselFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0,0], FState[0], Length(FStateStack[0])*SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1,0],FStateStack[0,0], (Length(FStateStack)-1)*Length(FStateStack[0])*SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

procedure TCustomBesselFilter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0,0], FStateStack[1,0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0,0], Length(FStateStack[0]) * SizeOf(Double));
end;

{ TBesselFilterLP }

function CalculateReverseBesselPolynomial(Order : Integer;  X : Double): Double;
begin
 if Order = 0 then Result := 1 else
 if Order = 1 then Result := X + 1
  else Result := (2 * Order - 1) * CalculateReverseBesselPolynomial(Order - 1, X)
                        + sqr(X) * CalculateReverseBesselPolynomial(Order - 2, X);
end;

function CalculateReverseBesselPolynomial0(Order : Integer): Double;
begin
 if Order < 2 then Result := 1
  else Result := (2 * Order - 1) * CalculateReverseBesselPolynomial0(Order - 1);
end;

function CalculateBesselFactor(Order, Term : Integer): Double;
begin
 Result := Factorial(Integer(2 * Order - Term)) / (IntPower(2, Order - Term) * Factorial(Integer(Term)) * Factorial(Integer(Order - Term)));
end;

procedure TBesselLowpassFilter.CalculateCoefficients;
var
  K, t: Double;
begin
// ShowMessage(FloatToStr(CalculateReverseBesselPolynomial0(6)));

 K := tan(FW0 * 0.5);
 if Order = 4 then
  begin
   t     := 1 / ((((    K + 10) * K + 45) * K + 105)*K + 105);
   FB[0] :=   - ( ((4 * K + 20) * K * K       - 210)*K - 420)*t;
   FB[1] :=   - (  (6 * K * K       - 90) * K * K      + 630)*t;
   FB[2] :=   - ( ((4 * K - 20) * K * K       + 210)*K - 420)*t;
   FB[3] :=   - ((((    K - 10) * K + 45) * K - 105)*K + 105)*t;

   FA[0] := 105 * t * fGainFactor;
   FA[1] := - 4 * FA[0];
   FA[2] := 6 * FA[0];
   FA[3] := FA[1];
   FA[4] := FA[0];


(*
   t     := 1 / ((((105 * K + 105) * K + 45) * K + 10)*K + 1);
   FB[0] :=   - ( ((420 * K + 210) * K * K       - 20)*K - 4)*t;
   FB[1] :=   - (  (630 * K * K        - 90) * K * K     + 6)*t;
   FB[2] :=   - ( ((420 * K - 210) * K * K       + 20)*K - 4)*t;
   FB[3] :=   - ((((105 * K - 105) * K + 45) * K - 10)*K + 1)*t;

   FA[0] := 105 * t * fGainFactor;
   FA[1] := 4 * FA[0];
   FA[2] := 6 * FA[0];
   FA[3] := FA[1];
   FA[4] := FA[0];
*)
  end else
 if Order = 6 then
  begin
   t     := 1 / ((((((     K + 1 * 21) * K + 1 * 210)* K + 1 * 1260)*K + 1 * 4725) * K + 1 * 10395)*K +  1 * 10395);
   FB[0] :=   - ( (((( 6 * K + 4 * 21) * K + 2 * 210)* K * K           - 2 * 4725) * K - 4 * 10395)*K -  6 * 10395)*t;
   FB[1] :=   - ((((((15 * K + 5 * 21) * K - 1 * 210)* K - 3 * 1260)*K - 1 * 4725) * K + 5 * 10395)*K + 15 * 10395)*t;
   FB[2] :=   - (   ((20 * K * K           - 4 * 210)* K * K           + 4 * 4725) * K * K            - 20 * 10395)*t;
   FB[3] :=   - ((((((15 * K - 5 * 21) * K - 1 * 210)* K + 3 * 1260)*K - 1 * 4725) * K - 5 * 10395)*K + 15 * 10395)*t;
   FB[4] :=   - ( (((( 6 * K - 4 * 21) * K + 2 * 210)* K * K           - 2 * 4725) * K + 4 * 10395)*K -  6 * 10395)*t;
   FB[5] :=   - ((((((     K - 1 * 21) * K + 1 * 210)* K - 1 * 1260)*K + 1 * 4725) * K - 1 * 10395)*K +  1 * 10395)*t;

   FA[0] :=   10395 * t * fGainFactor;
   FA[1] := - 6 * FA[0];
   FA[2] :=  15 * FA[0];
   FA[3] := -20 * FA[0];
   FA[4] :=  15 * FA[0];
   FA[5] := - 6 * FA[0];
   FA[6] :=       FA[0];
  end;
end;

function TBesselLowpassFilter.MagnitudeSquared(const Frequency: Double): Double;
(*var
  i    : Integer;
  a,cw : Double;*)
begin
  Result:=1; // dummy
(*
 cw:=2*cos(2*Frequency*pi*fSRR); a:=sqr(cw+2);
 Result:=1;
 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (FOrder mod 2) = 1 then
  begin
   i:=((FOrder+1) div 2) - 1;
   Result:=Result*sqr(fAB[4*i])*(cw+2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result:=Abs(1E-32+Result);
*)
end;

function TBesselLowpassFilter.Phase(const Frequency: Double): Double;
(*var cw, sw, Nom, Den : Double;
    i : Integer;*)
begin
  Result := 0; // dummy
(*
 GetSinCos(2*Frequency*pi*fSRR,sw,cw);
 Nom := sw * fAB[0] * 2 * (fAB[3] -1) * (1 + cw);
 Den := fAB[0] * (2 * fAB[2] * (1 + cw) + cw * (2 * fAB[3] * (cw + 1) + 2 * (1 + cw)));
 for i := 1 to (FOrder div 2) - 1 do
  begin
   Nom := Nom * sw * fAB[4*i] * 2 * (fAB[4*i+3] - 1) * (cw + 1);
   Den := Den * fAB[4*i] * (2 * fAB[4*i+2] * (1 + cw) + cw * (2 * fAB[4*i+3] * (cw + 1) + 2 * (1 + cw)));
  end;
 Result := ArcTan2(Nom,Den);
*)
end;

function TBesselLowpassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 Result := FA[0] * Input + FState[0];
 for i := 1 to FOrder - 1
  do FState[i - 1] := FA[i] * Input + FB[i - 1] * Result + FState[i];
 FState[FOrder - 1] := FA[FOrder] * Input + FB[FOrder - 1] * Result;
end;
{$ELSE}
asm
    FLD     Input.Double
    FMUL    [Self.FA].Double
    FADD    [Self.FState].Double
    MOV     ECX, 1
    MOV     EDX, [Self.FOrder]

@StageLoop:
    FLD     ST(0)
    FMUL    [Self.FB + ECX * 8 - 8].Double
    FADD    [Self.FState + ECX * 8].Double
    FLD     Input.Double
    FMUL    [Self.FA + ECX * 8].Double
    FADDP
    FSTP    [Self.FState + ECX * 8 - 8].Double
    INC     ECX
    TEST    ECX, EDX
    JNP     @StageLoop

    FLD     ST(0)
    FMUL    [Self.FB + ECX * 8 - 8].Double
    FLD     Input.Double
    FMUL    [Self.FA + ECX * 8].Double
    FADDP
    FSTP    [Self.FState + ECX * 8 - 8].Double
end;
{$ENDIF}


{ TBesselFilterHP }

procedure TBesselHighpassFilter.CalculateCoefficients;
var
  K, t  : Double;
begin
 K := tan(FW0 * 0.5);
 if Order = 4 then
  begin
   t     := 1 / ((((105 * K + 105) * K + 45) * K + 10)*K + 1);
   FB[0] :=   - ( ((420 * K + 210) * K * K       - 20)*K - 4)*t;
   FB[1] :=   - (  (630 * K * K        - 90) * K * K     + 6)*t;
   FB[2] :=   - ( ((420 * K - 210) * K * K       + 20)*K - 4)*t;
   FB[3] :=   - ((((105 * K - 105) * K + 45) * K - 10)*K + 1)*t;

   FA[0] := 105 * t * fGainFactor;
   FA[1] := - 4 * FA[0];
   FA[2] :=  6 * FA[0];
   FA[3] := FA[1];
   FA[4] := FA[0];
  end else
 if Order = 6 then
  begin
   t     := 1 / ((((((   K +   21)*K +  210)*K + 1260)*K +  4725)*K + 10395)*K +  10395);
   FB[0] :=   - ( (((( 6*K +   84)*K +  420)*K*K         -  9450)*K - 41580)*K -  62370) * t;
   FB[1] :=   - ((((((15*K +  105)*K -  210)*K - 3780)*K -  4725)*K + 51975)*K + 155925) * t;
   FB[2] :=   - (   ((20*K*K         -  840)*K*K         + 18900)*K*K          - 207900) * t;
   FB[3] :=   - ((((((15*K -  105)*K -  210)*K + 3780)*K -  4725)*K - 51975)*K + 155925) * t;
   FB[4] :=   - ( (((( 6*K -   84)*K +  420)*K*K         -  9450)*K + 41580)*K -  62370) * t;
   FB[5] :=   - ((((((   K -   21)*K +  210)*K - 1260)*K +  4725)*K - 10395)*K +  10395) * t;
   FA[0] := 10395 * t * fGainFactor;
   FA[1] := - 6 * FA[0];
   FA[2] :=  15 * FA[0];
   FA[3] := -20 * FA[0];
   FA[4] :=  15 * FA[0];
   FA[5] := - 6 * FA[0];
   FA[6] :=       FA[0];
  end;
end;

function TBesselHighpassFilter.MagnitudeSquared(const Frequency: Double): Double;
(*var
  i    : Integer;
  a,cw : Double;*)
begin
  Result := 1; // dummy
(*
 cw := 2 * cos(2 * Frequency * pi * fSRR);
 a := sqr(cw - 2);
 Result := 1;
 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*sqr(fAB[4*i])*a/(1+sqr(fAB[4*i+2])+sqr(fAB[4*i+3])+2*fAB[4*i+3]+cw*((fAB[4*i+2]-cw)*fAB[4*i+3]-fAB[4*i+2]));
 if (FOrder mod 2) = 1 then
  begin
   i:=((FOrder+1) div 2) - 1;
   Result:=Result*sqr(fAB[4*i])*(cw-2)/(1+sqr(fAB[4*i+2])-cw*fAB[4*i+2]);
  end;
 Result:=Abs(1E-32+Result);
*)
end;

function TBesselHighpassFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var i : Integer;
begin
 Result    := FA[0] * Input + FState[0];
 for i := 1 to FOrder - 1
  do FState[i - 1] := FA[i] * Input + FB[i - 1] * Result + FState[i];
 FState[FOrder - 1] := FA[FOrder] * Input + FB[FOrder - 1] * Result;
end;
{$ELSE}
asm
    FLD Input.Double
    FMUL [Self.FA].Double
    FADD [Self.FState].Double
    MOV ECX, 1
    MOV EDX, [Self.FOrder]

@StageLoop:
    FLD ST(0)
    FMUL [Self.FB + ECX * 8 - 8].Double
    FADD [Self.FState + ECX * 8].Double
    FLD Input.Double
    FMUL [Self.FA + ECX * 8].Double
    FADDP
    FSTP [Self.FState + ECX * 8 - 8].Double
    INC ECX
    TEST ECX, EDX
    JNP @StageLoop

    FLD ST(0)
    FMUL [Self.FB + ECX * 8 - 8].Double
    FLD Input.Double
    FMUL [Self.FA + ECX * 8].Double
    FADDP
    FSTP [Self.FState + ECX * 8 - 8].Double
end;
{$ENDIF}

initialization
  RegisterDspProcessors32([TBesselLowpassFilter, TBesselHighpassFilter]);
  RegisterDspProcessors64([TBesselLowpassFilter, TBesselHighpassFilter]);

end.
