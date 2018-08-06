unit DAV_DspFDNReverb;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF CPUx86_64}
  {$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_VectorMath,
  DAV_DspVibrato, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspFeedbackDelayNetwork;

type
  TDampingFilter = class(TCustomGainFrequencyFilter)
  protected
    FCoeffs : array [0..1] of Double;
    FState  : Double;
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    function Imaginary(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    function ProcessSample64(Input: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    procedure CalculateCoefficients; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Single); override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
  end;

  TReverbGeometry = (rgSphere, rgBox1, rgBox2, rgDense1, rgDense2, rgDrum,
    rgHallway, rgRoom1, rgRoom2, rgSparse1, rgSparse2, rgMinor, rgMajor,
    rgHarmonic, rgOctaves, rgFifth, rgDetuned);

  TCustomDspFDNReverb = class(TDspSampleRatePersistent)
  private
    FDamping           : Double;
    FDryMix            : Double;
    FFeedbackRotation  : TDAV2DoubleArray;
    FFeedbackInversion : Double;
    FHalfLife          : Double;
    FInputAngle        : Double;
    FModulationDepth   : Double;
    FModulationSpread  : Double;
    FNonLinearGain     : Double;
    FOutputAngle       : Double;
    FWetMix            : Double;
    FHold              : Boolean;
    FModulationActive  : Boolean;
    FNonLinearActive   : Boolean;
    FGeometry          : TReverbGeometry;
    FBaseDelay         : Double;
    function GetFeedbackRotation(Index: Integer): Double;
    procedure SetDamping(const Value: Double);
    procedure SetDryMix(const Value: Double);
    procedure SetFeedbackInversion(const Value: Double);
    procedure SetFeedbackRotation(Index: Integer; const Value: Double);
    procedure SetGeometry(const Value: TReverbGeometry);
    procedure SetHalfLife(const Value: Double);
    procedure SetHold(const Value: Boolean);
    procedure SetInputAngle(const Value: Double);
    procedure SetModulationActive(const Value: Boolean);
    procedure SetModulationDepth(const Value: Double);
    procedure SetModulationSpread(const Value: Double);
    procedure SetNonLinearActive(const Value: Boolean);
    procedure SetNonLinearGain(const Value: Double);
    procedure SetOutputAngle(const Value: Double);
    procedure SetWetMix(const Value: Double);
    procedure SetBaseDelay(const Value: Double);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BaseDelayChanged; virtual; abstract;
    procedure DampingChanged; virtual; abstract;
    procedure DryMixChanged; virtual; abstract;
    procedure FeedbackInversionChanged; virtual; abstract;
    procedure GeometryChanged; virtual; abstract;
    procedure HalfLifeChanged; virtual; abstract;
    procedure HoldChanged; virtual; abstract;
    procedure InputAngleChanged; virtual; abstract;
    procedure ModulationActiveChanged; virtual; abstract;
    procedure ModulationDepthChanged; virtual; abstract;
    procedure ModulationSpreadChanged; virtual; abstract;
    procedure NonLinearActiveChanged; virtual; abstract;
    procedure NonLinearGainChanged; virtual; abstract;
    procedure OutputAngleChanged; virtual; abstract;
    procedure WetMixChanged; virtual; abstract;
  public
    constructor Create; override;
    property Geometry: TReverbGeometry read FGeometry write SetGeometry;
    property BaseDelay: Double read FBaseDelay write SetBaseDelay;
    property HalfLife: Double read FHalfLife write SetHalfLife;
    property Damping: Double read FDamping write SetDamping;

    property ModulationActive: Boolean read FModulationActive write SetModulationActive;
    property ModulationDepth: Double read FModulationDepth write SetModulationDepth;
    property ModulationSpread: Double read FModulationSpread write SetModulationSpread;
    property NonLinearActive: Boolean read FNonLinearActive write SetNonLinearActive;
    property NonLinearGain: Double read FNonLinearGain write SetNonLinearGain;
    property InputAngle: Double read FInputAngle write SetInputAngle;
    property OutputAngle: Double read FOutputAngle write SetOutputAngle;
    property FeedbackRotation[Index : Integer]: Double read GetFeedbackRotation write SetFeedbackRotation;
    property FeedbackInversion: Double read FFeedbackInversion write SetFeedbackInversion;
    property DryMix: Double read FDryMix write SetDryMix;
    property WetMix: Double read FWetMix write SetWetMix;
    property Hold: Boolean read FHold write SetHold;
  end;

  TDspFDNReverb32 = class(TCustomDspFDNReverb, IDspProcessor32)
  private
    FFeedbackDelayNetwork : TFeedbackZDelayNetwork32;
    FHalflifeVector       : TDAVVector32;
    FDampingFilter        : Array [0..3] of TDampingFilter;
    FVibrato              : Array [0..3] of TDspVibrato32;
    procedure ReverbTimesChanged;
    procedure ProcessFeedbackPath(var FeedbackVector: TDAVVector32);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DampingChanged; override;
    procedure HalfLifeChanged; override;
    procedure InputAngleChanged; override;
    procedure ModulationActiveChanged; override;
    procedure NonLinearActiveChanged; override;
    procedure NonLinearGainChanged; override;
    procedure OutputAngleChanged; override;
    procedure SampleRateChanged; override;
    procedure GeometryChanged; override;
    procedure BaseDelayChanged; override;
    procedure FeedbackInversionChanged; override;
    procedure DryMixChanged; override;
    procedure WetMixChanged; override;
    procedure HoldChanged; override;
    procedure ModulationDepthChanged; override;
    procedure ModulationSpreadChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
    procedure ProcessStereo(const InLeft, InRight: Single; out OutLeft, OutRight: Single);
  end;

implementation

uses
  Math, SysUtils, DAV_Math, DAV_Approximations, DAV_DspInterpolation;

{ TDampingFilter }

constructor TDampingFilter.Create;
begin
 inherited;
 CalculateCoefficients;
end;

function TDampingFilter.GetOrder: Cardinal;
begin
 result := 1;
end;

procedure TDampingFilter.Reset;
begin
 FState := 0;
end;

procedure TDampingFilter.ResetStates;
begin
 FState := 0;
end;

procedure TDampingFilter.ResetStatesInt64;
begin
 PInt64(@FState)^ := 0;
end;

procedure TDampingFilter.SetFilterValues(const AFrequency, AGain : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := Exp(FGain_dB * ln10_0025);
 CalculateW0;
end;

procedure TDampingFilter.SetOrder(const Value: Cardinal);
begin
 // read only!
end;

function TDampingFilter.Real(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, result, Temp);
end;

function TDampingFilter.Imaginary(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TDampingFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
(*
var
  cw, Divider  : Double;
  cmplx        : TComplex64;
  i            : Integer;
*)
begin
(*
 if fOrder = 0 then
  begin
   Real := 1;
   Imaginary := 1;
  end
 else
  begin
   cw := cos(2 * Frequency * pi * fSRR);
   Divider   := 1 / ( sqr(FCoeffs[3]) - 2 * FCoeffs[3] + sqr(FCoeffs[2]) + 1
                      + 2 * cw * (FCoeffs[2] * (FCoeffs[3] + 1) + 2 * cw * FCoeffs[3]));
   Real      := (FCoeffs[0] + FCoeffs[1] * FCoeffs[2] + FCoeffs[0] * FCoeffs[3]
                + cw * (FCoeffs[1] * (1 + FCoeffs[3]) + FCoeffs[2] * 2 * FCoeffs[0])
                + (2 * sqr(cw) - 1) * FCoeffs[0] * (FCoeffs[3] + 1)) * Divider;
   Imaginary := (FCoeffs[1] * (1 - FCoeffs[3])
                + 2 * cw * FCoeffs[0] * (1 - FCoeffs[3])) * sqrt(1 - sqr(cw)) * Divider;
   for i := 1 to (fOrder div 2) - 1 do
    begin
     Divider   := 1 / ( sqr(FCoeffs[4*i+3]) - 2 * FCoeffs[4*i+3] + sqr(FCoeffs[4*i+2]) + 1
                + 2 * cw * (FCoeffs[4*i+2] * (FCoeffs[4*i+3] + 1) + 2 * cw * FCoeffs[4*i+3]));
     cmplx.Re  := (FCoeffs[4*i+0] + FCoeffs[4*i+1] * FCoeffs[4*i+2] + FCoeffs[4*i+0] * FCoeffs[4*i+3]
                 + cw * (FCoeffs[4*i+1] * (1 + FCoeffs[4*i+3]) + FCoeffs[4*i+2] * 2 * FCoeffs[4*i+0])
                 + (2*sqr(cw)-1) * FCoeffs[4*i+0] * (FCoeffs[4*i+3] + 1)) * Divider;
     cmplx.Im := (FCoeffs[4*i+1] * (1 - FCoeffs[4*i+3])
                 + 2 * cw * (FCoeffs[4*i+0] - FCoeffs[4*i+0] * FCoeffs[4*i+3])) * sqrt(1 - sqr(cw)) * Divider;
     ComplexMultiplyInplace(Real, Imaginary, cmplx.Re, cmplx.Im);
    end;
  end;
*)
end;

procedure TDampingFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  cmplx : TComplex64;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Real := Cmplx.Re;
 Imaginary := Cmplx.Im;
end;

function TDampingFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * Log10(MagnitudeSquared(Frequency));
end;

function TDampingFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplex64;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TDampingFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TDampingFilter then
  with TDampingFilter(Dest) do
   begin
    inherited;
    FCoeffs := Self.FCoeffs;
    FState  := Self.FState;
   end
 else inherited;
end;

procedure TDampingFilter.CalculateCoefficients;
var
  K, t : Double;
begin
 K := Tan(FW0 * 0.5);
 t := 1 / (K + 1);
 FCoeffs[0] := K * t;
 FCoeffs[1] := (1 - K) * t;
end;

function TDampingFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * pi * fSRR);
 Result := Abs(1E-32 + sqr(FCoeffs[0]) * (cw + 2) / (1 + sqr(FCoeffs[1]) - cw * FCoeffs[1]));
end;

function TDampingFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 x      := FCoeffs[0] * Input;
 Result := x + FState;
 FState := x + FCoeffs[1] * Result;
{$ELSE}
asm
{$IFDEF CPUx86_64}
    MOVSD   XMM1, [EAX.FCoeffs].Double
    MULSD   XMM0, XMM1                       // XMM0 = FCoeffs[0] * Input
    MOVSD   XMM2, XMM0                       // XMM2 = XMM0 = x
    MOVSD   XMM1, [EAX.FState].Double        // XMM1 = FState
    ADDSD   XMM0, XMM1                       // XMM0 = x + FState
    MOVSD   XMM1, [EAX.FCoeffs + 8].Double
    MULSD   XMM1, XMM0                       // XMM1 = FCoeffs[1] * Result
    ADDSD   XMM1, XMM2                       // XMM1 = x + FCoeffs[1] * Result
    MOVSD   [EAX.FState].Double, XMM1        // XMM1 = FState
{$ELSE}
    FLD     Input.Double;
    FMUL    [EAX.FCoeffs].Double             // x
    FLD     ST(0)                            // x, x
    FADD    [EAX.FState].Double              // x + FState, x
    FLD     ST(0)                            // x + FState, x + FState, x
    FMUL    [EAX.FCoeffs + 8].Double
    FADDP   ST(2), ST(0)
    FXCH
    FSTP    [EAX.FState].Double
{$ENDIF}
{$ENDIF}
end;

procedure TDampingFilter.PopStates;
begin
 raise Exception.Create('Not supported');
end;

procedure TDampingFilter.PushStates;
begin
 raise Exception.Create('Not supported');
end;


{ TCustomDspFDNReverb }

constructor TCustomDspFDNReverb.Create;
begin
 inherited;
 FHalfLife         := 1;
 FDamping          := 0.25;
 FNonLinearActive  := False;
 FModulationActive := False;
 FNonLinearGain    := 1;
end;

procedure TCustomDspFDNReverb.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspFDNReverb then
  with TCustomDspFDNReverb(Dest) do
   begin
    inherited;
    FDamping           := Self.FDamping;
    FDryMix            := Self.FDryMix;
    FFeedbackRotation  := Self.FFeedbackRotation;
    FFeedbackInversion := Self.FFeedbackInversion;
    FHalfLife          := Self.FHalfLife;
    FInputAngle        := Self.FInputAngle;
    FModulationDepth   := Self.FModulationDepth;
    FModulationSpread  := Self.FModulationSpread;
    FNonLinearGain     := Self.FNonLinearGain;
    FOutputAngle       := Self.FOutputAngle;
    FWetMix            := Self.FWetMix;
    FHold              := Self.FHold;
    FModulationActive  := Self.FModulationActive;
    FNonLinearActive   := Self.FNonLinearActive;
    FGeometry          := Self.FGeometry;
    FBaseDelay         := Self.FBaseDelay;
   end
 else inherited;
end;

function TCustomDspFDNReverb.GetFeedbackRotation(Index: Integer): Double;
begin
 if (Index < 0) or (Index > 1)
  then raise Exception.CreateFmt('Index out of bounds (%d)', [Index])
  else result := FFeedbackRotation[Index];
end;

procedure TCustomDspFDNReverb.SetBaseDelay(const Value: Double);
begin
 if BaseDelay <> Value then
  begin
   FBaseDelay := Value;
   BaseDelayChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetDamping(const Value: Double);
begin
 if Damping <> Value then
  begin
   FDamping := Value;
   DampingChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetDryMix(const Value: Double);
begin
 if DryMix <> Value then
  begin
   FDryMix := Value;
   DryMixChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetFeedbackInversion(const Value: Double);
begin
 if FeedbackInversion <> Value then
  begin
   FFeedbackInversion := Value;
   FeedbackInversionChanged;
  end
end;

procedure TCustomDspFDNReverb.SetFeedbackRotation(Index: Integer;
  const Value: Double);
begin
 if (Index < 0) or (Index > 1)
  then raise Exception.CreateFmt('Index out of bounds (%d)', [Index])
  else FFeedbackRotation[Index] := Value;
end;

procedure TCustomDspFDNReverb.SetGeometry(const Value: TReverbGeometry);
begin
 if Geometry <> Value then
  begin
   FGeometry := Value;
   GeometryChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetHalfLife(const Value: Double);
begin
 if HalfLife <> Value then
  begin
   FHalfLife := Value;
   HalfLifeChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetHold(const Value: Boolean);
begin
 if Hold <> Value then
  begin
   FHold := Value;
   HoldChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetInputAngle(const Value: Double);
begin
 if InputAngle <> Value then
  begin
   FInputAngle := Value;
   InputAngleChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetModulationActive(
  const Value: Boolean);
begin
 if FModulationActive <> Value then
  begin
   FModulationActive := Value;
   ModulationActiveChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetModulationDepth(const Value: Double);
begin
 if ModulationDepth <> Value then
  begin
   FModulationDepth := Value;
   ModulationDepthChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetModulationSpread(const Value: Double);
begin
 if ModulationSpread <> Value then
  begin
   FModulationSpread := Value;
   ModulationSpreadChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetNonLinearActive(const Value: Boolean);
begin
 if FNonLinearActive <> Value then
  begin
   FNonLinearActive := Value;
   NonLinearActiveChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetNonLinearGain(const Value: Double);
begin
 if FNonLinearGain <> Value then
  begin
   FNonLinearGain := Value;
   NonLinearGainChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetOutputAngle(const Value: Double);
begin
 if FOutputAngle <> Value then
  begin
   FOutputAngle := Value;
   OutputAngleChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetWetMix(const Value: Double);
begin
 if WetMix <> Value then
  begin
   FWetMix := Value;
   WetMixChanged;
  end;
end;


{ TDspFDNReverb32 }

constructor TDspFDNReverb32.Create;
var
  n : Integer;
begin
 inherited;
 SampleRateChanged;
 FFeedbackDelayNetwork := TFeedbackZDelayNetwork32.Create;
 FFeedbackDelayNetwork.OnProcessFeedbackPath := ProcessFeedbackPath;
 FDamping := 0.25;
 for n := 0 to 3 do
  begin
   FDampingFilter[n] := TDampingFilter.Create;
   FVibrato[n]       := TDspVibrato32.Create;
//   FVibrato[n].Depth :=
  end;
 DampingChanged;
end;

destructor TDspFDNReverb32.Destroy;
var
  n : Integer;
begin
 for n := 0 to 3 do
  begin
   FreeAndNil(FDampingFilter[n]);
   FreeAndNil(FVibrato[n]);
  end;
 FreeAndNil(FFeedbackDelayNetwork);
 inherited;
end;

procedure TDspFDNReverb32.DryMixChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.FeedbackInversionChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.GeometryChanged;
begin
 ReverbTimesChanged;
end;

procedure TDspFDNReverb32.DampingChanged;
var
  n : Integer;
begin
 for n := 0 to 3
  do FDampingFilter[n].Frequency := (0.01 + 0.99 * FDamping) * 5000 * (4 - n);
end;

procedure TDspFDNReverb32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspFDNReverb then
  with TCustomDspFDNReverb(Dest) do
   begin
    inherited;
    FFeedbackDelayNetwork.Assign(Self.FFeedbackDelayNetwork);
    FHalflifeVector := Self.FHalflifeVector;
    FDampingFilter[0].Assign(Self.FDampingFilter[0]);
    FDampingFilter[1].Assign(Self.FDampingFilter[1]);
    FDampingFilter[2].Assign(Self.FDampingFilter[2]);
    FDampingFilter[3].Assign(Self.FDampingFilter[3]);
    FVibrato[0].Assign(Self.FVibrato[0]);
    FVibrato[1].Assign(Self.FVibrato[1]);
    FVibrato[2].Assign(Self.FVibrato[2]);
    FVibrato[3].Assign(Self.FVibrato[3]);
   end
 else inherited;
end;

procedure TDspFDNReverb32.BaseDelayChanged;
begin
 ReverbTimesChanged;
 HalfLifeChanged;
end;

procedure TDspFDNReverb32.HalfLifeChanged;
var
  HL : Double;
begin
 HL := Exp(-ln2 / (FHalfLife * (FBaseDelay * SampleRate)));
 FHalflifeVector[0] := HL;
 FHalflifeVector[1] := HL;
 FHalflifeVector[2] := HL;
 FHalflifeVector[3] := HL;
end;

procedure TDspFDNReverb32.HoldChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.InputAngleChanged;
var
  Cmplx : TComplex32;
begin
 GetSinCos(FInputAngle * Pi / 180, Cmplx.Im, Cmplx.Re);
 FFeedbackDelayNetwork.InputVector[0] := Cmplx.Re;
 FFeedbackDelayNetwork.InputVector[1] := Cmplx.Im;
 FFeedbackDelayNetwork.InputVector[2] := -Cmplx.Re;
 FFeedbackDelayNetwork.InputVector[3] := -Cmplx.Im;
end;

procedure TDspFDNReverb32.ModulationActiveChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.ModulationDepthChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.ModulationSpreadChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.NonLinearActiveChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.NonLinearGainChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.OutputAngleChanged;
var
  Cmplx : TComplex32;
begin
 GetSinCos(FOutputAngle * Pi / 180, Cmplx.Im, Cmplx.Re);
 FFeedbackDelayNetwork.OutputVector[0] := Cmplx.Re;
 FFeedbackDelayNetwork.OutputVector[1] := Cmplx.Im;
 FFeedbackDelayNetwork.OutputVector[2] := -Cmplx.Re;
 FFeedbackDelayNetwork.OutputVector[3] := -Cmplx.Im;
end;

procedure TDspFDNReverb32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TDspFDNReverb32.ProcessFeedbackPath(var FeedbackVector: TDAVVector32);
begin
 if FNonLinearActive then
  begin
   {$IFDEF FPC}
   FeedbackVector[0] := FastTanhOpt5Term(FNonLinearGain * FeedbackVector[0]);
   FeedbackVector[1] := FastTanhOpt5Term(FNonLinearGain * FeedbackVector[1]);
   FeedbackVector[2] := FastTanhOpt5Term(FNonLinearGain * FeedbackVector[2]);
   FeedbackVector[3] := FastTanhOpt5Term(FNonLinearGain * FeedbackVector[3]);
   {$ELSE}
   FeedbackVector[0] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[0]);
   FeedbackVector[1] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[1]);
   FeedbackVector[2] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[2]);
   FeedbackVector[3] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[3]);
   {$ENDIF}
  end;

 // Halflife
 ScaleVector(FeedbackVector, FHalflifeVector);

 FeedbackVector[0] := FDampingFilter[0].ProcessSample64(FeedbackVector[0]);
 FeedbackVector[1] := FDampingFilter[1].ProcessSample64(FeedbackVector[1]);
 FeedbackVector[2] := FDampingFilter[2].ProcessSample64(FeedbackVector[2]);
 FeedbackVector[3] := FDampingFilter[3].ProcessSample64(FeedbackVector[3]);

 if FModulationActive then
  begin
   FeedbackVector[0] := FVibrato[0].ProcessSample32(FeedbackVector[0]);
   FeedbackVector[1] := FVibrato[0].ProcessSample32(FeedbackVector[1]);
   FeedbackVector[2] := FVibrato[0].ProcessSample32(FeedbackVector[2]);
   FeedbackVector[3] := FVibrato[0].ProcessSample32(FeedbackVector[3]);
  end;
end;

function TDspFDNReverb32.ProcessSample32(Input: Single): Single;
begin
 Result := FDryMix * Input +
           FWetMix * FFeedbackDelayNetwork.ProcessSample32(Input);
end;

procedure TDspFDNReverb32.ProcessStereo(const InLeft, InRight: Single;
  out OutLeft, OutRight: Single);
begin
 FFeedbackDelayNetwork.ProcessStereo(InLeft,  InRight, OutLeft, OutRight);
 OutLeft  := InLeft  * FDryMix + OutLeft  * FWetMix;
 OutRight := InRight * FDryMix + OutRight * FWetMix;
end;

procedure TDspFDNReverb32.ReverbTimesChanged;
const
  CGeometryTimeScales: Array [0..16, 0..2] of Single = (
    (1, 1, 1), (1.04, 1.11, 1.16), (1.05, 1.14, 1.3), (1.22, 1.34, 1.72),
    (1.78, 2.44, 2.62), (1.15, 1.63, 2.41), (1.41, 2.22, 3.13),
    (1.67, 2.52, 3.11), (1.44, 2.59, 3.35), (2.24, 3.78, 4.67),
    (2.61, 3.78, 5.1), (4/3, 5/6, 2), (4/3, 8/5, 2), (4/3, 2, 4),
    (2, 4, 8), (3/2, 2, 3), (1.0001, 1.0030, 1.0031));
begin
 if round(FBaseDelay * SampleRate) <= 0 then exit;
 FFeedbackDelayNetwork.DelaySamples[0] := round(FBaseDelay * SampleRate);
 case FGeometry of
  rgSphere : with FFeedbackDelayNetwork do
              begin
               DelaySamples[1] := DelaySamples[0];
               DelaySamples[2] := DelaySamples[0];
               DelaySamples[3] := DelaySamples[0];
              end;
   else with FFeedbackDelayNetwork do
         begin
          DelaySamples[1] := round(CGeometryTimeScales[Integer(FGeometry), 0] * FBaseDelay * SampleRate);
          DelaySamples[2] := round(CGeometryTimeScales[Integer(FGeometry), 1] * FBaseDelay * SampleRate);
          DelaySamples[3] := round(CGeometryTimeScales[Integer(FGeometry), 2] * FBaseDelay * SampleRate);
         end;
 end;
end;

procedure TDspFDNReverb32.SampleRateChanged;
var
  n : Integer;
begin
 for n := 0 to 3 do
  begin
   if assigned(FDampingFilter[n])
    then FDampingFilter[n].SampleRate := SampleRate;
   if assigned(FVibrato[n])
    then FVibrato[n].SampleRate := SampleRate;
  end;
end;

procedure TDspFDNReverb32.WetMixChanged;
begin
 // nothing here yet
end;

initialization
  RegisterDspProcessor32(TDspFDNReverb32);
//  RegisterDspProcessor64(TDspFDNReverb64);

end.
