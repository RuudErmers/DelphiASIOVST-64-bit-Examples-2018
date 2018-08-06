unit DAV_DspDitherNoiseShaper;

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
//  The code is based on coefficients posted by cshei[AT]indiana.edu For      //
//  more information see  http://www.musicdsp.org/archive.php?classid=5#99    //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterBasics;

const
  {$A4}
  // simple error feedback
  CNoiseShaperCoefficientsEFB  : array [0..0] of Single = (1);

  // Simple 2nd order
  CNoiseShaperCoefficients2Sc  : array [0..1] of Single = (1.0, -0.5);

  // modified-E weighted
  CNoiseShaperCoefficients2MEc : array [0..1] of Single = (1.537, -0.8367);
  CNoiseShaperCoefficients3MEc : array [0..2] of Single = (1.652, -1.049,
    0.1382);
  CNoiseShaperCoefficients9MEc : array [0..8] of Single = (1.662, -1.263,
    0.4827,  -0.2913, 0.1268, -0.1124, 0.03252, -0.01265, -0.03524);

  // improved-E weighted
  CNoiseShaperCoefficients5IEc : array [0..4] of Single = (2.033, -2.165,
    1.959, -1.590, 0.6149);
  CNoiseShaperCoefficients9IEc : array [0..8] of Single = (2.847, -4.685,
    6.214, -7.184, 6.639, -5.032,  3.263, -1.632,  0.4191);

  // F-weighted
  CNoiseShaperCoefficients3Fc  : array [0..2] of Single = (1.623, -0.982,
    0.109);
  CNoiseShaperCoefficients9Fc  : array [0..8] of Single = (2.412, -3.370,
    3.937, -4.174, 3.353, -2.205,  1.281, -0.569,  0.0847);

  // Sony "super bit mapping"
  CNoiseShaperCoefficientsSBM  : array [0..11] of Single = (1.47933, -1.59032,
    1.64436, -1.36613, 9.26704E-1, -5.57931E-1,  2.6786E-1, -1.06726E-1,
    2.8516E-2, 1.23066E-3, -6.16555E-3, 3.067E-3);

  // reduced super bit mapping"
  CNoiseShaperCoefficientsSBMr : array [0..9] of Single = (1.47933, -1.59032,
    1.64436, -1.36613, 9.26704E-1, -5.57931E-1,  2.6786E-1, -1.06726E-1,
    2.8516E-2, 1.23066E-3);

  // Experimental
  CNoiseShaperCoefficientsEX  : array [0..8] of Single = (1.2194769820734,
    -1.77912468394129, 2.18256539389233, -2.33622087251503, 2.2010985277411,
    -1.81964871362306, 1.29830681491534, -0.767889385169331, 0.320990893363264);

  CNoiseShaperCoefficients14kSharp44100 : array [0..6] of Single = (
    1.62019206878484, -2.26551157411517, 2.50884415683988, -2.25007947643775,
    1.62160867255441, -0.899114621685913, 0.35350816625238);

  CNoiseShaperCoefficients15kSharpEx40000 : array [0..7] of Single = (
    0.919387305668676, -1.04843437730544, 1.04843048925451, -0.868972788711174,
    0.60853001063849, -0.3449209471469, 0.147484332561636, -0.0370652871194614);

  CNoiseShaperCoefficients15kSharpEx44100 : array [0..7] of Single = (
    2.13029284627951, -3.37834026386511, 4.18650513140503, -4.13744252026737,
    3.33572681086378, -2.10101859689547, 1.01512367881576, -0.286474308856534);

  CNoiseShaperCoefficients15kSharpEx48000 : array [0..7] of Single = (
    1.4247141061364, -1.5437678148854, 1.0967969510044, -0.32075758107035,
    -0.32074811729292, 0.525494723539046, -0.38058984415197, 0.14824460513256);

  CNoiseShaperCoefficients15kSharpEx64000 : array [0..7] of Single = (
    2.49725554745212, -3.23587161287721, 2.31844946822861, -0.54326047010533,
    -0.54325301319653, 0.543289788745007, -0.142132484905, -0.0202120370327948);

  CNoiseShaperCoefficients15kSharpEx88200 : array [0..7] of Single = (
    2.90080649054909, -3.31387104345925, 1.07060472618978, 1.07052060977363,
   -0.68687433463692, -0.686840576074174, 0.888606037761557, -0.298648374809776);

  CNoiseShaperCoefficients15kSharpEx96000 : array [0..7] of Single = (
    3.14014081409305, -3.76888037179035, 1.26107138314221, 1.26088059917107,
    -0.807698715053922, -0.80767075968406, 1.0101984930848, -0.322351688402064);

  CNoiseShaperCoefficients15kSharp44100 : array [0..7] of Single = (
    1.34860378444905, -1.80123976889643, 2.04804746376671, -1.93234174830592,
    1.59264693241396, -1.04979311664936, 0.599422666305319, -0.213194268754789);

  CNoiseShaperCoefficients16kSharp44100 : array [0..8] of Single = (
    1.07618924753262, -1.41232919229157, 1.61374140100329, -1.5996973679788,
    1.42711666927426, -1.09986023030973, 0.750589080482029, -0.418709259968069,
    0.185132272731155);

(*
  CNoiseShaperCoefficientsEX   : array [0..7] of Single = (-0.952727263532277,
    0.601561451347511,  0.109057946728173,   0.0260489037563895,
    0.126886385245994,  0.0164113078217921, -0.132348709265001,
    0.151840173811289);
*)

type
  TNoiseShaperType = (nsNone, nsEFB, ns2Sc, ns2MEc, ns3MEc, ns9MEc, ns5IEc,
    ns9IEc, ns3Fc, ns9Fc, nsSBM, nsSBMr, nsSharp14k7thOrder,
    nsSharp15k8thOrder, nsSharp16k9thOrder, nsExperimental);

  TDitherType = (dtNone, dtEqual, dtTriangular, dtGauss, dtFastGauss);  

  TCustomDitherNoiseShaper = class(TDspSampleRatePersistent)
  private
    FBitDepth   : Byte;
    FDitherType : TDitherType;
    FLimit      : Boolean;
    FLimits     : array [0..1] of Integer;
    procedure SetBitDepth(Value: Byte);
    procedure SetDitherType(const Value: TDitherType);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BitDepthChanged; virtual; abstract;
    procedure DitherTypeChanged; virtual;
    procedure Reset; virtual; abstract;
  public
    constructor Create; override;

    property BitDepth: Byte read FBitDepth write SetBitDepth default 16;
    property Limit: Boolean read FLimit write FLimit default True;
    property DitherType: TDitherType read FDitherType write SetDitherType default dtTriangular;
  end;

  TCustomDitherFIRNoiseShaper = class(TCustomDitherNoiseShaper)
  private
    FOrder      : Integer;
    FHistoryPos : Integer;
    procedure SetOrder(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure OrderChanged; virtual; abstract;
    procedure Reset; override;

    property Order: Integer read FOrder write SetOrder;
  public
    constructor Create; override;
  end;

  TCustomDitherPredefinedNoiseShaper = class(TCustomDitherFIRNoiseShaper)
  private
    FNoiseshaperType : TNoiseShaperType;
    procedure SetNoiseshaperType(Value: TNoiseShaperType);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure NoiseshaperTypeChanged; virtual;
  public
    constructor Create; override;

    property NoiseshaperType: TNoiseShaperType read FNoiseshaperType write SetNoiseshaperType default ns9Fc;
  end;

  TCustomDitherIIRNoiseShaper = class(TCustomDitherNoiseShaper)
  private
    FFilter : TCustomFilter;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Reset; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TDitherNoiseShaper32 = class(TCustomDitherPredefinedNoiseShaper)
  private
    FBitMul, FBitDiv : Single;
    FDitherAmplitude : Single;
    procedure ChooseNoiseshaper;
  protected
    FCoefficients : PDAVSingleFixedArray; // Coefficients
    FHistory      : PDAVSingleFixedArray; // Error History
    procedure AssignTo(Dest: TPersistent); override;
    procedure BitDepthChanged; override;
    procedure NoiseshaperTypeChanged; override;
    procedure OrderChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessInteger(Input: Single): Integer;
    function ProcessFloat(Input: Single): Single;
    procedure Reset; override;
  published
    property BitDepth;
    property DitherAmplitude: Single read FDitherAmplitude write FDitherAmplitude;
    property DitherType;
    property Limit;
    property NoiseshaperType;
  end;

  TDitherNoiseShaper64 = class(TCustomDitherPredefinedNoiseShaper)
  private
    FBitMul, FBitDiv : Double;
    FDitherAmplitude : Double;
    procedure ChooseNoiseshaper;
  protected
    FCoefficients : PDAVDoubleFixedArray; // Coefficients
    FHistory      : PDAVDoubleFixedArray; // Error History
    procedure AssignTo(Dest: TPersistent); override;
    procedure BitDepthChanged; override;
    procedure NoiseshaperTypeChanged; override;
    procedure OrderChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessInteger(Input: Double): Integer;
    function ProcessFloat(Input: Double): Double;
    procedure Reset; override;
  published
    property BitDepth;
    property DitherAmplitude: Double read FDitherAmplitude write FDitherAmplitude;
    property DitherType;
    property Limit;
    property NoiseshaperType;
  end;

  TDitherSharpNoiseShaper32 = class(TCustomDitherFIRNoiseShaper)
  private
    FBitMul, FBitDiv : Single;
    FDitherAmplitude : Single;
    procedure ChooseNoiseshaper;
  protected
    FCoefficients : PDAVSingleFixedArray; // Coefficients
    FHistory      : PDAVSingleFixedArray; // Error History
    procedure BitDepthChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    function ProcessInteger(Input: Single): Integer;
    function ProcessFloat(Input: Single): Single;
    procedure Reset; override;
  published
    property BitDepth;
    property DitherAmplitude: Single read FDitherAmplitude write FDitherAmplitude;
    property DitherType;
    property Limit;
    property Samplerate;  
  end;

  TDitherHighShelfNoiseShaper32 = class(TCustomDitherIIRNoiseShaper)
  private
    FBitMul, FBitDiv : Single;
    FDitherAmplitude : Single;
    FLastSample      : Single;
    FFrequency       : Single;
    procedure SetFrequency(const Value: Single);
  protected
    procedure BitDepthChanged; override;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;

    function ProcessInteger(Input: Single): Integer;
    function ProcessFloat(Input: Single): Single;
  published
    property DitherAmplitude: Single read FDitherAmplitude write FDitherAmplitude;
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate;
    property BitDepth;
    property DitherType;
    property Limit;
  end;

  TDitherNoiseshaper = TDitherNoiseshaper32;

implementation

uses
  Math, SysUtils, DAV_Common, DAV_Math, DAV_Approximations;

{ TCustomDitherNoiseShaper }

procedure TCustomDitherNoiseShaper.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDitherNoiseShaper then
  with TCustomDitherNoiseShaper(Dest) do
   begin
    FBitDepth   := Self.FBitDepth;
    FDitherType := Self.FDitherType;
    FLimit      := Self.FLimit;
    FLimits     := Self.FLimits;
   end
  else inherited;
end;

constructor TCustomDitherNoiseShaper.Create;
begin
 inherited;
 FBitDepth := 16;
 FDitherType := dtTriangular;

 Randomize;
 BitDepthChanged;
end;

procedure TCustomDitherNoiseShaper.SetBitDepth(Value: Byte);
begin
 if Value < 1  then Value := 1 else
 if Value > 32 then Value := 32;
 if FBitDepth <> Value then
  begin
   FBitDepth := Value;
   BitDepthChanged;
  end;
end;

procedure TCustomDitherNoiseShaper.SetDitherType(const Value: TDitherType);
begin
 if FDitherType <> Value then
  begin
   FDitherType := Value;
   DitherTypeChanged;
  end;
end;

procedure TCustomDitherNoiseShaper.DitherTypeChanged;
begin
 Reset;
 Changed;
end;


{ TCustomDitherFIRNoiseShaper }

procedure TCustomDitherFIRNoiseShaper.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDitherFIRNoiseShaper then
  with TCustomDitherFIRNoiseShaper(Dest) do
   begin
    inherited;
    FOrder      := Self.FOrder;
    OrderChanged;
    FHistoryPos := Self.FHistoryPos;
   end
 else inherited;
end;

constructor TCustomDitherFIRNoiseShaper.Create;
begin
 inherited;
 FHistoryPos := 0;
end;

procedure TCustomDitherFIRNoiseShaper.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TCustomDitherFIRNoiseShaper.Reset;
begin
 FHistoryPos := 0;
end;


{ TCustomDitherPredefinedNoiseShaper }

constructor TCustomDitherPredefinedNoiseShaper.Create;
begin
 inherited;
 FNoiseshaperType := ns9Fc;
 NoiseshaperTypeChanged;
end;

procedure TCustomDitherPredefinedNoiseShaper.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDitherPredefinedNoiseShaper then
  with TCustomDitherPredefinedNoiseShaper(Dest) do
   begin
    inherited;
    FNoiseshaperType := Self.FNoiseshaperType;
   end
 else inherited;
end;

procedure TCustomDitherPredefinedNoiseShaper.SetNoiseshaperType(
  Value: TNoiseShaperType);
begin
 if FNoiseshaperType <> Value then
  begin
   FNoiseshaperType := Value;
   NoiseshaperTypeChanged;
  end;
end;

procedure TCustomDitherPredefinedNoiseShaper.NoiseshaperTypeChanged;
begin
 Reset;
 Changed;
end;

{ TCustomDitherIIRNoiseShaper }

constructor TCustomDitherIIRNoiseShaper.Create;
begin
 inherited;
end;

destructor TCustomDitherIIRNoiseShaper.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomDitherIIRNoiseShaper.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDitherIIRNoiseShaper then
  with TCustomDitherIIRNoiseShaper(Dest) do
   begin
    inherited;
    FFilter.Assign(Self.FFilter);
   end
 else inherited;
end;

procedure TCustomDitherIIRNoiseShaper.Reset;
begin
 FFilter.ResetStates;
end;

procedure TCustomDitherIIRNoiseShaper.SampleRateChanged;
begin
 inherited;
 if assigned(FFilter)
  then FFilter.SampleRate := SampleRate;
end;

{ TDitherNoiseShaper32 }

constructor TDitherNoiseShaper32.Create;
begin
 inherited;
 FDitherAmplitude := 1;
 GetMem(FHistory, FOrder * SizeOf(Single));
 Reset;
end;

destructor TDitherNoiseShaper32.Destroy;
begin
 Dispose(FHistory);
 inherited;
end;

procedure TDitherNoiseShaper32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TDitherNoiseShaper32 then
  with TDitherNoiseShaper32(Dest) do
   begin
    inherited;
    FBitMul          := Self.FBitMul;
    FBitDiv          := Self.FBitDiv;
    FDitherAmplitude := Self.FDitherAmplitude;

    assert(FOrder = Self.FOrder);
    Move(Self.FCoefficients^, FCoefficients^, FOrder * SizeOf(Single));
    Move(Self.FHistory^, FHistory^, FOrder * SizeOf(Single));
   end else
 if Dest is TDitherNoiseShaper32 then
  with TDitherNoiseShaper32(Dest) do
   begin
    inherited;
    FBitMul          := Self.FBitMul;
    FBitDiv          := Self.FBitDiv;
    FDitherAmplitude := Self.FDitherAmplitude;

    assert(FOrder = Self.FOrder);
    for Sample := 0 to FOrder - 1 do
     begin
      FCoefficients^[Sample] := Self.FCoefficients^[Sample];
      FHistory^[Sample] := Self.FHistory^[Sample];
     end;
   end
 else inherited;
end;

procedure TDitherNoiseShaper32.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf32;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := Round(-FBitMul - CHalf32);
 FLimits[1] := Round( FBitMul - CHalf32);
 Changed;
end;

procedure TDitherNoiseShaper32.NoiseshaperTypeChanged;
begin
 ChooseNoiseshaper;
 inherited;
end;

procedure TDitherNoiseShaper32.OrderChanged;
begin
 ReallocMem(FCoefficients, FOrder * SizeOf(Single));
 ReallocMem(FHistory, FOrder * SizeOf(Single));
 Changed;
end;

procedure TDitherNoiseShaper32.ChooseNoiseshaper;
begin
 case FNoiseshaperType of
  nsNone :
   begin
    Order := 1;
    FCoefficients[0] := 0;
   end;
  nsEFB :
   begin
    Order := Length(CNoiseShaperCoefficientsEFB);
    Move(CNoiseShaperCoefficientsEFB[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns2Sc :
   begin
    Order := Length(CNoiseShaperCoefficients2Sc);
    Move(CNoiseShaperCoefficients2Sc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns2MEc :
   begin
    Order := Length(CNoiseShaperCoefficients2MEc);
    Move(CNoiseShaperCoefficients2MEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns3MEc :
   begin
    Order := Length(CNoiseShaperCoefficients3MEc);
    Move(CNoiseShaperCoefficients3MEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns9MEc :
   begin
    Order := Length(CNoiseShaperCoefficients9MEc);
    Move(CNoiseShaperCoefficients9MEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns5IEc :
   begin
    Order := Length(CNoiseShaperCoefficients5IEc);
    Move(CNoiseShaperCoefficients5IEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns9IEc :
   begin
    Order := Length(CNoiseShaperCoefficients9IEc);
    Move(CNoiseShaperCoefficients9IEc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns3Fc :
   begin
    Order := Length(CNoiseShaperCoefficients3Fc);
    Move(CNoiseShaperCoefficients3Fc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  ns9Fc :
   begin
    Order := Length(CNoiseShaperCoefficients9Fc);
    Move(CNoiseShaperCoefficients9Fc[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSBM :
   begin
    Order := Length(CNoiseShaperCoefficientsSBM);
    Move(CNoiseShaperCoefficientsSBM[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSBMr :
   begin
    Order := Length(CNoiseShaperCoefficientsSBMr);
    Move(CNoiseShaperCoefficientsSBMr[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSharp14k7thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients14kSharp44100);
    Move(CNoiseShaperCoefficients14kSharp44100[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSharp15k8thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients15kSharp44100);
    Move(CNoiseShaperCoefficients15kSharp44100[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsSharp16k9thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients16kSharp44100);
    Move(CNoiseShaperCoefficients16kSharp44100[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
  nsExperimental :
   begin
    Order := Length(CNoiseShaperCoefficientsEX);
    Move(CNoiseShaperCoefficientsEX[0], FCoefficients[0], FOrder * SizeOf(Single));
   end;
 end;
end;

function TDitherNoiseShaper32.ProcessFloat(Input: Single): Single;
{.$DEFINE RenderFIR}
{$IFDEF RenderFIR}
var
  Coef : Integer;
{$ENDIF}
begin
 {$IFDEF RenderFIR}
 // render FIR filter
 Result := Input;
 for Coef := 0 to FOrder - 1
  do Result := Result - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];
 FHistoryPos := (FHistoryPos + 1) mod FOrder;
 FHistory[FHistoryPos] := Input;
 {$ELSE}
 Result := (ProcessInteger(Input) + CHalf32) * FBitDiv;
 {$ENDIF}
end;

function TDitherNoiseShaper32.ProcessInteger(Input: Single): Integer;
var
  Coef : Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 for Coef := 0 to FOrder - 1
  do Input := Input - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];

 FHistoryPos := (FHistoryPos + 1) mod FOrder;

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := Round(Input - CHalf32);
       dtEqual : Result := Round(Input - CHalf32 + FDitherAmplitude * FastRandom);
  dtTriangular : Result := Round(Input - CHalf32 + FDitherAmplitude * (random - random));
       dtGauss : Result := Round(Input - CHalf32 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := Round(Input - CHalf32 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FHistory[FHistoryPos] := Result - Input;
end;

procedure TDitherNoiseShaper32.Reset;
begin
 inherited;
 FillChar(FHistory^[0], FOrder * SizeOf(Single), 0);
end;


{ TDitherNoiseShaper64 }

constructor TDitherNoiseShaper64.Create;
begin
 inherited;
 FDitherAmplitude := 1;
 GetMem(FHistory, FOrder * SizeOf(Double));
 Reset;
end;

destructor TDitherNoiseShaper64.Destroy;
begin
 Dispose(FHistory);
 inherited;
end;

procedure TDitherNoiseShaper64.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TDitherNoiseShaper32 then
  with TDitherNoiseShaper32(Dest) do
   begin
    inherited;
    FBitMul          := Self.FBitMul;
    FBitDiv          := Self.FBitDiv;
    FDitherAmplitude := Self.FDitherAmplitude;

    assert(FOrder = Self.FOrder);
    for Sample := 0 to FOrder - 1 do
     begin
      FCoefficients^[Sample] := Self.FCoefficients^[Sample];
      FHistory^[Sample] := Self.FHistory^[Sample];
     end;
   end else
 if Dest is TDitherNoiseShaper64 then
  with TDitherNoiseShaper64(Dest) do
   begin
    inherited;
    FBitMul          := Self.FBitMul;
    FBitDiv          := Self.FBitDiv;
    FDitherAmplitude := Self.FDitherAmplitude;

    assert(FOrder = Self.FOrder);
    Move(Self.FCoefficients^, FCoefficients^, FOrder * SizeOf(Single));
    Move(Self.FHistory^, FHistory^, FOrder * SizeOf(Single));
   end
 else inherited;
end;

procedure TDitherNoiseShaper64.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf64;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := Round(-FBitMul - CHalf64);
 FLimits[1] := Round( FBitMul - CHalf64);
 Changed;
end;

procedure TDitherNoiseShaper64.NoiseshaperTypeChanged;
begin
 ChooseNoiseshaper;
 inherited;
end;

procedure TDitherNoiseShaper64.ChooseNoiseshaper;
var
  Coef : Integer;
begin
 case FNoiseshaperType of
  nsEFB :
   begin
    Order := Length(CNoiseShaperCoefficientsEFB);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients2Sc[Coef];
   end;
  ns2Sc :
   begin
    Order := Length(CNoiseShaperCoefficients2Sc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients2Sc[Coef];
   end;
  ns2MEc :
   begin
    Order := Length(CNoiseShaperCoefficients2MEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients2MEc[Coef];
   end;
  ns3Fc  :
   begin
    Order := Length(CNoiseShaperCoefficients3Fc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients3Fc[Coef];
   end;
  ns3MEc :
   begin
    Order := Length(CNoiseShaperCoefficients3MEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients3MEc[Coef];
   end;
  ns5IEc :
   begin
    Order := Length(CNoiseShaperCoefficients5IEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients5IEc[Coef];
   end;
  ns9Fc  :
   begin
    Order := Length(CNoiseShaperCoefficients9Fc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients9Fc[Coef];
   end;
  ns9MEc :
   begin
    Order := Length(CNoiseShaperCoefficients9MEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients9MEc[Coef];
   end;
  ns9IEc :
   begin
    Order := Length(CNoiseShaperCoefficients9IEc);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients9IEc[Coef];
   end;
  nsSBM :
   begin
    Order := Length(CNoiseShaperCoefficientsSBM);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficientsSBM[Coef];
   end;
  nsSBMr :
   begin
    Order := Length(CNoiseShaperCoefficientsSBMr);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficientsSBMr[Coef];
   end;
  nsSharp14k7thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients14kSharp44100);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients14kSharp44100[Coef];
   end;
  nsSharp15k8thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients15kSharp44100);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients15kSharp44100[Coef];
   end;
  nsSharp16k9thOrder :
   begin
    Order := Length(CNoiseShaperCoefficients16kSharp44100);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficients16kSharp44100[Coef];
   end;
  nsExperimental :
   begin
    Order := Length(CNoiseShaperCoefficientsEX);
    for Coef := 0 to Order - 1
     do FCoefficients[Coef] := CNoiseShaperCoefficientsEX[Coef];
   end;
 end;
end;

procedure TDitherNoiseShaper64.OrderChanged;
begin
 ReallocMem(FCoefficients, FOrder * SizeOf(Double));
 ReallocMem(FHistory, FOrder * SizeOf(Double));
 Changed;
end;

function TDitherNoiseShaper64.ProcessFloat(Input: Double): Double;
begin
 result := ProcessInteger(Input) * FBitDiv;
end;

function TDitherNoiseShaper64.ProcessInteger(Input: Double): Integer;
var
  Coef : Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 for Coef := 0 to FOrder - 1
  do Input := Input - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];

 FHistoryPos := (FHistoryPos + 1) mod FOrder;

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := Round(Input - CHalf64);
       dtEqual : Result := Round(Input - CHalf64 + FDitherAmplitude * (2 * random - 1));
  dtTriangular : Result := Round(Input - CHalf64 + FDitherAmplitude * (random - random));
       dtGauss : Result := Round(Input - CHalf64 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := Round(Input - CHalf64 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FHistory[FHistoryPos] := Result - Input;
end;

procedure TDitherNoiseShaper64.Reset;
begin
 inherited;
 FillChar(FHistory^[0], FOrder * SizeOf(Double), 0);
end;

{ TDitherSharpNoiseShaper32 }

constructor TDitherSharpNoiseShaper32.Create;
begin
 inherited;
 FOrder := 8;
 OrderChanged;
end;

procedure TDitherSharpNoiseShaper32.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf32;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := Round(-FBitMul - CHalf32);
 FLimits[1] := Round( FBitMul - CHalf32);
 Changed;
end;

procedure TDitherSharpNoiseShaper32.ChooseNoiseshaper;
begin
 if SampleRate < 41000
  then Move(CNoiseShaperCoefficients15kSharpEx40000[0], FCoefficients[0], FOrder * SizeOf(Single)) else
 if SampleRate < 46000
  then Move(CNoiseShaperCoefficients15kSharpEx44100[0], FCoefficients[0], FOrder * SizeOf(Single)) else
 if SampleRate < 55000
  then Move(CNoiseShaperCoefficients15kSharpEx48000[0], FCoefficients[0], FOrder * SizeOf(Single)) else
 if SampleRate < 75100
  then Move(CNoiseShaperCoefficients15kSharpEx64000[0], FCoefficients[0], FOrder * SizeOf(Single))
  else Move(CNoiseShaperCoefficients15kSharpEx96000[0], FCoefficients[0], FOrder * SizeOf(Single));
end;

procedure TDitherSharpNoiseShaper32.OrderChanged;
begin
 ReallocMem(FCoefficients, FOrder * SizeOf(Single));
 ReallocMem(FHistory, FOrder * SizeOf(Single));
 Changed;
end;

function TDitherSharpNoiseShaper32.ProcessFloat(Input: Single): Single;
{$IFDEF RenderFIR}
var
  Coef : Integer;
{$ENDIF}
begin
 {$IFDEF RenderFIR}
 Result := Input;
 for Coef := 0 to FOrder - 1
  do Result := Result - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];
 FHistoryPos := (FHistoryPos + 1) mod FOrder;
 FHistory[FHistoryPos] := Input;
 {$ELSE}
 result := (ProcessInteger(Input) + CHalf32) * FBitDiv;
 {$ENDIF}
end;

function TDitherSharpNoiseShaper32.ProcessInteger(Input: Single): Integer;
var
  Coef : Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 for Coef := 0 to FOrder - 1
  do Input := Input - FCoefficients[Coef] * FHistory[(FOrder + FHistoryPos - Coef) mod FOrder];

 FHistoryPos := (FHistoryPos + 1) mod FOrder;

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := Round(Input - CHalf32);
       dtEqual : Result := Round(Input - CHalf32 + FDitherAmplitude * FastRandom);
  dtTriangular : Result := Round(Input - CHalf32 + FDitherAmplitude * (random - random));
       dtGauss : Result := Round(Input - CHalf32 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := Round(Input - CHalf32 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FHistory[FHistoryPos] := Result - Input;
end;

procedure TDitherSharpNoiseShaper32.Reset;
begin
 inherited;
 FillChar(FHistory^[0], FOrder * SizeOf(Single), 0);
end;

procedure TDitherSharpNoiseShaper32.SamplerateChanged;
begin
 ChooseNoiseshaper;
 inherited;
end;

{ TDitherHighShelfNoiseShaper32 }

constructor TDitherHighShelfNoiseShaper32.Create;
begin
 inherited;
 FFrequency := 10000;

 FFilter := TBasicLowShelfFilter.Create;
 with TBasicLowShelfFilter(FFilter) do
  begin
   Frequency := Self.Frequency;
   SampleRate := Self.SampleRate;
   Bandwidth := 1;
   Gain := -5;
  end;
end;

procedure TDitherHighShelfNoiseShaper32.BitDepthChanged;
begin
 FBitMul := IntPower(2, FBitDepth - 1) - CHalf32;
 FBitDiv := 1 / FBitMul;
 FLimits[0] := Round(-FBitMul - CHalf32);
 FLimits[1] := Round( FBitMul - CHalf32);
 Changed;
end;

function TDitherHighShelfNoiseShaper32.ProcessFloat(Input: Single): Single;
{-$DEFINE RenderFIR}
{$IFDEF RenderFIR}
var
  Coef : Integer;
{$ENDIF}
begin
 {$IFDEF RenderFIR}
 // render FIR filter
 Result := Input - FFilter.ProcessSample(FLastSample);
 FLastSample := Input;
 {$ELSE}
 result := (ProcessInteger(Input) + CHalf32) * FBitDiv;
 {$ENDIF}
end;

function TDitherHighShelfNoiseShaper32.ProcessInteger(Input: Single): Integer;
begin
 // scale input to bit range
 Input := FBitMul * Input;

 // Direct FIR filter implementation
 Input := Input - FFilter.ProcessSample64(FLastSample);

 // add triangular distributed noise
 case FDitherType of
        dtNone : Result := Round(Input - CHalf32);
       dtEqual : Result := Round(Input - CHalf32 + FDitherAmplitude * FastRandom);
  dtTriangular : Result := Round(Input - CHalf32 + FDitherAmplitude * (random - random));
       dtGauss : Result := Round(Input - CHalf32 + FDitherAmplitude * RandomGauss);
   dtFastGauss : Result := Round(Input - CHalf32 + FDitherAmplitude * FastRandomGauss);
  else Result := 0;
 end;

 if FLimit then
  if Result < FLimits[0] then Result := FLimits[0] else
  if Result > FLimits[1] then Result := FLimits[1];

 // update buffer
 FLastSample := Result - Input;
end;

procedure TDitherHighShelfNoiseShaper32.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TDitherHighShelfNoiseShaper32.FrequencyChanged;
begin
 TBasicLowShelfFilter(FFilter).Frequency := Frequency;
 Changed;
end;

procedure TDitherHighShelfNoiseShaper32.SampleRateChanged;
begin
 FFilter.SampleRate := SampleRate;
 Changed;
end;

end.
