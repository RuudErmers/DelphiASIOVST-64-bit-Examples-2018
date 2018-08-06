unit DAV_DspAnalogueFilterPrototypes;

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

uses
  Classes, SysUtils, DAV_Common, DAV_Complex;

type
  TCustomAnalogueFilterPrototype = class(TPersistent)
  protected
    procedure CalculateCoefficients; virtual; abstract;
  public
    constructor Create; virtual; abstract;

    function Complex64(Frequency: Double): TComplex64; overload; virtual; abstract;
    function Complex32(Frequency: Single): TComplex32; overload; virtual;
    function Magnitude(Frequency: Double): Double; virtual;
    function Magnitude_dB(Frequency: Double): Double; virtual;
    function Phase(Frequency: Double): Double; virtual;
  end;
  TCustomAnalogueFilterPrototypeClass = class of TCustomAnalogueFilterPrototype;

  TCustomVariableOrderAnalogueFilterPrototype = class(TCustomAnalogueFilterPrototype)
  protected
    function GetOrder: Double; virtual; abstract;
    procedure SetOrder(Value: Double); virtual; abstract;
  public
    property Order: Double read GetOrder write SetOrder;
  end;

  TCustomIntegerOrderAnalogueFilterPrototype = class(TCustomAnalogueFilterPrototype)
  protected
    function GetOrder: Integer; virtual; abstract;
    procedure SetOrder(Value: Integer); virtual; abstract;
  public
    property Order: Integer read GetOrder write SetOrder;
  end;

  TCustomAnalogueArbitraryIntegerOrderFilterPrototype =
    class(TCustomIntegerOrderAnalogueFilterPrototype)
  private
    FOrder : Integer;
  protected
    function GetOrder: Integer; override;
    procedure SetOrder(Value: Integer); override;
    procedure OrderChanged; virtual;
  end;

  TCustomAnalogueButterworthFilterPrototype = class(TCustomAnalogueFilterPrototype)
  private
    FFrequency : Double;
    FOrder     : Double;
    procedure SetFrequency(const Value: Double);
    procedure SetOrder(const Value: Double);
    procedure FrequencyChanged;
    procedure OrderChanged;
  public
    // yet to complete (see Normalized Butterworth polynomials,
    // http://en.wikipedia.org/wiki/Butterworth_filter)

    function Magnitude(Frequency: Double): Double; override;

    property Frequency: Double read FFrequency write SetFrequency;
    property Order: Double read FOrder write SetOrder;
  end;

  TCustomBiquadAnalogueFilterPrototype = class(TCustomIntegerOrderAnalogueFilterPrototype)
  private
    FNominator   : array [0..2] of Double;
    FDenominator : array [0..2] of Double;
    FGain        : Double;
    FBandwidth   : Double;
    FFrequency   : Double;
    procedure SetBandwidth(const Value: Double);
    procedure SetFrequency(const Value: Double);
    procedure SetGain(const Value: Double);
    procedure CalculateAlpha;
    function GetDenominator(Index: Integer): Double;
    function GetNominator(Index: Integer): Double;
  protected
    FAlpha      : Double;
    FGainFactor : Double;
    function GetOrder: Integer; override;
    procedure SetOrder(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure BandwidthChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure GainChanged; virtual;
  public
    constructor Create; override;
    function Complex64(Frequency: Double): TComplex64; override;

    property Frequency: Double read FFrequency write SetFrequency;
    property Bandwidth: Double read FBandwidth write SetBandwidth;
    property Gain: Double read FGain write SetGain;
    property Nominator[Index: Integer]: Double read GetNominator;
    property Denominator[Index: Integer]: Double read GetDenominator;
  end;
  TCustomBiquadAnalogueFilterPrototypeClass = class of TCustomBiquadAnalogueFilterPrototype;

  TAnalogueLowpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueBandpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueAllpassFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueNotchFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnaloguePeakFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueLowshelfFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighshelfFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueLowshelfAFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighshelfAFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueLowshelfBFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueHighshelfBFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TCustomAnalogueShapeFilterPrototype = class(TCustomBiquadAnalogueFilterPrototype)
  private
    FShape : Double;
    procedure SetShape(const Value: Double);
  protected
    procedure ShapeChanged; virtual;
  public
    constructor Create; override;

    property Shape: Double read FShape write SetShape;
  end;

  TAnalogueSimpleShapeFilterPrototype = class(TCustomAnalogueShapeFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TAnalogueShapeFilterPrototype = class(TCustomAnalogueShapeFilterPrototype)
  protected
    procedure CalculateCoefficients; override;
  end;

  TCustomAnalogueWeightingFilterPrototype = class(TCustomIntegerOrderAnalogueFilterPrototype)
  protected
    procedure SetOrder(Value: Integer); override;
  end;
  TCustomAnalogueWeightingFilterPrototypeClass = class of TCustomAnalogueWeightingFilterPrototype;

  TAnalogueAWeightingFilterPrototype = class(TCustomAnalogueWeightingFilterPrototype)
  private
    FDenominator : Array [0..3] of Double;
    FGainFactor  : Double;
  protected
    function GetOrder: Integer; override;
    procedure CalculateCoefficients; override;
  public
    constructor Create; override;
    function Complex64(Frequency: Double): TComplex64; override;
  end;

  TAnalogueBWeightingFilterPrototype = class(TCustomAnalogueWeightingFilterPrototype)
  private
    FDenominator : Array [0..2] of Double;
    FGainFactor  : Double;
  protected
    function GetOrder: Integer; override;
    procedure CalculateCoefficients; override;
  public
    constructor Create; override;
    function Complex64(Frequency: Double): TComplex64; override;
  end;

  TAnalogueCWeightingFilterPrototype = class(TCustomAnalogueWeightingFilterPrototype)
  private
    FDenominator : Array [0..1] of Double;
    FGainFactor  : Double;
  protected
    procedure CalculateCoefficients; override;
    function GetOrder: Integer; override;
  public
    constructor Create; override;
    function Complex64(Frequency: Double): TComplex64; override;
  end;

  TAnalogueDWeightingFilterPrototype = class(TCustomAnalogueWeightingFilterPrototype)
  private
    FNominator   : Array [0..1] of Double;
    FDenominator : Array [0..3] of Double;
    FGainFactor  : Double;
  protected
    procedure CalculateCoefficients; override;
    function GetOrder: Integer; override;
  public
    constructor Create; override;
    function Complex64(Frequency: Double): TComplex64; override;
  end;

implementation

uses
  Math, DAV_Math;

resourcestring
  RCStrFixedOrder = 'The order of a biquad filter can not be changed';
  RCStrIndexOutOfBounds = 'Index out of bounds';

{ TCustomAnalogueFilterPrototype }

function TCustomAnalogueFilterPrototype.Magnitude(Frequency: Double): Double;
var
  Cmplx : TComplex64;
begin
 Cmplx := Complex64(Frequency);
 Result := Sqrt(Sqr(Cmplx.Re) + Sqr(Cmplx.Im));
end;

function TCustomAnalogueFilterPrototype.Magnitude_dB(Frequency: Double): Double;
var
  Cmplx : TComplex64;
begin
 Cmplx := Complex64(Frequency);
 Result := 10 * Log10(Sqr(Cmplx.Re) + Sqr(Cmplx.Im));
end;

function TCustomAnalogueFilterPrototype.Phase(Frequency: Double): Double;
var
  Cmplx : TComplex64;
begin
 Cmplx := Complex64(Frequency);
 Result := ArcTan2(Cmplx.Im, Cmplx.Re);
end;

function TCustomAnalogueFilterPrototype.Complex32(Frequency: Single): TComplex32;
var
  Cmplx : TComplex64;
begin
 Cmplx := Complex64(Frequency);
 Result.Re := Cmplx.Re;
 Result.Im := Cmplx.Im;
end;


{ TCustomAnalogueArbitraryIntegerOrderFilterPrototype }

function TCustomAnalogueArbitraryIntegerOrderFilterPrototype.GetOrder: Integer;
begin
 Result := FOrder;
end;

procedure TCustomAnalogueArbitraryIntegerOrderFilterPrototype.SetOrder(Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TCustomAnalogueArbitraryIntegerOrderFilterPrototype.OrderChanged;
begin
 CalculateCoefficients;
end;


{ TCustomAnalogueButterworthFilterPrototype }

function TCustomAnalogueButterworthFilterPrototype.Magnitude(
  Frequency: Double): Double;
begin
 Result := Sqrt( 1 / (1 + Power(Frequency / FFrequency, 2 * FOrder)));
end;

procedure TCustomAnalogueButterworthFilterPrototype.SetFrequency(
  const Value: Double);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomAnalogueButterworthFilterPrototype.SetOrder(
  const Value: Double);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TCustomAnalogueButterworthFilterPrototype.FrequencyChanged;
begin
end;

procedure TCustomAnalogueButterworthFilterPrototype.OrderChanged;
begin
end;


{ TCustomBiquadAnalogueFilterPrototype }

constructor TCustomBiquadAnalogueFilterPrototype.Create;
begin
 FGainFactor      := 1;
 FBandwidth := 1;
 FFrequency := 1000;
 CalculateAlpha;
 
 FNominator[0] := 1;
 FDenominator[0] := 1;
end;

function TCustomBiquadAnalogueFilterPrototype.Complex64(
  Frequency: Double): TComplex64;
var
  Omega   : Double;
  Divisor : Double;
begin
 Omega    := Frequency / FFrequency;

 Divisor  := 1 / (Sqr(FDenominator[0] - FDenominator[2] * Sqr(Omega)) +
   Sqr(FDenominator[1] * Omega));

 Result.Re := ((FNominator[0] - FNominator[2] * Sqr(Omega)) *
              (FDenominator[0] - FDenominator[2] * Sqr(Omega)) +
              (FNominator[1] * FDenominator[1] * Sqr(Omega))) * Divisor;

 Result.Im := Omega * (FNominator[1]   * (FDenominator[0] - FDenominator[2] * Sqr(Omega)) -
                     (FDenominator[1] * (FNominator[0] - FNominator[2] * Sqr(Omega)))) * Divisor;
end;

function TCustomBiquadAnalogueFilterPrototype.GetDenominator(
  Index: Integer): Double;
begin
 if Index in [0..2]
  then Result := FDenominator[Index]
  else raise Exception.Create(RCStrIndexOutOfBounds);
end;

function TCustomBiquadAnalogueFilterPrototype.GetNominator(
  Index: Integer): Double;
begin
 if Index in [0..2]
  then Result := FNominator[Index]
  else raise Exception.Create(RCStrIndexOutOfBounds);
end;

function TCustomBiquadAnalogueFilterPrototype.GetOrder: Integer;
begin
 Result := 2;
end;

procedure TCustomBiquadAnalogueFilterPrototype.CalculateAlpha;
begin
 FAlpha := (2 * Sinh(ln22 * FBandwidth));
end;

procedure TCustomBiquadAnalogueFilterPrototype.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBiquadAnalogueFilterPrototype then
  with TCustomBiquadAnalogueFilterPrototype(Dest) do
   begin
    Move(Self.FNominator[0], FNominator[0], Length(FNominator) * SizeOf(Double));
    Move(Self.FDenominator[0], FDenominator[0], Length(FDenominator) * SizeOf(Double));
    FGain := Self.FGain;
    FBandwidth := Self.FBandwidth;
    FFrequency := Self.FFrequency;
    FAlpha := Self.FAlpha;
    FGainFactor := Self.FGainFactor;
   end
 else inherited;
end;

procedure TCustomBiquadAnalogueFilterPrototype.BandwidthChanged;
begin
 CalculateAlpha;
 CalculateCoefficients;
end;

procedure TCustomBiquadAnalogueFilterPrototype.FrequencyChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomBiquadAnalogueFilterPrototype.GainChanged;
begin
 FGainFactor := dB_to_Amp(0.5 * FGain);
 CalculateCoefficients;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetBandwidth(
  const Value: Double);
begin
 if FBandwidth <> Value then
  begin
   FBandwidth := Value;
   BandwidthChanged;
  end;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetFrequency(
  const Value: Double);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetGain(const Value: Double);
begin
 if FGain <> Value then
  begin
   FGain := Value;
   GainChanged;
  end;
end;

procedure TCustomBiquadAnalogueFilterPrototype.SetOrder(Value: Integer);
begin
 raise Exception.Create(RCStrFixedOrder); 
end;


{ TAnalogueLowpassFilterPrototype }

procedure TAnalogueLowpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueHighpassFilterPrototype }

procedure TAnalogueHighpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 0;
 FNominator[1] := 0;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueBandpassFilterPrototype }

procedure TAnalogueBandpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 0;
 FNominator[1] := Sqr(FGainFactor) * FAlpha;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueAllpassFilterPrototype }

procedure TAnalogueAllpassFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := -Sqr(FGainFactor) * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueNotchFilterPrototype }

procedure TAnalogueNotchFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 1;
 FNominator[1] := 0;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnaloguePeakFilterPrototype }

procedure TAnaloguePeakFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := 1;
 FNominator[1] := FAlpha * FGainFactor;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha / FGainFactor;
 FDenominator[2] := 1;
end;


{ TAnalogueLowshelfFilterPrototype }

procedure TAnalogueLowshelfFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) * FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := Sqrt(FGainFactor) * FAlpha;
 FDenominator[2] := FGainFactor;
end;


{ TAnalogueHighshelfFilterPrototype }

procedure TAnalogueHighshelfFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := Sqrt(FGainFactor) * FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueLowshelfAFilterPrototype }

procedure TAnalogueLowshelfAFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := Sqr(FGainFactor) * FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := FAlpha;
 FDenominator[2] := FGainFactor;
end;


{ TAnalogueHighshelfAFilterPrototype }

procedure TAnalogueHighshelfAFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := FGainFactor;
 FNominator[1] := Sqr(FGainFactor) * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := FAlpha;
 FDenominator[2] := 1;
end;


{ TAnalogueLowshelfBFilterPrototype }

procedure TAnalogueLowshelfBFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor * FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := FGainFactor * FAlpha;
 FDenominator[2] := FGainFactor;
end;


{ TAnalogueHighshelfBFilterPrototype }

procedure TAnalogueHighshelfBFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor * FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := FGainFactor * FAlpha;
 FDenominator[2] := 1;
end;


{ TCustomAnalogueShapeFilterPrototype }

constructor TCustomAnalogueShapeFilterPrototype.Create;
begin
 inherited;
 FShape := 0;
end;

procedure TCustomAnalogueShapeFilterPrototype.SetShape(const Value: Double);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   ShapeChanged;
  end;
end;

procedure TCustomAnalogueShapeFilterPrototype.ShapeChanged;
begin
 CalculateCoefficients;
end;


{ TAnalogueSimpleShapeFilterPrototype }

procedure TAnalogueSimpleShapeFilterPrototype.CalculateCoefficients;
begin
 FNominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape)) * Power(FGainFactor, Abs(FShape));
 FNominator[1] := FGainFactor * Power(FGainFactor, 0.5 * Abs(FShape)) / FBandwidth;
 FNominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape)) * Power(FGainFactor, Abs(FShape));
 FDenominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape));
 FDenominator[1] := Power(FGainFactor, 0.5 * Abs(FShape)) / (Power(FGainFactor, 1 - Abs(FShape)) * FBandwidth);
 FDenominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape));
end;


{ TAnalogueShapeFilterPrototype }

procedure TAnalogueShapeFilterPrototype.CalculateCoefficients;
var
  InterShape : Double;
begin
(*

 //////////////
 // COMPLETE //
 //////////////

 // Lowpass
 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;

 // OR Highpass if Gain < 1
 FNominator[0] := 0;
 FNominator[1] := 0;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;


 // Lowshelf Type B !!!

 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor / FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := FGainFactor / FAlpha;
 FDenominator[2] := FGainFactor;


 // Normal Lowshelf !!!

 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) / FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := Sqrt(FGainFactor) / FAlpha;
 FDenominator[2] := FGainFactor;


 // Lowshelf Type A !!!

 FNominator[0] := Sqr(FGainFactor);
 FNominator[1] := Sqr(FGainFactor) / FAlpha;
 FNominator[2] := FGainFactor;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := FGainFactor;


 // Peak

 FNominator[0] := 1;
 FNominator[1] := FGainFactor / FAlpha;
 FNominator[2] := 1;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / (FGainFactor * FAlpha);
 FDenominator[2] := 1;


 // Highshelf Type A !!!

 FNominator[0] := FGainFactor;
 FNominator[1] := Sqr(FGainFactor) / FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;


 // Normal Highshelf !!!

 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor * Sqrt(FGainFactor) / FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := Sqrt(FGainFactor) / FAlpha;
 FDenominator[2] := 1;


 // Highshelf Type B !!!
 FNominator[0] := FGainFactor;
 FNominator[1] := FGainFactor / FAlpha;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := FGainFactor;
 FDenominator[1] := FGainFactor / FAlpha;
 FDenominator[2] := 1;


 // Lowpass
 FNominator[0] := 1;
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;

 // OR Highpass if Gain < 1
 FNominator[0] := 0;
 FNominator[1] := 0;
 FNominator[2] := Sqr(FGainFactor);
 FDenominator[0] := 1;
 FDenominator[1] := 1 / FAlpha;
 FDenominator[2] := 1;

*)


 //////////////
 // COMPLETE //
 //////////////

 // pass filters
 if FShape < -3 then
  begin
   InterShape := 4 + FShape;
   if FGainFactor > 1 then
    begin
     FNominator[0] := Sqr(FGainFactor);
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := Power(FGainFactor + 1, InterShape) - 1;
     FDenominator[0] := 1;
     FDenominator[1] := Power(FGainFactor, InterShape) * FAlpha;
     FDenominator[2] := Power(FGainFactor, InterShape);
    end else
   if FGainFactor < 1 then
    begin
     FNominator[0] := (Power(FGainFactor + 1, InterShape) - 1) * FGainFactor;
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := Power(FGainFactor, InterShape);
     FDenominator[0] := 1;
     FDenominator[1] := Power(FGainFactor, InterShape) * FAlpha;
     FDenominator[2] := Power(FGainFactor, InterShape);
    end
   else
    begin
     FNominator[0] := 1;
     FNominator[1] := FAlpha;
     FNominator[2] := 1;
     FDenominator[0] := 1;
     FDenominator[1] := FAlpha;
     FDenominator[2] := 1;
    end;
  end else
 if FShape > 3 then
  begin
   InterShape := 4 - FShape;
   if FGainFactor < 1 then
    begin
     FNominator[0] := Power(FGainFactor, Intershape);
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := (Power(FGainFactor + 1, InterShape) - 1) * FGainFactor;
     FDenominator[0] := Power(FGainFactor, Intershape);
     FDenominator[1] := Power(FGainFactor, Intershape) * FAlpha;
     FDenominator[2] := 1;
    end else
   if FGainFactor > 1 then
    begin
     FNominator[0] := Power(FGainFactor + 1, InterShape) - 1;
     FNominator[1] := (Power(FGainFactor + 1, InterShape) - 1) * FAlpha;
     FNominator[2] := Sqr(FGainFactor);
     FDenominator[0] := Power(FGainFactor, Intershape);
     FDenominator[1] := Power(FGainFactor, Intershape) * FAlpha;
     FDenominator[2] := 1;
    end
   else
    begin
     FNominator[0] := 1;
     FNominator[1] := FAlpha;
     FNominator[2] := 1;
     FDenominator[0] := 1;
     FDenominator[1] := FAlpha;
     FDenominator[2] := 1;
    end;
  end else
 // shape filters
 if FShape < -2 then
  begin
   InterShape := 3 + FShape;
   FNominator[0] := Sqr(FGainFactor);
   FNominator[1] := FGainFactor * Power(Sqrt(FGainFactor), InterShape) * FAlpha;
   FNominator[2] := FGainFactor;
   FDenominator[0] := 1;
   FDenominator[1] := FGainFactor * Power(Sqrt(FGainFactor), -InterShape) * FAlpha;
   FDenominator[2] := FGainFactor;
  end else
 if FShape < -1 then
  begin
   InterShape := 2 + FShape;
   FNominator[0] := Sqr(FGainFactor);
   FNominator[1] := Power(Sqrt(FGainFactor), InterShape) * FGainFactor * Sqrt(FGainFactor) * FAlpha;
   FNominator[2] := FGainFactor;
   FDenominator[0] := 1;
   FDenominator[1] := Power(Sqrt(FGainFactor), 1 - InterShape) * FAlpha;
   FDenominator[2] := FGainFactor;
  end else
 if FShape > 2 then
  begin
   InterShape := FShape - 2;
   FNominator[0] := FGainFactor;
   FNominator[1] := FGainFactor * Power(Sqrt(FGainFactor), 1 - InterShape) * FAlpha;
   FNominator[2] := Sqr(FGainFactor);
   FDenominator[0] := FGainFactor;
   FDenominator[1] := Sqrt(FGainFactor) * Power(Sqrt(FGainFactor), InterShape) * FAlpha;
   FDenominator[2] := 1;
  end else
 if FShape > 1 then
  begin
   InterShape := FShape - 1;
   FNominator[0] := FGainFactor;
   FNominator[1] := Sqr(FGainFactor) * Power(Sqrt(FGainFactor), -InterShape) * FAlpha;
   FNominator[2] := Sqr(FGainFactor);
   FDenominator[0] := FGainFactor;
   FDenominator[1] := Power(Sqrt(FGainFactor), InterShape) * FAlpha;
   FDenominator[2] := 1;
  end
 else
  begin
   FNominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape)) * Power(FGainFactor, Abs(FShape));
   FNominator[1] := Power(FGainFactor, Abs(FShape)) * FGainFactor * FAlpha;
   FNominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape)) * Power(FGainFactor, Abs(FShape));
   FDenominator[0] := Power(FGainFactor, 0.5 * (Abs(FShape) + FShape));
   FDenominator[1] := Power(FGainFactor, Abs(FShape) - 1) * FAlpha;
   FDenominator[2] := Power(FGainFactor, 0.5 * (Abs(FShape) - FShape));
  end;
end;


{ TCustomAnalogueWeightingFilterPrototype }

procedure TCustomAnalogueWeightingFilterPrototype.SetOrder(Value: Integer);
begin
 raise Exception.Create(RCStrFixedOrder);
end;


{ TAnalogueAWeightingFilterPrototype }

constructor TAnalogueAWeightingFilterPrototype.Create;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TAnalogueAWeightingFilterPrototype.CalculateCoefficients;
begin
 // exact values according to DIN EN 61672-1
 FDenominator[0] := 2 * Pi * 20.598997;
 FDenominator[1] := 2 * Pi * 107.65264864;
 FDenominator[2] := 2 * Pi * 737.86223074;
 FDenominator[3] := 2 * Pi * 12194.22;

 FGainFactor := 7390104064;
end;

function TAnalogueAWeightingFilterPrototype.GetOrder: Integer;
begin
  Result := 6;
end;

function TAnalogueAWeightingFilterPrototype.Complex64(
  Frequency: Double): TComplex64;
var
  Omega   : Double;
  Divisor : Double;
  Cmplex  : TComplex64;
  Temp    : array [0..1] of Double;
begin
 Omega := 2 * Pi * Frequency;

 Temp[0] := (Sqr(Omega) - Sqr(FDenominator[0]));
 Temp[1] := (Sqr(Omega) - Sqr(FDenominator[3]));

 Result.Re := Temp[0] * Temp[1] - 4 * Sqr(Omega) * FDenominator[0] * FDenominator[3];
 Result.Im := 2 * Omega * (FDenominator[0] * Temp[1] + FDenominator[3] * Temp[0]);

 Divisor  := FGainFactor * Sqr(Omega) * Sqr(Omega) /
   ((Sqr(Sqr(FDenominator[3]) - Sqr(Omega)) + Sqr(2 * FDenominator[3] * Omega)) *
    (Sqr(Sqr(FDenominator[0]) - Sqr(Omega)) + Sqr(2 * FDenominator[0] * Omega)) *
    (Sqr(FDenominator[1] * FDenominator[2] - Sqr(Omega)) + Sqr((FDenominator[1] + FDenominator[2]) * Omega)));

 Cmplex.Re := (FDenominator[1] * FDenominator[2] - Sqr(Omega)) * Divisor;
 Cmplex.Im := -Omega * (FDenominator[1] + FDenominator[2]) * Divisor;

 ComplexMultiplyInplace64(Result, Cmplex);
end;


{ TAnalogueBWeightingFilterPrototype }

constructor TAnalogueBWeightingFilterPrototype.Create;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TAnalogueBWeightingFilterPrototype.CalculateCoefficients;
begin
 // exact values according to DIN EN 61672-1
 FDenominator[0] := 2 * Pi * 20.598997058;
 FDenominator[2] := 2 * Pi * 12194.217148;

 // from http://en.wikipedia.org/wiki/A-weighting
 FDenominator[1] := 995.9;
 FGainFactor := 5.91797E9;
end;

function TAnalogueBWeightingFilterPrototype.Complex64(
  Frequency: Double): TComplex64;
var
  Omega   : Double;
  Divisor : Double;
  Cmplex  : TComplex64;
begin
 Omega := 2 * Pi * Frequency;

 Divisor  := Sqr(Omega) / (Sqr(Sqr(FDenominator[0]) - Sqr(Omega)) +
   Sqr(2 * FDenominator[0] * Omega));
 Result.Re := (Sqr(Omega) - Sqr(FDenominator[0])) * Divisor;
 Result.Im := 2 * Omega * FDenominator[0] * Divisor;

 Divisor  := 1 / (Sqr(Sqr(FDenominator[2]) - 1 * Sqr(Omega)) +
   Sqr(2 * FDenominator[2] * Omega));
 Cmplex.Re := (Sqr(FDenominator[2]) - Sqr(Omega)) * Divisor;
 Cmplex.Im := -2 * Omega * FDenominator[2] * Divisor;

 ComplexMultiplyInplace64(Result, Cmplex);

 Divisor  := FGainFactor / (Sqr(FDenominator[1]) + Sqr(Omega));
 Result.Re := Sqr(Omega) * Divisor;
 Result.Im := Omega * FDenominator[1] * Divisor;

 ComplexMultiplyInplace64(Result, Cmplex);
end;

function TAnalogueBWeightingFilterPrototype.GetOrder: Integer;
begin
 Result := 5;
end;


{ TAnalogueCWeightingFilterPrototype }

constructor TAnalogueCWeightingFilterPrototype.Create;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TAnalogueCWeightingFilterPrototype.CalculateCoefficients;
begin
 // exact values according to DIN EN 61672-1
 FDenominator[0] := 2 * Pi * 20.598997058;
 FDenominator[1] := 2 * Pi * 12194.217148;

 FGainFactor := 5912384512;
end;

function TAnalogueCWeightingFilterPrototype.GetOrder: Integer;
begin
  Result := 4;
end;

function TAnalogueCWeightingFilterPrototype.Complex64(
  Frequency: Double): TComplex64;
var
  Omega   : Double;
  Divisor : Double;
  Cmplex  : TComplex64;
begin
 Omega := 2 * Pi * Frequency;

 Divisor  := FGainFactor * Sqr(Omega) / (Sqr(Sqr(FDenominator[0]) - Sqr(Omega)) +
   Sqr(2 * FDenominator[0] * Omega));

 Result.Re := (Sqr(Omega) - Sqr(FDenominator[0])) * Divisor;
 Result.Im := 2 * Omega * FDenominator[0] * Divisor;

 Divisor  := 1 / (Sqr(Sqr(FDenominator[1]) - 1 * Sqr(Omega)) +
   Sqr(2 * FDenominator[1] * Omega));

 Cmplex.Re := (Sqr(FDenominator[1]) - Sqr(Omega)) * Divisor;
 Cmplex.Im := -2 * Omega * FDenominator[1] * Divisor;

 ComplexMultiplyInplace64(Result, Cmplex);
end;


{ TAnalogueDWeightingFilterPrototype }

constructor TAnalogueDWeightingFilterPrototype.Create;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TAnalogueDWeightingFilterPrototype.CalculateCoefficients;
begin
 // from http://en.wikipedia.org/wiki/A-weighting
 FNominator[0] := 4.0975E7;
 FNominator[1] := 6532;
 FDenominator[0] := 3.8836E8;
 FDenominator[1] := 21514;
 FDenominator[2] := 1776.3;
 FDenominator[3] := 7288.5;
 FGainFactor := 91104.32;
end;

function TAnalogueDWeightingFilterPrototype.Complex64(
  Frequency: Double): TComplex64;
var
  Omega   : Double;
  Divisor : Double;
  Cmplex  : TComplex64;
begin
 Omega := 2 * Pi * Frequency;

 Divisor  := 1 / (Sqr(FDenominator[0] - Sqr(Omega)) + Sqr(FDenominator[1] * Omega));
 Result.Re := ((FNominator[0] - Sqr(Omega)) * (FDenominator[0] - Sqr(Omega)) +
              (FNominator[1] * FDenominator[1] * Sqr(Omega))) * Divisor;
 Result.Im := Omega * (FNominator[1]   * (FDenominator[0] - Sqr(Omega)) -
                     (FDenominator[1] * (FNominator[0] - Sqr(Omega)))) * Divisor;

 Divisor  := FGainFactor / (Sqr(FDenominator[2] * FDenominator[3] - Sqr(Omega)) +
   Sqr((FDenominator[2] + FDenominator[3]) * Omega));

 Cmplex.Re := Sqr(Omega) * (FDenominator[2] + FDenominator[3]) * Divisor;
 Cmplex.Im := Omega * (FDenominator[2] * FDenominator[3] - Sqr(Omega)) * Divisor;

 ComplexMultiplyInplace64(Result, Cmplex);
end;

function TAnalogueDWeightingFilterPrototype.GetOrder: Integer;
begin
 Result := 4;
end;

end.
