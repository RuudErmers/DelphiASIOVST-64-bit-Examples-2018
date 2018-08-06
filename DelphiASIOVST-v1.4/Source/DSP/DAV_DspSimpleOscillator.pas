unit DAV_DspSimpleOscillator;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  Classes, DAV_Complex, DAV_Classes;

type
  TCustomOscillator = class(TDspSampleRatePersistent)
  end;

  TCustomSimpleOscillator = class(TCustomOscillator)
  private
    procedure CalculateSampleRateReciprocal;
  protected
    FAmplitude     : Double;
    FSampleRateInv : Double;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAmplitude(const Value: Double); virtual; abstract;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    procedure CalculateNextSample; virtual; abstract;
    procedure Reset; virtual; abstract;

    property Amplitude: Double read FAmplitude write SetAmplitude; //  0..1
  end;

  TCustomSimpleFrequencyOscillator = class(TCustomSimpleOscillator)
  private
    procedure SetFrequency(const Value: Single);
  protected
    FFrequency : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; virtual; abstract;
  public
    constructor Create; override;
    property Frequency: Single read FFrequency write SetFrequency; //  0..Samplerate
  end;

  TCustomSimpleOscillator32 = class(TCustomSimpleFrequencyOscillator)
  private
    function GetPhase: Single;
    procedure SetPhase(const Value: Single);
  protected
    FAngle    : TComplex32;
    FPosition : TComplex32;
    procedure FrequencyChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAmplitude(const Value: Double); override;
  public
    procedure CalculateNextSample; override;
    procedure Reset; override;

    property Sine: Single read FPosition.Re;
    property Cosine: Single read FPosition.Im;
    property Phase: Single read GetPhase write SetPhase; //  0..2*Pi;
  end;

  TCustomSimpleOscillator64 = class(TCustomSimpleFrequencyOscillator)
  private
    function GetPhase: Double;
    procedure SetPhase(const Value: Double);
  protected
    FAngle    : TComplex64;
    FPosition : TComplex64;
    procedure FrequencyChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetAmplitude(const Value: Double); override;
  public
    procedure CalculateNextSample; override;
    procedure Reset; override;

    property Sine: Double read FPosition.Re;
    property Cosine: Double read FPosition.Im;
    property Phase: Double read GetPhase write SetPhase; //  0..2*Pi;
  end;

  TSimpleOscillator32 = class(TCustomSimpleOscillator32)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

  TSimpleOscillator64 = class(TCustomSimpleOscillator64)
  published
    property Amplitude;
    property Frequency;
    property Phase;
    property SampleRate;
  end;

  TSimpleOscillator = TSimpleOscillator64;

implementation

uses
  Math, DAV_Math;

{ TCustomSimpleOscillator }

constructor TCustomSimpleOscillator.Create;
begin
 inherited;
 FAmplitude := 1;
 CalculateSampleRateReciprocal;
 Reset;
end;

procedure TCustomSimpleOscillator.SampleRateChanged;
begin
 inherited;
 CalculateSampleRateReciprocal;
 Changed;
end;

procedure TCustomSimpleOscillator.CalculateSampleRateReciprocal;
begin
 FSampleRateInv := 1 / SampleRate;
end;

procedure TCustomSimpleOscillator.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSimpleOscillator then
  with TCustomSimpleOscillator(Dest) do
   begin
    inherited;
    FAmplitude  := Self.FAmplitude;
   end
 else inherited;
end;


{ TCustomSimpleOscillator }

constructor TCustomSimpleFrequencyOscillator.Create;
begin
 inherited;
 FFrequency := 440;
 FrequencyChanged;
end;

procedure TCustomSimpleFrequencyOscillator.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomSimpleFrequencyOscillator then
  with TCustomSimpleFrequencyOscillator(Dest)
   do FFrequency := Self.FFrequency;
end;

procedure TCustomSimpleFrequencyOscillator.SampleRateChanged;
begin
 FrequencyChanged;
 Changed;
end;

procedure TCustomSimpleFrequencyOscillator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;


{ TCustomSimpleOscillator32 }

procedure TCustomSimpleOscillator32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSimpleOscillator32 then
  with TCustomSimpleOscillator32(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end else
 if Dest is TCustomSimpleOscillator64 then
  with TCustomSimpleOscillator64(Dest) do
   begin
    FAngle.Re    := Self.FAngle.Re;
    FAngle.Im    := Self.FAngle.Im;
    FPosition.Re := Self.FPosition.Im;
    FPosition.Re := Self.FPosition.Im;
   end
 else inherited;
end;

procedure TCustomSimpleOscillator32.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency * FSampleRateInv, FAngle.Im, FAngle.Re);
 Changed;
end;

procedure TCustomSimpleOscillator32.SetAmplitude(const Value: Double);
begin
 if FAmplitude <> Value then
  begin
   if FAmplitude = 0 then
    begin
     FPosition.Re := 0;
     FPosition.Im := Value;
    end
   else
    begin
     FPosition.Re := FPosition.Re / FAmplitude * Value;
     FPosition.Im := FPosition.Im / FAmplitude * Value;
    end;
   FAmplitude := Value;
  end;
end;

procedure TCustomSimpleOscillator32.SetPhase(const Value: Single);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

function TCustomSimpleOscillator32.GetPhase: Single;
begin
 Result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomSimpleOscillator32.Reset;
begin
 Phase := 0;
end;

procedure TCustomSimpleOscillator32.CalculateNextSample;
{$IFDEF PUREPASCAL}
var
  Temp : Single;
begin
  Temp := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := Temp;
end;
{$ELSE}
asm
    FLD     [Self.FPosition.Re].Single // FPosition.Re
    FMUL    [Self.FAngle.Re].Single    // FPosition.Re * FAngle.Re
    FLD     [Self.FPosition.Im].Single // FPosition.Im, FPosition.Re * FAngle.Re
    FMUL    [Self.FAngle.Im].Single    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
    FSUBP                              // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

    FLD     [Self.FPosition.Im].Single // FPosition.Im, New.Re
    FMUL    [Self.FAngle.Re].Single    // FPosition.Im * FAngle.Re, New.Re
    FLD     [Self.FPosition.Re].Single // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
    FMUL    [Self.FAngle.Im].Single    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
    FADDP                              // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
    FSTP    [Self.FPosition.Im].Single // FPosition.Im := New.Im, New.Re
    FSTP    [Self.FPosition.Re].Single // FPosition.Re := New.Re
end;
{$ENDIF}


{ TCustomSimpleOscillator64 }

procedure TCustomSimpleOscillator64.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomSimpleOscillator64 then
  with TCustomSimpleOscillator64(Dest) do
   begin
    FAngle    := Self.FAngle;
    FPosition := Self.FPosition;
   end else
 if Dest is TCustomSimpleOscillator32 then
  with TCustomSimpleOscillator32(Dest) do
   begin
    FAngle.Re    := Self.FAngle.Re;
    FAngle.Im    := Self.FAngle.Im;
    FPosition.Re := Self.FPosition.Re;
    FPosition.Im := Self.FPosition.Im;
   end;
end;

procedure TCustomSimpleOscillator64.SetAmplitude(const Value: Double);
begin
 if FAmplitude <> Value then
  begin
   if FAmplitude = 0 then
    begin
     FPosition.Re := 0;
     FPosition.Im := Value;
    end
   else
    begin
     FPosition.Re := FPosition.Re / FAmplitude * Value;
     FPosition.Im := FPosition.Im / FAmplitude * Value;
    end;
   FAmplitude := Value;
  end;
end;

procedure TCustomSimpleOscillator64.SetPhase(const Value: Double);
begin
 GetSinCos(Value, FPosition.Re, FPosition.Im);
 FPosition.Re := FPosition.Re * -FAmplitude;
 FPosition.Im := FPosition.Im * -FAmplitude;
end;

function TCustomSimpleOscillator64.GetPhase: Double;
begin
 Result := -ArcTan2(FPosition.Re, -FPosition.Im);
end;

procedure TCustomSimpleOscillator64.Reset;
begin
 Phase := 0;
end;

procedure TCustomSimpleOscillator64.CalculateNextSample;
{$IFDEF PUREPASCAL}
var
  Temp : Double;
begin
  Temp := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := Temp;
end;
{$ELSE}
asm
    FLD     [Self.FPosition.Re].Double // FPosition.Re
    FMUL    [Self.FAngle.Re].Double    // FPosition.Re * FAngle.Re
    FLD     [Self.FPosition.Im].Double // FPosition.Im, FPosition.Re * FAngle.Re
    FMUL    [Self.FAngle.Im].Double    // FPosition.Im * FAngle.Im, FPosition.Re * FAngle.Re
    FSUBP                              // FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im = New.Re

    FLD     [Self.FPosition.Im].Double // FPosition.Im, New.Re
    FMUL    [Self.FAngle.Re].Double    // FPosition.Im * FAngle.Re, New.Re
    FLD     [Self.FPosition.Re].Double // FPosition.Re, FPosition.Re * FAngle.Re, New.Re
    FMUL    [Self.FAngle.Im].Double    // FPosition.Re * FAngle.Im, FPosition.Re * FAngle.Re, New.Re
    FADDP                              // FPosition.Re * FAngle.Re + FPosition.Im * FAngle.Im = New.Im, New.Re
    FSTP    [Self.FPosition.Im].Double // FPosition.Im := New.Im, New.Re
    FSTP    [Self.FPosition.Re].Double // FPosition.Re := New.Re
end;
{$ENDIF}

procedure TCustomSimpleOscillator64.FrequencyChanged;
begin
 GetSinCos(2 * Pi * FFrequency * FSampleRateInv, FAngle.Im, FAngle.Re);
 Changed;
end;

end.
