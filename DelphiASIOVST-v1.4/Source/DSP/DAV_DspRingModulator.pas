unit DAV_DspRingModulator;

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

uses
  DAV_Types, DAV_Classes, DAV_DspLFO;

type
  TCustomRingModulator = class(TDspPersistent)
  end;

  TRingModulator = class(TCustomRingModulator)
  public
    function ProcessSample(Input: Single; Carrier: Single): Single; virtual;
  end;

  TCustomAnalogRingModulator = class(TCustomRingModulator)
  private
    FCoefficients : array [0..3] of Single;
  public
    constructor Create; virtual;
    function ProcessSample(Input: Single; Carrier: Single): Single; virtual; abstract;
  end;

  TAnalogRingModulator = class(TCustomAnalogRingModulator)
  public
    function ProcessSample(Input: Single; Carrier: Single): Single; override;
  end;

  TLightweightAnalogRingModulator = class(TCustomAnalogRingModulator)
  public
    function ProcessSample(Input: Single; Carrier: Single): Single; override;
  end;

  TCustomAutoRingModulator = class(TCustomRingModulator)
  private
    FSampleRate : Single;
    FFrequency  : Single;
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure FrequencyChanged; virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
  public
    constructor Create; virtual;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
  end;

  TCustomAnalogAutoRingModulator = class(TCustomAutoRingModulator)
  private
    FCoefficients : array [0..3] of Single;
  public
    constructor Create; override;
  end;

  TCustomAutoRingModulator32 = class(TCustomAutoRingModulator, IDspProcessor32)
  private
    FLfo : TLFOSine32;
  protected
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual;
  end;

  TCustomAnalogAutoRingModulator32 = class(TCustomAnalogAutoRingModulator,
    IDspProcessor32)
  private
    FLfo : TLFOSine32;
  protected
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual; abstract;
  end;

  TCustomAutoRingModulator64 = class(TCustomAutoRingModulator, IDspProcessor64)
  private
    FLfo : TLFOSine64;
  protected
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double; virtual;
  end;

  TCustomAnalogAutoRingModulator64 = class(TCustomAnalogAutoRingModulator,
    IDspProcessor64)
  private
    FLfo : TLFOSine64;
  protected
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double; virtual; abstract;
  end;


  TAutoRingModulator32 = class(TCustomAutoRingModulator32)
  published
    property Frequency;
    property SampleRate;
  end;

  TAnalogAutoRingModulator32 = class(TCustomAnalogAutoRingModulator32)
  public
    function ProcessSample32(Input: Single): Single; override;
  published
    property Frequency;
    property SampleRate;
  end;

  TLightweightAnalogAutoRingModulator32 = class(TCustomAnalogAutoRingModulator32)
  public
    function ProcessSample32(Input: Single): Single; override;
  published
    property Frequency;
    property SampleRate;
  end;

  TAutoRingModulator64 = class(TCustomAutoRingModulator32)
  published
    property Frequency;
    property SampleRate;
  end;

  TAnalogAutoRingModulator64 = class(TCustomAnalogAutoRingModulator64)
  public
    function ProcessSample64(Input: Double): Double; override;
  published
    property Frequency;
    property SampleRate;
  end;

  TLightweightAnalogAutoRingModulator64 = class(TCustomAnalogAutoRingModulator64)
  public
    function ProcessSample64(Input: Double): Double; override;
  published
    property Frequency;
    property SampleRate;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Math, DAV_Approximations;


{ TRingModulator }

function TRingModulator.ProcessSample(Input, Carrier: Single): Single;
begin
 Result := Input * Carrier;
end;


{ TCustomAnalogRingModulator }

constructor TCustomAnalogRingModulator.Create;
begin
 inherited;
 FCoefficients[0] := 0.01;
 FCoefficients[1] := 0.01;
 FCoefficients[2] := 0.01;
 FCoefficients[3] := 0.01;
end;


{ TAnalogRingModulator }

function TAnalogRingModulator.ProcessSample(Input, Carrier: Single): Single;
begin
 Result := (Input + FCoefficients[0] * Carrier) *
   Tanh(Carrier + FCoefficients[1] * Input) + FCoefficients[2] * Carrier +
   FCoefficients[3] * Input
end;


{ TLightweightAnalogRingModulator }

function TLightweightAnalogRingModulator.ProcessSample(Input,
  Carrier: Single): Single;
begin
 Result := (Input + FCoefficients[0] * Carrier) *
   FastTanhContinousError3(Carrier + FCoefficients[1] * Input) +
   FCoefficients[2] * Carrier + FCoefficients[3] * Input
end;


{ TCustomAutoRingModulator }

constructor TCustomAutoRingModulator.Create;
begin
 FSampleRate := 44100;
 FFrequency := 1000;
end;

procedure TCustomAutoRingModulator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomAutoRingModulator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;


{ TCustomAnalogAutoRingModulator }

constructor TCustomAnalogAutoRingModulator.Create;
begin
 inherited;
 FCoefficients[0] := 0.01;
 FCoefficients[1] := 0.01;
 FCoefficients[2] := 0.01;
 FCoefficients[3] := 0.01;
end;


{ TCustomAutoRingModulator32 }

constructor TCustomAutoRingModulator32.Create;
begin
 inherited;
 FLfo := TLFOSine32.Create;
 FLfo.SampleRate := SampleRate;
 FLfo.Frequency := Frequency;
end;

destructor TCustomAutoRingModulator32.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomAutoRingModulator32.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
 Changed;
end;

procedure TCustomAutoRingModulator32.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
 Changed;
end;

procedure TCustomAutoRingModulator32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TCustomAutoRingModulator32.ProcessSample32(Input: Single): Single;
begin
 Result := Input * FLfo.Sine;
 FLfo.CalculateNextSample;
end;


{ TCustomAutoRingModulator64 }

constructor TCustomAutoRingModulator64.Create;
begin
 inherited;
 FLfo := TLFOSine64.Create;
 FLfo.SampleRate := SampleRate;
 FLfo.Frequency := Frequency;
end;

destructor TCustomAutoRingModulator64.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomAutoRingModulator64.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
 Changed;
end;

procedure TCustomAutoRingModulator64.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
 Changed;
end;

procedure TCustomAutoRingModulator64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomAutoRingModulator64.ProcessSample64(Input: Double): Double;
begin
 Result := Input * FLfo.Sine;
 FLfo.CalculateNextSample;
end;

{ TCustomAnalogAutoRingModulator32 }

constructor TCustomAnalogAutoRingModulator32.Create;
begin
 inherited;
 FLfo := TLFOSine32.Create;
 FLfo.SampleRate := SampleRate;
 FLfo.Frequency := Frequency;
end;

destructor TCustomAnalogAutoRingModulator32.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomAnalogAutoRingModulator32.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
 Changed;
end;

procedure TCustomAnalogAutoRingModulator32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TCustomAnalogAutoRingModulator32.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
 Changed;
end;


{ TCustomAnalogAutoRingModulator64 }

constructor TCustomAnalogAutoRingModulator64.Create;
begin
 inherited;
 FLfo := TLFOSine64.Create;
 FLfo.SampleRate := SampleRate;
 FLfo.Frequency := Frequency;
end;

destructor TCustomAnalogAutoRingModulator64.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomAnalogAutoRingModulator64.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
 Changed;
end;

procedure TCustomAnalogAutoRingModulator64.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

procedure TCustomAnalogAutoRingModulator64.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
 Changed;
end;


{ TAnalogAutoRingModulator32 }

function TAnalogAutoRingModulator32.ProcessSample32(Input: Single): Single;
begin
 Result := (Input + FCoefficients[0] * FLfo.Sine) *
   Tanh(FLfo.Sine + FCoefficients[1] * Input) +
   FCoefficients[2] * FLfo.Sine + FCoefficients[3] * Input;
 FLfo.CalculateNextSample;
end;


{ TLightweightAnalogAutoRingModulator32 }

function TLightweightAnalogAutoRingModulator32.ProcessSample32(Input: Single): Single;
begin
 Result := (Input + FCoefficients[0] * FLfo.Sine) *
   FastTanhContinousError3(FLfo.Sine + FCoefficients[1] * Input) +
   FCoefficients[2] * FLfo.Sine + FCoefficients[3] * Input;
 FLfo.CalculateNextSample;
end;


{ TAnalogAutoRingModulator64 }

function TAnalogAutoRingModulator64.ProcessSample64(Input: Double): Double;
begin
 Result := (Input + FCoefficients[0] * FLfo.Sine) *
   Tanh(FLfo.Sine + FCoefficients[1] * Input) +
   FCoefficients[2] * FLfo.Sine + FCoefficients[3] * Input;
 FLfo.CalculateNextSample;
end;


{ TLightweightAnalogAutoRingModulator64 }

function TLightweightAnalogAutoRingModulator64.ProcessSample64(Input: Double): Double;
begin
 Result := (Input + FCoefficients[0] * FLfo.Sine) *
   FastTanhContinousError3(FLfo.Sine + FCoefficients[1] * Input) +
   FCoefficients[2] * FLfo.Sine + FCoefficients[3] * Input;
 FLfo.CalculateNextSample;
end;

initialization
  RegisterDspProcessors32([TAutoRingModulator32, TAnalogAutoRingModulator32,
    TLightweightAnalogAutoRingModulator32]);
  RegisterDspProcessors64([TAutoRingModulator64, TAnalogAutoRingModulator64,
    TLightweightAnalogAutoRingModulator64]);

end.
