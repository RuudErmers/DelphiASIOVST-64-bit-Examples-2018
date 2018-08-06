unit DAV_DspPulsing;

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

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TCustomPulsing = class(TDspSampleRatePersistent, IDspProcessor32, IDspProcessor64)
  private
    FPeriod_s      : Single;
    FMaximum_dB    : Single;
    FMinimum_dB    : Single;
    FMaximumGain   : Single;
    FMinimumGain   : Single;
    FSlewRate_dB_s : Single;
    FSampleCount   : Integer;
    FStateIndex    : Integer;
    FSamples       : array [0..1] of Integer;
    procedure SetMaximum(const Value: Single);
    procedure SetMinimum(const Value: Single);
    procedure SetPeriod(const Value: Single);
    procedure SetSlewRate(const Value: Single);
  protected
    FCurrentGain     : Double;
    FCurrentSlewRate : Double;
    FSlewRate        : Double;
    procedure CalculateSamples; virtual; abstract;
    procedure CalculateSlewRate;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure PeriodChanged; virtual;
    procedure SlewRateChanged; virtual;
  public
    constructor Create; override;
    procedure Reset; virtual;

    function ProcessSample32(Input: Single): Single;
    function ProcessSample64(Input: Double): Double;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);

    property Period_s: Single read FPeriod_s write SetPeriod;
    property Maximum_dB: Single read FMaximum_dB write SetMaximum;
    property Minimum_dB: Single read FMinimum_dB write SetMinimum;
    property SlewRate_dB_s: Single read FSlewRate_dB_s write SetSlewRate;
  end;

  TSymetricPulsing = class(TCustomPulsing)
  protected
    procedure CalculateSamples; override;
  published
    property Period_s;
    property Maximum_dB;
    property Minimum_dB;
    property SlewRate_dB_s;
  end;

  TAsymetricPulsing = class(TCustomPulsing)
  private
    FLength_s      : Single;
    procedure SetLength(const Value: Single);
  protected
    procedure CalculateSamples; override;
    procedure LengthChanged; virtual;
  public
    constructor Create; override;
  published
    property Length_s: Single read FLength_s write SetLength;
  end;

implementation

uses
  SysUtils, Math, DAV_Common;

resourcestring
  RCStrPeriodTooShort = 'Period must be longer than the length';
  RCStrLengthTooLong = 'Length must be shorter than period';

{ TCustomPulsing }

constructor TCustomPulsing.Create;
begin
 inherited;
 FPeriod_s := 1;
 FMaximum_dB :=   0;
 FMinimum_dB := -40;
 FSlewRate_dB_s := 400;
 FStateIndex := 0;

 MaximumChanged;
 MinimumChanged;
 SlewRateChanged;
 CalculateSamples;
end;

procedure TCustomPulsing.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TCustomPulsing.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

procedure TCustomPulsing.SetMaximum(const Value: Single);
begin
 if FMaximum_dB <> Value then
  begin
   FMaximum_dB := Value;
   MaximumChanged;
  end;
end;

procedure TCustomPulsing.SetMinimum(const Value: Single);
begin
 if FMinimum_dB <> Value then
  begin
   FMinimum_dB := Value;
   MinimumChanged;
  end;
end;

procedure TCustomPulsing.SetPeriod(const Value: Single);
begin
 if FPeriod_s <> Value then
  begin
   FPeriod_s := Value;
   PeriodChanged;
  end;
end;

procedure TCustomPulsing.SetSlewRate(const Value: Single);
begin
 if FSlewRate_dB_s <> Value then
  begin
   FSlewRate_dB_s := Value;
   SlewRateChanged;
  end;
end;

procedure TCustomPulsing.MaximumChanged;
begin
 FMaximumGain := dB_to_Amp(FMaximum_dB);
end;

procedure TCustomPulsing.MinimumChanged;
begin
 FMinimumGain := dB_to_Amp(FMinimum_dB);
end;

procedure TCustomPulsing.SlewRateChanged;
begin
 CalculateSlewRate;
 if FStateIndex = 0
  then FCurrentSlewRate := FSlewRate
  else FCurrentSlewRate := 1 / FSlewRate;
end;

procedure TCustomPulsing.PeriodChanged;
begin
 CalculateSamples;
end;

procedure TCustomPulsing.CalculateSlewRate;
begin
 FSlewRate := Power(Exp(FSlewRate_dB_s * 0.11512925464970228420089957273422), 1 / SampleRate);
end;

function TCustomPulsing.ProcessSample32(Input: Single): Single;
begin
 if FSampleCount >= FSamples[FStateIndex] then
  begin
   FCurrentSlewRate := 1 / FCurrentSlewRate;
   FStateIndex := 1 - FStateIndex;
   FSampleCount := 0;
  end;

 Inc(FSampleCount);
 FCurrentGain := FCurrentGain * FCurrentSlewRate;
 if FCurrentGain > FMaximumGain then FCurrentGain := FMaximumGain else
 if FCurrentGain < FMinimumGain then FCurrentGain := FMinimumGain;

 Result := FCurrentGain * Input;
end;

function TCustomPulsing.ProcessSample64(Input: Double): Double;
begin
 if FSampleCount >= FSamples[FStateIndex] then
  begin
   FCurrentSlewRate := 1 / FCurrentSlewRate;
   FStateIndex := 1 - FStateIndex;
   FSampleCount := 0;
  end;

 Inc(FSampleCount);
 FCurrentGain := FCurrentGain * FCurrentSlewRate;
 if FCurrentGain > FMaximumGain then FCurrentGain := FMaximumGain else
 if FCurrentGain < FMinimumGain then FCurrentGain := FMinimumGain;

 Result := FCurrentGain * Input;
end;

procedure TCustomPulsing.Reset;
begin
 FSampleCount := 0;
 FStateIndex := 0;
 FCurrentGain := FMinimumGain;
end;


{ TSymetricPulsing }

procedure TSymetricPulsing.CalculateSamples;
begin
 FSamples[0] := Round(0.5 * FPeriod_s * SampleRate);
 FSamples[1] := FSamples[0];
end;


{ TAsymetricPulsing }

constructor TAsymetricPulsing.Create;
begin
 inherited;
 FLength_s := 0.5;
end;

procedure TAsymetricPulsing.CalculateSamples;
begin
 FSamples[0] := Round(FLength_s * SampleRate);
 FSamples[1] := Round(FPeriod_s * SampleRate) - FSamples[0];
end;

procedure TAsymetricPulsing.SetLength(const Value: Single);
begin
 if Value >= FPeriod_s
  then raise Exception.Create(RCStrLengthTooLong);

 if FLength_s <> Value then
  begin
   FLength_s := Value;
   LengthChanged;
  end;
end;

procedure TAsymetricPulsing.LengthChanged;
begin
 CalculateSamples;
end;

end.
