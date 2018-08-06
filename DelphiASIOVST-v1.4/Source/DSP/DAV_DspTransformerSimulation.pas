unit DAV_DspTransformerSimulation;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterSimple,
  DAV_DspWaveshaper, DAV_DspPolyphaseUpsampler, DAV_DspPolyphaseDownsampler;

type
  TCustomTransformatorSimulation = class(TDspSampleRatePersistent,
    IDspProcessor32, IDspProcessor64)
  private
    FHighpass     : TFirstOrderLowcutFilter;
    FLowpass      : TFirstOrderHighcutFilter;
    FUpsampler    : TPolyphaseUpsampler64;
    FDownsampler  : TPolyphaseDownsampler64;
    FWaveshaper   : TChebyshevWaveshaper;
    FHpFreq       : Single;
    FInputGain    : Single;
    FDampFreq     : Single;
    FWSCoeffs     : array [0..3] of Single;
    procedure SetHPFreq(const Value: Single);
    procedure SetDampFreq(const Value: Single);
  protected
    procedure SampleRateChanged; override;
    procedure HighpassFrequencyChanged; virtual;
    procedure DampingFrequencyChanged; virtual;
    function Waveshaper(Input: Double): Double;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample32(Input: Single): Single;
    function ProcessSample64(Input: Double): Double;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);

    property HighpassFrequency: Single read FHpFreq write SetHPFreq;
    property DampingFrequency: Single read FDampFreq write SetDampFreq;
  end;

  TTransformatorSimulation = class(TCustomTransformatorSimulation)
  published
    property HighpassFrequency;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_DspPolyphaseFilter;

{ TCustomTransformatorSimulation }

constructor TCustomTransformatorSimulation.Create;
begin
 inherited;
 FHpFreq := 10;
 FDampFreq := 12000;

 // create highpass
 FHighpass := TFirstOrderLowcutFilter.Create;
 with FHighpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := FHpFreq;
  end;

 // create highpass
 FLowpass := TFirstOrderHighcutFilter.Create;
 with FLowpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := FDampFreq;
  end;

 // create waveshaper
 FWaveshaper := TChebyshevWaveshaper.Create;
 with FWaveshaper do
  begin
   Order := 3;
   Gain[0] := 1;
   Gain[1] := 1E-2;
   Gain[2] := 1E-4;
   Inverted[1] := False;
  end;
 FWSCoeffs[0] := FWaveshaper.Coefficients[0];
 FWSCoeffs[1] := FWaveshaper.Coefficients[1];
 FWSCoeffs[2] := FWaveshaper.Coefficients[2];
 FWSCoeffs[3] := FWaveshaper.Coefficients[3];

 // create upsampler
 FUpsampler := TPolyphaseUpsampler64.Create;
 with FUpsampler do
  begin
   NumberOfCoefficients := 6;
   Transition := 0.49;
  end;

 // create downsampler
 FDownsampler := TPolyphaseDownsampler64.Create;
 with FDownsampler do
  begin
   NumberOfCoefficients := 6;
   Transition := 0.24;
  end;

 FInputGain := 1 / FWaveshaper.ProcessSample64(1);
end;

procedure TCustomTransformatorSimulation.DampingFrequencyChanged;
begin
 if Assigned(FLowpass)
  then FLowpass.Frequency := FDampFreq;
end;

destructor TCustomTransformatorSimulation.Destroy;
begin
 FreeAndNil(FHighpass);
 FreeAndNil(FWaveshaper);
 FreeAndNil(FUpsampler);
 FreeAndNil(FDownsampler);
 inherited;
end;

procedure TCustomTransformatorSimulation.SetDampFreq(const Value: Single);
begin
 if FDampFreq <> Value then
  begin
   FDampFreq := Value;
   DampingFrequencyChanged;
  end;
end;

procedure TCustomTransformatorSimulation.SetHPFreq(const Value: Single);
begin
 if FHpFreq <> Value then
  begin
   FHpFreq := Value;
   HighpassFrequencyChanged;
  end;
end;

function TCustomTransformatorSimulation.Waveshaper(Input: Double): Double;
begin
// Result := FWaveshaper.ProcessSample64(Input);
 Result := FWSCoeffs[0] + Input * (FWSCoeffs[1] +
   (Input * (FWSCoeffs[2] + (FWSCoeffs[3] * Input))));
end;

procedure TCustomTransformatorSimulation.HighpassFrequencyChanged;
begin
 if Assigned(FHighpass)
  then FHighpass.Frequency := FHpFreq;
end;

procedure TCustomTransformatorSimulation.SampleRateChanged;
begin
 inherited;
 FHighpass.SampleRate := SampleRate;
 FLowpass.SampleRate := SampleRate;
end;

procedure TCustomTransformatorSimulation.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample32(Data[SampleIndex]);
end;

procedure TCustomTransformatorSimulation.ProcessBlock64(
  const Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample64(Data[SampleIndex]);
end;

function TCustomTransformatorSimulation.ProcessSample32(Input: Single): Single;
var
  Data : TDAV2DoubleArray;
begin
 Result := CHalf32 * (2 * Input + FLowpass.ProcessSample32(Input));
 Result := FHighpass.ProcessSample32(Result);
 FUpsampler.ProcessSample(FInputGain * Result, Data);
 Data[0] := Waveshaper(Data[0]);
 Data[1] := Waveshaper(Data[1]);
 Result := FDownsampler.ProcessSample(Data);
end;

function TCustomTransformatorSimulation.ProcessSample64(Input: Double): Double;
var
  Data : TDAV2DoubleArray;
begin
 Result := CHalf64 * (2 * Input + FLowpass.ProcessSample32(Input));
 Result := FHighpass.ProcessSample32(Result);
 FUpsampler.ProcessSample(FInputGain * Result, Data);
 Data[0] := Waveshaper(Data[0]);
 Data[1] := Waveshaper(Data[1]);
 Result := FDownsampler.ProcessSample(Data);
end;

end.
