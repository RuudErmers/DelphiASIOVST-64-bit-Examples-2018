unit DAV_DspMetronome;

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
  Classes, DAV_Types, DAV_Complex, DAV_Classes;

type
  TMetronome = class(TDspSampleRatePersistent, IDspGenerator32)
  private
    FAngle          : TComplex64;
    FPosition       : TComplex64;
    FDecayFactor    : Single;
    FVolume         : Single;
    FBeatPos        : Integer;
    FSamplesPerBeat : Single;
    FSamplesCount   : Single;
    FMetroVolume    : Single;
    FBeatsPerMinute : Double;
    procedure SetBeatsPerMinute(const Value: Double);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure BeatsPerMinuteChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    procedure CalculateSamplesPerBeat;

    procedure Reset;

    function ProcessSample32: Single;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
  published
    property BeatsPerMinute: Double read FBeatsPerMinute write SetBeatsPerMinute;
    property Samplerate;
  end;

implementation

uses
  DAV_Math;

{ TMetronome }

constructor TMetronome.Create;
begin
  inherited;
  FBeatsPerMinute := 120;
  FMetroVolume := 1;
  FDecayFactor := 0.995;
  FVolume := 1;
  CalculateSamplesPerBeat;
  Reset;
end;

procedure TMetronome.Reset;
begin
  FSamplesCount := 0;
  FPosition.Re := 1;
  FPosition.Im := 0;
end;

procedure TMetronome.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32;
end;

function TMetronome.ProcessSample32: Single;
begin
  Result := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
  FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
  FPosition.Re := Result;
  if FBeatPos = 0 then Result := 2 * FPosition.Re * FPosition.Re - 1;
  Result := FVolume * Result * FMetroVolume;
  FMetroVolume := FDecayFactor * FMetroVolume;
  FSamplesCount := FSamplesCount + 1;
  if FSamplesCount > FSamplesPerBeat then
   begin
    FMetroVolume := 1;
    FSamplesCount := FSamplesCount - FSamplesPerBeat;
    FPosition.Re := 1;
    FPosition.Im := 0;
    if FBeatPos < 3
     then Inc(FBeatPos)
     else FBeatPos := 0;
   end;
end;

procedure TMetronome.SetBeatsPerMinute(const Value: Double);
begin
  if FBeatsPerMinute <> Value then
   begin
    FBeatsPerMinute := Value;
    BeatsPerMinuteChanged;
   end;
end;

procedure TMetronome.BeatsPerMinuteChanged;
begin
 CalculateSamplesPerBeat;
 Changed;
end;

procedure TMetronome.AssignTo(Dest: TPersistent);
begin
 if Dest is TMetronome then
  with TMetronome(Dest) do
   begin
    inherited;
    FAngle          := Self.FAngle;
    FPosition       := Self.FPosition;
    FDecayFactor    := Self.FDecayFactor;
    FVolume         := Self.FVolume;
    FBeatPos        := Self.FBeatPos;
    FSamplesPerBeat := Self.FSamplesPerBeat;
    FSamplesCount   := Self.FSamplesCount;
    FMetroVolume    := Self.FMetroVolume;
    FBeatsPerMinute := Self.FBeatsPerMinute;
   end
 else inherited;
end;

procedure TMetronome.CalculateSamplesPerBeat;
begin
  FSamplesPerBeat := 60 / FBeatsPerMinute * SampleRate;
  GetSinCos(2000 * Pi / SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TMetronome.SampleRateChanged;
begin
 FDecayFactor := 0.995; // need to be samplerate independent in the future!
 CalculateSamplesPerBeat;
 inherited;
end;

end.
