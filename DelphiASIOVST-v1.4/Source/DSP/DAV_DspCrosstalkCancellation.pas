unit DAV_DspCrosstalkCancellation;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspDelayLines, DAV_DspFilterBasics;

type
  TCrosstalkFilterType = (cftHighshelf);

  TCustomCrosstalkCancellation = class(TDspSampleRatePersistent)
  private
    FSpeakerDistance          : Single;
    FListenerDistance         : Single;
    FAttenuation              : Single;
    FHeadRadius               : Single;
    FStageCount               : Integer;
    FCrosstalkFilterType      : TCrosstalkFilterType;
    FCrosstalkFilterFrequency : Single;
    FCrosstalkFilterGain      : Single;
    procedure SetListenerDistance(const Value: Single);
    procedure SetSpeakerDistance(const Value: Single);
    procedure SetStageCount(const Value: Integer);
    procedure SetCrosstalkFilterType(const Value: TCrosstalkFilterType);
    procedure SetCrosstalkFilterFrequency(const Value: Single);
    procedure SetCrosstalkFilterGain(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CrosstalkFilterFrequencyChanged; virtual; abstract;
    procedure CrosstalkFilterGainChanged; virtual; abstract;
    procedure ListenerDistanceChanged; virtual;
    procedure SpeakerDistanceChanged; virtual;
    procedure StageCountChanged; virtual; abstract;
  public
    constructor Create; override;
    procedure ProcessStereo(var Left, Right: Single); overload; virtual; abstract;

    property Attenuation: Single read FAttenuation write FAttenuation;
    property CrosstalkFilterType: TCrosstalkFilterType read FCrosstalkFilterType write SetCrosstalkFilterType;
    property CrosstalkFilterFrequency: Single read FCrosstalkFilterFrequency write SetCrosstalkFilterFrequency;
    property CrosstalkFilterGain: Single read FCrosstalkFilterGain write SetCrosstalkFilterGain;
    property HeadRadius: Single read FHeadRadius;
    property ListenerDistance: Single read FListenerDistance write SetListenerDistance;
    property SpeakerDistance: Single read FSpeakerDistance write SetSpeakerDistance;
    property StageCount: Integer read FStageCount write SetStageCount;
  end;

  TCrosstalkCancellation32 = class(TCustomCrosstalkCancellation)
  protected
    FDelayLine       : array [0..1] of array of TDelayLineTime32;
    FCrosstalkFilter : array [0..1] of array of TBasicHighShelfFilter;

    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateCoefficients; virtual;
    procedure CalculateCrosstalkFilter; virtual;
    procedure CrosstalkFilterFrequencyChanged; override;
    procedure CrosstalkFilterGainChanged; override;
    procedure ListenerDistanceChanged; override;
    procedure SampleRateChanged; override;
    procedure SpeakerDistanceChanged; override;
    procedure StageCountChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessStereo(var Left, Right: Single); overload; override;
  end;

  TCrosstalkCancellation = TCrosstalkCancellation32;

implementation

uses
  SysUtils, Math, DAV_DspFilter;

{ TCustomCrosstalkCancellation }

constructor TCustomCrosstalkCancellation.Create;
begin
 inherited;
 FHeadRadius               := 8;
 FSpeakerDistance          := 100;
 FListenerDistance         := 100;
 FAttenuation              := 0.5;
 FCrosstalkFilterFrequency := 1000;
 FCrosstalkFilterGain      := -10;
 SampleRateChanged;
end;

procedure TCustomCrosstalkCancellation.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCrosstalkCancellation then
  with TCustomCrosstalkCancellation(Dest) do
   begin
    inherited;
    FSpeakerDistance          := Self.FSpeakerDistance;
    FListenerDistance         := Self.FListenerDistance;
    FAttenuation              := Self.FAttenuation;
    FHeadRadius               := Self.FHeadRadius;
    FStageCount               := Self.FStageCount;
    FCrosstalkFilterType      := Self.FCrosstalkFilterType;
    FCrosstalkFilterFrequency := Self.FCrosstalkFilterFrequency;
    FCrosstalkFilterGain      := Self.FCrosstalkFilterGain;
   end
  else inherited;
end;

procedure TCustomCrosstalkCancellation.ListenerDistanceChanged;
begin
 if FSpeakerDistance > 2 * FListenerDistance
  then SpeakerDistance := 2 * FListenerDistance;
 Changed;
end;

procedure TCustomCrosstalkCancellation.SetCrosstalkFilterFrequency(
  const Value: Single);
begin
 if FCrosstalkFilterFrequency <> Value then
  begin
   FCrosstalkFilterFrequency := Value;
   CrosstalkFilterFrequencyChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetCrosstalkFilterGain(
  const Value: Single);
begin
 if FCrosstalkFilterGain <> Value then
  begin
   FCrosstalkFilterGain := Value;
   CrosstalkFilterGainChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetCrosstalkFilterType(
  const Value: TCrosstalkFilterType);
begin
 if FCrosstalkFilterType <> Value then
  begin
   FCrosstalkFilterType := Value;
  end;
end;

procedure TCustomCrosstalkCancellation.SetListenerDistance(const Value: Single);
begin
 if FListenerDistance <> Value then
  begin
   FListenerDistance := Value;
   ListenerDistanceChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetSpeakerDistance(const Value: Single);
begin
 if FSpeakerDistance <> Value then
  begin
   FSpeakerDistance := Value;
   SpeakerDistanceChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SetStageCount(const Value: Integer);
begin
 if FStageCount <> Value then
  begin
   FStageCount := Value;
   StageCountChanged;
  end;
end;

procedure TCustomCrosstalkCancellation.SpeakerDistanceChanged;
begin
 if 2 * FListenerDistance < FSpeakerDistance
  then FListenerDistance := 0.5 * SpeakerDistance;
 Changed;
end;

{ TCrosstalkCancellation32 }

constructor TCrosstalkCancellation32.Create;
begin
 inherited;
 FStageCount := 2;
 StageCountChanged;
 CalculateCrosstalkFilter;
end;

destructor TCrosstalkCancellation32.Destroy;
var
  Channel : Cardinal;
  Stage   : Cardinal;
begin
 for Channel := 0 to Length(FDelayLine) - 1 do
  for Stage := 0 to Length(FDelayLine[Channel]) - 1
   do FreeAndNil(FDelayLine[Channel, Stage]);
  inherited;
end;

procedure TCrosstalkCancellation32.CrosstalkFilterFrequencyChanged;
begin
 CalculateCrosstalkFilter;
 Changed;
end;

procedure TCrosstalkCancellation32.CrosstalkFilterGainChanged;
begin
 CalculateCrosstalkFilter;
 Changed;
end;

procedure TCrosstalkCancellation32.CalculateCrosstalkFilter;
var
  Channel    : Integer;
  Stage      : Integer;
begin
 for Channel := 0 to Length(FCrosstalkFilter) - 1 do
  for Stage := 0 to Length(FCrosstalkFilter[Channel]) - 1 do
   with FCrosstalkFilter[Channel, Stage] do
    begin
     Frequency := FCrosstalkFilterFrequency;
     Gain      := FCrosstalkFilterGain;
     Bandwidth := 3.3;
    end;
end;

procedure TCrosstalkCancellation32.AssignTo(Dest: TPersistent);
var
  Stage : Integer;
begin
 if Dest is TCrosstalkCancellation32 then
  with TCrosstalkCancellation32(Dest) do
   begin
    inherited;
    // assign delay lines
    for Stage := 0 to Length(FDelayLine[0]) - 1
     do FDelayLine[0, Stage].Assign(Self.FDelayLine[0, Stage]);
    for Stage := 0 to Length(FDelayLine[1]) - 1
     do FDelayLine[1, Stage].Assign(Self.FDelayLine[1, Stage]);

    // assign filters
    for Stage := 0 to Length(FCrosstalkFilter[0]) - 1
     do FCrosstalkFilter[0, Stage].Assign(Self.FCrosstalkFilter[0, Stage]);
    for Stage := 0 to Length(FCrosstalkFilter[1]) - 1
     do FCrosstalkFilter[1, Stage].Assign(Self.FCrosstalkFilter[1, Stage]);
   end
  else inherited;
end;

procedure TCrosstalkCancellation32.CalculateCoefficients;
var
  Channel    : Integer;
  Stage      : Integer;
  Alpha      : Single;
  DirectDist : Single;
  CTDistance : Single;
begin
 if FListenerDistance < 0.5 * FSpeakerDistance
  then exit;
 assert(FListenerDistance >= 0.5 * FSpeakerDistance);

 Alpha := arcsin(FSpeakerDistance * 0.5 / FListenerDistance);
 DirectDist := sqrt(sqr(FHeadRadius) + sqr(FListenerDistance) -
   2 * FHeadRadius * FListenerDistance * cos(0.5 * Pi - Alpha));

 CTDistance := sqrt(sqr(FHeadRadius) + sqr(FListenerDistance) -
   2 * FHeadRadius * FListenerDistance * cos(0.5 * Pi + Alpha));

 assert(CTDistance - DirectDist > 0);

 for Channel := 0 to Length(FDelayLine) - 1 do
  for Stage := 0 to Length(FDelayLine[Channel]) - 1 do
   begin
    FDelayLine[Channel, Stage].Time := (CTDistance - DirectDist) * 2.94E-5;
   end;
end;

procedure TCrosstalkCancellation32.ListenerDistanceChanged;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TCrosstalkCancellation32.SpeakerDistanceChanged;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TCrosstalkCancellation32.SampleRateChanged;
var
  Channel : Cardinal;
  Stage   : Cardinal;
begin
 for Channel := 0 to Length(FDelayLine) - 1 do
  if assigned(FDelayLine[Channel]) then
   for Stage := 0 to Length(FDelayLine[Channel]) - 1
    do FDelayLine[Channel, Stage].Samplerate := SampleRate;
 for Channel := 0 to Length(FDelayLine) - 1 do
  if assigned(FCrosstalkFilter[Channel]) then
   for Stage := 0 to Length(FCrosstalkFilter[Channel]) - 1
    do FCrosstalkFilter[Channel, Stage].Samplerate := SampleRate;
end;

procedure TCrosstalkCancellation32.StageCountChanged;
var
  Channel       : Integer;
  Stage         : Integer;
  OldStageCount : Integer;
begin
 for Channel := 0 to Length(FDelayLine) - 1 do
  begin
   if FStageCount > Length(FDelayLine[Channel]) then
    begin
     OldStageCount := Length(FDelayLine[Channel]);
     SetLength(FDelayLine[Channel], FStageCount);
     SetLength(FCrosstalkFilter[Channel], FStageCount);
     for Stage := OldStageCount to FStageCount - 1 do
      begin
       FDelayLine[Channel, Stage] := TDelayLineTime32.Create;
       FCrosstalkFilter[Channel, Stage] := TBasicHighShelfFilter.Create;
      end;
    end
   else
    begin
     for Stage := Length(FDelayLine[Channel]) - 1 downto FStageCount do
      begin
       FreeAndNil(FDelayLine[Channel, Stage]);
       FreeAndNil(FCrosstalkFilter[Channel, Stage]);
      end;
     SetLength(FDelayLine[Channel], FStageCount);
     SetLength(FCrosstalkFilter[Channel], FStageCount);
    end;
  end;
 CalculateCoefficients;
 Changed;
end;

procedure TCrosstalkCancellation32.ProcessStereo(var Left, Right: Single);
var
  Stage   : Integer;
  TempIn  : TDAV2SingleArray;
  Temp    : Single;
begin
(*
 TempIn[0] := InputLeft;
 TempIn[1] := InputRight;
 for Stage := 0 to FStageCount - 1 do
  begin
   TempIn[1] := FDelayLine[0, Stage].ProcessSample(TempIn[1]) - FAttenuation * FCrosstalkFilter[0, Stage].ProcessSample(TempIn[0]);
   TempIn[0] := FDelayLine[1, Stage].ProcessSample(TempIn[0]) - FAttenuation * FCrosstalkFilter[1, Stage].ProcessSample(TempIn[1]);
  end;
 OutputLeft  := TempIn[0];
 OutputRight := TempIn[1];
*)

 TempIn[0]   := Left;
 TempIn[1]   := Right;

 for Stage := 0 to FStageCount - 1 do
  begin
   Temp      := FDelayLine[0, Stage].ProcessSample32(FAttenuation * FCrosstalkFilter[0, Stage].ProcessSample32(-TempIn[1]));
   TempIn[1] := FDelayLine[1, Stage].ProcessSample32(FAttenuation * FCrosstalkFilter[1, Stage].ProcessSample32(-TempIn[0]));
   TempIn[0] := Temp;

   Left  := Left  + TempIn[0];
   Right := Right + TempIn[1];
  end;
end;

end.
