unit DAV_DspSoundTouch;

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

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_Classes, DAV_SoundTouchDLL; //DAV_SoundTouch;

type
  TDspSoundTouch = class(TDspSampleRatePersistent)
  private
    FHandle     : TSoundTouchHandle;
    FRate       : Single;
    FPitch      : Single;
    FTempo      : Single;
    FChannels   : Cardinal;
    function GetNumSamples: Cardinal;
    function GetNumUnprocessedSamples: Cardinal;
    function GetIsEmpty: Integer;
    function GetPitchChange: Single;
    function GetRateChange: Single;
    function GetTempoChange: Single;
    procedure SetRate(const Value: Single);
    procedure SetPitch(const Value: Single);
    procedure SetTempo(const Value: Single);
    procedure SetPitchChange(const Value: Single);
    procedure SetRateChange(const Value: Single);
    procedure SetTempoChange(const Value: Single);
    procedure SetChannels(const Value: Cardinal);
  protected
    procedure SampleRateChanged; override;
    procedure ChannelsChanged; virtual;
    procedure PitchChanged; virtual;
    procedure TempoChanged; virtual;
    procedure RateChanged; virtual;
  public
    class function GetVersionString: AnsiString;
    class function GetVersionId: Cardinal;
    constructor Create; override;
    destructor Destroy; override;
    procedure Flush; virtual;
    procedure Clear; virtual;

    procedure WriteSamples(const Data: PDAVSingleFixedArray; const SampleFrames: Cardinal);
    function ReadSamples(const Data: PDAVSingleFixedArray; const SampleFrames: Integer): Cardinal;

    function SetSetting(const SettingId: Integer; const Value: Integer): Boolean;
    function GetSetting(const SettingId: Integer): Integer;

    property VersionString: AnsiString read GetVersionString;
    property VersionID: Cardinal read GetVersionId;
    property Channels: Cardinal read FChannels write SetChannels;
    property Rate: Single read FRate write SetRate;
    property RateChange: Single read GetRateChange write SetRateChange;
    property Tempo: Single read FTempo write SetTempo;
    property TempoChange: Single read GetTempoChange write SetTempoChange;
    property Pitch: Single read FPitch write SetPitch;
    property PitchChange: Single read GetPitchChange write SetPitchChange;

    property NumSamples: Cardinal read GetNumSamples;
    property NumUnprocessedSamples: Cardinal read GetNumUnprocessedSamples;
    property IsEmpty: Integer read GetIsEmpty;
  end;

implementation

uses
  SysUtils;

constructor TDspSoundTouch.Create;
begin
 inherited;
 FHandle := SoundTouchCreateInstance;
 FRate := 1;
 FTempo := 1;
 FPitch := 1;
 FChannels := 1;
 SamplerateChanged;
 ChannelsChanged;
end;

destructor TDspSoundTouch.Destroy;
begin
 SoundTouchDestroyInstance(FHandle);
 inherited;
end;

procedure TDspSoundTouch.Flush;
begin
 SoundTouchFlush(FHandle);
end;

procedure TDspSoundTouch.Clear;
begin
 SoundTouchClear(FHandle);
end;

function TDspSoundTouch.GetIsEmpty: Integer;
begin
 Result := SoundTouchIsEmpty(FHandle);
end;

function TDspSoundTouch.GetNumSamples: Cardinal;
begin
 Result := SoundTouchNumSamples(FHandle);
end;

function TDspSoundTouch.GetNumUnprocessedSamples: Cardinal;
begin
 Result := SoundTouchNumUnprocessedSamples(FHandle);
end;

function TDspSoundTouch.GetPitchChange: Single;
begin
 Result := 100 * (FPitch - 1.0);
end;

function TDspSoundTouch.GetRateChange: Single;
begin
 Result := 100 * (FRate - 1.0);
end;

function TDspSoundTouch.GetTempoChange: Single;
begin
 Result := 100 * (FTempo - 1.0);
end;

class function TDspSoundTouch.GetVersionId: Cardinal;
begin
 Result := SoundTouchGetVersionId;
end;

class function TDspSoundTouch.GetVersionString: AnsiString;
begin
 Result := StrPas(SoundTouchGetVersionString);
end;

procedure TDspSoundTouch.SetChannels(const Value: Cardinal);
begin
 if FChannels <> Value then
  begin
   FChannels := Value;
   ChannelsChanged;
  end;
end;

procedure TDspSoundTouch.ChannelsChanged;
begin
 assert(FChannels in [1, 2]);
 SoundTouchSetChannels(FHandle, FChannels);
end;

procedure TDspSoundTouch.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TDspSoundTouch.PitchChanged;
begin
 SoundTouchSetPitch(FHandle, FPitch);
end;

procedure TDspSoundTouch.WriteSamples(const Data: PDAVSingleFixedArray;
  const SampleFrames: Cardinal);
begin
 SoundTouchPutSamples(FHandle, @Data^[0], SampleFrames);
end;

procedure TDspSoundTouch.RateChanged;
begin
 SoundTouchSetRate(FHandle, FRate);
end;

function TDspSoundTouch.ReadSamples(const Data: PDAVSingleFixedArray;
  const SampleFrames: Integer): Cardinal;
begin
 Result := SoundTouchReceiveSamples(FHandle, @Data^[0], SampleFrames);
end;

procedure TDspSoundTouch.SetPitchChange(const Value: Single);
begin
 Pitch := 1.0 + 0.01 * Value;
end;

procedure TDspSoundTouch.SetRate(const Value: Single);
begin
 if FRate <> Value then
  begin
   FRate := Value;
   RateChanged;
  end;
end;

procedure TDspSoundTouch.SetRateChange(const Value: Single);
begin
 Rate := 1.0 + 0.01 * Value;
end;

procedure TDspSoundTouch.SamplerateChanged;
begin
 Assert(SampleRate > 0);
 SoundTouchsetSampleRate(FHandle, Round(SampleRate));
end;

procedure TDspSoundTouch.SetTempo(const Value: Single);
begin
 if FTempo <> Value then
  begin
   FTempo := Value;
   TempoChanged;
  end;
end;

procedure TDspSoundTouch.SetTempoChange(const Value: Single);
begin
 Tempo := 1.0 + 0.01 * Value;
end;

function TDspSoundTouch.GetSetting(const SettingId: Integer): Integer;
begin
 Result := SoundTouchGetSetting(FHandle, SettingId);
end;

function TDspSoundTouch.SetSetting(const SettingId: Integer;
  const Value: Integer): Boolean;
begin
 Result := SoundTouchSetSetting(FHandle, SettingId, Value);
end;

procedure TDspSoundTouch.TempoChanged;
begin
 SoundTouchsetTempo(FHandle, FTempo);
end;

end.
