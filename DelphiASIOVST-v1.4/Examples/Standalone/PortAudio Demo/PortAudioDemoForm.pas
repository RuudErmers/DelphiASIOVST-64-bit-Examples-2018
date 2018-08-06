unit PortAudioDemoForm;

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
//  The code is part of the Delphi PortAudio & VST Project                         //
//                                                                            //
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde                                                        //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLType, Buttons, {$ELSE} Windows, {$ENDIF} Forms, Classes,
  Controls, StdCtrls, DAV_Complex, DAV_Types, DAV_DspSimpleOscillator,
  DAV_PortAudioHost, DAV_PortAudioTypes;

type
  TFmPortAudio = class(TForm)
    BtStartStop: TButton;
    DriverCombo: TComboBox;
    LbCopyright: TLabel;
    LbDrivername: TLabel;
    LbFreq: TLabel;
    LbVolume: TLabel;
    SbFreq: TScrollBar;
    SbVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    function PortAudioCallback(Sender: TObject; InBuffer,
      OutBuffer: TDAVArrayOfSingleFixedArray; FrameCount: NativeUInt;
      TimeInfo: PPaStreamCallbackTimeInfo; StatusFlags: TPaStreamCallbackFlags): LongInt;
    procedure PortAudioReset(Sender: TObject);
    procedure PortAudioSampleRateChanged(Sender: TObject);
    procedure SbFreqChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
    procedure SetAmplitude(const Value: Double);
  protected
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
  public
    FPortAudio  : TPortAudioHost;
    FOscillator : TSimpleOscillator64;
    FFreq, FAmp : Double;
  published
    property Frequency: Double read FFreq write SetFrequency;
    property Amplitude: Double read FAmp write SetAmplitude;
  end;

var
  FmPortAudio : TFmPortAudio;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles, DAV_Common, DAV_Math;

resourcestring
  RCStrNoPortAudioDriverPresent = 'No PortAudio Driver present! Application Terminated!';

procedure TFmPortAudio.FormCreate(Sender: TObject);
begin
 FPortAudio := TPortAudioHost.Create;
 FPortAudio.OnSampleRateChanged := PortAudioSampleRateChanged;
 FPortAudio.OnStreamCallback := PortAudioCallback;
 DriverCombo.Items := FPortAudio.OutputDeviceList;

 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create(RCStrNoPortAudioDriverPresent);
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'PortAudioDemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'PortAudio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
  finally
   Free;
  end;

 FAmp  :=    1;
 FFreq := 1000;

 FOscillator := TSimpleOscillator64.Create;
 with FOscillator do
  begin
   Frequency := FFreq;
   Amplitude := FAmp;
   SampleRate := FPortAudio.SampleRate;
  end;
end;

procedure TFmPortAudio.DriverComboChange(Sender: TObject);
var
  Channel : Integer;
begin
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   FPortAudio.Close;
   FPortAudio.OutputDevice := Integer(DriverCombo.Items.Objects[DriverCombo.ItemIndex]);
   FPortAudio.InputDevice := -1;
   FPortAudio.Open;

   // store current PortAudio driver index
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'PortAudioDemo.INI') do
    try
     WriteInteger('Audio', 'PortAudio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;

   BtStartStop.Enabled := True;
  end;
end;

procedure TFmPortAudio.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'PortAudioDemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'PortAudio Driver', DriverCombo.ItemIndex);
  finally
   Free;
  end;

 FPortAudio.Active := False;
 FreeAndNil(FOscillator);
 FreeAndNil(FPortAudio);
end;

procedure TFmPortAudio.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = '&Start Audio' then
  begin
   FPortAudio.Start; // Start Audio
   BtStartStop.Caption := '&Stop Audio';
  end
 else
  begin
   FPortAudio.Abort; // Stop Audio
   BtStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFmPortAudio.SbFreqChange(Sender: TObject);
begin
 Frequency := FreqLinearToLog(SbFreq.Position * 0.00001);
end;

procedure TFmPortAudio.SetAmplitude(const Value: Double);
begin
 if FAmp <> Value then
  begin
   FAmp := Value;
   AmplitudeChanged;
  end;
end;

procedure TFmPortAudio.SetFrequency(const Value: Double);
begin
 if FFreq <> Value then
  begin
   FFreq := Value;
   FrequencyChanged;
  end;
end;

procedure TFmPortAudio.AmplitudeChanged;
begin
 FOscillator.Amplitude := FAmp;
 if FAmp = 0
  then LbVolume.Caption := 'Volume: 0 equals -oo dB'
  else LbVolume.Caption := 'Volume: ' +
                           FloatToStrF(FAmp, ffFixed, 2, 2) + ' equals ' +
                           FloatToStrF(Amp_to_dB(FAmp), ffGeneral, 2, 2) + ' dB';
end;

procedure TFmPortAudio.FrequencyChanged;
begin
 FOscillator.Frequency := FFreq;
 LbFreq.Caption := 'Frequency: ' + FloatToStrF(FFreq, ffGeneral, 5, 5) + ' Hz';
end;

procedure TFmPortAudio.SbVolumeChange(Sender: TObject);
begin
 Amplitude := SbVolume.Position * 0.00001;
end;

procedure TFmPortAudio.PortAudioSampleRateChanged(Sender: TObject);
begin
 if Assigned(FOscillator)
  then FOscillator.SampleRate := FPortAudio.SampleRate;
end;

function TFmPortAudio.PortAudioCallback(Sender: TObject; InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray; FrameCount: NativeUInt;
  TimeInfo: PPaStreamCallbackTimeInfo; StatusFlags: TPaStreamCallbackFlags): LongInt;
var
  Sample       : Integer;
  ChannelIndex : Integer;
begin
 for Sample := 0 to FrameCount - 1 do
  begin
   for ChannelIndex := 0 to Length(OutBuffer) - 1
    do OutBuffer[ChannelIndex, Sample] := FOscillator.Sine;
   FOscillator.CalculateNextSample;
  end;
  Result := paContinue;
end;

procedure TFmPortAudio.PortAudioReset(Sender: TObject);
begin
 if BtStartStop.Caption = '&Stop Audio'
  then FPortAudio.Active := True;
end;

end.
