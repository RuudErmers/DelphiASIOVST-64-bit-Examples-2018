unit AsioDemoForm;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.ListBox, DAV_Complex, DAV_Types, DAV_ASIOHost, DAV_DspSimpleOscillator;

type
  TFmASIO = class(TForm)
    AsioHost: TAsioHost;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    LbChannels: TLabel;
    LbCopyright: TLabel;
    LbDrivername: TLabel;
    LbFreq: TLabel;
    LbPanorama: TLabel;
    LbVolume: TLabel;
    SbFreq: TScrollBar;
    SbPan: TScrollBar;
    SbVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure SbFreqChange(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
  private
    procedure SetFrequency(const Value: Double);
    procedure SetAmplitude(const Value: Double);
  protected
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
  public
    FOscillator       : TSimpleOscillator64;
    FPan, FFreq, FAmp : Double;
    FChannelOffset    : Byte;
  published
    property Frequency: Double read FFreq write SetFrequency;
    property Amplitude: Double read FAmp write SetAmplitude;
  end;

var
  FmASIO : TFmASIO;

implementation

{$R *.FMX}

uses
  Inifiles, DAV_Common, DAV_Math;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFmASIO.FormCreate(Sender: TObject);
begin
 DriverCombo.Items := ASIOHost.DriverList;

 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create(RCStrNoASIODriverPresent);
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   FChannelOffset := ChannelBox.ItemIndex * 2;
  finally
   Free;
  end;

 FAmp           :=    1;
 FFreq          := 1000;
 FPan           :=    0.5;
 FChannelOffset :=    0;

 FOscillator := TSimpleOscillator64.Create;
 with FOscillator do
  begin
   Frequency := FFreq;
   Amplitude := FAmp;
   SampleRate := ASIOHost.SampleRate;
  end;
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
var
  Channel : Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;

 // DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);

 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for Channel := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
    begin
     ChannelBox.Items.Add(
       string(ASIOHost.OutputChannelInfos[2 * Channel].Name) + ' / ' +
       string(ASIOHost.OutputChannelInfos[2 * Channel + 1].Name));
    end;

   // store current ASIO driver index
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := True;
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmASIO.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.Active := False;
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
  finally
   Free;
  end;

 AsioHost.Active := False; 
 FreeAndNil(FOscillator);  
end;

procedure TFmASIO.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Text = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtStartStop.Text := 'Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtStartStop.Text := 'Start Audio';
  end;
end;

procedure TFmASIO.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmASIO.SbFreqChange(Sender: TObject);
begin
 Frequency := FreqLinearToLog(SbFreq.Value * 0.00001);
end;

procedure TFmASIO.SetAmplitude(const Value: Double);
begin
 if FAmp <> Value then
  begin
   FAmp := Value;
   AmplitudeChanged;
  end;
end;

procedure TFmASIO.SetFrequency(const Value: Double);
begin
 if FFreq <> Value then
  begin
   FFreq := Value;
   FrequencyChanged;
  end;
end;

procedure TFmASIO.AmplitudeChanged;
begin
 FOscillator.Amplitude := FAmp;
 if FAmp = 0
  then LbVolume.Text := 'Volume: 0 equals -oo dB'
  else LbVolume.Text := 'Volume: ' +
                           FloatToStrF(FAmp, ffFixed, 2, 2) + ' equals ' +
                           FloatToStrF(Amp_to_dB(FAmp), ffGeneral, 2, 2) + ' dB';
end;

procedure TFmASIO.FrequencyChanged;
begin
 FOscillator.Frequency := FFreq;
 LbFreq.Text := 'Frequency: ' + FloatToStrF(FFreq, ffGeneral, 5, 5) + ' Hz';
end;

procedure TFmASIO.SbVolumeChange(Sender: TObject);
begin
 Amplitude := SbVolume.Value * 0.00001;
end;

procedure TFmASIO.SbPanChange(Sender: TObject);
begin
 FPan := SbPan.Value * 0.01;
 if FPan = 0.5
  then LbPanorama.Text := 'Panorama: C'
  else LbPanorama.Text := 'Panorama: ' + Inttostr(round(100 * (FPan * 2 - 1)));
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if Assigned(FOscillator)
  then FOscillator.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample : Integer;
  L, R   : Integer;
begin
 L := FChannelOffset;
 R := L + 1;
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   OutBuffer[L, Sample] := (1 - FPan) * FOscillator.Sine;
   OutBuffer[R, Sample] := FPan * FOscillator.Sine;
   FOscillator.CalculateNextSample;
  end;
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  Sample : Integer;
  L, R   : Integer;
begin
 L := FChannelOffset;
 R := L + 1;
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   OutBuffer[L, Sample] := (1 - FPan) * FOscillator.Sine;
   OutBuffer[R, Sample] := FPan * FOscillator.Sine;
   FOscillator.CalculateNextSample;
  end;
end;

procedure TFmASIO.ASIOHostReset(Sender: TObject);
begin
 if BtStartStop.Text = 'Stop Audio'
  then ASIOHost.Active := True;
end;

end.
