unit MultiSineGeneratorMain;

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
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Sysutils, Controls, StdCtrls, ExtCtrls, DAV_Complex,
  DAV_Types, DAV_ASIOHost, DAV_GuiBaseControl, DAV_GuiLevelMeter,
  DAV_DspSimpleOscillator, DAV_GuiLED;

const
  CDefaultFrequencies : Array [0..30] of Single = (20, 25, 31.5, 40, 50, 63,
    80, 100, 125, 160, 200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600,
    2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000, 12500, 16000, 20000);
type
  TFmASIO = class(TForm)
    ASIOHost: TASIOHost;
    BtAllOctaves: TButton;
    BtAllThirdOctaves: TButton;
    BtControlPanel: TButton;
    BtMute: TButton;
    BtStartStop: TButton;
    CbLinkChannels: TCheckBox;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    Lb0L: TLabel;
    Lb0R: TLabel;
    Lb100: TLabel;
    Lb10kHz: TLabel;
    Lb125: TLabel;
    Lb12AL: TLabel;
    Lb12AR: TLabel;
    Lb12k5Hz: TLabel;
    Lb160: TLabel;
    Lb16kHz: TLabel;
    Lb1k: TLabel;
    Lb1k25: TLabel;
    Lb1k6: TLabel;
    Lb200: TLabel;
    Lb20Hz: TLabel;
    Lb20kHz: TLabel;
    Lb24AL: TLabel;
    Lb24AR: TLabel;
    Lb250: TLabel;
    Lb25Hz: TLabel;
    Lb2k: TLabel;
    Lb2k5: TLabel;
    Lb315: TLabel;
    Lb31Hz: TLabel;
    Lb3k15: TLabel;
    Lb400: TLabel;
    Lb40Hz: TLabel;
    Lb4k: TLabel;
    Lb50: TLabel;
    Lb500: TLabel;
    Lb5k: TLabel;
    Lb63: TLabel;
    Lb630: TLabel;
    Lb6k3Hz: TLabel;
    Lb80: TLabel;
    Lb800: TLabel;
    Lb8kHz: TLabel;
    LbChannels: TLabel;
    LbDrivername: TLabel;
    LbLM: TLabel;
    LbRS: TLabel;
    LedClipL: TGuiLED;
    LedClipR: TGuiLED;
    MeterTimer: TTimer;
    MiddleL: TShape;
    MiddleR: TShape;
    PeakMeterLeft: TGuiColorLevelMeter;
    PeakMeterRight: TGuiColorLevelMeter;
    SB100L: TScrollBar;
    SB100R: TScrollBar;
    SB10kL: TScrollBar;
    SB10kR: TScrollBar;
    SB125L: TScrollBar;
    SB125R: TScrollBar;
    SB12k5L: TScrollBar;
    SB12k5R: TScrollBar;
    SB160L: TScrollBar;
    SB160R: TScrollBar;
    SB16kL: TScrollBar;
    SB16kR: TScrollBar;
    SB1k25L: TScrollBar;
    SB1k25R: TScrollBar;
    SB1k6L: TScrollBar;
    SB1k6R: TScrollBar;
    SB1kL: TScrollBar;
    SB1kR: TScrollBar;
    SB200L: TScrollBar;
    SB200R: TScrollBar;
    SB20kL: TScrollBar;
    SB20kR: TScrollBar;
    SB20L: TScrollBar;
    SB20R: TScrollBar;
    SB250L: TScrollBar;
    SB250R: TScrollBar;
    SB25L: TScrollBar;
    SB25R: TScrollBar;
    SB2k5L: TScrollBar;
    SB2k5R: TScrollBar;
    SB2kL: TScrollBar;
    SB2kR: TScrollBar;
    SB315L: TScrollBar;
    SB315R: TScrollBar;
    SB31L: TScrollBar;
    SB31R: TScrollBar;
    SB3k15L: TScrollBar;
    SB3k15R: TScrollBar;
    SB400L: TScrollBar;
    SB400R: TScrollBar;
    SB40L: TScrollBar;
    SB40R: TScrollBar;
    SB4kL: TScrollBar;
    SB4kR: TScrollBar;
    SB500L: TScrollBar;
    SB500R: TScrollBar;
    SB50L: TScrollBar;
    SB50R: TScrollBar;
    SB5kL: TScrollBar;
    SB5kR: TScrollBar;
    SB630L: TScrollBar;
    SB630R: TScrollBar;
    SB63L: TScrollBar;
    SB63R: TScrollBar;
    SB6k3L: TScrollBar;
    SB6k3R: TScrollBar;
    SB800L: TScrollBar;
    SB800R: TScrollBar;
    SB80L: TScrollBar;
    SB80R: TScrollBar;
    SB8kL: TScrollBar;
    SB8kR: TScrollBar;
    ShBackText: TShape;
    BtExport: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtAllOctavesClick(Sender: TObject);
    procedure BtAllThirdOctavesClick(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtExportClick(Sender: TObject);
    procedure BtMuteClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure LbFrequencyDblClick(Sender: TObject);
    procedure LedClipLClick(Sender: TObject);
    procedure LedClipRClick(Sender: TObject);
    procedure MeterTimerTimer(Sender: TObject);
    procedure SBVolumeChange(Sender: TObject);
  private
    procedure CalculatePeakDecay;
    procedure ExportToFilename(Filename: TFilename);
  public
    FOscillators   : array [0..30] of TSimpleOscillator64;
    FVolume        : array [0..1, 0..30] of Double;
    FPeak          : array [0..1] of Double;
    FPeakDecay      : Double;
    FChannelOffset : Byte;
  published
  end;

var
  FmASIO : TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  Inifiles, Dialogs, DAV_Approximations, DAV_Common, DAV_Math, DAV_AudioFile,
  DAV_AudioData, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU,
  MultiSineGeneratorFrequency;

procedure TFmASIO.FormCreate(Sender: TObject);
var
  BandIndex : Integer;
begin
 DriverCombo.Items := ASIOHost.DriverList;
 LedClipL.Brightness_Percent := 0;
 LedClipR.Brightness_Percent := 0;

 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'MultiSineGenerator.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
   CalculatePeakDecay;
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   FChannelOffset := ChannelBox.ItemIndex * 2;
  finally
   Free;
  end;

 for BandIndex := 0 to Length(CDefaultFrequencies) - 1 do
  begin
   FOscillators[BandIndex] := TSimpleOscillator64.Create;
   with FOscillators[BandIndex] do
    begin
     Frequency := CDefaultFrequencies[BandIndex];
     SampleRate := ASIOHost.SampleRate;
    end;
  end;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
var
  BandIndex : Integer;
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'MultiSineGenerator.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
  finally
   Free;
  end;

 ASIOHost.Active := False;

 for BandIndex := 0 to Length(FOscillators) - 1
  do FreeAndNil(FOscillators[BandIndex]);
end;

procedure TFmASIO.LbFrequencyDblClick(Sender: TObject);
var
  FormatSettings : TFormatSettings;
  ShiftAllFreqs  : Boolean;
  NewFrequency   : Single;
  Ratio          : Single;
  Band           : Integer;
begin
 GetLocaleFormatSettings(1033, FormatSettings);
 with TFmSetFrequency.Create(Self) do
  try
   EdFrequency.Text := FloatToStr(FOscillators[TLabel(Sender).Tag].Frequency);
   ShiftAllFreqs := ssShift in KeyboardStateToShiftState;

   if ShowModal = mrOK then
    try
     NewFrequency := StrToFloat(EdFrequency.Text);
     if ShiftAllFreqs then
      begin
       Ratio := NewFrequency / FOscillators[TLabel(Sender).Tag].Frequency;
       for Band := 0 to Length(FOscillators) - 1 do
        begin
         NewFrequency := FOscillators[Band].Frequency * Ratio;
         FOscillators[Band].Frequency := NewFrequency;
(*
         if NewFrequency > 1000
          then TLabel(Sender).Caption := FloatToStrF(1E-3 * NewFrequency, ffGeneral, 3, 3, FormatSettings) + ' kHz'
          else TLabel(Sender).Caption := FloatToStrF(       NewFrequency, ffGeneral, 3, 3, FormatSettings) + ' Hz';
*)
        end;
      end
     else
      begin
       FOscillators[TLabel(Sender).Tag].Frequency := NewFrequency;
       if NewFrequency > 1000
        then TLabel(Sender).Caption := FloatToStrF(1E-3 * NewFrequency, ffGeneral, 3, 3, FormatSettings) + ' kHz'
        else TLabel(Sender).Caption := FloatToStrF(       NewFrequency, ffGeneral, 3, 3, FormatSettings) + ' Hz';
      end;
    except
     on E: EConvertError do MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  finally
   Free;
  end;
end;

procedure TFmASIO.LedClipLClick(Sender: TObject);
begin
 LedClipL.Brightness_Percent := 0;
end;

procedure TFmASIO.LedClipRClick(Sender: TObject);
begin
 LedClipR.Brightness_Percent := 0;
end;

procedure TFmASIO.MeterTimerTimer(Sender: TObject);
begin
 if FPeak[0] > 1 then LedClipL.Brightness_Percent := 100;
 if FPeak[1] > 1 then LedClipR.Brightness_Percent := 100;

 PeakMeterLeft.PeakLevel := FPeak[0];
 PeakMeterRight.PeakLevel := FPeak[1];
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for ChannelIndex := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
    begin
     ChannelBox.Items.Add(
       string(ASIOHost.OutputChannelInfos[2 * ChannelIndex].Name) + ' / ' +
       string(ASIOHost.OutputChannelInfos[2 * ChannelIndex + 1].Name));
    end;

   // store current ASIO driver index
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'MultiSineGenerator.INI') do
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

procedure TFmASIO.BtAllOctavesClick(Sender: TObject);
begin
 SB20L.Position := 100;
 SB25L.Position := 100;
 SB31L.Position := 85;
 SB40L.Position := 100;
 SB50L.Position := 100;
 SB63L.Position := 85;
 SB80L.Position := 100;
 SB100L.Position := 100;
 SB125L.Position := 85;
 SB160L.Position := 100;
 SB200L.Position := 100;
 SB250L.Position := 85;
 SB315L.Position := 100;
 SB400L.Position := 100;
 SB500L.Position := 85;
 SB630L.Position := 100;
 SB800L.Position := 100;
 SB1KL.Position := 85;
 SB1K25L.Position := 100;
 SB1K6L.Position := 100;
 SB2KL.Position := 85;
 SB2K5L.Position := 100;
 SB3k15L.Position := 100;
 SB4kL.Position := 85;
 SB5kL.Position := 100;
 SB6k3L.Position := 100;
 SB8kL.Position := 85;
 SB10KL.Position := 100;
 SB12K5L.Position := 100;
 SB16KL.Position := 85;
 SB20KL.Position := 100;

 SB20R.Position := 100;
 SB25R.Position := 100;
 SB31R.Position := 85;
 SB40R.Position := 100;
 SB50R.Position := 100;
 SB63R.Position := 85;
 SB80R.Position := 100;
 SB100R.Position := 100;
 SB125R.Position := 85;
 SB160R.Position := 100;
 SB200R.Position := 100;
 SB250R.Position := 85;
 SB315R.Position := 100;
 SB400R.Position := 100;
 SB500R.Position := 85;
 SB630R.Position := 100;
 SB800R.Position := 100;
 SB1KR.Position := 85;
 SB1K25R.Position := 100;
 SB1K6R.Position := 100;
 SB2KR.Position := 85;
 SB2K5R.Position := 100;
 SB3k15R.Position := 100;
 SB4kR.Position := 85;
 SB5kR.Position := 100;
 SB6k3R.Position := 100;
 SB8kR.Position := 85;
 SB10KR.Position := 100;
 SB12K5R.Position := 100;
 SB16KR.Position := 85;
 SB20KR.Position := 100;
end;

procedure TFmASIO.BtAllThirdOctavesClick(Sender: TObject);
begin
 SB20L.Position := 93;
 SB25L.Position := 93;
 SB31L.Position := 93;
 SB40L.Position := 93;
 SB50L.Position := 93;
 SB63L.Position := 93;
 SB80L.Position := 93;
 SB100L.Position := 93;
 SB125L.Position := 93;
 SB160L.Position := 93;
 SB200L.Position := 93;
 SB250L.Position := 93;
 SB315L.Position := 93;
 SB400L.Position := 93;
 SB500L.Position := 93;
 SB630L.Position := 93;
 SB800L.Position := 93;
 SB1KL.Position := 93;
 SB1K25L.Position := 93;
 SB1K6L.Position := 93;
 SB2KL.Position := 93;
 SB2K5L.Position := 93;
 SB3k15L.Position := 93;
 SB4kL.Position := 93;
 SB5kL.Position := 93;
 SB6k3L.Position := 93;
 SB8kL.Position := 93;
 SB10KL.Position := 93;
 SB12K5L.Position := 93;
 SB16KL.Position := 93;
 SB20KL.Position := 93;

 SB20R.Position := 93;
 SB25R.Position := 93;
 SB31R.Position := 93;
 SB40R.Position := 93;
 SB50R.Position := 93;
 SB63R.Position := 93;
 SB80R.Position := 93;
 SB100R.Position := 93;
 SB125R.Position := 93;
 SB160R.Position := 93;
 SB200R.Position := 93;
 SB250R.Position := 93;
 SB315R.Position := 93;
 SB400R.Position := 93;
 SB500R.Position := 93;
 SB630R.Position := 93;
 SB800R.Position := 93;
 SB1KR.Position := 93;
 SB1K25R.Position := 93;
 SB1K6R.Position := 93;
 SB2KR.Position := 93;
 SB2K5R.Position := 93;
 SB3k15R.Position := 93;
 SB4kR.Position := 93;
 SB5kR.Position := 93;
 SB6k3R.Position := 93;
 SB8kR.Position := 93;
 SB10KR.Position := 93;
 SB12K5R.Position := 93;
 SB16KR.Position := 93;
 SB20KR.Position := 93;
end;

procedure TFmASIO.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIO.BtExportClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   Title := 'Export Audio File...';
   Filter := 'Wave (*.wav)|*.wav|AIFF (*.aiff)|*.aiff|AU (*.au)|*.au';
   DefaultExt := '.wav';
   if Execute
    then ExportToFilename(Filename);
  finally
   Free;
  end;
end;

procedure TFmASIO.ExportToFilename(Filename: TFilename);
var
  SampleIndex  : Integer;
  BandIndex    : Integer;
  Data         : Double;
begin
 with TAudioDataCollection32.Create(nil) do
  try
   ChannelCount := 2;
   SampleRate := ASIOHost.SampleRate;
   SampleFrames := Round(20 * SampleRate);

   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     // left channel
     Data := FVolume[0, 0] * FOscillators[0].Sine;
     for BandIndex := 1 to Length(FOscillators) - 1
      do Data := Data + FVolume[0, BandIndex] * FOscillators[BandIndex].Sine;
     ChannelDataPointer[0]^[SampleIndex] := Data;

     // right channel
     Data := FVolume[1, 0] * FOscillators[0].Sine;
     FOscillators[0].CalculateNextSample;
     for BandIndex := 1 to Length(FOscillators) - 1 do
      begin
       Data := Data + FVolume[1, BandIndex] * FOscillators[BandIndex].Sine;
       FOscillators[BandIndex].CalculateNextSample;
      end;
     ChannelDataPointer[1]^[SampleIndex] := Data;
    end;

   SaveToFile(Filename);
  finally
   Free;
  end;
end;

procedure TFmASIO.BtMuteClick(Sender: TObject);
begin
 SB20L.Position := 100;
 SB25L.Position := 100;
 SB31L.Position := 100;
 SB40L.Position := 100;
 SB50L.Position := 100;
 SB63L.Position := 100;
 SB80L.Position := 100;
 SB100L.Position := 100;
 SB125L.Position := 100;
 SB160L.Position := 100;
 SB200L.Position := 100;
 SB250L.Position := 100;
 SB315L.Position := 100;
 SB400L.Position := 100;
 SB500L.Position := 100;
 SB630L.Position := 100;
 SB800L.Position := 100;
 SB1KL.Position := 100;
 SB1K25L.Position := 100;
 SB1K6L.Position := 100;
 SB2KL.Position := 100;
 SB2K5L.Position := 100;
 SB3k15L.Position := 100;
 SB4kL.Position := 100;
 SB5kL.Position := 100;
 SB6k3L.Position := 100;
 SB8kL.Position := 100;
 SB10KL.Position := 100;
 SB12K5L.Position := 100;
 SB16KL.Position := 100;
 SB20KL.Position := 100;
 SB20R.Position := 100;
 SB25R.Position := 100;
 SB31R.Position := 100;
 SB40R.Position := 100;
 SB50R.Position := 100;
 SB63R.Position := 100;
 SB80R.Position := 100;
 SB100R.Position := 100;
 SB125R.Position := 100;
 SB160R.Position := 100;
 SB200R.Position := 100;
 SB250R.Position := 100;
 SB315R.Position := 100;
 SB400R.Position := 100;
 SB500R.Position := 100;
 SB630R.Position := 100;
 SB800R.Position := 100;
 SB1KR.Position := 100;
 SB1K25R.Position := 100;
 SB1K6R.Position := 100;
 SB2KR.Position := 100;
 SB2K5R.Position := 100;
 SB3k15R.Position := 100;
 SB4kR.Position := 100;
 SB5kR.Position := 100;
 SB6k3R.Position := 100;
 SB8kR.Position := 100;
 SB10KR.Position := 100;
 SB12K5R.Position := 100;
 SB16KR.Position := 100;
 SB20KR.Position := 100;
end;

procedure TFmASIO.SBVolumeChange(Sender: TObject);
var
  Channel : Integer;
begin
 if Sender is TScrollBar then
  with TScrollBar(Sender) do
   begin
    Channel := Tag mod 31;
    FVolume[Channel, Tag - 31 * Channel] := 1 - 0.01 * Position;
   end;
end;

procedure TFmASIO.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtStartStop.Caption := 'Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtStartStop.Caption := 'Start Audio';
  end;
end;

procedure TFmASIO.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
var
  BandIndex : Integer;
begin
 for BandIndex := 0 to Length(FOscillators) - 1
  do FOscillators[BandIndex].SampleRate := ASIOHost.SampleRate;
 CalculatePeakDecay
end;

procedure TFmASIO.CalculatePeakDecay;
begin
 // fixed time = 4 second
 FPeakDecay := FastPower2ContinousError3(-ASIOHost.BufferSize / (4 * ASIOHost.SampleRate));
end;

procedure TFmASIO.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  SampleIndex  : Integer;
  BandIndex    : Integer;
  Data         : Double;
begin
 for SampleIndex := 0 to ASIOHost.BufferSize - 1 do
  begin
   // left channel
   Data := FVolume[0, 0] * FOscillators[0].Sine;
   for BandIndex := 1 to Length(FOscillators) - 1
    do Data := Data + FVolume[0, BandIndex] * FOscillators[BandIndex].Sine;
   OutBuffer[FChannelOffset, SampleIndex] := Data;
   if Abs(Data) > FPeak[0]
    then FPeak[0] := Abs(Data);

   // right channel
   Data := FVolume[1, 0] * FOscillators[0].Sine;
   FOscillators[0].CalculateNextSample;
   for BandIndex := 1 to Length(FOscillators) - 1 do
    begin
     Data := Data + FVolume[1, BandIndex] * FOscillators[BandIndex].Sine;
     FOscillators[BandIndex].CalculateNextSample;
    end;
   OutBuffer[FChannelOffset + 1, SampleIndex] := Data;
   if Abs(Data) > FPeak[1]
    then FPeak[1] := Abs(Data);
  end;
 FPeak[0] := FPeakDecay * FPeak[0];
 FPeak[1] := FPeakDecay * FPeak[1];
end;

procedure TFmASIO.ASIOHostBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  SampleIndex  : Integer;
  BandIndex    : Integer;
  Data         : Double;
begin
 for SampleIndex := 0 to ASIOHost.BufferSize - 1 do
  begin
   // left channel
   Data := FVolume[0, 0] * FOscillators[0].Sine;
   for BandIndex := 1 to Length(FOscillators) - 1
    do Data := Data + FVolume[0, BandIndex] * FOscillators[BandIndex].Sine;
   OutBuffer[FChannelOffset, SampleIndex] := Data;
   if Abs(Data) > FPeak[0]
    then FPeak[0] := Abs(Data);

   // right channel
   Data := FVolume[1, 0] * FOscillators[0].Sine;
   FOscillators[0].CalculateNextSample;
   for BandIndex := 1 to Length(FOscillators) - 1 do
    begin
     Data := Data + FVolume[1, BandIndex] * FOscillators[BandIndex].Sine;
     FOscillators[BandIndex].CalculateNextSample;
    end;
   OutBuffer[FChannelOffset + 1, SampleIndex] := Data;
   if Abs(Data) > FPeak[1]
    then FPeak[1] := Abs(Data);
  end;
 FPeak[0] := FPeakDecay * FPeak[0];
 FPeak[1] := FPeakDecay * FPeak[1];
end;

procedure TFmASIO.ASIOHostReset(Sender: TObject);
begin
  ASIOHost.Active := True;
end;

{$IFDEF FPC}
initialization
  {$i MultiSineGeneratorForm.lrs}
{$ENDIF}

end.

