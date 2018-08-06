unit GenMain;

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
  {$IFDEF FPC}LCLIntf, LMessages, {$ELSE}Windows, Messages, {$ENDIF} SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Spin, ComCtrls,
  ExtCtrls, Buttons, SyncObjs, DAV_Types, DAV_Classes, DAV_GuiBaseControl,
  DAV_GuiAudioDataDisplay, DAV_GuiLED, DAV_ASIOHost, DAV_DspPinkNoiseGenerator,
  DAV_DspSimpleOscillator, DAV_DspSweepOscillator, DAV_AudioData, DAV_AudioFile,
  DAV_AudioFileWav, DAV_AudioFileAiff, DAV_AudioFileAu,
  DAV_DspBufferedAudioFilePlayer;

type
  TFadeDirection = (fdNone, fdUp, fdDown);
  TFmGenerator = class(TForm)
    AdTimeDomain: TGuiAudioDataDisplay;
    ASIOHost: TASIOHost;
    BtPause: TSpeedButton;
    BtPlay: TSpeedButton;
    BtSelectWavFile: TButton;
    BtStop: TSpeedButton;
    CbChannelSine: TComboBox;
    CbChannelSweep: TComboBox;
    CbIdenticalChannelsSine: TCheckBox;
    CbIdenticalChannelsSweep: TCheckBox;
    CbSine: TComboBox;
    CbTimeLimit: TCheckBox;
    DriverCombo: TComboBox;
    EdEndFreq: TEdit;
    EdFrequency: TEdit;
    EdGainSweep: TEdit;
    EdInitialPhase: TEdit;
    EdSineGain: TEdit;
    EdStartFreq: TEdit;
    EdSweepModulationTime: TEdit;
    EdWavFile: TEdit;
    GbParameterSine: TGroupBox;
    GbSweep: TGroupBox;
    LED6: TGuiLED;
    LED5: TGuiLED;
    LED4: TGuiLED;
    LED3: TGuiLED;
    LED2: TGuiLED;
    LED1: TGuiLED;
    LbChannelCount: TLabel;
    LbDistribution: TLabel;
    LbEndFreq: TLabel;
    LbFrequency: TLabel;
    LbGain: TLabel;
    LbGainSweep: TLabel;
    LbInitialPhase: TLabel;
    LbM30: TLabel;
    LbModulationTime: TLabel;
    LbNoChannelSine: TLabel;
    LbNoChannelSweep: TLabel;
    LbP30: TLabel;
    LbPinkNoiseGain: TLabel;
    LbSineCount: TLabel;
    LbSineNo: TLabel;
    LbStartFreq: TLabel;
    LbTime: TLabel;
    LbVolume: TLabel;
    LbWhiteNoiseGain: TLabel;
    LbZero: TLabel;
    OpenWavDialog: TOpenDialog;
    PcSelect: TPageControl;
    PnTime: TPanel;
    PnTimeDomain: TPanel;
    RbFallingSweep: TRadioButton;
    RbRectangle: TRadioButton;
    RbRisingSweep: TRadioButton;
    RbTriangle: TRadioButton;
    SeChannelCount: TSpinEdit;
    SePinkNoiseGain: TSpinEdit;
    SeSineCount: TSpinEdit;
    SeWhiteNoiseGain: TSpinEdit;
    TbVolume: TTrackBar;
    TsPinkNoise: TTabSheet;
    TsSine: TTabSheet;
    TsSweep: TTabSheet;
    TsWavFile: TTabSheet;
    TsWhiteNoise: TTabSheet;
    GuiTimer: TTimer;
    ADC: TAudioDataCollection32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BufferSwitchWhiteNoise32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchPinkNoise32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchSine32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchSweep32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BufferSwitchWave32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure DriverComboChange(Sender: TObject);
    procedure PcSelectChange(Sender: TObject);
    procedure SeChannelCountChange(Sender: TObject);
    procedure TbVolumeChange(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure SeWhiteNoiseGainChange(Sender: TObject);
    procedure SePinkNoiseGainChange(Sender: TObject);
    procedure BtSelectWavFileClick(Sender: TObject);
    procedure BtPlayClick(Sender: TObject);
    procedure BtStopClick(Sender: TObject);
    procedure BtPauseClick(Sender: TObject);
    procedure GuiTimerTimer(Sender: TObject);
    procedure CbIdenticalChannelsSineClick(Sender: TObject);
    procedure SeSineCountChange(Sender: TObject);
    procedure CbSineChannelChange(Sender: TObject);
    procedure EdFrequencyChange(Sender: TObject);
    procedure EdSineGainChange(Sender: TObject);
    procedure EdInitialPhaseChange(Sender: TObject);
    procedure CbIdenticalChannelsSweepClick(Sender: TObject);
    procedure EdWavFileChange(Sender: TObject);
    procedure EdSweepModulationTimeChange(Sender: TObject);
    procedure CbChannelSweepChange(Sender: TObject);
    procedure EdStartFreqChange(Sender: TObject);
    procedure EdEndFreqChange(Sender: TObject);
    procedure EdGainSweepChange(Sender: TObject);
    procedure RbRisingSweepClick(Sender: TObject);
    procedure RbFallingSweepClick(Sender: TObject);
  private
    FIniFile         : TFileName;
    FCriticalSection : TCriticalSection;
    FMainGain        : Double;
    FFadeGain        : Double;
    FFadeFactor      : Double;
    FFadeDirection   : TFadeDirection;
    FPinkNoise       : array of TPinkNoiseGenerator;
    FSineOsc         : array of array of TSimpleOscillator;
    FSweepOsc        : array of TRangeSweepOscillator64;
    FAudioFile       : TBufferedAudioFilePlayer;
    FChannelCount    : Integer;
    FSineCount       : Integer;
    FSweepCount      : Integer;
    FWhiteNoiseGain  : Double;
    FPinkNoiseGain   : Double;
    FTime            : Double;
    FPeak            : Double;
    procedure SetChannelCount(const Value: Integer);
    procedure SetSineCount(const Value: Integer);
    function GetCurrentGain: Single;
  protected
    procedure ChannelCountChanged; virtual;
    procedure SineCountChanged; virtual;
    procedure UpdateTime;
    procedure CalculatePeak(Data: Single);
  public
    property ChannelCount: Integer read FChannelCount write SetChannelCount;
    property SineCount: Integer read FSineCount write SetSineCount;
    property CurrentGain: Single read GetCurrentGain;
  end;

var
  FmGenerator: TFmGenerator;

implementation

uses
  Math, IniFiles, DAV_Common;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmGenerator.FormCreate(Sender: TObject);
begin
 FIniFile         := ExtractFilePath(ParamStr(0)) + 'Generator.ini';
 FCriticalSection := TCriticalSection.Create;
 FSineCount       := 1;
 FChannelCount    := 1;
 FWhiteNoiseGain  := 1;
 FPinkNoiseGain   := 1;
 FFadeGain        := 1;
 FFadeFactor      := 1.001;
 FFadeDirection   := fdUp;

 ChannelCountChanged;
 TbVolumeChange(Sender);

 FAudioFile := TBufferedAudioFilePlayer.Create;
 with FAudioFile do
  begin
   SampleRate := ASIOHost.SampleRate;
   Interpolation := biLinear;
  end;

 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);

   EdWavFile.Text := ReadString('Recent', 'Audio File', EdWavFile.Text);
  finally
   Free;
  end;
end;

procedure TFmGenerator.FormDestroy(Sender: TObject);
var
  Channel : Integer;
  Band    : Integer;
begin
 // stop audio processing
 ASIOHost.Active  := False;
 GuiTimer.Enabled := False;

 with TIniFile.Create(FIniFile) do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
   WriteString('Recent', 'Audio File', EdWavFile.Text);
  finally
   Free;
  end;

 FreeAndNil(FAudioFile);

 for Channel := 0 to Length(FPinkNoise) - 1
  do FreeAndNil(FPinkNoise[Channel]);
 for Channel := 0 to Length(FSineOsc) - 1 do
  for Band := 0 to Length(FSineOsc[Channel]) - 1
   do FreeAndNil(FSineOsc[Channel, Band]);
 for Channel := 0 to Length(FSweepOsc) - 1
  do FreeAndNil(FSweepOsc[Channel]);
 FreeAndNil(FCriticalSection);
end;

procedure TFmGenerator.DriverComboChange(Sender: TObject);
begin
 BtPlay.Enabled := False;
 BtStop.Enabled := False;
 BtPause.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   with TIniFile.Create(FIniFile) do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   BtPlay.Enabled := True;
   BtPause.Enabled := True;
   BtStop.Enabled := True;
  end;
end;

procedure TFmGenerator.EdEndFreqChange(Sender: TObject);
var
  Channel : Integer;
  Freq    : Single;
begin
 try
  Freq := StrToFloat(EdEndFreq.Text);
 except
  Exit;
 end;

 if CbIdenticalChannelsSweep.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSweepOsc[Channel].StopFrequency := Freq
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSweepOsc[Channel].StopFrequency := Freq;
  end;
end;

procedure TFmGenerator.EdFrequencyChange(Sender: TObject);
var
  SineNo  : Integer;
  Channel : Integer;
  Freq    : Single;
begin
 try
  Freq := StrToFloat(EdFrequency.Text);
 except
  Exit;
 end;

 SineNo := CbSine.ItemIndex;
 if CbIdenticalChannelsSine.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSineOsc[Channel, SineNo].Frequency := Freq
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSineOsc[Channel, SineNo].Frequency := Freq;
  end;
end;

procedure TFmGenerator.EdGainSweepChange(Sender: TObject);
var
  Channel : Integer;
  Amp    : Single;
begin
 try
  Amp := db_to_Amp(StrToFloat(EdGainSweep.Text));
 except
  Exit;
 end;

 if CbIdenticalChannelsSweep.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSweepOsc[Channel].Amplitude := Amp
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSweepOsc[Channel].Amplitude := Amp;
  end;
end;

procedure TFmGenerator.EdInitialPhaseChange(Sender: TObject);
var
  SineNo  : Integer;
  Channel : Integer;
  Phase   : Single;
begin
 try
  Phase := StrToFloat(EdInitialPhase.Text);
 except
  Exit;
 end;

 SineNo := CbSine.ItemIndex;
 if CbIdenticalChannelsSine.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSineOsc[Channel, SineNo].Phase := Phase
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSineOsc[Channel, SineNo].Phase := Phase;
  end;
end;

procedure TFmGenerator.EdSineGainChange(Sender: TObject);
var
  SineNo    : Integer;
  Channel   : Integer;
  Amplitude : Single;
begin
 try
  Amplitude := dB_to_Amp(StrToFloat(EdSineGain.Text));
 except
  Exit;
 end;

 SineNo := CbSine.ItemIndex;
 if CbIdenticalChannelsSine.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSineOsc[Channel, SineNo].Amplitude := Amplitude
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSineOsc[Channel, SineNo].Amplitude := Amplitude;
  end;
end;

procedure TFmGenerator.EdStartFreqChange(Sender: TObject);
var
  Channel : Integer;
  Freq    : Single;
begin
 try
  Freq := StrToFloat(EdStartFreq.Text);
 except
  Exit;
 end;

 if CbIdenticalChannelsSweep.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSweepOsc[Channel].StartFrequency := Freq
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSweepOsc[Channel].StartFrequency := Freq;
  end;
end;

procedure TFmGenerator.EdSweepModulationTimeChange(Sender: TObject);
var
  Channel : Integer;
  ModTime : Single;
begin
 try
  ModTime := 0.001 * StrToFloat(EdSweepModulationTime.Text);
 except
  Exit;
 end;

 for Channel := 0 to FChannelCount - 1
  do FSweepOsc[Channel].ModulationFrequency := 1 / ModTime;
end;

procedure TFmGenerator.EdWavFileChange(Sender: TObject);
begin
 if FileExists(EdWavFile.Text)
  then FAudioFile.Filename := EdWavFile.Text
end;

procedure TFmGenerator.PcSelectChange(Sender: TObject);
begin
 case PcSelect.ActivePageIndex of
  0 : ASIOHost.OnBufferSwitch32 := BufferSwitchWhiteNoise32;
  1 : ASIOHost.OnBufferSwitch32 := BufferSwitchPinkNoise32;
  2 : ASIOHost.OnBufferSwitch32 := BufferSwitchSine32;
  3 : ASIOHost.OnBufferSwitch32 := BufferSwitchSweep32;
  4 : ASIOHost.OnBufferSwitch32 := BufferSwitchWave32;
 end;
end;

procedure TFmGenerator.RbFallingSweepClick(Sender: TObject);
var
  Channel : Integer;
  Freq    : Single;
begin
(*
 if CbIdenticalChannelsSweep.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSweepOsc[Channel].S
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSweepOsc[Channel].StartFrequency := Freq;
  end;
*)
end;

procedure TFmGenerator.RbRisingSweepClick(Sender: TObject);
var
  Channel : Integer;
  Freq    : Single;
begin
(*
 try
  Freq := StrToFloat(EdStartFreq.Text);
 except
  Exit;
 end;

 if CbIdenticalChannelsSweep.Checked then
  for Channel := 0 to FChannelCount - 1
   do FSweepOsc[Channel].StartFrequency := Freq
 else
  begin
   Channel := CbChannelSine.ItemIndex;
   FSweepOsc[Channel].StartFrequency := Freq;
  end;
*)
end;

procedure TFmGenerator.SeChannelCountChange(Sender: TObject);
begin
 ChannelCount := SeChannelCount.Value;
end;

procedure TFmGenerator.SePinkNoiseGainChange(Sender: TObject);
begin
 FPinkNoiseGain := dB_to_Amp(SePinkNoiseGain.Value);
end;

procedure TFmGenerator.SeSineCountChange(Sender: TObject);
begin
 SineCount := SeSineCount.Value;
 CbSineChannelChange(Sender);
end;

procedure TFmGenerator.SetChannelCount(const Value: Integer);
begin
 if FChannelCount <> Value then
  begin
   FChannelCount := Value;
   ChannelCountChanged;
  end;
end;

procedure TFmGenerator.SetSineCount(const Value: Integer);
begin
 if FSineCount <> Value then
  begin
   FSineCount := Value;
   SineCountChanged;
  end;
end;

procedure TFmGenerator.SineCountChanged;
var
  Channel : Integer;
  Band    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to FChannelCount - 1 do
   begin
    for Band := FSineCount to Length(FSineOsc[Channel]) - 1
     do FreeAndNil(FSineOsc[Channel, Band]);

    // Band oscillator
    SetLength(FSineOsc[Channel], FSineCount);
    for Band := 1 to FSineCount - 1 do
     if not assigned(FSineOsc[Channel, Band]) then
      begin
       FSineOsc[Channel, Band] := TSimpleOscillator.Create;
       FSineOsc[Channel, Band].Assign(FSineOsc[Channel, 0]);
      end;
   end;

  CbSine.Visible := FSineCount > 1;
  LbSineNo.Visible := CbSine.Visible;
  if CbSine.Visible then
   begin
    Band := CbSine.ItemIndex;
    CbSine.Clear;
    for Channel := 1 to FSineCount
     do CbSine.Items.Add(IntToStr(Channel));
    CbSine.ItemIndex := Band;
   end else CbSine.ItemIndex := 0;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TFmGenerator.SeWhiteNoiseGainChange(Sender: TObject);
begin
 FWhiteNoiseGain := dB_to_Amp(SeWhiteNoiseGain.Value);
end;

procedure TFmGenerator.BtPauseClick(Sender: TObject);
begin
 FFadeDirection := fdDown;
end;

procedure TFmGenerator.BtPlayClick(Sender: TObject);
begin
 FFadeGain := dB_to_Amp(-90);
 FFadeDirection := fdUp;
 ASIOHost.Active  := True;
 GuiTimer.Enabled := True;
end;

procedure TFmGenerator.BtStopClick(Sender: TObject);
begin
 BtPauseClick(Sender);
 FTime := 0;
 LbTime.Caption := '0:00:00:000';
end;

procedure TFmGenerator.BtSelectWavFileClick(Sender: TObject);
begin
 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   OpenWavDialog.InitialDir := ReadString('Recent', 'Audio File Directory', OpenWavDialog.InitialDir);
   if OpenWavDialog.Execute then
    begin
     EdWavFile.Text := OpenWavDialog.FileName;
     WriteString('Recent', 'Audio File Directory', ExtractFilePath(OpenWavDialog.FileName));
    end;
  finally
   Free;
  end;
end;

function TFmGenerator.GetCurrentGain: Single;
begin
 Result := FMainGain * FFadeGain;
 case FFadeDirection of
    fdUp : begin
            FFadeGain := FFadeGain * FFadeFactor;
            if FFadeGain > 1 then
             begin
              FFadeGain := 1;
              FFadeDirection := fdNone;
             end;
           end;
  fdDown : begin
            FFadeGain := FFadeGain / FFadeFactor;
            if FFadeGain < 1E-10 then FFadeGain := 0;
           end;
 end;
end;

procedure TFmGenerator.GuiTimerTimer(Sender: TObject);
var
  NewTime  : Double;
  Mil, Sec : Integer;
  Min, Hr  : Integer;
  PeakdB   : Double;
begin
 Hr := Trunc(FTime / 3600);
 NewTime := FTime - Hr * 3600;
 Min := Trunc(NewTime / 60);
 NewTime := NewTime - Min * 60;
 Sec := Trunc(NewTime);
 NewTime := NewTime - Sec;
 Mil := trunc(1000 * NewTime);

 // hour
 LbTime.Caption := IntToStr(Hr) + ':';

 // minute
 if Min < 10
  then LbTime.Caption := LbTime.Caption + '0';
 LbTime.Caption := LbTime.Caption + IntToStr(Min) + ':';

 // seconds
 if Sec < 10
  then LbTime.Caption := LbTime.Caption + '0';
 LbTime.Caption := LbTime.Caption + IntToStr(Sec) + ':';

 // milli seconds
 if Mil < 100
  then LbTime.Caption := LbTime.Caption + '0';
 if Mil < 10
  then LbTime.Caption := LbTime.Caption + '0';
 LbTime.Caption := LbTime.Caption + IntToStr(Mil);

 // display peak
 PeakdB := Amp_to_dB(1E-10 + FPeak);
 if PeakdB > -25
  then LED1.Brightness_Percent := 90
  else LED1.Brightness_Percent := 10;
 if PeakdB > -15
  then LED2.Brightness_Percent := 90
  else LED2.Brightness_Percent := 10;
 if PeakdB > -5
  then LED3.Brightness_Percent := 90
  else LED3.Brightness_Percent := 10;
 if PeakdB > 5
  then LED4.Brightness_Percent := 90
  else LED4.Brightness_Percent := 10;
 if PeakdB > 15
  then LED5.Brightness_Percent := 90
  else LED5.Brightness_Percent := 10;
 if PeakdB > 25
  then LED6.Brightness_Percent := 90
  else LED6.Brightness_Percent := 10;

 // eventually stop audio processing
 if FFadeGain = 0 then
  begin
   ASIOHost.Active  := False;
   GuiTimer.Enabled := False;
   FAudioFile.Reset;
  end;

 AdTimeDomain.Refresh; 
end;

procedure TFmGenerator.TbVolumeChange(Sender: TObject);
begin
 FMainGain := dB_to_Amp(-0.1 * TbVolume.Position);
end;

procedure TFmGenerator.UpdateTime;
begin
 if FFadeDirection = fdNone
  then FTime := FTime + ASIOHost.BufferSize / ASIOHost.SampleRate;
end;

procedure TFmGenerator.CalculatePeak(Data: Single);
begin
 if abs(Data) > FPeak then FPeak := abs(Data);
 FPeak := 0.9999 * FPeak;
end;

procedure TFmGenerator.CbChannelSweepChange(Sender: TObject);
var
  Channel   : Integer;
begin
 Channel := CbChannelSine.ItemIndex;

 EdSweepModulationTime.Text := FloatToStrF(FSweepOsc[Channel].ModulationFrequency, ffGeneral, 5, 5);
end;

procedure TFmGenerator.CbIdenticalChannelsSineClick(Sender: TObject);
begin
 CbChannelSine.Visible := (not CbIdenticalChannelsSine.Checked) and
   (SeChannelCount.Value > 0);
 LbNoChannelSine.Visible := CbChannelSine.Visible;
end;

procedure TFmGenerator.CbIdenticalChannelsSweepClick(Sender: TObject);
begin
 CbChannelSweep.Visible := (not CbIdenticalChannelsSweep.Checked) and
   (SeChannelCount.Value > 0);
 LbNoChannelSweep.Visible := CbChannelSweep.Visible;
end;

procedure TFmGenerator.CbSineChannelChange(Sender: TObject);
var
  SineNo    : Integer;
  Channel   : Integer;
begin
 SineNo := CbSine.ItemIndex;
 Channel := CbChannelSine.ItemIndex;

 EdSineGain.Text := FloatToStrF(Amp_to_dB(FSineOsc[Channel, SineNo].Amplitude), ffGeneral, 3, 3);
 EdFrequency.Text := FloatToStrF(FSineOsc[Channel, SineNo].Frequency, ffGeneral, 5, 5);
 EdInitialPhase.Text := '0';
end;

procedure TFmGenerator.ChannelCountChanged;
var
  Channel : Integer;
  Band    : Integer;
begin
 FCriticalSection.Enter;
 try
  // pink noise
  for Channel := FChannelCount to Length(FPinkNoise) - 1
   do FreeAndNil(FPinkNoise[Channel]);

  // sine oscillator
  for Channel := FChannelCount to Length(FSineOsc) - 1 do
   for Band := 0 to FSineCount - 1
    do FreeAndNil(FSineOsc[Channel, Band]);

  // sweep oscillator
  for Channel := FChannelCount to Length(FSweepOsc) - 1
   do FreeAndNil(FSweepOsc[Channel]);

  // pink noise
  SetLength(FPinkNoise, FChannelCount);
  for Channel := 0 to FChannelCount - 1 do
   if not assigned(FPinkNoise[Channel])
    then FPinkNoise[Channel] := TPinkNoiseGenerator.Create;

  // Band oscillator
  SetLength(FSineOsc, FChannelCount, FSineCount);
  for Channel := 0 to FChannelCount - 1 do
   for Band := 0 to FSineCount - 1 do
    if not assigned(FSineOsc[Channel, Band]) then
     begin
      FSineOsc[Channel, Band] := TSimpleOscillator.Create;
      FSineOsc[Channel, Band].SampleRate := ASIOHost.SampleRate;
      FSineOsc[Channel, Band].Frequency := 1000;
     end;

  // sweep oscillator
  SetLength(FSweepOsc, FChannelCount);
  for Channel := 0 to FChannelCount - 1 do
   if not assigned(FSweepOsc[Channel]) then
    begin
     FSweepOsc[Channel] := TRangeSweepOscillator64.Create;
     FSweepOsc[Channel].SampleRate := ASIOHost.SampleRate;
     FSweepOsc[Channel].StartFrequency := 20;
     FSweepOsc[Channel].StopFrequency := 20000;
     FSweepOsc[Channel].ModulationFrequency := 1;
    end;
 finally
  FCriticalSection.Leave;
 end;

 // update controls 
 CbIdenticalChannelsSine.Visible := SeChannelCount.Value > 1;
 CbChannelSine.Visible := CbIdenticalChannelsSine.Visible and
   (not CbIdenticalChannelsSine.Checked);
 LbNoChannelSine.Visible := CbChannelSine.Visible;

 CbIdenticalChannelsSweep.Visible := SeChannelCount.Value > 1;
 CbChannelSweep.Visible := CbIdenticalChannelsSweep.Visible and
   (not CbIdenticalChannelsSweep.Checked);
 LbNoChannelSweep.Visible := CbChannelSweep.Visible;
end;

procedure TFmGenerator.ASIOHostSampleRateChanged(Sender: TObject);
var
  Channel, Band : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSineOsc) - 1 do
   for Band := 0 to FSineCount - 1
    do FSineOsc[Channel, Band].SampleRate := ASIOHost.SampleRate;
  for Channel := 0 to Length(FSweepOsc) - 1
   do FSweepOsc[Channel].SampleRate := ASIOHost.SampleRate;
  if assigned(FAudioFile)
   then FAudioFile.SampleRate := ASIOHost.SampleRate;
 finally
  FCriticalSection.Leave;
 end;
end;

////////////////
// processing //
////////////////

procedure TFmGenerator.BufferSwitchWhiteNoise32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
  Gain    : Double;
begin
 FCriticalSection.Enter;
 try
  if RbRectangle.Checked then
   for Sample := 0 to ASIOHost.BufferSize - 1 do
    begin
     Gain := CurrentGain;

     for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
      begin
       OutBuffer[Channel, Sample] := Gain * (2 * random - 1);
       CalculatePeak(Gain * OutBuffer[Channel, Sample]);
      end;
    end
  else
   for Sample := 0 to ASIOHost.BufferSize - 1 do
    begin
     Gain := CurrentGain;

     for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
      begin
       OutBuffer[Channel, Sample] := Gain * (random - random);
       CalculatePeak(Gain * OutBuffer[Channel, Sample]);
      end;
    end;

   // update waveform
   ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
   ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
   for Channel := 0 to ADC.ChannelCount - 1
    do Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0], ADC.SampleFrames * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;

 // update time information
 UpdateTime;
end;

procedure TFmGenerator.BufferSwitchPinkNoise32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
  Data    : array of Double;
  Gain    : Double;
begin
 FCriticalSection.Enter;
 try
  SetLength(Data, FChannelCount);
  for Sample := 0 to ASIOHost.BufferSize - 1 do
   begin
    // calculate data
    for Channel := 0 to FChannelCount - 1
     do Data[Channel] := FPinkNoise[Channel mod FChannelCount].ProcessSample64;

    Gain := CurrentGain;

    // calculate peak
    for Channel := 0 to FChannelCount - 11
     do CalculatePeak(Gain * Data[Channel]);

    // distribute data
    if CbIdenticalChannelsSine.Checked then
     for Channel := 0 to ASIOHost.OutputChannelCount - 1
      do OutBuffer[Channel, Sample] := Gain * Data[0]
    else
     for Channel := 0 to ASIOHost.OutputChannelCount - 1
      do OutBuffer[Channel, Sample] := Gain * Data[Channel mod FChannelCount]
   end;

   // update waveform
   ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
   ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
   for Channel := 0 to ADC.ChannelCount - 1
    do Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0], ADC.SampleFrames * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;

 // update time information
 UpdateTime;
end;

procedure TFmGenerator.BufferSwitchSine32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
  Band    : Integer;
  Data    : array of Double;
  Gain    : Double;
begin
 FCriticalSection.Enter;
 try
  SetLength(Data, FChannelCount);
  for Sample := 0 to ASIOHost.BufferSize - 1 do
   begin
    // calculate data
    for Channel := 0 to FChannelCount - 1 do
     begin
      Data[Channel] := 0;
      for Band := 0 to FSineCount - 1 do
       begin
        Data[Channel] := Data[Channel] + FSineOsc[Channel, Band].Sine;
        FSineOsc[Channel, Band].CalculateNextSample;
       end;
     end;

    Gain := CurrentGain;

    // calculate peak
    for Channel := 0 to FChannelCount - 11
     do CalculatePeak(Gain * Data[Channel]);

    // distribute data
    if CbIdenticalChannelsSine.Checked then
     for Channel := 0 to ASIOHost.OutputChannelCount - 1
      do OutBuffer[Channel, Sample] := Gain * Data[0]
    else
     for Channel := 0 to ASIOHost.OutputChannelCount - 1
      do OutBuffer[Channel, Sample] := Gain * Data[Channel mod FChannelCount]
   end;

   // update waveform
   ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
   ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
   for Channel := 0 to ADC.ChannelCount - 1
    do Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0], ADC.SampleFrames * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;

 // update time information
 UpdateTime;
end;

procedure TFmGenerator.BufferSwitchSweep32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
  Data    : array of Double;
  Gain    : Double;
begin
 FCriticalSection.Enter;
 try
  SetLength(Data, FChannelCount);
  for Sample := 0 to ASIOHost.BufferSize - 1 do
   begin
    // calculate data
    for Channel := 0 to FChannelCount - 1 do
     begin
      Data[Channel] := FSweepOsc[Channel].Sine;
      FSweepOsc[Channel].CalculateNextSample;
     end;

    Gain := CurrentGain;

    // calculate peak
    for Channel := 0 to FChannelCount - 11
     do CalculatePeak(Gain * Data[Channel]);

    // distribute data
    if CbIdenticalChannelsSine.Checked then
     for Channel := 0 to ASIOHost.OutputChannelCount - 1
      do OutBuffer[Channel, Sample] := Gain * Data[0]
    else
     for Channel := 0 to ASIOHost.OutputChannelCount - 1
      do OutBuffer[Channel, Sample] := Gain * Data[Channel mod FChannelCount]
   end;

   // update waveform
   ADC.ChannelCount := Min(FChannelCount, ASIOHost.OutputChannelCount);
   ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
   for Channel := 0 to ADC.ChannelCount - 1
    do Move(OutBuffer[Channel, 0], ADC[Channel].ChannelDataPointer^[0], ADC.SampleFrames * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;

 // update time information
 UpdateTime;
end;

procedure TFmGenerator.BufferSwitchWave32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Sample  : Integer;
  Gain    : Double;
begin
 FCriticalSection.Enter;
 try
  FAudioFile.GetSamples(OutBuffer, ASIOHost.BufferSize);

  for Sample := 0 to ASIOHost.BufferSize - 1 do
   begin

    Gain := CurrentGain;

    // calculate peak
    for Channel := 0 to 1
     do CalculatePeak(Gain * OutBuffer[Channel, Sample]);

    for Channel := ASIOHost.OutputChannelCount - 1 downto 0
     do OutBuffer[Channel, Sample] := Gain * OutBuffer[Channel mod 2, Sample];
   end;

   // update waveform
   ADC.ChannelCount := 2;
   ADC.SampleFrames := Max(256, ASIOHost.BufferSize);
   Move(OutBuffer[0, 0], ADC[0].ChannelDataPointer^[0], ADC.SampleFrames * SizeOf(Single));
   Move(OutBuffer[1, 0], ADC[1].ChannelDataPointer^[0], ASIOHost.BufferSize * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;

 // update time information
 UpdateTime;
end;

end.
