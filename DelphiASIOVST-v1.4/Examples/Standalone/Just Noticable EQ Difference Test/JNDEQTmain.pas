unit JNDEQTmain;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  ExtCtrls, DAV_Types, DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiSlider,
  DAV_AsioHost, DAV_GuiMediaButton, DAV_DspPinkNoiseGenerator, DAV_DspFilter,
  DAV_DspFilterBasics, DAV_DspFilterSimple, DAV_DspBufferedMp3Player,
  DAV_GuiGroup, DAV_GuiPanel, DAV_GuiLED, DAV_GuiButton, DAV_GuiGraphicControl;

type
  TXAssignment = (xaXisA = 1, xaXisB = 2);
  TGuess = (gNone = 0, gXisA = 1, gXisB = 2);
  TSelection = (sX, sA, sB);
  TTestParameter = (tpGain, tpFrequency, tpBandwidth);
  TFmJNDEQT = class(TForm)
    AsioHost: TAsioHost;
    BtMedia: TGuiMediaButton;
    ClipLED: TGuiLED;
    GbEQFilter: TGuiGroup;
    LbAudioFile: TGuiLabel;
    LbAudioFileValue: TGuiLabel;
    LbAutoVolumeAdj: TGuiLabel;
    LbAutoVolumeAdjValue: TGuiLabel;
    LbBandwidth: TGuiLabel;
    LbBandwidthValue: TGuiLabel;
    LbClipIndicator: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    LbInformation: TGuiLabel;
    LbSelectionA: TGuiLabel;
    LbSelectionB: TGuiLabel;
    LbSelectionX: TGuiLabel;
    LbSkip: TGuiLabel;
    LbVolume: TGuiLabel;
    LbVolumeValue: TGuiLabel;
    LbXisA: TGuiLabel;
    LbXisB: TGuiLabel;
    MainMenu: TMainMenu;
    MiAudioSettings: TMenuItem;
    MiDecryptJNDfile: TMenuItem;
    MiExit: TMenuItem;
    MiLatchButtons: TMenuItem;
    MiPinkNoise: TMenuItem;
    MiSettings: TMenuItem;
    MiTest: TMenuItem;
    MiTestFull: TMenuItem;
    MiTestFullBandwidth: TMenuItem;
    MiTestFullBandwidthReference: TMenuItem;
    MiTestFullFrequency: TMenuItem;
    MiTestFullFrequencyReference: TMenuItem;
    MiTestFullGain: TMenuItem;
    MiTestFullGainReference: TMenuItem;
    MiTestStart: TMenuItem;
    MiTestTraining: TMenuItem;
    MiTestTrainingBandwidth: TMenuItem;
    MiTestTrainingFrequency: TMenuItem;
    MiTestTrainingGain: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    OD: TOpenDialog;
    OpenDialog: TOpenDialog;
    PeakCheck: TTimer;
    PnSelectorA: TGuiPanel;
    PnSelectorB: TGuiPanel;
    PnSelectorX: TGuiPanel;
    PnSelectorXisA: TGuiPanel;
    PnSelectorXisB: TGuiPanel;
    PnSkip: TGuiPanel;
    PuAudioFile: TPopupMenu;
    ResultButtonEnabler: TTimer;
    SliderBandwidth: TGuiSlider;
    SliderFrequency: TGuiSlider;
    SliderGain: TGuiSlider;
    SliderVolume: TGuiSlider;
    MiTestFullGainNarrow: TMenuItem;
    MiTestFullGainWide: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure AsioHostBuffersCreate(Sender: TObject);
    procedure AsioHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure AsioHostSampleRateChanged(Sender: TObject);
    procedure BtMediaClick(Sender: TObject);
    procedure LbAudioFileValueDblClick(Sender: TObject);
    procedure LbAutoVolumeAdjustmentClick(Sender: TObject);
    procedure LbSelectionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LbSelectionMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LbSkipClick(Sender: TObject);
    procedure LbXisAClick(Sender: TObject);
    procedure LbXisBClick(Sender: TObject);
    procedure MiAudioSettingsClick(Sender: TObject);
    procedure MiDecryptJNDfileClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiLatchButtonsClick(Sender: TObject);
    procedure MiPinkNoiseClick(Sender: TObject);
    procedure MiTestFullGainReferenceClick(Sender: TObject);
    procedure MiTestTrainingGainClick(Sender: TObject);
    procedure PeakCheckTimer(Sender: TObject);
    procedure ResultButtonEnablerTimer(Sender: TObject);
    procedure SliderBandwidthChange(Sender: TObject);
    procedure SliderFrequencyChange(Sender: TObject);
    procedure SliderGainChange(Sender: TObject);
    procedure SliderVolumeChange(Sender: TObject);
    procedure MiTestTrainingFrequencyClick(Sender: TObject);
    procedure MiTestTrainingBandwidthClick(Sender: TObject);
    procedure MiTestFullGainNarrowClick(Sender: TObject);
    procedure MiTestFullGainWideClick(Sender: TObject);
    procedure ClipLEDClick(Sender: TObject);
  private
    FBackgroundBitmap    : TGuiCustomPixelMap;
    FBufferedPlayer      : TBufferedMP3FilePlayer;
    FPinkNoise           : array [0..1] of TPinkNoiseGenerator;
    FHighpass            : array [0..1] of TFirstOrderHighpassFilter;
    FEQFilter            : array [0..1, 0..1] of TBasicPeakFilter;
    FAudioBuffer         : array [0..1, 0..1] of PDAVSingleFixedArray;
    FPeak                : array [0..1, 0..1] of Double;
    FCurrentX            : TXAssignment;
    FCurrentIndex        : Integer;
    FSelection           : TSelection;
    FCurrentGainDelta    : Double;
    FVolumeFactor        : Double;
    FAdditionalFactor    : Double;
    FPeakReleaseFactor   : Double;
    FPeakRelease         : Double;
    FBandwidth           : Double;
    FFrequency           : Double;
    FTrialNo             : Integer;
    FTrialCount          : Integer;
    FLog                 : TStringList;
    FTestParameter       : TTestParameter;
    FEncryptLogFile      : Boolean;
    FIniFile             : string;
    FVolumeAutoAdj       : Boolean;
    FOutputChannelOffset : Integer;
    function CheckDecryptEnabled: Boolean;
    procedure DisableResultButtons;
    procedure SetSelection(const Value: TSelection);
    procedure SetPeakReleaseFactor(const Value: Double);
    procedure SetVolumeAutoAdjustment(const Value: Boolean);
  protected
    procedure PeakReleaseChanged;
    procedure SelectionChanged;
    procedure BandwidthChanged;
    procedure FrequencyChanged;
    procedure VolumeAutoAdjustmentChanged;
    procedure CalculatePeakReleaseFactor;
    procedure RandomizeAssignment;
    procedure UpdateSelection;
    procedure TestDone;
    procedure UpdateFilterGain;
    procedure NextTrial(GuessWasCorrect: Boolean); virtual;
    procedure LogMessage(MessageText: string);
  public
    procedure StartTest(TestParameter: TTestParameter = tpGain);
    procedure LoadFromFile(FileName: TFileName);

    property IniFile: string read FIniFile;
    property OutputChannelOffset: Integer read FOutputChannelOffset write FOutputChannelOffset;
    property Selection: TSelection read FSelection write SetSelection;
    property PeakRelease: Double read FPeakRelease write SetPeakReleaseFactor;
    property VolumeAutoAdjustment: Boolean read FVolumeAutoAdj write SetVolumeAutoAdjustment;
  end;

var
  FmJNDEQT: TFmJNDEQT;

implementation

{$R *.dfm}

uses
  IniFiles, Math, Mapi, ShellApi, AnsiStrings,
  ZLibEx, // get this library here: http://www.dellapasqua.com/delphizlib/
  DAV_GuiCommon, DAV_Common, DAV_Approximations, JNDEQTaudio, JNDEQTsurvey;

procedure TFmJNDEQT.FormCreate(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 // randomize random seed
 Randomize;

 // locate ini file
 FIniFile := ExtractFilePath(ParamStr(0)) + 'JNEQDT.ini';

 // create background bitmap
 FBackgroundBitmap := TGuiPixelMapMemory.Create;

 // create pink noise generator
 for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1
  do FPinkNoise[ChannelIndex] := TPinkNoiseGenerator.Create;

 // create DC filters
 for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1 do
  begin
   FHighpass[ChannelIndex] := TFirstOrderHighpassFilter.Create;
   FHighpass[ChannelIndex].Frequency := 5;
   FHighpass[ChannelIndex].SampleRate := AsioHost.SampleRate;
  end;

 // create buffered MP3 player
 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 with FBufferedPlayer do
  begin
   Interpolation := biBSpline6Point5thOrder;
   Loop := True;
   BufferSize := 65536;
   BlockSize  := 4096;
  end;

 // create EQ filters
 for BandIndex := 0 to Length(FEQFilter) - 1 do
  for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1 do
   begin
    FEQFilter[BandIndex, ChannelIndex] := TBasicPeakFilter.Create;
    with FEQFilter[BandIndex, ChannelIndex] do
     begin
      Frequency := 1000;
      Bandwidth := 1;
      Gain := 0;
      SampleRate := AsioHost.SampleRate;
     end;
   end;

 // create log
 FLog := TStringList.Create;
 if FileExists(ExtractFilePath(ParamStr(0)) + 'Temp.log') then
  begin
   FLog.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Temp.log');
   DeleteFile(ExtractFilePath(ParamStr(0)) + 'Temp.log');
  end;

 // initialize defaults
 FTestParameter := tpGain;
 FFrequency := SliderFrequency.Value;
 FBandwidth := SliderBandwidth.Value;
 FOutputChannelOffset := 0;
 FVolumeFactor := 1;
 FCurrentIndex := 0;
 FVolumeAutoAdj := False;
 FAdditionalFactor := 1;
end;

procedure TFmJNDEQT.FormDestroy(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 // stop ASIO playback
 ASIOHost.Active := False;

 // free EQ filters
 for BandIndex := 0 to Length(FEQFilter) - 1 do
  for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1
   do FreeAndNil(FEQFilter[BandIndex, ChannelIndex]);

 // free audio buffers
 for BandIndex := 0 to Length(FAudioBuffer) - 1 do
  for ChannelIndex := 0 to Length(FAudioBuffer[BandIndex]) - 1
   do Dispose(FAudioBuffer[BandIndex, ChannelIndex]);

 // free buffered player
 FreeAndNil(FBufferedPlayer);

 // free pink noise generator
 for ChannelIndex := 0 to Length(FPinkNoise) - 1
  do FreeAndNil(FPinkNoise[ChannelIndex]);

 // free DC filters
 for ChannelIndex := 0 to Length(FHighpass) - 1
  do FreeAndNil(FHighpass[ChannelIndex]);

 FreeAndNil(FBackgroundBitmap);

 // free log
 FLog.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Temp.log');
 FreeAndNil(FLog);
end;

procedure TFmJNDEQT.FormKeyPress(Sender: TObject; var Key: Char);
begin
 case Key of
  '1', 'a', 'A' : Selection := sA;
  '3', 'b', 'B' : Selection := sB;
  '2', 'x', 'X' : Selection := sX;
 end;
end;

procedure TFmJNDEQT.FormShow(Sender: TObject);
var
  LastAudioFile : TFileName;
begin
 ClientHeight := BtMedia.Top + BtMedia.Height + 4;

 with TIniFile.Create(FmJNDEQT.IniFile) do
  try
   LastAudioFile := ReadString('Test', 'File', '');
   LoadFromFile(LastAudioFile);
   PeakRelease := ReadFloat('Test', 'Peak Release Time', 5);
   SliderVolume.Value := ReadFloat('Test', 'Volume', -3);
   SliderGain.Value := ReadFloat('Test', 'Gain', SliderGain.Value);
   FFrequency := ReadFloat('Test', 'Frequency', FFrequency);
   FBandwidth := ReadFloat('Test', 'Bandwidth', FBandwidth);
   SliderFrequency.Value := FFrequency;
   SliderBandwidth.Value := FBandwidth;
   MiLatchButtons.Checked := ReadBool('Settings', 'Latch Buttons', MiLatchButtons.Checked);
  finally
   Free;
  end;
end;

procedure TFmJNDEQT.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // inifile
 with TIniFile.Create(FmJNDEQT.IniFile) do
  try
   WriteFloat('Test', 'Volume', SliderVolume.Value);
   WriteFloat('Test', 'Peak Release Time', PeakRelease);
   WriteFloat('Test', 'Gain', SliderGain.Value);
   WriteFloat('Test', 'Frequency', SliderFrequency.Value);
   WriteFloat('Test', 'Bandwidth', SliderBandwidth.Value);
   WriteString('Test', 'File', FBufferedPlayer.Filename);
   WriteBool('Settings', 'Latch Buttons', MiLatchButtons.Checked);
  finally
   Free;
  end;
end;

procedure TFmJNDEQT.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmJNDEQT.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 if Assigned(FBackgroundBitmap) then
  with FBackgroundBitmap do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    hr   := 1 / Height;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      h     := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * Random;
        s[0] := s[1];

        ScnLn[x].B := Round($9D - $34 * (s[1] - h));
        ScnLn[x].G := Round($AE - $48 * (s[1] - h));
        ScnLn[x].R := Round($BD - $50 * (s[1] - h));
       end;
     end;
   end;
end;

procedure TFmJNDEQT.AsioHostSampleRateChanged(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 CalculatePeakReleaseFactor;

 // update EQ filters
 for BandIndex := 0 to Length(FEQFilter) - 1 do
  for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1 do
   if Assigned(FEQFilter[BandIndex, ChannelIndex])
    then FEQFilter[BandIndex, ChannelIndex].SampleRate := AsioHost.SampleRate;

 // update DC filters
 for ChannelIndex := 0 to Length(FHighpass) - 1 do
  if Assigned(FHighpass[ChannelIndex])
   then FHighpass[ChannelIndex].SampleRate := AsioHost.SampleRate;

 // updated buffered MP3 file player
 if Assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := AsioHost.SampleRate;
end;

procedure TFmJNDEQT.BtMediaClick(Sender: TObject);
begin
 with TGuiMediaButton(Sender) do
  if ButtonState = mbsPlay then
   begin
    ButtonState := mbsPause;
    AsioHost.Active := True;
   end
  else
   begin
    ButtonState := mbsPlay;
    AsioHost.Active := False;
   end;
end;

procedure TFmJNDEQT.LbAudioFileValueDblClick(Sender: TObject);
begin
 with OD do
  begin
   if Execute
    then LoadFromFile(OD.FileName);
  end;
end;

procedure TFmJNDEQT.LbAutoVolumeAdjustmentClick(Sender: TObject);
begin
 VolumeAutoAdjustment := False; //not VolumeAutoAdjustment;

 if VolumeAutoAdjustment
  then LbAutoVolumeAdjValue.Caption := 'On'
  else LbAutoVolumeAdjValue.Caption := 'Off';
end;

procedure TFmJNDEQT.LbSkipClick(Sender: TObject);
var
  str : string;
begin
 str := 'Trial ' + IntToStr(FTrialNo + 1) + ': Skipped';
 str := TimeToStr(Now) + ' - ' + str;
 FLog.Add(str + ' (' + FloatToStrF(FCurrentGainDelta, ffGeneral, 4, 4) + ')');

 Inc(FTrialNo);
 if (FTrialCount > 0) and (FTrialNo >= FTrialCount) then
  begin
   TestDone;
   Exit;
  end;

 // trial information
 str := 'Trial: ' + IntToStr(FTrialNo + 1);
 if FTrialCount > 0 then str := str + ' / ' + IntToStr(FTrialCount);
 LbInformation.Caption := str;

 // temporarily disable buttons
 DisableResultButtons;

 // update gain delta
 FCurrentGainDelta := FCurrentGainDelta * 1.6;
 UpdateFilterGain;

 RandomizeAssignment;
 UpdateSelection;
end;

procedure TFmJNDEQT.DisableResultButtons;
begin
 // temporarily disable buttons
 PnSelectorXisA.OnClick := nil;
 PnSelectorXisB.OnClick := nil;
 PnSkip.OnClick := nil;
 LbXisA.OnClick := nil;
 LbXisB.OnClick := nil;
 LbSkip.OnClick := nil;
 ResultButtonEnabler.Enabled := True;
end;

procedure TFmJNDEQT.LbSelectionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if MiLatchButtons.Checked
  then Selection := sX;
end;

procedure TFmJNDEQT.LbSelectionMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 Selection := TSelection(TComponent(Sender).Tag);
end;

procedure TFmJNDEQT.LbXisAClick(Sender: TObject);
begin
 LbXisA.Font.Color := $00006CFF;
 NextTrial(Integer(FCurrentX) = Integer(gXisA));
end;

procedure TFmJNDEQT.LbXisBClick(Sender: TObject);
begin
 LbXisB.Font.Color := $00006CFF;
 NextTrial(Integer(FCurrentX) = Integer(gXisB));
end;

procedure TFmJNDEQT.SetSelection(const Value: TSelection);
begin
 if FSelection <> Value then
  begin
   FSelection := Value;
   SelectionChanged;
  end;
end;

procedure TFmJNDEQT.SetVolumeAutoAdjustment(const Value: Boolean);
begin
 if FVolumeAutoAdj <> Value then
  begin
   FVolumeAutoAdj := Value;
   VolumeAutoAdjustmentChanged;
  end;
end;

procedure TFmJNDEQT.VolumeAutoAdjustmentChanged;
begin
 // yet todo
end;

procedure TFmJNDEQT.SelectionChanged;
begin
 LbSelectionA.Font.Color := clBlack;
 LbSelectionB.Font.Color := clBlack;
 LbSelectionX.Font.Color := clBlack;

 case FSelection of
  sX : begin
        LbSelectionX.Font.Color := $6CFF;
        FLog.Add(TimeToStr(Now) + ' - ' + 'Selection: X');
        FCurrentIndex := Integer(FCurrentX) - 1;
       end;
  sA : begin
        LbSelectionA.Font.Color := $6CFF;
        FLog.Add(TimeToStr(Now) + ' - ' + 'Selection: A');
        FCurrentIndex := 0;
       end;
  sB : begin
        LbSelectionB.Font.Color := $6CFF;
        FLog.Add(TimeToStr(Now) + ' - ' + 'Selection: B');
        FCurrentIndex := 1;
       end;
  else raise Exception.Create('Wrong tag!');
 end;
 UpdateSelection;
end;

procedure TFmJNDEQT.UpdateSelection;
begin
 case FSelection of
  sX : begin
        SliderGain.Value := 0;
        LbGainValue.Caption := '???';
       end;
  sA : SliderGain.Value := -0.5 * FCurrentGainDelta;
  sB : SliderGain.Value := +0.5 * FCurrentGainDelta;
  else raise Exception.Create('Wrong tag!');
 end;
end;

procedure TFmJNDEQT.SetPeakReleaseFactor(const Value: Double);
begin
 if FPeakRelease <> Value then
  begin
   FPeakRelease := Value;
   PeakReleaseChanged;
  end;
end;

procedure TFmJNDEQT.PeakCheckTimer(Sender: TObject);
var
  Peak : Double;
begin
 Peak := Max(Max(FPeak[0, 0], FPeak[0, 1]), Max(FPeak[1, 0], FPeak[1, 1]));
 ClipLED.Brightness_Percent := Power(Limit(Peak - 1, 0, 1), 0.01) * 100;
end;

procedure TFmJNDEQT.PeakReleaseChanged;
begin
 CalculatePeakReleaseFactor;
end;

procedure TFmJNDEQT.CalculatePeakReleaseFactor;
begin
 if FPeakRelease = 0
  then FPeakReleaseFactor := 0
  else FPeakReleaseFactor := FastPower2MinError3( -AsioHost.BufferSize / (FPeakRelease * AsioHost.SampleRate));
end;

procedure TFmJNDEQT.LoadFromFile(FileName: TFileName);
begin
 if FileExists(FileName) then
  begin
   FBufferedPlayer.Filename := FileName;
   LbAudioFileValue.Caption := ExtractFileName(FileName);
   LogMessage('audio file ' + ExtractFileName(FileName) + ' selected');
  end;
end;

procedure TFmJNDEQT.MiAudioSettingsClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmJNDEQT.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmJNDEQT.MiLatchButtonsClick(Sender: TObject);
begin
 MiLatchButtons.Checked := not MiLatchButtons.Checked;
end;

procedure TFmJNDEQT.MiPinkNoiseClick(Sender: TObject);
begin
 FBufferedPlayer.Filename := '';
 LbAudioFileValue.Caption := 'Pink Noise (double click to change)';
 LogMessage('pink noise selected');
end;

procedure TFmJNDEQT.MiTestFullGainReferenceClick(Sender: TObject);
begin
 SliderGain.Enabled := False;
 SliderFrequency.Value := 1000;
 SliderFrequency.Enabled := False;
 SliderBandwidth.Value := 1;
 SliderBandwidth.Enabled := False;
 FTrialCount := 20;
 FEncryptLogFile := True;
 StartTest(tpGain);
 GbEQFilter.Visible := False;
 LogMessage('Full test started (Gain: Reference)');
 ClientHeight := LbInformation.Top + LbInformation.Height + 6;
 ClipLED.Top := LbInformation.Top;
end;

procedure TFmJNDEQT.MiTestFullGainNarrowClick(Sender: TObject);
begin
 SliderGain.Enabled := False;
 SliderFrequency.Value := 1000;
 SliderFrequency.Enabled := False;
 SliderBandwidth.Value := 3;
 SliderBandwidth.Enabled := False;
 FTrialCount := 20;
 FEncryptLogFile := True;
 StartTest(tpGain);
 GbEQFilter.Visible := False;
 LogMessage('Full test started (Gain: Narrow)');
 ClientHeight := LbInformation.Top + LbInformation.Height + 6;
 ClipLED.Top := LbInformation.Top;
end;

procedure TFmJNDEQT.MiTestFullGainWideClick(Sender: TObject);
begin
 SliderGain.Enabled := False;
 SliderFrequency.Value := 1000;
 SliderFrequency.Enabled := False;
 SliderBandwidth.Value := 1/3;
 SliderBandwidth.Enabled := False;
 FTrialCount := 20;
 FEncryptLogFile := True;
 StartTest(tpGain);
 GbEQFilter.Visible := False;
 LogMessage('Full test started (Gain: Narrow)');
 ClientHeight := LbInformation.Top + LbInformation.Height + 6;
 ClipLED.Top := LbInformation.Top;
end;

procedure TFmJNDEQT.MiTestTrainingGainClick(Sender: TObject);
begin
 SliderGain.Enabled := False;
 SliderFrequency.Enabled := True;
 SliderBandwidth.Enabled := True;
 FTrialCount := 0;
 FEncryptLogFile := False;
 StartTest(tpGain);
 GbEQFilter.Visible := True;
 LogMessage('Training test started (Gain)');
 ClientHeight := LbVolume.Top + LbVolume.Height + 8;
 ClipLED.Top := LbVolume.Top;
end;

procedure TFmJNDEQT.MiTestTrainingFrequencyClick(Sender: TObject);
begin
 SliderGain.Enabled := True;
 SliderFrequency.Enabled := False;
 SliderBandwidth.Enabled := True;
 FTrialCount := 0;
 FEncryptLogFile := False;
 StartTest(tpFrequency);
 GbEQFilter.Visible := True;
 LogMessage('Training test started (Frequency)');
 ClientHeight := LbVolume.Top + LbVolume.Height + 8;
 ClipLED.Top := LbVolume.Top;
end;

procedure TFmJNDEQT.MiTestTrainingBandwidthClick(Sender: TObject);
begin
 SliderGain.Enabled := True;
 SliderFrequency.Enabled := True;
 SliderBandwidth.Enabled := False;
 FTrialCount := 0;
 FEncryptLogFile := False;
 StartTest(tpBandwidth);
 GbEQFilter.Visible := True;
 LogMessage('Training test started (Bandwidth)');
 ClientHeight := LbVolume.Top + LbVolume.Height + 8;
 ClipLED.Top := LbVolume.Top;
end;

procedure TFmJNDEQT.LogMessage(MessageText: string);
begin
 FLog.Add(TimeToStr(Now) + ' - ' + MessageText);
 LbInformation.Caption := MessageText;
end;

procedure TFmJNDEQT.RandomizeAssignment;
begin
 FCurrentX := TXAssignment(1 + Random(2));
 if FSelection = sX
  then FCurrentIndex := Integer(FCurrentX) - 1;
end;

procedure TFmJNDEQT.ResultButtonEnablerTimer(Sender: TObject);
begin
 ResultButtonEnabler.Enabled := False;
 PnSelectorXisA.OnClick := LbXisAClick;
 PnSelectorXisB.OnClick := LbXisBClick;
 PnSkip.OnClick := LbSkipClick;
 LbXisA.OnClick := LbXisAClick;
 LbXisB.OnClick := LbXisBClick;
 LbSkip.OnClick := LbSkipClick;
 LbXisA.Font.Color := clBlack;
 LbXisB.Font.Color := clBlack;
 LbSkip.Font.Color := clBlack;
end;

procedure TFmJNDEQT.NextTrial(GuessWasCorrect: Boolean);
var
  str : string;
begin
 str := 'Trial ' + IntToStr(FTrialNo + 1) + ': ';
 if GuessWasCorrect
  then str := str + 'Correct'
  else str := str + 'Wrong';
 str := TimeToStr(Now) + ' - ' + str;
 FLog.Add(str + ' (' + FloatToStrF(FCurrentGainDelta, ffGeneral, 4, 4) + ')');

 Inc(FTrialNo);
 if (FTrialCount > 0) and (FTrialNo >= FTrialCount) then
  begin
   TestDone;   
   Exit;
  end;

 // trial information
 str := 'Trial: ' + IntToStr(FTrialNo + 1);
 if FTrialCount > 0 then str := str + ' / ' + IntToStr(FTrialCount);
 LbInformation.Caption := str;

 // temporarily disable buttons
 DisableResultButtons;

 if GuessWasCorrect
  then FCurrentGainDelta := FCurrentGainDelta * 0.75
  else FCurrentGainDelta := FCurrentGainDelta * 3.1;
 UpdateFilterGain;

 RandomizeAssignment;
 UpdateSelection;
end;

procedure TFmJNDEQT.StartTest(TestParameter: TTestParameter = tpGain);
begin
 FVolumeFactor := 1;
 FCurrentGainDelta := 10;
 FTrialNo := 0;
 FCurrentX := TXAssignment(1 + Random(2));

 UpdateFilterGain;
 RandomizeAssignment;
 UpdateSelection;
end;

function SendEMail(Handle: THandle; Mail: TStrings): Cardinal;
type
  TAttachAccessArray = array [0..0] of TMapiFileDesc;
  PAttachAccessArray = ^TAttachAccessArray;
var
  MapiMessage  : TMapiMessage;
  Receip       : TMapiRecipDesc;
  Attachments  : PAttachAccessArray;
  AttachCount  : Integer;
  i1           : Integer;
  FileName     : AnsiString;
  dwRet        : Cardinal;
  MAPI_Session : Cardinal;
  WndList      : Pointer;
  TempString   : AnsiString;
begin
 Result := 0;
 
 // log on
 dwRet := MapiLogon(Handle, PAnsiChar(''), PAnsiChar(''), MAPI_LOGON_UI or
   MAPI_NEW_SESSION, 0, @MAPI_Session);

 if (dwRet <> SUCCESS_SUCCESS)
  then MessageBox(Handle, PChar('Error while trying to send email'),
    PChar('Error'), MB_ICONERROR or MB_OK)
  else
   begin
    FillChar(MapiMessage, SizeOf(MapiMessage), #0);
    Attachments := nil;
    FillChar(Receip, SizeOf(Receip), #0);

    if Mail.Values['to'] <> '' then
     begin
      Receip.ulReserved := 0;
      Receip.ulRecipClass := MAPI_TO;
      TempString := AnsiString(Mail.Values['to']);
      Receip.lpszName := StrNew(PAnsiChar(TempString));
      TempString := AnsiString('SMTP:' + Mail.Values['to']);
      Receip.lpszAddress := StrNew(PAnsiChar(TempString));
      Receip.ulEIDSize := 0;
      MapiMessage.nRecipCount := 1;
      MapiMessage.lpRecips := @Receip;
     end;

    AttachCount := 0;

    for i1 := 0 to MaxInt do
     begin
      if Mail.Values['attachment' + IntToStr(i1)] = ''
       then Break;
      Inc(AttachCount);
     end;

    if AttachCount > 0 then
     begin
      GetMem(Attachments, SizeOf(TMapiFileDesc) * AttachCount);

      for i1 := 0 to AttachCount - 1 do
       begin
        FileName := AnsiString(Mail.Values['attachment' + IntToStr(i1)]);
        Attachments[i1].ulReserved := 0;
        Attachments[i1].flFlags := 0;
        Attachments[i1].nPosition := ULONG($FFFFFFFF);
        Attachments[i1].lpszPathName := StrNew(PAnsiChar(FileName));
        FileName := ExtractFileName(FileName);
        Attachments[i1].lpszFileName :=
          StrNew(PAnsiChar(FileName));
        Attachments[i1].lpFileType := nil;
       end;
      MapiMessage.nFileCount := AttachCount;
      MapiMessage.lpFiles := @Attachments^;
     end;

    TempString := AnsiString(Mail.Values['subject']);
    if TempString <> ''
     then MapiMessage.lpszSubject := StrNew(PAnsiChar(TempString));

    TempString := AnsiString(Mail.Values['body']);
    if TempString <> ''
     then MapiMessage.lpszNoteText := StrNew(PAnsiChar(TempString));

    WndList := DisableTaskWindows(0);
    try
     Result := MapiSendMail(MAPI_Session, Handle, MapiMessage, MAPI_DIALOG, 0);
    finally
     EnableTaskWindows(WndList);
    end;

    for i1 := 0 to AttachCount - 1 do
     begin
      StrDispose(Attachments[i1].lpszPathName);
      StrDispose(Attachments[i1].lpszFileName);
     end;

    if Assigned(MapiMessage.lpszSubject)
     then StrDispose(MapiMessage.lpszSubject);
    if Assigned(MapiMessage.lpszNoteText)
     then StrDispose(MapiMessage.lpszNoteText);
    if Assigned(Receip.lpszAddress)
     then StrDispose(Receip.lpszAddress);
    if Assigned(Receip.lpszName)
     then StrDispose(Receip.lpszName);

    // log off
    MapiLogOff(MAPI_Session, Handle, 0, 0);
  end;
end;


procedure TFmJNDEQT.TestDone;
var
  FS             : TFileStream;
  CS             : TZCompressionStream;
  Y, H, M, D, MO : Word;
  FileBaseName   : TFileName;
  MailStrings    : TStringList;
begin
 BtMedia.ButtonState := mbsPlay;
 AsioHost.Active := False;
 LbInformation.Caption := 'Test Done';
 ClientHeight := BtMedia.Top + BtMedia.Height + 4;

 DecodeTime(Now, H, M, D, MO);
 DecodeDate(Now, Y, MO, D);
 FileBaseName := ExtractFilePath(ParamStr(0)) + 'Test-' + IntToStr(Y) + '-' +
   IntToStr(MO) + '-' + IntToStr(D) + '-' + IntToStr(H)+ '-' + IntToStr(M);

 if FEncryptLogFile then
  begin
   // open survey dialog and add information
   with TFmSurvey.Create(Self) do
    try
     ShowModal;
     if EdAge.Text <> '' then FLog.Add('Age: ' + EdAge.Text);
     if EdSetup.Text <> '' then FLog.Add('Setup: ' + EdSetup.Text);
     if LEDGenderMale.Brightness_Percent > 50 then FLog.Add('Gender: Male') else
     if LEDGenderFemale.Brightness_Percent > 50 then FLog.Add('Gender: Male');
    finally
     Free;
    end;

   // eventually add MP3 filename
   if FBufferedPlayer.Filename <> ''
    then FLog.Add(FBufferedPlayer.Filename);

   // encrypt JND file
   FS := TFileStream.Create(FileBaseName + '.JND', fmCreate);
   with FS do
    try
     CS := TZCompressionStream.Create(FS, zcMax);
     try
      FLog.SaveToStream(CS);
     finally
      FreeAndNil(CS);
     end;
    finally
     Free;
    end;

   // send JND file via email
   MailStrings := TStringList.Create;
   with MailStrings do
    try
     Values['to'] := 'Christian@savioursofsoul.de';
     Values['subject'] := 'JND Equalizer Test Results';
     Values['body'] := 'This is an automated message containing the test results';
     Values['attachment0'] := FileBaseName + '.JND';
     SendEMail(Application.Handle, MailStrings);
    finally
     Free;
    end;
  end
 else FLog.SaveToFile(FileBaseName + '.log');

 // clear log file
 FLog.Clear;
 Assert(FLog.Count = 0);
end;

function TFmJNDEQT.CheckDecryptEnabled: Boolean;
const
  CDecryptPassword = 'tseTqEdnJ';
begin
 with TIniFile.Create(FmJNDEQT.IniFile) do
  try
   Result := (ReadString('Decrypt', 'Password', '') = CDecryptPassword) or
     (InputBox('Enter Password', 'Enter Password:', '') = CDecryptPassword);
  finally
   Free;
  end;
end;

procedure TFmJNDEQT.ClipLEDClick(Sender: TObject);
begin
 FPeak[0, 0] := 0;
 FPeak[0, 1] := 0;
 FPeak[1, 0] := 0;
 FPeak[1, 1] := 0;
 ClipLED.Brightness_Percent := 0;
end;

procedure TFmJNDEQT.MiDecryptJNDfileClick(Sender: TObject);
var
  FS : TFileStream;
  CS : TZDecompressionStream;
  FN : TFileName;
begin
 if OpenDialog.Execute then
  if CheckDecryptEnabled then
   begin
    FS := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    with FS do
     try
      CS := TZDecompressionStream.Create(FS);
      try
       with TStringList.Create do
        try
         LoadFromStream(CS);
         FN := ChangeFileExt(OpenDialog.FileName, '.log');
         SaveToFile(FN);
         try
          ShellExecute(Handle, 'open', 'notepad.exe', PChar(FN), nil, SW_SHOWNORMAL);
         except
         end;
        finally
         Free;
        end;
      finally
       FreeAndNil(CS);
      end;
     finally
      Free;
     end;
   end;
end;

procedure TFmJNDEQT.UpdateFilterGain;
var
  ChannelIndex : Integer;
begin
 // update filters
 for ChannelIndex := 0 to Length(FEQFilter[0]) - 1
  do FEQFilter[0, ChannelIndex].Gain := -0.5 * FCurrentGainDelta;

 for ChannelIndex := 0 to Length(FEQFilter[1]) - 1
  do FEQFilter[1, ChannelIndex].Gain := +0.5 * FCurrentGainDelta;

 // calculate additional gain factor
 FAdditionalFactor := dB_to_Amp(-0.5 * FCurrentGainDelta);
end;

procedure TFmJNDEQT.SliderBandwidthChange(Sender: TObject);
begin
 if SliderBandwidth.Value <> FBandwidth then
  begin
   FBandwidth := SliderBandwidth.Value;
   BandwidthChanged;
  end;
end;

procedure TFmJNDEQT.BandwidthChanged;
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 // update filters
 for BandIndex := 0 to Length(FEQFilter) - 1 do
  for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1
   do FEQFilter[BandIndex, ChannelIndex].Bandwidth := SliderBandwidth.Value;

 // update GUI
 LbBandwidthValue.Caption := FloatToStrF(SliderBandwidth.Value, ffGeneral, 4, 4);

 FLog.Add(TimeToStr(Now) + ' - ' + 'Bandwidth changed: ' + LbBandwidthValue.Caption);
end;

procedure TFmJNDEQT.SliderFrequencyChange(Sender: TObject);
begin
 if SliderFrequency.Value <> FFrequency then
  begin
   FFrequency := SliderFrequency.Value;
   FrequencyChanged;
  end;
end;

procedure TFmJNDEQT.FrequencyChanged;
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 // update filters
 for BandIndex := 0 to Length(FEQFilter) - 1 do
  for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1
   do FEQFilter[BandIndex, ChannelIndex].Frequency := SliderFrequency.Value;

 // update GUI
 if SliderFrequency.Value > 1000
  then LbFrequencyValue.Caption := FloatToStrF(SliderFrequency.Value * 1E-3, ffGeneral, 4, 4) + ' kHz'
  else LbFrequencyValue.Caption := FloatToStrF(SliderFrequency.Value       , ffGeneral, 4, 4) + ' Hz';

 FLog.Add(TimeToStr(Now) + ' - ' + 'Frequency changed: ' + LbFrequencyValue.Caption);
end;

procedure TFmJNDEQT.SliderGainChange(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 // update filters
 if SliderGain.Enabled then
  for BandIndex := 0 to Length(FEQFilter) - 1 do
   for ChannelIndex := 0 to Length(FEQFilter[BandIndex]) - 1
    do FEQFilter[BandIndex, ChannelIndex].Gain := SliderGain.Value;

 // update GUI
 LbGainValue.Caption := FloatToStrF(SliderGain.Value, ffGeneral, 2, 2) + ' dB';
end;

procedure TFmJNDEQT.SliderVolumeChange(Sender: TObject);
begin
 FVolumeFactor := dB_to_Amp(SliderVolume.Value);
 LbVolumeValue.Caption := FloatToStrF(SliderVolume.Value, ffGeneral, 2, 2) + ' dB';
end;

procedure TFmJNDEQT.AsioHostBuffersCreate(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 // enable playback in case an ASIO driver is selected
 BtMedia.Enabled := AsioHost.DriverIndex >= 0;

 // calculate release factor based on new buffer size
 CalculatePeakReleaseFactor;

 // allocate and clear audio buffer
 for BandIndex := 0 to Length(FAudioBuffer) - 1 do
  for ChannelIndex := 0 to Length(FAudioBuffer[BandIndex]) - 1 do
   begin
    ReallocMem(FAudioBuffer[BandIndex, ChannelIndex], AsioHost.BufferSize * SizeOf(Single));
    FillChar(FAudioBuffer[BandIndex, ChannelIndex]^, AsioHost.BufferSize * SizeOf(Single), 0);
   end;
end;

procedure TFmJNDEQT.AsioHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
  SampleIndex  : Integer;
begin
 // generator
 if FileExists(FBufferedPlayer.Filename)
  then FBufferedPlayer.GetSamples(FAudioBuffer[0, 0], FAudioBuffer[0, 1], ASIOHost.Buffersize)
  else
   for ChannelIndex := 0 to Length(FAudioBuffer[0]) - 1 do
    for SampleIndex := 0 to AsioHost.BufferSize - 1
     do FAudioBuffer[0, ChannelIndex, SampleIndex] := FHighpass[ChannelIndex].ProcessSample64(
       FPinkNoise[ChannelIndex].ProcessSample64);

 // apply peak release 
 for BandIndex := Length(FAudioBuffer) - 1 downto 0 do
  for ChannelIndex := 0 to Length(FAudioBuffer[0]) - 1
   do FPeak[BandIndex, ChannelIndex] := FPeakReleaseFactor * FPeak[BandIndex, ChannelIndex];

 // process EQ filter
 for BandIndex := Length(FAudioBuffer) - 1 downto 0 do
  for ChannelIndex := 0 to Length(FAudioBuffer[0]) - 1 do
   for SampleIndex := 0 to AsioHost.BufferSize - 1 do
    begin
     FAudioBuffer[BandIndex, ChannelIndex, SampleIndex] := FVolumeFactor * FAdditionalFactor *
       FEQFilter[BandIndex, ChannelIndex].ProcessSample64(FAudioBuffer[0, ChannelIndex, SampleIndex]);
     if Abs(FAudioBuffer[BandIndex, ChannelIndex, SampleIndex]) > FPeak[BandIndex, ChannelIndex]
      then FPeak[BandIndex, ChannelIndex] := Abs(FAudioBuffer[BandIndex, ChannelIndex, SampleIndex]);
    end;

 for ChannelIndex := OutputChannelOffset to Min(OutputChannelOffset + 2, AsioHost.OutputChannelCount) - 1 do
  for SampleIndex := 0 to AsioHost.BufferSize - 1
   do OutBuffer[ChannelIndex, SampleIndex] := FAudioBuffer[FCurrentIndex, ChannelIndex mod Length(FAudioBuffer[FCurrentIndex]), SampleIndex];
end;

end.
