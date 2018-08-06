unit AaseMain;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls, Dialogs, Menus, DAV_Types,
  DAV_DspBufferedMp3Player, DAV_DspSimpleOscillator, DAV_GuiPixelMap,
  DAV_AsioHost, DAV_AudioData, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAU,
  DAV_AudioFileAIFF, DAV_GuiLabel, DAV_GuiCustomControl, DAV_GuiGraphicControl,
  DAV_GuiSlider, DAV_GuiButton, DAV_GuiImageControl, DAV_GuiStitchedControls,
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList, DAV_GuiPanel, DAV_GuiGroup;

type
  TDriveParameter = record
    Percent : Single;
    Scale   : array [0..1] of Single;
  end;

  TFmAASE = class(TForm)
    ASIOHost: TASIOHost;
    BtStartStop: TGuiButton;
    DlDrive1: TGuiStitchedDial;
    DlDrive2: TGuiStitchedDial;
    DlDrive3: TGuiStitchedDial;
    DlDrive4: TGuiStitchedDial;
    DlDrive5: TGuiStitchedDial;
    DlDrive6: TGuiStitchedDial;
    DlFrequency1: TGuiStitchedDial;
    DlFrequency2: TGuiStitchedDial;
    DlFrequency3: TGuiStitchedDial;
    DlFrequency4: TGuiStitchedDial;
    DlFrequency5: TGuiStitchedDial;
    DlFrequency6: TGuiStitchedDial;
    DlLevel1: TGuiStitchedDial;
    DlLevel2: TGuiStitchedDial;
    DlLevel3: TGuiStitchedDial;
    DlLevel4: TGuiStitchedDial;
    DlLevel5: TGuiStitchedDial;
    DlLevel6: TGuiStitchedDial;
    GbOscillator: TGuiGroupSide;
    LbSynthLevel: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbLevel: TGuiLabel;
    LbDrive: TGuiLabel;
    PnReadOut: TGuiPanel;
    LbMp3File: TGuiLabel;
    LbMp3FileName: TGuiButton;
    LbMp3Level: TGuiLabel;
    LbOsc1: TGuiLabel;
    LbOsc2: TGuiLabel;
    LbOsc3: TGuiLabel;
    LbOsc4: TGuiLabel;
    LbOsc5: TGuiLabel;
    LbOsc6: TGuiLabel;
    LbOscillator: TGuiLabel;
    LbReadOut: TGuiLabel;
    LbReadOutValue: TGuiLabel;
    MainMenu: TMainMenu;
    MiAdvanced: TMenuItem;
    MiAsio: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiOpen: TMenuItem;
    MiSave: TMenuItem;
    MiSaveAs: TMenuItem;
    MiSettings: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    OpenDialogMp3: TOpenDialog;
    SlLevelMp3: TGuiSlider;
    SlLevelSynth: TGuiSlider;
    SPL: TGuiStitchedPNGList;
    LbWaveFile: TGuiLabel;
    LbAudioFileName: TGuiButton;
    OpenDialogAudio: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure DlDriveChange(Sender: TObject);
    procedure DlDriveMouseEnter(Sender: TObject);
    procedure DlFrequencyChange(Sender: TObject);
    procedure DlFrequencyMouseEnter(Sender: TObject);
    procedure DlLevelChange(Sender: TObject);
    procedure DlLevelMouseEnter(Sender: TObject);
    procedure LbAudioFileNameClick(Sender: TObject);
    procedure LbBufferClick(Sender: TObject);
    procedure LbMp3FileNameClick(Sender: TObject);
    procedure LbOsc1Click(Sender: TObject);
    procedure LbOsc2Click(Sender: TObject);
    procedure LbOsc3Click(Sender: TObject);
    procedure LbOsc4Click(Sender: TObject);
    procedure LbOsc5Click(Sender: TObject);
    procedure LbOsc6Click(Sender: TObject);
    procedure MiAdvancedClick(Sender: TObject);
    procedure MiAsioClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure SlFreqGetText(Sender: TObject; var Text: string);
    procedure SlLevelGetText(Sender: TObject; var Text: string);
    procedure SlLevelMp3Change(Sender: TObject);
    procedure SlLevelSynthChange(Sender: TObject);
    procedure LbMp3FileNameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LbAudioFileNameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FIniFile             : TFileName;
    FBufferedPlayer      : TBufferedMP3FilePlayer;
    FSynthFactor         : Single;
    FFileFactor           : Single;
    FSineGenerators      : array [0..5] of TSimpleOscillator32;
    FDrive               : array [0..5] of TDriveParameter;
    FOutputChannelOffset : Integer;
    FMp3File             : TFileName;
    FAudioFile           : TFileName;
    FAudioData           : TAudioDataCollection32;
    FAudioDataPosition   : Integer;
    procedure SetMp3File(const Value: TFileName);
    procedure SetAudioFile(const Value: TFileName);
  protected
    FBackgroundBitmap : TGuiCustomPixelMap;
    procedure AudioFileChanged; virtual;
    procedure Mp3FileChanged; virtual;
  public
    property IniFile: TFileName read FIniFile;
    property MP3File: TFileName read FMp3File write SetMp3File;
    property AudioFile: TFileName read FAudioFile write SetAudioFile;
    property OutputChannelOffset: Integer read FOutputChannelOffset write FOutputChannelOffset;
  end;

var
  FmAASE: TFmAASE;

implementation

{$R *.dfm}

uses
  Math, IniFiles, DAV_Common, DAV_GuiCommon, DAV_GuiBlend, DAV_BlockProcessing,
  DAV_Approximations, AaseSetup;

{ TFmASIOMP3 }

procedure TFmAASE.FormCreate(Sender: TObject);
var
  GeneratorIndex : Integer;
  SynthLevel     : Single;
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'AASE.INI';

 // create background bitmap
 FBackgroundBitmap := TGuiPixelMapMemory.Create;

 // create and setup MP3 player
 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 FBufferedPlayer.Pitch := 1;
 FBufferedPlayer.Interpolation := biBSpline6Point5thOrder;
 with FBufferedPlayer do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
  end;

 FOutputChannelOffset := 0;
 FSynthFactor := 1;
 FFileFactor := 1;
// ClientHeight := 68;

 // create audio data collection
 FAudioData := TAudioDataCollection32.Create(Self);
 FAudioDataPosition := 0;

 // create sine generators
 SynthLevel := 1 / Length(FSineGenerators);
 for GeneratorIndex := 0 to Length(FSineGenerators) - 1 do
  begin
   FSineGenerators[GeneratorIndex] := TSimpleOscillator32.Create;
   with FSineGenerators[GeneratorIndex] do
    begin
     Frequency := 100 * Power(2, GeneratorIndex);
     Amplitude := SynthLevel;
     SampleRate := ASIOHost.SampleRate;
    end;
  end;

 SynthLevel := Amp_to_dB(SynthLevel);
 DlLevel1.Value := SynthLevel;
 DlLevel2.Value := SynthLevel;
 DlLevel3.Value := SynthLevel;
 DlLevel4.Value := SynthLevel;
 DlLevel5.Value := SynthLevel;
 DlLevel6.Value := SynthLevel;
 DlLevel1.DefaultValue := DlLevel1.Value;
 DlLevel2.DefaultValue := DlLevel2.Value;
 DlLevel3.DefaultValue := DlLevel3.Value;
 DlLevel4.DefaultValue := DlLevel4.Value;
 DlLevel5.DefaultValue := DlLevel5.Value;
 DlLevel6.DefaultValue := DlLevel6.Value;
end;

procedure TFmAASE.FormDestroy(Sender: TObject);
var
  GeneratorIndex : Integer;
begin
 // stop ASIO
 ASIOHost.Active := False;

 // free background bitmap
 FreeAndNil(FBackgroundBitmap);

 // free buffered MP3 player
 FreeAndNil(FBufferedPlayer);

 // free audio data
 FreeAndNil(FAudioData);

 // free sine generators
 for GeneratorIndex := 0 to Length(FSineGenerators) - 1
  do FreeAndNil(FSineGenerators[GeneratorIndex]);
end;

procedure TFmAASE.FormShow(Sender: TObject);
begin
 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);

   MP3File := ReadString('Audio', 'MP3 File', MP3File);
  finally
   Free;
  end;
end;

procedure TFmAASE.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFile) do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteString('Audio', 'MP3 File', MP3File);
  finally
   Free;
  end;
end;

procedure TFmAASE.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmAASE.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
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
      h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * Random;
        s[0] := s[1];

        ScnLn[x].B := Round($10 - $8 * (s[1] - h));
        ScnLn[x].G := Round($63 - $31 * (s[1] - h));
        ScnLn[x].R := Round($AF - $57 * (s[1] - h));
       end;
     end;
   end;
end;

procedure TFmAASE.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := 'Stop Audio';
   BtStartStop.ButtonColor := clWhite;
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedPlayer.Reset;
   BtStartStop.Caption := 'Start Audio';
   BtStartStop.ButtonColor := $001063AF;
  end;
end;

procedure TFmAASE.DlDriveMouseEnter(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender) do
   begin
    LbReadOutValue.Caption := 'Oscillator ' + IntToStr(Tag + 1) + ': ' +
      'Drive = ' + FloatToStrF(Value, ffGeneral, 5, 5) + ' %';
   end;
end;

procedure TFmAASE.DlDriveChange(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender) do
   begin
    if FDrive[Tag].Percent <> Value then
     begin
      FDrive[Tag].Percent := Value;
      FDrive[Tag].Scale[0] := 0.01 * Power(2, 10 * Log2(1 + 0.01 * Value));
      FDrive[Tag].Scale[1] := 1 / FDrive[Tag].Scale[0];
     end;

    LbReadOutValue.Caption := 'Oscillator ' + IntToStr(Tag + 1) + ': ' +
      'Drive = ' + FloatToStrF(Value, ffGeneral, 5, 5) + ' %';
   end;
end;

procedure TFmAASE.DlFrequencyMouseEnter(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender) do
   begin
    LbReadOutValue.Caption := 'Oscillator ' + IntToStr(Tag + 1) + ': ' +
      'Frequency = ' + FloatToStrF(Value, ffGeneral, 5, 5) + ' Hz';
   end;
end;

procedure TFmAASE.DlFrequencyChange(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender) do
   begin
    FSineGenerators[Tag].Frequency := Value;
    LbReadOutValue.Caption := 'Oscillator ' + IntToStr(Tag + 1) + ': ' +
      'Frequency = ' + FloatToStrF(Value, ffGeneral, 5, 5) + ' Hz';
   end;
end;

procedure TFmAASE.DlLevelMouseEnter(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender) do
   begin
    LbReadOutValue.Caption := 'Oscillator ' + IntToStr(Tag + 1) + ': ' +
      'Level = ' + FloatToStrF(Value, ffGeneral, 3, 3) + ' dB';
   end;
end;

procedure TFmAASE.DlLevelChange(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender) do
   begin
    FSineGenerators[Tag].Amplitude := dB_to_Amp(Value);
    LbReadOutValue.Caption := 'Oscillator ' + IntToStr(Tag + 1) + ': ' +
      'Level = ' + FloatToStrF(Value, ffGeneral, 3, 3) + ' dB';
   end;
end;

procedure TFmAASE.LbBufferClick(Sender: TObject);
begin
 ASIOHost.SampleRate := 48000;
end;

procedure TFmAASE.LbMp3FileNameClick(Sender: TObject);
begin
 if OpenDialogMp3.Execute
  then MP3File := OpenDialogMp3.FileName;
end;

procedure TFmAASE.LbMp3FileNameMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbRight
  then MP3File := '';
end;

procedure TFmAASE.LbOsc1Click(Sender: TObject);
begin
 DlLevel1.Enabled := not DlLevel1.Enabled;
 DlFrequency1.Enabled := DlLevel1.Enabled;
 if DlLevel1.Enabled then
  begin
   LbOsc1.Font.Color := clWhite;
   FSineGenerators[0].Amplitude := dB_to_Amp(DlLevel1.Value);
  end
 else
  begin
   LbOsc1.Font.Color := $001063AF;
   FSineGenerators[0].Amplitude := 0;
  end;
end;

procedure TFmAASE.LbOsc2Click(Sender: TObject);
begin
 DlLevel2.Enabled := not DlLevel2.Enabled;
 DlFrequency2.Enabled := DlLevel2.Enabled;
 if DlLevel2.Enabled then
  begin
   LbOsc2.Font.Color := clWhite;
   FSineGenerators[1].Amplitude := dB_to_Amp(DlLevel2.Value);
  end
 else
  begin
   LbOsc2.Font.Color := $001063AF;
   FSineGenerators[1].Amplitude := 0;
  end;
end;

procedure TFmAASE.LbOsc3Click(Sender: TObject);
begin
 DlLevel3.Enabled := not DlLevel3.Enabled;
 DlFrequency3.Enabled := DlLevel3.Enabled;
 if DlLevel3.Enabled then
  begin
   LbOsc3.Font.Color := clWhite;
   FSineGenerators[2].Amplitude := dB_to_Amp(DlLevel3.Value);
  end
 else
  begin
   LbOsc3.Font.Color := $001063AF;
   FSineGenerators[2].Amplitude := 0;
  end;
end;

procedure TFmAASE.LbOsc4Click(Sender: TObject);
begin
 DlLevel4.Enabled := not DlLevel4.Enabled;
 DlFrequency4.Enabled := DlLevel4.Enabled;
 if DlLevel4.Enabled then
  begin
   LbOsc4.Font.Color := clWhite;
   FSineGenerators[3].Amplitude := dB_to_Amp(DlLevel4.Value);
  end
 else
  begin
   LbOsc4.Font.Color := $001063AF;
   FSineGenerators[3].Amplitude := 0;
  end;
end;

procedure TFmAASE.LbOsc5Click(Sender: TObject);
begin
 DlLevel5.Enabled := not DlLevel5.Enabled;
 DlFrequency5.Enabled := DlLevel5.Enabled;
 if DlLevel5.Enabled then
  begin
   LbOsc5.Font.Color := clWhite;
   FSineGenerators[4].Amplitude := dB_to_Amp(DlLevel5.Value);
  end
 else
  begin
   LbOsc5.Font.Color := $001063AF;
   FSineGenerators[4].Amplitude := 0;
  end;
end;

procedure TFmAASE.LbOsc6Click(Sender: TObject);
begin
 DlLevel6.Enabled := not DlLevel6.Enabled;
 DlFrequency6.Enabled := DlLevel6.Enabled;
 if DlLevel6.Enabled then
  begin
   LbOsc6.Font.Color := clWhite;
   FSineGenerators[5].Amplitude := dB_to_Amp(DlLevel6.Value);
  end
 else
  begin
   LbOsc6.Font.Color := $001063AF;
   FSineGenerators[5].Amplitude := 0;
  end;
end;

procedure TFmAASE.LbAudioFileNameClick(Sender: TObject);
begin
 if OpenDialogAudio.Execute
  then AudioFile := OpenDialogAudio.FileName;
end;

procedure TFmAASE.LbAudioFileNameMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbRight then AudioFile := '';
end;

procedure TFmAASE.MiAdvancedClick(Sender: TObject);
begin
 MiAdvanced.Checked := not MiAdvanced.Checked;

 if MiAdvanced.Checked
  then ClientHeight := 345
  else ClientHeight := 68;
end;

procedure TFmAASE.MiAsioClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmAASE.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAASE.Mp3FileChanged;
begin
 if FileExists(FMp3File) then
  begin
   FBufferedPlayer.Filename := FMp3File;
   LbMp3FileName.Caption := ExtractFileName(FMp3File)
  end
 else
  begin
   FBufferedPlayer.Reset;
   LbMp3FileName.Caption := '[none]';
  end;
end;

procedure TFmAASE.SetAudioFile(const Value: TFileName);
begin
 if FAudioFile <> Value then
  begin
   FAudioFile := Value;
   AudioFileChanged;
  end;
end;

procedure TFmAASE.SetMp3File(const Value: TFileName);
begin
 if FMp3File <> Value then
  begin
   FMp3File := Value;
   Mp3FileChanged;
  end;
end;

procedure TFmAASE.SlLevelMp3Change(Sender: TObject);
begin
 FFileFactor := dB_to_Amp(SlLevelMp3.Value);
end;

procedure TFmAASE.SlLevelGetText(Sender: TObject; var Text: string);
begin
 if Sender is TGuiSlider then
  with TGuiSlider(Sender)
   do Text := FloatToStrF(Value, ffGeneral, 2, 2) + ' dB';
end;

procedure TFmAASE.SlLevelSynthChange(Sender: TObject);
begin
 FSynthFactor := dB_to_Amp(SlLevelSynth.Value);
end;

procedure TFmAASE.SlFreqGetText(Sender: TObject; var Text: string);
begin
 if Sender is TGuiSlider then
  with TGuiSlider(Sender) do
   if Value >= 1000
    then Text := FloatToStrF(1E-3 * Value, ffGeneral, 4, 4) + ' kHz'
    else Text := FloatToStrF(Value, ffGeneral, 4, 4) + ' Hz';
end;

procedure TFmAASE.ASIOHostSampleRateChanged(Sender: TObject);
var
  GeneratorIndex : Integer;
begin
 if Assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := ASIOHost.SampleRate;

 for GeneratorIndex := 0 to Length(FSineGenerators) - 1
  do FSineGenerators[GeneratorIndex].SampleRate := ASIOHost.SampleRate;
end;

procedure TFmAASE.AudioFileChanged;
begin
 try
  if FileExists(FAudioFile) then
   begin
    FAudioData.LoadFromFile(FAudioFile);
    FAudioDataPosition := 0;
    LbAudioFileName.Caption := ExtractFileName(FAudioFile);
   end
  else LbMp3FileName.Caption := '[none]';
 except
  FAudioData.Clear;
  LbAudioFileName.Caption := '[none]';
 end;
end;

procedure TFmAASE.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAASE.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  GeneratorIndex  : Integer;
  SampleIndex     : Integer;
  GeneratorSignal : Single;
  AudioDataPtr    : array [0..1] of PDAVSingleFixedArray;
  AudioData       : array [0..1] of Single;
begin
 FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);

 if (FAudioData.Channels.Count > 0) and (FAudioData.SampleFrames > 0) then
  begin
   AudioDataPtr[0] := FAudioData.ChannelDataPointer[0];
   AudioDataPtr[1] := FAudioData.ChannelDataPointer[1 mod FAudioData.Channels.Count];
  end
 else
  begin
   AudioDataPtr[0] := nil;
   AudioDataPtr[1] := nil;
  end;

 for SampleIndex := 0 to ASIOHost.Buffersize - 1 do
  begin
   if FDrive[0].Percent > 0
    then GeneratorSignal := FastTanhOpt5Term(FDrive[0].Scale[0] * FSineGenerators[0].Sine) * FDrive[0].Scale[1]
    else GeneratorSignal := FSineGenerators[0].Sine;

   FSineGenerators[0].CalculateNextSample;
   for GeneratorIndex := 1 to Length(FSineGenerators) - 1 do
    begin
     if FDrive[GeneratorIndex].Percent > 0
      then GeneratorSignal := GeneratorSignal + FastTanhOpt5Term(FDrive[GeneratorIndex].Scale[0] * FSineGenerators[GeneratorIndex].Sine) * FDrive[GeneratorIndex].Scale[1]
      else GeneratorSignal := GeneratorSignal + FSineGenerators[GeneratorIndex].Sine;
     FSineGenerators[GeneratorIndex].CalculateNextSample;
    end;

   if Assigned(AudioDataPtr[0]) then
    begin
     AudioData[0] := AudioDataPtr[0]^[FAudioDataPosition];
     AudioData[1] := AudioDataPtr[1]^[FAudioDataPosition];
     Inc(FAudioDataPosition);
     if FAudioDataPosition >= FAudioData.SampleFrames
      then FAudioDataPosition := 0;
    end;

   OutBuffer[0, SampleIndex] := FFileFactor * (OutBuffer[0, SampleIndex] + AudioData[0]) +
     FSynthFactor * GeneratorSignal;
   OutBuffer[1, SampleIndex] := FFileFactor * (OutBuffer[1, SampleIndex] + AudioData[1]) +
     FSynthFactor * GeneratorSignal;
  end;

end;

end.
