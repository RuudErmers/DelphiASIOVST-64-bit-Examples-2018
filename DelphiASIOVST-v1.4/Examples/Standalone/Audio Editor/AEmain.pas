unit AEmain;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, ToolWin, ComCtrls, ExtCtrls, 
  StdCtrls, Buttons, DAV_Types, DAV_GuiStaticWaveform, DAV_GuiBaseControl, 
  DAV_GuiLevelMeter, DAV_AudioFile, DAV_AudioData, DAV_AudioFileWav, 
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_ASIOHost, DAV_VSTHost, 
  DAV_GuiAudioDataDisplay, DAV_Classes;

type
  TFmAudioEditor = class(TForm)
    ASIOHost: TASIOHost;
    AudioDataCollection32: TAudioDataCollection32;
    ControlBar1: TControlBar;
    GuiAudioDataDisplay: TGuiAudioDataDisplay;
    GuiLevelMeter: TGuiLevelMeter;
    MainMenu: TMainMenu;
    MIAbout: TMenuItem;
    MIASIOSetup: TMenuItem;
    MIAusschneiden: TMenuItem;
    MIDelete: TMenuItem;
    MIEdit: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIGenerate: TMenuItem;
    MIHelp: TMenuItem;
    MIInvert: TMenuItem;
    MIKopieren: TMenuItem;
    MINew: TMenuItem;
    MINoise: TMenuItem;
    MINormalize: TMenuItem;
    MIOpen: TMenuItem;
    MIOptions: TMenuItem;
    MIPaste: TMenuItem;
    MIPinkNoise: TMenuItem;
    MIProcess: TMenuItem;
    MIRectify: TMenuItem;
    MIRedo: TMenuItem;
    MIRemoveDC: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MISelectAll: TMenuItem;
    MISelectNone: TMenuItem;
    MIUndo: TMenuItem;
    MIView: TMenuItem;
    MIVstSetup: TMenuItem;
    MIWaveform: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    VstHost: TVstHost;
    WhiteNoise1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure DataChangedHandler(Sender: TObject);
    procedure MIASIOSetupClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIInvertClick(Sender: TObject);
    procedure MINormalizeClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MIRectifyClick(Sender: TObject);
    procedure MIRemoveDCClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MIVstSetupClick(Sender: TObject);
    procedure MIWhiteNoiseClick(Sender: TObject);
  private
    FFileName : TFileName;
  public
    property FileName: TFileName read FFileName;
  end;

var
  FmAudioEditor: TFmAudioEditor;

implementation

{$R *.dfm}

uses
  AEAsioSetup, AEVstSetup;

procedure TFmAudioEditor.FormCreate(Sender: TObject);
var
  Sample: Integer;
begin
 for Sample := 0 to AudioDataCollection32.SampleFrames - 1 do
  begin
   AudioDataCollection32[0].ChannelData[Sample] := 2 * random - 1;
  end;
end;

procedure TFmAudioEditor.DataChangedHandler(Sender: TObject);
begin
 GuiAudioDataDisplay.Invalidate;
end;

procedure TFmAudioEditor.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAudioEditor.MINormalizeClick(Sender: TObject);
var
  ch, i  : Integer;
  max    : Double;
  chdata : PDAVSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   max := 0;

   // scan for maximum
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointer[ch];
     for i := 0 to SampleFrames - 1 do
      if abs(chdata^[i]) > max
       then max := abs(chdata^[i]);
    end;

   // actually normalize
   if max > 0 then
    begin
     max := 1 / max;
     for ch := 0 to ChannelCount - 1 do
      begin
       chdata := ChannelDataPointer[ch];
       for i := 0 to SampleFrames - 1
        do chdata^[i] := max * chdata^[i];
      end;
    end;

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIRectifyClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PDAVSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   // rectify every sample
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointer[ch];
     for i := 0 to SampleFrames - 1
      do chdata^[i] := abs(chdata^[i]);
    end;

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIRemoveDCClick(Sender: TObject);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
  TempSum      : Double;
  TempDC       : Double;
  ChannelData  : PDAVSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   for ChannelIndex := 0 to ChannelCount - 1 do
    begin
     ChannelData := ChannelDataPointer[ChannelIndex];

     // build TempSum of
     TempSum := 0;
     for SampleIndex := 0 to SampleFrames - 1
      do TempSum := TempSum + ChannelData^[SampleIndex];

     TempDC := TempSum / SampleFrames;
     for SampleIndex := 0 to SampleFrames - 1
      do ChannelData^[SampleIndex] := ChannelData^[SampleIndex] - TempDC;
    end;

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIInvertClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PDAVSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
   // rectify every sample
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointer[ch];
     for i := 0 to SampleFrames - 1
      do chdata^[i] := -chdata^[i];
    end;

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIWhiteNoiseClick(Sender: TObject);
var
  ch, i  : Integer;
  chdata : PDAVSingleFixedArray;
begin
 with AudioDataCollection32 do
  begin
//   ChannelCount := 2;
   SampleFrames   := round(ASIOHost.SampleRate);
   for ch := 0 to ChannelCount - 1 do
    begin
     chdata := ChannelDataPointer[ch];
     for i := 0 to SampleFrames - 1
      do chdata^[i] := 2 * random - 1;
    end;

   GuiAudioDataDisplay.Invalidate;
  end;
end;

procedure TFmAudioEditor.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'All known files |*.wav;*.aif*;*.au|' +
             'Wave File (*.wav)|*.wav|' +
             'AIFF File (*.aif)|*.aif*|' +
             'AU File (*.au)|*.au;*.snd';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load Audio File';
   if Execute then
    begin
     FFileName := FileName;
     AudioDataCollection32.LoadFromFile(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmAudioEditor.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'wav';
   Filter := 'All known files |*.wav;*.aif*;*.au|' +
             'Wave File (*.wav)|*.wav|' +
             'AIFF File (*.aif)|*.aif*|' +
             'AU File (*.au)|*.au;*.snd';
   Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing];
   Title := 'Save Audio File';
   if Execute then
    begin
     FFileName := FileName;
     AudioDataCollection32.SaveToFile(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmAudioEditor.MIVstSetupClick(Sender: TObject);
begin
 FmVstSetup.ShowModal;
end;

procedure TFmAudioEditor.MIASIOSetupClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

end.
