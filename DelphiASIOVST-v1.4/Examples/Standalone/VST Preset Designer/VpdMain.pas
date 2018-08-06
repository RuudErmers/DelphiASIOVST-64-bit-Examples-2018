unit VpdMain;

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

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, ToolWin, ComCtrls, ExtCtrls, 
  DAV_Types, DAV_VSTHost, DAV_MidiFile, DAV_AsioHost, DAV_DspBufferedMp3Player;

type
  TFmVstPresetDesigner = class(TForm)
    ASIOHost: TAsioHost;
    MainMenu: TMainMenu;
    MiASIO: TMenuItem;
    MiAsioControlPanel: TMenuItem;
    MiAsioSplitter: TMenuItem;
    MiBank: TMenuItem;
    MiBankDesign: TMenuItem;
    MiBankLoad: TMenuItem;
    MiBankRandomize: TMenuItem;
    MiBankSave: TMenuItem;
    MiBankShuffle: TMenuItem;
    MiEmbed: TMenuItem;
    MiExit: TMenuItem;
    MiOpen: TMenuItem;
    MiOpenAudio: TMenuItem;
    MiOpenMIDI: TMenuItem;
    MiPlayPreview: TMenuItem;
    MiPreview: TMenuItem;
    MiProgram: TMenuItem;
    MiProgramDesign: TMenuItem;
    MiProgramLoad: TMenuItem;
    MiProgramRandomize: TMenuItem;
    MiProgramRename: TMenuItem;
    MiProgramSave: TMenuItem;
    MiProgramShuffle: TMenuItem;
    MiProgramSplitter: TMenuItem;
    MIVstPlugin: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    OpenAudio: TOpenDialog;
    OpenDialog: TOpenDialog;
    OpenMidi: TOpenDialog;
    OpenVSTDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    VstHost: TVstHost;
    VSTPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure MiAsioControlPanelClick(Sender: TObject);
    procedure MiAsioDriverClick(Sender: TObject);
    procedure MiBankLoadClick(Sender: TObject);
    procedure MiBankRandomizeClick(Sender: TObject);
    procedure MiBankSaveClick(Sender: TObject);
    procedure MiBankShuffleClick(Sender: TObject);
    procedure MiEmbedClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiOpenAudioClick(Sender: TObject);
    procedure MiOpenClick(Sender: TObject);
    procedure MiOpenMIDIClick(Sender: TObject);
    procedure MiPlayPreviewClick(Sender: TObject);
    procedure MIPresetClick(Sender: TObject);
    procedure MiProgramDesignClick(Sender: TObject);
    procedure MiProgramLoadClick(Sender: TObject);
    procedure MiProgramRandomizeClick(Sender: TObject);
    procedure MiProgramRenameClick(Sender: TObject);
    procedure MiProgramSaveClick(Sender: TObject);
    procedure MiProgramShuffleClick(Sender: TObject);
  private
    FIniFile    : TFileName;
    FVstPlugin  : TFileName;
    FMidiFile   : TMidiFile;
    FAudioFile  : TFileName;
    FInBuffer   : TDAVArrayOfSingleFixedArray;
    FOutBuffer  : TDAVArrayOfSingleFixedArray;
    FBufferSize : Integer;
    FMp3Player  : TBufferedMP3FilePlayer;
    procedure LoadVSTPlugin(FileName: TFileName);
    procedure SetBufferSize(const Value: Integer);
  protected
    procedure BufferSizeChanged; virtual;
    procedure UpdatePrograms;
  public
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  end;

var
  FmVstPresetDesigner: TFmVstPresetDesigner;

implementation

{$R *.dfm}

uses
  Inifiles, Math, DAV_Common, DAV_DLLResources; //, VpdModifier;

procedure TFmVstPresetDesigner.FormCreate(Sender: TObject);
var
  VstFileName : TFileName;
  Index       : Integer;
  MenuItem    : TMenuItem;
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'VST Preset Designer.ini';

 with TIniFile.Create(FIniFile) do
  try
   Top := ReadInteger('Layout', 'Main Top', Top);
   Left := ReadInteger('Layout', 'Main Left', Left);

   // ASIO driver
   with ASIOHost
    do DriverIndex := DriverList.IndexOf(ReadString('Program', 'ASIO Driver', ''));
   MiPlayPreview.Enabled := (ASIOHost.DriverIndex >= 0);

   // VST Plugins
   VstFileName := ReadString('Program', 'Last VST Plugin', '');
   if FileExists(VstFileName)
    then LoadVSTPlugin(VstFileName);

   // VST Plugins
   FAudioFile := ReadString('Program', 'Last MP3 File', '');
  finally
   Free;
  end;

 // build ASIO driver menu items
 for Index := 0 to ASIOHost.DriverList.Count - 1 do
  begin
   MenuItem := TMenuItem.Create(MiASIO);
   MenuItem.Caption := ASIOHost.DriverList[Index];
   MenuItem.Tag := Index;
   MenuItem.RadioItem := True;
   MenuItem.Checked := ASIOHost.DriverIndex = Index;
   MenuItem.OnClick := MiAsioDriverClick;
   MiASIO.Insert(0, MenuItem);
  end;

 // create MP3 player
 FMp3Player := TBufferedMP3FilePlayer.Create;
 with FMp3Player do
  begin
   Pitch := 0;
   Interpolation := biBSpline6Point5thOrder;
   SampleRate := ASIOHost.SampleRate;
   BufferSize := 65536;
   BlockSize  := 4096;
   Filename := FAudioFile;
  end;
end;

procedure TFmVstPresetDesigner.FormDestroy(Sender: TObject);
begin
 ASIOHost.Active := False;

 with TIniFile.Create(FIniFile) do
  try
   WriteString('Program', 'Last VST Plugin', FVstPlugin);
   WriteString('Program', 'ASIO Driver', ASIOHost.DriverName);
  finally
   Free;
  end;

 FreeAndNil(FMp3Player);
end;

procedure TFmVstPresetDesigner.MiAsioControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmVstPresetDesigner.MiAsioDriverClick(Sender: TObject);
begin
 if Sender is TMenuItem then
  with TMenuItem(Sender) do
   begin
    ASIOHost.Active := False;
    ASIOHost.DriverIndex := Tag;
    ASIOHost.Active := MiPlayPreview.Checked;
    MiPlayPreview.Enabled := (ASIOHost.DriverIndex >= 0);
   end;
end;

procedure TFmVstPresetDesigner.MiOpenClick(Sender: TObject);
begin
 with OpenVSTDialog do
  begin
   // restore VST Plugin Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Plugin Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     LoadVSTPlugin(FileName);

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Plugin Directory',
         ExtractFileDir(OpenVSTDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiOpenMIDIClick(Sender: TObject);
begin
 with OpenDialog do
  begin
   // restore MIDI Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'Preview MIDI Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     if Assigned(FMidiFile)
      then FreeAndNil(FMidiFile);

     FMidiFile := TMidiFile.Create(Self);
     FMidiFile.Filename := FileName;

     MiOpen.Caption := 'Open &MIDI... (' + ExtractFileName(FileName) + ')';

     // Store Audio Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'Preview Audio Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiPlayPreviewClick(Sender: TObject);
begin
 MiPlayPreview.Checked := not MiPlayPreview.Checked;
 ASIOHost.Active := MiPlayPreview.Checked;
 if ASIOHost.Active = False
  then FMp3Player.Reset;
end;

procedure TFmVstPresetDesigner.MiOpenAudioClick(Sender: TObject);
begin
 with OpenAudio do
  begin
   // restore Audio Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'Preview Audio Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     FAudioFile := FileName;
     FMp3Player.Filename := FAudioFile;
     MiOpen.Caption := 'Open &Audio... (' + ExtractFileName(FileName) + ')';

     // Store MIDI Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'Preview MIDI Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiProgramLoadClick(Sender: TObject);
begin
 with OpenDialog do
  begin
   // restore VST Preset/Bank Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Preset/Bank Directory', InitialDir);
    finally
     Free;
    end;

   FilterIndex := 2;

   if Execute then
    begin
     case FilterIndex of
      1 : begin
           VstHost[0].LoadBank(OpenDialog.FileName);
           UpdatePrograms;
          end;
      2 : VstHost[0].LoadPreset(OpenDialog.FileName);
     end;

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Preset/Bank Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiProgramSaveClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   // restore VST Preset/Bank Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Preset/Bank Directory', InitialDir);
    finally
     Free;
    end;

   FilterIndex := 2;

   if Execute then
    begin
     case FilterIndex of
      1 : VstHost[0].SaveBank(SaveDialog.FileName);
      2 : VstHost[0].SavePreset(SaveDialog.FileName);
     end;

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Preset/Bank Directory',
         ExtractFileDir(SaveDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiBankLoadClick(Sender: TObject);
begin
 with OpenDialog do
  begin
   // restore VST Preset/Bank Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Preset/Bank Directory', InitialDir);
    finally
     Free;
    end;

   FilterIndex := 1;

   if Execute then
    begin
     case FilterIndex of
      1 : begin
           VstHost[0].LoadBank(OpenDialog.FileName);
           UpdatePrograms;
          end;
      2 : VstHost[0].LoadPreset(OpenDialog.FileName);
     end;

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Preset/Bank Directory',
         ExtractFileDir(OpenDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiBankRandomizeClick(Sender: TObject);
var
  Param : Integer;
begin
 for Param := 0 to VstHost[0].numParams - 1
  do VstHost[0].Parameter[Param] := Random;
end;

procedure TFmVstPresetDesigner.MiBankSaveClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   // restore VST Preset/Bank Directory
   with TIniFile.Create(FIniFile) do
    try
     InitialDir := ReadString('Program', 'VST Preset/Bank Directory', InitialDir);
    finally
     Free;
    end;

   FilterIndex := 1;

   if Execute then
    begin
     case FilterIndex of
      1 : VstHost[0].SaveBank(SaveDialog.FileName);
      2 : VstHost[0].SavePreset(SaveDialog.FileName);
     end;

     // Store VST Plugin Directory
     with TIniFile.Create(FIniFile) do
      try
       WriteString('Program', 'VST Preset/Bank Directory',
         ExtractFileDir(SaveDialog.FileName));
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmVstPresetDesigner.MiBankShuffleClick(Sender: TObject);
var
  Param       : Integer;
  OldParamVal : Single;
  NewParamVal : Single;
  Trial       : Integer;
begin
 for Param := 0 to VstHost[0].numParams - 1 do
  begin
   OldParamVal := VstHost[0].Parameter[Param];
   Trial := 0;
   repeat
    NewParamVal := Limit(VstHost[0].Parameter[Param] + 0.01 * Random, 0, 1);
    Inc(Trial);
   until (NewParamVal <> OldParamVal) or (Trial > 15);

   VstHost[0].Parameter[Param] := NewParamVal;
  end;
end;

procedure TFmVstPresetDesigner.MiProgramRandomizeClick(Sender: TObject);
var
  CurrentProg : Integer;
  Prog, Param : Integer;
begin
 CurrentProg := VstHost[0].CurrentProgram;
 for Prog := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].CurrentProgram := Prog;
   for Param := 0 to VstHost[0].numParams - 1
    do VstHost[0].Parameter[Param] := Random;
  end;
 VstHost[0].CurrentProgram := CurrentProg;
end;

procedure TFmVstPresetDesigner.MiProgramShuffleClick(Sender: TObject);
var
  CurrentProg : Integer;
  Prog, Param : Integer;
  OldParamVal : Single;
  NewParamVal : Single;
  Trial       : Integer;
begin
 CurrentProg := VstHost[0].CurrentProgram;
 for Prog := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].CurrentProgram := Prog;
   for Param := 0 to VstHost[0].numParams - 1 do
    begin
     OldParamVal := VstHost[0].Parameter[Param];
     Trial := 0;
     repeat
      NewParamVal := Limit(VstHost[0].Parameter[Param] + 0.01 * Random, 0, 1);
      inc(Trial);
     until (NewParamVal <> OldParamVal) or (Trial > 15);

     VstHost[0].Parameter[Param] := NewParamVal;
    end;
  end;
 VstHost[0].CurrentProgram := CurrentProg;
end;

procedure TFmVstPresetDesigner.MiProgramRenameClick(Sender: TObject);
var
  ProgName : string;
begin
 ProgName := string(VstHost[0].ProgramName);
 if InputQuery('Rename', 'Please enter the new program name', ProgName)
  then VstHost[0].ProgramName := AnsiString(ProgName);
 UpdatePrograms;
end;

procedure TFmVstPresetDesigner.MiProgramDesignClick(Sender: TObject);
begin
(*
 if FmModifier.ShowModal = mrOK then
  begin
  end;
*)
end;

procedure TFmVstPresetDesigner.MiEmbedClick(Sender: TObject);
var
  MS : TMemoryStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
begin
 if FileExists(FVstPlugin) then
  try
   // create memory stream
   MS := TMemoryStream.Create;

   // store preset to memory stream
   VstHost[0].SavePreset(MS);

   try
    RM := TPEResourceModule.Create;
    RM.LoadFromFile(FVstPlugin);
    RD := TResourceDetails.CreateResourceDetails(RM, 0, 'FXP',
      IntToStr(VstHost[0].CurrentProgram), MS.Size, MS.Memory);
    RM.AddResource(RD);
    RM.SortResources;
    RM.SaveToFile(FVstPlugin);

    ShowMessage('Plugin will be reloaded now!');

    // reload VST plugin
    LoadVSTPlugin(FVstPlugin);
   finally
    if Assigned(RM)
     then FreeAndNil(RM);
   end;
  finally
   if Assigned(MS)
    then FreeAndNil(MS);
  end;
end;

procedure TFmVstPresetDesigner.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVstPresetDesigner.MIPresetClick(Sender: TObject);
begin
 with Sender as TMenuItem do
  begin
   VstHost[0].CurrentProgram := Tag;
   Checked := True;
  end;
end;

procedure TFmVstPresetDesigner.LoadVSTPlugin(FileName: TFileName);
var
  MS : TMemoryStream;
begin
 // load VST Plugin
 with VstHost[0] do
  begin
   Active := False;
   FVstPlugin := FileName;
   try
    MS := TMemoryStream.Create;
    MS.LoadFromFile(FVstPlugin);
    MS.Position := 0;
    LoadFromStream(MS);
    FreeAndNil(MS);
    MiEmbed.Visible := True;
   except
    MiEmbed.Visible := False;
    DLLFileName := FVstPlugin;
   end;
   Active := True;
   Idle;
   ShowEdit(VSTPanel);
   Idle;
   EditIdle;
   Caption :=  string(GetVendorString + ' ' + GetEffectName);

   FBufferSize := 8192;
   SetLength(FInBuffer,  Max(2, numInputs));
   SetLength(FOutBuffer, Max(2, numOutputs));
   BufferSizeChanged;
  end;

 // update size
 with VstHost[0].GetRect do
  begin
   ClientWidth := Right - Left;
   ClientHeight := Bottom - Top;
  end;

  UpdatePrograms;
end;

procedure TFmVstPresetDesigner.UpdatePrograms;
var
  i        : Integer;
  s        : string;
  temp     : AnsiString;
  MenuItem : TMenuItem;
begin
 // remove and previous program menu items
 i := MiProgram.IndexOf(MiProgramSplitter) + 1;
 while MiProgram.Count > i do MiProgram.Delete(i);

 for i := 0 to VstHost[0].numPrograms - 1 do
  begin
   VstHost[0].GetProgramNameIndexed(-1, i, temp);
   s := IntToStr(i);
   if (VstHost[0].numPrograms >= 100) and (i < 100) then s := '0' + s;
   if (VstHost[0].numPrograms >= 10) and (i < 10) then s := '0' + s;

   s := s + ' - ' + string(temp);
   MenuItem := TMenuItem.Create(MIProgram);
   with MenuItem do
    begin
     Caption := s;
     Tag := i;
     RadioItem := True;
     OnClick := MIPresetClick;
     Checked := VstHost[0].CurrentProgram = i;
    end;
   MIProgram.Add(MenuItem);
  end;
end;

procedure TFmVstPresetDesigner.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TFmVstPresetDesigner.BufferSizeChanged;
var
  Channel : Integer;
begin
 VstHost.BlockSize := FBufferSize;

 with VstHost[0] do
  begin
   for Channel := 0 to Length(FInBuffer) - 1 do ReallocMem(FInBuffer[Channel], FBufferSize);
   for Channel := 0 to Length(FOutBuffer) - 1 do ReallocMem(FOutBuffer[Channel], FBufferSize);
  end;
end;

procedure TFmVstPresetDesigner.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Index : Integer;
begin
 FMp3Player.GetSamples(FInBuffer[0], FInBuffer[1], ASIOHost.Buffersize);

 if VSTHost[0].Active then
  begin
   VSTHost[0].Process32Replacing(@FInBuffer[0], @FOutBuffer[0],
     ASIOHost.BufferSize);

   for Index := 0 to Min(2, ASIOHost.OutputChannelCount) - 1
    do Move(FOutBuffer[Index]^, OutBuffer[Index]^, ASIOHost.Buffersize * SizeOf(Single));
  end
 else
  for Index := 0 to Min(2, ASIOHost.OutputChannelCount) - 1
   do Move(FInBuffer[Index]^, OutBuffer[Index]^, ASIOHost.Buffersize * SizeOf(Single));
end;

end.
