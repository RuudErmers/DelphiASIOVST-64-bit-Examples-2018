unit ASIOMP3VSTGUI;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, Menus, ActnList, ToolWin, 
  ActnMan, ActnCtrls, ActnMenus, PlatformDefaultStyleActnCtrls, StdActns, 
  ComCtrls, ImgList, ExtCtrls, DAV_Types, DAV_AsioHost, DAV_MpegAudio, 
  DAV_DspBufferedMp3Player, DAV_VSTHost;

type
  TFmASIOMP3VST = class(TForm)
    AcAsioSettings: TAction;
    AcFileExit: TFileExit;
    AcFileOpenMP3: TFileOpen;
    AcFileOpenVST: TFileOpen;
    ActionManager: TActionManager;
    ASIOHost: TASIOHost;
    CoolBar: TCoolBar;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionToolBar: TActionToolBar;
    AcPlay: TAction;
    ImageList: TImageList;
    VstHost: TVstHost;
    PnVSTPlugin: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AcAsioSettingsExecute(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure AcFileOpenVSTAccept(Sender: TObject);
    procedure AcPlayExecute(Sender: TObject);
    procedure AcFileOpenMP3Accept(Sender: TObject);
    procedure ASIOHostDriverChanged(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
  private
    FBufferedPlayer : TBufferedMP3FilePlayer;
    FChannelOffset  : Integer;
    FVolumeFactor   : Single;
    FVstFileName    : TFileName;
    FVstBuffers     : array [0..1, 0..1] of PDAVSingleFixedArray;
    procedure LoadVSTPlugin(FileName: TFileName);
    procedure LoadMP3File(FileName: TFileName);
  public
    property OutputChannelOffset: Integer read FChannelOffset write FChannelOffset;
    property Volume: Single read FVolumeFactor write FVolumeFactor;
  end;

var
  FmASIOMP3VST: TFmASIOMP3VST;

implementation

{$R *.dfm}

uses
  IniFiles, DAV_VSTEffect, ASIOMP3VSTSetup;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFmASIOMP3VST.FormCreate(Sender: TObject);
begin
 if ASIOHost.DriverList.Count = 0 then
  try
   raise Exception.Create(RCStrNoASIODriverPresent);
  except
   on E: Exception do
    begin
     MessageDlg(E.Message , mtError, [mbOK], 0);
     Application.Terminate;
    end;
  end;

 FVolumeFactor := 1;
 FChannelOffset := 0;

 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 with FBufferedPlayer do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
   Pitch := 0;
   Interpolation := biBSpline6Point5thOrder;
   SampleRate := ASIOHost.SampleRate;
  end;
end;

procedure TFmASIOMP3VST.FormDestroy(Sender: TObject);
begin
 Dispose(FVstBuffers[0, 0]);
 Dispose(FVstBuffers[0, 1]);
 Dispose(FVstBuffers[1, 0]);
 Dispose(FVstBuffers[1, 1]);
 FreeAndNil(FBufferedPlayer);
end;

procedure TFmASIOMP3VST.FormShow(Sender: TObject);
var
  Index : Integer;
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIOMP3VST.INI') do
  try
   // load MP3 file
   FBufferedPlayer.Filename := ReadString('Audio', 'MP3 File', '');
   AcPlay.Enabled := FileExists(FBufferedPlayer.Filename);

   // load VST plugin
   FVstFileName := ReadString('Audio', 'VST Plugin', FVstFileName);
   if FileExists(FVstFileName)
    then LoadVSTPlugin(FVstFileName);

   with FmSetup do
    begin
     CBDrivers.Items := FmASIOMP3VST.ASIOHost.DriverList;
     CBDrivers.ItemIndex := CBDrivers.Items.IndexOf(ReadString('Audio', 'ASIO Driver', 'ASIO4ALL v2'));
     if CBDrivers.ItemIndex >= 0 then
      begin
       ASIOHost.DriverIndex := CBDrivers.ItemIndex;
       CBOutput.Clear;
       for Index := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
         CBOutput.Items.Add(
           string(ASIOHost.OutputChannelInfos[2 * Index].Name) + ' / ' +
           string(ASIOHost.OutputChannelInfos[2 * Index + 1].Name));

       BtControlPanel.Enabled := True;
      end;
     CBOutput.ItemIndex := ReadInteger('Audio', 'Output Channel Offset', 0);
    end;
  finally
   Free;
  end;
end;

procedure TFmASIOMP3VST.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIOMP3VST.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteString('Audio', 'MP3 File', FBufferedPlayer.Filename);
   WriteString('Audio', 'ASIO Driver', ASIOHost.DriverName);
   WriteString('Audio', 'VST Plugin', FVstFileName);
   WriteInteger('Audio', 'Output Channel Offset', FChannelOffset);
  finally
   Free;
  end;

 ASIOHost.Active := False;
 VSTHost[0].Close;
end;

procedure TFmASIOMP3VST.LoadVSTPlugin(FileName: TFileName);
var
  rct : TRect;
begin
 with VstHost[0] do
  begin
   LoadFromFile(FileName);
   Active := True;
   Idle;
   ShowEdit(PnVSTPlugin);
   Idle;
   EditIdle;
   Caption := 'MP3 ASIO & VST Host' + GetVendorString + ' ' + GetEffectName;
  end;

 if (effFlagsHasEditor in VstHost[0].EffectOptions) then
  begin
   rct := VstHost[0].GetRect;
   ClientWidth := rct.Right - rct.Left;
   ClientHeight := rct.Bottom - rct.Top + ActionToolBar.Height;
  end;
end;


procedure TFmASIOMP3VST.LoadMP3File(FileName: TFileName);
begin
 FBufferedPlayer.Filename := Filename;
 AcPlay.Enabled := FileExists(FBufferedPlayer.Filename);
end;

procedure TFmASIOMP3VST.AcAsioSettingsExecute(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmASIOMP3VST.AcFileOpenMP3Accept(Sender: TObject);
begin
 if FileExists(AcFileOpenMP3.Dialog.FileName)
  then LoadMP3File(AcFileOpenMP3.Dialog.FileName);
end;

procedure TFmASIOMP3VST.AcFileOpenVSTAccept(Sender: TObject);
begin
 FVstFileName := AcFileOpenVST.Dialog.FileName;
 if FileExists(FVstFileName)
  then LoadVSTPlugin(FVstFileName);
end;

procedure TFmASIOMP3VST.AcPlayExecute(Sender: TObject);
begin
 if AcPlay.Caption = '&Play' then
  begin
   ASIOHost.Active := True;
   AcPlay.Caption := '&Stop';
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedPlayer.Reset;
   AcPlay.Caption := '&Play';
  end;
end;

procedure TFmASIOMP3VST.ASIOHostDriverChanged(Sender: TObject);
begin
 ReallocMem(FVstBuffers[0, 0], ASIOHost.BufferSize * SizeOf(Single));
 ReallocMem(FVstBuffers[0, 1], ASIOHost.BufferSize * SizeOf(Single));
 ReallocMem(FVstBuffers[1, 0], ASIOHost.BufferSize * SizeOf(Single));
 ReallocMem(FVstBuffers[1, 1], ASIOHost.BufferSize * SizeOf(Single));
end;

procedure TFmASIOMP3VST.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if Assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmASIOMP3VST.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Index : Integer;
begin
 FBufferedPlayer.GetSamples(FVstBuffers[0, 0], FVstBuffers[0, 1], ASIOHost.Buffersize);

 if VSTHost[0].Active then
  begin
   VSTHost[0].Process32Replacing(@FVstBuffers[0, 0], @FVstBuffers[1, 0],
     ASIOHost.BufferSize);

   Move(FVstBuffers[1, 0]^, OutBuffer[0]^, ASIOHost.Buffersize * SizeOf(Single));
   Move(FVstBuffers[1, 1]^, OutBuffer[1]^, ASIOHost.Buffersize * SizeOf(Single));
  end
 else
  begin
   Move(FVstBuffers[0, 0]^, OutBuffer[0]^, ASIOHost.Buffersize * SizeOf(Single));
   Move(FVstBuffers[0, 1]^, OutBuffer[1]^, ASIOHost.Buffersize * SizeOf(Single));
  end;
end;

end.
