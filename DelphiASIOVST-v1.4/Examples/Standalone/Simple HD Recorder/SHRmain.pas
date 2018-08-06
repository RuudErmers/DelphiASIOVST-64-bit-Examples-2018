unit SHRmain;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Forms, Controls, ExtCtrls, Dialogs, Graphics, StdCtrls, DAV_Types, 
  DAV_ASIOHost, DAV_GuiPixelMap, DAV_GuiCommon, DAV_GuiGraphicControl, 
  DAV_GuiLabel, DAV_GuiButton, DAV_GuiLED;

type
  TStorageThread = class(TThread)
  private
    FCurrentIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean = True);
  end;

  TFmSimpleHDRecorder = class(TForm)
    ASIOHost: TASIOHost;
    BtExit: TGuiButton;
    BtSetup: TGuiButton;
    BtStartStop: TGuiButton;
    GuiLED1: TGuiLED;
    GuiLED2: TGuiLED;
    GuiLED3: TGuiLED;
    GuiLED4: TGuiLED;
    LbMic1: TGuiLabel;
    LbMic2: TGuiLabel;
    LbMic3: TGuiLabel;
    LbMic4: TGuiLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostBufferSwitch32Recording(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtExitClick(Sender: TObject);
    procedure BtSetupClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground      : TGuiCustomPixelMap;
    FChannelOffsets  : array [0..1] of Integer;
    FPeaks           : array [0..3] of Single;
    FDataBuffers     : array [0..1, 0..3] of PDAVSingleFixedArray;
    FDataBufferIndex : Integer;
    FDataBufferPos   : Integer;
    FDataBufferSize  : Integer;
    FRecordTimeInSec : Integer;
    FStorageThread   : TStorageThread;
    procedure PrepareRecording;
    procedure SwitchRecordingBuffer;
    procedure StopRecording;
  public
    procedure StoreData(const Index: Integer);
    property DataBufferIndex: Integer read FDataBufferIndex;
    property InputChannelOffset: Integer read FChannelOffsets[0] write FChannelOffsets[0];
    property OutputChannelOffset: Integer read FChannelOffsets[1] write FChannelOffsets[1];
  end;

var
  FmSimpleHDRecorder: TFmSimpleHDRecorder;

implementation

uses
  Math, IniFiles, DAV_AudioData, DAV_AudioFileWAV, DAV_AudioFileAIFF,
  DAV_AudioFileAU, WaveIOX, SHRSetup;

{$R *.dfm}

{ StorageThread }

constructor TStorageThread.Create(CreateSuspended: Boolean);
begin
 inherited Create(CreateSuspended);
 FCurrentIndex := 0;
end;

procedure TStorageThread.Execute;
begin
 while not Terminated do
  begin
   if FCurrentIndex <> FmSimpleHDRecorder.DataBufferIndex then
    begin
     FCurrentIndex := FmSimpleHDRecorder.DataBufferIndex;
     try
      FmSimpleHDRecorder.StoreData(1 - FCurrentIndex);
     finally
      Suspend;
     end;
    end;
   Sleep(10);
  end;
end;


{ TFmSimpleHDRecorder }

procedure TFmSimpleHDRecorder.ASIOHostBufferSwitch32Recording(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample, Channel, AsioChannel : Integer;
begin
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   for Channel := 0 to 3 do
    begin
     AsioChannel := (FChannelOffsets[0] + Channel) mod ASIOHost.InputChannelCount;
     FPeaks[Channel] := 0.999 * FPeaks[Channel];
     if Abs(InBuffer[AsioChannel, Sample]) > FPeaks[Channel]
      then FPeaks[Channel] := Abs(InBuffer[AsioChannel, Sample]);
     FDataBuffers[FDataBufferIndex, Channel]^[FDataBufferPos] := InBuffer[AsioChannel, Sample];
    end;
   Inc(FDataBufferPos);
   if FDataBufferPos >= FDataBufferSize then SwitchRecordingBuffer;
  end;
end;

procedure TFmSimpleHDRecorder.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample, Channel, AsioChannel : Integer;
begin
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  for Channel := 0 to 3 do
   begin
    AsioChannel := (FChannelOffsets[0] + Channel) mod ASIOHost.InputChannelCount;
    FPeaks[Channel] := 0.999 * FPeaks[Channel];
    if abs(InBuffer[AsioChannel, Sample]) > FPeaks[Channel]
     then FPeaks[Channel] := Abs(InBuffer[AsioChannel, Sample]);
   end;
end;

procedure TFmSimpleHDRecorder.BtExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmSimpleHDRecorder.BtSetupClick(Sender: TObject);
begin
 FmSetup.ShowModal;
end;

procedure TFmSimpleHDRecorder.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = 'Start' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := 'Stop';
   Caption := 'Simple HD Recorder (running)';
   PrepareRecording;
  end
 else
  begin
   ASIOHost.Active := False;
   BtStartStop.Caption := 'Start';
   Caption := 'Simple HD Recorder';
   StopRecording;
  end;
end;

procedure TFmSimpleHDRecorder.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
 FormResize(Sender);

 FPeaks[0] := 0; // reset peaks
 FPeaks[1] := 0; // reset peaks
 FPeaks[2] := 0; // reset peaks
 FPeaks[3] := 0; // reset peaks
end;

procedure TFmSimpleHDRecorder.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
 if assigned(FStorageThread) then
  begin
   if FStorageThread.Suspended then FStorageThread.Resume;
   FStorageThread.Terminate;
   FStorageThread.WaitFor;
   FreeAndNil(FStorageThread);
  end;
 Dispose(FDataBuffers[0, 0]);
 Dispose(FDataBuffers[0, 1]);
 Dispose(FDataBuffers[0, 2]);
 Dispose(FDataBuffers[0, 3]);
 Dispose(FDataBuffers[1, 0]);
 Dispose(FDataBuffers[1, 1]);
 Dispose(FDataBuffers[1, 2]);
 Dispose(FDataBuffers[1, 3]);
end;

procedure TFmSimpleHDRecorder.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSimpleHDRecorder.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  b      : ShortInt;
  ScnLn  : PPixel32Array;
  h, hr  : Single;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   hr   := 1 / Height;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.6 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * Random - 1);
       b := Round($3F + $18 * (h + s[1]));
       s[0] := s[1];
       ScnLn[x].B := b;
       ScnLn[x].G := b;
       ScnLn[x].R := b;
      end;
    end;
  end;
end;

procedure TFmSimpleHDRecorder.PrepareRecording;
begin
 FDataBufferSize := round(ASIOHost.SampleRate * 10);
 ReallocMem(FDataBuffers[0, 0], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[0, 1], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[0, 2], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[0, 3], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 0], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 1], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 2], FDataBufferSize * SizeOf(Single));
 ReallocMem(FDataBuffers[1, 3], FDataBufferSize * SizeOf(Single));
 FDataBufferIndex := 0;
 FDataBufferPos := 0;

 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + IniFileName) do
  try
   if ReadBool('Application', 'Use Threads', True) then
    begin
     if assigned(FStorageThread) then
      begin
       if FStorageThread.Suspended then FStorageThread.Resume;
       FStorageThread.Terminate;
       FStorageThread.WaitFor;
       FreeAndNil(FStorageThread);
      end;
     FStorageThread := TStorageThread.Create;
    end;
   FRecordTimeInSec := ReadInteger('Application', 'Block Time', 10);
  finally
   Free;
  end;
 ASIOHost.OnBufferSwitch32 := ASIOHostBufferSwitch32Recording;
end;

procedure TFmSimpleHDRecorder.StoreData(const Index: Integer);
var
  Y, H, M, D, S, MO : Word;
  FileBaseName   : TFileName;
begin
 DecodeTime(Now, H, M, S, MO);
 DecodeDate(Now, Y, MO, D);
 FileBaseName := 'Rec' + IntToStr(Y) + '-' + IntToStr(MO) + '-' +
   IntToStr(D) + '-' + IntToStr(H)+ '-' + IntToStr(M) + '-' + IntToStr(S);
 SaveWAVFile(FileBaseName + '-Ch1.wav', @FDataBuffers[Index, 0]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
 SaveWAVFile(FileBaseName + '-Ch2.wav', @FDataBuffers[Index, 1]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
 SaveWAVFile(FileBaseName + '-Ch3.wav', @FDataBuffers[Index, 2]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
 SaveWAVFile(FileBaseName + '-Ch4.wav', @FDataBuffers[Index, 3]^[0], round(ASIOHost.SampleRate), 1, 16, FDataBufferSize);
end;

procedure TFmSimpleHDRecorder.SwitchRecordingBuffer;
begin
 FDataBufferIndex := 1 - FDataBufferIndex;
 FDataBufferPos := 0;
 if not assigned(FStorageThread)
  then StoreData(FDataBufferIndex)
  else FStorageThread.Resume;
end;

procedure TFmSimpleHDRecorder.StopRecording;
var
  Y, H, M, D, MO : Word;
  FileBaseName   : TFileName;
begin
 DecodeTime(Now, H, M, D, MO);
 DecodeDate(Now, Y, MO, D);
 FileBaseName := 'Rec' + IntToStr(Y) + '-' + IntToStr(MO) + '-' +
   IntToStr(D) + '-' + IntToStr(H)+ '-' + IntToStr(M);
 SaveWAVFile(FileBaseName + '-Ch1.wav', FDataBuffers[FDataBufferIndex, 0], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
 SaveWAVFile(FileBaseName + '-Ch2.wav', FDataBuffers[FDataBufferIndex, 1], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
 SaveWAVFile(FileBaseName + '-Ch3.wav', FDataBuffers[FDataBufferIndex, 2], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
 SaveWAVFile(FileBaseName + '-Ch4.wav', FDataBuffers[FDataBufferIndex, 3], round(ASIOHost.SampleRate), 1, 16, FDataBufferIndex);
end;

procedure TFmSimpleHDRecorder.TimerTimer(Sender: TObject);
begin
 GuiLED1.Brightness_Percent := FPeaks[0] * 100;
 GuiLED2.Brightness_Percent := FPeaks[1] * 100;
 GuiLED3.Brightness_Percent := FPeaks[2] * 100;
 GuiLED4.Brightness_Percent := FPeaks[3] * 100;
end;

end.
