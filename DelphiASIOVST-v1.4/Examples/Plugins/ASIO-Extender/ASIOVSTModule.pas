unit ASIOVSTModule;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_ASIOHost, DAV_VSTModule;

type
  TASIOVSTModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure AHBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure AHBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray);
    procedure AHShortCircuit(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIODriverChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ASIODriverDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterUseSSEMMXChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterUseSSEMMXDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAccuracyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAccuracyChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FASIOHost        : TASIOHost;
    FInBuffer        : array of PDavSingleFixedArray;
    FOutBuffer       : array of PDavSingleFixedArray;
    FIntBufSize      : Integer;
    FIntWritePos     : Integer;
    FIntReadPos      : Integer;
    FNrOfBuffers     : Integer;
    FBufferUnderruns : Integer;
    procedure AHLatencyChanged(Sender: TObject);
    procedure InternalBufferSizeChanged;
  public
    property BufferUnderruns: Integer read FBufferUnderruns;
    property AsioHost: TASIOHost read FASIOHost;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, ASIOVSTGUI;

procedure TASIOVSTModule.VSTModuleOpen(Sender: TObject);
begin
 FNrOfBuffers := 2;
 FIntWritePos := 1;
 FIntReadPos := 0;
 SetLength(FInBuffer, 2);
 SetLength(FOutBuffer, 2);

 FASIOHost := TASIOHost.Create(Self);
 with FASIOHost do
  begin
   Active := False;
   PreventClipping := pcDigital;
   PreFillInBuffer := bpfNone;
   PreFillOutBuffer := bpfNone;
//   ConvertOptimizations := [coSSE, co3DNow];
   ASIOTime.Speed := 1;
   ASIOTime.SampleRate := 44100;
   ASIOTime.Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid];
   OnBufferSwitch32 := AHBufferSwitch32;
//   OnBufferSwitch64 := AHBufferSwitch64;
   OnLatencyChanged := AHLatencyChanged;
  end;
 with ParameterProperties[0] do
  begin
   Min := 0;
   Max := FASIOHost.DriverList.Count - 1;
  end;

 EditorFormClass := TFmASIOVST;
 AHLatencyChanged(Self);

 if Assigned(FASIOHost)
  then FASIOHost.Active := True;
end;

procedure TASIOVSTModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FASIOHost)
  then FASIOHost.Active := False;

 FreeAndNil(FASIOHost);
end;

procedure TASIOVSTModule.VSTModuleBlockSizeChange(Sender: TObject;
  const BlockSize: Integer);
begin
 InternalBufferSizeChanged;
end;

procedure TASIOVSTModule.InternalBufferSizeChanged;
begin
 if Assigned(FASIOHost) then
  with FASIOHost do
   begin
    FIntBufSize := max(Integer(FASIOHost.BufferSize), BlockSize) * FNrOfBuffers;
    InitialDelay := InputLatency + OutputLatency + FIntBufSize;
   end
 else
  begin
   FIntBufSize := Integer(BlockSize) * FNrOfBuffers;
   InitialDelay := FIntBufSize;
  end;
 ReallocMem(FInBuffer[0],  FIntBufSize * SizeOf(Single));
 ReallocMem(FInBuffer[1],  FIntBufSize * SizeOf(Single));
 ReallocMem(FOutBuffer[0], FIntBufSize * SizeOf(Single));
 ReallocMem(FOutBuffer[1], FIntBufSize * SizeOf(Single));
 FIntWritePos := 0;
 FIntReadPos := FIntBufSize div 2;
end;

procedure TASIOVSTModule.AHLatencyChanged(Sender: TObject);
begin
 InternalBufferSizeChanged;
end;

procedure TASIOVSTModule.AHBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  BufferByteSize : Integer;
begin
 BufferByteSize := FASIOHost.BufferSize * SizeOf(Single);
 if (FIntWritePos > FIntReadPos) and (FIntWritePos < FIntReadPos + Integer(FASIOHost.BufferSize))
  then Inc(FBufferUnderruns);
 Move(FInBuffer[0, FIntReadPos], OutBuffer[0, 0], BufferByteSize);
 Move(FInBuffer[1, FIntReadPos], OutBuffer[1, 0], BufferByteSize);
 Move(InBuffer[0, 0], FOutBuffer[0, FIntReadPos], BufferByteSize);
 Move(InBuffer[1, 0], FOutBuffer[1, FIntReadPos], BufferByteSize);
 FIntReadPos := (FIntReadPos + Integer(FASIOHost.BufferSize)) mod (FNrOfBuffers * Integer(FASIOHost.BufferSize));
end;

procedure TASIOVSTModule.AHBufferSwitch64(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfDoubleFixedArray);
var
  BufferByteSize : Integer;
begin
 BufferByteSize := FASIOHost.BufferSize * SizeOf(Single);
 if (FIntWritePos > FIntReadPos) and (FIntWritePos < FIntReadPos + Integer(FASIOHost.BufferSize))
  then Inc(FBufferUnderruns);
 Move(FInBuffer[0, FIntReadPos], OutBuffer[0, 0], BufferByteSize);
 Move(FInBuffer[1, FIntReadPos], OutBuffer[1, 0], BufferByteSize);
 Move(InBuffer[0, 0], FOutBuffer[0, FIntReadPos], BufferByteSize);
 Move(InBuffer[1, 0], FOutBuffer[1, FIntReadPos], BufferByteSize);
 FIntReadPos := (FIntReadPos + Integer(FASIOHost.BufferSize)) mod (FNrOfBuffers * Integer(FASIOHost.BufferSize));
end;

procedure TASIOVSTModule.AHShortCircuit(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  BufferByteSize : Integer;
begin
 BufferByteSize := FASIOHost.BufferSize * SizeOf(Single);
 if (FIntWritePos > FIntReadPos) and (FIntWritePos < FIntReadPos + Integer(FASIOHost.BufferSize))
  then inc(FBufferUnderruns);
 Move(FInBuffer[0, FIntReadPos], FOutBuffer[0, FIntReadPos], BufferByteSize);
 Move(FInBuffer[1, FIntReadPos], FOutBuffer[0, FIntReadPos], BufferByteSize);
 FIntReadPos := (FIntReadPos + Integer(FASIOHost.BufferSize)) mod (FNrOfBuffers * Integer(FASIOHost.BufferSize));
end;

procedure TASIOVSTModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, DelayCnt : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FInBuffer[0, FIntWritePos] := Inputs[0, Sample];
   FInBuffer[1, FIntWritePos] := Inputs[1, Sample];
   Outputs[0, Sample] := FOutBuffer[0, FIntWritePos];
   Outputs[1, Sample] := FOutBuffer[1, FIntWritePos];
   Inc(FIntWritePos);
   if FIntWritePos >= FIntBufSize then FIntWritePos := 0;
   DelayCnt := 0;
   while (DelayCnt < 500) and (FIntWritePos = FIntReadPos) and FASIOHost.Active do
    begin
     Inc(DelayCnt);
     Sleep(1);
    end;
  end;
end;

procedure TASIOVSTModule.ASIODriverDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FASIOHost.DriverName;
end;

procedure TASIOVSTModule.ParameterAccuracyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0.5
  then FASIOHost.ConvertMethod := cm64
  else FASIOHost.ConvertMethod := cm32;
end;

procedure TASIOVSTModule.ParameterAccuracyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := '64'
  else PreDefined := '32';
end;

procedure TASIOVSTModule.ParameterUseSSEMMXDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TASIOVSTModule.ParameterUseSSEMMXChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 with FASIOHost do
  if Value > 0.5
   then ConvertOptimizations := [coSSE, co3DNow]
   else ConvertOptimizations := [];
end;

procedure TASIOVSTModule.ASIODriverChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if not Assigned(FASIOHost) then Exit;
 if FASIOHost.DriverIndex = Round(Value) then Exit;
 FASIOHost.Active := False;
 FASIOHost.DriverIndex := Round(Value);
 InitialDelay := FASIOHost.InputLatency + FASIOHost.OutputLatency + Integer(FASIOHost.BufferSize);
 if Assigned(EditorForm) then
  with TFmASIOVST(EditorForm) do
   if CbASIO.ItemIndex <> FASIOHost.DriverIndex then
    begin
     CbASIO.ItemIndex := FASIOHost.DriverIndex;
     DisplayASIOInformation;
    end;
 FASIOHost.Active := True;
end;

end.
