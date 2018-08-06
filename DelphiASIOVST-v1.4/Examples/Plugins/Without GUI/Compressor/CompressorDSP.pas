unit CompressorDSP;

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

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TCompressorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FCompressors     : array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TCompressorDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TCompressorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FCompressors) - 1 do
  begin
   FCompressors[ChannelIndex] := TSimpleRMSCompressor.Create;
   if Abs(SampleRate) > 0
    then FCompressors[ChannelIndex].SampleRate := Abs(SampleRate);
  end;
end;

procedure TCompressorDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FCompressors) - 1
  do FreeAndNil(FCompressors[ChannelIndex]);
end;

procedure TCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressors) - 1 do
   if Assigned(FCompressors[ChannelIndex])
    then FCompressors[ChannelIndex].Threshold_dB := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex    : Integer;
  ReciprocalRatio : Double;
begin
 FCriticalSection.Enter;
 try
  ReciprocalRatio := 1 / Value;
  for ChannelIndex := 0 to Length(FCompressors) - 1 do
   if Assigned(FCompressors[ChannelIndex])
    then FCompressors[ChannelIndex].Ratio := ReciprocalRatio
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressors) - 1 do
   if Assigned(FCompressors[ChannelIndex])
    then FCompressors[ChannelIndex].Release := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressors) - 1 do
   if Assigned(FCompressors[ChannelIndex])
    then FCompressors[ChannelIndex].Attack := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   for ChannelIndex := 0 to Length(FCompressors) - 1
    do Outputs[ChannelIndex, SampleIndex] := FCompressors[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) <> 0 then
   for ChannelIndex := 0 to Length(FCompressors) - 1 do
    if Assigned(FCompressors[ChannelIndex])
     then FCompressors[ChannelIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
