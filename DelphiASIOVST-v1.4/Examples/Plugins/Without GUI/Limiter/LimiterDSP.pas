unit LimiterDSP;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF} SysUtils, Classes,
  SyncObjs, Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TLimiterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FLimiters        : array [0..1] of TLimiter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TLimiterDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLimiterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLimiterDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FLimiters) - 1 do
  begin
   FLimiters[ChannelIndex] := TLimiter.Create;
   FLimiters[ChannelIndex].SampleRate := SampleRate;
  end;

 Parameter[0] := -10;
end;

procedure TLimiterDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FLimiters) - 1
  do FreeAndNil(FLimiters[ChannelIndex]);
end;

procedure TLimiterDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FLimiters) - 1 do
   if Assigned(FLimiters[ChannelIndex])
    then FLimiters[ChannelIndex].Threshold_dB := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FLimiters) - 1 do
   if Assigned(FLimiters[ChannelIndex])
    then FLimiters[ChannelIndex].Release := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLimiterDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FLimiters) - 1 do
   if Assigned(FLimiters[ChannelIndex])
    then FLimiters[ChannelIndex].Attack := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLimiterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   for ChannelIndex := 0 to Length(FLimiters) - 1
    do Outputs[ChannelIndex, SampleIndex] := FLimiters[ChannelIndex].ProcessSample32(Inputs[ChannelIndex, SampleIndex]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for ChannelIndex := 0 to Length(FLimiters) - 1 do
    if Assigned(FLimiters[ChannelIndex])
     then FLimiters[ChannelIndex].SampleRate := SampleRate;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
