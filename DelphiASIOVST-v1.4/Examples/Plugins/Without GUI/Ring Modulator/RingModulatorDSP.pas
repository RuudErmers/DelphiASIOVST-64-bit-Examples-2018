unit RingModulatorDSP;

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
  SyncObjs, Forms, Registry, DAV_Types, DAV_VSTModule, DAV_DspRingModulator;

type
  TRingModulatorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMultiChannel(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FCriticalSection : TCriticalSection;
    FRingMod         : array of TAutoRingModulator32;
    procedure ChooseProcess;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_VSTParameters;

const
  CRegKeyRoot = 'Software\Delphi ASIO & VST Project\Ring Modulator';

procedure TRingModulatorDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TRingModulatorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TRingModulatorDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  UpperFreq    : Single;
begin
 Assert(numInputs = numOutputs);
 Assert(numInputs > 0);

 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegKeyRoot, False) then
    begin
     if ValueExists('Upper Frequency') then
      begin
       UpperFreq := StrToFloat(ReadString('Upper Frequency'));
       if UpperFreq > ParameterProperties[0].Min
        then ParameterProperties[0].Max := UpperFreq;
      end;
    end
  finally
   Free;
  end;

 SetLength(FRingMod, numInputs);

 ChooseProcess;

 for ChannelIndex := 0 to Length(FRingMod) - 1
  do FRingMod[ChannelIndex] := TAutoRingModulator32.Create;

 Parameter[0] := 10;

 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
end;

procedure TRingModulatorDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FRingMod) - 1
  do FreeAndNil(FRingMod[ChannelIndex]);
end;

procedure TRingModulatorDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TRingModulatorDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FRingMod) - 1 do
   if Assigned(FRingMod[ChannelIndex])
    then FRingMod[ChannelIndex].Frequency := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TRingModulatorDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := FloatToAnsiString(Parameter[Index], 4)
  else PreDefined := FloatToAnsiString(0.001 * Parameter[Index], 4);
end;

procedure TRingModulatorDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := 'Hz'
  else PreDefined := 'kHz';
end;

procedure TRingModulatorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for ChannelIndex := 0 to Length(FRingMod) - 1 do
    if Assigned(FRingMod[ChannelIndex])
     then FRingMod[ChannelIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TRingModulatorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1
   do Outputs[0, Sample] := FRingMod[0].ProcessSample32(Inputs[0, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TRingModulatorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FRingMod[0].ProcessSample32(Inputs[0, Sample]);
    Outputs[1, Sample] := FRingMod[1].ProcessSample32(Inputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TRingModulatorDataModule.VSTModuleProcessMultiChannel(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FRingMod) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, Sample] := FRingMod[ChannelIndex].ProcessSample32(Inputs[ChannelIndex, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
