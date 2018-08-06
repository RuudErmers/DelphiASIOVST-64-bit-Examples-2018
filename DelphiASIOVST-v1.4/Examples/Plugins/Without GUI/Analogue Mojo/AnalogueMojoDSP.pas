unit AnalogueMojoDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspTransformerSimulation;

type
  TAnalogueMojoDM = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMulti(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParameterDampingDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FCriticalSection : TCriticalSection;
    FTransformator   : array of TTransformatorSimulation;
    procedure ChooseProcess;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TAnalogueMojoDM.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FTransformator, numInputs);

 ChooseProcess;

 for ChannelIndex := 0 to Length(FTransformator) - 1
  do FTransformator[ChannelIndex] := TTransformatorSimulation.Create;

 // initialize parameter
 Parameter[0] := 12;
end;

procedure TAnalogueMojoDM.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FTransformator) - 1
  do FreeAndNil(FTransformator[ChannelIndex]);
end;

procedure TAnalogueMojoDM.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TAnalogueMojoDM.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TAnalogueMojoDM.ParameterDampingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
  PreDefined := AnsiString(FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 3, 3));
end;

procedure TAnalogueMojoDM.ChooseProcess;
begin
 case Length(FTransformator) of
  1 : OnProcess := VSTModuleProcessMono;
  2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMulti;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TAnalogueMojoDM.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FTransformator) - 1 do
   if Assigned(FTransformator[ChannelIndex])
    then FTransformator[ChannelIndex].HighpassFrequency := (1 + 0.02 * ChannelIndex) * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAnalogueMojoDM.ParameterDampingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FTransformator) - 1 do
   if Assigned(FTransformator[ChannelIndex])
    then FTransformator[ChannelIndex].DampingFrequency := (1 - 0.02 * ChannelIndex) * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAnalogueMojoDM.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for ChannelIndex := 0 to Length(FTransformator) - 1 do
    if Assigned(FTransformator[ChannelIndex])
     then FTransformator[ChannelIndex].SampleRate := SampleRate;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAnalogueMojoDM.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1
   do Outputs[0, SampleIndex] := FTransformator[0].ProcessSample64(Inputs[0, SampleIndex])
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAnalogueMojoDM.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
const
  CCrossTalkCoeff : array [0..1, 0..1] of Single = (
    (0.033, 0.967), (0.034, 0.967));
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Outputs[0, SampleIndex] := FTransformator[0].ProcessSample64(CCrossTalkCoeff[0, 0] * Inputs[1, SampleIndex] + CCrossTalkCoeff[0, 1] * Inputs[0, SampleIndex]);
    Outputs[1, SampleIndex] := FTransformator[1].ProcessSample64(CCrossTalkCoeff[1, 0] * Inputs[0, SampleIndex] + CCrossTalkCoeff[1, 1] * Inputs[1, SampleIndex]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAnalogueMojoDM.VSTModuleProcessMulti(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FTransformator) - 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FTransformator[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex])
 finally
  FCriticalSection.Leave;
 end;
end;

end.
