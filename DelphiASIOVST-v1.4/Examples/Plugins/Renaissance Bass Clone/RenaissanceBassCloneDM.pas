unit RenaissanceBassCloneDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspPsychoacousticBassEnhancer;

type
  TResurrectionBassCloneModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIntensityChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterdBDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAddOriginalBassDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAddOriginalBassChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FRenaissanceBass : array [0..1] of TResurrectionBass;
    FCriticalSection : TCriticalSection;
  public
  end;

implementation

uses
  Math, DAV_Common, DAV_Approximations, RenaissanceBassCloneGUI;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TResurrectionBassCloneModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TResurrectionBassCloneModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TResurrectionBassCloneModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // create & setup upward compressor
 for Channel := 0 to Length(FRenaissanceBass) - 1 do
  begin
   FRenaissanceBass[Channel] := TResurrectionBass.Create;
   FRenaissanceBass[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 80;
 Parameter[1] := 1;
 Parameter[2] := 20;
 Parameter[3] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 80;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[2] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[3] do
  begin
   Parameter[0] := 85;
   Parameter[1] := 1;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;
 with Programs[4] do
  begin
   Parameter[0] := 75;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 1;
  end;

 // set editor form
 EditorFormClass := TFmRenaissanceBassClone;
end;

procedure TResurrectionBassCloneModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FRenaissanceBass) - 1
  do FreeAndNil(FRenaissanceBass[Channel]);
end;

procedure TResurrectionBassCloneModule.ParameterAddOriginalBassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TResurrectionBassCloneModule.ParameterdBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] = 0
  then PreDefined := '-oo'
  else PreDefined := FloatToStrF(RoundTo(Amp_to_dB(Parameter[Index]), -2), ffGeneral, 3, 3);
end;

procedure TResurrectionBassCloneModule.ParameterAddOriginalBassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if Assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].AddOriginalBass := Boolean(Round(Value));
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.ParameterIntensityChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if Assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].Intensity := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if Assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].Gain := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   if Assigned(FRenaissanceBass[Channel])
    then FRenaissanceBass[Channel].Frequency := Value;
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for Channel := 0 to Length(FRenaissanceBass) - 1 do
    if Assigned(FRenaissanceBass[Channel])
     then FRenaissanceBass[Channel].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel   : Integer;
  Sample    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FRenaissanceBass[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

procedure TResurrectionBassCloneModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel   : Integer;
  Sample    : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FRenaissanceBass) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FRenaissanceBass[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Release;
 end;
end;

end.
