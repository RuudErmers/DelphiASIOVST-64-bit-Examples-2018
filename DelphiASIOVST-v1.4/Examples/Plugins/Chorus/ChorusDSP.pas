unit ChorusDSP;

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

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspChorus;

type
  TChorusModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDriftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleResume(Sender: TObject);
  private
    FChorus          : array [0..1] of TDspChorus32;
    FCriticalSection : TCriticalSection;
    function GetChorus(Index: Integer): TDspChorus32;
  public
    property Chorus[Index: Integer]: TDspChorus32 read GetChorus;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  DAV_Approximations, ChorusGUI;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TChorusModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TChorusModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TChorusModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FChorus[Channel] := TDspChorus32.Create;
   FChorus[Channel].SampleRate := SampleRate;
  end;

 // initialize parameters
 Parameter[0] :=  0.2;
 Parameter[1] :=  4;
 Parameter[2] :=  5;
 Parameter[3] := 50;
 Parameter[4] :=  8;
 with Programs[0] do
  begin
   Parameter[0] :=  0.2;
   Parameter[1] :=  4;
   Parameter[2] :=  5;
   Parameter[3] := 50;
   Parameter[4] :=  8;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  0.02;
   Parameter[1] :=  2;
   Parameter[2] :=  2;
   Parameter[3] := 50;
   Parameter[4] :=  0.2;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  0.04;
   Parameter[1] :=  4;
   Parameter[2] :=  4;
   Parameter[3] := 50;
   Parameter[4] :=  0.4;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  0.62;
   Parameter[1] :=  4;
   Parameter[2] :=  4.5;
   Parameter[3] := 50;
   Parameter[4] :=  19.8;
  end;
 with Programs[4] do
  begin
   Parameter[0] :=  1.3;
   Parameter[1] :=  6;
   Parameter[2] :=  9.5;
   Parameter[3] := 77;
   Parameter[4] := 16.8;
  end;
 with Programs[5] do
  begin
   Parameter[0] :=  2.5;
   Parameter[1] :=  1;
   Parameter[2] :=  25;
   Parameter[3] :=  25;
   Parameter[4] :=  25;
  end;
 with Programs[6] do
  begin
   Parameter[0] :=  2.5;
   Parameter[1] :=  1;
   Parameter[2] :=  25;
   Parameter[3] :=  25;
   Parameter[4] :=  25;
  end;
 with Programs[7] do
  begin
   Parameter[0] :=  0.33;
   Parameter[1] :=  8;
   Parameter[2] :=  57;
   Parameter[3] :=  15;
   Parameter[4] :=  25;
  end;
 with Programs[8] do
  begin
   Parameter[0] :=  0.33;
   Parameter[1] :=  8;
   Parameter[2] :=  100;
   Parameter[3] :=  72.7;
   Parameter[4] :=  33.6;
  end;

  EditorFormClass := TFmChorus;
end;

procedure TChorusModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FChorus[0]);
 FreeAndNil(FChorus[1]);
end;

function TChorusModule.GetChorus(Index: Integer): TDspChorus32;
begin
 if Index in [0..1]
  then Result := FChorus[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TChorusModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChorus[0]) then FChorus[0].Speed := Value;
  if Assigned(FChorus[1]) then FChorus[1].Speed := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmChorus then
  with TFmChorus(EditorForm)
   do UpdateSpeed;
end;

procedure TChorusModule.ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChorus[0]) then FChorus[0].Stages := Round(Value);
  if Assigned(FChorus[1]) then FChorus[1].Stages := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmChorus then
  with TFmChorus(EditorForm)
   do UpdateStages;
end;

procedure TChorusModule.ParamDriftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChorus[0]) then FChorus[0].Drift := 0.01 * Value;
  if Assigned(FChorus[1]) then
   if Value > 0
    then FChorus[1].Drift := 0.01 * Value
    else for i := 0 to FChorus[1].Stages - 1
          do FChorus[1].LFO[i].Assign(FChorus[0].LFO[i]);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmChorus then
  with TFmChorus(EditorForm)
   do UpdateDrift;
end;

procedure TChorusModule.ParamDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChorus[0]) then FChorus[0].Depth := 0.01 * Value;
  if Assigned(FChorus[1]) then FChorus[1].Depth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmChorus then
  with TFmChorus(EditorForm)
   do UpdateDepth;
end;

procedure TChorusModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChorus[0]) then FChorus[0].Mix := 0.01 * Value;
  if Assigned(FChorus[1]) then FChorus[1].Mix := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmChorus then
  with TFmChorus(EditorForm)
   do UpdateMix;
end;

procedure TChorusModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhContinousError4(FChorus[Channel].ProcessSample32(Inputs[Channel, Sample]))
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TChorusModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhContinousError4(FChorus[Channel].ProcessSample32(Inputs[Channel, Sample]))
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TChorusModule.VSTModuleResume(Sender: TObject);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChorus[0]) then FChorus[0].Reset;
  if Assigned(FChorus[1]) then FChorus[1].Reset;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TChorusModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   FCriticalSection.Enter;
   try
    if Assigned(FChorus[0]) then FChorus[0].SampleRate := SampleRate;
    if Assigned(FChorus[1]) then FChorus[1].SampleRate := SampleRate;
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

end.
