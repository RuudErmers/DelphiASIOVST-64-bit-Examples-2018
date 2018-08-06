unit GranularPitchShifterDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspGranularPitchShifter;

type
  TGranularPitchShifterModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamSemitonesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamGranularityChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FGranularPitchShifter : array [0..1] of TDspGranularPitchShifter32;
    FCriticalSection      : TCriticalSection;
    function GetGranularPitchShifter(Index: Integer): TDspGranularPitchShifter32;
  public
    property GranularPitchShifter[Index: Integer]: TDspGranularPitchShifter32 read GetGranularPitchShifter;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  GranularPitchShifterGUI, DAV_Approximations, DAV_VSTCustomModule;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TGranularPitchShifterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TGranularPitchShifterModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TGranularPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FGranularPitchShifter) - 1 do
  begin
   FGranularPitchShifter[Channel] := TDspGranularPitchShifter32.Create;
   FGranularPitchShifter[Channel].SampleRate := SampleRate;
  end;
 Parameter[0] :=  0.01;
 Parameter[1] :=  1024 / SampleRate;
 Parameter[2] :=  2;
 Parameter[3] := 100;

 with Programs[0] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.2;
   Parameter[2] :=  5;
   Parameter[3] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.02;
   Parameter[2] :=  2;
   Parameter[3] := 50;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.04;
   Parameter[2] :=  4;
   Parameter[3] := 50;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.62;
   Parameter[2] :=  4.5;
   Parameter[3] := 50;
  end;

 // set editor form class
 EditorFormClass := TFmGranularPitchShifter;
end;

procedure TGranularPitchShifterModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FGranularPitchShifter[0]);
 FreeAndNil(FGranularPitchShifter[1]);
end;

function TGranularPitchShifterModule.GetGranularPitchShifter(Index: Integer): TDspGranularPitchShifter32;
begin
 if Index in [0..1]
  then Result := FGranularPitchShifter[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGranularPitchShifterModule.ParamSemitonesChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Semitones := Value;
  if Assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Semitones := Value;
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateSemitones;
end;

procedure TGranularPitchShifterModule.ParamStagesDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TGranularPitchShifterModule.ParamStagesChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Stages := Round(Value);
  if Assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Stages := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateStages;
end;

procedure TGranularPitchShifterModule.ParamGranularityChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Granularity := 1E-3 * Value;
  if Assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Granularity := 1E-3 * Value;
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateGranularity;
end;

procedure TGranularPitchShifterModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
 FCriticalSection.Enter;
 try
//  if Assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].Mix := 0.01 * Value;
//  if Assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].Mix := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
(*
 if EditorForm is TFmGranularPitchShifter then
  with TFmGranularPitchShifter(EditorForm)
   do UpdateMix;
*)
end;

procedure TGranularPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FGranularPitchShifter[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TGranularPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if SampleRate <= 0 then exit;
 FCriticalSection.Enter;
 try
  if Assigned(FGranularPitchShifter[0]) then FGranularPitchShifter[0].SampleRate := SampleRate;
  if Assigned(FGranularPitchShifter[1]) then FGranularPitchShifter[1].SampleRate := SampleRate;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
