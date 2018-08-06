unit BarberpoleFlangerDM;

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
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspBarberpole;

type

  { TBarberpoleFlangerModule }

  TBarberpoleFlangerModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStagesDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAlgorithmDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterAlgorithmChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBarberpole      : array [0..1] of TDspBarberpole32;
    FCriticalSection : TCriticalSection;
    function GetBarberpole(Index: Integer): TDspBarberpole32;
  public
    property Barberpole[Index: Integer]: TDspBarberpole32 read GetBarberpole;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  DAV_Approximations, DAV_VSTCustomModule, BarberpoleFlangerGUI;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TBarberpoleFlangerModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TBarberpoleFlangerModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TBarberpoleFlangerModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FBarberpole[Channel] := TDspBarberpole32.Create;
   FBarberpole[Channel].SampleRate := SampleRate;
  end;

 // register editor
 EditorFormClass := TFmBarberpoleFlanger;

 // default parameters
 Parameter[0] :=  2;
 Parameter[1] :=  0.2;
 Parameter[2] :=  5;
 Parameter[3] := 50;
 Parameter[4] :=  2;

 // program parameters
 with Programs[0] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.2;
   Parameter[2] :=  5;
   Parameter[3] := 50;
   Parameter[4] :=  2;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.02;
   Parameter[2] :=  2;
   Parameter[3] := 50;
   Parameter[4] :=  2;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.04;
   Parameter[2] :=  4;
   Parameter[3] := 50;
   Parameter[4] :=  2;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.62;
   Parameter[2] :=  4.5;
   Parameter[3] := 50;
   Parameter[4] :=  2;
  end;
end;

procedure TBarberpoleFlangerModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FBarberpole[0]);
 FreeAndNil(FBarberpole[1]);
end;

function TBarberpoleFlangerModule.GetBarberpole(Index: Integer): TDspBarberpole32;
begin
 if Index in [0..1]
  then Result := FBarberpole[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarberpoleFlangerModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FBarberpole[0]) then FBarberpole[0].Speed := Value;
  if Assigned(FBarberpole[1]) then FBarberpole[1].Speed := Value;
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateSpeed;
end;

procedure TBarberpoleFlangerModule.ParamStagesDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TBarberpoleFlangerModule.ParameterAlgorithmChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FBarberpole[0]) then FBarberpole[0].Direction := TBarberpoleDirection(Round(Value));
  if Assigned(FBarberpole[1]) then FBarberpole[1].Direction := TBarberpoleDirection(Round(Value));
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateAlgorithm;
end;

procedure TBarberpoleFlangerModule.ParameterAlgorithmDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Up';
  1 : PreDefined := 'Down';
  2 : PreDefined := 'Up (Inv.)';
  3 : PreDefined := 'Down (Inv.)';
 end;
end;

procedure TBarberpoleFlangerModule.ParamStagesChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FBarberpole[0]) then FBarberpole[0].Stages := Round(Value);
  if Assigned(FBarberpole[1]) then FBarberpole[1].Stages := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateStages;
end;

procedure TBarberpoleFlangerModule.ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FBarberpole[0]) then FBarberpole[0].Depth := 0.01 * Value;
  if Assigned(FBarberpole[1]) then FBarberpole[1].Depth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateDepth;
end;

procedure TBarberpoleFlangerModule.ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FBarberpole[0]) then FBarberpole[0].Mix := 0.01 * Value;
  if Assigned(FBarberpole[1]) then FBarberpole[1].Mix := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmBarberpoleFlanger then
  with TFmBarberpoleFlanger(EditorForm)
   do UpdateMix;
end;

procedure TBarberpoleFlangerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) = 0 then exit;

 FCriticalSection.Enter;
 try
  if Assigned(FBarberpole[0]) then FBarberpole[0].SampleRate := Abs(SampleRate);
  if Assigned(FBarberpole[1]) then FBarberpole[1].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBarberpoleFlangerModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FBarberpole) - 1 do
   for Sample := 0 to SampleFrames - 1
  {$IFDEF CPU64}
    do Outputs[Channel, Sample] := FastTanhOpt5Term(FBarberpole[Channel].ProcessSample32(Inputs[Channel, Sample]))
  {$ELSE}
    do Outputs[Channel, Sample] := FastTanhOpt5TermFPU(FBarberpole[Channel].ProcessSample32(Inputs[Channel, Sample]))
  {$ENDIF}
 finally
  FCriticalSection.Leave;
 end;
end;

end.
