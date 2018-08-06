unit PlateReverbModule;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspPlateReverb;

type
  TPlateReverbVST = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessReplacing(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPreDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInputDiffusionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChangeDiffusion(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModulationChange(Sender: TObject; const Index: Integer; var Value: Single);
  protected
    FPlateReverb     : TPlateReverb;
    FCrossover       : Single;
    FState           : Single;
    FMix             : array [0..1] of Single;
    FCriticalSection : TCriticalSection;
  public
    property Dry: Single read FMix[0] write FMix[0];
    property Wet: Single read FMix[1] write FMix[1];
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_VSTCustomModule, PlateReverbGUI;

procedure TPlateReverbVST.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TPlateReverbVST.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TPlateReverbVST.VSTModuleOpen(Sender: TObject);
begin
 FPlateReverb := TPlateReverb.Create;
 FPlateReverb.SampleRate := SampleRate;
 FCrossover := 0.01;

 // default parameter
 Parameter[0] := 50;
 Parameter[1] := 50;
 Parameter[2] := 10;
 Parameter[3] := 50;
 Parameter[4] := 13000;
 Parameter[5] := 75;
 Parameter[6] := 70;
 Parameter[7] := 50;

 // default preset
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);

(*
 // preset 1
 with Programs[1] do
  begin
   Parameter[0] := 0.5;
   Parameter[1] := 0.6;
   Parameter[2] := 0.4;
   Parameter[3] := 0.5;
   Parameter[4] := 0;
   Parameter[5] := 0;
   Parameter[6] := 1;
  end;

 // preset 2
 with Programs[2] do
  begin
   Parameter[0] := 0.2;
   Parameter[1] := 0.6;
   Parameter[2] := 0.8;
   Parameter[3] := 1;
   Parameter[4] := 0;
   Parameter[5] := 1;
   Parameter[6] := 1;
  end;
*)

 // preset 3
 with Programs[3] do
  begin
   Parameter[0] := 100 * Random;
   Parameter[1] := 100 * Random;
   Parameter[2] := 100 * Random;
   Parameter[3] := 100 * Random;
   Parameter[4] := 100 * Random;
   Parameter[5] := 100 * Random;
   Parameter[6] := 100 * Random;
   Parameter[7] := 100 * Random;
  end;

 // set editor form class
 EditorFormClass := TFmPlateReverb;
end;

procedure TPlateReverbVST.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FPlateReverb);
end;

procedure TPlateReverbVST.ParameterDryChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry := 0.01 * Value;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDry;
end;

procedure TPlateReverbVST.ParameterWetChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet := 0.01 * Value;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateWet;
end;

procedure TPlateReverbVST.ParameterModulationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.Modulation := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateModulation;
end;

procedure TPlateReverbVST.ParameterPreDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.PreDelay := 1E-3 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdatePreDelay;
end;

procedure TPlateReverbVST.ParameterDecayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.Decay := 0.0025 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDecay;
end;

procedure TPlateReverbVST.ParameterDampingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.DampingFrequency := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDampingFrequency;
end;

procedure TPlateReverbVST.ParameterInputDiffusionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.InputDiffusion := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateInputDiffusion;
end;

procedure TPlateReverbVST.ParameterDecayChangeDiffusion(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.DecayDiffusion := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPlateReverb
  then TFmPlateReverb(EditorForm).UpdateDecayDiffusion;
end;

procedure TPlateReverbVST.VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex: Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FState := FPlateReverb.ProcessSample32(FCrossover * FState + (1 - FCrossover) * (Inputs[0, SampleIndex] + Inputs[1, SampleIndex]) * CHalf32);

    // Calculate output MIXING with anything already there
    Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + FMix[0] * Inputs[0, SampleIndex] + FMix[1] * FPlateReverb.OutputLeft;
    Outputs[1, SampleIndex] := Outputs[1, SampleIndex] + FMix[0] * Inputs[1, SampleIndex] + FMix[1] * FPlateReverb.OutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TPlateReverbVST.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex: Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FState := FPlateReverb.ProcessSample32(FCrossover * FState + (1 - FCrossover) * (Inputs[0, SampleIndex] + Inputs[1, SampleIndex]) * CHalf32);

    // Calculate output REPLACING with anything already there
    Outputs[0, SampleIndex] := FMix[0] * Inputs[0, SampleIndex] + FMix[1] * FPlateReverb.OutputLeft;
    Outputs[1, SampleIndex] := FMix[0] * Inputs[1, SampleIndex] + FMix[1] * FPlateReverb.OutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TPlateReverbVST.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 if SampleRate = 0 then exit;
 FCriticalSection.Enter;
 try
  if Assigned(FPlateReverb)
   then FPlateReverb.SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
