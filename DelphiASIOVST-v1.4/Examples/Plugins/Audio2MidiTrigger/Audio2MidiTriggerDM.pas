unit Audio2MidiTriggerDM;

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
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  DAV_Types, DAV_VSTModule, DAV_DspAudioToMidiTrigger;

type
  TAudio2MidiTriggerModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMidiNoteDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterMidiNoteChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIntervalChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterVelocityShiftChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FAudio2MidiTrigger : TAudio2MidiTrigger;
    FMidiNote          : Byte;
    FVelocityShift     : Byte;
    procedure MidiTrigger(Sender: TObject; const Level: Single);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_VSTBasicModule;

procedure TAudio2MidiTriggerModule.VSTModuleOpen(Sender: TObject);
begin
 FAudio2MidiTrigger := TAudio2MidiTrigger.Create;
 with FAudio2MidiTrigger do
  begin
   SampleRate := Self.SampleRate;
   Threshold  := -30;
   Interval   := 0.02;
   Flags      := [amFilterOutput];
   OnTrigger  := MidiTrigger;
  end;
 FMidiNote := 64;

 // initialize parameters
 Parameter[0] := -30;
 Parameter[1] :=   0;
 Parameter[2] :=  20;
 Parameter[3] :=  64;
 Parameter[4] :=   0;

 // set editor GUI
 // EditorFormClass := TFmAudio2MidiTrigger;
end;

procedure TAudio2MidiTriggerModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FAudio2MidiTrigger);
end;

procedure TAudio2MidiTriggerModule.ParameterIntervalChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FAudio2MidiTrigger)
  then FAudio2MidiTrigger.Interval := 0.001 * Value;
end;

procedure TAudio2MidiTriggerModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FAudio2MidiTrigger)
  then FAudio2MidiTrigger.Threshold := Value;
end;

procedure TAudio2MidiTriggerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  if Assigned(FAudio2MidiTrigger)
   then FAudio2MidiTrigger.SampleRate := Abs(SampleRate);
end;

procedure TAudio2MidiTriggerModule.ParameterMidiNoteChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMidiNote := Round(Value);
end;

procedure TAudio2MidiTriggerModule.ParameterMidiNoteDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(FMidiNote));
end;

procedure TAudio2MidiTriggerModule.ParameterVelocityShiftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FVelocityShift := Round(Value);
end;

procedure TAudio2MidiTriggerModule.MidiTrigger(
  Sender: TObject; const Level: Single);
var
  Velocity : Byte;
begin
 if FAudio2MidiTrigger.Threshold = 0
  then Velocity := 100
  else
   with FAudio2MidiTrigger
    do Velocity := Round(Limit((Level - Threshold) / abs(Threshold), 0, 1) * 127);
 Velocity := Limit(Velocity + FVelocityShift, 0, 127);
 MidiNoteOn(0, FMidiNote, Velocity);
end;

procedure TAudio2MidiTriggerModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Outputs[0, Sample] := FAudio2MidiTrigger.ProcessSample32(Inputs[0, Sample]);
 SendVstEventsToHost(FVstEvents);
end;

end.
