unit CTCDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspCrosstalkCancellation;

type
  TCTCDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamSpeakerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDistanceDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamListenerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRecursionStepsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamAttenuationChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDistanceLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFilterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFilterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterBypassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection       : TCriticalSection;
    FOutputGain            : Single;
    FCrosstalkCancellation : TCrosstalkCancellation32;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common, CTCGui;

procedure TCTCDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TCTCDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TCTCDataModule.VSTModuleOpen(Sender: TObject);
begin
 FCrosstalkCancellation := TCrosstalkCancellation32.Create;

 Parameter[0] := 100;
 Parameter[1] := 100;
 Parameter[2] := 8;
 Parameter[3] := -6;
 Parameter[4] := 1;
 Parameter[5] := 1400;
 Parameter[6] := -10;
 Parameter[7] := -3;

 with Programs[1] do
  begin
   Parameter[0] := 77;
   Parameter[1] := 65;
   Parameter[2] := 8;
   Parameter[3] := -1;
   Parameter[4] := 1;
   Parameter[5] := 1000;
   Parameter[6] := -10;
   Parameter[7] := -6;
  end;

 // set editor form class
 EditorFormClass := TFmCTC;
end;

procedure TCTCDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCrosstalkCancellation);
end;

procedure TCTCDataModule.ParamRecursionStepsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FCrosstalkCancellation)
   then FCrosstalkCancellation.StageCount := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateRecursionSteps;
end;

procedure TCTCDataModule.ParamDistanceDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 100
  then PreDefined := AnsiString(FloatToStrF(
    RoundTo(0.01 * Parameter[Index], -2), ffGeneral, 3, 3));
end;

procedure TCTCDataModule.ParamDistanceLabel(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
  if Parameter[Index] >= 100
  then PreDefined := 'm'
  else PreDefined := 'cm'
end;

procedure TCTCDataModule.ParameterFilterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  1 : PreDefined := 'Simple Highpass';
 end;
end;

procedure TCTCDataModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutputGain := db_to_Amp(Value);
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateOutputGain;
end;

procedure TCTCDataModule.ParameterFilterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.CrosstalkFilterGain := Value;
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateFilterGain;
end;

procedure TCTCDataModule.ParameterFilterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.CrosstalkFilterFrequency := Value;
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateFilterFrequency;
end;

procedure TCTCDataModule.ParameterFilterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.CrosstalkFilterGain := Value;
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateFilterType;
end;

procedure TCTCDataModule.ParameterFilterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 3, 3))
  else PreDefined := AnsiString(FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 3, 3));
end;

procedure TCTCDataModule.ParameterFilterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := 'Hz'
  else PreDefined := 'kHz';
end;

procedure TCTCDataModule.ParameterBypassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Boolean(Round(Value))
  then OnProcess := VSTModuleProcessBypass
  else OnProcess := VSTModuleProcess;
 OnProcess32Replacing := OnProcess;
end;

procedure TCTCDataModule.ParamAttenuationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCrosstalkCancellation)
  then FCrosstalkCancellation.Attenuation := dB_to_Amp(Value);

 // update GUI
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateAttenuation;
end;

procedure TCTCDataModule.ParamListenerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FCrosstalkCancellation)
   then FCrosstalkCancellation.ListenerDistance := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateListenerDistance;
end;

procedure TCTCDataModule.ParamSpeakerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FCrosstalkCancellation)
   then FCrosstalkCancellation.SpeakerDistance := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmCTC
  then TFmCTC(EditorForm).UpdateSpeakerDistance;
end;

procedure TCTCDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Cardinal;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FOutputGain * Inputs[0, Sample];
    Outputs[1, Sample] := FOutputGain * Inputs[1, Sample];
    FCrosstalkCancellation.ProcessStereo(Outputs[0, Sample], Outputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCTCDataModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TCTCDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Cardinal;
  Data   : array [0..1] of Single;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Data[0] := FOutputGain * Inputs[0, Sample];
    Data[1] := FOutputGain * Inputs[1, Sample];
    FCrosstalkCancellation.ProcessStereo(Data[0], Data[1]);
    Outputs[0, Sample] := Data[0];
    Outputs[1, Sample] := Data[1];
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TCTCDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  if Assigned(FCrosstalkCancellation)
   then FCrosstalkCancellation.SampleRate := Abs(SampleRate);
end;

end.
