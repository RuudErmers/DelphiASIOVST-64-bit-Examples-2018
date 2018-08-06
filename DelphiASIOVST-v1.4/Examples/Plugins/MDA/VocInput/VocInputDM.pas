unit VocInputDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTModule,
  DAV_DspVoiceInput;

type
  TVocInputDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterBreathChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMaxFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMaxFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterPitchChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPitchDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTrackingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTrackingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterVoicedUnvoicedDetectorChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FVoiceInput : TVoiceInput;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math;

procedure TVocInputDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVoiceInput);
end;

procedure TVocInputDataModule.VSTModuleOpen(Sender: TObject);
begin
 FVoiceInput := TVoiceInput.Create;
 FVoiceInput.SampleRate := SampleRate;

 // Initial Parameters
 Parameter[0] := 0.5; // Tracking Off / On / Quant
 Parameter[1] := 0.5; // Pitch
 Parameter[2] := 20;  // Breath FNoise
 Parameter[3] := 50;  // Voiced / Unvoiced Thresh
 Parameter[4] := 69;  // Max Freq
end;

procedure TVocInputDataModule.ParameterTrackingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.Tracking := TTrackingType(Round(Value))
end;

procedure TVocInputDataModule.ParameterTrackingDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0: PreDefined := 'OFF';
  1: PreDefined := 'FREE';
  2: PreDefined := 'QUANT';
 end;
end;

procedure TVocInputDataModule.ParameterPitchChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.Pitch := Value;
end;

procedure TVocInputDataModule.ParameterPitchDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Assigned(FVoiceInput) then
  if FVoiceInput.Tracking = ttOff
   then PreDefined := Midi2String(Round(48.0 * Parameter[Index] + 21.0))
   else PreDefined := IntToStr(Round(48.0 * Parameter[Index] - 24.0));
end;

procedure TVocInputDataModule.ParameterBreathChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.Breath := Value;
end;

procedure TVocInputDataModule.ParameterVoicedUnvoicedDetectorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.VoicedUnvoicedRatio := Value;
end;

procedure TVocInputDataModule.ParameterMaxFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.MaximumFrequency := Value;
end;

procedure TVocInputDataModule.ParameterMaxFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := Midi2String(Parameter[4]);
end;

procedure TVocInputDataModule.VSTModuleSuspend(Sender: TObject);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.ResetStates;
end;

procedure TVocInputDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FVoiceInput)
  then FVoiceInput.SampleRate := SampleRate;
end;

procedure TVocInputDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FVoiceInput.ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := Inputs[0, Sample];
  end;
end;

end.
