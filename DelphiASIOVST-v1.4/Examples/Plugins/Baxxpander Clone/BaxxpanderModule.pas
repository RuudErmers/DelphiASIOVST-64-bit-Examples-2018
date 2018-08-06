unit BaxxpanderModule;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilterButterworth;

type
  TBaxxpanderModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcessNormal(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessSaturated(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterDryWetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FButterworthSplitter : array of TButterworthSplitBandFilter;
    FGains               : array [0..4] of Single;
    procedure CalculateGains;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Approximations, DAV_DspWaveshaper, BaxxpanderGui;

procedure TBaxxpanderModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(NumInputs = NumOutputs);
 SetLength(FButterworthSplitter,  numInputs);

 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  begin
   FButterworthSplitter[Channel] := TButterworthSplitBandFilter.Create(1);
   with FButterworthSplitter[Channel] do
    begin
     SampleRate := Self.SampleRate;
     Frequency := 250;
    end;
  end;

 // set editor form class
 EditorFormClass := TFmBaxxpanderGui;

 // set default parameters
 Parameter[0] := 100;
 Parameter[1] := 100;
 Parameter[2] := 100;
 Parameter[3] := 1;
 Parameter[4] := 100;
end;

procedure TBaxxpanderModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1
  do FreeAndNil(FButterworthSplitter[Channel]);
end;

procedure TBaxxpanderModule.ParameterDryWetChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;

 // update GUI
 if EditorForm is TFmBaxxpanderGui
  then TFmBaxxpanderGui(EditorForm).UpdateDryWet;
end;

procedure TBaxxpanderModule.ParameterMixerChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;

 // update GUI
 if EditorForm is TFmBaxxpanderGui
  then TFmBaxxpanderGui(EditorForm).UpdateMixer;
end;

procedure TBaxxpanderModule.ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;

 // update GUI
 if EditorForm is TFmBaxxpanderGui
  then TFmBaxxpanderGui(EditorForm).UpdateLimit;
end;

procedure TBaxxpanderModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TBaxxpanderModule.ParameterShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateGains;

 // update GUI
 if EditorForm is TFmBaxxpanderGui
  then TFmBaxxpanderGui(EditorForm).UpdateShape;
end;

procedure TBaxxpanderModule.ParameterOnOffChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0.5
  then OnProcess := VSTModuleProcessSaturated
  else OnProcess := VSTModuleProcessNormal;

 OnProcess32Replacing := OnProcess;

 // update GUI
 if EditorForm is TFmBaxxpanderGui
  then TFmBaxxpanderGui(EditorForm).UpdateShape;
end;

procedure TBaxxpanderModule.CalculateGains;
begin
 FGains[0] := 1 - 0.002 * Parameter[0];
 FGains[1] := (1 + 0.0015 * Parameter[2]) * 12 * (1 - FGains[0]);
 FGains[2] := (0.5 + 0.005 * Parameter[2]) * 1 - FGains[0];
 FGains[4] := 0.01 * (0.25 + Parameter[4]);
 FGains[3] := 0.75 * FGains[1] * (0.005 * Parameter[1] / (0.01 + FGains[4]));
end;

procedure TBaxxpanderModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FButterworthSplitter) - 1 do
   if Assigned(FButterworthSplitter[ChannelIndex])
    then FButterworthSplitter[ChannelIndex].SampleRate := Abs(SampleRate);
end;

procedure TBaxxpanderModule.VSTModuleProcessNormal(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel : Integer;
  Low, High       : Single;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FButterworthSplitter[Channel].ProcessSample32(Inputs[Channel, Sample], Low, High);
    Outputs[Channel, Sample] := FGains[0] * Inputs[Channel, Sample] +
                                FGains[1] * Low + FGains[2] * High;
   end;
end;

procedure TBaxxpanderModule.VSTModuleProcessSaturated(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel : Integer;
  Low, High       : Single;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    FButterworthSplitter[Channel].ProcessSample32(Inputs[Channel, Sample], Low, High);
    Outputs[Channel, Sample] := FGains[0] * Inputs[Channel, Sample] +
                                FGains[3] * FastTanhContinousError5(FGains[4] * Low) + FGains[2] * High;
   end;
end;

end.
