unit DitherNoiseshaperDM;

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

{$I DAV_Compiler.inc}

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspDitherNoiseShaper;

type
  TDitherNoiseshaperModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBitDepthDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterDitherTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterNoiseshaperTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNoiseshaperTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterDitherChangeAmplitude(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDitherTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FDitherNoiseshaper : array of TDitherNoiseShaper32;
    FCriticalSection   : TCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DitherNoiseshaperGUI, Dialogs;

resourcestring
  ECStrOnly44100 = 'Only a samplerate of 44.1 kHz is supported!';

procedure TDitherNoiseshaperModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TDitherNoiseshaperModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TDitherNoiseshaperModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);
 Assert(numInputs > 0);
 SetLength(FDitherNoiseshaper, numInputs);
 for Channel := 0 to Length(FDitherNoiseshaper) - 1
  do FDitherNoiseshaper[Channel] := TDitherNoiseShaper32.Create;

 // initialize parameters
 Parameter[0] := 16;
 Parameter[1] := 1;
 Parameter[2] := 2;
 Parameter[3] := 1;
 Parameter[4] := 0;

 // set editor form class
 EditorFormClass := TFmDitherNoiseshaper;
end;

procedure TDitherNoiseshaperModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1
  do FreeAndNil(FDitherNoiseshaper[Channel]);
end;

procedure TDitherNoiseshaperModule.ParameterDitherTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if Assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].DitherType := TDitherType(Round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateDitherType;
end;

procedure TDitherNoiseshaperModule.ParameterDitherChangeAmplitude(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if Assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].DitherAmplitude := Value;

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateDitherAmplitude;
end;

procedure TDitherNoiseshaperModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if Assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].Limit := Boolean(Round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateLimit;
end;

procedure TDitherNoiseshaperModule.ParameterBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if Assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].BitDepth := Round(Value);

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateBitDepth;
end;

procedure TDitherNoiseshaperModule.ParameterNoiseshaperTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
  if Assigned(FDitherNoiseshaper[Channel])
   then FDitherNoiseshaper[Channel].NoiseshaperType := TNoiseshaperType(Round(Value));

 // eventually update editor
 if EditorForm is TFmDitherNoiseshaper
  then TFmDitherNoiseshaper(EditorForm).UpdateNoiseShaper;
end;

procedure TDitherNoiseshaperModule.ParameterBitDepthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TDitherNoiseshaperModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
const
  COnOff :array [0..1] of AnsiString = ('Off', 'On');
begin
 PreDefined := COnOff[Round(Parameter[Index])];
end;

procedure TDitherNoiseshaperModule.ParameterDitherTypeDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 case TDitherType(Round(Parameter[Index])) of
        dtNone : PreDefined := 'None';
       dtEqual : PreDefined := 'Rectangular';
  dtTriangular : PreDefined := 'Triangular';
       dtGauss : PreDefined := 'Gauss';
   dtFastGauss : PreDefined := 'Gauss (fast)';
 end;
end;

procedure TDitherNoiseshaperModule.ParameterNoiseshaperTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case TNoiseShaperType(Round(Parameter[Index])) of
              nsNone : PreDefined := 'None';
              nsEFB  : PreDefined := 'Simple Error Feedback (1st Order)';
              ns2Sc  : PreDefined := 'Simple Highpass (2nd Order)';
              ns2MEc : PreDefined := 'mod. E-weighting (2nd Order)';
              ns3MEc : PreDefined := 'mod. E-weighting (3rd Order)';
              ns9MEc : PreDefined := 'mod. E-weighting (9th Order)';
              ns5IEc : PreDefined := 'improved E-weighting (5th Order)';
              ns9IEc : PreDefined := 'improved E-weighting (9th Order)';
              ns3Fc  : PreDefined := 'F-weighting (3rd Order)';
              ns9Fc  : PreDefined := 'F-weighting (9th Order';
              nsSBM  : PreDefined := 'Sony "Super Bit Mapping"';
              nsSBMr : PreDefined := 'Reduced "Super Bit Mapping"';
  nsSharp14k7thOrder : PreDefined := 'Sharp 14 kHz (7th Order)';
  nsSharp15k8thOrder : PreDefined := 'Sharp 15 kHz (8th Order)';
  nsSharp16k9thOrder : PreDefined := 'Sharp 16 kHz (9th Order)';
      nsExperimental : PreDefined := 'Experimental';
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDitherNoiseshaper[Channel].ProcessFloat(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FDitherNoiseshaper) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FDitherNoiseshaper[Channel].ProcessFloat(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDitherNoiseshaperModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 try
  if ((Abs(SampleRate) / 44100)) - 1 > 0.1
   then raise Exception.Create(ECStrOnly44100)
 except
  on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
 end;
end;

end.
