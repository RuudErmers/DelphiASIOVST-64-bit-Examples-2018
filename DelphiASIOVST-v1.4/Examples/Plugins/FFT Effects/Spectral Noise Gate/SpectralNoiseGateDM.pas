unit SpectralNoiseGateDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF} Classes,
  Forms, SyncObjs, DAV_Types, DAV_Complex, DAV_DspDelayLines,
  DAV_DspSpectralNoiseReduction, DAV_DspWindowFunctions,
  DAV_VSTModule {$IFDEF Use_IPPS}, DAV_DspWindowFunctionsAdvanced{$ENDIF};

type
  TSpectralNoiseGateModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFftOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure Parameter2DigitDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FCriticalSection   : TCriticalSection;
    FAdditionalDelay   : array of TDelayLineSamples32;
    FSpectralNoiseGate : array of TSpectralNoiseGate32;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, Math, SpectralNoiseGateGui, DAV_VSTModuleWithPrograms,
  DAV_VSTCustomModule;

procedure TSpectralNoiseGateModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpectralNoiseGateModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSpectralNoiseGateModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);

 InitialDelay := 1 shl ParameterProperties[1].MaxInteger +
   1 shl (ParameterProperties[1].MaxInteger - 1);

 SetLength(FSpectralNoiseGate, numInputs);
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel] := TSpectralNoiseGate32.Create;

 SetLength(FAdditionalDelay, numInputs);
 for Channel := 0 to Length(FAdditionalDelay) - 1
  do FAdditionalDelay[Channel] := TDelayLineSamples32.Create(512);

 with ParameterProperties[2] do
  begin
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;

 Parameter[0] := -30;
 Parameter[1] := 9;
 Parameter[2] := 4;
 Parameter[3] := 10;
 Parameter[4] := 1;
 Parameter[5] := 0.5;
 Parameter[6] := 50;
end;

procedure TSpectralNoiseGateModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FreeAndNil(FSpectralNoiseGate[Channel]);

 for Channel := 0 to Length(FAdditionalDelay) - 1
  do FreeAndNil(FAdditionalDelay[Channel]);
end;

procedure TSpectralNoiseGateModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSpectralNoiseGate.Create(Self);
end;

procedure TSpectralNoiseGateModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Threshold := Value;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateThreshold;
end;

procedure TSpectralNoiseGateModule.ParameterWindowFunctionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralNoiseGate) - 1
   do FSpectralNoiseGate[Channel].WindowFunctionClass := GWindowFunctions[Round(Parameter[Index])];
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateWindowFunction;
end;

procedure TSpectralNoiseGateModule.ParameterWindowFunctionDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TSpectralNoiseGateModule.Parameter2DigitDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 3, 1);
end;

procedure TSpectralNoiseGateModule.ParameterFftOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
  Delay   : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralNoiseGate) - 1
   do FSpectralNoiseGate[Channel].FFTOrder := Round(Value);

  Delay := InitialDelay - 1 shl Round(Value) + 1 shl (Round(Value) - 1);

  for Channel := 0 to Length(FAdditionalDelay) - 1
   do FAdditionalDelay[Channel].BufferSize := Delay;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateFftOrder;
end;

procedure TSpectralNoiseGateModule.ParameterFftOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TSpectralNoiseGateModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Ratio := 1 / Value;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateRatio;
end;

procedure TSpectralNoiseGateModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Knee := Value;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateKnee;
end;

procedure TSpectralNoiseGateModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Attack := Value;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateAttack;
end;

procedure TSpectralNoiseGateModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].Release := Value;

 // update GUI
 if EditorForm is TFmSpectralNoiseGate
  then TFmSpectralNoiseGate(EditorForm).UpdateRelease;
end;

procedure TSpectralNoiseGateModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseGate) - 1
  do FSpectralNoiseGate[Channel].SampleRate := SampleRate;
end;

procedure TSpectralNoiseGateModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralNoiseGate) - 1 do
   begin
    FSpectralNoiseGate[Channel].ProcessBlock(@Inputs[Channel, 0],
      @Outputs[Channel, 0], SampleFrames);
    FAdditionalDelay[Channel].ProcessBlock32(@Outputs[Channel, 0],
      SampleFrames);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
