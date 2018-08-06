unit StkReverbDM;

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
  SyncObjs, Forms, DAV_Types, DAV_VSTModule, DAV_StkJCReverb, DAV_StkNReverb,
  DAV_StkPerryCookReverb;

type
  TStkReverbModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcessNetwork(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessJC(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessPC(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBlendA(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBlendB(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacingNetwork(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacingJC(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacingPC(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacingBlendA(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacingBlendB(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParamT60Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamAlgorithmChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamAlgorithmDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FNReverb: TStkNReverb;
    FJCReverb: TStkJCReverb;
    FPCReverb: TStkPerryCookReverb;
    FCriticalSection: TCriticalSection;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  StkReverbGUI, DAV_StkReverb;

procedure TStkReverbModule.VSTModuleCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
end;

procedure TStkReverbModule.VSTModuleDestroy(Sender: TObject);
begin
  FCriticalSection.Free;
end;

procedure TStkReverbModule.VSTModuleOpen(Sender: TObject);
var
  Params : TDAVSingleDynArray;
begin
 FNReverb := TStkNReverb.Create;
 FJCReverb := TStkJCReverb.Create;
 FPCReverb := TStkPerryCookReverb.Create;
 Parameter[0] := 500;
 Parameter[1] :=  30;
 Parameter[2] :=   0;
 SetLength(Params, numParams);
 Params[0] := Parameter[0];
 Params[1] := Parameter[1];
 Params[2] := Parameter[2];
 Programs[0].SetParameters(Params);
 Params[0] := 400;
 Params[1] :=  25;
 Params[2] :=   1;
 Programs[1].SetParameters(Params);
 Params[0] := 450;
 Params[1] :=  28;
 Params[2] :=   2;
 Programs[2].SetParameters(Params);
 Params[0] := 200;
 Params[1] :=  33;
 Params[2] :=   0;
 Programs[3].SetParameters(Params);
 Params[0] := 600;
 Params[1] :=  60;
 Params[2] :=   2;
 Programs[4].SetParameters(Params);

 EditorFormClass := TFmStkReverb;
end;

procedure TStkReverbModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FNReverb);
 FreeAndNil(FPCReverb);
 FreeAndNil(FJCReverb);
end;

const
  CFixMix: array [0..1] of Single = (0.2, 0.8);
  CHalf32: Single = 0.5;

procedure TStkReverbModule.VSTModuleProcessNetwork(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FNReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FNReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FNReverb.LastOutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessJC(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FJCReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FJCReverb.LastOutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessPC(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FPCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FPCReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FPCReverb.LastOutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessBlendA(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(Inputs[0, Sample] + Inputs[1, Sample]);
    FNReverb.Tick(Inputs[0, Sample] - Inputs[1, Sample]);
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputLeft + FNReverb.LastOutputRight);
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputRight + FNReverb.LastOutputLeft);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessBlendB(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(Inputs[0, Sample] + Inputs[1, Sample]);
    FPCReverb.Tick(Inputs[0, Sample] - Inputs[1, Sample]);
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputLeft + FPCReverb.LastOutputRight);
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputRight + FPCReverb.LastOutputLeft);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessDoubleReplacingNetwork(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FNReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FNReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FNReverb.LastOutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessDoubleReplacingJC(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FJCReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FJCReverb.LastOutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessDoubleReplacingPC(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FPCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] * FPCReverb.LastOutputLeft;
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] * FPCReverb.LastOutputRight;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessDoubleReplacingBlendA(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    FNReverb.Tick(Inputs[0, Sample] - Inputs[1, Sample]);
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputLeft + FNReverb.LastOutputRight);
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputRight + FNReverb.LastOutputLeft);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.VSTModuleProcessDoubleReplacingBlendB(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FJCReverb.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    FPCReverb.Tick(Inputs[0, Sample] - Inputs[1, Sample]);
    Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputLeft + FPCReverb.LastOutputRight);
    Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CHalf32 * CFixMix[1] * (FJCReverb.LastOutputRight + FPCReverb.LastOutputLeft);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TStkReverbModule.ParamT60Change(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FNReverb.T60 := 0.001 * Value;
  FJCReverb.T60 := 0.001 * Value;
  FPCReverb.T60 := 0.001 * Value;
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmStkReverb
  then TFmStkReverb(EditorForm).UpdateT60;
end;

procedure TStkReverbModule.ParamAlgorithmDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'modified STK "NRev"';
  1 : PreDefined := 'modified STK "JCRev"';
  2 : PreDefined := 'modified STK "PRCRev"';
  3 : PreDefined := 'STK "NRev" & "JCRev" blend';
  4 : PreDefined := 'STK "PRCRev" & "JCRev" blend';
 end;
end;

procedure TStkReverbModule.ParamAlgorithmChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
  0 : begin
       OnProcess := VSTModuleProcessNetwork;
       OnProcess64Replacing := VSTModuleProcessDoubleReplacingNetwork;
      end;
  1 : begin
       OnProcess := VSTModuleProcessJC;
       OnProcess64Replacing := VSTModuleProcessDoubleReplacingJC;
      end;
  2 : begin
       OnProcess := VSTModuleProcessPC;
       OnProcess64Replacing := VSTModuleProcessDoubleReplacingPC;
      end;
  3 : begin
       OnProcess := VSTModuleProcessBlendA;
       OnProcess64Replacing := VSTModuleProcessDoubleReplacingBlendA;
      end;
  4 : begin
       OnProcess := VSTModuleProcessBlendB;
       OnProcess64Replacing := VSTModuleProcessDoubleReplacingBlendB;
      end;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TStkReverbModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FNReverb.EffectMix := 0.01 * Value;
 FJCReverb.EffectMix := 0.01 * Value;
 FPCReverb.EffectMix := 0.01 * Value;

 if EditorForm is TFmStkReverb
  then TFmStkReverb(EditorForm).UpdateT60;
end;

procedure TStkReverbModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FNReverb.SampleRate := SampleRate;
 FJCReverb.SampleRate := SampleRate;
 FPCReverb.SampleRate := SampleRate;
end;

end.
