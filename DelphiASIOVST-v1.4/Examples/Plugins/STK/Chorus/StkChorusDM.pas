unit StkChorusDM;

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
  SyncObjs, Forms, DAV_Types, DAV_VSTModule, DAV_StkChorus;

type
  TStkChorusModule = class(TVSTModule)
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs,
      Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParamModDepthChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure ParamModFreqChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FChorus: TStkChorus;
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
  StkChorusGUI;

procedure TStkChorusModule.VSTModuleCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
end;

procedure TStkChorusModule.VSTModuleDestroy(Sender: TObject);
begin
  FCriticalSection.Free;
end;

procedure TStkChorusModule.VSTModuleOpen(Sender: TObject);
var
  Params: TDAVSingleDynArray;
begin
  FChorus := TStkChorus.Create(SampleRate, SampleRate);
  Parameter[0] := 500;
  Parameter[1] := 1;
  Parameter[2] := 30;

  SetLength(Params, numParams);

  Params[0] := Parameter[0];
  Params[1] := Parameter[1];
  Params[2] := Parameter[2];
  Programs[0].SetParameters(Params);

  Params[0] := 400;
  Params[1] := 0.4;
  Params[2] := 25;
  Programs[1].SetParameters(Params);

  Params[0] := 450;
  Params[1] := 0.6;
  Params[2] := 28;
  Programs[2].SetParameters(Params);

  Params[0] := 200;
  Params[1] := 0.8;
  Params[2] := 33;
  Programs[3].SetParameters(Params);

  Params[0] := 600;
  Params[1] := 1.6;
  Params[2] := 60;
  Programs[4].SetParameters(Params);

  EditorFormClass := TFmStkChorus;
end;

procedure TStkChorusModule.VSTModuleClose(Sender: TObject);
begin
  FreeAndNil(FChorus);
end;

const
  CHalf32: Single = 0.5;
  CFixMix: array [0 .. 1] of Single = (0.2, 0.8);

procedure TStkChorusModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample: Integer;
begin
  FCriticalSection.Enter;
  try
    for Sample := 0 to SampleFrames - 1 do
    begin
      FChorus.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
      Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] *
        FChorus.LastOutputLeft;
      Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] *
        FChorus.LastOutputRight;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TStkChorusModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample: Integer;
begin
  FCriticalSection.Enter;
  try
    for Sample := 0 to SampleFrames - 1 do
    begin
      FChorus.Tick(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
      Outputs[0, Sample] := CFixMix[0] * Inputs[0, Sample] + CFixMix[1] *
        FChorus.LastOutputLeft;
      Outputs[1, Sample] := CFixMix[0] * Inputs[1, Sample] + CFixMix[1] *
        FChorus.LastOutputRight;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TStkChorusModule.ParamModDepthChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FCriticalSection.Enter;
  try
    FChorus.ModDepth := 0.001 * Value;
  finally
    FCriticalSection.Leave;
  end;

  // update GUI
  if EditorForm is TFmStkChorus then
    TFmStkChorus(EditorForm).UpdateModDepth;
end;

procedure TStkChorusModule.ParamModFreqChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FCriticalSection.Enter;
  try
    FChorus.ModFrequency := 0.001 * Value;
  finally
    FCriticalSection.Leave;
  end;

  // update GUI
  if EditorForm is TFmStkChorus then
    TFmStkChorus(EditorForm).UpdateModFreq;
end;

procedure TStkChorusModule.ParamMixChange(Sender: TObject; const Index: Integer;
  var Value: Single);
begin
  FChorus.EffectMix := 0.01 * Value;

  // update GUI
  if EditorForm is TFmStkChorus then
    TFmStkChorus(EditorForm).UpdateEffectMix;
end;

procedure TStkChorusModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
  FChorus.SampleRate := SampleRate;
end;

end.
