unit OversampledTanhModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspPolyphaseDownsampler,
  DAV_DspPolyphaseUpSampler, DAV_DspFilterSimple;

const
  CChannelCount = 2;

type
  TOversampledTanhModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParamCoeffsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamTransitionChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FDownSampler2x   : array [0..CChannelCount - 1] of TPolyphaseDownsampler32;
    FUpSampler2x     : array [0..CChannelCount - 1] of TPolyphaseUpsampler32;
    FLowpassFilter   : array [0..CChannelCount - 1] of TFirstOrderLowpassFilter;
    FTransition      : Double;
    FCriticalSection : TCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_Math, DAV_Approximations, DAV_VSTModuleWithDsp,
  OversampledTanhGUI;

procedure TOversampledTanhModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TOversampledTanhModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TOversampledTanhModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // create up & down sampler
 for Channel := 0 to CChannelCount - 1 do
  begin
   FDownSampler2x[Channel] := TPolyphaseDownsampler32.Create;
   FUpSampler2x[Channel] := TPolyphaseUpsampler32.Create;
   FLowpassFilter[Channel] := TFirstOrderLowpassFilter.Create;
   FLowpassFilter[Channel].SampleRate := 2 * SampleRate;
   FLowpassFilter[Channel].Frequency := 0.95 * SampleRate;
  end;

 // set editor class
 EditorFormClass := TFmOversampledTanh;
 FTransition := 0.25;

 // initial parameters
(*
 Parameter[0] := 16;
 Parameter[1] := 0.01;
*)

 Parameter[0] := 4;
 Parameter[1] := 0.5;
end;

procedure TOversampledTanhModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to CChannelCount - 1 do
  begin
   FreeAndNil(FDownSampler2x[Channel]);
   FreeAndNil(FUpSampler2x[Channel]);
   FreeAndNil(FLowpassFilter[Channel]);
  end;
end;

procedure TOversampledTanhModule.ParamTransitionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  FTransition := 0.0001 + 0.4998 * Value;
  for Channel := 0 to CChannelCount - 1 do
   begin
    if Assigned(FDownSampler2x[Channel]) then FDownSampler2x[Channel].Transition := FTransition;
    if Assigned(FUpSampler2x[Channel]) then FUpSampler2x[Channel].Transition := FTransition;
   end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmOversampledTanh then
  with TFmOversampledTanh(EditorForm)
   do UpdateTransition;
end;

procedure TOversampledTanhModule.ParamCoeffsChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel          : Integer;
  CoefficientCount : Integer;
begin
 FCriticalSection.Enter;
 try
  if (Value < 1) or (Value > 32) then Exit;
  CoefficientCount := Round(Value);

  for Channel := 0 to CChannelCount - 1 do
   begin
    if Assigned(FDownSampler2x[Channel]) then
     begin
      FDownSampler2x[Channel].NumberOfCoefficients := CoefficientCount;
      FDownSampler2x[Channel].Transition := FTransition;
     end;
    if Assigned(FUpSampler2x[Channel]) then
     begin
      FUpSampler2x[Channel].NumberOfCoefficients := CoefficientCount;
      FUpSampler2x[Channel].Transition := FTransition;
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmOversampledTanh then
  with TFmOversampledTanh(EditorForm)
   do UpdateCoeffs;
end;

procedure TOversampledTanhModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
  Buffer  : TDAV2SingleArray;
begin
  FCriticalSection.Enter;
  try
   for Channel := 0 to CChannelCount - 1 do
    for Sample := 0 to SampleFrames - 1 do
     begin
      FUpSampler2x[Channel].ProcessSample32(Inputs[Channel][Sample] + CDenorm32, Buffer);
      with FLowpassFilter[Channel] do
       begin
        Buffer[0] := ProcessSample32(FastTanhContinousError4(Buffer[0]) + CDenorm32);
        Buffer[1] := ProcessSample32(FastTanhContinousError4(Buffer[1]) - CDenorm32);
       end;
      Outputs[Channel][Sample] := FDownSampler2x[Channel].ProcessSample32(Buffer);
     end;
  finally
   FCriticalSection.Leave;
  end;
end;

procedure TOversampledTanhModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to CChannelCount - 1 do
   begin
    FLowpassFilter[Channel].SampleRate := 2 * SampleRate;
    FLowpassFilter[Channel].Frequency := 0.95 * SampleRate;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
