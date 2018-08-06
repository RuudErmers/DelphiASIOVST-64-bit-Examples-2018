unit SampleDelayDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Types, SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspInterpolation;

type
  TSampleDelayVST = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDryMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInvFBDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterInvFBChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FBuffer          : array [0..1] of PDAVSingleFixedArray;
    FMix             : array [0..1] of Single;
    FClearBuffer     : Boolean;
    FFeedbackSign    : Single;
    FFeedback        : Single;
    FFeedFactor      : Single;
    FBufferSize      : Integer;
    FBufferPos       : Integer;
    procedure CalculateFeedfactor;
  public
    property ClearBufferOnChange: Boolean read FClearBuffer write FClearBuffer;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_VSTCustomModule;

procedure TSampleDelayVST.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSampleDelayVST.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSampleDelayVST.VSTModuleOpen(Sender: TObject);
begin
 FBufferPos := 0;
 FClearBuffer := True;

 // initialize parameters
 Parameter[0] := 441;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 100;
end;

procedure TSampleDelayVST.VSTModuleClose(Sender: TObject);
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
end;

procedure TSampleDelayVST.SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, newLatency : Integer;
begin
 FCriticalSection.Enter;
 try
  newLatency := (Round(Value) + 1);
  if FBufferSize < newLatency then
   begin
    for Channel := 0 to Length(FBuffer) - 1 do
     begin
      ReallocMem(FBuffer[Channel], newLatency * SizeOf(Single));

      if ClearBufferOnChange
       then FillChar(FBuffer[Channel]^[FBufferSize], (newLatency - FBufferSize) * SizeOf(Single), 0)
       else
        begin
         Move(FBuffer[Channel]^[FBufferPos], FBuffer[Channel]^[(newLatency - FBufferPos)], (FBufferSize - FBufferPos) * SizeOf(Single));
         if (newLatency - 2 * FBufferPos) > 0
          then FillChar(FBuffer[Channel]^[FBufferPos], (newLatency - 2 * FBufferPos) * SizeOf(Single), 0);
        end;
     end;
    FBufferSize := newLatency;
   end else
  if FBufferSize > newLatency then
   begin
    FBufferSize := newLatency;
    for Channel := 0 to Length(FBuffer) - 1 do
     begin
      ReallocMem(FBuffer[Channel], newLatency * SizeOf(Single));
      if not ClearBufferOnChange and (FBufferPos < newLatency)
       then Move(FBuffer[Channel]^[FBufferSize + FBufferPos - newLatency], FBuffer[Channel]^[FBufferPos], (newLatency - FBufferPos) * SizeOf(Single));
     end;
    if FBufferPos >= FBufferSize
     then FBufferPos := 0;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSampleDelayVST.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedback := (0.01 * Value);
 CalculateFeedFactor;
end;

procedure TSampleDelayVST.ParameterInvFBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedbackSign := 2 * Value - 1;
 CalculateFeedFactor;
end;

procedure TSampleDelayVST.CalculateFeedfactor;
begin
 FFeedFactor := FFeedbackSign * abs(FFeedback);
end;

procedure TSampleDelayVST.ParameterInvFBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Off'
  else PreDefined := 'On';
end;

procedure TSampleDelayVST.ParameterWetMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;
end;

procedure TSampleDelayVST.ParamDryMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.01 * Value;
end;

procedure TSampleDelayVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FBuffer[0, FBufferPos] := Inputs[0, SampleIndex] + FFeedFactor * FBuffer[0, FBufferPos];
    FBuffer[1, FBufferPos] := Inputs[1, SampleIndex] + FFeedFactor * FBuffer[1, FBufferPos];
    Inc(FBufferPos);
    if FBufferPos >= FBufferSize
     then FBufferPos := 0;
    Outputs[0, SampleIndex] := FMix[0] * Inputs[0, SampleIndex] + FMix[1] * FBuffer[0, FBufferPos];
    Outputs[1, SampleIndex] := FMix[0] * Inputs[1, SampleIndex] + FMix[1] * FBuffer[1, FBufferPos];
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSampleDelayVST.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FBuffer[0, FBufferPos] := Inputs[0, SampleIndex] + FFeedFactor * FBuffer[0, FBufferPos];
    FBuffer[1, FBufferPos] := Inputs[1, SampleIndex] + FFeedFactor * FBuffer[1, FBufferPos];
    Inc(FBufferPos);
    if FBufferPos >= FBufferSize
     then FBufferPos := 0;
    Outputs[0, SampleIndex] := FMix[0] * Inputs[0, SampleIndex] + FMix[1] * FBuffer[0, FBufferPos];
    Outputs[1, SampleIndex] := FMix[0] * Inputs[1, SampleIndex] + FMix[1] * FBuffer[1, FBufferPos];
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
