unit LinearPhaseFilterFFTDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFilterLinearPhase;

type
  TLinearPhaseFilterFFTDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFftOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterfftOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLinearPhaseSplitter : array of TArbitraryLinearPhaseBandSplitter;
    FCriticalSection     : TCriticalSection;
    FFrequency           : Double;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_VSTModuleWithPrograms, DAV_DspWindowing;

procedure TLinearPhaseFilterFFTDataModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(2 * numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLinearPhaseFilterFFTDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLinearPhaseFilterFFTDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 SetLength(FLinearPhaseSplitter, numInputs);

 for Channel := 0 to Length(FLinearPhaseSplitter) - 1
  do FLinearPhaseSplitter[Channel] := TArbitraryLinearPhaseBandSplitter.Create;

 // set editor class
 // EditorFormClass := TFmLinearPhaseFilterFFT;

 // set initial parameters
 Parameter[0] := SampleRate / 4;
 Parameter[1] := 10;
 Parameter[2] := 17;
end;

procedure TLinearPhaseFilterFFTDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLinearPhaseSplitter) - 1
  do FreeAndNil(FLinearPhaseSplitter[Channel]);
end;

procedure TLinearPhaseFilterFFTDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   for Channel := 0 to Length(FLinearPhaseSplitter) - 1
    do FLinearPhaseSplitter[Channel].Frequency := FFrequency;
  end;
end;

procedure TLinearPhaseFilterFFTDataModule.ParameterfftOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 Predefined := IntToStr(Round(Parameter[Index]));
end;

procedure TLinearPhaseFilterFFTDataModule.ParameterFftOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FLinearPhaseSplitter) - 1 do
   if Assigned(FLinearPhaseSplitter[Channel])
    then FLinearPhaseSplitter[Channel].FFTOrder := Round(Value);

  InitialDelay := FLinearPhaseSplitter[0].Latency;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLinearPhaseFilterFFTDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 // lock processing
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FLinearPhaseSplitter) - 1 do
   for Sample := 0 to SampleFrames - 1
    do FLinearPhaseSplitter[Channel].ProcessSample(Inputs[Channel, Sample],
         Outputs[2 * Channel, Sample], Outputs[2 * Channel + 1, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
