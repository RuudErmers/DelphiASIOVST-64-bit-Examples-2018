unit ChebyshevWaveshaperDSP;

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
  SyncObjs, Forms, DAV_Types, DAV_VSTModule, DAV_DspWaveshaper;

const
  CHarmCount : Integer = 24;
  CdBMin : Single = -140;

type
  TChebyshevWaveshaperDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDouble(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParamVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHarmDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamHarmLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FCriticalSection    : TCriticalSection;
    FChebysheWaveshaper : TChebyshevWaveshaper;
    FVolume             : Single;
    procedure ParamHarmonicChange(Sender: TObject; const Index: Integer; var Value: Single);
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common;

procedure TChebyshevWaveshaperDataModule.VSTModuleCreate(Sender: TObject);
var
  HarmIndex : Integer;
begin
 // create critical section
 FCriticalSection := TCriticalSection.Create;

 // create parameters
 FVolume := 1;
 for HarmIndex := CHarmCount - 1 downto 0 do
  with ParameterProperties.Insert(0) do
   begin
    DisplayName       := 'Harmonic ' + IntToStr(HarmIndex + 1);
    Min               := -1;
    Max               := 1;
    StepFloat         := 1;
    StepInteger       := 1;
    SmallStepFloat    := 0.1;
    LargeStepFloat    := 10;
    LargeStepInteger  := 10;
    ShortLabel        := AnsiString('H' + IntToStr(HarmIndex + 1));
    Units             := 'dB';
    OnParameterChange := ParamHarmonicChange;
    OnCustomParameterDisplay := ParamHarmDisplay;
    OnCustomParameterLabel := ParamHarmLabel;
   end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 FChebysheWaveshaper := TChebyshevWaveshaper.Create;
 FChebysheWaveshaper.Order := CHarmCount;

 // initial parameters
 Parameter[0] := 1;
 for i := 1 to CHarmCount - 1
  do Parameter[i] := 0;
 Parameter[CHarmCount] := 0;

 // programs
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FChebysheWaveshaper);
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0 then PreDefined := 'dB (+)' else
 if Parameter[Index] < 0
  then PreDefined := 'dB (-)'
  else PreDefined := 'dB';
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Val : Single;
begin
 Val := Abs(140 * Parameter[Index]) - 140;
 if Abs(Parameter[Index]) < 1E-3
  then PreDefined := '-oo'
  else PreDefined := FloatToAnsiString(Val, 3);
end;

procedure TChebyshevWaveshaperDataModule.ParamHarmonicChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChebysheWaveshaper)then
   begin
    if Abs(Value) < 1E-3 then
     begin
      FChebysheWaveshaper.Gain[Index] := 0;
     end
    else
     begin
      FChebysheWaveshaper.Level[Index]    := Abs(Value * 140) - 140;
      FChebysheWaveshaper.Inverted[Index] := Value < 0;
     end;
   end;
 finally
  FreeAndNil(FCriticalSection);
 end;
end;

procedure TChebyshevWaveshaperDataModule.ParamVolumeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FVolume := dB_to_Amp(Value);
 finally
  FreeAndNil(FCriticalSection);
 end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FVolume * FChebysheWaveshaper.ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
 finally
  FreeAndNil(FCriticalSection);
 end;
end;

procedure TChebyshevWaveshaperDataModule.VSTModuleProcessDouble(
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to 1 do
   for SampleIndex := 0 to SampleFrames - 1
    do Outputs[ChannelIndex, SampleIndex] := FVolume * FChebysheWaveshaper.ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
 finally
  FreeAndNil(FCriticalSection);
 end;
end;

end.
