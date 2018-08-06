unit BodeFrequencyShifterDSP;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFrequencyShifter;

type
  TBodeFrequencyShifterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMultiChannel(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCoeffsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTransitionBWChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FFreqShifter     : array of TBodeFrequencyShifter32;
    FUpMix, FDownMix : Single;
    FCriticalSection : TCriticalSection;
    procedure ChooseProcess;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Registry, DAV_Common;

const
  CRegKeyRoot = 'Software\Delphi ASIO & VST Project\Bode Frequency Shifter';

procedure TBodeFrequencyShifterDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel  : Integer;
  TempFreq : Single;
begin
 Assert(numInputs = numOutputs);
 Assert(numInputs > 0);
 SetLength(FFreqShifter, numInputs);

 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegKeyRoot, False) then
    begin
     if ValueExists('Upper Frequency') then
      begin
       TempFreq := StrToFloat(ReadString('Upper Frequency'));
       if TempFreq > ParameterProperties[0].Min
        then ParameterProperties[0].Max := TempFreq;
      end;
     if ValueExists('Lower Frequency') then
      begin
       TempFreq := StrToFloat(ReadString('Lower Frequency'));
       if TempFreq < ParameterProperties[0].Max
        then ParameterProperties[0].Min := TempFreq;
      end;
    end
  finally
   Free;
  end;

 ChooseProcess;

 for Channel := 0 to Length(FFreqShifter) - 1
  do FFreqShifter[Channel] := TBodeFrequencyShifter32.Create;

 // default parameters
 Parameter[0] := 100;
 Parameter[1] := 100;
 Parameter[2] := 12;
 Parameter[3] := 0.1;

(*
 Programs[0].Parameter[0] := 10;
 Programs[1].Parameter[0] := 0.1;
 Programs[2].Parameter[0] := 1000;
*)
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1
  do FreeAndNil(FFreqShifter[Channel]);
end;

procedure TBodeFrequencyShifterDataModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FFreqShifter) - 1 do
   if Assigned(FFreqShifter[Channel])
    then FFreqShifter[Channel].Frequency := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBodeFrequencyShifterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FUpMix   := 0.5 * ((0.01 * Value) + 1);
 FDownMix := 1 - FUpMix;
 FUpMix   := sqrt(FUpMix);
 FDownMix := sqrt(FDownMix);
end;

procedure TBodeFrequencyShifterDataModule.ParameterCoeffsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FFreqShifter) - 1 do
   if Assigned(FFreqShifter[Channel])
    then FFreqShifter[Channel].CoefficientCount := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBodeFrequencyShifterDataModule.ParameterTransitionBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFreqShifter) - 1 do
  if Assigned(FFreqShifter[Channel])
   then FFreqShifter[Channel].TransitionBandwidth := Value;
end;

procedure TBodeFrequencyShifterDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := FloatToAnsiString(Parameter[Index], 4)
  else PreDefined := FloatToAnsiString(0.001 * Parameter[Index], 4);
end;

procedure TBodeFrequencyShifterDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := 'Hz'
  else PreDefined := 'kHz';
end;

procedure TBodeFrequencyShifterDataModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := VSTModuleProcessMultiChannel;
 end;
 OnProcess32Replacing := OnProcess;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
    Outputs[0, Sample] := FUpMix * Up + FDownMix * Down;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample   : Integer;
  Up, Down : Single;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    FFreqShifter[0].ProcessSample(Inputs[0, Sample], Up, Down);
    Outputs[0, Sample] := FUpMix * Up + FDownMix * Down;

    FFreqShifter[1].ProcessSample(Inputs[1, Sample], Up, Down);
    Outputs[1, Sample] := FUpMix * Up + FDownMix * Down;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleProcessMultiChannel(
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal);
var
  Channel  : Integer;
  Sample   : Integer;
  Up, Down : Single;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FFreqShifter) - 1 do
   for Sample := 0 to SampleFrames - 1 do
    begin
     FFreqShifter[Channel].ProcessSample(Inputs[Channel, Sample], Up, Down);
     Outputs[Channel, Sample] := FUpMix * Up + FDownMix * Down;
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBodeFrequencyShifterDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FFreqShifter) - 1 do
   if Assigned(FFreqShifter[Channel])
    then FFreqShifter[Channel].SampleRate := Abs(SampleRate);
end;

end.
