unit DualLinkwitzRileyFiltersDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Forms, SyncObjs, DAV_Types, DAV_VSTModule,
  DAV_DspFilterButterworth;

type
  TProcessMode = (pmBypass, pmLowpass, pmHighpass, pmBandpass);

  { TDualLinkwitzRileyFiltersModule }

  TDualLinkwitzRileyFiltersModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessLowpass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessHighpass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBandpass(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterLowpassOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowpassFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterHighpassFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure StringOrderToParameter(Sender: TObject; const Index: Integer; const ParameterString: AnsiString; var Value: Single);
    procedure VSTModuleResume(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FLowpass         : array of array [0..1] of TButterworthLowPassFilter;
    FHighpass        : array of array [0..1] of TButterworthHighPassFilter;
    FSign            : Single;
    FProcessMode     : TProcessMode;
    function GetHighpass(Channel, Index: Integer): TButterworthHighPassFilter;
    function GetLowpass(Channel, Index: Integer): TButterworthLowPassFilter;
  public
    procedure LoadLow(Index: Integer);
    procedure LoadHigh(Index: Integer);
    procedure StoreLow(Index: Integer);
    procedure StoreHigh(Index: Integer);
    function Magnitude_dB(Frequency: Single): Single;

    property Lowpass[Channel, Index: Integer]: TButterworthLowPassFilter read GetLowpass;
    property Highpass[Channel, Index: Integer]: TButterworthHighPassFilter read GetHighpass;
  end;

const
  CRegistryKey = 'SOFTWARE\Delphi ASIO & VST Project\';

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Registry, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF}
  DAV_Common, DAV_Approximations, DualLinkwitzRileyFiltersGui;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 Assert(numOutputs = numInputs);
 SetLength(FLowpass, numInputs);
 for Channel := 0 to Length(FLowpass) - 1 do
  begin
   FLowpass[Channel][0] := TButterworthLowPassFilter.Create;
   FLowpass[Channel][0].SampleRate := SampleRate;
   FLowpass[Channel][1] := TButterworthLowPassFilter.Create;
   FLowpass[Channel][1].SampleRate := SampleRate;
  end;

 SetLength(FHighpass, numInputs);
 for Channel := 0 to Length(FHighpass) - 1 do
  begin
   FHighpass[Channel][0] := TButterworthHighPassFilter.Create;
   FHighpass[Channel][0].SampleRate := SampleRate;
   FHighpass[Channel][1] := TButterworthHighPassFilter.Create;
   FHighpass[Channel][1].SampleRate := SampleRate;
  end;

 FSign := 1;
 Parameter[0] := 10000;
 Parameter[1] := 2;
 Parameter[2] := 100;
 Parameter[3] := 2;
 Parameter[4] := 0;

 with Programs[0] do
  begin
   Parameter[0] := 10000;
   Parameter[1] := 2;
   Parameter[2] := 100;
   Parameter[3] := 2;
   Parameter[4] := 0;
  end;

 with Programs[1] do
  begin
   Parameter[0] := 20000;
   Parameter[1] := 2;
   Parameter[2] := 2;
   Parameter[3] := 2;
   Parameter[4] := 3;
  end;

 // set editor form class
 EditorFormClass := TFmLinkwitzRiley;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLowpass) - 1 do
  begin
   FreeAndNil(FLowpass[Channel, 0]);
   FreeAndNil(FLowpass[Channel, 1]);
  end;

 for Channel := 0 to Length(FHighpass) - 1 do
  begin
   FreeAndNil(FHighpass[Channel, 0]);
   FreeAndNil(FHighpass[Channel, 1]);
  end;
end;

procedure TDualLinkwitzRileyFiltersModule.LoadLow(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if KeyExists(CRegistryKey + IntToStr(Index)) then
    if OpenKey(CRegistryKey + IntToStr(Index), False) then
     begin
      if ValueExists('Frequency') then Parameter[0] := ReadFloat('Frequency');
      if ValueExists('Order') then Parameter[1] := ReadInteger('Order');
     end;
  finally
   Free;
  end;
end;

function TDualLinkwitzRileyFiltersModule.Magnitude_dB(
  Frequency: Single): Single;
begin
 if Integer(FProcessMode) and 1 <> 0
  then Result := FLowpass[0, 0].MagnitudeSquared(Frequency) *
                 FLowpass[0, 1].MagnitudeSquared(Frequency)
  else Result := 1;
 if Integer(FProcessMode) and 2 <> 0
  then Result := Result * FHighpass[0, 0].MagnitudeSquared(Frequency) *
                 FHighpass[0, 1].MagnitudeSquared(Frequency);
 Result := 10 * FastLog10Laurent5(Result);
end;

procedure TDualLinkwitzRileyFiltersModule.StringOrderToParameter(Sender: TObject;
  const Index: Integer; const ParameterString: AnsiString; var Value: Single);
var
  ProcStr : AnsiString;
  Indxes  : array [0..1] of Integer;
begin
 with ParameterProperties[Index] do
  begin
   ProcStr := AnsiString(Trim(string(ParameterString)));

   Indxes[0] := 1;
   while (Indxes[0] <= Length(ProcStr)) and
    (not (ProcStr[Indxes[0]] in ['0'..'9', ',', '.'])) do Inc(Indxes[0]);

   if (Indxes[0] <= Length(ProcStr)) then
    begin
     Indxes[1] := Indxes[0] + 1;
     while (Indxes[1] <= Length(ProcStr)) and
      (ProcStr[Indxes[1]] in ['0'..'9', ',', '.']) do Inc(Indxes[1]);

     ProcStr := Copy(ProcStr, Indxes[0], Indxes[1] - Indxes[0]);

     Value := Round(StrToFloat(string(ProcStr)) / 12);
    end;
  end;
end;

function TDualLinkwitzRileyFiltersModule.GetHighpass(Channel,
  Index: Integer): TButterworthHighPassFilter;
begin
 if (Channel >= 0) and (Channel < Length(FHighpass)) then
  if Index in [0..1]
   then Result := FHighpass[Channel, Index]
   else Result := nil
 else Result := nil;
end;

function TDualLinkwitzRileyFiltersModule.GetLowpass(Channel,
  Index: Integer): TButterworthLowPassFilter;
begin
 if (Channel >= 0) and (Channel < Length(FLowpass)) then
  if Index in [0..1]
   then Result := FLowpass[Channel, Index]
   else Result := nil
 else Result := nil;
end;

procedure TDualLinkwitzRileyFiltersModule.LoadHigh(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if KeyExists(CRegistryKey + IntToStr(Index)) then
    if OpenKey(CRegistryKey + IntToStr(Index), False) then
     begin
      if ValueExists('Frequency') then Parameter[2] := ReadFloat('Frequency');
      if ValueExists('Order') then Parameter[3] := ReadInteger('Order');
     end;
  finally
   Free;
  end;
end;

procedure TDualLinkwitzRileyFiltersModule.StoreLow(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegistryKey + IntToStr(Index), True) then
    begin
     WriteFloat('Frequency', Parameter[0]);
     WriteInteger('Order', Round(Parameter[1]));
    end;
  finally
   Free;
  end;
end;

procedure TDualLinkwitzRileyFiltersModule.StoreHigh(Index: Integer);
begin
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegistryKey + IntToStr(Index), True) then
    begin
     WriteFloat('Frequency', Parameter[2]);
     WriteInteger('Order', Round(Parameter[3]));
    end;
  finally
   Free;
  end;
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(12 * Round(Parameter[Index])));
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then PreDefined := AnsiString(FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3));
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FProcessMode := TProcessMode(Round(Limit(Value, 0, 4)));
  case FProcessMode of
   pmBypass   : OnProcess := VSTModuleProcessBypass;
   pmLowpass  : OnProcess := VSTModuleProcessLowpass;
   pmHighpass : OnProcess := VSTModuleProcessHighpass;
   pmBandpass : OnProcess := VSTModuleProcessBandpass;
  end;
  OnProcess32Replacing := OnProcess;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateType;
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0: PreDefined := 'Bypass';
  1: PreDefined := 'Highcut';
  2: PreDefined := 'Lowcut';
  3: PreDefined := 'Bandpass';
 end;
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterLowpassFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  if Length(FLowpass) > 0 then
   for Channel := 0 to Length(FLowpass) - 1 do
    begin
     if Assigned(FLowpass[Channel][0])
      then FLowpass[Channel][0].Frequency := Value;
     if Assigned(FLowpass[Channel][1])
      then FLowpass[Channel][1].Frequency := Value;
    end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateLowpassFrequency;
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterLowpassOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  if Length(FLowpass) > 0 then
   for Channel := 0 to Length(FLowpass) - 1 do
    begin
     if Assigned(FLowpass[Channel][0])
      then FLowpass[Channel][0].Order := Round(Value);
     if Assigned(FLowpass[Channel][1])
      then FLowpass[Channel][1].Order := Round(Value);
    end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateLowpassSlope;
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterHighpassFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  if Length(FHighpass) > 0 then
   for Channel := 0 to Length(FHighpass) - 1 do
    begin
     if Assigned(FHighpass[Channel][0])
      then FHighpass[Channel][0].Frequency := Value;
     if Assigned(FHighpass[Channel][1])
      then FHighpass[Channel][1].Frequency := Value;
    end;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateHighpassFrequency;
end;

procedure TDualLinkwitzRileyFiltersModule.ParameterHighpassOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  if Length(FHighpass) > 0 then
   for Channel := 0 to Length(FHighpass) - 1 do
    begin
     if Assigned(FHighpass[Channel][0])
      then FHighpass[Channel][0].Order := Round(Value);
     if Assigned(FHighpass[Channel][1])
      then FHighpass[Channel][1].Order := Round(Value);
    end;
  FSign := 1 - 2 * (Round(Value) mod 2);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmLinkwitzRiley
  then TFmLinkwitzRiley(EditorForm).UpdateHighpassSlope;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleResume(Sender: TObject);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FLowpass) - 1 do
   begin
    if Assigned(FLowpass[Channel][0]) then FLowpass[Channel][0].ResetStates;
    if Assigned(FLowpass[Channel][1]) then FLowpass[Channel][1].ResetStates;
   end;
  for Channel := 0 to Length(FHighpass) - 1 do
   begin
    if Assigned(FHighpass[Channel][0]) then FHighpass[Channel][0].ResetStates;
    if Assigned(FHighpass[Channel][1]) then FHighpass[Channel][1].ResetStates;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   begin
    for Channel := 0 to Length(FLowpass) - 1 do
     begin
      if Assigned(FLowpass[Channel][0])
       then FLowpass[Channel][0].SampleRate := Abs(SampleRate);
      if Assigned(FLowpass[Channel][1])
       then FLowpass[Channel][1].SampleRate := Abs(SampleRate);
     end;
    for Channel := 0 to Length(FHighpass) - 1 do
     begin
      if Assigned(FHighpass[Channel][0])
       then FHighpass[Channel][0].SampleRate := Abs(SampleRate);
      if Assigned(FHighpass[Channel][1])
       then FHighpass[Channel][1].SampleRate := Abs(SampleRate);
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FLowpass) - 1
   do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleProcessLowpass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FLowpass) - 1
    do Outputs[Channel, Sample] := FLowpass[Channel][0].ProcessSample64(
         FLowpass[Channel][1].ProcessSample64(Inputs[Channel, Sample]));
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleProcessHighpass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FLowpass) - 1
    do Outputs[Channel, Sample] := FSign * FHighpass[Channel][0].ProcessSample64(
         FHighpass[Channel][1].ProcessSample64(Inputs[Channel, Sample]));
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TDualLinkwitzRileyFiltersModule.VSTModuleProcessBandpass(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   for Channel := 0 to Length(FLowpass) - 1 do
    begin
     Outputs[Channel, Sample] := FSign * FLowpass[Channel][0].ProcessSample64(
       FLowpass[Channel][1].ProcessSample64(
       FHighpass[Channel][0].ProcessSample64(
       FHighpass[Channel][1].ProcessSample64(Inputs[Channel, Sample]))));
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
