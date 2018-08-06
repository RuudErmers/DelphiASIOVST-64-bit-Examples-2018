unit ButterworthDM;

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
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilterButterworth, DAV_VstWindowSizer;

type
  TButterworthHPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure StringToFrequencyParameter(Sender: TObject; const Index: Integer; const ParameterString: AnsiString; var Value: Single);
    procedure StringToOrderParameter(Sender: TObject; const Index: Integer; const ParameterString: AnsiString; var Value: Single);
  private
    FFilter: array of TCustomButterworthFilter;
  public  
    function Magnitude_dB(Frequency: Single): Single;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} Math, DAV_Approximations,
  DAV_DspFilter, ButterworthGUI;

procedure TButterworthHPModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 Assert(numInputs = numOutputs);
 Assert(numInputs > 0);
 SetLength(FFilter, numInputs);
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FFilter[ChannelIndex] := TButterworthHighPassFilter.Create;
   FFilter[ChannelIndex].SetFilterValues(20, 0);
  end;

 Parameter[0] := 20;
 Parameter[1] := 4;

 EditorFormClass := TFmButterworth;
end;

procedure TButterworthHPModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[ChannelIndex]);
end;

// parameter change

procedure TButterworthHPModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[ChannelIndex])
   then FFilter[ChannelIndex].Order := Round(Value);

 // update GUI
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm)
   do UpdateOrder;
end;

procedure TButterworthHPModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[ChannelIndex])
   then FFilter[ChannelIndex].Frequency := Value;

 // update GUI
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm)
   do UpdateFrequency;
end;


// parameter display

procedure TButterworthHPModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 4, 4))
  else PreDefined := AnsiString(FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 4, 4))
end;

procedure TButterworthHPModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := 'Hz'
  else PreDefined := 'kHz';
end;

procedure TButterworthHPModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;


// AnsiString to parameter conversion

procedure TButterworthHPModule.StringToFrequencyParameter(
  Sender: TObject; const Index: Integer; const ParameterString: AnsiString; var Value: Single);
var
  Str    : AnsiString;
  Indxes : array [0..1] of Integer;
  Mult   : Single;
begin
 Str := Trim(ParameterString);
 if Str = '' then Exit;

 // process unit extensions
 if AnsiPos(AnsiString('k'), Str) > 0 then Mult := 1E3 else
 if AnsiPos(AnsiString('m'), Str) > 0 then Mult := 1E-3
  else Mult := 1;

 Indxes[0] := 1;
 while (Indxes[0] <= Length(Str)) and
  {$IFDEF DELPHI14_UP}
  (not CharInSet(Str[Indxes[0]], ['0'..'9']))
  {$ELSE}
  (not (Str[Indxes[0]] in ['0'..'9']))
  {$ENDIF}
  do Inc(Indxes[0]);

 if (Indxes[0] > Length(Str)) then Exit;

 Indxes[1] := Indxes[0] + 1;
 while (Indxes[1] <= Length(Str)) and
  {$IFDEF DELPHI14_UP}
  CharInSet(Str[Indxes[1]], ['0'..'9'])
  {$ELSE}
  (Str[Indxes[1]] in ['0'..'9'])
  {$ENDIF}
  do Inc(Indxes[1]);

 Str := Copy(Str, Indxes[0], Indxes[1] - Indxes[0]);

 try
  Value := Mult * StrToFloat(string(Str));
 except
 end;
end;

procedure TButterworthHPModule.StringToOrderParameter(Sender: TObject;
  const Index: Integer; const ParameterString: AnsiString; var Value: Single);
var
  Str    : AnsiString;
  Indxes : array [0..1] of Integer;
begin
 Str := Trim(ParameterString);
 if Str = '' then Exit;

 Indxes[0] := 1;
 while (Indxes[0] <= Length(Str)) and
  {$IFDEF DELPHI14_UP}
  (not CharInSet(Str[Indxes[0]], ['0'..'9']))
  {$ELSE}
  (not (Str[Indxes[0]] in ['0'..'9']))
  {$ENDIF}
  do Inc(Indxes[0]);

 if (Indxes[0] > Length(Str)) then Exit;

 Indxes[1] := Indxes[0] + 1;
 while (Indxes[1] <= Length(Str)) and
  {$IFDEF DELPHI14_UP}
  CharInSet(Str[Indxes[1]], ['0'..'9'])
  {$ELSE}
  (Str[Indxes[1]] in ['0'..'9'])
  {$ENDIF}
  do Inc(Indxes[1]);

 Str := Copy(Str, Indxes[0], Indxes[1] - Indxes[0]);

 try
  Value := Round(StrToFloat(string(Str)));
 except
 end;
end;


// magnitude calculation

function TButterworthHPModule.Magnitude_dB(Frequency: Single): Single;
begin
 if Assigned(FFilter[0])
  then Result := 10 * FastLog10MinError5(FFilter[0].MagnitudeSquared(Frequency))
  else Result := 0; 
end;


// process related stuff

procedure TButterworthHPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FFilter) - 1
   do FFilter[ChannelIndex].SampleRate := Abs(SampleRate);
end;

procedure TButterworthHPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do Outputs[ChannelIndex, SampleIndex] := FFilter[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
end;

procedure TButterworthHPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do Outputs[ChannelIndex, SampleIndex] := FFilter[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
end;

end.
