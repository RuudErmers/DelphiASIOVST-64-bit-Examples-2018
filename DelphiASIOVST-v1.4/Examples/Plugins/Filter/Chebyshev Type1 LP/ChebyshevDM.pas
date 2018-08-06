unit ChebyshevDM;

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
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_DspFilterChebyshev,
  DAV_DspFilterChebyshevType1, DAV_VstWindowSizer;

type
  TChebyshevLPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRippleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure StringToFrequency(
      Sender: TObject; const Index: Integer; const ParameterString: AnsiString;
      var Value: Single);
    procedure StringToOrder(
      Sender: TObject; const Index: Integer; const ParameterString: AnsiString;
      var Value: Single);
    procedure ParamOrderDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamFrequencyDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FFilter  : array of TCustomChebyshev1LowpassFilter;
    FResizer : TVstWindowSizer;
  public
    property Resizer: TVstWindowSizer read FResizer;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} Math, ChebyshevGUI;

procedure TChebyshevLPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 Assert(numInputs = numOutputs);
 Assert(numInputs > 0);
 SetLength(FFilter, numInputs);
 for ch := 0 to Length(FFilter) - 1 do
  begin
   FFilter[ch] := TChebyshev1LowpassFilter.Create(4);
   FFilter[ch].SetFilterValues(1000, 0, 1);
  end;
(*
 FResizer := TVstWindowSizer.Create;
 FResizer.Effect := Self;
*)

 // Initial Parameters
 Parameter[0] := 1000;
 Parameter[1] := 1;
 Parameter[2] := 4;

 with Programs[0] do
  begin
   Parameter[0] := 1000;
   Parameter[1] := 1;
   Parameter[2] := 4;
  end;

  // set editor class
  EditorFormClass := TFmChebyshev;
end;

procedure TChebyshevLPModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[Channel]);
// FreeAndNil(FResizer);
end;

procedure TChebyshevLPModule.ParamRippleChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[Channel]) then FFilter[Channel].Ripple := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateRipple;
end;

procedure TChebyshevLPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[Channel])
   then FFilter[Channel].Order := Round(Value); // max(2, 2 * Round(0.5 * Value));

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateOrder;
end;

procedure TChebyshevLPModule.StringToFrequency(
  Sender: TObject; const Index: Integer; const ParameterString: AnsiString;
  var Value: Single);
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
  (not (Str[Indxes[0]] in ['0'..'9', ',', '.'])) do Inc(Indxes[0]);

 if (Indxes[0] >= Length(Str)) then Exit;

 Indxes[1] := Indxes[0] + 1;
 while (Indxes[1] <= Length(Str)) and
  (Str[Indxes[1]] in ['0'..'9', ',', '.']) do Inc(Indxes[1]);

 Str := Copy(Str, Indxes[0], Indxes[1] - Indxes[0]);

 try
  Value := Mult * StrToFloat(string(Str));
 except
 end;
end;

procedure TChebyshevLPModule.StringToOrder(
  Sender: TObject; const Index: Integer; const ParameterString: AnsiString;
  var Value: Single);
var
  Str    : AnsiString;
  Indxes : array [0..1] of Integer;
begin
 Str := Trim(ParameterString);
 if Str = '' then Exit;

 Indxes[0] := 1;
 while (Indxes[0] <= Length(Str)) and
  (not (Str[Indxes[0]] in ['0'..'9'])) do Inc(Indxes[0]);

 if (Indxes[0] > Length(Str)) then Exit;

 Indxes[1] := Indxes[0] + 1;
 while (Indxes[1] <= Length(Str)) and
  (Str[Indxes[1]] in ['0'..'9']) do Inc(Indxes[1]);

 Str := Copy(Str, Indxes[0], Indxes[1] - Indxes[0]);

 try
  Value := Round(StrToFloat(string(Str)));
 except
 end;
end;

procedure TChebyshevLPModule.ParamOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TChebyshevLPModule.ParamFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 4, 4))
  else PreDefined := AnsiString(FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 4, 4));
end;

procedure TChebyshevLPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[Channel])
   then FFilter[Channel].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateFrequency;
end;

procedure TChebyshevLPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if Assigned(FFilter[Channel])
   then FFilter[Channel].SampleRate := SampleRate;
end;

procedure TChebyshevLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample64(Inputs[Channel, Sample]);
end;

procedure TChebyshevLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample64(Inputs[Channel, Sample]);
end;

end.
