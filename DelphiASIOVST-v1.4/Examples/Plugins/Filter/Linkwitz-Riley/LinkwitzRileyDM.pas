unit LinkwitzRileyDM;

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
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspFilterLinkwitzRiley;

type
  TLinkwitzRileyModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLinkwitzRiley : array of TLinkwitzRiley;
  public
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TLinkwitzRileyModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 Assert(numOutputs = 2 * numInputs);
 SetLength(FLinkwitzRiley, numInputs);
 for Channel := 0 to Length(FLinkwitzRiley) - 1 do
  begin
   FLinkwitzRiley[Channel] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel].SampleRate := SampleRate;
  end;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 {$ENDIF}

 Parameter[0] := 1000;
 Parameter[1] := 2;
end;

procedure TLinkwitzRileyModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FLinkwitzRiley) - 1
  do FreeAndNil(FLinkwitzRiley[Channel]);
end;

procedure TLinkwitzRileyModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(Parameter[Index])));
end;

procedure TLinkwitzRileyModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then PreDefined := AnsiString(FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3));
end;

procedure TLinkwitzRileyModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TLinkwitzRileyModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FLinkwitzRiley) - 1 do
  if Assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].Frequency := Value;
end;

procedure TLinkwitzRileyModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FLinkwitzRiley) - 1 do
  if Assigned(FLinkwitzRiley[Channel])
   then FLinkwitzRiley[Channel].Order := Round(Value);
end;

procedure TLinkwitzRileyModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FLinkwitzRiley) - 1 do
   if Assigned(FLinkwitzRiley[Channel])
    then FLinkwitzRiley[Channel].SampleRate := Abs(SampleRate);
end;

procedure TLinkwitzRileyModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel: Integer;
begin
 // CDenorm32 +
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FLinkwitzRiley) - 1
   do FLinkwitzRiley[Channel].ProcessSample32(Inputs[Channel, Sample],
        Outputs[2 * Channel, Sample], Outputs[2 * Channel + 1, Sample])
end;

end.
