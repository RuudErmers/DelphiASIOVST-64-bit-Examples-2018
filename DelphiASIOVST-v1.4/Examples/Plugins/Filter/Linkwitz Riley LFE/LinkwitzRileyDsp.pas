unit LinkwitzRileyDsp;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilterButterworth;

type
  TLinkwitzRileyModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FButterworthCascade : array [0..1] of TButterworthLowPassFilter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} LinkwitzRileyGUI;

procedure TLinkwitzRileyModule.VSTModuleOpen(Sender: TObject);
var
  BandIndex : Integer;
begin
 for BandIndex := 0 to Length(FButterworthCascade) - 1 do
  begin
   FButterworthCascade[BandIndex] := TButterworthLowPassFilter.Create(2);
   FButterworthCascade[BandIndex].SampleRate := SampleRate;;
  end;

 // initialize parameters
 Parameter[0] := 100;
 Parameter[1] := 2;

 // set editor form class
 EditorFormClass := TFmLinkwitzRiley;
end;

procedure TLinkwitzRileyModule.VSTModuleClose(Sender: TObject);
var
  BandIndex : Integer;
begin
 for BandIndex := 0 to Length(FButterworthCascade) - 1
  do FreeAndNil(FButterworthCascade[BandIndex]);
end;

procedure TLinkwitzRileyModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(Parameter[Index])));
end;

procedure TLinkwitzRileyModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  BandIndex : Integer;
begin
 for BandIndex := 0 to Length(FButterworthCascade) - 1 do
  if Assigned(FButterworthCascade[BandIndex])
   then FButterworthCascade[BandIndex].Frequency := Value;
end;

procedure TLinkwitzRileyModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  BandIndex : Integer;
begin
 for BandIndex := 0 to Length(FButterworthCascade) - 1 do
  if Assigned(FButterworthCascade[BandIndex])
   then FButterworthCascade[BandIndex].Order := Round(Value);
end;

procedure TLinkwitzRileyModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   if Assigned(FButterworthCascade[0]) then FButterworthCascade[0].SampleRate := SampleRate;
   if Assigned(FButterworthCascade[1]) then FButterworthCascade[1].SampleRate := SampleRate;
  end;
end;

procedure TLinkwitzRileyModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SamplelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   if ChannelIndex = 3 then
    for SamplelIndex := 0 to SampleFrames - 1 do
     begin
      Outputs[ChannelIndex, SamplelIndex] := FButterworthCascade[0].ProcessSample64(
        FButterworthCascade[1].ProcessSample64(Inputs[ChannelIndex, SamplelIndex]));
     end
   else Move(Inputs[ChannelIndex, 0], Outputs[ChannelIndex, 0], SampleFrames * SizeOf(Single));
  end;
end;

end.
