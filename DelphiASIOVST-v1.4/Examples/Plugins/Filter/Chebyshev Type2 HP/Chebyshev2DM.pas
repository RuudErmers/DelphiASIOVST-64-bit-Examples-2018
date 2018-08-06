unit Chebyshev2DM;

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
  DAV_DspFilterChebyshevType2, DAV_VstWindowSizer;

type
  TChebyshev2HPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStopbandChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCorrectFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FFilter  : array [0..1] of TCustomChebyshev2HighpassFilter;
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
  Math, Chebyshev2GUI;

procedure TChebyshev2HPModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FFilter[ChannelIndex] := TChebyshev2HighpassFilter.Create;
   FFilter[ChannelIndex].SetFilterValues(1000, 0, -10);
  end;
(*
 FResizer := TVstWindowSizer.Create;
 FResizer.Effect := Self;
*)

 // Initial Parameters
 Parameter[0] := 1000;
 Parameter[1] := -24;
 Parameter[2] := 4;

 with Programs[0] do
  begin
   Parameter[0] := 1000;
   Parameter[1] := -24;
   Parameter[2] := 4;
  end;
end;

procedure TChebyshev2HPModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1
  do FreeAndNil(FFilter[ChannelIndex]);
// FreeAndNil(FResizer);
end;

procedure TChebyshev2HPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev2.Create(Self);
end;

procedure TChebyshev2HPModule.ParamStopbandChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  if Assigned(FFilter[ChannelIndex]) then FFilter[ChannelIndex].Stopband := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev2
  then TFmChebyshev2(EditorForm).UpdateStopband;
end;

procedure TChebyshev2HPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  if Assigned(FFilter[ChannelIndex])
   then FFilter[ChannelIndex].Order := Round(Value); //max(2, 2 * Round(0.5 * Value));

 // update GUI if necessary
 if EditorForm is TFmChebyshev2
  then TFmChebyshev2(EditorForm).UpdateOrder;
end;

procedure TChebyshev2HPModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TChebyshev2HPModule.ParameterCorrectFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  if Assigned(FFilter[ChannelIndex])
   then FFilter[ChannelIndex].FixFrequency := Value > 0.5;
end;

procedure TChebyshev2HPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  if Assigned(FFilter[ChannelIndex])
   then FFilter[ChannelIndex].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev2
  then TFmChebyshev2(EditorForm).UpdateFrequency;
end;

procedure TChebyshev2HPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FFilter[0].ProcessSample64(Inputs[0, SampleIndex]);
   Outputs[1, SampleIndex] := FFilter[1].ProcessSample64(Inputs[1, SampleIndex]);
  end;
end;

procedure TChebyshev2HPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FFilter[0].ProcessSample64(Inputs[0, SampleIndex]);
   Outputs[1, SampleIndex] := FFilter[1].ProcessSample64(Inputs[1, SampleIndex]);
  end;
end;

procedure TChebyshev2HPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to numInputs - 1 do
   if Assigned(FFilter[ChannelIndex])
    then FFilter[ChannelIndex].SampleRate := Abs(SampleRate);
end;

end.
