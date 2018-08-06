unit LayeredFreqSplitDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  DAV_Types, DAV_VSTModule, DAV_DspFilterLinkwitzRiley;

type
  TLayeredFreqSplitModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
      const SampleFrames: Cardinal);
    procedure ParameterLayersChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIntegerDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLinkwitzRileyFilters : array [0..1, 0..9] of TLinkwitzRiley;
    FLayers               : Integer;
  public

  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  LayeredFreqSplitGUI;

procedure TLayeredFreqSplitModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  LayerIndex   : Integer;
begin
 // assign editor form class
 EditorFormClass := TFmLayeredFreqSplit;

 for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
  for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1 do
   begin
    FLinkwitzRileyFilters[ChannelIndex, LayerIndex] := TLinkwitzRiley.Create;
    with FLinkwitzRileyFilters[ChannelIndex, LayerIndex] do
     begin
      SampleRate := Self.SampleRate;
      Frequency := 1000;
     end;
   end;

 Parameter[0] := 1000;
 Parameter[1] := 4;
 Parameter[2] := 1;
end;

procedure TLayeredFreqSplitModule.ParameterLayersChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLayers := Round(Value) - 1;
end;

procedure TLayeredFreqSplitModule.ParameterIntegerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure TLayeredFreqSplitModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(Parameter[Index])));
end;

procedure TLayeredFreqSplitModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  LayerIndex   : Integer;
begin
 for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
  for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1
   do FLinkwitzRileyFilters[ChannelIndex, LayerIndex].Order := Round(Value);
end;

procedure TLayeredFreqSplitModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  LayerIndex   : Integer;
begin
 for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
  for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1
   do FLinkwitzRileyFilters[ChannelIndex, LayerIndex].Frequency := Value;
end;

procedure TLayeredFreqSplitModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  LayerIndex   : Integer;
  Data         : Double;
  Low, High    : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to Length(FLinkwitzRileyFilters) - 1 do
   begin
    Data := Inputs[ChannelIndex, SampleIndex];
    for LayerIndex := 0 to Length(FLinkwitzRileyFilters[ChannelIndex]) - 1 do
     begin
      FLinkwitzRileyFilters[ChannelIndex, LayerIndex].ProcessSample64(Data, Low, High);
      Data := Low + High;
      if LayerIndex = FLayers
       then Outputs[ChannelIndex, SampleIndex] := Data;
     end;
   end;
end;

end.
