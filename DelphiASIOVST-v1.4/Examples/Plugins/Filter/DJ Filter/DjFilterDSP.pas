unit DjFilterDSP;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspFilterButterworth;

const
  CGainRange = 15;

type
  TDjFilterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLow    : array [0..1] of TBasicLowShelfFilter;
    FMid    : array [0..1] of TBasicPeakFilter;
    FHigh   : array [0..1] of TBasicHighShelfFilter;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_VSTCustomModule, DjFilterGUI;

procedure TDjFilterDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  begin
   FLow[ChannelIndex]  := TBasicLowShelfFilter.Create;
   with FLow[ChannelIndex] do
    begin
     Frequency := 200;
     Gain      := 0;
     Bandwidth := 2;
    end;
   FMid[ChannelIndex]  := TBasicPeakFilter.Create;
   with FMid[ChannelIndex] do
    begin
     Frequency := 2000;
     Gain      := 0;
     Bandwidth := 2;
    end;
   FHigh[ChannelIndex] := TBasicHighShelfFilter.Create;
   with FHigh[ChannelIndex] do
    begin
     Frequency := 6000;
     Gain      := 0;
     Bandwidth := 2;
    end;
  end;

 // set editor form class
 EditorFormClass := TFmDjFilter;

 // initialize parameters
 Parameter[0] := 1;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 0;
 Parameter[6] := 0;
end;

procedure TDjFilterDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  begin
   FreeAndNil(FLow[ChannelIndex]);
   FreeAndNil(FMid[ChannelIndex]);
   FreeAndNil(FHigh[ChannelIndex]);
  end;
end;

procedure TDjFilterDataModule.ParamLowChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain : Single;
begin
 if Assigned(FLow[0]) then FLow[0].Gain := Value;
 if Assigned(FLow[1]) then FLow[1].Gain := Value;

 // update GUI
 if Assigned(EditorForm) then
  with TFmDjFilter(EditorForm)
   do UpdateLow;
end;

procedure TDjFilterDataModule.ParamMidChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain, BW : Single;
begin
 if Assigned(FMid[0]) then FMid[0].Gain := Value;
 if Assigned(FMid[1]) then FMid[1].Gain := Value;

 // update GUI
 if Assigned(EditorForm) then
  with TFmDjFilter(EditorForm)
   do UpdateMid;
end;

procedure TDjFilterDataModule.ParamHigh(Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain,
  Freq  : Single;
begin
 if Assigned(FHigh[0]) then FHigh[0].Gain := Value;
 if Assigned(FHigh[1]) then FHigh[1].Gain := Value;

 // update GUI
 if Assigned(EditorForm) then
  with TFmDjFilter(EditorForm)
   do UpdateHigh;
end;

procedure TDjFilterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to 1 do
   begin
    if Assigned(FLow[ChannelIndex]) then FLow[ChannelIndex].SampleRate   := SampleRate;
    if Assigned(FMid[ChannelIndex]) then FMid[ChannelIndex].SampleRate   := SampleRate;
    if Assigned(FHigh[ChannelIndex]) then FHigh[ChannelIndex].SampleRate := SampleRate;
   end;
end;

procedure TDjFilterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex, SampleIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Outputs[ChannelIndex, SampleIndex] :=
      FLow[ChannelIndex].ProcessSample64(
      FMid[ChannelIndex].ProcessSample64(
      FHigh[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex])));
   end;
end;

end.
