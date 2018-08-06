unit HardKneeCompressorDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  THardKneeCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSimpleCompressors : array [0..1] of TSimpleCompressor;
    function GetCompressor(Index: Integer): TSimpleCompressor;
  public
    property Compressor[Index: Integer]: TSimpleCompressor read GetCompressor;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, HardKneeCompressorGUI;

procedure THardKneeCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSimpleCompressors[0] := TSimpleCompressor.Create;
 FSimpleCompressors[1] := TSimpleCompressor.Create;
 EditorFormClass := TFmHardKneeCompressor;
end;

procedure THardKneeCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleCompressors[0]);
 FreeAndNil(FSimpleCompressors[1]);
end;

procedure THardKneeCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 // update compressor parameter on all channels
 for ChannelIndex := 0 to 1 do
  if Assigned(FSimpleCompressors[ChannelIndex])
   then FSimpleCompressors[ChannelIndex].Threshold_dB := Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateThreshold;
end;

procedure THardKneeCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 // update compressor parameter on all channels
 for ChannelIndex := 0 to 1 do
  if Assigned(FSimpleCompressors[ChannelIndex])
   then FSimpleCompressors[ChannelIndex].Ratio := 1 / Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateRatio;
end;

procedure THardKneeCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 // update compressor parameter on all channels
 for ChannelIndex := 0 to 1 do
  if Assigned(FSimpleCompressors[ChannelIndex])
   then FSimpleCompressors[ChannelIndex].Release := Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateRelease;
end;

procedure THardKneeCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 // update compressor parameter on all channels
 for ChannelIndex := 0 to 1 do
  if Assigned(FSimpleCompressors[ChannelIndex])
   then FSimpleCompressors[ChannelIndex].Attack := Value;

 // update GUI
 if EditorForm is TFmHardKneeCompressor then
  with TFmHardKneeCompressor(EditorForm)
   do UpdateAttack;
end;

function THardKneeCompressorDataModule.GetCompressor(
  Index: Integer): TSimpleCompressor;
begin
 if Index in [0..1]
  then Result := FSimpleCompressors[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure THardKneeCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleCompressors[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSimpleCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure THardKneeCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   if Assigned(FSimpleCompressors[0]) then FSimpleCompressors[0].SampleRate := Abs(SampleRate);
   if Assigned(FSimpleCompressors[1]) then FSimpleCompressors[1].SampleRate := Abs(SampleRate);
  end;
end;

end.
