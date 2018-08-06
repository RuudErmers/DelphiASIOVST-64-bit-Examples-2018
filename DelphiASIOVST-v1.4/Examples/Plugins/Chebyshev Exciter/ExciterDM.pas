unit ExciterDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFilterButterworth,
  DAV_DspWaveshaper;

type
  TExciterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection        : TCriticalSection;
    FSourceLowpassFilter    : array [0..1, 0..1] of TButterworthLowPassFilter;
    FSourceHighpassFilter   : array [0..1, 0..1] of TButterworthHighPassFilter;
    FSplitterHighpassFilter : array [0..1, 0..1] of TButterworthHighPassFilter;
    FMix                    : array [0..1] of Single;
    FOverdriveGain          : Single;
    FChebyshevWaveshaper    : TChebyshevWaveshaperSquareShape;
    procedure InvertMix;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_VSTCustomModule, ExciterGUI, DAV_VSTModuleWithPrograms;

procedure TExciterDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TExciterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TExciterDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex, BandIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  for BandIndex := 0 to 1 do
   begin
    FSourceLowpassFilter[ChannelIndex, BandIndex] := TButterworthLowPassFilter.Create;
    FSourceHighpassFilter[ChannelIndex, BandIndex] := TButterworthHighPassFilter.Create;
    FSplitterHighpassFilter[ChannelIndex, BandIndex] := TButterworthHighPassFilter.Create;
   end;
 FChebyshevWaveshaper := TChebyshevWaveshaperSquareShape.Create;

 // initialize parameters
 Parameter[0] := 8000;
 Parameter[1] := 4;
 Parameter[2] := 50;
 Parameter[3] := 50;

 with Programs[0] do
  begin
   Parameter[0] := 8000;
   Parameter[1] := 4;
   Parameter[2] := 50;
   Parameter[3] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] := 10000;
   Parameter[1] := 2;
   Parameter[2] := 80;
   Parameter[3] := 70;
  end;

 // set editor form class
 EditorFormClass := TFmExciter;
end;

procedure TExciterDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex, BandIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  for BandIndex := 0 to 1 do
   begin
    FreeAndNil(FSourceLowpassFilter[ChannelIndex, BandIndex]);
    FreeAndNil(FSourceHighpassFilter[ChannelIndex, BandIndex]);
    FreeAndNil(FSplitterHighpassFilter[ChannelIndex, BandIndex]);
   end;
 FreeAndNil(FChebyshevWaveshaper);
end;

procedure TExciterDataModule.InvertMix;
begin
 if Round(ParameterByName['Order']) mod 2 = 1
  then FMix[0] := -Abs(FMix[0])
  else FMix[0] := Abs(FMix[0]);
end;

procedure TExciterDataModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FMix[1] := 0.01 * Value;
  FMix[0] := 1 - FMix[1];
  FMix[1] := 2 * FMix[1];
  InvertMix;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateMix;
end;

procedure TExciterDataModule.ParamShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FChebyshevWaveshaper)
   then FChebyshevWaveshaper.Shape := 2 - (0.01 * Value);
  FOverdriveGain := 1.4 - 0.4 * (0.01 * Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateShape;
end;

procedure TExciterDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to numInputs - 1 do
   for BandIndex := 0 to 1 do
    begin
     if Assigned(FSourceLowpassFilter[ChannelIndex, BandIndex])
      then FSourceLowpassFilter[ChannelIndex, BandIndex].Order    := Round(Value);
     if Assigned(FSourceHighpassFilter[ChannelIndex, BandIndex])
      then FSourceHighpassFilter[ChannelIndex, BandIndex].Order   := Round(Value);
     if Assigned(FSplitterHighpassFilter[ChannelIndex, BandIndex])
      then FSplitterHighpassFilter[ChannelIndex, BandIndex].Order := Round(Value);
    end;
  InvertMix;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateOrder;
end;

procedure TExciterDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 Assert(Value > 0);
 if Assigned(FChebyshevWaveshaper) then
  begin
   FCriticalSection.Enter;
   try
    FChebyshevWaveshaper.Order := Round(Min(22000, 0.48 * SampleRate) / Value + 0.5);
    for ChannelIndex := 0 to numInputs - 1 do
     for BandIndex := 0 to 1 do
      begin
       FSourceLowpassFilter[ChannelIndex, BandIndex].Frequency := Value;
       FSourceHighpassFilter[ChannelIndex, BandIndex].Frequency := Value;
       FSplitterHighpassFilter[ChannelIndex, BandIndex].Frequency := Value;
      end;
   finally
    FCriticalSection.Leave;
   end;
  end;

 // update GUI
 if EditorForm is TFmExciter
  then TFmExciter(EditorForm).UpdateTune;
end;

procedure TExciterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 if Abs(SampleRate) > 0 then
  begin
   FCriticalSection.Enter;
   try
    for ChannelIndex := 0 to numInputs - 1 do
     for BandIndex := 0 to 1 do
      begin
       if Assigned(FSourceLowpassFilter[ChannelIndex, BandIndex])
        then FSourceLowpassFilter[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
       if Assigned(FSourceHighpassFilter[ChannelIndex, BandIndex])
        then FSourceHighpassFilter[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
       if Assigned(FSplitterHighpassFilter[ChannelIndex, BandIndex])
        then FSplitterHighpassFilter[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
      end;
    if Assigned(FChebyshevWaveshaper)
     then FChebyshevWaveshaper.Order := Round(Min(22000, 0.48 * Abs(SampleRate)) / ParameterByName['Tune'] + 0.5);
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TExciterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  Input        : Double;
  Source       : Double;
  Low, High    : Double;
const
  CDenorm = 1E-31;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to Min(FBlockSize, SampleFrames) - 1 do
   for ChannelIndex := 0 to 1 do
    begin
     if IsNan(Inputs[ChannelIndex, SampleIndex])
      then Input  := CDenorm
      else Input  := CDenorm + Inputs[ChannelIndex, SampleIndex];
     Low    := FSourceLowpassFilter[ChannelIndex, 1].ProcessSample64(
               FSourceLowpassFilter[ChannelIndex, 0].ProcessSample64(Input));
     Source := FChebyshevWaveshaper.ProcessSample64(FOverdriveGain * Low);
     Source := FSourceHighpassFilter[ChannelIndex, 1].ProcessSample64(
               FSourceHighpassFilter[ChannelIndex, 0].ProcessSample64(Source));

     High  := FSplitterHighpassFilter[ChannelIndex, 1].ProcessSample64(
              FSplitterHighpassFilter[ChannelIndex, 0].ProcessSample64(Input));

     Outputs[ChannelIndex, SampleIndex] := Low + FMix[0] * High + FMix[1] * Source;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TExciterDataModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  Input        : Double;
  Source       : Double;
  Low, High    : Double;
const
  cDenorm = 1E-61;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to Min(FBlockSize, SampleFrames) - 1 do
   for ChannelIndex := 0 to 1 do
    begin
     if IsNan(Inputs[ChannelIndex, SampleIndex])
      then Input  := CDenorm
      else Input  := CDenorm + Inputs[ChannelIndex, SampleIndex];
     Low    := FSourceLowpassFilter[ChannelIndex, 1].ProcessSample64(
               FSourceLowpassFilter[ChannelIndex, 0].ProcessSample64(Input));
     Source := FChebyshevWaveshaper.ProcessSample64(FOverdriveGain * Low);
     Source := FSourceHighpassFilter[ChannelIndex, 1].ProcessSample64(
               FSourceHighpassFilter[ChannelIndex, 0].ProcessSample64(cDenorm + Source));

     High  := FSplitterHighpassFilter[ChannelIndex, 1].ProcessSample64(
              FSplitterHighpassFilter[ChannelIndex, 0].ProcessSample64(Input));

     Outputs[ChannelIndex, SampleIndex] := Low + FMix[0] * High + FMix[1] * Source;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TExciterDataModule.VSTModuleResume(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to 1 do
   for BandIndex := 0 to 1 do
    begin
     if Assigned(FSourceLowpassFilter[ChannelIndex, BandIndex])
      then FSourceLowpassFilter[ChannelIndex, BandIndex].ResetStates;
     if Assigned(FSourceHighpassFilter[ChannelIndex, BandIndex])
      then FSourceHighpassFilter[ChannelIndex, BandIndex].ResetStates;
     if Assigned(FSplitterHighpassFilter[ChannelIndex, BandIndex])
      then FSplitterHighpassFilter[ChannelIndex, BandIndex].ResetStates;
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
