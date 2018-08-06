unit BassExtenderDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics, DAV_DspFrequencyDivider, 
  DAV_DspFilterButterworth;

type
  TBassExtenderModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMS32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMS64(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessLight32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessLight64(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessLightMS32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessLightMS64(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParamAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamBalanceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamCompressionMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDividerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamReleaseLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamShapeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamSplitOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFreqDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamFreqLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamModeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLowpass       : array [0..1, 0..1] of TButterworthLowpassFilter;
    FHighpass      : array [0..1, 0..1] of TButterworthHighpassFilter;
    FSign          : Single;
    FDivideMix     : array [0..1] of Single;
    FCompressorMix : array [0..1] of Single;
    FBalance       : array [0..1] of Single;
    FCompressor    : array [0..1] of TSimpleCompressor;
    FOctaveDivider : array [0..1] of TOcatveDivider;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common, 
  DAV_VSTModuleWithPrograms, DAV_VSTCustomModule, BassExtenderGUI;

procedure TBassExtenderModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 FDivideMix[0]     := 0.5;
 FDivideMix[1]     := 0.5;
 FCompressorMix[0] := 0.5;
 FCompressorMix[1] := 0.5;
 FBalance[0]       := 1;
 FBalance[1]       := 1;
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FLowpass[ChannelIndex, 0]  := TButterworthLowpassFilter.Create;
   FLowpass[ChannelIndex, 1]  := TButterworthLowpassFilter.Create;
   FHighpass[ChannelIndex, 0] := TButterworthHighpassFilter.Create;
   FHighpass[ChannelIndex, 1] := TButterworthHighpassFilter.Create;
   FLowpass[ChannelIndex, 0].SetFilterValues(80, 0);
   FLowpass[ChannelIndex, 1].SetFilterValues(80, 0);
   FHighpass[ChannelIndex, 0].SetFilterValues(80, 0);
   FHighpass[ChannelIndex, 1].SetFilterValues(80, 0);

   FCompressor[ChannelIndex]    := TSimpleCompressor.Create;
   FCompressor[ChannelIndex].AutoMakeUp := True;
   FOctaveDivider[ChannelIndex] := TOcatveDivider.Create;
  end;

 // set editor class
 EditorFormClass := TFmBassExtender;

 // default parameters
 Parameter[ 0] := 70;    // Split Frequency [Hz]
 Parameter[ 1] := 3;     // Split Order
 Parameter[ 2] := 50;    // Divider [%]
 Parameter[ 3] := 50;    // Shape [%]
 Parameter[ 4] := -20;   // Threshold [dB]
 Parameter[ 5] := 6;     // Ratio (1 : x)
 Parameter[ 6] := 50;    // Attack [탎]
 Parameter[ 7] := 50;    // Release [ms]
 Parameter[ 8] := 50;    // Mix [%]
 Parameter[ 9] := 0;     // Balance [%]
 Parameter[10] := 3;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)

 Programs[0].SetParameters(FParameter);

 with Programs[1] do
  begin
   Parameter[ 0] := 1000;  // Split Frequency [Hz]
   Parameter[ 1] := 1;     // Split Order
   Parameter[ 2] := 0;     // Divider [%]
   Parameter[ 3] := 0;     // Shape [%]
   Parameter[ 4] := 0;     // Threshold [dB]
   Parameter[ 5] := 1;     // Ratio (1 : x)
   Parameter[ 6] := 50;    // Attack [탎]
   Parameter[ 7] := 50;    // Release [ms]
   Parameter[ 8] := 0;     // Mix [%]
   Parameter[ 9] := 0;     // Balance [%]
   Parameter[10] := 0;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)
  end;

 with Programs[2] do
  begin
   Parameter[ 0] := 100;   // Split Frequency [Hz]
   Parameter[ 1] := 3;     // Split Order
   Parameter[ 2] := 30;    // Divider [%]
   Parameter[ 3] := 0;     // Shape [%]
   Parameter[ 4] := -10;   // Threshold [dB]
   Parameter[ 5] := 2;     // Ratio (1 : x)
   Parameter[ 6] := 100;   // Attack [탎]
   Parameter[ 7] := 200;   // Release [ms]
   Parameter[ 8] := 50;    // Mix [%]
   Parameter[ 9] := 0;     // Balance [%]
   Parameter[10] := 3;     // Mode (0 = Stereo, 1 = Mid only, 2 = Light, 1 = Light Mid only)
  end;

 with Programs[3] do
  begin
   Parameter[ 0] := 80;    // Split Frequency [Hz]
   Parameter[ 1] := 4;     // Split Order
   Parameter[ 2] := 60;    // Divider [%]
   Parameter[ 3] := 5;     // Shape [%]
   Parameter[ 4] := -15;   // Threshold [dB]
   Parameter[ 5] := 4;     // Ratio (1 : x)
   Parameter[ 6] := 60;    // Attack [탎]
   Parameter[ 7] := 300;   // Release [ms]
   Parameter[ 8] := 70;    // Mix [%]
   Parameter[ 9] := 5;     // Balance [%]
   Parameter[10] := 1;     // Mode (0 = Stereo, 1 = Mid only)
  end;
end;

procedure TBassExtenderModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FreeAndNil(FLowpass[ChannelIndex, 0]);
   FreeAndNil(FLowpass[ChannelIndex, 1]);
   FreeAndNil(FHighpass[ChannelIndex, 0]);
   FreeAndNil(FHighpass[ChannelIndex, 1]);
   FreeAndNil(FCompressor[ChannelIndex]);
   FreeAndNil(FOctaveDivider[ChannelIndex]);
  end;
end;

procedure TBassExtenderModule.ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex, Order : Integer;
begin
 Order := Round(Value);
 for ChannelIndex := 0 to 1 do
  begin
   if Assigned(FLowpass[ChannelIndex, 0]) then FLowpass[ChannelIndex, 0].Order  := Order;
   if Assigned(FLowpass[ChannelIndex, 1]) then FLowpass[ChannelIndex, 1].Order  := Order;
   if Assigned(FHighpass[ChannelIndex, 0]) then FHighpass[ChannelIndex, 0].Order := Order;
   if Assigned(FHighpass[ChannelIndex, 1]) then FHighpass[ChannelIndex, 1].Order := Order;
  end;

 FSign := 1 - 2 * (Order mod 2);

 // update GUI
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateSplitOrder;
end;

procedure TBassExtenderModule.ParamReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then FCompressor[0].Release := Value;
 if Assigned(FCompressor[1]) then FCompressor[1].Release := Value;

 // update GUI
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateRelease;
end;

procedure TBassExtenderModule.ParamReleaseLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[index] < 1000
  then PreDefined := 'ms'
  else PreDefined := 's'
end;

procedure TBassExtenderModule.ParamAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Attack : Single;
begin
 Attack := Parameter[index];
 if Attack < 1000
  then PreDefined := AnsiString(FloatToStrF(Attack, ffGeneral, 3, 2))
  else PreDefined := AnsiString(FloatToStrF(Attack * 1E-3, ffGeneral, 3, 2));
end;

procedure TBassExtenderModule.ParamReleaseDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Release : Single;
begin
 Release := Parameter[index];
 if Release < 1000
  then PreDefined := AnsiString(FloatToStrF(Release, ffGeneral, 3, 2))
  else PreDefined := AnsiString(FloatToStrF(Release * 1E-3, ffGeneral, 3, 2));
end;

procedure TBassExtenderModule.ParamRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] = 1000
  then PreDefined := '1 : oo'
  else PreDefined := '1 : ' + AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 3, 5));
end;

procedure TBassExtenderModule.ParamThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then FCompressor[0].Threshold_dB := Value;
 if Assigned(FCompressor[1]) then FCompressor[1].Threshold_dB := Value;

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateThreshold;
end;

procedure TBassExtenderModule.ParamRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then
  begin
   FCompressor[0].Ratio := 1 / Value;
   if Assigned(FCompressor[1])
    then FCompressor[1].Ratio := FCompressor[0].Ratio;
  end;

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateRatio;
end;

procedure TBassExtenderModule.ParamShapeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FOctaveDivider[0]) then FOctaveDivider[0].Shape := 0.01 * Value;
 if Assigned(FOctaveDivider[1]) then FOctaveDivider[1].Shape := 0.01 * Value;

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateShape;
end;

procedure TBassExtenderModule.ParamDividerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDivideMix[0] := Limit(0.01 * Value, 0, 1);
 FDivideMix[1] := 1 - FDivideMix[0];

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateDivider;
end;

procedure TBassExtenderModule.ParamCompressionMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressorMix[0] := Limit(0.01 * Value, 0, 1);
 FCompressorMix[1] := 1 - FCompressorMix[0];

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateCompressionMix;
end;

procedure TBassExtenderModule.ParamBalanceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FBalance[1] := Limit(1 + 0.01 * Value, 0, 2);
 FBalance[0] := 2 - FBalance[1];

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateBalance;
end;

procedure TBassExtenderModule.ParamSplitOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(2 * Parameter[Index])));
end;

procedure TBassExtenderModule.ParamFreqDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq < 1000
  then Predefined := AnsiString(FloatToStrF(Freq, ffGeneral, 4, 4))
  else Predefined := AnsiString(FloatToStrF(Freq * 1E-3, ffGeneral, 4, 4));
end;

procedure TBassExtenderModule.ParamFreqLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 1000
  then Predefined := 'Hz'
  else Predefined := 'kHz'
end;

procedure TBassExtenderModule.ParamModeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Parameter[Index]) of
  0: begin
      OnProcess := VSTModuleProcess32;
      OnProcess32Replacing := VSTModuleProcess32;
      OnProcess64Replacing := VSTModuleProcess64;
     end;
  1: begin
      OnProcess := VSTModuleProcessMS32;
      OnProcess32Replacing := VSTModuleProcessMS32;
      OnProcess64Replacing := VSTModuleProcessMS64;
     end;
  2: begin
      OnProcess := VSTModuleProcessLight32;
      OnProcess32Replacing := VSTModuleProcessLight32;
      OnProcess64Replacing := VSTModuleProcessLight64;
     end;
  3: begin
      OnProcess := VSTModuleProcessLightMS32;
      OnProcess32Replacing := VSTModuleProcessLightMS32;
      OnProcess64Replacing := VSTModuleProcessLightMS64;
     end;
 end;
end;

procedure TBassExtenderModule.ParamModeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0: PreDefined := 'Stereo';
  1: PreDefined := 'Mid only';
  2: PreDefined := 'Light';
  3: PreDefined := 'Light Mid Only';
 end;
end;

procedure TBassExtenderModule.ParamAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FCompressor[0]) then FCompressor[0].Attack := 1E-3 * Value;
 if Assigned(FCompressor[1]) then FCompressor[1].Attack := 1E-3 * Value;

 // update GUI
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateAttack;
end;

procedure TBassExtenderModule.ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[index] < 1000
  then PreDefined := '탎'
  else PreDefined := 'ms'
end;

procedure TBassExtenderModule.ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to 1 do
  begin
   if Assigned(FLowpass[ChannelIndex, 0]) then FLowpass[ChannelIndex, 0].Frequency  := Value;
   if Assigned(FLowpass[ChannelIndex, 1]) then FLowpass[ChannelIndex, 1].Frequency  := Value;
   if Assigned(FHighpass[ChannelIndex, 0]) then FHighpass[ChannelIndex, 0].Frequency := Value;
   if Assigned(FHighpass[ChannelIndex, 1]) then FHighpass[ChannelIndex, 1].Frequency := Value;
  end;

 // update GUI 
 if EditorForm is TFmBassExtender
  then TFmBassExtender(EditorForm).UpdateSplitFrequency;
end;

procedure TBassExtenderModule.VSTModuleProcess32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  L, H         : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to 1 do
   begin
    L := cDenorm64 + Inputs[ChannelIndex, SampleIndex];
    H := FHighpass[ChannelIndex, 0].ProcessSample64(cDenorm64 +
         FHighpass[ChannelIndex, 1].ProcessSample64(FSign * L));
    L := FLowpass[ChannelIndex, 1].ProcessSample64(L);
    L := FLowpass[ChannelIndex, 0].ProcessSample64(FDivideMix[0] *
      FOctaveDivider[ChannelIndex].ProcessSample32(L) + FDivideMix[1] * L);
    FCompressor[ChannelIndex].InputSample(L);
    Outputs[ChannelIndex, SampleIndex] := FBalance[0] * (FCompressorMix[0] * FCompressor[ChannelIndex].GainSample(L) +
                           FCompressorMix[1] * L) + FBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcess64(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  L, H         : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to 1 do
   begin
    L := cDenorm64 + Inputs[ChannelIndex, SampleIndex];
    H := FHighpass[ChannelIndex, 0].ProcessSample64(cDenorm64 +
         FHighpass[ChannelIndex, 1].ProcessSample64(FSign * L));
    L := FLowpass[ChannelIndex, 1].ProcessSample64(L);
    L := FLowpass[ChannelIndex, 0].ProcessSample64(FDivideMix[0] *
      FOctaveDivider[ChannelIndex].ProcessSample64(L) + FDivideMix[1] * L);
    FCompressor[ChannelIndex].InputSample(L);
    Outputs[ChannelIndex, SampleIndex] := FBalance[0] * (FCompressorMix[0] * FCompressor[ChannelIndex].GainSample(L) +
                           FCompressorMix[1] * L) + FBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcessMS32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  L, H, M, S  : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   // Mid
   L := cDenorm64 + Inputs[0, SampleIndex] + Inputs[1, SampleIndex];
   H := FHighpass[0, 0].ProcessSample64(cDenorm64 +
        FHighpass[0, 1].ProcessSample64(FSign * L));
   L := FLowpass[0, 1].ProcessSample64(L);
   L := FLowpass[0, 0].ProcessSample64(FDivideMix[0] *
     FOctaveDivider[0].ProcessSample32(L) + FDivideMix[1] * L);
   FCompressor[0].InputSample(L);
   M := FBalance[0] * (FCompressorMix[0] * FCompressor[0].GainSample(L) +
                       FCompressorMix[1] * L) + FBalance[1] * H;

   // Side
   L := Inputs[0, SampleIndex] - Inputs[1, SampleIndex];
   H := FHighpass[1, 0].ProcessSample64(
        FHighpass[1, 1].ProcessSample64(FSign * L));
   L := FLowpass[1, 0].ProcessSample32(
        FLowpass[1, 1].ProcessSample32(L));
   S := FBalance[0] * L + FBalance[1] * H;

   Outputs[0, SampleIndex] := 0.5 * (M + S);
   Outputs[1, SampleIndex] := 0.5 * (M - S);
  end;
end;

procedure TBassExtenderModule.VSTModuleProcessMS64(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  L, H, M, S  : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   // Mid
   L := cDenorm64 + Inputs[0, SampleIndex] + Inputs[1, SampleIndex];
   H := FHighpass[0, 0].ProcessSample64(cDenorm64 +
        FHighpass[0, 1].ProcessSample64(FSign * L));
   L := FLowpass[0, 1].ProcessSample64(L);
   L := FLowpass[0, 0].ProcessSample64(FDivideMix[0] *
     FOctaveDivider[0].ProcessSample64(L) + FDivideMix[1] * L);
   FCompressor[0].InputSample(L);
   M := FBalance[0] * (FCompressorMix[0] * FCompressor[0].GainSample(L) +
                       FCompressorMix[1] * L) + FBalance[1] * H;

   // Side
   L := Inputs[0, SampleIndex] - Inputs[1, SampleIndex];
   H := FHighpass[1, 0].ProcessSample64(
        FHighpass[1, 1].ProcessSample64(FSign * L));
   L := FLowpass[1, 0].ProcessSample64(
        FLowpass[1, 1].ProcessSample64(L));
   S := FBalance[0] * L + FBalance[1] * H;

   Outputs[0, SampleIndex] := 0.5 * (M + S);
   Outputs[1, SampleIndex] := 0.5 * (M - S);
  end;
end;

procedure TBassExtenderModule.VSTModuleProcessLight32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  L, H         : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to 1 do
   begin
    L := FLowpass[ChannelIndex, 1].ProcessSample64(cDenorm64 + Inputs[ChannelIndex, SampleIndex]);
    H := FHighpass[ChannelIndex, 1].ProcessSample64(Inputs[ChannelIndex, SampleIndex] - L);
    L := FLowpass[ChannelIndex, 0].ProcessSample64(FDivideMix[0] *
      FOctaveDivider[ChannelIndex].ProcessSample32(L) + FDivideMix[1] * L);
    FCompressor[ChannelIndex].InputSample(L);
    Outputs[ChannelIndex, SampleIndex] := FBalance[0] * (FCompressorMix[0] * FCompressor[ChannelIndex].GainSample(L) +
                           FCompressorMix[1] * L) + FBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcessLight64(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  L, H         : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to 1 do
   begin
    L := FLowpass[ChannelIndex, 1].ProcessSample64(cDenorm64 + Inputs[ChannelIndex, SampleIndex]);
    H := FHighpass[ChannelIndex, 1].ProcessSample64(Inputs[ChannelIndex, SampleIndex] - L);
    L := FLowpass[ChannelIndex, 0].ProcessSample64(FDivideMix[0] *
      FOctaveDivider[ChannelIndex].ProcessSample64(L) + FDivideMix[1] * L);
    FCompressor[ChannelIndex].InputSample(L);
    Outputs[ChannelIndex, SampleIndex] := FBalance[0] * (FCompressorMix[0] * FCompressor[ChannelIndex].GainSample(L) +
                           FCompressorMix[1] * L) + FBalance[1] * H;
   end;
end;

procedure TBassExtenderModule.VSTModuleProcessLightMS32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  L, H, M, S  : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   // Mid
   M := cDenorm64 + Inputs[0, SampleIndex] + Inputs[1, SampleIndex];
   L := FLowpass[0, 1].ProcessSample64(M);
   H := FHighpass[0, 1].ProcessSample64(M - L);
   L := FLowpass[0, 0].ProcessSample64(FDivideMix[0] *
     FOctaveDivider[0].ProcessSample32(L) + FDivideMix[1] * L);

   FCompressor[0].InputSample(L);
   M := FBalance[0] * (FCompressorMix[0] * FCompressor[0].GainSample(L) +
                       FCompressorMix[1] * L) + FBalance[1] * H;

   // Side
   L := Inputs[0, SampleIndex] - Inputs[1, SampleIndex];
   H := FHighpass[1, 0].ProcessSample64(
        FHighpass[1, 1].ProcessSample64(FSign * L));
   L := FLowpass[1, 0].ProcessSample64(
        FLowpass[1, 1].ProcessSample64(L));
   S := FBalance[0] * L + FBalance[1] * H;

   Outputs[0, SampleIndex] := 0.5 * (M + S);
   Outputs[1, SampleIndex] := 0.5 * (M - S);
  end;
end;

procedure TBassExtenderModule.VSTModuleProcessLightMS64(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  L, H, M, S  : Double;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   // Mid
   M := cDenorm64 + Inputs[0, SampleIndex] + Inputs[1, SampleIndex];
   L := FLowpass[0, 1].ProcessSample64(M);
   H := FHighpass[0, 1].ProcessSample64(M - L);
   L := FLowpass[0, 0].ProcessSample64(FDivideMix[0] *
     FOctaveDivider[0].ProcessSample64(L) + FDivideMix[1] * L);

   FCompressor[0].InputSample(L);
   M := FBalance[0] * (FCompressorMix[0] * FCompressor[0].GainSample(L) +
                       FCompressorMix[1] * L) + FBalance[1] * H;

   // Side
   L := Inputs[0, SampleIndex] - Inputs[1, SampleIndex];
   H := FHighpass[1, 0].ProcessSample64(
        FHighpass[1, 1].ProcessSample64(FSign * L));
   L := FLowpass[1, 0].ProcessSample64(
        FLowpass[1, 1].ProcessSample64(L));
   S := FBalance[0] * L + FBalance[1] * H;

   Outputs[0, SampleIndex] := 0.5 * (M + S);
   Outputs[1, SampleIndex] := 0.5 * (M - S);
  end;
end;

end.
