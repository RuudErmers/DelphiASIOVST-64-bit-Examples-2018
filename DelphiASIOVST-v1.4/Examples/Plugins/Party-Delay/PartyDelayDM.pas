unit PartyDelayDM;

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
  Windows, Messages, SyncObjs, SysUtils, Classes, Forms, DAV_Types,
  DAV_VSTModule, DAV_DspDelayLines, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspFrequencyShifter;

type
  TPartyDelayDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPanChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPolarityDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterPolarityChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterShiftFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyShiftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBalanceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterShiftOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FOnOff           : array [0..3] of Boolean;
    FPan             : array [0..3] of Single;
    FGain            : array [0..3] of Single;
    FPolarity        : array [0..3] of Boolean;
    FDelayLine       : array [0..3] of TDelayLineTime32;
    FFeedback        : array [0..3] of Single;
    FFilter          : array [0..3] of TCustomBandwidthIIRFilter;
    FFreqShift       : array [0..3] of TBodeFrequencyShifter32;
    FShiftOrder      : array [0..3] of Integer;
    FShiftFreq       : array [0..3] of Single;
    FBalance         : array [0..3] of Single;
    FFSGain          : array [0..3, 0..2] of Single;
    FScale           : array [0..3, 0..1] of Single;
    FDrive           : array [0..3] of Single;
    FOutMix          : array [0..3, 0..1] of Single;
    FLastOutput      : array [0..3] of Single;
    FDry, FWet       : Single;
    FParPrBand       : Integer;
    procedure OutMixChanged(Band: Integer);
  protected
    procedure CalculateScale(Index: Integer);
  public
    property ParametersPerBand: Integer read FParPrBand;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_Math, DAV_Complex, DAV_Approximations,
  DAV_VSTModuleWithPrograms, PartyDelayGui;

procedure TPartyDelayDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 FParPrBand := 15;
end;

procedure TPartyDelayDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TPartyDelayDataModule.VSTModuleOpen(Sender: TObject);
var
  Band : Integer;
begin
 FDry := 0.707;
 FWet := 0.707;
 for Band := 0 to Length(FDelayLine) - 1 do
  begin
   FDelayLine[Band] := TDelayLineTime32.Create;
   FDelayLine[Band].Samplerate := 44100;
  end;

 for Band := 0 to Length(FFilter) - 1
  do FFilter[Band] := TBasicGainFilter.Create;

 // Band 1
 Parameter[ 0] := 1;
 Parameter[ 1] := -100;
 Parameter[ 2] := -3;
 Parameter[ 3] := 0;
 Parameter[ 4] := 20;
 Parameter[ 5] := 20;
 Parameter[ 6] := 1;
 Parameter[ 7] := 800;
 Parameter[ 8] := 0;
 Parameter[ 9] := 1;
 Parameter[10] := 0.1;
 Parameter[11] := 0.3;
 Parameter[12] := 12;
 Parameter[13] := 2;
 Parameter[14] := -100;

 // Band 2
 Parameter[15] := 1;
 Parameter[16] := 100;
 Parameter[17] := -3;
 Parameter[18] := 0;
 Parameter[19] := 20;
 Parameter[20] := 20;
 Parameter[21] := 1;
 Parameter[22] := 800;
 Parameter[23] := 0;
 Parameter[24] := 1;
 Parameter[25] := 0.9;
 Parameter[26] := 0.3;
 Parameter[27] := 12;
 Parameter[28] := 2;
 Parameter[29] := 100;

 // Band 3
 Parameter[30] := 0;
 Parameter[31] := 0;
 Parameter[32] := -6;
 Parameter[33] := 0;
 Parameter[34] := 20;
 Parameter[35] := 20;
 Parameter[36] := 1;
 Parameter[37] := 800;
 Parameter[38] := 0;
 Parameter[39] := 1;
 Parameter[40] := 0;
 Parameter[41] := 1;
 Parameter[42] := 12;
 Parameter[43] := 0;
 Parameter[44] := 0;

 // Band 4
 Parameter[55] := 0;
 Parameter[56] := 0;
 Parameter[57] := -6;
 Parameter[58] := 0;
 Parameter[59] := 20;
 Parameter[60] := 20;
 Parameter[61] := 1;
 Parameter[62] := 800;
 Parameter[63] := 0;
 Parameter[64] := 1;
 Parameter[65] := 0;
 Parameter[66] := 1;
 Parameter[67] := 12;
 Parameter[68] := 0;
 Parameter[69] := 0;

 // Mix
 Parameter[70] := 50;

 // set editor form class
 EditorFormClass := TFmPartyDelay;
end;

procedure TPartyDelayDataModule.VSTModuleClose(Sender: TObject);
var
  Band : Integer;
begin
 for Band := 0 to Length(FDelayLine) - 1
  do FreeAndNil(FDelayLine[Band]);
 for Band := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[Band]);
end;

procedure TPartyDelayDataModule.ParameterPanChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Pan : Single;
begin
 Pan := 0.5 * (1 + 0.01 * Value);
 if FPan[Index div ParametersPerBand] <> Pan then
  begin
   FPan[Index div ParametersPerBand] := Pan;
   CalculateScale(Index div ParametersPerBand);
  end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdatePan;
end;

procedure TPartyDelayDataModule.ParameterPolarityDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] < 0.5
  then PreDefined := '+'
  else PreDefined := '-';
end;

procedure TPartyDelayDataModule.ParameterShiftFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 if FShiftFreq[Band] <> Value then
  begin
   FShiftFreq[Band] := Value;
   if Assigned(FFreqShift[Band])
    then FFreqShift[Band].Frequency := Value;
  end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateShiftFrequency;
end;

// Limit a Value to be Lower <= Value <= Upper
function LimitInt(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
 if Value < Lower then Result := Lower else
 if Value > Upper then Result := Upper else Result := Value;
end;

procedure TPartyDelayDataModule.ParameterShiftOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 if FShiftOrder[Band] <> LimitInt(Round(Value), 1, 32) then
  begin
   FCriticalSection.Enter;
   try
    FShiftOrder[Band] := LimitInt(Round(Value), 1, 32);
    if Assigned(FFreqShift[Band])
     then FFreqShift[Band].CoefficientCount := FShiftOrder[Band];
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TPartyDelayDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := 0.01 * Value;
 FDry := sqrt(1 - FWet);
 FWet := sqrt(FWet);
 OutMixChanged(0);
 OutMixChanged(1);
 OutMixChanged(2);
 OutMixChanged(3);

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateMix;
end;

procedure TPartyDelayDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TPartyDelayDataModule.ParameterOnOffChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 FOnOff[Band] := Value > 0.5;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateActive;
end;

procedure TPartyDelayDataModule.ParameterDriveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 FDrive[Band] := 0.04 * Value;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateDrive;
end;

procedure TPartyDelayDataModule.ParameterBalanceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band  : Integer;
begin
 Band := Index div ParametersPerBand;
 if FBalance[Band] <> Value then
  begin
   FBalance[Band] := Value;
   OutMixChanged(Band);
  end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateBalance;
end;


procedure TPartyDelayDataModule.OutMixChanged(Band: Integer);
begin
 FOutMix[Band, 1] := 0.5 * (0.01 * FBalance[Band] + 1);
 FOutMix[Band, 0] := FWet * sqrt(1 - FOutMix[Band, 1]);
 FOutMix[Band, 1] := FWet * sqrt(FOutMix[Band, 1]);
end;

procedure TPartyDelayDataModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 if Assigned(FFilter[Band])
  then FFilter[Band].BandWidth := Value;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateFilterBandwidth;
end;

procedure TPartyDelayDataModule.ParameterFilterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 if Assigned(FFilter[Band])
  then FFilter[Band].Gain := Value;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateFilterGain;
end;

procedure TPartyDelayDataModule.ParameterFilterFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 if Assigned(FFilter[Band])
  then FFilter[Band].Frequency := Value;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateFilterFrequency;
end;

procedure TPartyDelayDataModule.ParameterFrequencyShiftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band  : Integer;
  Cmplx : TComplex32;
begin
 Band := Index div ParametersPerBand;
 if Value <> 0 then
  begin
   if not Assigned(FFreqShift[Band]) then
    begin
     FCriticalSection.Enter;
     try
      FFreqShift[Band] := TBodeFrequencyShifter32.Create;
      with FFreqShift[Band] do
       begin
        SampleRate := Self.SampleRate;
        Frequency := FShiftFreq[Band];
        CoefficientCount := FShiftOrder[Band];
       end;
     finally
      FCriticalSection.Leave;
     end;
    end;

   GetSinCos(2 * Pi * Value, Cmplx.Im, Cmplx.Re);

   if Cmplx.Re >= 0 then
    begin
     // Flanger
     FFSGain[Band, 0] := Cmplx.Re;
     if Cmplx.Im < 0 then
      begin
       FFSGain[Band, 1] := -Cmplx.Im;
       FFSGain[Band, 2] := 0;
      end
     else
      begin
       FFSGain[Band, 1] := 0;
       FFSGain[Band, 2] := Cmplx.Im;
      end
    end
   else
    begin
     // RingMod
     FFSGain[Band, 0] := 0;
     if Cmplx.Im < 0 then
      begin
       FFSGain[Band, 1] := 0.5 * (1 - Cmplx.Im);
       FFSGain[Band, 2] := sqrt(1 - FFSGain[Band, 1]);
       FFSGain[Band, 1] := sqrt(FFSGain[Band, 1]);
      end
     else
      begin
       FFSGain[Band, 2] := 0.5 * (1 + Cmplx.Im);
       FFSGain[Band, 1] := sqrt(1 - FFSGain[Band, 2]);
       FFSGain[Band, 2] := sqrt(FFSGain[Band, 2]);
      end
    end;
  end
 else
  if Assigned(FFreqShift[Band])
   then FreeAndNil(FFreqShift[Band]);

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateFrequencyShifter;
end;

procedure TPartyDelayDataModule.ParameterFilterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'None';
  1 : PreDefined := 'Lowcut';
  2 : PreDefined := 'Lowshelf';
  3 : PreDefined := 'Peak';
  4 : PreDefined := 'Bandpass';
  5 : PreDefined := 'Allpass';
  6 : PreDefined := 'Notch';
  7 : PreDefined := 'Highshelf';
  8 : PreDefined := 'Highcut';
 end;
end;

procedure TPartyDelayDataModule.ParameterFilterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band      : Integer;
  OldFilter : TCustomFilter;
const
  CFilterClasses : array [0..8] of TBandwidthIIRFilterClass = (
    TBasicGainFilter, TBasicLowcutFilter, TBasicLowShelfFilter,
    TBasicPeakFilter, TBasicBandpassFilter, TBasicAllpassFilter,
    TBasicNotchFilter, TBasicHighShelfFilter, TBasicHighcutFilter);
begin
 Band := Index div ParametersPerBand;
 OldFilter := FFilter[Band];

 FCriticalSection.Enter;
 try
  if not (OldFilter is CFilterClasses[Round(Value)]) then
   begin
    FFilter[Band] := CFilterClasses[Round(Value)].Create;
    if OldFilter <> nil then
     begin
      FFilter[Band].Assign(OldFilter);
      FreeAndNil(OldFilter);
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateFilterType;
end;

procedure TPartyDelayDataModule.ParameterDelayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := Index div ParametersPerBand;
 if Assigned(FDelayLine[Band]) then
  begin
   FDelayLine[Band].Time := 0.001 * Value;
   FDelayLine[Band].Reset;
  end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateDelay;
end;

procedure TPartyDelayDataModule.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedback[Index div ParametersPerBand] := 0.01 * Value;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateFeedback;
end;

procedure TPartyDelayDataModule.ParameterPolarityChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Polarity : Boolean;
begin
 Polarity := Value > 0.5;
 if FPolarity[Index div ParametersPerBand] <> Polarity then
  begin
   FPolarity[Index div ParametersPerBand] := Polarity;
   CalculateScale(Index div ParametersPerBand);
  end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateInvert;
end;

procedure TPartyDelayDataModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Gain : Single;
begin
 Gain := db_to_Amp(Value);
 if FGain[Index div ParametersPerBand] <> Gain then
  begin
   FGain[Index div ParametersPerBand] := Gain;
   CalculateScale(Index div ParametersPerBand);
  end;

 if EditorForm is TFmPartyDelay
  then TFmPartyDelay(EditorForm).UpdateGain;
end;

procedure TPartyDelayDataModule.CalculateScale(Index: Integer);
begin
 FScale[Index, 0] := FGain[Index] * (1 - FPan[Index]);
 FScale[Index, 1] := FGain[Index] * FPan[Index];
 if FPolarity[Index] then
  begin
   FScale[Index, 0] := -FScale[Index, 0];
   FScale[Index, 1] := -FScale[Index, 1];
  end
end;

procedure TPartyDelayDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Band : Integer;
begin
 if abs(SampleRate) > 0 then
  begin
   for Band := 0 to Length(FDelayLine) - 1 do
    if Assigned(FDelayLine[Band])
     then FDelayLine[Band].Samplerate := SampleRate;

   for Band := 0 to Length(FFilter) - 1 do
    if Assigned(FFilter[Band])
     then FFilter[Band].Samplerate := SampleRate;

   for Band := 0 to Length(FFreqShift) - 1 do
    if Assigned(FFreqShift[Band])
     then FFreqShift[Band].Samplerate := SampleRate;
  end;
end;

procedure TPartyDelayDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(EditorForm) then
  with TFmPartyDelay(EditorForm) do
   begin
    StatusBar.SimpleText := string(ParameterProperties[Index].DisplayName +
      AnsiString(': ') + ParameterDisplay[Index] + AnsiChar(' ') +
      ParameterLabel[Index]);
   end;
end;

procedure TPartyDelayDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample   : Integer;
  Band     : Integer;
  Up, Down : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   for Band := 0 to Length(FLastOutput) - 1 do
    if FOnOff[Band] then
     begin
      // eventually filter last output
      FCriticalSection.Enter;
      try
       FLastOutput[Band] := FScale[Band, 0] * Inputs[0, Sample] +
         FScale[Band, 1] * Inputs[1, Sample] +
         FFeedback[Band] * FLastOutput[Band];

       if Assigned(FFilter[Band])
        then FLastOutput[Band] := FFilter[Band].ProcessSample64(FLastOutput[Band]);

       // eventually shift frequency
       if Assigned(FFreqShift[Band]) then
        begin
         FFreqShift[Band].ProcessSample(FLastOutput[Band], Up, Down);
         FLastOutput[Band] := FFSGain[Band, 0] * FLastOutput[Band] +
           FFSGain[Band, 1] * Up + FFSGain[Band, 2] * Down;
        end;

       // non-linear processing
       if FDrive[Band] > 0
        then FLastOutput[Band] := FastTanhContinousError4((1 + FDrive[Band]) * FLastOutput[Band]);

       FLastOutput[Band] := FDelayLine[Band].ProcessSample32(FLastOutput[Band]);
      finally
       FCriticalSection.Leave;
      end;
     end
    else FLastOutput[Band] := 0; 

   Outputs[0, Sample] := FDry * Inputs[0, Sample] +
     FOutMix[0, 0] * FLastOutput[0] +
     FOutMix[1, 0] * FLastOutput[1] +
     FOutMix[2, 0] * FLastOutput[2] +
     FOutMix[3, 0] * FLastOutput[3];
   Outputs[1, Sample] := FDry * Inputs[1, Sample] +
     FOutMix[0, 1] * FLastOutput[0] +
     FOutMix[1, 1] * FLastOutput[1] +
     FOutMix[2, 1] * FLastOutput[2] +
     FOutMix[3, 1] * FLastOutput[3];
  end;
end;

end.
