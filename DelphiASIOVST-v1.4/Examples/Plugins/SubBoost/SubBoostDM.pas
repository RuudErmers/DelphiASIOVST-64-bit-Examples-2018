unit SubBoostDM;

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
  Windows, Forms, Messages, SysUtils, Classes, DAV_Types, DAV_VSTModule,
  DAV_DspFilterButterworth;

type
  TProcessType = (ptDistort, ptDivide, ptInvert, ptKeyOsc);

  TSubBoostDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);

    procedure ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FInputFilter  : TButterworthLowPassFilter;
    FOutputFilter : TButterworthLowPassFilter;

    FPhi          : Single;
    FEnv          : Single;
    FDivide       : Single;
    FPhase        : Single;
    FOsc          : Single;
    FType         : TProcessType;
    FWet          : Single;
    FDry          : Single;
    FThreshold    : Single;
    FRelease      : Single;
    FDeltaPhi     : Single;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common,
  DAV_VSTCustomModule, SubBoostGUI;

procedure TSubBoostDataModule.VSTModuleOpen(Sender: TObject);
begin
 FInputFilter  := TButterworthLowPassFilter.Create;
 FOutputFilter := TButterworthLowPassFilter.Create;
 FOutputFilter.Order := 1;

 // initialize parameters
 Parameter[0] :=  0;    // Type
 Parameter[1] := 30;    // Level
 Parameter[2] := 0.6;   // Tune
 Parameter[3] := 10;    // Dry Mix
 Parameter[4] := -24;   // Threshold
 Parameter[5] := 0.65;  // Release

 VSTModuleResume(Sender);

 // set editor form class
 EditorFormClass := TFmSubBoost;
end;

procedure TSubBoostDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FInputFilter);
 FreeAndNil(FOutputFilter);
end;

procedure TSubBoostDataModule.ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := 0.01 * Value;
end;

procedure TSubBoostDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Parameter[4]);
end;

procedure TSubBoostDataModule.ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
// PreDefined := IntToStr(Round(0.0726 * SampleRate * Power(10, -2.5 + (1.5 * Parameter[index]))));
 PreDefined := AnsiString(FloatToStrF(FInputFilter.Frequency, ffGeneral, 3, 3));
end;

procedure TSubBoostDataModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FInputFilter)
  then FInputFilter.Order := Round(Value);
end;

procedure TSubBoostDataModule.ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FInputFilter) then FInputFilter.Frequency := Value;
 if Assigned(FOutputFilter) then FOutputFilter.Frequency := Value;

 // update GUI if necessary
 if Assigned(EditorForm) then
  with TFmSubBoost(EditorForm) do
   begin
    UpdateTune;
   end;
end;

procedure TSubBoostDataModule.ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(-301.03 / (SampleRate * Log10(FRelease)))));
end;

procedure TSubBoostDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FRelease := 1 - Power(10, -2 - (3 * Parameter[5]));
end;

procedure TSubBoostDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0: PreDefined := 'Distort';
  1: PreDefined := 'Divide';
  2: PreDefined := 'Invert';
  3: PreDefined := 'Key Osc.';
 end;
end;

procedure TSubBoostDataModule.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDry := 0.01 * Value;
end;

procedure TSubBoostDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FDivide := 1;
  FPhase  := 1;
  FOsc    := 0; // Oscillator phase
  FType   := TProcessType(Round(Parameter[0]));
(*
  if FType = ptKeyOsc
   then fFilterIn := 0.018
   else fFilterIn := Power(10, -3 + (2 * Parameter[2]));
  fFilterOut := 1 - fFilterIn;
*)
  FDeltaPhi  := 0.456159 * Power(10, -2.5 + (1.5 * Parameter[2]));
end;

procedure TSubBoostDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  FilteredIn   : Single;
  SubBass      : Single;
  rl, th, dv   : Single;
  ph, phii     : Single;
  dph, os, en  : Single;

  Sample       : Integer;

const
  CDenormalThreshold: Single = 1E-16;  
begin
 dph  := FDeltaPhi;
 rl   := FRelease;
 phii := FPhi;
 en   := FEnv;
 os   := FOsc;
 th   := FThreshold;
 dv   := FDivide;
 ph   := FPhase;

 for Sample := 0 to SampleFrames - 1 do
  begin
   // Input Filter
   FilteredIn := FInputFilter.ProcessSample64(Inputs[0, Sample] + Inputs[1, Sample]);


   if FilteredIn * dv < 0 then     // Octave Divider
    begin
     dv := -dv;
     if dv < 0 then ph := -ph;
    end;

   case FType of
    ptDistort : if FilteredIn > th then SubBass := 1 else
                 if FilteredIn < -th
                  then SubBass := -1
                  else SubBass := 0;
    ptDivide  : if FilteredIn > th then SubBass := ph else
                 if FilteredIn < -th
                  then SubBass := ph
                  else SubBass := 0;
    ptInvert  : SubBass := ph * FilteredIn * 2;  // Invert
    ptKeyOsc  : begin                    // Osc
                 if (FilteredIn > th)
                  then en := 1
                  else en := en * rl;
                 SubBass  := (en * sin(phii));
                 phii     := FastMod(phii + dph, 2 * Pi);
                end;
    else SubBass := 0;            
   end;

   // Output Filter
   SubBass := FOutputFilter.ProcessSample64(SubBass);

   // Output
   Outputs[0, Sample] := Inputs[0, Sample] * FDry + SubBass * FWet;
   Outputs[1, Sample] := Inputs[1, Sample] * FDry + SubBass * FWet;
  end;

 FDivide := dv;
 FPhase  := ph;
 FOsc    := os;
 FPhi    := phii;
 FEnv    := en;
end;

procedure TSubBoostDataModule.VSTModuleResume(Sender: TObject);
begin
 FPhi     := 0;
 FEnv     := 0;
end;

procedure TSubBoostDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   if Assigned(FInputFilter) then FInputFilter.SampleRate := Abs(SampleRate);
   if Assigned(FOutputFilter) then FOutputFilter.SampleRate := Abs(SampleRate);
  end;
end;

end.
