unit SubSynthDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTModule;

type
  TProcessType = (ptDistort, ptDivide, ptInvert, ptKeyOsc);

  TSubSynthDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FFilterState : array [0..3] of Single;
    FFilterIn    : Single;
    FFilterOut   : Single;

    FPhi         : Single;
    FEnv         : Single;
    FDivide      : Single;
    FPhase       : Single;
    FOsc         : Single;
    FType        : TProcessType;
    FWet         : Single;
    FDry         : Single;
    FThreshold   : Single;
    FRelease     : Single;
    FDeltaPhi    : Single;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math;

procedure TSubSynthDataModule.ParameterLevelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := 0.01 * Value;
end;

procedure TSubSynthDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Parameter[4]);
end;

procedure TSubSynthDataModule.ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(0.0726 * SampleRate * Power(10, -2.5 + (1.5 * Parameter[index]))));
end;

procedure TSubSynthDataModule.ParameterReleaseDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(-301.03 / (SampleRate * log10(FRelease))));
end;

procedure TSubSynthDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FRelease := 1 - Power(10, -2 - (3 * Parameter[5]));
end;

procedure TSubSynthDataModule.ParameterModeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of 
  0: PreDefined := 'Distort';
  1: PreDefined := 'Divide';
  2: PreDefined := 'Invert';
  3: PreDefined := 'Key Osc.';
 end;
end;

procedure TSubSynthDataModule.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDry := 0.01 * Value;
end;

procedure TSubSynthDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] :=  0;    // Type
 Parameter[1] := 30;    // Level
 Parameter[2] := 0.6;   // Tune
 Parameter[3] := 10;    // Dry Mix
 Parameter[4] := -24;   // Threshold
 Parameter[5] := 0.65;  // Release

 VSTModuleResume(Sender);
end;

procedure TSubSynthDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  FDivide := 1;
  FPhase  := 1;
  FOsc    := 0; // Oscillator phase
  FType   := TProcessType(Round(Parameter[0]));
  if FType = ptKeyOsc
   then FFilterIn := 0.018
   else FFilterIn := Power(10, -3 + (2 * Parameter[2]));
  FFilterOut := 1 - FFilterIn;
  FDeltaPhi  := 0.456159 * Power(10, -2.5 + (1.5 * Parameter[2]));
end;

procedure TSubSynthDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  fi, fo       : Single;
  f1, f2       : Single;
  f3, f4, sub  : Single;
  rl, th, dv   : Single;
  ph, phii     : Single;
  dph, os, en  : Single;

  Sample       : Integer;
begin
 dph  := FDeltaPhi;
 rl   := FRelease;
 phii := FPhi;
 en   := FEnv;
 os   := FOsc;
 th   := FThreshold;
 dv   := FDivide;
 ph   := FPhase;
 f1   := FFilterState[0];
 f2   := FFilterState[1];
 f3   := FFilterState[2];
 f4   := FFilterState[3];

 fi   := FFilterIn;
 fo   := FFilterOut;

 for Sample := 0 to SampleFrames - 1 do
  begin
   f1 := fo * f1 + fi * (Inputs[0, Sample] + Inputs[1, Sample]);
   f2 := fo * f2 + fi * f1;

   sub := f2;
   if sub > th then sub := 1 else
    if sub < -th then sub := -1 else sub := 0;

   if sub * dv < 0 then     // Octave Divider
    begin
     dv := -dv;
     if dv < 0 then ph := -ph;
    end;

   case FType of
    ptDivide : sub := ph * sub;     // Divide
    ptInvert : sub := ph * f2 * 2;  // Invert
    ptKeyOsc : begin                // Osc
                if (f2 > th)
                 then en := 1
                 else en := en * rl;
                sub  := (en * sin(phii));
                phii := f_mod(phii + dph, 6.283185);
               end;
   end;

   f3 := (fo * f3) + (fi * sub);
   f4 := (fo * f4) + (fi * f3);

  Outputs[0, Sample] := Inputs[0, Sample] * FDry + f4 * FWet; // output
  Outputs[1, Sample] := Inputs[1, Sample] * FDry + f4 * FWet;
  end;

 if (abs(f1) < 1E-10) then FFilterState[0] := 0 else FFilterState[0] := f1;
 if (abs(f2) < 1E-10) then FFilterState[1] := 0 else FFilterState[1] := f2;
 if (abs(f3) < 1E-10) then FFilterState[2] := 0 else FFilterState[2] := f3;
 if (abs(f4) < 1E-10) then FFilterState[3] := 0 else FFilterState[3] := f4;

 FDivide := dv;
 FPhase  := ph;
 FOsc    := os;
 FPhi    := phii;
 FEnv    := en;
end;

procedure TSubSynthDataModule.VSTModuleResume(Sender: TObject);
begin
 FPhi     := 0;
 FEnv     := 0;
 FFilterState[0] := 0;
 FFilterState[1] := 0;
 FFilterState[2] := 0;
 FFilterState[3] := 0;
 FFilterIn   := 0;
 FFilterOut   := 0;
end;

end.
