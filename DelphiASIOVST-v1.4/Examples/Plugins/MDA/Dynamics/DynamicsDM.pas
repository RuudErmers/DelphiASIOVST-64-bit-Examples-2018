unit DynamicsDM;

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
  Forms, DAV_Types, DAV_VSTModule;

type
  TDynamicsDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessCompressor(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateChangeRelease(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLimiterDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGateThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGateAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGateReleaseDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterLimiterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGateThresholdLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FThreshold         : Single;
    FRatio             : Single;
    FTrim              : Single;
    FAttack            : Single;
    FRelease           : Single;
    FLimiterThreshold  : Single;
    FExpanderThreshold : Single;
    FGateRelease       : Single;
    FIntRelease        : Single;
    FGateAttack        : Single;
    FDry               : Single;
    FEnv               : array [0..2] of Single;
    procedure CheckProcessChanged;
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

procedure TDynamicsDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := -20;  // Threshold [dB]
 Parameter[1] := 0.40; // Ratio
 Parameter[2] := 4;    // Level [dB]
 Parameter[3] := 0.18; // Attack
 Parameter[4] := 0.55; // Release
 Parameter[5] := 0;    // Limiter [dB]
 Parameter[6] := -61;  // Gate Threshold [dB]
 Parameter[7] := 0.10; // Gate Attack
 Parameter[8] := 0.50; // Gate Decay
 Parameter[9] := 1.00; // FX Mix
end;

procedure TDynamicsDataModule.ParameterGateChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FGateAttack := Power(10, -0.002 - 3 * Value);
end;

procedure TDynamicsDataModule.ParameterGateChangeRelease(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FGateRelease := 1 - Power(10, -2 - 3.3 * Value);
end;

procedure TDynamicsDataModule.ParameterRatioDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.58 then
  if Parameter[Index] < 0.62
   then PreDefined := 'Limit'
   else PreDefined := FloatToStrF(-FRatio, ffGeneral, 2, 2)
 else
  if(Parameter[Index] < 0.2)
   then PreDefined := FloatToStrF(0.5 + 2.5 * Parameter[Index], ffGeneral, 2, 2)
   else PreDefined := FloatToStrF(1 / (1 - FRatio), ffGeneral, 2, 2);
end;

procedure TDynamicsDataModule.ParameterAttackDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(-301030.1 / (SampleRate * log10(1 - FAttack))));
end;

procedure TDynamicsDataModule.ParameterReleaseDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(-301.0301 / (SampleRate * log10(1 - FRelease))));
end;

procedure TDynamicsDataModule.ParameterLimiterDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if FLimiterThreshold = 0 then PreDefined := 'OFF';
end;

procedure TDynamicsDataModule.ParameterLimiterLabel(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if FLimiterThreshold = 0 then PreDefined := '';
end;

procedure TDynamicsDataModule.ParameterGateThresholdChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if (Value < -60)
  then FExpanderThreshold := 0 // Expander
  else FExpanderThreshold := dB_to_Amp(Value);
end;

procedure TDynamicsDataModule.ParameterGateThresholdDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if FExpanderThreshold = 0 then PreDefined := 'OFF';
end;

procedure TDynamicsDataModule.ParameterGateThresholdLabel(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if FExpanderThreshold = 0 then PreDefined := '';
end;

procedure TDynamicsDataModule.ParameterGateAttackDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(-301030.1 / (SampleRate * log10(1 - FGateAttack))));
end;

procedure TDynamicsDataModule.ParameterGateReleaseDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(-1806 / (SampleRate * log10(FGateRelease))));
end;

procedure TDynamicsDataModule.ParameterMixDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(100 * Parameter[9]));
end;

procedure TDynamicsDataModule.ParameterLimiterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Value >= 0
  then FLimiterThreshold := 0                    // Limiter
  else FLimiterThreshold  := dB_to_Amp(Value);
 CheckProcessChanged;
end;

procedure TDynamicsDataModule.ParameterAttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FAttack  := Power(10, -0.002 - 2 * Value);
end;

procedure TDynamicsDataModule.ParameterReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FRelease := Power(10, -2 - 3 * Value);
end;

procedure TDynamicsDataModule.CheckProcessChanged;
begin
 if (FRatio > 1) or (Parameter[5] < 0) or (Parameter[6] > -60)
  then OnProcess := VSTModuleProcess
  else OnProcess := VSTModuleProcessCompressor;
 OnProcessReplacing := OnProcess;
end;

procedure TDynamicsDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Parameter[0]);
 FRatio     := 2.5 * Parameter[1] - 0.5;
 if (FRatio > 1) then FRatio := 1 + 16 * sqr(FRatio - 1) else
 if (FRatio < 0) then FRatio := 0.6 * FRatio;

 FTrim   := dB_to_Amp(Parameter[2]);

 FIntRelease := Power(10, -2 / SampleRate);

 if (FRatio < 0) and (FThreshold < 0.1)
  then FRatio := FRatio * FThreshold * 15;

 FDry   := 1 - Parameter[9];
 FTrim := FTrim * Parameter[9];                // FX Mix
 CheckProcessChanged;
end;

procedure TDynamicsDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  env    : array [0..2] of Single;
  i, g, ra, re, at, ga : Single;
  tr, th, lth, xth, gr, y : Single;
begin
 env[0] := FEnv[0];
 env[1] := FEnv[1];
 env[2] := FEnv[2];
 ra     := FRatio;
 re     := (1 - FRelease);
 at     := FAttack;
 ga     := FGateAttack;
 gr     := (1 - FGateRelease);
 tr     := FTrim;
 th     := FThreshold;
 lth    := FLimiterThreshold;
 xth    := FExpanderThreshold;
 y      := FDry;

 if lth = 0 then lth := 1000;
 for Sample := 0 to SampleFrames - 1 do
  begin
   i := max(abs(Inputs[0, Sample]), abs(Inputs[1, Sample]));

   if (i > env[0])
    then env[0] := env[0] + at * (i - env[0])
    else env[0] := env[0] * re;
   if (i > env[0])
    then env[1] := i
    else env[1] := env[1] * re;

   if env[0] > th
    then g := tr / (1 + ra * ((env[0] / th) - 1))
    else g := tr;

   if g < 0 then g := 0;
   if g * env[1] > lth then g := lth / env[1];      // Limit

   if env[0] > xth
    then env[2] := env[2] + ga - ga * env[2]
    else env[2] := gr + env[2] * gr;                // Gate

   Outputs[0, Sample] := Inputs[0, Sample] * (g * env[2] + y);
   Outputs[1, Sample] := Inputs[1, Sample] * (g * env[2] + y);
  end;

 if (env[0] < 1E-10) then FEnv[0] := 0 else FEnv[0] := env[0];
 if (env[1] < 1E-10) then FEnv[1] := 0 else FEnv[1] := env[1];
 if (env[2] < 1E-10) then FEnv[2] := 0 else FEnv[2] := env[2];
end;

procedure TDynamicsDataModule.VSTModuleProcessCompressor(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  env    : Single;
  tr, th, y, i, g, ra, re, at : Single;
begin
 env := FEnv[0];
 ra  := FRatio;
 re  := (1 - FRelease);
 at  := FAttack;
 tr  := FTrim;
 th  := FThreshold;
 y   := FDry;

 for Sample := 0 to SampleFrames - 1 do
  begin
   i := max(abs(Inputs[0, Sample]), abs(Inputs[1, Sample])); // Get peak level

   if i > env
    then env := env + at * (i - env)
    else env := env * re;                                        // Envelope
   if env > th
    then g := tr / (1 + ra * ((env / th) - 1))
    else g := tr;                                            // Gain

   Outputs[0, Sample] := Inputs[0, Sample] * (g + y);        // VCA
   Outputs[1, Sample] := Inputs[1, Sample] * (g + y);
  end;

 if (env  < 1E-10) then FEnv[0] := 0 else FEnv[0] := env;
end;

end.
