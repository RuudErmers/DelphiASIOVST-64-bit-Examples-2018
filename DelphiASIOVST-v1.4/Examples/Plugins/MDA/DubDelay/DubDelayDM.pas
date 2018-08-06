unit DubDelayDM;

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
  TDubDelayDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimiterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FEnv          : Single;
    FEnv2         : Single;
    FRatio        : Single;
    FAttack       : Single;
    FRelease      : Single;
    FIRelease     : Single;
    FGainAtt      : Single;
    FTrim         : Single;
    FThreshold    : Single;
    FLowThreshold : Single;
    FXThreshold   : Single;
    FXRate        : Single;
    FGEnv         : Single;
    FDry          : Single;
    FMode         : Integer;
    procedure RatioChanged;
    procedure TrimChanged;
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

procedure TDubDelayDataModule.ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FRelease := Power(10, -2 - 3 * Value);
end;

procedure TDubDelayDataModule.ParameterLimiterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Value > 0.98)
  then FLowThreshold := 0  // Limiter
  else
   begin
    FLowThreshold := 0.99 * Power(10, Round(30 * Value - 20) / 20);
    FMode := 1;
   end;
end;

procedure TDubDelayDataModule.ParameterGateThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Value < 0.02)
  then FXThreshold := 0    // Expander
  else
   begin
    FXThreshold  := Power (10, 3 * Value - 3);
    FMode := 1;
   end;
end;

procedure TDubDelayDataModule.ParameterGateReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FXRate    := 1 - Power(10, -2 - 3.3 * Value);
end;

procedure TDubDelayDataModule.ParameterGateAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGainAtt  := Power(10, -0.002 - 3 * Value);
end;

procedure TDubDelayDataModule.ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FAttack := Power(10, -0.002 - 2 * Value);
end;

procedure TDubDelayDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDry  := 1 - Value;
 TrimChanged;
end;

procedure TDubDelayDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 TrimChanged;
end;

procedure TDubDelayDataModule.ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FRatio := 2.5 * Value - 0.5;
 RatioChanged;
end;

procedure TDubDelayDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Value);
 RatioChanged;
end;

procedure TDubDelayDataModule.TrimChanged;
begin
 FTrim := dB_to_Amp(Parameter[2]) * 0.01 * Parameter[9]; //fx mix
end;

procedure TDubDelayDataModule.RatioChanged;
begin
 if (FRatio < 0) and (FThreshold < 0.1)
  then FRatio := FRatio * FThreshold * 15;

 if (FRatio > 1) then
  begin
   FRatio := 1 + 16 * sqr(FRatio - 1);
   FMode := 1;
  end;
 if (FRatio < 0) then
  begin
   FRatio := 0.6 * FRatio;
   FMode := 1;
  end;
end;

procedure TDubDelayDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := Amp_to_dB(-16); // Thresh
 Parameter[2] := Amp_to_dB(  4); // Level
 Parameter[9] := 100;            // FX Mix

(*
 Parameter[1] := 0.40;   // Ratio
 Parameter[3] := 0.18;   // Attack
 Parameter[4] := 0.55;   // Release
 Parameter[5] := 1.00;   // Limiter
 Parameter[6] := 0.00;   // Gate Thresh
 Parameter[7] := 0.10;   // Gate Attack
 Parameter[8] := 0.50;   // Gate Decay
*)
end;

procedure TDubDelayDataModule.VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMode := 0;
 FIRelease := Power(10, -2 / SampleRate);
end;

procedure TDubDelayDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample                         : Integer;
  i, j, g, e, e2, ra, re, at, ga : Single;
  tr, th, lth, xth, ge           : Single;
begin
  e   := FEnv;
  e2  := FEnv2;
  ra  := FRatio;
  re  := 1 - FRelease;
  at  := FAttack;
  ga  := FGainAtt;
  tr  := FTrim;
  th  := FThreshold;
  lth := FLowThreshold;
  xth := FXThreshold;
  ge  := FGEnv;

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
  end;

 if (FMode > 0)  then //comp/gate/lim
  begin
   if (lth = 0) then lth := 1000;
    for Sample := 0 to SampleFrames - 1 do
     begin
      i := abs(Inputs[0, Sample]);
      j := abs(Inputs[1, Sample]);
      if (j > i)
       then i := j;

      if i > e
       then e := e + at * (i - e)
       else e := e * re;
      if i > e
       then e2 := i
       else e2 := e2 * re; //ir;

      if (e > th)
       then g := tr / (1 + ra * ((e / th) - 1))
       else g := tr;

      if (g < 0) then g := 0;
      if (g * e2 > lth)
       then g := lth / e2; //limit

      if (e > xth)
       then ge := ge + ga - ga * ge
       else ge := ge * FXRate; //gate

      Outputs[0, Sample] := Inputs[0, Sample] * (g * ge + FDry);
      Outputs[1, Sample] := Inputs[1, Sample] * (g * ge + FDry);
     end;
  end
 else //compressor only
  begin
   for Sample := 0 to SampleFrames - 1 do
    begin
     i := abs(Inputs[0, Sample]);
     j := abs(Inputs[1, Sample]);
     if j > i
      then i := j; //get peak level

     if i > e
      then e := e + at * (i - e)
      else e := e * re;           // Envelope
     if e > th
      then g := tr / (1 + ra * ((e / th) - 1))
      else g := tr;                            // Gain

     Outputs[0, Sample] := Inputs[0, Sample] * (g + FDry); //vca
     Outputs[1, Sample] := Inputs[1, Sample] * (g + FDry);
    end;
  end;

  if (e  < 1E-10) then FEnv  := 0 else FEnv  := e;
  if (e2 < 1E-10) then FEnv2 := 0 else FEnv2 := e2;
  if (ge < 1E-10) then FGEnv := 0 else FGEnv := ge;
end;

end.
