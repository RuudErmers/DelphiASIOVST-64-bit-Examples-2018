unit DAV_DspTransientProcessor;

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
//  The code is based on the mda VST plug-ins by Paul Kellett, which is       //
//  located at http://sourceforge.net/projects/mda-vst/                       //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes;

type
  TCustomTransientProcessor = class(TDspSampleRatePersistent)
  private
    FAttackHold  : Single;
    FOutputGain  : Single;
    FFilter      : Single;
    FReleaseHold : Single;
    FRelease     : Single;
    FAttack      : Single;
    procedure SetAttack(const Value: Single);
    procedure SetAttackHold(const Value: Single);
    procedure SetFilter(const Value: Single);
    procedure SetOutput(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetReleaseHold(const Value: Single);
    procedure CalculateAttackFactors;
    procedure CalculateAttackHoldFactor;
    procedure CalculateReleaseHoldFactor;
    procedure CalculateReleaseFactors;
  protected
    FReleaseHoldFactor : Single;
    FAttackHoldFactor  : Single;
    FOutputLevelFactor : Single;
    FAttackFactors     : Array [0..1] of Single;
    FReleaseFactors    : Array [0..1] of Single;
    FEnv               : Array [0..3] of Single;
    FFilterIn          : Single;
    FFilterOut         : Single;
    FFilterState       : Single;
    procedure SampleRateChanged; override;
    procedure AttackChanged; virtual; 
    procedure AttackHoldChanged; virtual;
    procedure FilterChanged; virtual;
    procedure OutputChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure ReleaseHoldChanged; virtual;
  public
    constructor Create; override;

    property Attack: Single read FAttack write SetAttack;
    property AttackHold: Single read FAttackHold write SetAttackHold;
    property Filter: Single read FFilter write SetFilter;
    property Output: Single read FOutputGain write SetOutput;
    property Release: Single read FRelease write SetRelease;
    property ReleaseHold: Single read FReleaseHold write SetReleaseHold;
  end;

  TCustomMonoTransientProcessor = class(TCustomTransientProcessor, IDspProcessor32)
  protected
    FState : Single;
  public
    constructor Create; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
  end;

  TCustomStereoTransientProcessor = class(TCustomTransientProcessor)
  protected
    FState : Array [0..1] of Single;
  public
    constructor Create; override;
    procedure ProcessSample(const InLeft, InRight: Single; out OutLeft, OutRight: Single);
    procedure ProcessStereo(var Left, Right: Single);
  end;

  TMonoTransientProcessor = class(TCustomMonoTransientProcessor)
  published
    property Attack;
    property AttackHold;
    property Filter;
    property Output;
    property Release;
    property ReleaseHold;
    property SampleRate;
  end;

  TStereoTransientProcessor = class(TCustomStereoTransientProcessor)
  published
    property Attack;
    property AttackHold;
    property Filter;
    property Output;
    property Release;
    property ReleaseHold;
    property SampleRate;
  end;

implementation

uses
  Math, DAV_Common;

const
  CSampleRateScaling : Single = 2.2675736961451247165532879818594E-5;

{ TCustomTransientProcessor }

constructor TCustomTransientProcessor.Create;
begin
 inherited;

 FAttack      := 0;     // Attack [%]
 FRelease     := 0;     // Release [%]
 FOutputGain  := 0;     // Output Gain [dB]
 FFilter      := -1;    // Filter [%]
 FAttackHold  := 35;    // Att-rel [%]
 FReleaseHold := 35;    // Rel-att [%]

 CalculateAttackFactors;
 CalculateAttackHoldFactor;
 FilterChanged;
 OutputChanged;
 CalculateReleaseFactors;
 CalculateReleaseHoldFactor;
end;

procedure TCustomTransientProcessor.SetAttack(const Value: Single);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomTransientProcessor.SetAttackHold(const Value: Single);
begin
 if FAttackHold <> Value then
  begin
   FAttackHold := Value;
   AttackHoldChanged;
  end;
end;

procedure TCustomTransientProcessor.SetFilter(const Value: Single);
begin
 if FFilter <> Value then
  begin
   FFilter := Value;
   FilterChanged;
  end;
end;

procedure TCustomTransientProcessor.SetOutput(const Value: Single);
begin
 if FOutputGain <> Value then
  begin
   FOutputGain := Value;
   OutputChanged;
  end;
end;

procedure TCustomTransientProcessor.SetRelease(const Value: Single);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomTransientProcessor.SetReleaseHold(const Value: Single);
begin
 if FReleaseHold <> Value then
  begin
   FReleaseHold := Value;
   ReleaseHoldChanged;
  end;
end;

procedure TCustomTransientProcessor.SampleRateChanged;
begin
 CalculateAttackFactors;
 CalculateAttackHoldFactor;
 CalculateReleaseFactors;
 CalculateReleaseHoldFactor;
 Changed;
end;

procedure TCustomTransientProcessor.AttackChanged;
begin
 CalculateAttackFactors;
 Changed;
end;

procedure TCustomTransientProcessor.CalculateAttackFactors;
begin
 if FAttack > 0 then
  begin
   FAttackFactors[0] := Power(10, -1.5 * CSampleRateScaling * SampleRate);
   FAttackFactors[1] := Power(10, 1 - 0.025 * (FAttack + 100) * CSampleRateScaling * SampleRate);
  end
 else
  begin
   FAttackFactors[0] := Power(10, -4 + 0.025 * (FAttack + 100) * CSampleRateScaling * SampleRate);
   FAttackFactors[1] := Power(10, -1.5 * CSampleRateScaling * SampleRate);
  end;
end;

procedure TCustomTransientProcessor.AttackHoldChanged;
begin
 CalculateAttackHoldFactor;
 Changed;
end;

procedure TCustomTransientProcessor.CalculateAttackHoldFactor;
begin
 FAttackHoldFactor := 1 - Power(10, -2 - 0.04 * FAttackHold * CSampleRateScaling * SampleRate);
end;

procedure TCustomTransientProcessor.FilterChanged;
begin
 if FFilter > 0 then
  begin
   FFilterIn := 0.8 - 0.8 * (0.01 * FFilter + 1);
   FFilterOut := 1 + FFilterIn;
   FFilterState := 1;
  end
 else
  begin
   FFilterIn := 0.1 + 0.9 * (0.01 * FFilter + 1);
   FFilterOut := 1 - FFilterIn;
   FFilterState := 0;
  end;
 Changed;
end;

procedure TCustomTransientProcessor.OutputChanged;
begin
 FOutputLevelFactor := dB_to_Amp(FOutputGain);
 Changed;
end;

procedure TCustomTransientProcessor.ReleaseChanged;
begin
 CalculateReleaseHoldFactor;
 Changed;
end;

procedure TCustomTransientProcessor.CalculateReleaseFactors;
begin
 if FRelease > 0 then
  begin
   FReleaseFactors[0] := 1 - Power(10, -4.5 * CSampleRateScaling * SampleRate);
   FReleaseFactors[1] := 1 - Power(10, -5.85 + 0.0135 * (FRelease + 100) * CSampleRateScaling * SampleRate);
  end
 else
  begin
   FReleaseFactors[0] := 1 - Power(10, -3.15 - 0.0135 * (FRelease + 100) * CSampleRateScaling * SampleRate);
   FReleaseFactors[1] := 1 - Power(10, -4.5 * CSampleRateScaling * SampleRate);
  end;
end;

procedure TCustomTransientProcessor.ReleaseHoldChanged;
begin
 CalculateReleaseHoldFactor;
 Changed;
end;

procedure TCustomTransientProcessor.CalculateReleaseHoldFactor;
begin
 FReleaseHoldFactor := Power(10, - 0.04 * FReleaseHold * CSampleRateScaling * SampleRate);
end;


{ TCustomMonoTransientProcessor }

constructor TCustomMonoTransientProcessor.Create;
begin
 inherited;
 FState := 0;
end;

procedure TCustomMonoTransientProcessor.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TCustomMonoTransientProcessor.ProcessSample32(Input: Single): Single;
var
  Gain   : Single;
begin
 FState := FFilterOut * FState + FFilterIn * Input;
 Result := FState + FFilterState * Input;
 Gain   := abs(Input);

 if Gain > FEnv[0] then FEnv[0] := FEnv[0] + FAttackFactors[0]  * (Gain - FEnv[0]) else FEnv[0] := FEnv[0] * FAttackHoldFactor;
 if Gain > FEnv[1] then FEnv[1] := FEnv[1] + FAttackFactors[1]  * (Gain - FEnv[1]) else FEnv[1] := FEnv[1] * FAttackHoldFactor;
 if Gain > FEnv[2] then FEnv[2] := FEnv[2] + FReleaseHoldFactor * (Gain - FEnv[2]) else FEnv[2] := FEnv[2] * FReleaseFactors[0];
 if Gain > FEnv[3] then FEnv[3] := FEnv[3] + FReleaseHoldFactor * (Gain - FEnv[3]) else FEnv[3] := FEnv[3] * FReleaseFactors[1];
 Gain := (FEnv[0] - FEnv[1] + FEnv[2] - FEnv[3]);

 Result := FOutputLevelFactor * (Input + Result * Gain);
end;


{ TCustomStereoTransientProcessor }

constructor TCustomStereoTransientProcessor.Create;
begin
 inherited;
 FState[0] := 0;
 FState[1] := 0;
end;

procedure TCustomStereoTransientProcessor.ProcessSample(const InLeft, InRight: Single;
  out OutLeft, OutRight: Single);
var
  Gain, Mono  : Single;
begin
 FState[0] := FFilterOut * FState[0] + FFilterIn * InLeft;
 FState[1] := FFilterOut * FState[1] + FFilterIn * InRight;
 OutLeft   := FState[0] + FFilterState * InLeft;
 OutRight  := FState[1] + FFilterState * InRight;
 Mono := abs(InLeft + InRight);

 if Mono > FEnv[0] then FEnv[0] := FEnv[0] + FAttackFactors[0]  * (Mono - FEnv[0]) else FEnv[0] := FEnv[0] * FAttackHoldFactor;
 if Mono > FEnv[1] then FEnv[1] := FEnv[1] + FAttackFactors[1]  * (Mono - FEnv[1]) else FEnv[1] := FEnv[1] * FAttackHoldFactor;
 if Mono > FEnv[2] then FEnv[2] := FEnv[2] + FReleaseHoldFactor * (Mono - FEnv[2]) else FEnv[2] := FEnv[2] * FReleaseFactors[0];
 if Mono > FEnv[3] then FEnv[3] := FEnv[3] + FReleaseHoldFactor * (Mono - FEnv[3]) else FEnv[3] := FEnv[3] * FReleaseFactors[1];
 Gain := (FEnv[0] - FEnv[1] + FEnv[2] - FEnv[3]);

 OutLeft := FOutputLevelFactor * (InLeft + OutLeft * Gain);
 OutRight := FOutputLevelFactor * (InRight + OutRight * Gain);
end;

procedure TCustomStereoTransientProcessor.ProcessStereo(var Left, Right: Single);
begin
 ProcessSample(Left, Right, Left, Right);
end;

initialization
  RegisterDspProcessor32(TMonoTransientProcessor);

end.
