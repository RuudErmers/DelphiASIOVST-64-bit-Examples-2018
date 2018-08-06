unit DAV_DspFilterAllpasses;

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

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Classes, DAV_DspFilter;

type
  TCustomThiranAllpass = class(TCustomFilter)
  private
    FFrequency   : Single;
    FPhaseDelay  : Single;
    procedure SetFrequency(const Value: Single);
    procedure SetPhaseDelay(const Value: Single);
  protected  
    procedure FrequencyChanged; virtual;
    procedure PhaseDelayChanged; virtual;
    procedure CalculateCoefficients; virtual; abstract;
  public
    property Frequency: Single read FFrequency write SetFrequency;
    property PhaseDelay: Single read FPhaseDelay write SetPhaseDelay;
  end;

  TThiranAllpass1stOrder = class(TCustomThiranAllpass)
  private
    FCoefficient : Single;
    FLastOutput  : Single;
    FLastInput   : Single;
  protected  
    procedure CalculateCoefficients; override;
  public
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
  published                
    property Frequency;
    property PhaseDelay;
  end;

  TThiranAllpass2ndOrder = class(TCustomThiranAllpass)
  private
    FCoefficient : array [0..1] of Single;
    FState       : array [0..1, 0..1] of Single;
  protected
    procedure CalculateCoefficients; override;
  public
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;

    function MagnitudeLog10(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;

    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override;
  published
    property Frequency;
    property PhaseDelay;
  end;

implementation

uses
  Math, SysUtils, DAV_Complex;

{ TCustomThiranAllpass }

procedure TCustomThiranAllpass.FrequencyChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomThiranAllpass.PhaseDelayChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomThiranAllpass.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomThiranAllpass.SetPhaseDelay(const Value: Single);
begin
 if FPhaseDelay <> Value then
  begin
   FPhaseDelay := Value;
   PhaseDelayChanged;
  end;
end;


{ TThiranAllpass1stOrder }

procedure TThiranAllpass1stOrder.CalculateCoefficients;
var
  NormalizedFrequency : Single;
begin
 NormalizedFrequency := 0.5 * FFrequency / SampleRate;
 FCoefficient := Sin((1 - FPhaseDelay) * NormalizedFrequency) /
   Sin((1 + FPhaseDelay) * NormalizedFrequency);
end;

function TThiranAllpass1stOrder.ProcessSample32(Input: Single): Single;
begin
 Result := FCoefficient * (Input - FLastOutput) + FLastInput;
 FLastInput := Input;
 FLastOutput := Result;
end;

function TThiranAllpass1stOrder.ProcessSample64(Input: Double): Double;
begin
 Result := FCoefficient * (Input - FLastOutput) + FLastInput;
 FLastInput := Input;
 FLastOutput := Result;
end;


{ TThiranAllpass2ndOrder }

procedure TThiranAllpass2ndOrder.CalculateCoefficients;
begin
 FCoefficient[0] := (FPhaseDelay - 2) / (FPhaseDelay + 1);
 FCoefficient[0] := (FPhaseDelay - 2) * (FPhaseDelay - 1) /
   ((FPhaseDelay + 1) * (FPhaseDelay + 2));
end;

function TThiranAllpass2ndOrder.ProcessSample32(Input: Single): Single;
begin
 Result := FCoefficient[1] * Input + FCoefficient[0] * FState[0, 0] +
   FState[0, 1] - FCoefficient[0] * FState[1, 0] -
   FCoefficient[1] * FState[1, 1];

 FState[0, 1] := FState[0, 0];
 FState[0, 0] := Input;
 FState[1, 1] := FState[1, 0];
 FState[1, 0] := Result;
end;

function TThiranAllpass2ndOrder.ProcessSample64(Input: Double): Double;
begin
 Result := FCoefficient[1] * Input + FCoefficient[0] * FState[0, 0] +
   FState[0, 1] - FCoefficient[0] * FState[1, 0] -
   FCoefficient[1] * FState[1, 1];

 FState[0, 1] := FState[0, 0];
 FState[0, 0] := Input;
 FState[1, 1] := FState[1, 0];
 FState[1, 0] := Result;
end;

function TThiranAllpass2ndOrder.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Result, Temp);
end;

function TThiranAllpass2ndOrder.Imaginary(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Temp, Result);
end;

function TThiranAllpass2ndOrder.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := Log10(MagnitudeSquared(Frequency));
end;

procedure TThiranAllpass2ndOrder.PopStates;
begin
 inherited;
 raise Exception.Create('not yet implemented');
end;

procedure TThiranAllpass2ndOrder.PushStates;
begin
 inherited;
 raise Exception.Create('not yet implemented');
end;

procedure TThiranAllpass2ndOrder.Reset;
begin
 inherited;
 Frequency := 0.5;
 PhaseDelay := 2;
end;

procedure TThiranAllpass2ndOrder.ResetStates;
begin
 inherited;
 FState[0, 0] := 0;
 FState[0, 1] := 0;
 FState[1, 0] := 0;
 FState[1, 1] := 0;
end;

procedure TThiranAllpass2ndOrder.ResetStatesInt64;
begin
 inherited;
 FState[0, 0] := 0;
 FState[0, 1] := 0;
 FState[1, 0] := 0;
 FState[1, 1] := 0;
end;

initialization
  RegisterDspProcessors32([TThiranAllpass1stOrder, TThiranAllpass2ndOrder]);
  RegisterDspProcessors64([TThiranAllpass1stOrder, TThiranAllpass2ndOrder]);

end.
