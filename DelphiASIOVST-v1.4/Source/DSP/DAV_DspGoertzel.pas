unit DAV_DspGoertzel;

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
//                                                                            //
//  based on http://en.wikipedia.org/wiki/Goertzel_algorithm                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Classes, DAV_Types, DAV_Complex, DAV_DspDft;

type
  TCustomDspGoertzel = class(TDspSampleRatePersistent, IDspSink32, IDspSink64)
  private
    FFrequency: Double;
    procedure SetFrequency(const Value: Double);
    function GetPower: Double;
    function GetPower_dB: Double;
  protected
    FState       : array [0..1] of Double;
    FCoefficient : Double;
    procedure CalculateCoefficient;
    procedure FrequencyChanged; virtual;
  public
    constructor Create; override;
    procedure ProcessSample32(Input: Single);
    procedure ProcessSample64(Input: Double);
    procedure Reset;

    property Frequency: Double read FFrequency write SetFrequency;
    property Power: Double read GetPower;
    property Power_dB: Double read GetPower_dB;
  end;

  TDspGoertzel = class(TCustomDspGoertzel)
  published
    property Frequency;
    property SampleRate;
  end;

implementation

uses
  Math;

{ TCustomDspGoertzel }

constructor TCustomDspGoertzel.Create;
begin
 inherited;
 FFrequency := 1000;
 CalculateCoefficient;
end;

procedure TCustomDspGoertzel.SetFrequency(const Value: Double);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomDspGoertzel.FrequencyChanged;
begin
 CalculateCoefficient;
end;

function TCustomDspGoertzel.GetPower: Double;
begin
 Result := Sqr(FState[1]) + Sqr(FState[0]) - FCoefficient * FState[1] * FState[0];
end;

function TCustomDspGoertzel.GetPower_dB: Double;
begin
 Result := 10 * Log10(Power);
end;

procedure TCustomDspGoertzel.CalculateCoefficient;
begin
 FCoefficient := 2 * Cos(2 * Pi * FFrequency / SampleRate);
end;

procedure TCustomDspGoertzel.ProcessSample32(Input: Single);
begin
 Input := Input + FCoefficient * FState[0] - FState[1];
 FState[1] := FState[0];
 FState[0] := Input;
end;

procedure TCustomDspGoertzel.ProcessSample64(Input: Double);
begin
 Input := Input + FCoefficient * FState[0] - FState[1];
 FState[1] := FState[0];
 FState[0] := Input;
end;

procedure TCustomDspGoertzel.Reset;
begin
 FState[0] := 0;
 FState[1] := 0;
end;

end.
