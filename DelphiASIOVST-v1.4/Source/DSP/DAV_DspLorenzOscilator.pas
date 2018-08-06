unit DAV_DspLorenzOscilator;

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
//  The code is based on a C++ version (c) 2004 Russell Borogove              //
//  Initial Delphi Pascal Version ©2005, Thaddy de Koning                     //
//  Reviewed and rewritten as part of this project by Christian-W. Budde      //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Lorenz/Rossler iterative function systems as LFOs                         //
//  -------------------------------------------------                         //
//                                                                            //
//  This module defines the classes TLorenzOsc and TRosslerOsc - low          //
//  frequency oscillators suitable for modeling 'analog drift' or other       //
//  random-but-smooth processes. Both classes have identical APIs - you       //
//  could unify the interface with virtual functions easily.                  //
//                                                                            //
//  SetSampleRate:                                                            //
//  Sets the sample SampleRate at which the Iterate function will be called.  //
//  Only meaningful in conjunction with the SetFreq function.                 //
//                                                                            //
//  SetFreq:                                                                  //
//  Sets the fundamental frequency of the oscillator. The Rossler oscillator  //
//  should exhibit harmonic peaks at multiples of that frequency; the Lorenz  //
//  oscillator has a linear frequency-amplitude relation, so SetFreq will     //
//  only control the scale of waveform features in a general way.             //
//                                                                            //
//  Iterate:                                                                  //
//  Advances the clock by one sample period and returns the value of the      //
//  function at the current clock; it should be called once per sample-tick.  //
//                                                                            //
//  GetCurrent:                                                               //
//  Returns the same value returned by the latest call to Iterate. Useful     //
//  in cases where one generator modulates multiple destinations, for         //
//  example.                                                                  //
//                                                                            //
//  GetAlternate:                                                             //
//  Returns a value separate from the current value but correlated with it;   //
//  these are the X and Y values used for the well-known "butterfly" plots    //
//  of the Lorenz and Rossler functions. You can use GetAlternate if you      //
//  want two separate LFOs which are related in mysterious ways at a low      //
//  cost - for example, you can fine-tune one audio oscillator with the       //
//  return from Iterate and another oscillator with the return from           //
//  GetAlternate.                                                             //
//                                                                            //
//  Both the primary and alternate returns are calibrated to a -1.0 to +1.0   //
//  range in normal usage. The implementation is discrete, though, so if the  //
//  sample SampleRate is low or the frequency high, it may occasionally jump  //
//  outside that range -- the user is responsible for clamping if the range   //
//  is critical.                                                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  SysUtils, DAV_Types, DAV_Classes;

const
  CLorenzScale: Single = 0.05107;
  CLorenzAltScale: Single = 0.03679;
  CRosslerScale: Single = 0.05757;
  CRosslerAltScale: Single = 0.06028;

type
  TCustomLorenzRosslerOsc = class(TDspSampleRatePersistent, IDspGenerator32)
  private
    procedure SetFrequency(const Frequency: Single);
  protected
    FDX, FDY   : Single;
    FDZ, FDT   : Single;
    FFrequency : Single;
    FX, FY, FZ : Single;
    FA, FB, FC : Single;
    procedure FrequencyChanged; virtual; abstract;
  public
    constructor Create; override;
    function ProcessSample32: Single; virtual; abstract;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray;
      SampleCount: Integer);
    property Frequency: Single read FFrequency write SetFrequency;
  end;


  // Lorenz function - very broad spectrum noise function with amplitude
  // decreasing with increasing frequency, but tight short-term correlation.
  //
  // The scale of waveform features will change somewhat with the set frequency
  // and sample SampleRate, but not drastically - it's fairly fractal. In particular,
  // there will not be substantial spectral peaks at multiples of the frequency
  // selected by SetFreq.

  TLorenzOsc = class(TCustomLorenzRosslerOsc)
  private
    procedure CalculateSampleCycles;
  protected
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; override;
  public
    constructor Create; override;

    function GetCurrent: Single;
    function GetAlternate: Single;
    function ProcessSample32: Single; override;
  published
    property Frequency;
    property SampleRate;
  end;


  // Rossler function - broad spectrum noise function with amplitude
  // decreasing with increasing frequency, and distinct harmonic peaks. The
  // peaks should occur at harmonics of the frequency set by SetFreq.

  TRosslerOsc = class(TCustomLorenzRosslerOsc)
  private
    procedure CalculateSampleCycles;
  protected
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; override;
  public
    constructor Create; override;

    function GetCurrent: Single;
    function GetAlternate: Single;
    function ProcessSample32: Single; override;
  published
    property Frequency;
    property SampleRate;
  end;

implementation

{ TCustomLorenzRosslerOsc }

constructor TCustomLorenzRosslerOsc.Create;
begin
  inherited;
  FFrequency := 440;
  FDX := 0;
  FDY := 0;
  FDZ := 0;
  FX := 1;
  FY := 1;
  FZ := 1;
end;

procedure TCustomLorenzRosslerOsc.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32;
end;

procedure TCustomLorenzRosslerOsc.SetFrequency(const Frequency: Single);
begin
 if FFrequency <> Frequency then
  begin
   FFrequency := Frequency;
   FrequencyChanged;
  end;
end;

{ TLorenzOsc }

constructor TLorenzOsc.Create;
begin  
  inherited;
  FA := 10.0;
  FB := 28.0;
  FC := 2.666;
  CalculateSampleCycles;
end;

procedure TLorenzOsc.SampleRateChanged;
begin
 CalculateSampleCycles;
 Changed;
end;

procedure TLorenzOsc.FrequencyChanged;
begin
 CalculateSampleCycles;
 Changed;
end;

procedure TLorenzOsc.CalculateSampleCycles;
begin
 FDT := FFrequency / SampleRate;
end;

function TLorenzOsc.GetCurrent: Single;
begin
  Result := FX * CLorenzScale;
end;

function TLorenzOsc.GetAlternate: Single;
begin
  Result := FY * CLorenzAltScale;
end;

function TLorenzOsc.ProcessSample32: Single;
begin
  FDX := FA * (FY-FX);
  FDY := FX * (FB-FZ) - FY;
  FDZ := FX * FY - FC * FZ;

  FX := FX + FDX * FDT;
  FY := FY + FDY * FDT;
  FZ := FZ + FDZ * FDT;

  Result:= FX * CLorenzScale;
end;


{ TRosslerOsc }

constructor TRosslerOsc.Create;
begin
  inherited;
  FA := 0.15;
  FB := 0.20;
  FC := 10;
  CalculateSampleCycles;
end;

procedure TRosslerOsc.SampleRateChanged;
begin
 CalculateSampleCycles;
 Changed;
end;

procedure TRosslerOsc.FrequencyChanged;
begin
 CalculateSampleCycles;
 Changed;
end;

procedure TRosslerOsc.CalculateSampleCycles;
begin
 FDT := 2.91 * FFrequency / SampleRate;
end;

function TRosslerOsc.GetCurrent: Single;
begin
  Result:= FX * CRosslerScale;
end;

function TRosslerOsc.GetAlternate: Single;
begin
  Result:= FY * CRosslerAltScale;
end;

function TRosslerOsc.ProcessSample32: Single;
begin
  FDX := -FY - FZ;
  FDY := FX + FA * FY;
  FDZ := FB + FZ * (FX - FC);

  FX := FX + FDX * FDT;
  FY := FY + FDY * FDT;
  FZ := FZ + FDZ * FDT;

  Result := FX * CRosslerScale;
end;

end.

