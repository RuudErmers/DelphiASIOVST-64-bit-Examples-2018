unit DAV_DspStateVariableFilter;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TFrequencyTuningMethod = (ftmSimple, ftmIdeal);

  TSVF = class(TDspSampleRatePersistent)
  private
    FQ1, FQ, FF1, FF : Single;
    FSampleRateInv   : Single;
    FDelay           : Array [0..1] of Double;
    FFTM             : TFrequencyTuningMethod;
    procedure SetFrequency(Value: Single);
    procedure SetQ(Value: Single);
    procedure CalculateQ;
  protected
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; virtual;
    procedure QChanged; virtual;
  public
    constructor Create; override;
    procedure ProcessSample(const Input: Single; var Low, Band, Notch, High: Single);
    procedure ProcessBlock(Input, Low, Band, Notch, High: PDAVSingleFixedArray; SampleFrames: Integer);
    property Frequency: Single read FF write SetFrequency;
    property SampleRate;
    property Q: Single read FQ write SetQ;
    property FrequencyTuningMethod: TFrequencyTuningMethod read FFTM write FFTM;
  end;

implementation

uses
  SysUtils, DAV_Common;

constructor TSVF.Create;
begin
  inherited;
  FQ1            := 1;
  FF1            := 1000;
  FSampleRateInv := 1 / SampleRate;
  FFTM           := ftmIdeal;
end;

procedure TSVF.SampleRateChanged;
begin
 inherited;
 FSampleRateInv := 1 / SampleRate;
end;

procedure TSVF.SetFrequency(Value: Single);
begin
 if Value <> FF then
  begin
   FF := Value;
   FrequencyChanged;
  end;
end;

procedure TSVF.FrequencyChanged;
begin
 case FFTM of
   ftmSimple :
    begin
     // simple frequency tuning with error towards nyquist
     // F is the filter's center frequency, and Fs is the sampling rate
     if FF > 17000
      then FF1 := 2 * pi * 17000 * FSampleRateInv
      else FF1 := 2 * pi * FF * FSampleRateInv;
     CalculateQ;
    end;
   ftmIdeal :
    begin
     // ideal tuning:
     if FF > 17000
      then FF1 := 2 * sin(pi * 17000 * FSampleRateInv)
      else FF1 := 2 * sin(pi * FF * FSampleRateInv);
     CalculateQ;
    end;
  end;
 Changed;
end;

procedure TSVF.CalculateQ;
const
  CSpd: Double = 1 / 1200;
begin
  if FF > 5000
   then FQ1 := 1 / (FQ + ((FF - 5000) * CSpd))
   else FQ1 := 1 / FQ;
end;

procedure TSVF.SetQ(Value: Single);
begin
  if Value < 0.5 then Value := 0.5;
  if Value <> FQ then
   begin
    FQ := Value;
    QChanged;

   end;
end;

procedure TSVF.QChanged;
begin
 CalculateQ;
 Changed;
end;

procedure TSVF.ProcessSample(const Input: Single; var Low, Band, Notch, High: Single);
begin
  Low := FDelay[1] + FF1 * FDelay[0];
  High := (Input + CDenorm32) - Low - FQ1 * FDelay[0];
  Band := FF1 * High + FDelay[0];
  Notch := High + Low;
 // store delays
  FDelay[0] := Band;
  FDelay[1] := Low - CDenorm32;
end;

procedure TSVF.ProcessBlock(Input, Low, Band, Notch, High: PDAVSingleFixedArray; SampleFrames: Integer);
var
  c: Integer;
begin
  for c := 0 to SampleFrames - 1 do
   begin
    Low^[c] := FDelay[1] + FF1 * FDelay[0];
    High^[c] := (Input^[c] + CDenorm32) - Low^[c] - FQ1 * FDelay[0] - CDenorm32;
    Band^[c] := FF1 * High^[c] + FDelay[0];
    Notch^[c] := High^[c] + Low^[c];

    // store delays
    FDelay[0] := Band^[c];
    FDelay[1] := Low^[c] - CDenorm32;
   end;
end;

end.
