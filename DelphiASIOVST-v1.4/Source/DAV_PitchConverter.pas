{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_PitchConverter;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Pitch converters                                                          //
//  The four representations for pitch in MPEG4-SA:                           //
//                                                                            //
//  - MIDI pitch number representation. A pitch is represented as an          //
//    integer number of semitones above or below middle C, represented        //
//    as 60.                                                                  //
//    For example, 57 is the A below middle C.                                //
//                                                                            //
//  - Frequency, or cps representation. A pitch is represented as some        //
//    number of cycles per second.                                            //
//    For example, 220 Hz is the A below middle C.                            //
//                                                                            //
//  - pitch-class, or pch representation. A pitch is represented as an        //
//    integer part, which represents the octave number, where 8 shall be      //
//    the octave containing middle C (C4); plus a fractional part, which      //
//    represents the pitch-class, where .00 shall be C, .01 shall be C#,      //
//    .02 shall be D, and so forth. Fractional parts larger than .11 (B)      //
//    have no meaning in this representation; fractional parts between        //
//    the pitch-class steps are rounded to the nearest pitch-class.           //
//    For example, 7.09 is the A below middle C.                              //
//                                                                            //
//  - octave-fraction, or oct representation. A pitch is represented as       //
//    an integer part, which represents the octave number, where 8 shall      //
//    be the octave containing middle C (C4); plus a fractional part, which   //
//    represents a fraction of an octave, where each step of 1/12 represents  //
//    a semitone.                                                             //
//    For example, 7.75 is the A below middle C, in equal-tempered tuning.    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  SysUtils;

type
  TPitchConverter = class
  private
    FCps          : Double;
    FPch          : Double;
    FOct          : Double;
    FMidi         : Integer;
    FName         : string;
    FGlobalTuneA4 : Double;
    procedure SetCPS(const Value: Double);
    procedure SetMidi(const Value: Integer);
    procedure SetOct(const Value: Double);
    procedure SetPch(const Value: Double);
    procedure SetName(const Value: String);
  public
    constructor Create; virtual;
    procedure SaveFreqFile(const FileName: TFileName);
    function PchToOct(Value: Double): Double;
    function PchToCyclesPerSample(Value: Double): Double;
    function PchToMidi(const Value: Double): Integer;
    function OctToPch(Value: Double): Double;
    function OctToCyclesPerSample(Value: Double): Double;
    function OctToMidi(const Value: Double): Integer;
    function CyclesPerSampleToPch(Value: Double): Double;
    function CyclesPerSampleToOct(Value: Double): Double;
    function CyclesPerSampleToMidi(Value: Double): Integer;
    function MidiToPch(Value: Integer): Double;
    function MidiToOct(const Value: Integer): Double;
    function MidiToCyclesPerSample(const Value: Integer): Double;
    function MidiToName(const Value: Integer): String;

    property CyclesPerSample: Double read FCps write SetCPS;
    property OctaveFraction: Double read FOct write SetOct;
    property PitchClass: Double read FPch write SetPch;
    property Midi: Integer read FMidi write SetMidi;
    property Name: string read FName write SetName;
    property GlobalTuneA4: Double read FGlobalTuneA4 write FGlobalTuneA4;
  end;

const
  CNotes: array[0..11] of String =
    ('C ', 'C#', 'D ', 'D#', 'E ', 'F ', 'F#', 'G ', 'G#', 'A ', 'A#', 'B ');

implementation

uses
  Math;

{ TPitchConverter }

constructor TPitchConverter.Create;
begin
 inherited;
 FGlobalTuneA4 := 440;
 DecimalSeparator := '.';
end;

function TPitchConverter.CyclesPerSampleToMidi(Value: Double): Integer;
begin
 if Value < 0 then Value := 0;
 Result := abs(round(12 * log2(Value / FGlobalTuneA4) + 69));
 if Result > 127 then Result := -1;
end;

function TPitchConverter.CyclesPerSampleToOct(Value: Double): Double;
begin
  if Value < 0 then Value := 0;
  Result := log2(Value / FGlobalTuneA4) + 8.75;
end;

function TPitchConverter.CyclesPerSampleToPch(Value: Double): Double;
var
  k, z: Double;
begin
  if Value < 0 then Value := 0;
  k := log2(Value / FGlobalTuneA4) + 8.75;
  z := round(12 * frac(k));
  Result := int(k) + z * 1E-2;
end;

function TPitchConverter.MidiToCyclesPerSample(const Value: Integer): Double;
begin
  if (Value >= 0) and (Value <= 127)
   then Result := FGlobalTuneA4 * power(2, (Value - 69) / 12)
   else Result := -1;
end;

function TPitchConverter.MidiToName(const Value: Integer): String;
begin
 if (Value >= 0) and (Value <= 127)
  then Result := CNotes[Value mod 12] + IntToStr(Value div 12 - 2)
  else Result := '---';
end;

function TPitchConverter.MidiToOct(const Value: Integer): Double;
begin
 if (Value >= 0) and (Value <= 127)
  then Result := (Value + 36) / 12
  else Result := -1;
end;

function TPitchConverter.MidiToPch(Value: Integer): Double;
var
  k: Double;
begin
 if (Value >= 0) and (Value <= 127) then
  begin
   Value := round(Value);
   k := (Value + 36) / 12;
   Result := int(k) + 12 * frac(k) * 1E-2;
  end else Result := -1;
end;

function TPitchConverter.OctToCyclesPerSample(Value: Double): Double;
begin
 if Value < 0 then Value := 0;
 Result := FGlobalTuneA4 * Power(2, Value - 8.75);
end;

function TPitchConverter.OctToMidi(const Value: Double): Integer;
begin
  if (Value <= 3)
   then Result := -1
   else Result := round(12 * (Value - 3));
  if Result > 127 then Result := -1;
end;

function TPitchConverter.OctToPch(Value: Double): Double;
var
  z: Double;
begin
  if Value < 0 then Value := 0;
  z := round(frac(Value) * 12);
  Result := int(Value) + z * 1E-2;
end;

function TPitchConverter.PchToCyclesPerSample(Value: Double): Double;
var
  z: Double;
begin
  if Value < 0 then Value := 0;
  z := round(frac(Value) * 100);
  if (z < 0) or (z > 11) then z := 0;
  Result := FGlobalTuneA4 * power(2, Value + z / 12 - 8.75);
end;

function TPitchConverter.PchToMidi(const Value: Double): Integer;
var
  z: Double;
begin
  if Value <= 3 then Result := -1
  else
   begin
    z := round(100 * frac(Value)) * 1E-2;
    if (z < 0) or (z > 0.11) then
      z := 0;
    Result := round(100 * z + 12 * (Value - 3));
   end;
  if Result > 127 then Result := -1;
end;

function TPitchConverter.PchToOct(Value: Double): Double;
var
  z: Double;
begin
  if Value < 0 then Value := 0;
  z := round(frac(Value) * 100) * 1E-2;
  if (z < 0) or (z > 0.11) then z := 0;
  Result := int(Value) + 100 * z / 12;
end;

procedure TPitchConverter.SetCPS(const Value: Double);
begin
 if FCps <> Value then
  begin
   FCps  := Value;
   FOct  := CyclesPerSampleToOct(FCps);
   FPch  := CyclesPerSampleToPch(FCps);
   FMidi := CyclesPerSampleToMidi(FCps);
   FName := MidiToName(FMidi);
  end;
end;

procedure TPitchConverter.SaveFreqFile(const FileName: TFileName);
var
  f    : textfile;
  i, j : Integer;
  s    : string;
begin
  assignfile(f, FileName);
  rewrite(f);
  s := 'const FrequTable : array[0..127] of double = (';
  writeln(f, s);
  for i := 0 to 126 do
   begin
    s := FloatToStr(MidiToCyclesPerSample(i)) + ',';
    for j := 1 to Length(s) - 1 do
      if s[j] = ',' then
        s[j] := '.';
    writeln(f, s);
   end;
  s := FloatToStr(MidiToCyclesPerSample(127)) + ');';
  for j := 1 to Length(s) - 1 do
    if s[j] = ',' then
      s[j] := '.';
  writeln(f, s);
  closefile(f);
end;

procedure TPitchConverter.SetMidi(const Value: Integer);
begin
 if FMidi <> Value then
  begin
   FMidi := Value;
   FOct  := MidiToOct(FMidi);
   FPch  := MidiToPch(FMidi);
   FCps  := MidiToCyclesPerSample(FMidi);
   FName := MidiToName(FMidi);
  end;
end;

procedure TPitchConverter.SetName(const Value: String);
var
  n, c, i: Integer;
  s: string;
begin
  s := UpperCase(Value);
  if Length(s) = 2
   then s := s[1] + '  ' + s[2];
  if Length(s) = 3 then
   if s[2] = '-'
    then s := s[1] + ' ' + s[2] + s[3]
    else s := s[1] + s[2] + ' ' + s[3];
  if Length(s) <> 4 then exit;
  c := -1;
  for i := 0 to 11 do
   if CNotes[i] = copy(s, 1, 2) then
    begin
     c := i;
     break;
    end;
   try
    i := StrToInt(copy(s, 3, 2)) + 2;
    n := i * 12 + c;
   except
    n := -1;
   end;
  if c < 0 then n := -1;
  Midi := n;
end;

procedure TPitchConverter.SetOct(const Value: Double);
begin
 if FOct <> Value then
  begin
   FOct := Value;
   FCps := OctToCyclesPerSample(FOct);
   FPch := OctToPch(FOct);
   FMidi := OctToMidi(FOct);
   FName := MidiToName(FMidi);
  end;
end;

procedure TPitchConverter.SetPch(const Value: Double);
begin
 if FPch <> Value then
  begin
   FPch := Value;
   FCps := PchToCyclesPerSample(FPch);
   FOct := PchToOct(FPch);
   FMidi := PchToMidi(FPch);
   FName := MidiToName(FMidi);
  end;
end;

end.
