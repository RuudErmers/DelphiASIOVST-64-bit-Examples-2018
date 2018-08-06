unit LunchBoxEvent;

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

{$I DAV_Compiler.inc}

uses
  DAV_Complex;

type
  TLunchBoxSample = class(TObject)
  private
    FMidiKeyNr   : Integer;
    FVelocity    : Integer;
    FSampleRate  : Single;
    FFrequency   : Single;
    FAmplitude   : Single;
    FSamplePos   : Integer;
    FSampleFrac  : Single;
    FSampleInc   : Single;
    FIsPlaying   : Boolean;
    FSampleIndex : Integer;
    FMem         : array [0..3] of Single;

    FAngle,
    FPosition    : TComplex64;
    FPatPos      : Integer;
    function GetSampleRate: Single; virtual;
    procedure SetSampleRate(Value: Single); virtual;
  public
    constructor Create(Sample: Integer);
    destructor Destroy; override;

    procedure SetFrequency(Frequency: Single); virtual;
    function ProcessSample32: Single; virtual;
    procedure NoteOn(Amplitude: Single);
    procedure NoteOff;

    property IsPlaying: Boolean read FIsPlaying write FIsPlaying;
    property PatternPosition: Integer read FPatPos write FPatPos;
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read GetSampleRate write SetSampleRate;
    property MidiKeyNr: Integer read FMidiKeyNr write FMidiKeyNr;
    property Velocity: Integer read FVelocity write FVelocity;
    property SampleIndex: Integer read FSampleIndex;
  end;

implementation

uses
  DAV_Types, DAV_Math, DAV_DspInterpolation, LunchBoxMain;


{ TLunchBoxSample }

constructor TLunchBoxSample.Create(Sample : Integer);
begin
 SampleRate   := 44100;
 FPosition.Re := 0;
 FPosition.Im := -1;
 FSamplePos   := 0;
 FSampleFrac  := 0;
 FSampleInc   := 0;
 FPatPos      := 0;
 FSampleIndex := Sample;
 FIsPlaying   := False;
end;

destructor TLunchBoxSample.Destroy;
begin
 inherited;
end;

function TLunchBoxSample.GetSampleRate: Single;
begin
 result := FSampleRate;
end;

procedure TLunchBoxSample.NoteOn(Amplitude: Single);
begin
 FIsPlaying := True;
 FAmplitude := Amplitude;
end;

procedure TLunchBoxSample.NoteOff;
begin
 FSamplePos := 0;
 FIsPlaying := False;
end;

procedure TLunchBoxSample.SetSampleRate(Value: Single);
begin
 if (Value > 0) then FSampleRate := Value;
end;

function TLunchBoxSample.ProcessSample32: Single;
begin
 Result := FAmplitude * Hermite32_asm(FSampleFrac, @FMem[0]);
 FSampleFrac := FSampleFrac + FSampleInc;
 while FSampleFrac >= 1 do
  begin
   inc(FSamplePos);
   if FSamplePos >= Length(Samples[FSampleIndex].Data)
    then NoteOff;
   FSampleFrac := FSampleFrac - 1;
   Move(FMem[1], FMem[0], 12);
   FMem[3] := Samples[FSampleIndex].Data[FSamplePos];
  end;
end;

procedure TLunchBoxSample.SetFrequency(Frequency: Single);
begin
 FFrequency := Frequency;
 FSampleInc := Frequency;
 GetSinCos(2 * Pi * FFrequency / FSampleRate, FAngle.Im, FAngle.Re);
end;

end.
