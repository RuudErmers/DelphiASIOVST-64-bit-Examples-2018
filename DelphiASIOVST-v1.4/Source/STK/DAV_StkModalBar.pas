unit DAV_StkModalBar;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK resonant bar instrument class.

  This class implements a number of different struck bar instruments.
  It inherits from the modal class.

  Control Change Numbers:
    - Stick Hardness = 2
    - Stick Position = 4
    - Vibrato Gain = 11
    - Vibrato Frequency = 1
    - Volume = 128
    - Modal CPresets = 3
      - Marimba = 0
      - Vibraphone = 1
      - Agogo = 2
      - Wood1 = 3
      - Reso = 4
      - Wood2 = 5
      - Beats = 6
      - Two Fixed = 7
      - Clump = 8
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkModal, DAV_StkWavePlayer, Math;

  // CPresets:
  //     First line:  relative modal frequencies (negative number is
  //                  a fixed mode that doesn't scale with frequency
  //     Second line: resonances of the modes
  //     Third line:  mode volumes
  //     Fourth line: stickHardness, strikePosition, and direct stick
  //                  gain (mixed directly into the output)
const

  CPresets: array[0..8, 0..3, 0..3] of Single =
    (((1.0, 3.99, 10.65, -2443),    // Marimba
    (0.9996, 0.9994, 0.9994, 0.999),
    (0.04, 0.01, 0.01, 0.008),
    (0.429688, 0.445312, 0.093750, 0)),

    ((1.0, 2.01, 3.9, 14.37),     // Vibraphone
    (0.99995, 0.99991, 0.99992, 0.9999),
    (0.025, 0.015, 0.015, 0.015),
    (0.390625, 0.570312, 0.078125, 0)),
    ((1.0, 4.08, 6.669, -3725.0),    // Agogo
    (0.999, 0.999, 0.999, 0.999),
    (0.06, 0.05, 0.03, 0.02),
    (0.609375, 0.359375, 0.140625, 0)),
    ((1.0, 2.777, 7.378, 15.377),    // Wood1
    (0.996, 0.994, 0.994, 0.99),
    (0.04, 0.01, 0.01, 0.008),
    (0.460938, 0.375000, 0.046875, 0)),
    ((1.0, 2.777, 7.378, 15.377),    // Reso
    (0.99996, 0.99994, 0.99994, 0.9999),
    (0.02, 0.005, 0.005, 0.004),
    (0.453125, 0.250000, 0.101562, 0)),
    ((1.0, 1.777, 2.378, 3.377),    // Wood2
    (0.996, 0.994, 0.994, 0.99),
    (0.04, 0.01, 0.01, 0.008),
    (0.312500, 0.445312, 0.109375, 0)),
    ((1.0, 1.004, 1.013, 2.377),    // Beats
    (0.9999, 0.9999, 0.9999, 0.999),
    (0.02, 0.005, 0.005, 0.004),
    (0.398438, 0.296875, 0.070312, 0)),
    ((1.0, 4.0, -1320.0, -3960.0),    // 2Fix
    (0.9996, 0.999, 0.9994, 0.999),
    (0.04, 0.01, 0.01, 0.008),
    (0.453125, 0.453125, 0.070312, 0)),
    ((1.0, 1.217, 1.475, 1.729),    // Clump
    (0.999, 0.999, 0.999, 0.999),
    (0.03, 0.03, 0.03, 0.03),
    (0.390625, 0.570312, 0.078125, 0)));

type
  TStkModalBar = class(TStkModal)
  private
    FPreset: Integer;

// Set the modulation (vibrato) depth.
//  procedure setModulationDepth(mDepth:Single);

    // Set stick hardness (0.0 - 1.0).
    procedure SetStickHardness(const Hardness: Single);

    // Set stick position (0.0 - 1.0).
    procedure SetStrikePosition(const Position: Single);

    // Select a bar preset (currently modulo 9).
    procedure SetPreset(const Value: Integer);
  protected
    procedure PresetChanged; virtual;
  public
    constructor Create(const SampleRate: Single; const Modes: Integer = 4); override;
    destructor Destroy; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;

    property StickHardness: Single read FStickHardness write SetStickHardness;
    property StrikePosition: Single read FStrikePosition write SetStrikePosition;
    property Preset: Integer read FPreset write SetPreset;
  end;

implementation

uses
  SysUtils;

constructor TStkModalBar.Create(const SampleRate: Single; const Modes: Integer = 4);
begin
  inherited Create(SampleRate, Modes);
  FWave := TStkWavePlayer.Create(SampleRate, 'marmstk1.wav');
  FWave.OneShot := False;
  FWave.Frequency := 22050;

  // Set the resonances for preset 0 (marimba).
  FPreset := 0;
  PresetChanged;
end;

destructor TStkModalBar.Destroy;
begin
 FreeAndNil(FWave);
 inherited Destroy;
end;

procedure TStkModalBar.setStickHardness;
begin
  FStickHardness := hardness;
  if (hardness < 0.0) then
    FStickHardness := 0.0
  else if (hardness > 1.0) then
    FStickHardness := 1.0;
  FWave.Frequency := 0.25 * Power(4.0, FStickHardness);
  FMasterGain := 0.1 + (1.8 * FStickHardness);
end;

procedure TStkModalBar.setStrikePosition;
var
  temp, temp2: Single;
begin
  strikePosition := position;
  if (position < 0.0) then
    strikePosition := 0.0
  else if (position > 1.0) then
    strikePosition := 1.0;

  // Hack only first three modes.
  temp2 := position * PI;
  temp := sin(temp2);
  setModeGain(0, 0.12 * temp);

  temp := sin(0.05 + (3.9 * temp2));
  setModeGain(1, -0.03 * temp);

  temp := sin(-0.05 + (11 * temp2));
  setModeGain(2, 0.11 * temp);
end;

procedure TStkModalBar.SetPreset(const Value: Integer);
begin
 if FPreset <> Value then
  begin
   FPreset := Value;
   PresetChanged;
  end;
end;

procedure TStkModalBar.PresetChanged;
var
  i, temp: Integer;
begin
 temp := (preset mod 9);
 for i := 0 to CMaxModes - 1 do
  begin
   SetRatioAndRadius(i, CPresets[temp][0][i], CPresets[temp][1][i]);
   SetModeGain(i, CPresets[temp][2][i]);
  end;

 setStickHardness(CPresets[temp][3][0]);
 setStrikePosition(CPresets[temp][3][1]);
 directGain := CPresets[temp][3][2];

 if (temp = 1)
  then FVibratoGain := 0.2 // vibraphone
  else FVibratoGain := 0.0;
end;

procedure TStkModalBar.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMidiStickHardness) then // 2
    setStickHardness(norm)
  else if (number = CMidiStrikePosition) then // 4
    setStrikePosition(norm)
  else if (number = CMidiProphesyRibbon) then // 3
    setPreset(round(Value))
{  else if (number = CMidiModWheel) then // 1
    directGain:=norm}
  else if (number = 11) then // 11
    FVibratoGain := norm * 0.3
  else if (number = CMidiModFrequency) then // 1
    FVibrato.Frequency := norm * 12.0
  else if (number = CMidiAfterTouchCont) then // 128
    FEnvelope.Target := norm;
end;

end.

