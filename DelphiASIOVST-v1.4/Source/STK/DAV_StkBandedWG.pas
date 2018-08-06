unit DAV_StkBandedWG;

{ Banded waveguide modeling class.

  This class uses banded waveguide techniques to model a variety of sounds,
  including bowed bars, glasses, and bowls.  For more information, see Essl, G.
  and Cook, P. "Banded Waveguides: Towards Physical Modelling of Bar Percussion
  Instruments", Proceedings of the 1999 International Computer Music Conference.

  Control Change Numbers:
    - Bow Pressure = 2
    - Bow Motion = 4
    - Strike Position = 8
    - Vibrato Frequency = 11
    - Gain = 1
    - Bow Velocity = 128
    - Set Striking = 64
    - Instrument Presets = 16
      - Uniform Bar = 0
      - Tuned Bar = 1
      - Glass Harmonica = 2
      - Tibetan Bowl = 3
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayL, DAV_StkBowTable,
  DAV_StkAdsr, DAV_StkBiquad, Math;

const
  CMaxBandedModes = 20;

type
  TStkBandedWG = class(TStkInstrument)
  protected
    FDoPluck               : Boolean;
    FTrackVelocity         : Boolean;
    FNModes, FPresetModes  : Integer;
    FBowTabl               : TBowTable;
    FADSR                  : TAdsr;
    FBandpass              : array[0..CMaxBandedModes - 1] of TBiquad;
    FDelay                 : array[0..CMaxBandedModes - 1] of TDelayl;
    FFreakency             : Single;
    FBaseGain              : Single;
    FMaxVelocity           : Single;
    FGains, FBaseGains     : Single;
    FExcitation, FModes    : array[0..CMaxBandedModes - 1] of Single;
    FIntegrationConstant   : Single;
    FVelocityInput         : Single;
    FBowVelocity           : Single;
    FBowTarget             : Single;
    FStrikeAmp             : Single;
    FBowPosition           : Single;
    FStrikePosition        : Integer;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override; 
    procedure Clear;

    // Set strike position (0.0 - 1.0).
    procedure setStrikePosition(position: Single);

    // Select a preset.
    procedure setPreset(preset: Integer);

    // Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

    // Apply bow velocity/pressure to instrument with given amplitude and rate of increase.
    procedure startBowing(amplitude, rate: Single);

    // Decrease bow velocity/breath pressure with given rate of decrease.
    procedure stopBowing(rate: Single);

    // Pluck the instrument with given amplitude.
    procedure pluck(amp: Single);

    // Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: Integer; Value: Single);
  end;

implementation

constructor TStkBandedWG.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  FDoPluck := True;
  for i := 0 to CMaxBandedModes - 1 do
   begin
    FDelay[i] := TDelayl.Create(srate);
    FBandpass[i] := TBiquad.Create(srate);
   end;

  FBowTabl := TBowTable.Create(srate);
  FBowTabl.setSlope(3.0);

  FADSR := TAdsr.Create(srate);
  FADSR.setAllTimes(0.02, 0.005, 0.9, 0.01);

  FFreakency := 220.0;
  setPreset(0);

  FBowPosition := 0;
  FBaseGain := 0.999;

  FIntegrationConstant := 0.0;
  FTrackVelocity := False;

  FBowVelocity := 0.0;
  FBowTarget := 0.0;

  FStrikeAmp := 0.0;
end;

destructor TStkBandedWG.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  FBowTabl.Free;
  FADSR.Free;
  for i := 0 to CMaxBandedModes - 1 do
   begin
    FDelay[i].Free;
    FBandpass[i].Free;
   end;
end;

procedure TStkBandedWG.Clear;
var
  i: Integer;
begin
  for i := 0 to FNModes - 1 do
   begin
    FDelay[i].Clear;
    FBandpass[i].Clear;
   end;
end;

procedure TStkBandedWG.setPreset;
var
  i: Integer;
begin
  case preset of
    1 : // Tuned Bar
     begin
      FPresetModes := 4;
      FModes[0] := 1.0;
      FModes[1] := 4.0198391420;
      FModes[2] := 10.7184986595;
      FModes[3] := 18.0697050938;

      for i := 0 to FPresetModes - 1 do
       begin
        FBaseGains[i] := power(0.999, i + 1);
        FExcitation[i] := 1.0;
       end;

     end;

    2 : // Glass Harmonica
     begin
      FPresetModes := 5;
      FModes[0] := 1.0;
      FModes[1] := 2.32;
      FModes[2] := 4.25;
      FModes[3] := 6.63;
      FModes[4] := 9.38;
    // FModes[5]:= 12.22;

      for i := 0 to FPresetModes - 1 do
       begin
        FBaseGains[i] := power(0.999, i + 1);
        FExcitation[i] := 1.0;
       end;

     end;

    3 : // Tibetan Prayer Bowl (ICMC'02)
     begin
      FPresetModes := 12;
      FModes[0] := 0.996108344;
      FBaseGains[0] := 0.999925960128219;
      FExcitation[0] := 11.900357 / 10.0;
      FModes[1] := 1.0038916562;
      FBaseGains[1] := 0.999925960128219;
      FExcitation[1] := 11.900357 / 10.;
      FModes[2] := 2.979178;
      FBaseGains[2] := 0.999982774366897;
      FExcitation[2] := 10.914886 / 10.;
      FModes[3] := 2.99329767;
      FBaseGains[3] := 0.999982774366897;
      FExcitation[3] := 10.914886 / 10.;
      FModes[4] := 5.704452;
      FBaseGains[4] := 1.0; //0.999999999999999999987356406352;
      FExcitation[4] := 42.995041 / 10.;
      FModes[5] := 5.704452;
      FBaseGains[5] := 1.0; //0.999999999999999999987356406352;
      FExcitation[5] := 42.995041 / 10.;
      FModes[6] := 8.9982;
      FBaseGains[6] := 1.0; //0.999999999999999999996995497558225;
      FExcitation[6] := 40.063034 / 10.;
      FModes[7] := 9.01549726;
      FBaseGains[7] := 1.0; //0.999999999999999999996995497558225;
      FExcitation[7] := 40.063034 / 10.;
      FModes[8] := 12.83303;
      FBaseGains[8] := 0.999965497558225;
      FExcitation[8] := 7.063034 / 10.;
      FModes[9] := 12.807382;
      FBaseGains[9] := 0.999965497558225;
      FExcitation[9] := 7.063034 / 10.;
      FModes[10] := 17.2808219;
      FBaseGains[10] := 0.9999999999999999999965497558225;
      FExcitation[10] := 57.063034 / 10.;
      FModes[11] := 21.97602739726;
      FBaseGains[11] := 0.999999999999999965497558225;
      FExcitation[11] := 57.063034 / 10.;

     end;

  else // Uniform Bar
   begin
    FPresetModes := 4;
    FModes[0] := 1.0;
    FModes[1] := 2.756;
    FModes[2] := 5.404;
    FModes[3] := 8.933;

    for i := 0 to FPresetModes - 1 do
     begin
      FBaseGains[i] := power(0.9, i + 1);
      FExcitation[i] := 1.0;
     end;

   end;
   end;

  FNModes := FPresetModes;
  setFrequency(FFreakency);
end;

procedure TStkBandedWG.setFrequency;
var
  radius, base, length: Single;
  i: Integer;
begin
  FFreakency := frequency;
  if (frequency <= 0.0) then
    FFreakency := 220.0
  else
  if (FFreakency > 1568.0) then
    FFreakency := 1568.0;
  base := srate / FFreakency;
  for i := 0 to FPresetModes - 1 do
   begin
    // Calculate the FDelay line lengths for each mode.
    length := round(base / FModes[i]);
    if (length > 2.0) then
     begin
      FDelay[i].setDelay(length);
      FGains[i] := FBaseGains[i];
     end else
     begin
      FNModes := i;
      break;
     end;
    //  cerr << endl;

    // Set the FBandpass filter resonances
    radius := 1.0 - PI * 32 / srate; //FFreakency * FModes[i] / Stk::sampleRate/32;
    if (radius < 0.0) then
      radius := 0.0;
    FBandpass[i].setResonance(FFreakency * FModes[i], radius, True);

    FDelay[i].Clear;
    FBandpass[i].Clear;
   end;

  //int olen:=(int)(FDelay[0].getDelay);
  //FStrikePosition:=(int)(FStrikePosition*(length/FModes[0])/olen);
end;

procedure TStkBandedWG.setStrikePosition;
begin
  FStrikePosition := round(FDelay[0].getDelay * position / 2.0);
end;

procedure TStkBandedWG.startBowing;
begin
  FADSR.setRate(rate);
  FADSR.keyOn;
  FMaxVelocity := 0.03 + (0.1 * amplitude);
end;

procedure TStkBandedWG.stopBowing;
begin
  FADSR.setRate(rate);
  FADSR.keyOff;
end;

procedure TStkBandedWG.pluck;
var
  i, j: Integer;
  min_len: Single;
begin
  min_len := FDelay[FNModes - 1].getDelay;
  for i := 0 to FNModes - 1 do
    for j := 0 to round(FDelay[i].getDelay / min_len) - 1 do
      FDelay[i].tick(FExcitation[i] * amp / FNModes);
end;

procedure TStkBandedWG.noteOn;
begin
  setFrequency(frequency);
  if (FDoPluck) then
    pluck(amplitude)
  else
    startBowing(amplitude, amplitude * 0.001);
end;

procedure TStkBandedWG.noteOff;
begin
  if (not FDoPluck) then
    stopBowing((1.0 - amplitude) * 0.005);
end;

function TStkBandedWG.tick: Single;
var
  k: Integer;
  Data, input: Single;
begin
  if (FDoPluck) then
    input := 0.0//  input:=FStrikeAmp/FNModes;
//  FStrikeAmp:=0.0;

  else
   begin
    if (FIntegrationConstant = 0.0) then
      FVelocityInput := 0.0
    else
      FVelocityInput := FIntegrationConstant * FVelocityInput;

    for k := 0 to FNModes - 1 do
      FVelocityInput := FVelocityInput + (FBaseGain * FDelay[k].lastOut);

    if (FTrackVelocity) then
     begin
      FBowVelocity := FBowVelocity * 0.9995;
      FBowVelocity := FBowVelocity + FBowTarget;
      FBowTarget := FBowTarget * 0.995;
     end
    else
      FBowVelocity := FADSR.tick * FMaxVelocity;

    input := FBowVelocity - FVelocityInput;
    input := input * FBowTabl.tick(input);
    input := input / FNModes;
   end;

  Data := 0.0;
  for k := 0 to FNModes - 1 do
   begin
    FBandpass[k].tick(input + FGains[k] * FDelay[k].lastOut);
    FDelay[k].tick(FBandpass[k].lastOut);
    Data := Data + FBandpass[k].lastOut;
   end;

  //lastOutput:=data * FNModes;
  lastOutput := Data * 4;
  Result := lastOutput;
end;

procedure TStkBandedWG.controlChange;
var
  norm: Single;
  i: Integer;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_BowPressure_) then
   begin // 2
    if (norm = 0.0) then
      FDoPluck := True
    else
     begin
      FDoPluck := False;
      FBowTabl.setSlope(10.0 - (9.0 * norm));
     end;
   end
  else if (number = 4) then
   begin // 4
    if (not FTrackVelocity) then
      FTrackVelocity := True;
    FBowTarget := FBowTarget + 0.005 * (norm - FBowPosition);
    FBowPosition := norm;
    //FADSR.setTarget(FBowPosition);
   end
  else if (number = 8) then // 8
    setStrikePosition(norm)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    //FBowTarget += 0.02 * (norm - FBowPosition);
    //FBowPosition:=norm;
    if (FTrackVelocity) then
      FTrackVelocity := False;
    FMaxVelocity := 0.13 * norm;
    FADSR.setTarget(norm);
   end
  else if (number = __SK_ModWheel_) then
   begin // 1
    //    FBaseGain:=0.9989999999 + (0.001 * norm );
    FBaseGain := 0.8999999999999999 + (0.1 * norm);
    //  cerr << "Yuck!" << endl;
    for i := 0 to FNModes - 1 do
      FGains[i] := FBaseGains[i] * FBaseGain;
    //      FGains[i]= pow(FBaseGain, (int)(FDelay[i].getDelay+i));
   end
  else if (number = __SK_ModFrequency_) then // 11
    FIntegrationConstant := norm
  else if (number = __SK_Sustain_) then
   begin // 64
    if (norm < 0.5) then
      FDoPluck := True
    else
      FDoPluck := False;
   end
  else if (number = __SK_Portamento_) then
   begin // 65
    if (norm < 0.5) then
      FTrackVelocity := False
    else
      FTrackVelocity := True;
   end
  else if (number = __SK_ProphesyRibbon_) then // 16
    setPreset(round(Value * 3));
end;

end.
