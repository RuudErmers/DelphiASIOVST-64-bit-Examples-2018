unit DAV_StkMandolin;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  Stk TStkMandolin instrument model class.

   This class inherits from PluckTwo and uses "commuted synthesis" techniques
   to model a mandolin instrument.

   This is a digital waveguide model, making its use possibly subject to
   patents held by Stanford University, Yamaha, and others.
   Commuted Synthesis, in particular, is covered by patents, granted, pending,
   and/or applied-for. All are assigned to the Board of Trustees,
   Stanford University. For information, contact the Office of Technology
   Licensing, Stanford University.

   Control Change Numbers:
     - Body Size = 2
     - Pluck APosition = 4
     - String Sustain = 11
     - String Detuning = 1
     - Microphone APosition = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkPluckTwo, DAV_StkWavePlayer;

type
  TStkMandolin = class(TStkPluckTwo)
  private
    // Set the body Size (a value of 1.0 produces the "default" Size).
    procedure SetBodySize(const Value: Single);
  protected
    FSoundFile  : array[0..11] of TStkWavePlayer;
    FBodySize   : Single;
    FDirectBody : Single;
    FMic        : Integer;
    FDampTime   : Integer;
    FWaveDone   : Boolean;

    procedure BodySizeChanged; virtual;
  public
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;
    destructor Destroy; override;

    // Pluck the strings with the given amplitude (0.0 - 1.0) using the current frequency.
    procedure Pluck(const Amplitude: Single); overload;

    // Pluck the strings with the given amplitude (0.0 - 1.0) and position (0.0 - 1.0).
    procedure Pluck(const Amplitude, Position: Single); overload;

    // Start a note with the given frequency and amplitude (0.0 - 1.0).
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by number and value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;

    property BodySize: Single read FBodySize write SetBodySize;
  end;

implementation

uses
  SysUtils;

constructor TStkMandolin.Create;
begin
  inherited Create(SampleRate, LowestFrequency);
  FSoundFile[ 0] := TStkWavePlayer.Create(SampleRate, 'mand1.wav');
  FSoundFile[ 1] := TStkWavePlayer.Create(SampleRate, 'mand2.wav');
  FSoundFile[ 2] := TStkWavePlayer.Create(SampleRate, 'mand3.wav');
  FSoundFile[ 3] := TStkWavePlayer.Create(SampleRate, 'mand4.wav');
  FSoundFile[ 4] := TStkWavePlayer.Create(SampleRate, 'mand5.wav');
  FSoundFile[ 5] := TStkWavePlayer.Create(SampleRate, 'mand6.wav');
  FSoundFile[ 6] := TStkWavePlayer.Create(SampleRate, 'mand7.wav');
  FSoundFile[ 7] := TStkWavePlayer.Create(SampleRate, 'mand8.wav');
  FSoundFile[ 8] := TStkWavePlayer.Create(SampleRate, 'mand9.wav');
  FSoundFile[ 9] := TStkWavePlayer.Create(SampleRate, 'mand10.wav');
  FSoundFile[10] := TStkWavePlayer.Create(SampleRate, 'mand11.wav');
  FSoundFile[11] := TStkWavePlayer.Create(SampleRate, 'mand12.wav');
  FDirectBody := 1.0;
  FMic := 0;
  FDampTime := 0;
// FWaveDone := FSoundFile[FMic].isFinished;
end;

destructor TStkMandolin.Destroy;
var
  i: Integer;
begin
 for i := 0 to 11 do FreeAndNil(FSoundFile[i]);
 inherited Destroy;
end;

procedure TStkMandolin.Pluck(const Amplitude: Single);
begin
 // This function gets interesting, because pluck
 // may be longer than string length, so we just
 // reset the FSoundFile and add in the pluck in
 // the Tick method.
  FSoundFile[FMic].reset;
  FWaveDone := False;
  FPluckAmplitude := Amplitude;
  if (Amplitude < 0.0) then FPluckAmplitude := 0.0
  else if (Amplitude > 1.0) then FPluckAmplitude := 1.0;
 // Set the pick APosition, which puts zeroes at APosition * length.
  FCombDelay.Delay := (0.5 * FPluckPosition * FLastLength);
  FDampTime := round(FLastLength);   // See Tick method below.
end;

procedure TStkMandolin.Pluck(const Amplitude, Position: Single);
begin
  // Pluck APosition puts zeroes at APosition * length.
  FPluckPosition := Limit(Position, 0, 1);
  Pluck(Amplitude);
end;

procedure TStkMandolin.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  Pluck(Amplitude);
end;

procedure TStkMandolin.SetBodySize(const Value: Single);
begin
 if FBodySize <> Value then
  begin
   FBodySize := Value;
   BodySizeChanged;
  end;
end;

procedure TStkMandolin.BodySizeChanged;
var
  Rate : Single;
  i    : Integer;
begin
  // Scale the commuted body response by its sample rate (22050).
  Rate := FBodySize;
  for i := 0 to 11
   do FSoundFile[i].Frequency := Rate;
end;

function TStkMandolin.Tick: Single;
var
  temp: Single;
begin
// if ( not FWaveDone ) then
   begin
   // Scale the pluck excitation with comb
   // filtering for the duration of the file.
    temp := FSoundFile[FMic].Tick * FPluckAmplitude;
    temp := temp - FCombDelay.Tick(temp);
//    FWaveDone:=FSoundFile[FMic].isFinished;
   end;

  // Damping hack to help aprocedure overflow on re-plucking.
  if (FDampTime >= 0) then
   begin
    FDampTime := FDampTime - 1;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    FLastOutput := FDelayLine.Tick(
      FFilter.Tick(temp + (FDelayLine.LastOutput * 0.7)));
    // Calculate 2nd delay just like the 1st.
    FLastOutput := FLastOutput + FDelayLine2.Tick(
      FFilter2.Tick(temp + (FDelayLine2.LastOutput * 0.7)));
   end
  else
   begin // No damping hack after 1 period.
    FLoopgain := 0.999;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    FLastOutput := FDelayLine.Tick(
      FFilter.Tick(temp + (FDelayLine.LastOutput * FLoopGain)));
    // Calculate 2nd delay just like the 1st.
    FLastOutput := FLastOutput + FDelayLine2.Tick(
      FFilter2.Tick(temp + (FDelayLine2.LastOutput * FLoopGain)));
   end;

  FLastOutput := FLastOutput * 0.3;
  Result := FLastOutput;
end;

procedure TStkMandolin.ControlChange(const Number: Integer; const Value: Single);
var
  norm : Single;
begin
  norm := Limit(Value, 0, 1);

  if (Number = CMidiBodySize) then // 2
    SetBodySize(norm * 2.0)
  else if (Number = CMidiPickPosition) then // 4
    PluckPosition := norm
  else if (Number = CMidiStringDamping) then // 11
    BaseLoopGain := 0.97 + (norm * 0.03)
  else if (Number = CMidiStringDetune) then // 1
    Detune := 1.0 - (norm * 0.1)
  else if (Number = CMidiAfterTouchCont) then // 128
    FMic := round(norm * 11.0);
end;

end.
