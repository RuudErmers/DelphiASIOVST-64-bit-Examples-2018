unit DAV_StkRagamat;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkReverb, DAV_StkJCReverb,
  DAV_StkDrone, DAV_StkSitar, DAV_StkTabla, DAV_StkVoiceDrum;

const
  ragaUp: array[0..1, 0..11] of Integer =
    ((57, 60, 62, 64, 65, 68, 69, 71, 72, 76, 77, 81),
    (52, 54, 55, 57, 59, 60, 63, 64, 66, 67, 71, 72));
  ragaDown: array[0..1, 0..11] of Integer =
    ((57, 60, 62, 64, 65, 67, 69, 71, 72, 76, 79, 81),
    (48, 52, 53, 55, 57, 59, 60, 64, 66, 68, 70, 72));

type
  TStkRagamat = class(TStkControlableInstrument)
  protected
    FPport      : Integer;
    FRagaStep   : Integer;
    FRagaPoint  : Integer;
    FVoiceNote  : Integer;
    FT60        : Single;
    FDrone_prob : Single;
    FNote_prob  : Single;
    FDrum_prob  : Single;
    FVoic_prob  : Single;
    FDroneFreqs : array[0..2] of Single;
    FTempo      : Integer;
    FCounter    : Integer;
    FKey        : Integer;
    FDrones     : array[0..2] of TStkDrone;
    FTabla      : TStkTabla;
    FVoicdrums  : TStkVoicedrum;
    FSitar      : TStkSitar;
    FReverbs    : array[0..1] of TStkJCReverb;
    procedure SetFrequency(const Value: Single); override;
    function GetFrequency: Single; override;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure NoteOn(const Instrument, Amplitude: Single); override;
    procedure ControlChange(const Number: Integer; const Value: Single); override;
    procedure Tick(var i1, i2: Single);
  end;

implementation

uses
  SysUtils;

constructor TStkRagamat.Create;
begin
  inherited Create(SampleRate);
  FT60 := 4.0;  // in seconds
  FDrone_prob := 0.01;
  FNote_prob := 0.0;
  FDrum_prob := 0.0;
  FVoic_prob := 0.0;
  FDroneFreqs[0] := 55.0;
  FDroneFreqs[1] := 82.5;
  FDroneFreqs[2] := 220.0;
  FTempo := 3000;
  FCounter := 3000;
  FKey := 0;
  FRagaPoint := 6;
  FPport := -1;

  FDrones[0] := TStkDrone.Create(SampleRate, 50.0);
  FDrones[1] := TStkDrone.Create(SampleRate, 50.0);
  FDrones[2] := TStkDrone.Create(SampleRate, 50.0);
  FSitar := TStkSitar.Create(SampleRate, 50.0);
  FVoicdrums := TStkVoicedrum.Create(SampleRate);
  FTabla := TStkTabla.Create(SampleRate);

  FReverbs[0] := TStkJCReverb.Create(SampleRate, FT60);
  FReverbs[0].EffectMix := 0.5;
  FReverbs[1] := TStkJCReverb.Create(SampleRate, 2.0);
  FReverbs[1].EffectMix := 0.2;

  FDrones[0].noteOn(FDroneFreqs[0], 0.1);
  FDrones[1].noteOn(FDroneFreqs[1], 0.1);
  FDrones[2].noteOn(FDroneFreqs[2], 0.1);

{ Single outSamples[2];
 for (i := 0 to SampleRate - 1 do // warm everybody up a little
  begin
   outSamples[0] := FReverbs[0].Tick(FDrones[0].Tick + FDrones[2].Tick);
   outSamples[1] := FReverbs[1].Tick(1.5 * FDrones[1].Tick);
   output.Tick(outSamples);
 end;
}
end;

destructor TStkRagamat.Destroy;
begin
 FreeAndNil(FDrones[0]);
 FreeAndNil(FDrones[1]);
 FreeAndNil(FDrones[2]);
 FreeAndNil(FSitar);
 FreeAndNil(FTabla);
 FreeAndNil(FVoicdrums);
 FreeAndNil(FReverbs[0]);
 FreeAndNil(FReverbs[1]);
 inherited Destroy;
end;

procedure TStkRagamat.Tick(var i1, i2: Single);
var
  temp, rateScaler: Single;
begin
{
 i1 := FReverbs[0].Tick(FSitar.Tick);
 i2 := i1;
 FCounter := FCounter - 1;
 if (FCounter <= 0) then
  begin
   rateScaler := 22050.0 * FSampleRateInv;
   FCounter := round(FTempo / rateScaler);
   FSitar.noteOn(Midi2Pitch[ragaUp[FKey][FRagaPoint]], 0.5 + random * 0.3)
  end;
 exit;
 //x}

 i1 := FReverbs[0].Tick(FDrones[0].Tick + FDrones[2].Tick + FSitar.Tick);
 i2 := FReverbs[1].Tick(1.5 * FDrones[1].Tick + 0.5 * FVoicdrums.Tick + 0.5 * FTabla.Tick);
 // mix a little left to right and back
 temp := i1;
 i1 := i1 + 0.3 * i2;
 i2 := i2 + 0.3 * temp;
 FCounter := FCounter - 1;
 if (FCounter <= 0) then
  begin
   rateScaler := 22050.0 * FSampleRateInv;
   FCounter := round(FTempo / rateScaler);
   if (random < FDrone_prob) then FDrones[0].noteOn(FDroneFreqs[0] + random(10), 0.1);
   if (random < FDrone_prob) then FDrones[1].noteOn(FDroneFreqs[1] + random(10), 0.1);
   if (random < FDrone_prob) then FDrones[2].noteOn(FDroneFreqs[2] + random(10), 0.1);
   if (random < FNote_prob) then
    begin
     temp := random;
     if (temp < 0.1) then FRagaStep := 0
     else if (temp < 0.5) then FRagaStep := 1
     else FRagaStep := -1;
     FRagaPoint := FRagaPoint + FRagaStep;
     if (FRagaPoint < 0) then FRagaPoint := FRagaPoint - (2 * FRagaStep);
     if (FRagaPoint > 11) then FRagaPoint := 11;
     if (FRagaStep > 0)
      then FSitar.NoteOn(Midi2Pitch[ragaUp[FKey][FRagaPoint]], 0.05 + Random * 0.3)
      else FSitar.NoteOn(Midi2Pitch[ragaDown[FKey][FRagaPoint]], 0.05 + random * 0.3);
    end;
   if (random < FVoic_prob) then
    begin
     FVoiceNote := Random(11);
     FVoicdrums.noteOn(FVoiceNote, 0.3 + (0.4 * FDrum_prob) + random * 0.3 * FVoic_prob);
    end;
   if (random < FDrum_prob) then
    begin
     FVoiceNote := Random(CTablaNumWaves);
     FTabla.NoteOn(FVoiceNote, 0.2 + (0.2 * FDrum_prob) + random * 0.6 * FDrum_prob);
    end;
  end;
end;

procedure TStkRagamat.ControlChange(const Number: Integer; const Value: Single);
var
  norm: Single;
begin
 norm := Limit(Value, 0, 1);

 case number of
  CMidiBreath      : FDrone_prob := norm; // 2
  CMidiFootControl : FNote_prob := norm;  // 4
  CMidiModFrequency : FVoic_prob := norm; // 11

  CMidiModWheel : FDrum_prob := norm; // 1
  3: FTempo := round(11025 - (norm * 128 * 70)); // 3
  CMidiAfterTouchCont: // 128
   if norm < 0.5 then
    begin
     FKey := 1;
     FDroneFreqs[0] := 55.0;
     FDroneFreqs[1] := 82.5;
     FDroneFreqs[2] := 220.0;
    end else
    begin
     FKey := 0;
     FDroneFreqs[0] := 82.5;
     FDroneFreqs[1] := 123.5;
     FDroneFreqs[2] := 330.0;
    end;
 end;
end;

procedure TStkRagamat.NoteOn(const Instrument, Amplitude: Single);
begin
  FSitar.NoteOn(Instrument, Amplitude);
end;

procedure TStkRagamat.SetFrequency(const Value: Single);
begin
 inherited;
 // nothing in here yet
end;

function TStkRagamat.GetFrequency: Single;
begin
 result := 0;
end;

end.
