unit UWaveFormVoice;

interface

uses DAV_Types,UWaveFormOscillator,UXPluginBase,XMoogFilter,USampleValue;

type IPluginRompler = interface
  function GetSound(osc:integer): EWaveSound;
  function GetOscFrequency(osc:integer;pitch: single): single;
  function GetOscLevel(osc:integer): single;
  function GetGlide:single;
  procedure GetConvertedVCA(VAR A,D,S,R,gain:single);
  procedure OnVoiceDone(Sender: TObject);
  function GetLevel:single;
  function GetCutoff(pitch:single):single;
end;
  TADSRStage = (adsrAttack, adsrDecay, adsrSustain, adsrRelease);
type TXWaveVoice = class (TXVoice)
private
  FNoteLevel  : Single;
  FADSRStageVCA: TADSRStage;
  FADSRGainVCA : Single;
  FADSRStageVCF: TADSRStage;
  FADSRGainVCF : Single;
  FFilter : TMoogFilter;

  FReleaseQuickADSRRange:Single;

  FOscillator: array[0..2] of TWaveFormOscillator;
  FPluginRompler:IPluginRompler;
  Fstartpitch:integer;
  FTicks:integer;
  function GetOscFrequency(osc:integer;VAR currPitch:single): single;
  procedure ReleaseQuick; override;
    procedure ProcessADSR(var FADSRStage: TADSRStage; var FADSRGain: Single; A,
      D, S, R: Single);
public
  procedure NoteOn(pitch, startpitch: integer; Amplitude: Single); override;
  constructor Create(Xplugin:TXPluginBase;XPluginRompler:IPluginRompler);
  function Process: TSampleValue; override;
end;

implementation

{ TWaveFormVoice }

constructor TXWaveVoice.Create(Xplugin:TXPluginBase;XPluginRompler:IPluginRompler);
VAR i:integer;
begin
  inherited Create(Xplugin);
  FPluginRompler:=XPluginRompler;
  for i:=0 to 2 do Foscillator[i]:=TWaveFormOscillator.Create(Samplerate);
  FFilter:=TMoogFilter.Create(SampleRate);
end;

function TXWaveVoice.GetOscFrequency(osc:integer;VAR currPitch:single):single;
VAR depth,ratio:single;
    spitch,timeToGlide:single;
begin
  if CanGlide then
  begin
    depth:=FPluginRompler.GetGlide;
    if depth < 1/128 then // No Glide...
      ratio:=1
    else
    begin
      timeToGlide:=abs(Pitch-FStartPitch) * depth / 12; // in Seconds...
      ratio:=FTicks / (timeToGlide * SampleRate);
      if ratio>=1 then ratio:=1;
    end;
  end
  else ratio:=1;
  spitch:=Pitch*ratio+FStartPitch*(1-ratio);
  currPitch:=spitch;
  result:=FPluginRompler.GetOscFrequency(osc,spitch);
end;

procedure TXWaveVoice.ProcessADSR(VAR FADSRStage:TADSRStage; VAR FADSRGain:Single; A,D,S,R:Single);
begin
  if Released then FADSRStage:=adsrRelease;
 case FADSRStage of
  adsrAttack  : begin
                 FADSRGain := FADSRGain + A * (1 - FADSRGain);
                 if FADSRGain > 0.999
                  then FADSRStage := adsrDecay;
                end;
  adsrDecay   : begin
                 FADSRGain := FADSRGain - D * (FADSRGain - S);
                 if FADSRGain < S
                  then
                  begin
                    FADSRStage := adsrSustain;
                    FADSRGain:=S;
                  end;
                end;
  adsrSustain : begin
                  FADSRGain := S;
                  // make sure ADSRGAin <> 0, because the note will be ended, even if you slide sustain up
                end;
  adsrRelease : begin
                 FADSRGain := FADSRGain - R * FADSRGain;
                 if FADSRGain < 0.001
                  then FADSRGain := 0;
                end;
 end;
end;


function TXWaveVoice.Process:TSampleValue;
VAR osc:integer;
    gain,t,vcf,vca,A,D,S,R,rate,currPitch:single;
begin
  inc(FTicks);
  result.zero;
  for osc:=0 to 2 do
  begin
    FOscillator[osc].Frequency:=GetOscFrequency(osc,currPitch);
    FOscillator[osc].WaveSound:=FPluginRompler.GetSound(osc);
    result.add(Foscillator[osc].Process*FPluginRompler.GetOscLevel(osc));
  end;
  // VCA
  if IsQuickReleasing then
  begin
    FReleaseQuickADSRRange:=FReleaseQuickADSRRange-0.0003;
//    if FReleaseQuickADSRRange<0 then FReleaseQuickADSRRange:=0;
    FADSRGainVCA:=FReleaseQuickADSRRange;
    gain:=1;
  end
  else
  begin
    FPluginRompler.GetConvertedVCA(A,D,S,R,gain);
    ProcessADSR(FADSRStageVCA,FADSRGainVCA,A,D,S,R);
  end;
  vca:=FADSRGainVCA*gain;
  if (FADSRGainVCA <= 0) then
  begin
    result.zero;
    FPluginRompler.OnVoiceDone(self);
    exit;
  end;
  FFilter.Cutoff:=FPluginRompler.GetCutoff(currPitch);
  FFilter.Resonance:=0;
  result:=FFilter.Process(result);
  result.mul(vca*FPluginRompler.GetLevel*FNoteLevel);

end;

procedure TXWaveVoice.ReleaseQuick;
begin
  inherited;
  FReleaseQuickADSRRange := FADSRGainVCA;
end;


procedure TXWaveVoice.NoteOn(pitch,startpitch:integer; Amplitude: Single);
VAR i:integer;
begin
  inherited;
  FADSRStageVCA  := adsrAttack;
  FADSRGainVCA   := 0;
  FADSRStageVCF  := adsrAttack;
  FADSRGainVCF   := 0;
  FTicks    := 0;
  FStartPitch:=startpitch;
  FNoteLevel := Amplitude;
  for i:=0 to 2 do Foscillator[i].Reset;
end;


end.
