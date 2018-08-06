unit USyntorchestraVoice;

interface

uses DAV_Types,UWaveformOscillator,UXPluginBase,XMoogFilter,USampleValue;

type IPluginSyntorchestra = interface
  function GetSound(osc:integer): EWaveSound;
  function GetOscFrequency(osc,sub:integer;pitch: single): single;
  function GetOscLevel(osc:integer): single;
  function GetGlide:single;
  procedure GetConvertedVCA(VAR A,D,S,R,gain:single);
  procedure OnVoiceDone(Sender: TObject);
  function GetLFOValue:single;
  procedure GetLFODelay(VAR enabled: boolean;VAR rate:single);
  function GetVibrato(osc:integer):single;
  procedure GetConvertedVCF(osc:integer;var A, D, S, R, gain: single);
  function GetWhaWha(osc:integer):single;
  function GetCutoff(osc:integer;pitch:single):single;
  function GetResonance(osc:integer):single;
end;
  TADSRStage = (adsrAttack, adsrDecay, adsrSustain, adsrRelease);
type TSyntorchestraVoice = class (TXVoice)
private
  FNoteLevel  : Single;
  FADSRStageVCA: TADSRStage;
  FADSRGainVCA : Single;
  FADSRStageVCF: array[0..1] of TADSRStage;
  FADSRGainVCF : array[0..1] of Single;
  FFilter : array[0..1] of TMoogFilter;

  FReleaseQuickADSRRange:Single;

  FOscillator: array[0..3] of TWaveformOscillator;
  FPluginSyntorchestra:IPluginSyntorchestra;
  Fstartpitch:integer;
  FTicks:integer;
  function GetOscFrequency(osc,sub:integer;VAR currPitch:single): single;
  procedure ReleaseQuick; override;
  procedure ProcessADSR(var FADSRStage: TADSRStage; var FADSRGain: Single; A, D, S, R: Single);
    function LFODelay(enabled: boolean; value, delay: single): single;
public
  procedure NoteOn(pitch, startpitch: integer; Amplitude: Single); override;
  constructor Create(Xplugin:TXPluginBase;XPluginSyntorchestra:IPluginSyntorchestra);
  function Process: TSampleValue; override;
end;

implementation

{ TWaveFormVoice }

constructor TSyntorchestraVoice.Create(Xplugin:TXPluginBase;XPluginSyntorchestra:IPluginSyntorchestra);
VAR i:integer;
begin
  inherited Create(Xplugin);
  FPluginSyntorchestra:=XPluginSyntorchestra;
  for i:=0 to 3 do Foscillator[i]:=TWaveFormOscillator.Create(Samplerate);
  for i:=0 to 1 do FFilter[i]:=TMoogFilter.Create(SampleRate);
end;

function TSyntorchestraVoice.GetOscFrequency(osc,sub:integer;VAR currPitch:single):single;
VAR depth,ratio:single;
    spitch,timeToGlide:single;
begin
  if CanGlide and (osc=1) then
  begin
    depth:=FPluginSyntorchestra.GetGlide;
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
  result:=FPluginSyntorchestra.GetOscFrequency(osc,sub,spitch);
end;

procedure TSyntorchestraVoice.ProcessADSR(VAR FADSRStage:TADSRStage; VAR FADSRGain:Single; A,D,S,R:Single);
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

function TSyntorchestraVoice.LFODelay(enabled:boolean;value:single;delay:single):single;
VAR t:single;
begin
  if (not enabled) or (delay<0.01) then result:=value
  else
  begin
    // maximum delay time = 5 seconds
    t:= FTicks * SampleReci * 0.2 / delay;
    if t>1 then t:=1;
    result:=t*value;
  end;
end;

function TSyntorchestraVoice.Process:TSampleValue;
VAR j,iosc,ilfo:integer;
    wha,vib,gain,t,vcf,vca,A,D,S,R,rate,currPitch:single;
    lfo: single;
    enabled:boolean;
    res,sample:TSampleValue;

begin
  inc(FTicks);
  lfo:=FPluginSyntorchestra.GetLFOValue;
  FPluginSyntorchestra.GetLFODelay(enabled,rate);
  lfo:=LFODelay(enabled,lfo,rate);
  result.zero;
  for iosc:=0 to 1 do
  begin
    res.zero;
    vib:=FPluginSyntorchestra.GetVibrato(iosc) / 50;
    t:=lfo*vib;
    for j:=0 to 1 do
    begin
      FOscillator[2*iosc+j].Frequency:=GetOscFrequency(iosc,j,currPitch)*(1+t);
      FOscillator[2*iosc+j].WaveSound:=FPluginSyntorchestra.GetSound(iosc);
      sample[j]:=Foscillator[2*iosc+j].Process*FPluginSyntorchestra.GetOscLevel(iosc)/2 ;
      res.add(sample);
    end;
    // VCF
    FPluginSyntorchestra.GetConvertedVCF(iosc,A,D,S,R,gain);
    ProcessADSR(FADSRStageVCF[iosc],FADSRGainVCF[iosc],A,D,S,R);
    vcf:=FADSRGainVCF[iosc]*gain;
    // VCF LFO & Filter
    wha:=FPluginSyntorchestra.GetWhaWha(iosc) / 2;
    t:=lfo*wha;
    FFilter[iosc].Cutoff:=FPluginSyntorchestra.GetCutoff(iosc,currPitch)*vcf*(1+t);
    FFilter[iosc].Resonance:=FPluginSyntorchestra.GetResonance(iosc);
    res:=FFilter[iosc].Process(res);
    result.add(res);
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
    FPluginSyntorchestra.GetConvertedVCA(A,D,S,R,gain);
    ProcessADSR(FADSRStageVCA,FADSRGainVCA,A,D,S,R);
  end;
  vca:=FADSRGainVCA*gain;
  if (FADSRGainVCA <= 0) then
  begin
    result.zero;
    FPluginSyntorchestra.OnVoiceDone(self);
    exit;
  end;

  result.mul(vca*FNoteLevel);

end;

procedure TSyntorchestraVoice.ReleaseQuick;
begin
  inherited;
  FReleaseQuickADSRRange := FADSRGainVCA;
end;


procedure TSyntorchestraVoice.NoteOn(pitch,startpitch:integer; Amplitude: Single);
VAR i:integer;
begin
  inherited;
  FADSRStageVCA  := adsrAttack;
  FADSRGainVCA   := 0;
  for i:=0 to 1 do
  begin
    FADSRStageVCF[i]  := adsrAttack;
    FADSRGainVCF[i]   := 0;
  end;
  FTicks    := 0;
  FStartPitch:=startpitch;
  FNoteLevel := Amplitude;
  for i:=0 to 3 do Foscillator[i].Reset;
end;


end.
