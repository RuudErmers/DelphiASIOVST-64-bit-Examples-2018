unit XPluginRESyntorchestra;

{ This unit
- implements the real plugin
- sets the view form
- communicates with the view form }

interface

uses UXPluginBase,Forms,URMCVSTView,DAV_VSTModule,USyntorchestraVoice,XOscillator, UWaveFormOscillator, USampleValue,XEffectsBase;

type TVCF = record
           Attack  : Single;
           Decay   : Single;
           Release : Single;
           Sustain : Single;
           ModDepth: single;
           ModTarget:integer;
           Cutoff,Resonance:Single;
           Level:single;
        end;
  TVCA = record
           Attack  : Single;
           Decay   : Single;
           Release : Single;
           Sustain : Single;
           ModDepth: single;
           ModTarget:integer;
           Level:single;
         end;
  TLFO =   record
             Wave   : integer;
             speed   : single;
             delayEnable:boolean;
             delay:single;
             value: single; // current lfo value
           end;

type TXPluginRESyntorchestra = class(TXPluginBase,IPluginSyntorchestra)
  private
    FmonoSound,FPolySound:EWaveSound;
    FGlide,FVibrato,FResonance,FDetune,FSpread:single;
    FCutoff:array[0..1] of single;
    FVCA:TVCA;
    FVCF:array[0..1] of TVCF;
    FLFO:    TLFO;
    FLFOOsc: TXOscillator;
    FOscLevel:array[0..1] of single;
    FVibratoDig:array[0..1] of boolean;
    FVibratoDelay,FwhaWha,FDecayDig:boolean;
    FSoffiato,FDecay,FSemi:integer;
    procedure SetMonoSound(value: integer);
    procedure SetPolySound(value: integer);
    procedure SetMonoSemi(value: integer);
    procedure UpdateADSRs;
    function ConvertADSR(x, MAXSECONDS: single): single;
protected
    function paramtopcc(param: integer): integer;override;
    function pcctoparam(pcc: integer): integer;override;
    function GetOscFrequency(osc,sub:integer;pitch: single): single;
    function GetGlide:single;
    procedure GetConvertedVCA(VAR A,D,S,R,gain:single);
    procedure OnParameterChanged(Sender: TObject; const Index,Value: Integer);override;
    function GetOscLevel(osc:integer): single;
    function GetLFOValue:single;
    procedure GetLFODelay(VAR enabled: boolean;VAR rate:single);
    function GetVibrato(osc:integer):single;
    procedure GetConvertedVCF(osc:integer;var A, D, S, R, gain: single);
    function GetWhaWha(osc:integer):single;
    function GetCutoff(osc:integer;pitch:single):single;
    function GetResonance(osc:integer):single;
    function GetSound(osc:integer):EWaveSound;
    function CreateEffectsSettings(paramStart: integer;createdefault:boolean=true): TEffectsSettings;override;
public
    function ParamCount:integer;override;
    function CreateGUIForm(form:TForm):TRMCVSTView;override;
    function getSample:TSampleValue;override;
    function CreateVoice:TXVoice;override;
    procedure AddParameters;override;
    constructor create(vstModule:TVSTModule;Samplerate:single);override;

end;

implementation

uses Math,DAV_Common,UVirtCC,XSynthModule, URMCSyntorchestraView,CodeSiteLogging, SysUtils, XEffects64;

const recip127 = 1/127;

function TXPluginRESyntorchestra.CreateVoice: TXVoice;
begin
  result:=TSyntorchestraVoice.Create(self,self);
end;

function TXPluginRESyntorchestra.getSample:TSampleValue;
VAR dummy:boolean;
begin
  FLFO.value:=FLFOOsc.Process(dummy);
  result:=inherited;
end;

procedure TXPluginRESyntorchestra.OnParameterChanged(Sender: TObject; const Index,  Value: Integer);
function   sZeroOne:single;    begin result:=Value * Recip127; end;
function   bSelection:boolean; begin result:=Value <> 0; end;
function   sLFO:single;        begin result:=0.1*exp(0.0598*Value); end;   // between 0.1 and 20

begin
  CodeSite.Send('Changed Parameter:'+inttostr(index)+' - '+ inttostr(Value));

  case index of
    XRESYNTORCHESTRAPARM_VIBRATO:        FVibrato:=sZeroOne;
    XRESYNTORCHESTRAPARM_MONOGLIDE:      FGlide:=sZeroOne;
    XRESYNTORCHESTRAPARM_MONOVOLUME:     FOscLevel[1]:=sZeroOne;
    XRESYNTORCHESTRAPARM_POLYVOLUME:     FOscLevel[0]:=sZeroOne;
    XRESYNTORCHESTRAPARM_RESONANCE:      FResonance:=sZeroOne;
    XRESYNTORCHESTRAPARM_MONOBRILLIANCE: FCutoff[1]:=20+sZeroOne*20000;
    XRESYNTORCHESTRAPARM_POLYBRILLIANCE: FCutoff[0]:=20+sZeroOne*20000;
    XRESYNTORCHESTRAPARM_SOFFIATO:       begin FSoffiato:=value; UpdateADSRs; end;
    XRESYNTORCHESTRAPARM_DECAY:          begin FDecay:=value; UpdateADSRs; end;

    XRESYNTORCHESTRAPARM_POLYVIBDIG:     FVibratoDig[0]:=bSelection;
    XRESYNTORCHESTRAPARM_MONOVIBDIG:     FVibratoDig[1]:=bSelection;
    XRESYNTORCHESTRAPARM_VIBDELAYDIG:    FVibratoDelay :=bSelection;
    XRESYNTORCHESTRAPARM_POLYSOUND:      SetPolySound(value);
    XRESYNTORCHESTRAPARM_MONOSOUND:      SetMonoSound(value);
    XRESYNTORCHESTRAPARM_LFO1SPEED:      FLFOOsc.Frequency:=sLFO;
    XRESYNTORCHESTRAPARM_LFO2SPEED:      FSpread:=sZeroOne;
    XRESYNTORCHESTRAPARM_WHAWHADIG:      FWhaWha:=bSelection;
    XRESYNTORCHESTRAPARM_DECAYDIG:       begin FDecayDig:=bSelection;UpdateADSRs; end;
    XRESYNTORCHESTRAPARM_PITCHSEMI:      SetMonoSemi(value);
    XRESYNTORCHESTRAPARM_DETUNE:         Fdetune:=sZeroOne;

  end;
  inherited;
end;

procedure TXPluginRESyntorchestra.SetPolySound(value:integer);
const polysounds:array[0..3] of EWaveSound = (wvSynthPolyTrombone,wvSynthPolyTrumpet,wvSynthPolyPiano,wvSynthPolyViola);
begin
  FpolySound:=polysounds[value DIV 32];
  UpdateADSRs;
end;

procedure TXPluginRESyntorchestra.SetMonoSemi(value:integer);
const semis:array[0..3] of integer = (0,-3,-5,-8);
begin
  FSemi:=semis[value DIV 32];
end;

procedure TXPluginRESyntorchestra.SetMonoSound(value:integer);
const monosounds:array[0..8] of EWaveSound = ( wvSynthMonoTuba,wvSynthMonoTrombone,wvSynthMonoTrumpet,wvSynthMonoBaritoneSax,wvSynthMonoAltoSax,wvSynthMonoBassFlute,wvSynthMonoFlute,wvSynthMonoPiccolo,    wvSynthMonoViolin);
begin
  FMonoSound:=monosounds[value*9 DIV 128];
end;

const ADSR_MUL_A = 4;
const ADSR_MUL_D = 4;
const ADSR_MUL_R = 8;

function TXPluginRESyntorchestra.ConvertADSR(x:single;MAXSECONDS:single):single;
Var T:single;
const
      VMIN = 0.005;
      function f(x:single):single;
      begin
        if x<0.5 then result:=x*x
                 else result:=0.25+1.5*(x-0.5);
      end;
begin
  x:=f(x);
  T := (x+0.001) * MAXSECONDS * FSampleRate;
  result:= 1 - Power(VMIN,1/T);
end;

function TXPluginRESyntorchestra.ParamCount: integer;
begin
  result:= ParamCountXRESYNTORCHESTRA;
end;

procedure TXPluginRESyntorchestra.AddParameters;
begin
  AddParameter(XRESYNTORCHESTRAPARM_VIBRATO,'Vibrato',0,100,0,'%');
  AddParameter(XRESYNTORCHESTRAPARM_MONOGLIDE,'Glide',0,100,0,'%');
  AddParameter(XRESYNTORCHESTRAPARM_MONOVOLUME,'Mono Volume',0,100,80,'%');
  AddParameter(XRESYNTORCHESTRAPARM_POLYVOLUME,'Poly Volume',0,100,80,'%');
  AddParameter(XRESYNTORCHESTRAPARM_RESONANCE,'Mono Resonance (sys)',0,100,0,'%');
  AddParameter(XRESYNTORCHESTRAPARM_MONOBRILLIANCE,'Mono Brilliance',20,20000,10000,'Hz','__CUTOFF');
  AddParameter(XRESYNTORCHESTRAPARM_POLYBRILLIANCE,'Poly Brilliance',20,20000,10000,'Hz','__CUTOFF');
  AddParameter(XRESYNTORCHESTRAPARM_SOFFIATO,'Soffiato',0,100,20,'%');
  AddParameter(XRESYNTORCHESTRAPARM_DECAY,'Decay',0,100,40,'%');
  AddParameter(XRESYNTORCHESTRAPARM_POLYVIBDIG,'Poly Vibrato',0,1,0,'','Off;On');
  AddParameter(XRESYNTORCHESTRAPARM_MONOVIBDIG,'Mono Vibrato',0,1,0,'','Off;On');
  AddParameter(XRESYNTORCHESTRAPARM_VIBDELAYDIG,'Vibrato Delay',0,1,0,'','Off;On');
  AddParameter(XRESYNTORCHESTRAPARM_POLYSOUND,'Poly Sound',0,3,0,'','Sound 0;Sound 1;Sound 2;Sound 3;');
  AddParameter(XRESYNTORCHESTRAPARM_MONOSOUND,'Mono Sound',0,3,0,'','Sound 0;Sound 1;Sound 2;Sound 3;');

  AddParameter(XRESYNTORCHESTRAPARM_LFO1SPEED,'LFO1 Speed (sys)',0,100,40,'%');
  AddParameter(XRESYNTORCHESTRAPARM_LFO2SPEED,'Stereo Spread',0,100,35,'%');
  AddParameter(XRESYNTORCHESTRAPARM_WHAWHADIG,'Wha Wha',0,1,0,'','Off;On');
  AddParameter(XRESYNTORCHESTRAPARM_DECAYDIG,'Filter Mode',0,1,0,'','Soffiato;Decay');
  AddParameter(XRESYNTORCHESTRAPARM_PITCHSEMI,'Pitch Semi',0,3,0,'','Off;-3;-5;-6');
  AddParameter(XRESYNTORCHESTRAPARM_DETUNE,'Pitch Detune',0,100,50,'%');
  inherited;
end;


constructor TXPluginRESyntorchestra.create(vstModule: TVSTModule; Samplerate: single);
VAR i,lfo:integer;
begin
  inherited;
  FLFOOsc:=TXOscillator.Create(SampleRate,true);
  FLFOOsc.WaveShape:=wsSine;
  FPoly:=true;
end;

function TXPluginRESyntorchestra.CreateEffectsSettings(paramStart: integer;createdefault:boolean): TEffectsSettings;
begin
  result:=inherited CreateEffectsSettings(XRESYNTORCHESTRAPARM_DELAYAMOUNT,true);
  result.factory:=TEffects64;
end;

function TXPluginRESyntorchestra.CreateGUIForm(form: TForm): TRMCVSTView;
begin
  result:= TRMCSyntorchestraView.Create(form);
end;

procedure TXPluginRESyntorchestra.UpdateADSRs;
VAR A,D,S,R:integer;
begin
  // osc0 -> Poly
(*
  A:=10;
  D:=0;
  S:=127;
  R:=20;
  with FVCA do
  begin
    Attack:= ConvertADSR(A * Recip127,ADSR_MUL_A);
    Decay:= ConvertADSR(D * Recip127,ADSR_MUL_D);
    Sustain:= S * Recip127;
    Release:= ConvertADSR(R * Recip127,ADSR_MUL_R);
  end;

  with FVCF[0] do
  begin
    Attack:= ConvertADSR(A * Recip127,ADSR_MUL_A);
    Decay:= ConvertADSR(D * Recip127,ADSR_MUL_D);
    Sustain:= S * Recip127;
    Release:= ConvertADSR(R * Recip127,ADSR_MUL_R);
  end;
  if FDecayDig then
  begin
    A:=10;
    D:=FDecay;
    S:=60;
    R:=FDecay;
  end
  else
  begin
    A:=FSoffiato;
    D:=FDecay;
    S:=127;
    R:=FDecay;
  end;

  with FVCF[1] do
  begin
    Attack:= ConvertADSR(A * Recip127,ADSR_MUL_A);
    Decay:= ConvertADSR(D * Recip127,ADSR_MUL_D);
    Sustain:= S * Recip127;
    Release:= ConvertADSR(R * Recip127,ADSR_MUL_R);
  end;
  *)

  A:=FSoffiato;
  D:=FDecay;
  S:=127;
  R:=FDecay;
  with FVCA do
  begin
    Attack:= ConvertADSR(A * Recip127,ADSR_MUL_A);
    Decay:= ConvertADSR(D * Recip127,ADSR_MUL_D);
    Sustain:= S * Recip127;
    Release:= ConvertADSR(R * Recip127,ADSR_MUL_R);
  end;
  with FVCF[0] do
  begin
    Attack:= ConvertADSR(A * Recip127,ADSR_MUL_A);
    Decay:= ConvertADSR(D * Recip127,ADSR_MUL_D);
    Sustain:= S * Recip127;
    Release:= ConvertADSR(R * Recip127,ADSR_MUL_R);
  end;
  FVCF[1]:=FVCF[0];
end;

procedure TXPluginRESyntorchestra.GetConvertedVCA(var A, D, S, R, gain: single);
begin
  with FVCA do
  begin
     A  := Attack;
     D  := Decay;
     S  := Sustain;
     R  := Release;
  end;
  gain:=1;
  gain:=gain*gain*gain;
end;

procedure TXPluginRESyntorchestra.GetConvertedVCF(osc: integer; var A, D, S, R,  gain: single);
begin
  with FVCF[osc] do
  begin
     A  := Attack;
     D  := Decay;
     S  := Sustain;
     R  := Release;
  end;
  gain:=1;
  gain:=gain*gain*gain;
end;

function TXPluginRESyntorchestra.GetSound(osc: integer): EWaveSound;
begin
  if osc=0 then result:=FPolySound else result:=FMonoSound
end;

function TXPluginRESyntorchestra.GetVibrato(osc: integer): single;
begin
  result:=FVibrato*ord(FVibratoDig[osc]);
end;

function TXPluginRESyntorchestra.GetWhaWha(osc: integer): single;
begin
  result:=0;
  if (osc=1) and FwhaWha then result:=FVibrato;
end;


function TXPluginRESyntorchestra.GetCutoff(osc:integer;pitch: single): single;
begin
  result:=FCutoff[osc];
end;

function TXPluginRESyntorchestra.GetGlide: single;
begin
  result:=FGlide;
end;


procedure TXPluginRESyntorchestra.GetLFODelay(var enabled: boolean; var rate: single);
begin
  enabled:=FVibratoDelay;
  rate:=0.3;
end;

function TXPluginRESyntorchestra.GetLFOValue: single;
begin
  result:=FLFO.value;
end;

function TXPluginRESyntorchestra.GetOscFrequency(osc,sub:integer;pitch: single): single;
VAR f0,f1,ratio:single;
begin
  pitch:=pitch+16*(FPitchBend-0.5);
  if pitch<12 then pitch:=12;
  if pitch>108 then pitch:=108;
  if (osc=1) then pitch:=pitch+FSemi+2*(FDetune-0.5);
  if sub=1 then pitch:=pitch+FSPread*GetVibrato(osc)/15;
  f0:=pitchToFrequency[trunc(pitch)];
  f1:=pitchToFrequency[trunc(pitch)+1];
  ratio:=pitch-trunc(pitch);
  result:=(1-ratio)*f0+ratio*f1;
end;


function TXPluginRESyntorchestra.GetOscLevel(osc: integer): single;
begin
  result:=FOscLevel[osc];
end;

function TXPluginRESyntorchestra.GetResonance(osc: integer): single;
begin
  if osc=0 then result:=0
           else result:=FResonance;
end;

function TXPluginRESyntorchestra.paramtopcc(param: integer): integer;
begin
  result:=ParamToPhysXRESyntorchestra[param];
end;

function TXPluginRESyntorchestra.pcctoparam(pcc: integer): integer;
begin
  result:=PhysToParamXRESyntorchestra[pcc];
end;

end.

