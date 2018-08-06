unit XSynthModelBase;
{ partial class which implements a Plugin Synth }

interface

uses XOscillator,XSynthVoice, Generics.Collections,Controls,Forms,URMCVSTView,DAV_VSTModule,DAV_Types,UIXSynthModel,Classes,UIXPlugin,DAV_VSTEffect,UXPluginBase, USampleValue;


type TOsc = record
           Wave   : integer;
           Level   : Single;
           ModDepth: single;
           ModTarget:integer;
           Foot    : integer;
           Semi    : integer;
           Detune  : single ;
           Sync    : boolean;
  end;
  TVCF = record
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
  TPWM = record
           ModDepth: single;
           ModTarget:integer;
         end;

    TXSynthModelBase = class(TXPluginBase,IXSynthModel)
private
    procedure ButtonClick(Sender: TObject);

protected
    FLevel  : Single;
    FOscs   : array [0..4] of TOsc;
    FVCF  : TVCF;
    FVCA  : TVCA;
    FRingLevel:single;
    FPWM: array[0..4] of TPWM;
    FFat:  single;
    FLFOs:    array [0..1] of TLFO;
    FLFOOscs: array [0..1] of TXOscillator;
    FLastFrequency: array[0..1] of single;
    FVCFKeyFollow,FGlide:single;
    FGlideEnable:boolean;
    function GetLFOSpeed(lfo:integer):single;
    function GetLFOWaveshape(lfo: integer): TWaveShape;virtual;
    function GetSync(osc:integer):boolean;
    function CanOscPulseWidth(osc:integer):boolean;virtual;
    procedure SetSampleRate(rate:single);override;
    function getSample: TSampleValue;override;

    function CreateVoice:TXVoice;override;

public
    constructor create(vstModule:TVSTModule;Samplerate:single);override;
    function GetOscWaveshape(osc:integer;VAR noisecolor:integer):TWaveShape;virtual;
    function GetOscFrequency(osc:integer;pitch:single):single;

    function GetCutoff(pitch:single):single;
    function GetResonance:single;
    function GetOscModDepth(osc,lfo:integer):single;
    function GetVCFModDepth(lfo:integer):single;
    function GetVCAModDepth(lfo:integer):single;
    function GetRingLevel:single;
    function GetOscLevel(osc:integer):single;
    procedure GetConvertedVCA(VAR A,D,S,R,gain:single);
    procedure GetConvertedVCF(var A, D, S, R, gain: single);
    function GetLevel:single;
    function GetLFOValue(lfo:integer):single;
    procedure GetLFODelay(lfo:integer;VAR enabled: boolean;VAR rate:single);
    function GetOscPulseWidth(osc:integer):single;
    function GetGlide:single;

end;

const recip127 = 1/127;
      NOISE_OSCILLATOR = 2;

implementation

{ TXSynthModel }

uses Math,DAV_Common,DAV_Approximations,XSynthModule,XSynthMainFrame,SysUtils,Windows;

function TXSynthModelBase.CanOscPulseWidth(osc: integer): boolean;
begin
 // const Waves:array[0..7] of TWaveShape = (wsSawUp,wsSquare,wsSine,wsTriangle,wsSquare,wsSquare,wsTriangle,wsNoise);
 // Saw, Square, Sine, Triangle, HPulse, Qpulse, TriSaw, Noise
  result:=Foscs[osc].Wave in [ 1,4,5 ];
end;

constructor TXSynthModelBase.create(vstModule:TVSTModule;SampleRate:single);
VAR lfo:integer;
begin
  inherited;
  FLevel := 1;
  for lfo:=0 to 1 do
  begin
    FLFOOscs[lfo]:=TXOscillator.Create(SampleRate,true);
    FLFOOscs[lfo].WaveShape:=wsNone;
  end;
end;

function TXSynthModelBase.CreateVoice: TXVoice;
begin
  result:=TXSynthVoice.Create(self,self); // ja, jammer {:
end;

function TXSynthModelBase.getSample: TSampleValue;
VAR lfo:integer;
    dummy:boolean;
begin
  for lfo:=0 to 1 do
  begin
    FLFOOscs[lfo].WaveShape:=GetLFOWaveshape(lfo);
    FLFOOscs[lfo].Frequency:=GetLFOSpeed(lfo);
    FLFOs[lfo].value:=FLFOOscs[lfo].Process(dummy);
  end;
  result:=inherited;
end;


procedure TXSynthModelBase.ButtonClick(Sender:TObject);
begin
  ShowEffects;
end;

procedure TXSynthModelBase.SetSampleRate(rate: single);
VAR lfo:integer;
begin
  inherited;
  if FLFOOscs[0] = NIL then exit;
  for lfo:=0 to 1 do
    FLFOOscs[lfo].SampleRate:=FSampleRate;
end;


function TXSynthModelBase.GetCutoff(pitch:single):single;
VAR t:single;
begin
  result:=FVCF.Cutoff;
  // pitch bepaalt bij FVCFKeyFollow de uiteindelijke cutoff
  t:= FVCFKeyFollow*(84-pitch) / 48;  // ongeveer tussen 0 en 1, komt niet zo nauw
  if t>1 then t:=1;
  result:=result*(1-0.7*t);
end;

function TXSynthModelBase.GetGlide: single;
begin
  result:=FGlide*ord(FGlideEnable);
end;

function TXSynthModelBase.GetResonance:single;
begin
  result:=FVCF.Resonance
end;

function TXSynthModelBase.GetRingLevel: single;
begin
  result:=FRingLevel;
end;

procedure TXSynthModelBase.GetConvertedVCA(var A, D, S, R,gain: single);
begin
  with FVCA do
  begin
     A  := Attack;
     D  := Decay;
     S  := Sustain;
     R  := Release;
  end;
  gain:=FVCA.Level;
  gain:=gain*gain*gain;
end;

procedure TXSynthModelBase.GetConvertedVCF(var A, D, S, R,gain: single);
begin
  with FVCF do
  begin
     A  := Attack;
     D  := Decay;
     S  := Sustain;
     R  := Release;
  end;
  gain:=FVCF.Level;
end;

function TXSynthModelBase.GetLevel: single;
begin
  result:=FLevel;
end;

procedure TXSynthModelBase.GetLFODelay(lfo:integer;var enabled: boolean; var rate: single);
begin
  enabled:=FLFOs[lfo].delayEnable;
  rate:=FLFOs[lfo].delay;
end;

function TXSynthModelBase.GetLFOSpeed(lfo: integer): single;
begin
  result:=FLFOs[lfo].speed;
end;

function TXSynthModelBase.GetLFOValue(lfo: integer): single;
begin
  result:=FLFOs[lfo].value;
end;

function TXSynthModelBase.GetOscModDepth(osc, lfo: integer): single;
begin
  if FOscs[osc].ModTarget and (1 shl lfo) <> 0 then
        result:=max(FOscs[osc].ModDepth,FModWheel)
  else result:=0;
end;

function TXSynthModelBase.GetOscPulseWidth(osc:integer): single;
VAR lfo:integer;
    t:single;
begin
  result:=0.5;
  if (osc <>NOISE_OSCILLATOR) and CanOscPulseWidth(osc) then
  begin
    if FPWM[osc].ModTarget = 0 then
      result:=0.5 - 0.45*FPWM[osc].ModDepth
    else
    begin
      t:=0;
      for lfo:=0 to 1 do
       if FPWM[osc].ModTarget and (1 shl lfo) <> 0 then
         t:=t+GetLFOValue(lfo) / 2;
      result:=0.5 + 0.45*FPWM[osc].ModDepth*t;
    end;
  end;
end;

function TXSynthModelBase.GetVCFModDepth(lfo: integer): single;
begin
  if FVCF.ModTarget and (1 shl lfo) <> 0 then
    result:=max(FVCF.ModDepth,FModWheel)
  else result:=0;
end;

function TXSynthModelBase.GetSync(osc: integer): boolean;
begin
  if osc<4 then result:=FOscs[osc].Sync
           else result:=false;
end;

function TXSynthModelBase.GetVCAModDepth(lfo: integer): single;
begin
  if FVCA.ModTarget and (1 shl lfo) <> 0 then
      result:=max(FVCA.ModDepth,FModWheel)
  else result:=0;
end;

function TXSynthModelBase.GetOscLevel(osc: integer): single;
begin
  result:=FOscs[osc].Level;
  result:=result*result;
end;

function TXSynthModelBase.GetOscFrequency(osc: integer; pitch: single): single;
VAR f0,f1,ratio:single;
const fatdivert:array[0..4] of single = (-0.5,0.5,0,0.25,-0.25);
const sign:array[0..4] of integer = (1,-1,0,-1,1);

begin
  if osc=NOISE_OSCILLATOR then begin result:=10000; exit; end;
  pitch:=pitch+FOscs[osc].Foot*12+FOscs[osc].Semi+Foscs[osc].Detune*sign[osc];
  if pitch<12 then pitch:=12;
  if pitch>108 then pitch:=108;
  pitch:=pitch + 16*(FPitchBend-0.5);
  pitch:=pitch+FFAT*fatdivert[osc];
  f0:=Midi2Pitch[trunc(pitch)];
  f1:=Midi2Pitch[trunc(pitch)+1];
  ratio:=pitch-trunc(pitch);
  result:=(1-ratio)*f0+ratio*f1;
end;


function TXSynthModelBase.GetOscWaveshape(osc: integer;VAR noisecolor:integer): TWaveShape;
//Saw,Square,Sine,Triangle,HPulse,Qpulse,TriSaw,Noise
const Waves:array[0..7] of TWaveShape = (wsSawUp,wsSquare,wsSine,wsTriangle,wsSquare,wsSquare,wsTriangle,wsNoise);
begin
  if osc=NOISE_OSCILLATOR then begin result:=wsNoise; noisecolor:=FOscs[osc].Wave; end
                          else begin result:=Waves[FOscs[osc].Wave];noisecolor:=127; end;
end;

function TXSynthModelBase.GetLFOWaveshape(lfo: integer): TWaveShape;
// Saw,Squ,Tri,Noise
begin
  case FLFOs[lfo].Wave of
    0:  result:=wsSawUp;
    1:  result:=wsSquare;
    2:  result:=wsSine;
    3:  result:=wsSAndH;
  end;
end;

end.

