unit XPluginRERompler;

{ This unit
- implements the real plugin
- sets the view form
- communicates with the view form }

interface

uses UXPluginBase,Forms,URMCVSTView,DAV_VSTModule,UWaveFormVoice,UWaveFormOscillator,USampleValue, XEffectsBase,XEffects64;

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

type TXPluginRERompler = class(TXPluginBase,IPluginRompler)
  private
    FWaveSound:array[0..2] of EWaveSound;
    FLevel,FCutoff,FSound,FDetune:single;
    FVCA:TVCA;
protected
    function paramtopcc(param: integer): integer;override;
    function pcctoparam(pcc: integer): integer;override;
    function GetSound(osc:integer):EWaveSound;
    function GetOscFrequency(osc:integer;pitch: single): single;
    function GetGlide:single;
    procedure GetConvertedVCA(VAR A,D,S,R,gain:single);
    procedure OnParameterChanged(Sender: TObject; const Index,Value: Integer);override;
    function GetLevel:single;
    procedure SetADSR(phase,value:integer);
    function GetOscLevel(osc:integer): single;
    function GetCutoff(pitch:single):single;
    function CreateEffectsSettings(paramStart: integer; createdefault: boolean): TEffectsSettings; override;

public
    function ParamCount:integer;override;
    function CreateGUIForm(form:TForm):TRMCVSTView;override;
    function CreateVoice:TXVoice;override;
    procedure AddParameters;override;
    constructor create(vstModule:TVSTModule;Samplerate:single);override;

end;

implementation

uses Math,DAV_Common,UVirtCC,XSynthModule, URMCRomplerView;

const recip127 = 1/127;

function TXPluginRERompler.CreateVoice: TXVoice;
begin
  result:=TXWaveVoice.Create(self,self);
end;

function TXPluginRERompler.CreateEffectsSettings(paramStart: integer;createdefault:boolean): TEffectsSettings;
begin
  result:=inherited CreateEffectsSettings(XREROMPLERPARM_DELAYAMOUNT,true);
  result.factory:=TEffects64;
end;

function TXPluginRERompler.GetSound(osc: integer): EWaveSound;
begin
  result:=FWaveSound[osc];
end;

procedure TXPluginRERompler.OnParameterChanged(Sender: TObject; const Index,  Value: Integer);
function   sZeroOne:single;    begin result:=Value * Recip127; end;
begin
  case index of
    XREROMPLERPARM_VOLUME: FLevel:=sZeroOne;
    XREROMPLERPARM_ATTACK: SetADSR(0,value);
    XREROMPLERPARM_RELEASE:SetADSR(3,value);
    XREROMPLERPARM_TONE:   FCutoff:=20+sZeroOne*20000;
    XREROMPLERPARM_SOUND:  FSound:=sZeroOne;
    XREROMPLERPARM_TUNE:   FDetune:=sZeroOne;

  end;
  inherited;
end;

procedure TXPluginRERompler.SetADSR(phase,value:integer);
function   sZeroOne:single;    begin result:=Value * Recip127; end;
function ConvertADSR(x:single;MAXSECONDS:single):single;
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
const ADSR_MUL_A = 4;
const ADSR_MUL_D = 4;
const ADSR_MUL_R = 8;
begin
  with FVCA do
  case phase of
    0: Attack:= ConvertADSR(sZeroOne,ADSR_MUL_A);
    1: Decay:= ConvertADSR(sZeroOne,ADSR_MUL_D);
    2: Sustain:= sZeroOne;
    3: Release:= ConvertADSR(sZeroOne,ADSR_MUL_R);
  end;
end;


function TXPluginRERompler.ParamCount: integer;
begin
  result:= ParamCountXREROMPLER;
end;

procedure TXPluginRERompler.AddParameters;
begin
  AddParameter(XREROMPLERPARM_VOLUME,'Output Level',0,100,80,'%');
  AddParameter(XREROMPLERPARM_ATTACK,'Attack',0,100,20,'%');
  AddParameter(XREROMPLERPARM_RELEASE,'Release',0,100,40,'%');
  AddParameter(XREROMPLERPARM_TONE,'Tone',20,20000,10000,'Hz','__CUTOFF');
  AddParameter(XREROMPLERPARM_SOUND,'Sound',0,100,0,'%');
  AddParameter(XREROMPLERPARM_TUNE,'Tune',0,100,50,'%');
end;

constructor TXPluginRERompler.create(vstModule: TVSTModule; Samplerate: single);
VAR i:integer;
begin
  inherited;
  FPoly:=true;
  FWaveSound[0]:=wvMeloFlute;
  FWaveSound[1]:=wvMeloStrings;
  FWaveSound[2]:=wvMeloChoir;
  SetADSR(1,127);
  SetADSR(2,127);
end;

function TXPluginRERompler.CreateGUIForm(form: TForm): TRMCVSTView;
begin
  result:= TRMCRomplerView.Create(form);
end;

procedure TXPluginRERompler.GetConvertedVCA(var A, D, S, R, gain: single);
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

function TXPluginRERompler.GetCutoff(pitch: single): single;
begin
  result:=FCutoff;
end;

function TXPluginRERompler.GetGlide: single;
begin
  // TODO result:=FGlide*ord(FGlideEnable);
  result:=0;
end;

function TXPluginRERompler.GetLevel: single;
begin
  result:=FLevel;
end;

function TXPluginRERompler.GetOscFrequency(osc:integer;pitch: single): single;
VAR f0,f1,ratio:single;
begin
  pitch:=pitch+2*(FDetune-0.5);
  if pitch<12 then pitch:=12;
  if pitch>108 then pitch:=108;
  pitch:=pitch + 16*(FPitchBend-0.5);
  f0:=pitchToFrequency[trunc(pitch)];
  f1:=pitchToFrequency[trunc(pitch)+1];
  ratio:=pitch-trunc(pitch);
  result:=(1-ratio)*f0+ratio*f1;
end;


function TXPluginRERompler.GetOscLevel(osc: integer): single;
begin
  // sound = 0 , osc 0 -> 1, osc 1 0 osc 2 0
  // sound =0.5, osc 0 -> 0, osc 1 1 osc 2 0
  // sound =1  , osc 0 -> 0, osc 1 0 osc 2 1
  case osc of
    0: result:=max(1-2*FSound,0);
    1: result:=max(1-2*abs(FSound-0.5),0);
    2: result:=max(-1+2*FSound,0);
  end;
end;

function TXPluginRERompler.paramtopcc(param: integer): integer;
begin
  result:=ParamToPhysXRERompler[param];
end;

function TXPluginRERompler.pcctoparam(pcc: integer): integer;
begin
  result:=PhysToParamXRERompler[pcc];
end;




end.

