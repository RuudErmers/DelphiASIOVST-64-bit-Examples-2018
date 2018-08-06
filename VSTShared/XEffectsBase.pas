unit XEffectsBase;

interface

uses DAV_Types;

type TEffectsArray = array of integer;
     TEffectsBase = class;
     TEffectsBaseClass = class of TEffectsBase;
     TEffectsSettings = class
                         ParamStart:integer;
                         createdefault:boolean;
                         effects:TEffectsArray;
                         factory:TEffectsBaseClass;
                         constructor Create(effStart:integer;createdefault:boolean);
                         procedure AddEffect(eff:integer);
                       end;
    TEffectsBase = class
    effectsSettings:TEffectsSettings;
    procedure SetEffectsEnable(enable: boolean);virtual;
    procedure ShowEffects;virtual;
    procedure SetTempo(tempo: single);virtual;
    constructor Create(effectsSettings:TEffectsSettings);virtual;
    procedure ShowEffect(fx:integer);virtual;
    procedure Process(const Buf0,Buf1: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);virtual;
    procedure SetEffect(index, value: integer);virtual;
    function ProcessCount: integer;virtual;
    procedure Close;virtual;
    procedure AddDefaultEffects;virtual;
end;

implementation


procedure TEffectsBase.AddDefaultEffects;
begin

end;

procedure TEffectsBase.Close;
begin

end;

constructor TEffectsBase.Create(effectsSettings:TEffectsSettings);
begin
  self.effectsSettings:=effectsSettings;
  if effectsSettings.createdefault then
    AddDefaultEffects;
end;

procedure TEffectsBase.Process(const Buf0, Buf1: TDAVArrayOfSingleFixedArray;  const SampleFrames: Cardinal);
begin

end;

function TEffectsBase.ProcessCount: integer;
begin
  result:=0;
end;

procedure TEffectsBase.SetEffect(index, value: integer);
begin

end;

procedure TEffectsBase.SetEffectsEnable(enable: boolean);
begin

end;

procedure TEffectsBase.SetTempo(tempo: single);
begin

end;

procedure TEffectsBase.ShowEffect(fx: integer);
begin

end;

procedure TEffectsBase.ShowEffects;
begin

end;

{ TEffectsSettings }

procedure TEffectsSettings.AddEffect(eff: integer);
VAR l:integer;
begin
  l:=length(effects);
  SetLength(effects,l+1);
  effects[l]:=eff;
end;

constructor TEffectsSettings.Create(effStart: integer;createdefault:boolean);
begin
  self.createdefault:=createdefault;
  ParamStart:=effStart;
end;

end.


OK, ze zitten erin.
- Beweging via Modular

- Hoe zgaat het met Bypass:

Als 'Effects Bypassed' dan:
- Wordt Feffects.Process niet aangeroepen

VstEffect.Count levert niet meer VSTHost.Count maar aantal te processen VSTs



Reaverb
Length time = 0 (2)
Length musical = gaat tot 256 8ste noten...    koppelt to Time   (3)
Wet = koppeld ro Depth (0)
Feedback koppelt to Feedback (5)


128 = 512 16ste noten

LeetDelay

Delay koppel to 0
2 Tempo Delay MUST be 127
3 = Feedback
9 = Wet

Reverb

wet = 18

room size = feedback 6
decay time = time    5

Chorus
Simple
rate=rate en depth = depth (3 en 4)

Phase simple
rate=rate en depth = depth (1 en 0)

We moeten nog wat doen aan de Volumes:
LeetDelay	-> Wet = Wet Mix		, Dry Mix available tot 10 Db  (Default Dry = 0)
Oril River	-> Idem (tot 0 db) (Default Dry = 0), 0 zit bij 80
Miltiply    -> Idem (to 18 db) (Default Dry = 0), 0 zit bij 80 => Kan hier niet bij, dus? is ok.
Hy - Phaser doet verhouding en zit niet bij Depth

Ik laat het maar even zo...Zou ik maar niet doen ...











