unit XEffects64;

interface

uses DAV_VSTEffect,DAV_VSTHost,DAV_Types,Classes,ExtCtrls,ComCtrls,XEffects;
const EFF_DELAY = 17;
const EFF_REVERB = 13;
const EFF_CHORUS = 8;
const EFF_PHASER = 5;
type  TEffects64 = class(TEffects)
private
public
  procedure AddDefaultEffects;override;
end;

implementation

{ TEffects }

uses SysUtils,Windows,Math,Controls,UVirtCC;
const FxPath='C:\Midi\VST\XREFX\';

type TLeetDelay = class (TEffectPlugin)
  FDelayTimeEnabled,FDelayDepthEnabled:boolean;
  constructor Create(effects:TEffects;dllname:string='');override;
  procedure SetParameter(index, value: integer);override;
  class function getID:integer; override;
end;

type TOrilRiverReverb = class (TEffectPlugin)
  constructor Create(effects:TEffects;dllname:string='');override;
  procedure SetParameter(index, value: integer);override;
  class function getID:integer; override;
end;

type TTalChorus = class (TEffectPlugin)
  constructor Create(effects:TEffects;dllname:string='');override;
  procedure SetParameter(index, value: integer);override;
  class function getID:integer; override;
end;

type TTalPhaser = class (TEffectPlugin)
  constructor Create(effects:TEffects;dllname:string='');override;
  procedure SetParameter(index, value: integer);override;
  class function getID:integer; override;
end;

{ TLeetDelay }

const LEET_DELAYTIME = 0;
const LEET_FEEDBACK = 3;
const LEET_WET = 9;


constructor TLeetDelay.Create(effects: TEffects;dllname:string='');
begin
  inherited create(effects,FxPath+'leetdelay.dll');
  SetParameter(0,50);
  SetParameter(2,127);
  SetParameter(4,127);
  SetParameter(5,0);
  SetParameter(6,0);

//  SetParameter(8,0);
  AddEffect(0,LEET_WET);        // VirtCC_DELAY_DEPTH
  AddEffect(1,LEET_DELAYTIME);  // VirtCC_DELAY_PARAM1
  AddEffect(8,LEET_FEEDBACK);   // VirtCC_DELAY_PARAM2
end;


class function TLeetDelay.getID: integer;
begin
  result:=EFF_DELAY;
end;

procedure TLeetDelay.SetParameter(index, value: integer);
  function mapFastAndLimit(value:integer):integer;
  // 0->0, 20 -> 64, 127 -> 120
  begin
    if value<20 then result:=round(value*64/20)
                     else result:=64+round((value-20)*56 / (127-20));
  end;
begin
  case index of
    0:begin
        FDelayTimeEnabled:=value>=5;
        if FDelayTimeEnabled then value:=round((value-5)*127/122);
        InternalEnabled:=FDelayTimeEnabled and FDelayDepthEnabled;
      end;
    // Hier stond een 8 ??
    3:value:=mapFastAndLimit(value);
    9:begin
        value:=mapFastAndLimit(value);
        FDelayDepthEnabled:=value>0;
        InternalEnabled:=FDelayTimeEnabled and FDelayDepthEnabled;
      end;
  end;
  inherited;
end;

{ TOrilRiverReverb }

const ORILRIVER_DECAYTIME = 5;
const ORILRIVER_ROOMSIZE = 6;
const ORILRIVER_WET = 18;

constructor TOrilRiverReverb.Create(effects:TEffects;dllname:string);
begin
  inherited create(effects,FxPath+'OrilRiver.dll');
  AddEffect(2,18);  // VirtCC_REVERB_DEPTH
  AddEffect(3,5);   // VirtCC_REVERB_TIME
  AddEffect(9,6);   // VirtCC_REVERB_FEEDBACK
  SetParameter(0,50);
end;

class function TOrilRiverReverb.getID: integer;
begin
  result:=EFF_REVERB;
end;

procedure TOrilRiverReverb.SetParameter(index, value: integer);
begin
  case index of
    18: begin
          value:=value dIV 3; // Reduce Wet
          SetParameter(0,127-value);
          internalEnabled:=value>0;
        end;
  end;
  inherited;
end;

{ TTalChorus }

constructor TTalChorus.Create(effects:TEffects;dllname:string);
begin
  inherited create(effects,FxPath+'TAL-Chorus.dll');
  AddEffect(4,3);
  AddEffect(5,10); // 10 -> Non existing to modify params 0 and 1
  SetParameter(2,64);
end;

class function TTalChorus.getID: integer;
begin
  result:=EFF_CHORUS;
end;

procedure TTalChorus.SetParameter(index, value: integer);
begin
  case index of
    10:   begin  // 10 -> Non existing to modify params 0 and 1
            value:=value DIV 32;
            SetParameter(0,127*(value MOD 2));
            SetParameter(1,127*(value DIV 2));
            exit;
          end;
     3:   begin
            setParameter(2,max(45,64 - value DIV 4));
            internalEnabled:=value>0;
          end;

  end;
  inherited;
end;

{ TTalPhaser }

constructor TTalPhaser.Create(effects:TEffects;dllname:string);
begin
  inherited create(effects,FxPath+'TAL-Phaser.dll');
  AddEffect(6,7);
  AddEffect(7,1);
end;

class function TTalPhaser.getID: integer;
begin
  result:=EFF_PHASER;
end;

procedure TTalPhaser.SetParameter(index, value: integer);
begin
  case index of
    7: internalEnabled:=value>0;
  end;
  inherited;
end;


{ TEffects64 }

procedure TEffects64.AddDefaultEffects;
begin
  EffectsSettings.AddEffect(EFF_DELAY);
  EffectsSettings.AddEffect(EFF_REVERB);
  EffectsSettings.AddEffect(EFF_CHORUS);
  EffectsSettings.AddEffect(EFF_PHASER);
end;

begin
  AddEffectPluginClass(TLeetDelay);
  AddEffectPluginClass(TOrilRiverReverb);
  AddEffectPluginClass(TTalChorus);
  AddEffectPluginClass(TTalPhaser);
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











