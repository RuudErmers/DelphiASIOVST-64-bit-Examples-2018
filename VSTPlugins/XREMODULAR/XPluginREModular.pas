unit XPluginREModular;

{ This unit
- implements the real plugin
- sets the view form
- communicates with the view form }

interface

uses XSynthModelBase,UIXPlugin,Forms,URMCVSTView,DAV_VSTModule,XEffectsBase,XEffects64;

type TXPluginREModular = class(TXSynthModelBase)
  private
protected
    function CreateEffectsSettings(paramStart: integer; createdefault: boolean): TEffectsSettings; override;

    function paramtopcc(param: integer): integer;override;
    function pcctoparam(pcc: integer): integer;override;
    function CanOscPulseWidth(osc: integer): boolean;override;

public
    procedure OnParameterChanged(Sender: TObject; const Index,Value: Integer);override;
    function ParamCount:integer;override;
    procedure AddParameters;override;
    function CreateGUIForm(form:TForm):TRMCVSTView;override;
end;

implementation

{ TXSynthModel }

uses Math,DAV_Common,UVirtCC,XSynthModule,URMCModularView, BassMidi;



function TXPluginREModular.ParamCount: integer;
begin
  result:= ParamCountXREMODULAR;
end;

//Saw,Square,Sine,Triangle,HPulse,Qpulse,TriSaw,Noise
// Saw,Squ,Tri,Noise

function TXPluginREModular.CanOscPulseWidth(osc: integer): boolean;
begin
  result:=true;
end;

function TXPluginREModular.CreateEffectsSettings(paramStart: integer;createdefault:boolean): TEffectsSettings;
begin
  result:=inherited CreateEffectsSettings(XREMODULARPARM_DELAYAMOUNT,true);
  result.factory:=TEffects64;
end;


procedure TXPluginREModular.AddParameters;
begin

    AddParameter(XREMODULARPARM_OSC1WAVE,'OSC1 Wave',0,7,0,'','Saw;Square;Sine;Triangle;HPulse;Qpulse;TriSaw;Noise');
    AddParameter(XREMODULARPARM_VCAATTACK,'VCA Attack',0,100,20,'%');
    AddParameter(XREMODULARPARM_VCADECAY,'VCA Decay',0,100,80,'%');
    AddParameter(XREMODULARPARM_VCASUSTAIN,'VCA Sustain',0,100,80,'%');
    AddParameter(XREMODULARPARM_VCARELEASE,'VCA Release',0,100,50,'%');
    AddParameter(XREMODULARPARM_OSC1LEVEL,'OSC1 Level',0,100,80,'%');
    AddParameter(XREMODULARPARM_OSC2WAVE,'OSC2 Wave',0,7,0,'','Saw;Square;Sine;Triangle;HPulse;Qpulse;TriSaw;Noise');
    AddParameter(XREMODULARPARM_VCFATTACK,'VCF Attack',0,100,20,'%');
    AddParameter(XREMODULARPARM_VCFDECAY,'VCF Decay',0,100,80,'%');
    AddParameter(XREMODULARPARM_VCFSUSTAIN,'VCF Sustain',0,100,80,'%');
    AddParameter(XREMODULARPARM_VCFRELEASE,'VCF Release',0,100,50,'%');
    AddParameter(XREMODULARPARM_OSC2LEVEL,'OSC2 Level',0,100,40,'%');
    AddParameter(XREMODULARPARM_CUTOFF,'Cutoff',20,20000,10000,'Hz','__CUTOFF');
    AddParameter(XREMODULARPARM_RESONANCE,'Resonance',0,1,0,'');
    AddParameter(XREMODULARPARM_OUTPUTLEVEL,'Output Level',0,100,100,'%');
    AddParameter(XREMODULARPARM_LFO1WAVE,'LFO1 Wave',0,3,0,'','Saw;Square;Triangle;SandH');
    AddParameter(XREMODULARPARM_LFO1RATE,'LFO1 Speed',0,127,10,'Hz','__LFO');
    AddParameter(XREMODULARPARM_OSC1MODSELECT,'OSC1 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_OSC1MODLEVEL,'OSC1 Mod Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_NOISECOLOR,'Noise Color',0,127,0,'','');
    AddParameter(XREMODULARPARM_NOISELEVEL,'Noise Level',0,100,0,'%');
    AddParameter(XREMODULARPARM_OSC1FOOT,'OSC1 Foot',0,6,3,'','64;32;16;8;4;2;1');
    AddParameter(XREMODULARPARM_OSC1SEMI,'OSC1 Semi',-12,12,0,'');
    AddParameter(XREMODULARPARM_OSC2FOOT,'OSC2 Foot',0,6,3,'','64;32;16;8;4;2;1');
    AddParameter(XREMODULARPARM_OSC2SEMI,'OSC2 Semi',-12,12,0,'');
    AddParameter(XREMODULARPARM_OSC2DETUNE,'OSC2 Detune',0,100,0,'%');
    AddParameter(XREMODULARPARM_OSCFAT,'OSC Fat',0,100,10,'%');
    AddParameter(XREMODULARPARM_OSC2MODSELECT,'OSC2 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_OSC2MODLEVEL,'OSC2 Mod Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_VCFMODSELECT,'VCF Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_VCFMODLEVEL,'VCF Mod Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_VCAMODSELECT,'VCA Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_VCAMODLEVEL,'VCA Mod Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_VCFLEVEL,'VCF ADSR',0,100,80,'%');
    AddParameter(XREMODULARPARM_VCALEVEL,'VCA ADSR',0,100,80,'%');
    AddParameter(XREMODULARPARM_VCAMODLEVEL,'VCA Mod Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_LFO1DELAY,'LFO1 Delay',0,1,0,'','Off;On');
    AddParameter(XREMODULARPARM_LFO2WAVE,'LFO1 Wave',0,3,0,'','Saw;Square;Triangle;SandH');
    AddParameter(XREMODULARPARM_LFO2RATE,'LFO2 Speed',0,127,5,'Hz','__LFO');
    AddParameter(XREMODULARPARM_LFO2DELAY,'LFO2 Delay',0,1,0,'','Off;On');
    AddParameter(XREMODULARPARM_LFO1DELAY,'LFO1 Delay',0,100,30,'%');
    AddParameter(XREMODULARPARM_LFO2DELAY,'LFO2 Delay',0,100,30,'%');
    AddParameter(XREMODULARPARM_PWM1MODSELECT,'PWM OSC1 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_PWM1MODLEVEL,'PWM OSC1 Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_GLIDE, 'Glide',0,100,30,'%');
    AddParameter(XREMODULARPARM_VCFKEYFOLLOW,'Key Follow',0,100,0,'%');
    AddParameter(XREMODULARPARM_POLY,'Poly',0,1,0,'','Mono;Poly');
    AddParameter(XREMODULARPARM_GLIDEENABLE,'Glide Enable',0,1,0,'','Off;On');

    AddParameter(XREMODULARPARM_OSC3WAVE,'OSC3 Wave',0,7,0,'','Saw;Square;Sine;Triangle;HPulse;Qpulse;TriSaw;Noise');
    AddParameter(XREMODULARPARM_OSC3FOOT,'OSC3 Foot',0,6,3,'','64;32;16;8;4;2;1');
    AddParameter(XREMODULARPARM_OSC3SEMI,'OSC3 Semi',-12,12,0,'');
    AddParameter(XREMODULARPARM_OSC3LEVEL,'OSC3 Level',0,100,0,'%');
    AddParameter(XREMODULARPARM_OSC3DETUNE,'OSC3 Detune',0,100,0,'%');

    AddParameter(XREMODULARPARM_OSC4WAVE,'OSC4 Wave',0,7,0,'','Saw;Square;Sine;Triangle;HPulse;Qpulse;TriSaw;Noise');
    AddParameter(XREMODULARPARM_OSC4FOOT,'OSC4 Foot',0,6,3,'','64;32;16;8;4;2;1');
    AddParameter(XREMODULARPARM_OSC4SEMI,'OSC4 Semi',-12,12,0,'');
    AddParameter(XREMODULARPARM_OSC4LEVEL,'OSC4 Level',0,100,0,'%');
    AddParameter(XREMODULARPARM_OSC4DETUNE,'OSC4 Detune',0,100,0,'%');

    AddParameter(XREMODULARPARM_OSC3MODSELECT,'OSC3 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_OSC3MODLEVEL,'OSC3 Mod Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_OSC4MODSELECT,'OSC4 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_OSC4MODLEVEL,'OSC4 Mod Depth',0,100,0,'%');


    AddParameter(XREMODULARPARM_PWM2MODSELECT,'PWM OSC2 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_PWM2MODLEVEL,'PWM OSC2 Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_PWM3MODSELECT,'PWM OSC3 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_PWM3MODLEVEL,'PWM OSC3 Depth',0,100,0,'%');
    AddParameter(XREMODULARPARM_PWM4MODSELECT,'PWM OSC3 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XREMODULARPARM_PWM4MODLEVEL,'PWM OSC3 Depth',0,100,0,'%');
(* dit moet toch weg ?
    AddParameter(XREMODULARPARM_DELAYAMOUNT,'Delay Amount',0,100,0,'%');
    AddParameter(XREMODULARPARM_DELAYTIME, 'Delay Time',0,96,0,'beat/24');
    AddParameter(XREMODULARPARM_REVERBAMOUNT,'Reverb Amount',0,100,0,'%');
    AddParameter(XREMODULARPARM_REVERBTIME, 'Reverb Time',0,4000,0,'msec');
    AddParameter(XREMODULARPARM_CHORUSDEPTH,'Chorus Amount',0,100,0,'%');
    AddParameter(XREMODULARPARM_CHORUSRATE,'Chorus Rate',0,100,50,'%');
    AddParameter(XREMODULARPARM_PHASERDEPTH,'Phaser Amount',0,100,0,'%');
    AddParameter(XREMODULARPARM_PHASERRATE ,'Phaser Rate',0,100,50,'%');
    AddParameter(XREMODULARPARM_DELAYFEEDBACK,'Delay Feedback',0,100,50,'%');
    AddParameter(XREMODULARPARM_REVERBFEEDBACK,'Reverb Feedback',0,100,50,'%');
    AddParameter(XREMODULARPARM_EFFECTSETTINGS,'Effect Settings',0,127,4,'(Internal)'); *)
    AddParameter(XREMODULARPARM_RINGMODULATION,'Ring Modulation',0,100,0,'%');
    AddParameter(XREMODULARPARM_OSC1_SYNC,'Flow OSC 1 2 LFO 1',0,1,0,'','Off;On');
    AddParameter(XREMODULARPARM_OSC2_SYNC,'Sync OSC2 To OSC1',0,1,0,'','Off;On');
    AddParameter(XREMODULARPARM_OSC3_SYNC,'Flow OSC 3 4 LFO 2',0,1,0,'','Off;On');
    AddParameter(XREMODULARPARM_OSC4_SYNC,'Sync OSC3 To OSC4',0,1,0,'','Off;On');
    inherited;
end;

function TXPluginREModular.CreateGUIForm(form: TForm): TRMCVSTView;
begin
  result:=TRMCModularView.Create(form);
	BASS_Init(0, 44100, 0, 0, nil);

end;

function CreateObject(vstModule: TvstModule;  samplerate: single): IXPlugin;
begin
  result:=TXPluginREModular.Create(vstModule, samplerate);
end;

procedure TXPluginREModular.OnParameterChanged(Sender: TObject; const Index,Value: Integer);
function   sZeroOne:single;    begin result:=Value * Recip127; end;
function   iSelection:integer; begin result:= Round(Value*3*Recip127); end;
function   iSelection7:integer; begin result:= Round(Value*6*Recip127); end;
function   iSelection8:integer; begin result:= Round(Value*7*Recip127); end;
function   bSelection:boolean; begin result:=Value <> 0; end;
function   sLFO:single;        begin result:=0.1*exp(0.0598*Value); end;   // between 0.1 and 20
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
  case index of
    XREMODULARPARM_OSC1WAVE:    FOscs[0].Wave := iSelection8;
    XREMODULARPARM_VCAATTACK:   FVCA.Attack := ConvertADSR(sZeroOne,ADSR_MUL_A);
    XREMODULARPARM_VCADECAY:    FVCA.Decay := ConvertADSR(sZeroOne,ADSR_MUL_D);
    XREMODULARPARM_VCARELEASE:  FVCA.Release := ConvertADSR(sZeroOne,ADSR_MUL_R);
    XREMODULARPARM_VCASUSTAIN:  FVCA.Sustain := sZeroOne;
    XREMODULARPARM_OSC1LEVEL:   FOscs[0].Level := sZeroOne;

    XREMODULARPARM_OSC2WAVE:    FOscs[1].Wave := iSelection8;
    XREMODULARPARM_VCFATTACK:   FVCF.Attack := ConvertADSR(sZeroOne,ADSR_MUL_A);
    XREMODULARPARM_VCFDECAY:    FVCF.Decay := ConvertADSR(sZeroOne,ADSR_MUL_D);
    XREMODULARPARM_VCFRELEASE:  FVCF.Release := ConvertADSR(sZeroOne,ADSR_MUL_R);
    XREMODULARPARM_VCFSUSTAIN:  FVCF.Sustain := sZeroOne;
    XREMODULARPARM_OSC2LEVEL:   FOscs[1].Level := sZeroOne;

    XREMODULARPARM_CUTOFF:      FVCF.Cutoff:=20+sZeroOne*20000;
    XREMODULARPARM_RESONANCE:   FVCF.Resonance:=sZeroOne;
    XREMODULARPARM_OUTPUTLEVEL: FLevel := sZeroOne;
    XREMODULARPARM_LFO1WAVE:    FLFOs[0].Wave:=iSelection;
    XREMODULARPARM_LFO1RATE:    FLFOs[0].speed:=sLFO;
    XREMODULARPARM_OSC1MODSELECT: FOscs[0].ModTarget:=iSelection;
    XREMODULARPARM_OSC1MODLEVEL:FOscs[0].ModDepth:=sZeroOne;
    XREMODULARPARM_NOISECOLOR:  FOscs[NOISE_OSCILLATOR].Wave:=Value;
    XREMODULARPARM_NOISELEVEL:  FOscs[NOISE_OSCILLATOR].Level := sZeroOne;
    XREMODULARPARM_OSC1FOOT:    FOscs[0].Foot:=iSelection7-2;
    XREMODULARPARM_OSC1SEMI:    FOscs[0].Semi:=Round(-12+24*Value*Recip127 );
    XREMODULARPARM_OSC2FOOT:    FOscs[1].Foot:=iSelection7-2;
    XREMODULARPARM_OSC2SEMI:    FOscs[1].Semi:=Round(-12+24*Value*Recip127 );
    XREMODULARPARM_OSCFAT:      FFat:= sZeroOne;
    XREMODULARPARM_OSC2MODSELECT: FOscs[1].ModTarget:=iSelection;
    XREMODULARPARM_OSC2MODLEVEL:FOscs[1].ModDepth:=sZeroOne;
    XREMODULARPARM_VCFMODSELECT:  FVCF.ModTarget:=iSelection;
    XREMODULARPARM_VCFMODLEVEL: FVCF.ModDepth:=sZeroOne;
    XREMODULARPARM_VCAMODSELECT:  FVCA.ModTarget:=iSelection;
    XREMODULARPARM_VCAMODLEVEL: FVCA.ModDepth:=sZeroOne;
    XREMODULARPARM_VCFLEVEL:    FVCF.Level:=sZeroOne;
    XREMODULARPARM_VCALEVEL:    FVCA.Level:=sZeroOne;
    XREMODULARPARM_LFO1DELAYENABLE:   FLFOs[0].delayEnable:=bSelection;
    XREMODULARPARM_LFO2WAVE:    FLFOs[1].Wave:=iSelection;
    XREMODULARPARM_LFO2RATE:    FLFOs[1].speed:=sLFO;
    XREMODULARPARM_LFO2DELAYENABLE:   FLFOs[1].delayEnable:=bSelection;
    XREMODULARPARM_LFO1DELAY:    FLFOs[0].Delay:=sZeroOne;
    XREMODULARPARM_LFO2DELAY:    FLFOs[1].Delay:=sZeroOne;
    XREMODULARPARM_PWM1MODSELECT:  FPWM[0].ModTarget:=iSelection;
    XREMODULARPARM_PWM1MODLEVEL: FPWM[0].ModDepth:=sZeroOne;
    XREMODULARPARM_GLIDE:       FGlide:=sZeroOne;
    XREMODULARPARM_GLIDEENABLE: FGlideEnable:=bSelection;
    XREMODULARPARM_VCFKEYFOLLOW:FVCFKeyFollow:=sZeroOne;
    XREMODULARPARM_POLY:        FPoly:=bSelection;

    XREMODULARPARM_OSC3WAVE:    FOscs[3].Wave := iSelection8;
    XREMODULARPARM_OSC3FOOT:    FOscs[3].Foot:=iSelection7-2;
    XREMODULARPARM_OSC3SEMI:    FOscs[3].Semi:=Round(-12+24*Value*Recip127 );
    XREMODULARPARM_OSC3LEVEL:   FOscs[3].Level := sZeroOne;
    XREMODULARPARM_OSC4WAVE:    FOscs[4].Wave := iSelection8;
    XREMODULARPARM_OSC4FOOT:    FOscs[4].Foot:=iSelection7-2;
    XREMODULARPARM_OSC4SEMI:    FOscs[4].Semi:=Round(-12+24*Value*Recip127 );
    XREMODULARPARM_OSC4LEVEL:   FOscs[4].Level := sZeroOne;

    XREMODULARPARM_PWM2MODSELECT:FPWM[1].ModTarget:=iSelection;
    XREMODULARPARM_PWM2MODLEVEL: FPWM[1].ModDepth:=sZeroOne;
    XREMODULARPARM_PWM3MODSELECT:FPWM[3].ModTarget:=iSelection;
    XREMODULARPARM_PWM3MODLEVEL: FPWM[3].ModDepth:=sZeroOne;
    XREMODULARPARM_PWM4MODSELECT:FPWM[4].ModTarget:=iSelection;
    XREMODULARPARM_PWM4MODLEVEL: FPWM[4].ModDepth:=sZeroOne;

    XREMODULARPARM_OSC3MODSELECT: FOscs[3].ModTarget:=iSelection;
    XREMODULARPARM_OSC3MODLEVEL:FOscs[3].ModDepth:=sZeroOne;
    XREMODULARPARM_OSC4MODSELECT: FOscs[4].ModTarget:=iSelection;
    XREMODULARPARM_OSC4MODLEVEL:FOscs[4].ModDepth:=sZeroOne;

    XREMODULARPARM_DELAYAMOUNT,
    XREMODULARPARM_DELAYTIME,
    XREMODULARPARM_REVERBAMOUNT,
    XREMODULARPARM_REVERBTIME,
    XREMODULARPARM_CHORUSDEPTH,
    XREMODULARPARM_CHORUSRATE,
    XREMODULARPARM_PHASERDEPTH,
    XREMODULARPARM_PHASERRATE,
    XREMODULARPARM_DELAYFEEDBACK,
    XREMODULARPARM_REVERBFEEDBACK:SetEffect(index-XREMODULARPARM_DELAYAMOUNT,value);
    XREMODULARPARM_EFFECTSETTINGS: EffectSettings(value);
    XREMODULARPARM_RINGMODULATION: FRingLevel := sZeroOne;
    XREMODULARPARM_OSC2DETUNE: FOscs[1].Detune:=sZeroOne;
    XREMODULARPARM_OSC3DETUNE: FOscs[2].Detune:=sZeroOne;
    XREMODULARPARM_OSC4DETUNE: FOscs[3].Detune:=sZeroOne;
    XREMODULARPARM_OSC1_SYNC: FOscs[0].Sync:=bSelection;
    XREMODULARPARM_OSC2_SYNC: FOscs[1].Sync:=bSelection;
    XREMODULARPARM_OSC3_SYNC: FOscs[2].Sync:=bSelection;
    XREMODULARPARM_OSC4_SYNC: FOscs[3].Sync:=bSelection;
    else exit;
  end;
  inherited;
end;

function TXPluginREModular.paramtopcc(param: integer): integer;
begin
  result:=ParamToPhysXREModular[param];
end;

function TXPluginREModular.pcctoparam(pcc: integer): integer;
begin
  result:=PhysToParamXREModular[pcc];
end;



end.

