unit XPluginRECDS2;

interface

uses XSynthModelBase,Forms,URMCVSTView,DAV_VSTModule,XOscillator,UIXPlugin, XEffectsBase,XEffects64;

type TXPluginRECDS2 = class(TXSynthModelBase)
  private
protected
    function GetLFOWaveshape(osc: integer): TWaveShape; override;
    function GetOscWaveshape(osc: integer;VAR noisecolor:integer): TWaveShape;override;
    function paramtopcc(param: integer): integer;override;
    function pcctoparam(pcc: integer): integer;override;
    function CreateEffectsSettings(paramStart: integer;  createdefault: boolean): TEffectsSettings; override;

public
    procedure OnParameterChanged(Sender: TObject; const Index,Value: Integer);override;
    function ParamCount:integer;override;
    procedure AddParameters;override;
    function CreateGUIForm(form:TForm):TRMCVSTView;override;
    function CanOscPulseWidth(osc:integer):boolean;override;
end;

implementation

{ TXSynthModel }

uses Math,DAV_Common,UVirtCC,XSynthModule,URMCCrumarView;


function TXPluginRECDS2.ParamCount: integer;
begin
  result:= ParamCountXRECDS2;
end;

function TXPluginRECDS2.CreateEffectsSettings(paramStart: integer;createdefault:boolean): TEffectsSettings;
begin
  result:=inherited CreateEffectsSettings(XRECDS2PARM_DELAYAMOUNT,true);
  result.factory:=TEffects64;
end;

procedure TXPluginRECDS2.AddParameters;
begin
    AddParameter(XRECDS2PARM_OSC1WAVE,'OSC1 Wave',0,3,0,'','Square PW;Square;Triangle;Saw');
    AddParameter(XRECDS2PARM_VCAATTACK,'VCA Attack',0,100,20,'%');
    AddParameter(XRECDS2PARM_VCADECAY,'VCA Decay',0,100,80,'%');
    AddParameter(XRECDS2PARM_VCASUSTAIN,'VCA Sustain',0,100,80,'%');
    AddParameter(XRECDS2PARM_VCARELEASE,'VCA Release',0,100,50,'%');
    AddParameter(XRECDS2PARM_OSC1LEVEL,'OSC1 Level',0,100,80,'%');
    AddParameter(XRECDS2PARM_OSC2WAVE,'OSC2 Wave',0,3,0,'','Square PW;Square;Triangle;Saw');
    AddParameter(XRECDS2PARM_VCFATTACK,'VCF Attack',0,100,20,'%');
    AddParameter(XRECDS2PARM_VCFDECAY,'VCF Decay',0,100,80,'%');
    AddParameter(XRECDS2PARM_VCFSUSTAIN,'VCF Sustain',0,100,80,'%');
    AddParameter(XRECDS2PARM_VCFRELEASE,'VCF Release',0,100,50,'%');
    AddParameter(XRECDS2PARM_OSC2LEVEL,'OSC2 Level',0,100,40,'%');
    AddParameter(XRECDS2PARM_CUTOFF,'Cutoff',20,20000,10000,'Hz','__CUTOFF');
    AddParameter(XRECDS2PARM_RESONANCE,'Resonance',0,1,0,'');
    AddParameter(XRECDS2PARM_OUTPUTLEVEL,'Output Level',0,100,100,'%');
    AddParameter(XRECDS2PARM_LFO1WAVE,'LFO1 Wave',0,3,0,'','Saw Up;Square;Triangle;Saw Down');
    AddParameter(XRECDS2PARM_LFO1RATE,'LFO1 Speed',0,127,10,'Hz','__LFO');
    AddParameter(XRECDS2PARM_OSC1MODSELECT,'OSC1 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_OSC1MODLEVEL,'OSC1 Mod Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_NOISECOLOR,'Noise Color',0,127,0,'','');
    AddParameter(XRECDS2PARM_NOISELEVEL,'Noise Level',0,100,0,'%');
    AddParameter(XRECDS2PARM_OSC1FOOT,'OSC1 Foot',0,3,2,'','32;16;8;4');
    AddParameter(XRECDS2PARM_OSC1SEMI,'OSC1 Semi',-12,12,0,'');
    AddParameter(XRECDS2PARM_OSC2FOOT,'OSC2 Foot',0,3,2,'','32;16;8;4');
    AddParameter(XRECDS2PARM_OSC2SEMI,'OSC2 Semi',-12,12,0,'');
    AddParameter(XRECDS2PARM_OSCFAT,'OSC Fat',0,100,10,'%');
    AddParameter(XRECDS2PARM_OSC2MODSELECT,'OSC2 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_OSC2MODLEVEL,'OSC2 Mod Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_VCFMODSELECT,'VCF Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_VCFMODLEVEL,'VCF Mod Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_VCAMODSELECT,'VCA Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_VCAMODLEVEL,'VCA Mod Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_VCFLEVEL,'VCF ADSR',0,100,80,'%');
    AddParameter(XRECDS2PARM_VCALEVEL,'VCA ADSR',0,100,80,'%');
    AddParameter(XRECDS2PARM_VCAMODLEVEL,'VCA Mod Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_LFO1DELAYENABLE,'LFO1 Delay',0,1,0,'','Off;On');
    AddParameter(XRECDS2PARM_LFO2WAVE,'LFO2 Wave',0,3,0,'','Stairway;Square;Triangle;Sample+Hold');
    AddParameter(XRECDS2PARM_LFO2RATE,'LFO2 Speed',0,127,5,'Hz','__LFO');
    AddParameter(XRECDS2PARM_LFO2DELAYENABLE,'LFO2 Delay',0,1,0,'','Off;On');
    AddParameter(XRECDS2PARM_LFO1DELAY,'LFOs Delay',0,100,30,'%');
    AddParameter(XRECDS2PARM_LFO2DELAY,'LFO2 Delay (Unused)',0,100,30,'%');
    AddParameter(XRECDS2PARM_PWM1MODSELECT,'PWM OSC1 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_PWM1MODLEVEL,'PWM OSC1 Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_GLIDE, 'Glide',0,100,30,'%');
    AddParameter(XRECDS2PARM_VCFKEYFOLLOW,'Key Follow',0,100,0,'%');
    AddParameter(XRECDS2PARM_POLY,'Poly',0,1,0,'','Mono;Poly');
    AddParameter(XRECDS2PARM_GLIDEENABLE,'Glide Enable',0,1,0,'','Off;On');

    AddParameter(XRECDS2PARM_OSC3WAVE,'OSC3 Wave',0,3,0,'','Square PW;Square;Triangle;Saw');
    AddParameter(XRECDS2PARM_OSC3FOOT,'OSC3 Foot',0,3,2,'','32;16;8;4');
    AddParameter(XRECDS2PARM_OSC3SEMI,'OSC3 Semi',-12,12,0,'');
    AddParameter(XRECDS2PARM_OSC3LEVEL,'OSC3 Level',0,100,0,'%');

    AddParameter(XRECDS2PARM_OSC4WAVE,'OSC4 Wave',0,3,0,'','Square PW;Square;Triangle;Saw');
    AddParameter(XRECDS2PARM_OSC4FOOT,'OSC4 Foot',0,3,2,'','32;16;8;4');
    AddParameter(XRECDS2PARM_OSC4SEMI,'OSC4 Semi',-12,12,0,'');
    AddParameter(XRECDS2PARM_OSC4LEVEL,'OSC4 Level',0,100,0,'%');

    AddParameter(XRECDS2PARM_OSC3MODSELECT,'OSC3 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_OSC3MODLEVEL,'OSC3 Mod Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_OSC4MODSELECT,'OSC4 Mod Type',0,3,0,'','None;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_OSC4MODLEVEL,'OSC4 Mod Depth',0,100,0,'%');


    AddParameter(XRECDS2PARM_PWM2MODSELECT,'PWM OSC2 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_PWM2MODLEVEL,'PWM OSC2 Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_PWM3MODSELECT,'PWM OSC3 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_PWM3MODLEVEL,'PWM OSC3 Depth',0,100,0,'%');
    AddParameter(XRECDS2PARM_PWM4MODSELECT,'PWM OSC3 Type',0,3,0,'','Manual;LFO1;LFO2;LFO1+2');
    AddParameter(XRECDS2PARM_PWM4MODLEVEL,'PWM OSC3 Depth',0,100,0,'%');
    inherited;
end;

function TXPluginRECDS2.CanOscPulseWidth(osc: integer): boolean;
begin
  result:=(osc<> NOISE_OSCILLATOR ) and (FOscs[osc].Wave=0);
end;

function TXPluginRECDS2.CreateGUIForm(form: TForm): TRMCVSTView;
begin
  result:=TRMCCrumarView.Create(form);
end;

procedure TXPluginRECDS2.OnParameterChanged(Sender: TObject; const Index,Value: Integer);
function   sZeroOne:single;    begin result:=Value * Recip127; end;
function   iSelection:integer; begin result:= Round(Value*3*Recip127); end;
function   bSelection:boolean; begin result:=Value <> 0; end;
function   sLFO:single;        begin result:=0.1*exp(0.0598*Value); end;
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
    XRECDS2PARM_OSC1WAVE:    FOscs[0].Wave := iSelection;
    XRECDS2PARM_VCAATTACK:   FVCA.Attack := ConvertADSR(sZeroOne,ADSR_MUL_A);
    XRECDS2PARM_VCADECAY:    FVCA.Decay := ConvertADSR(sZeroOne,ADSR_MUL_D);
    XRECDS2PARM_VCARELEASE:  FVCA.Release := ConvertADSR(sZeroOne,ADSR_MUL_R);
    XRECDS2PARM_VCASUSTAIN:  FVCA.Sustain := sZeroOne;
    XRECDS2PARM_OSC1LEVEL:   FOscs[0].Level := sZeroOne;

    XRECDS2PARM_OSC2WAVE:    FOscs[1].Wave := iSelection;
    XRECDS2PARM_VCFATTACK:   FVCF.Attack := ConvertADSR(sZeroOne,ADSR_MUL_A);
    XRECDS2PARM_VCFDECAY:    FVCF.Decay := ConvertADSR(sZeroOne,ADSR_MUL_D);
    XRECDS2PARM_VCFRELEASE:  FVCF.Release := ConvertADSR(sZeroOne,ADSR_MUL_R);
    XRECDS2PARM_VCFSUSTAIN:  FVCF.Sustain := sZeroOne;
    XRECDS2PARM_OSC2LEVEL:   FOscs[1].Level := sZeroOne;

    XRECDS2PARM_CUTOFF:      FVCF.Cutoff:=20+sZeroOne*20000;
    XRECDS2PARM_RESONANCE:   FVCF.Resonance:=sZeroOne;
    XRECDS2PARM_OUTPUTLEVEL: FLevel := sZeroOne;
    XRECDS2PARM_LFO1WAVE:    FLFOs[0].Wave:=iSelection;
    XRECDS2PARM_LFO1RATE:    FLFOs[0].speed:=sLFO;
    XRECDS2PARM_OSC1MODSELECT: FOscs[0].ModTarget:=iSelection;
    XRECDS2PARM_OSC1MODLEVEL:FOscs[0].ModDepth:=sZeroOne;
    XRECDS2PARM_NOISECOLOR:  FOscs[NOISE_OSCILLATOR].Wave:=Value;
    XRECDS2PARM_NOISELEVEL:  FOscs[NOISE_OSCILLATOR].Level := sZeroOne;
    XRECDS2PARM_OSC1FOOT:    FOscs[0].Foot:=iSelection-2;
    XRECDS2PARM_OSC1SEMI:    FOscs[0].Semi:=Round(-12+24*Value*Recip127 );
    XRECDS2PARM_OSC2FOOT:    FOscs[1].Foot:=iSelection-2;
    XRECDS2PARM_OSC2SEMI:    FOscs[1].Semi:=Round(-12+24*Value*Recip127 );
    XRECDS2PARM_OSCFAT:  FFat:= sZeroOne;
    XRECDS2PARM_OSC2MODSELECT: FOscs[1].ModTarget:=iSelection;
    XRECDS2PARM_OSC2MODLEVEL:FOscs[1].ModDepth:=sZeroOne;
    XRECDS2PARM_VCFMODSELECT:  FVCF.ModTarget:=iSelection;
    XRECDS2PARM_VCFMODLEVEL: FVCF.ModDepth:=sZeroOne;
    XRECDS2PARM_VCAMODSELECT:  FVCA.ModTarget:=iSelection;
    XRECDS2PARM_VCAMODLEVEL: FVCA.ModDepth:=sZeroOne;
    XRECDS2PARM_VCFLEVEL:    FVCF.Level:=sZeroOne;
    XRECDS2PARM_VCALEVEL:    FVCA.Level:=sZeroOne;
    XRECDS2PARM_LFO1DELAYENABLE:   FLFOs[0].delayEnable:=bSelection;
    XRECDS2PARM_LFO2WAVE:    FLFOs[1].Wave:=iSelection;
    XRECDS2PARM_LFO2RATE:    FLFOs[1].speed:=sLFO;
    XRECDS2PARM_LFO2DELAYENABLE:   FLFOs[1].delayEnable:=bSelection;
    XRECDS2PARM_LFO1DELAY:    begin FLFOs[0].delay:=sZeroOne;FLFOs[1].delay:=sZeroOne; end;
    XRECDS2PARM_PWM1MODSELECT:  FPWM[0].ModTarget:=iSelection;
    XRECDS2PARM_PWM1MODLEVEL: FPWM[0].ModDepth:=sZeroOne;
    XRECDS2PARM_GLIDE:       FGlide:=sZeroOne;
    XRECDS2PARM_GLIDEENABLE: FGlideEnable:=bSelection;
    XRECDS2PARM_VCFKEYFOLLOW:FVCFKeyFollow:=sZeroOne;
    XRECDS2PARM_POLY:        FPoly:=bSelection;

    XRECDS2PARM_OSC3WAVE:    FOscs[3].Wave := iSelection;
    XRECDS2PARM_OSC3FOOT:    FOscs[3].Foot:=iSelection-2;
    XRECDS2PARM_OSC3SEMI:    FOscs[3].Semi:=Round(-12+24*Value*Recip127 );
    XRECDS2PARM_OSC3LEVEL:   FOscs[3].Level := sZeroOne;
    XRECDS2PARM_OSC4WAVE:    FOscs[4].Wave := iSelection;
    XRECDS2PARM_OSC4FOOT:    FOscs[4].Foot:=iSelection-2;
    XRECDS2PARM_OSC4SEMI:    FOscs[4].Semi:=Round(-12+24*Value*Recip127 );
    XRECDS2PARM_OSC4LEVEL:   FOscs[4].Level := sZeroOne;

    XRECDS2PARM_PWM2MODSELECT:FPWM[1].ModTarget:=iSelection;
    XRECDS2PARM_PWM2MODLEVEL: FPWM[1].ModDepth:=sZeroOne;
    XRECDS2PARM_PWM3MODSELECT:FPWM[3].ModTarget:=iSelection;
    XRECDS2PARM_PWM3MODLEVEL: FPWM[3].ModDepth:=sZeroOne;
    XRECDS2PARM_PWM4MODSELECT:FPWM[4].ModTarget:=iSelection;
    XRECDS2PARM_PWM4MODLEVEL: FPWM[4].ModDepth:=sZeroOne;

    XRECDS2PARM_OSC3MODSELECT: FOscs[3].ModTarget:=iSelection;
    XRECDS2PARM_OSC3MODLEVEL:FOscs[3].ModDepth:=sZeroOne;
    XRECDS2PARM_OSC4MODSELECT: FOscs[4].ModTarget:=iSelection;
    XRECDS2PARM_OSC4MODLEVEL:FOscs[4].ModDepth:=sZeroOne;
  end;
  inherited;
end;

function TXPluginRECDS2.GetOscWaveshape(osc: integer;VAR noisecolor:integer): TWaveShape;
const Waves:array[0..3] of TWaveShape = (wsSquare,wsSquare,wsTriangle,wsSawUp);
begin
  if osc = NOISE_OSCILLATOR then begin result:=wsNoise; noisecolor:= FOscs[NOISE_OSCILLATOR].Wave end
                            else begin result:=Waves[FOscs[osc].Wave]; noisecolor:=127; end;
end;

function TXPluginRECDS2.GetLFOWaveshape(osc: integer): TWaveShape;
begin
  case FLFOs[osc].Wave of
    0:  if osc=0 then result:=wsSawUp else result:=wsStairCase;
    1:  result:=wsSquare;
    2:  result:=wsSine;
    3:  if osc=0 then result:=wsSawDown else result:=wsSAndH;
  end;
end;

function TXPluginRECDS2.paramtopcc(param: integer): integer;
begin
  result:=ParamToPhysXRECDS2[param];
end;

function TXPluginRECDS2.pcctoparam(pcc: integer): integer;
begin
  result:=PhysToParamXRECDS2[pcc];
end;


end.

