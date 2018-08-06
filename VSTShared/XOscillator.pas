unit XOscillator;

interface

{$I DAV_Compiler.inc}

uses DAV_Complex,DAV_DspPinkNoiseGenerator;
type  TWaveShape = (wsNone,wsSine,wsSquare,wsNoise,wsTriangle,wsSawUp,wsSawDown,wsStaircase,wsSAndH);


type TXBaseOscillator = class(TObject)
  private
    FSampleRate : Single;
    FSampleReci : Single;
    FFrequency  : Single;
    FWaveshape  : TWaveShape;
    FPinky: TFastPinkNoiseGenerator;
    FSAndHLevel, FStairCaseLevel:single;
    FIsLfo: boolean;
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure SampleRateChanged; virtual;
    procedure OnZeroCrossed;
    function ValueAt(FxPos:single): Single;virtual; // FxPos: 0..1
  public

    {property} NoiseColor:integer;
    constructor Create(const SampleRate: Single;isLFO:boolean); virtual;
    destructor Destroy; override;
    property WaveShape: TWaveShape read FWaveShape write FWaveShape;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

type TXOscillatorBlep = class(TXBaseOscillator)
    function ValueAt(FxPos:single): Single;override; // FxPos: 0..1
  private
    function PolyBlep(t: single): single;
end;

type
     TXOscillator = class;
     TWTFDef = record
                 Start,Eind, StartFx,EindFx:single;
                 procedure CalcFx(osc:TXOscillator;up:boolean);
                 function  ValueAtInterpolate(FXPos:single):single;
               end;

     TWTF = record
              t1,t2: TWTFDef;
              function ValueAt(osc:TXOscillator;FXpos:single):single;
            end;

     TXOscillator = class(TXOscillatorBlep)
  private
    FValue      : Single;
    FXpos       : single;
    FPulseWidth : single;
    FWTF:TWTF;
    FWTFWidth   : single;
    function PulseWidthAdjusted(FxPos:single): single;
    procedure SetPulseWidth(value:single);
    procedure UpdateWTF;
  public
    { property } DoWTF:boolean;
    {property} WTFWaveShape: TWaveShape;
    property PulseWidth:single read FPulseWidth write SetPulseWidth;
    procedure ResetPhase;
    function Process(VAR ZeroCrossed:boolean):single;
    function ValueAt(FxPos:single): Single;override; // FxPos: 0..1
    property Value: single read FValue;
    constructor Create(const SampleRate: Single;isLFO:boolean);override;
end;


implementation

{ TOscillator }

uses DAV_Math,Math;

constructor TXBaseOscillator.Create(const SampleRate: Single;isLFO:boolean);
begin
  FFrequency  := 1000;
  FSAndHLevel:=0;
  FStairCaseLevel:=0;
  FPinky:=NIL;
  FWaveshape:= wsNone;
  FIsLfo:=IsLFO;
  Randomize;
  Self.SampleRate := SampleRate;
end;

destructor TXBaseOscillator.Destroy;
begin
  if FPinky<>NIL then FPinky.Free;
  inherited;
end;


procedure TXBaseOscillator.OnZeroCrossed;
begin
  FSAndHLevel:=2*random-1;
  FStairCaseLevel:=FStairCaseLevel- 1 / 6;
  if (FStairCaseLevel<=-1) then FStairCaseLevel:=1;
end;

function TXBaseOscillator.ValueAt(FxPos:single):Single;
VAR pos,pinknoise,whitenoise:single;
begin
  case FWaveShape of
    wsNone       : result:=0;
    wsNoise :  begin
                 whiteNoise:=(2 * random - 1) ;
                 if Fpinky = NIL then
                   FPinky:=TFastPinkNoiseGenerator.Create;
                 pinkNoise:=FPinky.ProcessSample32;
                 result:=whitenoise * NoiseColor / 127 + pinknoise * (127-NoiseColor)/ 127;
               end;
    wsSAndH      : result:=FSAndHLevel;
    wsStaircase  : result:=FStairCaseLevel;
    wsSine       : result:=sin(2*pi*FxPos);
    wsSquare     : if FXPos<0.5 then result:=1 else result:=-1;
    wsTriangle   : begin
                     if Fxpos<0.25 then result:=4*Fxpos
                     else if Fxpos<0.75 then result:=2-4*Fxpos
                     else result:=-4+4*Fxpos;
                   end;
    wsSawup      : if FxPos<0.5 then result:=2*FxPos else result:=-2+2*FxPos;
    wsSawDown    : if FxPos<0.5 then result:=-2*FxPos else result:=2-2*FxPos;
  end;
end;

procedure TXBaseOscillator.SampleRateChanged;
begin
 FSampleReci := 1 / FSampleRate;
end;

procedure TXBaseOscillator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
   FFrequency := Value;
end;

procedure TXBaseOscillator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TXOscillatorBlep }

// http://www.martin-finke.de/blog/articles/audio-plugins-018-polyblep-oscillator/

function TXOscillatorBlep.ValueAt(FxPos:single): Single;
function fmod(s:single): single;
begin
  result:=s-trunc(s);
end;
begin
  result:=inherited;
  if FisLfo then exit;
  case FWaveShape of
    wsSawup :  result:=result - PolyBlep(fmod(FxPos+0.5));
    wsSawDown: result:=result + PolyBlep(fmod(FxPos+0.5));
      wsSquare: begin
                result:=result + PolyBlep(FxPos);
                result:=result - PolyBlep(fmod(FxPos + 1-0.5));
              end;
  end;
end;

constructor TXOscillator.Create(const SampleRate: Single;isLFO:boolean);
begin
  inherited;
  FXPos:=0;
  FPulseWidth:=0.5;
end;

function TXOscillator.Process(VAR ZeroCrossed:boolean): single;
begin
  Fxpos:=Fxpos+ FSampleReci* FFrequency;
  ZeroCrossed:=Fxpos>=1;
  if ZeroCrossed then
  begin
    Fxpos:=Fxpos-1;
    OnZeroCrossed;
    if DoWTF then UpdateWTF;
  end;
  if DoWTF then
    result:=FWTF.ValueAt(self,FxPos)
  else
    result:=ValueAt(FxPos);
  FValue:=result;
end;

function TXOscillator.PulseWidthAdjusted(fxPos:single):single;
begin
  if DoWTF then result:=fxPos
  else if fxPos<fPulseWidth then
    result:=fxpos*0.5 / fPulseWidth
       else
    result:=0.5+ 0.5*(fxPos-fPulseWidth) / (1 - fPulseWidth);
end;

procedure TXOscillator.ResetPhase;
begin
  FxPos:=0;
end;

procedure TXOscillator.UpdateWTF;
begin
  FWTF.t1.Start:=max(0,0.49-0.5*FWTFWidth);
  FWTF.t1.Eind:=min(0.5,0.51-0.5*FWTFWidth);
  FWTF.t2.Start:=max(0.5,0.49+0.5*FWTFWidth);
  FWTF.t2.Eind:=min(1,0.51+0.5*FWTFWidth);
  FWTF.t1.CalcFx(self,true);
  FWTF.t2.CalcFx(self,false);
end;

function TXOscillator.ValueAt(FxPos: single): Single;
begin
  result:=inherited ValueAt(PulseWidthAdjusted(FxPos));
end;

procedure TXOscillator.SetPulseWidth(value: single);
begin
  if value<0.05 then value:=0.05;
  if value>0.95 then value:=0.95;
  FPulseWidth:=value;
  // 0.5 window = 0 0.05 window = 1, 0.95 window = 1
  FWTFWidth:=2*abs(value-0.5)/0.9;
end;

function TXOscillatorBlep.PolyBlep(t:single):single;
VAR dt:single;
begin
    dt := FSampleReci* FFrequency;
    // 0 <= t < 1
    if (t < dt) then
    begin
        t :=t / dt;
        result:= 2*t - t*t - 1.0;
    end
    // -1 < t < 0
    else if (t > 1.0 - dt) then
    begin
        t := (t - 1.0) / dt;
        result:= t*t + 2*t + 1.0;
    end
    // 0 otherwise
    else result:=0.0;
end;

{ TWTFDef }

procedure TWTFDef.CalcFx(osc:TXOscillator;up:boolean);
VAR FWaveShape:TWaveShape;
begin
  FWaveShape:=osc.WaveShape;
  if up then
  begin
    StartFx:=osc.ValueAt(Start);
    osc.WaveShape:=osc.WTFWaveShape;
    EindFx :=osc.ValueAt(Eind);
    osc.WaveShape:=FWaveShape;
  end
  else
  begin
    osc.WaveShape:=osc.WTFWaveShape;
    StartFx:=osc.ValueAt(Start);
    osc.WaveShape:=FWaveShape;
    EindFx :=osc.ValueAt(Eind);
  end;
end;

function TWTFDef.ValueAtInterpolate(FXPos: single): single;
begin
  result:=StartFX+ (FXPos-Start)*(EindFx-StartFx)/(Eind-Start);
end;

{ TWTF }

function TWTF.ValueAt(osc: TXOscillator; FXpos: single): single;
VAR FWaveShape:TWaveShape;
begin
  if (osc.FWTFWidth>0.03) and (FXpos >= osc.FWTF.t1.Start) and (FXpos < osc.FWTF.t2.Eind) then
  begin
    if (FXpos < osc.FWTF.t1.Eind) then
      result:=osc.FWTF.t1.ValueAtInterpolate(FxPos)
    else if (FXpos < osc.FWTF.t2.Start) then
    begin
      FWaveShape:=osc.WaveShape;
      osc.WaveShape:=osc.WTFWaveShape;
      result:=osc.ValueAt(FxPos);
      osc.WaveShape:=FWaveShape;
    end
    else
      result:=osc.FWTF.t2.ValueAtInterpolate(FxPos)
  end
  else
    result:=osc.ValueAt(FxPos)
end;

end.
