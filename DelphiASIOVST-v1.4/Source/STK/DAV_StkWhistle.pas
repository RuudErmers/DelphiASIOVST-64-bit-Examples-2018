unit DAV_StkWhistle;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK police/referee whistle instrument class.

  This class implements a hybrid physical/spectral model of a police
  whistle (a la Cook).

  Control Change Numbers:
    - FNoise Gain = 4
    - Fipple Modulation Frequency = 11
    - Fipple Modulation Gain = 1
    - Blowing Frequency Modulation = 2
    - Volume = 128
}
interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkSphere, DAV_StkVector3d,
  DAV_StkNoise, DAV_StkLfo, DAV_StkOnePole, DAV_StkEnvelope;

type
  TStkWhistle = class(TStkControlableInstrument)
  protected
    FTempVector    : TStkVector3D;
    FTempVectorP   : TStkVector3D;
    FOnePole       : TStkOnePole;
    FNoise         : TStkNoise;
    FEnvelope      : TStkEnvelope;
    FPea           : TStkSphere;
    FBumper        : TStkSphere;
    FCan           : TStkSphere;           // Declare a Spherical "can".
    FSine          : TStkLfo;
    FBaseFrequency : Single;
    FmaxPressure   : Single;
    FNoiseGain     : Single;
    FFippleFreqMod : Single;
    FippleGainMod  : Single;
    FBlowFreqMod   : Single;
    FTickSize      : Single;
    FCanLoss       : Single;
    FSubSample     : Integer;
    FSubSampCount  : Integer;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const frequency: Single); override;

  public
    // Class constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Apply breath velocity to instrument with given amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure StopBlowing(const Rate: Single);

    // Start a note with the given frequency and amplitude.
    procedure NoteOn(const frequency, amplitude: Single); override;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(const amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils, Math;

const
  CCanRadius = 100;
  CPeaRadius = 30;
  CBumpRadius = 5;
  CNormCanLoss = 0.97;
  CSlowCanLoss = 0.90;
  CGravity = 20.0;
  CNormTickSize = 0.004;
  CSlowTickSize = 0.0001;
  CEnvRate = 0.001;

constructor TStkWhistle.Create;
begin
  inherited Create(SampleRate);
  FTempVector := TStkVector3D.Create(0, 0, 0);
  FCan := TStkSphere.Create(CCanRadius);
  FPea := TStkSphere.Create(CPeaRadius);
  FBumper := TStkSphere.Create(CBumpRadius);
  FSine := TStkLfo.Create(SampleRate);
  FSine.Frequency := 2800.0;
  FCan.SetPosition(0, 0, 0); // set can location
  FCan.SetVelocity(0, 0, 0); // and the velocity
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FOnePole := TStkOnePole.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);
  FOnePole.setPole(0.95);  // 0.99
  FBumper.setPosition(0.0, CCanRadius - CBumpRadius, 0);
  FBumper.setPosition(0.0, CCanRadius - CBumpRadius, 0);
  FPea.setPosition(0, CCanRadius / 2, 0);
  FPea.setVelocity(35, 15, 0);

  FEnvelope.Rate := CEnvRate;
  FEnvelope.keyOn;

  FFippleFreqMod := 0.5;
  FippleGainMod := 0.5;
  FBlowFreqMod := 0.25;
  FNoiseGain := 0.125;
  FmaxPressure := 0.0;
  FBaseFrequency := 2000;

  FTickSize := CNormTickSize;
  FCanLoss := CNormCanLoss;

  FSubSample := 1;
  FSubSampCount := FSubSample;
end;

destructor TStkWhistle.Destroy;
begin
 FreeAndNil(FTempVector);
 FreeAndNil(FCan);
 FreeAndNil(FPea);
 FreeAndNil(FBumper);
 FreeAndNil(FSine);
 FreeAndNil(FEnvelope);
 FreeAndNil(FOnePole);
 FreeAndNil(FNoise);
 inherited Destroy;
end;

procedure TStkWhistle.Clear;
begin
end;

procedure TStkWhistle.SetFrequency;
var
  freakency: Single;
begin
  freakency := frequency * 4;  // the Whistle is a transposing instrument
  if (frequency <= 0.0) then
    freakency := 220.0;
  FBaseFrequency := freakency;
end;

procedure TStkWhistle.StartBlowing;
begin
  FEnvelope.Rate := CEnvRate;
  FEnvelope.Target := Amplitude;
end;

procedure TStkWhistle.StopBlowing;
begin
  FEnvelope.Rate := rate;
  FEnvelope.KeyOff;
end;

procedure TStkWhistle.NoteOn;
begin
  SetFrequency(frequency);
  StartBlowing(Amplitude * 2.0, Amplitude * 0.2);
end;

procedure TStkWhistle.NoteOff;
begin
  StopBlowing(amplitude * 0.02);
end;

function TStkWhistle.Tick: Single;
var
(*
  soundMix       : Single;
  tempFreq       : Single;
  temp, temp1    : Double;
  temp2, tempX   : Double;
  tempY, phi     : Double;
  cosphi, sinphi : Double;
*)
  dmod, envout   : Double;
  gain           : Double;
begin
  envOut := 0;
  gain := 0.5;
  FSubSampCount := FSubSampCount - 1;
  if (FSubSampCount <= 0) then
   begin
    FTempVectorP := FPea.Position;
    FSubSampCount := FSubSample;
{   temp := FBumper.isInside(FTempVectorP);
    envOut:=FEnvelope.Tick;

    if (temp < (CBumpRadius + CPeaRadius)) then
    begin
      tempX:=envOut * FTickSize * 2000 * FNoise.Tick;
      tempY:=-envOut * FTickSize * 1000 * (1.0 + FNoise.Tick);
      FPea.addVelocity(tempX,tempY,0);
      FPea.Tick(FTickSize);
    end;

{    dmod :=exp(-temp * 0.01);  // exp. distance falloff of fipple/FPea effect
    temp:=FOnePole.Tick(dmod);  // smooth it a little
    gain:=(1.0 - (FippleGainMod*0.5)) + (2.0 * FippleGainMod * temp);
    gain :=gain*gain;        // squared distance/gain
    tempFreq:=1.0 + FFippleFreqMod*(0.25-temp) + FBlowFreqMod*(envOut-1.0);
    tempFreq :=tempfreq* FBaseFrequency;

    FSine.SetFrequency(tempFreq);

    FTempVectorP:=FPea.getPosition;
    temp:=FCan.isInside(FTempVectorP);
    temp :=-temp;       // We know (hope) it's inside, just how much??
    if (temp < (CPeaRadius * 1.25)) then
    begin
      FPea.getVelocity(FTempVector);  //  This is the FCan/FPea collision
      tempX:=FTempVectorP.getX;  // calculation.  Could probably
      tempY:=FTempVectorP.getY;  // simplify using tables, etc.
      phi:=-arctan2(tempY,tempX);
      cosphi:=cos(phi);
      sinphi:=sin(phi);
      temp1:=(cosphi*FTempVector.getX) - (sinphi*FTempVector.getY);
      temp2:=(sinphi*FTempVector.getX) + (cosphi*FTempVector.getY);
      temp1:=-temp1;
      tempX:=(cosphi*temp1) + (sinphi*temp2);
      tempY:=(-sinphi*temp1) + (cosphi*temp2);
      FPea.setVelocity(tempX, tempY, 0);
      FPea.Tick(FTickSize);
      FPea.setVelocity(tempX*FCanLoss, tempY*FCanLoss, 0);
      FPea.Tick(FTickSize);
    end;

    temp:=FTempVectorP.getLength;
    if (temp > 0.01) then
    begin
      tempX:=FTempVectorP.getX;
      tempY:=FTempVectorP.getY;
      phi:=arctan2(tempY,tempX);
      phi :=phi+( 0.3 * temp / CCanRadius);
      cosphi:=cos(phi);
      sinphi:=sin(phi);
      tempX:=3.0 * temp * cosphi;
      tempY:=3.0 * temp * sinphi;
    end
    else begin
      tempX:=0.0;
      tempY:=0.0;
    end;

    temp:=(0.9 + 0.1*FSubSample*FNoise.Tick) * envOut * 0.6 * FTickSize;
    FPea.addVelocity(temp * tempX,
    (temp*tempY) - (CGravity*FTickSize),0);
    FPea.Tick(FTickSize);
 }
    //    FBumper.Tick(0.0);
   end;
{
  temp:=envOut * envOut * gain / 2;
  soundMix:=temp * (FSine.Tick + (FNoiseGain*FNoise.Tick));
  lastOutput:=0.25 * soundMix; // should probably do one-zero filter here
 }
  Result := lastOutput;
end;

procedure TStkWhistle.ControlChange;
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMidiNoiseLevel) then // 4
    FNoiseGain := 0.25 * norm
  else if (number = CMidiModFrequency) then // 11
    FFippleFreqMod := norm
  else if (number = CMidiModWheel) then // 1
    FippleGainMod := norm
  else if (number = CMidiAfterTouchCont) then // 128
    FEnvelope.Target := norm * 2.0
  else if (number = CMidiBreath) then // 2
    FBlowFreqMod := norm * 0.5
  else if (number = CMidiSustain) then // 64
    if (Value < 1.0) then FSubSample := 1;
end;

end.
