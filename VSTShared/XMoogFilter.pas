unit XMoogFilter;

interface

uses USampleValue;

type TLadderFilter = class
private
  FCutoff,FResonance, FSampleRate:double;
  procedure SetCutoff(value:double);virtual;abstract;
  procedure SetResonance(value:double);virtual;abstract;
  procedure SetSampleRate(value:double);
public
  constructor Create(samplerate:double);virtual;
  function Process(input:TSampleValue):TSampleValue;virtual;abstract;
  procedure Reset;virtual;
  property Cutoff: double  write SetCutoff;      // 0..20000
  property Resonance: double  write SetResonance; // 0..1
  property SampleRate: double write SetSampleRate;
end;

type TMusicDspMoog = class(TLadderFilter)             // doet het FANTASTISCH...
private
	stage,delay:array[0..3] of TSampleValue;
	p, k, t1, t2:double;
public
  procedure Reset;override;
  constructor Create(samplerate:double);override;
  function Process(input:TSampleValue):TSampleValue;override;
  procedure SetCutoff(value:double);override;
  procedure SetResonance(value:double);override;
end;

type TMoogFilter = TMusicDspMoog; //TMusicDspMoog;

implementation

{ TMoogFilter }

uses Math,SysUtils;

constructor TMusicDspMoog.Create(samplerate: double);
begin
  inherited;
  Reset;
end;

function TMusicDspMoog.Process(input: TSampleValue): TSampleValue;
VAR temp,delayi,stagei,stageimin1:TSampleValue;
    i:integer;
begin
  input.bound;
//  input := input - Fresonance * stage[3];
  temp:=stage[3];
  temp.mul(-FResonance);
  input.add(temp);
  // Four cascaded one-pole filters (bilinear transform)

//  stage[0] := input * p + delay[0]  * p - k * stage[0];
//  stage[1] := stage[0] * p + delay[1] * p - k * stage[1];
//  stage[2] := stage[1] * p + delay[2] * p - k * stage[2];
//  stage[3] := stage[2] * p + delay[3] * p - k * stage[3];

  for i:=0 to 3 do
  begin
    if i=0 then stageimin1:=input else stageimin1:=stage[i-1];
    delayi:=delay[i];
    stagei:=stage[i];
    stageimin1.mul(p);
    delayi.mul(p);
    stagei.mul(-k);
    stage[i].zero;
    stage[i].add(stageimin1);
    stage[i].add(delayi);
    stage[i].add(stagei);
  end;
  // Clipping band-limited sigmoid
//  stage[3] :=stage[3]- (stage[3] * stage[3] * stage[3]) / 6.0;

  temp.one;
  for i:=0 to 2 do
   temp.mul(stage[3]);
  temp.mul(-1/6.0);
  stage[3].add(temp);

  delay[0] := input;
  delay[1] := stage[0];
  delay[2] := stage[1];
  delay[3] := stage[2];

  result:= stage[3];

end;

procedure TMusicDspMoog.Reset;
VAR i:integer;
begin
  for i:=0 to 3 do stage[i].zero;
  for i:=0 to 3 do delay[i].zero;
  p:=0;
  SetCutoff(FCutoff);
end;

procedure TMusicDspMoog.SetCutoff(value: double);
begin
  if value<0 then raise Exception.Create('Negative Cutoff?');
  value:=2.0 * value / FsampleRate;
  if FCutoff<>value then
  begin
   	Fcutoff := value;
		p := Fcutoff * (1.8 - 0.8 * Fcutoff);
		k := 2.0 * sin(Fcutoff * pi * 0.5) - 1.0;
		t1 := (1.0 - p) * 1.386249;
		t2 := 12.0 + t1 * t1;
    SetResonance(Fresonance);
  end;
end;

procedure TMusicDspMoog.SetResonance(value: double);
begin
  if (value>0.99) then value := 0.99;
  Fresonance := value * (t2 + 6.0 * t1) / (t2 - 6.0 * t1);
end;

{ TLadderFilter }

constructor TLadderFilter.Create(samplerate:double);
begin
  FSampleRate:=samplerate;
  FCutoff:=1000;
  Fresonance:=0;
end;

procedure TLadderFilter.Reset;
begin

end;

procedure TLadderFilter.SetSampleRate(value: double);
begin
  FSampleRate:=value;
  SetCutoff(FCutoff);
end;

{ THuovilainenMoog }


end.

