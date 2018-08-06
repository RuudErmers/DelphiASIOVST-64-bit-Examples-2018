unit DAV_StkMultiOsc2;

interface

{$I ..\DAV_Compiler.inc}

uses
  mm_osc, Math;

type
  TStkMultiOsc2 = class(TOsc)
  private
    procedure SetMorph(const morph: Single);
    function GetMorph: Single;
    procedure SetActiveWave(i: Integer);
    function GetActiveWave: Integer;
  protected
    FWave: Integer;
    FPwm: Single;
  public
    constructor Create(const SampleRate: Integer); override;
    function Process: Single; override;

    property Morph: Single read GetMorph write SetMorph;
    property ActiveWave: Integer read GetActiveWave write SetActiveWave;
  end;

implementation

function fmod(const x: Single): Single;
begin
  Result := x - floor(x);
end;

function fpulse(x, a: Single): Single;
begin
  if (fmod(x) < a) then
    Result := -1
  else
    Result := 1;
end;

function fsaw(x, a: Single): Single;
begin
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  x := fmod(x);
  if (x < a) then
    Result := x / a * 2 - 1
  else
    Result := (1 - x) / (1 - a) * 2 - 1;
end;

function ftri(x, a: Single): Single;
begin
  x := fmod(x + 0.25);
  a := 1 - a;
  if (a < 0.00001) then
    a := 0.00001;
  if (x < 0.5) then
    x := x * 4 - 1
  else
    x := (1 - x) * 4 - 1;
  x := x / (-a);
  if (x < -1) then
    x := -1
  else if (x > 1) then
    x := 1;
  Result := x;
end;

function fpower(x, a: Single): Single;
begin
  x := fmod(x);
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  Result := power(x, exp((a - 0.5) * 10)) * 2 - 1;
end;

function fgauss(x, a: Single): Single;
begin
  x := fmod(x) * 2 - 1;
  if (a < 0.00001) then
    a := 0.00001;
  Result := exp(-x * x * (exp(a * 8) + 5)) * 2 - 1;
end;

function fdiode(x, a: Single): Single;
begin
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  a := a * 2 - 1;
  x := cos((x + 0.5) * 2 * pi) - a;
  if (x < 0) then
    x := 0;
  Result := x / (1 - a) * 2 - 1;
end;

function fsine(x, a: Single): Single;
var
  y: Single;
begin
  x := fmod(x);
  if (x < 0.5) then
    y := 1
  else
    y := -1;
  Result := sin(x * 2 * pi) * (1 - a) + a * y;
end;

function fabssine(x, a: Single): Single;
begin
  x := fmod(x);
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  Result := sin(power(x, exp((a - 0.5) * 5)) * pi) * 2 - 1;
end;

function fpulsesine(x, a: Single): Single;
begin
  if (a < 0.00001) then
    a := 0.00001;
  x := (fmod(x) - 0.5) * exp((a - 0.5) * log10(128));
  if (x < -0.5) then
    x := -0.5
  else if (x > 0.5) then
    x := 0.5;
  x := sin(x * pi * 2);
  Result := x;
end;

function fstretchsine(x, a: Single): Single;
var
  b: Single;
begin
  x := fmod(x + 0.5) * 2 - 1;
  a := (a - 0.5) * 4;
  if (a > 0) then
    a := a * 2;
  a := power(3, a);
  b := power(abs(x), a);
  if (x < 0) then
    b := -b;
  Result := -sin(b * pi);
end;

function fchirp(x, a: Single): Single;
begin
  x := fmod(x) * 2 * pi;
  a := (a - 0.5) * 4;
  if (a < 0) then
    a := a * 2;
  a := power(3, a);
  Result := sin(x / 2) * sin(a * x * x);
end;

function fsinex(x, a: Single): Single;
begin
  x := fmod(x);
  Result := 0.5 * (sin(x * 2 * pi) + sin(x * 2 * pi * (1 + a * 10)));
end;

function fabsstretchsine(x, a: Single): Single;
var
  b: Single;
begin
  x := fmod(x + 0.5) * 2 - 1;
  a := (a - 0.5) * 9;
  a := power(3, a);
  b := power(abs(x), a);
  if (x < 0) then
    b := -b;
  Result := -2 * power(sin(b * pi), 2) + 1;
end;

constructor TStkMultiOsc2.Create(const SampleRate: Integer);
begin
  Wave := 0;
  inherited Create(SampleRate);
end;

function TStkMultiOsc2.GetActiveWave: Integer;
begin
  Result := Wave;
end;

function TStkMultiOsc2.GetMorph: Single;
begin
  Result := Pwm;
end;

function TStkMultiOsc2.Process: Single;
var
  y, j: Single;
begin
  j := srate / freq;
  phase := cnt / j;
  case Wave of
    0 : y := fsine(phase, Pwm);
    1 : y := fsaw(phase, Pwm);
    2 : y := fpulse(phase, Pwm);
    3 : y := ftri(phase, Pwm);
    4 : y := tmp;
    5 : y := random * 2 - 1;
    6 : y := fpower(phase, Pwm);
    7 : y := fgauss(phase, Pwm);
    8 : y := fdiode(phase, Pwm);
    9 : y := fstretchsine(phase, Pwm);
    10 : y := fpulsesine(phase, Pwm);
    11 : y := fabssine(phase, Pwm);
    12 : y := fabsstretchsine(phase, Pwm);
    13 : y := fchirp(phase, Pwm);

    14 : if ((Pwm = 1) or (phase < Pwm)) then
        y := sin(pi * phase / Pwm)
      else
        y := -sin(pi * (phase - Pwm) / (1 - Pwm));
    15 : if ((Pwm = 1) or (phase < Pwm)) then
        y := phase / Pwm
      else
        y := ((phase - Pwm) / (1 - Pwm)) - 1;
    16 : if (Pwm = 0) then
       begin
        if (phase < 0.5 * (Pwm + 1)) then
          y := -2 * (phase - Pwm) / (1 - Pwm)
        else
          y := -1 + (2 * phase - (Pwm + 1)) / (1 - Pwm);
       end else
      if (Pwm = 1) then
       begin
        if (phase < 0.5 * Pwm) then
          y := 2 * phase / Pwm
        else
          y := 1 - (2 * phase - Pwm) / Pwm;
       end else
      if (phase < Pwm / 2) then
        y := 2 * phase / Pwm
      else
      if ((phase >= Pwm / 2) and (phase < Pwm)) then
        y := 1 - (2 * phase - Pwm) / Pwm
      else if ((phase >= Pwm) and (phase < 0.5 * (Pwm + 1))) then
        y := -2 * (phase - Pwm) / (1 - Pwm)
      else
        y := -1 + (2 * phase - (Pwm + 1)) / (1 - Pwm);
    17 : y := fsine(phase * (Pwm), 0) * ((1 - Pwm) + 1) - (1 - Pwm);
    18 : y := fsinex(phase, Pwm);
  else y := 0;
   end;
  cnt := cnt + 1;
  while (cnt > j) do
   begin
    cnt := cnt - j;
    tmp := random * 2 - 1;
   end;
  Result := y;
end;

procedure TStkMultiOsc2.SetActiveWave(i: Integer);
begin
  Wave := i;
end;

procedure TStkMultiOsc2.SetMorph(morph: Single);
begin
  if morph > 1 then
    morph := 1
  else if morph < 0 then
    morph := 0;
  Pwm := morph;
end;

end.
