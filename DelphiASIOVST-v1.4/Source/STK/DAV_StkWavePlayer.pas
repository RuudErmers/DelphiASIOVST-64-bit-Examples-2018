unit DAV_StkWavePlayer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ TStkWavePlayer }

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, SysUtils, DAV_Common, DAV_StkCommon, DAV_StkLfo, DAV_AudioFileWAV,
  DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_AudioData;

type
  TStkWavePlayer = class(TStkLFO)
  private
    FOneShot    : Boolean;
    FSampleData : PSingle;
    FLoopstart  : Integer;
    FLoopend    : Integer;
    FStart      : Single;
    FEnd        : Single;

    pfofs       : Single;

    procedure SetOneShot(const Value: Boolean);
  protected
    FSize       : Longint;
    FInvSize    : Single;
    FLength     : Single;
    FIsFinished : Boolean;

    procedure OneShotChanged; virtual;

    // Sets the playback rate
    procedure SetRate(const Value: Single);

    // Set the loop points
    procedure SetLoop(StartPosition, EndPosition: Integer);

    // Seek to position in file
    procedure SetPos(const Value: Longint);
  public
    // Class constructor, taking the desired number of modes to create.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor, load file on create
    constructor Create(const SampleRate: Single; const FileName: String); reintroduce; overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Reset; virtual;

    // Loads a WAV audio file
    procedure LoadFile(const FileName: TFileName);

    // Compute one output sample.
    function Tick: Single; override;

    property OneShot: Boolean read FOneShot write SetOneShot;
    property Size: Longint read FSize;
    property Length: Single read FLength;
    property IsFinished: Boolean read FIsFinished;
  end;

implementation

constructor TStkWavePlayer.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  pfofs       := 0;
  FSize       := 0;
  FSampleData := nil;
  FLoopstart  := 0;
  FLoopend    := 0;
  FStart      := 0;
  FEnd        := 0;
end;

constructor TStkWavePlayer.Create(const SampleRate: Single; const FileName: String);
begin
  if SampleRate <= 0
   then raise Exception.Create('Samplerate must be larger than zero');
  inherited Create(SampleRate);
  FSize       := 0;
  FSampleData := nil;
  FLoopstart  := 0;
  FLoopend    := 0;
  FStart      := 0;
  FEnd        := 0;
  LoadFile(FileName);
end;

destructor TStkWavePlayer.Destroy;
begin
  inherited Destroy;
  if assigned(FSampleData) then Dispose(FSampleData);
end;

procedure TStkWavePlayer.Reset;
begin
  SetPos(0);
end;

procedure TStkWavePlayer.LoadFile(const FileName: TFileName);
var
  ADC : TAudioDataCollection32;
begin
  ADC := TAudioDataCollection32.Create(nil);
  with ADC do
   try
    LoadFromFile(FileName);
    FSize := ADC.SampleFrames;
    FInvSize := 1 / FSize;
    GetMem(FSampleData, FSize * SizeOf(Single));
    Move(ADC[0].ChannelDataPointer^[0], FSampleData^, FSize * SizeOf(Single));
    FLength := FSize * FSampleRateInv;
    FLoopstart := 0;
    FLoopend := FSize - 1;
    FStart := 0;
    FEnd := (FSize - 1) * FInvSize;
    FOneShot := True;
    FIsFinished := False;
   finally
    FreeAndNil(ADC);
   end;
end;

function TStkWavePlayer.Tick: Single;
var
  x, y   : Longint;
  q      : Single;
  s1, s2 : psingle;
begin
  phase := pfofs + phase + (FFreq * FSampleRateInv);
  if FOneShot then
   begin
    FIsFinished := (phase >= 1);
    if FIsFinished then
     begin
      Result := 0;
      exit;
     end;
   end
  else
    while (phase >= 1) do phase := phase - 1;
  q := (phase * (FEnd - FStart) + FStart);
  if q > 1 then
    q := 1
  else if q < 0 then
    q := 0;
  q := q * FSize;
  x := round(q);
  q := q - x;
  s1 := psingle(Longint(FSampleData) + 4 * x);
  y := x + 1;
  if y > FLoopend then
    y := FLoopstart;
  s2 := psingle(Longint(FSampleData) + 4 * y);
  Result := s1^ * (1 - q) + s2^ * q;
end;

procedure TStkWavePlayer.SetLoop(StartPosition, EndPosition: Integer);
begin
  if StartPosition < 0 then StartPosition := 0
  else if StartPosition > FSize - 1
   then StartPosition := FSize - 1;
  if EndPosition < 1
   then EndPosition := 1
  else if EndPosition > FSize - 1
   then EndPosition := FSize - 1;
  if StartPosition > EndPosition
   then StartPosition := 0;
  FLoopstart := StartPosition;
  FLoopend := EndPosition;
  FStart := FLoopStart * FInvSize;
  FEnd := FLoopEnd * FInvSize;
end;

procedure TStkWavePlayer.SetPos(const Value: Longint);
begin
  Phase := Value * FInvSize;
end;

procedure TStkWavePlayer.SetRate(const Value: Single);
begin
  Frequency := Value;
end;

procedure TStkWavePlayer.SetOneShot(const Value: Boolean);
begin
 if FOneShot <> Value then
  begin
   FOneShot := Value;
   OneShotChanged;
  end;
end;

procedure TStkWavePlayer.OneShotChanged;
begin
 // nothing todo yet!
end;


end.
