unit DAV_StkSubNoise;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK sub-sampled noise generator.

  Generates a new random number every "rate" ticks using the random function.
  The quality of the random function varies from one OS to another.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkNoise;

type
  TStkSubNoise = class(TStkNoise)
  private
    // Set the sub-sampling rate.
    procedure SetSubRate(const Value: Integer);
  protected
    FCounter : Integer;
    FSubRate : Integer;
  public
    // Default constructor sets sub-sample rate to 16.
    constructor Create(const SampleRate: Single; const subRate: Integer); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Return a sub-sampled random number between -1.0 and 1.0.
    function Tick: Single; override;

    property SubRate: Integer read FSubRate write SetSubRate;
  end;

implementation

constructor TStkSubNoise.Create;
begin
  inherited Create(SampleRate);
  FSubRate := subRate;
  FCounter := FSubRate;
end;

destructor TStkSubNoise.Destroy;
begin
  inherited Destroy;
end;

procedure TStkSubNoise.SetSubRate(const Value: Integer);
begin
  if (Value > 0) then FSubRate := Value;
end;

function TStkSubNoise.Tick: Single;
begin
  FCounter := FCounter + 1;
  if (FCounter > FSubRate) then
   begin
    inherited tick;
    FCounter := 1;
   end;
  Result := lastOutput;
end;

end.
