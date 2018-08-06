unit DAV_StkReedTable;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK reed table class.

  This class implements a simple one breakpoint, non-linear reed function, as
  described by Smith (1986).  This function is based on a memoryless non-linear
  spring model of the reed (the reed mass is ignored) which saturates when the
  reed collides with the mouthpiece facing.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon;

type
  TStkReedTable = class(TStk)
  private
    // Set the table FOffSet value.
  {
    The table offset roughly corresponds to the size
    of the initial reed tip opening (a greater offset
    represents a smaller opening).
  }
    procedure SetOffset(const Value: Single);

    // Set the table slope value.
  {
   The table slope roughly corresponds to the reed
   stiffness (a greater slope represents a harder
   reed).
  }
    procedure SetSlope(const Value: Single);

  protected
    FOffSet     : Single;
    FSlope      : Single;
    FLastOutput : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Return the function value for \e input.
  {
    The function input represents the differential
    pressure across the reeds.
  }
    function Tick(const Input: Single): Single; overload; virtual;

    // Processes 'SampleFrames' samples in-place
    procedure Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer); overload; virtual;

    property OffSet: Single read FOffSet write SetOffset;
    property Slope: Single read FSlope write SetSlope;
    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TStkReedTable.Create;
begin
  inherited Create(SampleRate);
  FOffSet := 0.6;  // FOffSet is a bias, related to reed rest position.
  FSlope := -0.8;  // FSlope corresponds loosely to reed stiffness.
end;

destructor TStkReedTable.Destroy;
begin
  inherited Destroy;
end;

procedure TStkReedTable.SetOffset(const Value: Single);
begin
  FOffSet := Value;
end;

procedure TStkReedTable.SetSlope(const Value: Single);
begin
  FSlope := Value;
end;

function TStkReedTable.Tick(const Input: Single): Single;
begin
  // The input is differential pressure across the reed.
  FLastOutput := FOffSet + (FSlope * Input);

  // If output is > 1, the reed has slammed shut and the
  // reflection function value saturates at 1.0.
  if (FLastOutput > 1.0) then
    FLastOutput := 1.0;

  // This is nearly impossible in a physical system, but
  // a reflection function value of -1.0 corresponds to
  // an open end (and no discontinuity in bore profile).
  if (FLastOutput < -1.0) then
    FLastOutput := -1.0;
  Result := FLastOutput;
end;

procedure TStkReedTable.Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer);
var
  Sample: integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Data^[Sample] := Tick(Data^[Sample]);
end;

end.
