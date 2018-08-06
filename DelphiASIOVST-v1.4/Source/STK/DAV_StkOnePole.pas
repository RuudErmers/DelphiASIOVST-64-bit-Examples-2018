unit DAV_StkOnePole;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ Stk one-pole filter class.

  This protected filter subclass implements a one-pole digital filter. A method
  is provided for setting the pole position along the real axis of the z-plane
  while maintaining a constant peak filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkOnePole = class(TStkFilter)
  public
    // Default constructor creates a first-order low-pass filter.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which sets the pole position during instantiation.
    constructor Create(const SampleRate, thePole: Single); reintroduce; overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure SetB0(const Value: Single);

    // Set the a[1] coefficient value.
    procedure SetA1(const Value: Single);

    // Set the pole position in the z-plane.
  {
    This method sets the pole position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive pole value produces a low-pass filter, while a negative
    pole value produces a high-pass filter.  This method does not
    affect the filter \e gain value.
  }
    procedure SetPole(const Value: Single);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Processes 'SampleFrames' samples inplace
    procedure Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
  end;

implementation

constructor TStkOnePole.Create(const SampleRate: Single);
var
  a: array[0..1] of Single;
  b: Single;
begin
  inherited Create(SampleRate);
  B := 0.1;
  A[0] := 1.0;
  A[1] := -0.9;
  inherited setCoefficients(1, @B, 2, @A);
end;

constructor TStkOnePole.Create(const SampleRate, thePole: Single);
var
  a: array[0..1] of Single;
  b: Single;
begin
  inherited Create(SampleRate);
  A[0] := 1.0;
  A[1] := -0.9;
  // Normalize coefficients for peak unity gain.
  B := (1.0 - abs(thePole));
  A[1] := -thePole;
  inherited SetCoefficients(1, @B, 2, @A);
end;

destructor TStkOnePole.Destroy;
begin
  inherited Destroy;
end;

procedure TStkOnePole.Clear;
begin
  inherited Clear;
end;

procedure TStkOnePole.setB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkOnePole.setA1(const Value: Single);
begin
 PDAV4SingleArray(FA)^[1] := Value;
end;

procedure TStkOnePole.SetPole(const Value: Single);
begin
  // Normalize coefficients for peak unity gain.
  FB^[0] := (1.0 - abs(Value));
  PDAV4SingleArray(FA)^[1] := -Value;
end;

function TStkOnePole.Tick(const Sample: Single): Single;
begin
  FInputs^[0] := Gain * Sample;
  FOutputs^[0] := FB^[0] * FInputs^[0] -
    PDAV4SingleArray(FA)^[1] * PDAV4SingleArray(FOutputs)^[1];
  PDAV4SingleArray(FOutputs)^[1] := FOutputs^[0];
  Result := FOutputs^[0];
end;

procedure TStkOnePole.Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample: integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Data^[Sample] := Tick(Data^[Sample]);
end;

end.
