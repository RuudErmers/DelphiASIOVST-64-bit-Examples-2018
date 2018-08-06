unit DAV_StkTwoPole;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK two-pole filter class.

  This protected filter subclass implements a two-pole digital filter. A method
  is provided for creating a resonance in the frequency response while
  maintaining a nearly constant filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkTwoPole = class(TStkFilter)
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Clears the internal states of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure SetB0(const Value: Single);

    // Set the a[1] coefficient value.
    procedure SetA1(const Value: Single);

    // Set the a[2] coefficient value.
    procedure SetA2(const Value: Single);

    // Sets the filter coefficients for a resonance at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  If \e normalize is true,
    the coefficients are then normalized to produce unity gain at \e
    frequency (the actual maximum filter gain tends to be slightly
    greater than unity when \e radius is not close to one).  The
    resulting filter frequency response has a resonance at the given
    \e frequency.  The closer the poles are to the unit-circle (\e
    radius close to one), the narrower the resulting resonance width.
    An unstable filter will result for \e radius >= 1.0.  For a better
    resonance filter, use a BiQuad filter. \sa BiQuad filter class
  }
    procedure SetResonance(const Frequency, Radius: Single; const Normalize: Boolean = False);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    procedure Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
  end;

implementation

constructor TStkTwoPole.Create(const SampleRate: Single);
var
  b: Single;
  a: array[0..2] of Single;
begin
  inherited Create(SampleRate);
  B := 1.0;
  A[0] := 1;
  A[1] := 0;
  A[2] := 0;
  inherited setCoefficients(1, @B, 3, @A);
end;

destructor TStkTwoPole.Destroy;
begin
  inherited Destroy;
end;

procedure TStkTwoPole.Clear;
begin
  inherited Clear;
end;

procedure TStkTwoPole.SetB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkTwoPole.SetA1(const Value: Single);
begin
 PDAV4SingleArray(FA)^[1] := Value;
end;

procedure TStkTwoPole.SetA2(const Value: Single);
begin
 PDAV4SingleArray(FA)^[2] := Value;
end;

procedure TStkTwoPole.setResonance;
var
  real, imag: Single;
begin
  PDAV4SingleArray(FA)^[2] := sqr(Radius);
  PDAV4SingleArray(FA)^[1] := 2.0 * radius * cos(2 * Pi * Frequency * FSampleRateInv);

  if Normalize then
   begin
    // Normalize the filter gain ... not terribly efficient.
    real := 1 - radius + (PDAV4SingleArray(FA)^[2] - radius) *
      cos(4 * Pi * Frequency * FSampleRateInv);
    imag := (PDAV4SingleArray(FA)^[2] - radius) *
      sin(4 * Pi * Frequency * FSampleRateInv);
    FB^[0] := sqrt(real * real + imag * imag);
   end;
end;

function TStkTwoPole.Tick(const Sample: Single): Single;
begin
 FInputs^[0] := FGain * Sample;
 FOutputs^[0] := FB^[0] * FInputs^[0] -
   PDAV4SingleArray(FA)^[2] * PDAV4SingleArray(FOutputs)^[2] -
   PDAV4SingleArray(FA)^[1] * PDAV4SingleArray(FOutputs)^[1];

 Move(PDAV4SingleArray(FOutputs)^[0],
      PDAV4SingleArray(FOutputs)^[1], 2 * SizeOf(Single));

 Result := FOutputs^[0];

end;

procedure TStkTwoPole.Tick(const Data: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Data^[Sample] := Tick(Data^[Sample]);
end;

end.
