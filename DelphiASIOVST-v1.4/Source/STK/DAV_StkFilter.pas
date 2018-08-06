unit DAV_StkFilter;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK filter class.

   This class implements a generic structure which can be used to create a wide
   range of filters. It can function independently or be subclassed to provide
   more specific controls based on a particular filter type.

   In particular, this class implements the standard difference equation:

   A[0] * y[n] := B[0] * x[n] + ... + B[numB] * x[n - numB] -
                  A[1] * y[n-1] - ... - A[numA] * y[n - numA]

   If A[0] is not equal to 1, the filter coeffcients are normalized by A[0].

   The gain parameter is applied at the filter input and does not affect the
   coefficient values. The default gain value is 1.0.  This structure results
   in one extra multiply per computed sample, but allows easy control of the
   overall filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon;

type
  TStkFilter = class(TStk)
  private
    function GetLastOutput: Single;
    procedure SetGain(const AGain: Single);
  protected
    FGain    : Single;
    FnumB    : Integer;
    FnumA    : Integer;
    FA, FB   : PDAVSingleFixedArray;
    FOutputs : PDAVSingleFixedArray;
    FInputs  : PDAVSingleFixedArray;
    procedure GainChanged; virtual;
  public
    constructor Create(const SampleRate: Single); overload; override;
    constructor Create(const SampleRate: Single;
      const BCoefficientCount: Integer; BCoefficients: PDAVSingleFixedArray;
      const ACoefficientCount: Integer; ACoefficients: PDAVSingleFixedArray); reintroduce; overload;

    // Class destructor.
    destructor Destroy; override;

    // Clears all internal states of the filter.
    procedure Clear; virtual;

    // Set filter coefficients.
  {
    An StkError can be thrown if either \e numB or \e numA is less than
    one, or if the A[0] coefficient is equal to zero.  If A[0] is not
    equal to 1, the filter coeffcients are normalized by A[0].
  }
    procedure SetCoefficients(const BCoefficientCount: Integer;
      BCoefficients: PDAVSingleFixedArray; const ACoefficientCount: Integer;
      ACoefficients: PDAVSingleFixedArray);

    // Set numerator coefficients.
  {
    An StkError can be thrown if \e numB is less than one.  Any
    previously set denominator coefficients are left unaffected.
    Note that the default constructor sets the single denominator
    coefficient A[0] to 1.0.
  }
    procedure SetNumerator(BCoefficientCount: Integer;
      BCoefficients: PDAVSingleFixedArray);

    // Set denominator coefficients.
  {
    An StkError can be thrown if \e numA is less than one or if the
    A[0] coefficient is equal to zero.  Previously set numerator
    coefficients are unaffected unless A[0] is not equal to 1, in
    which case all coeffcients are normalized by A[0].  Note that the
    default constructor sets the single numerator coefficient B[0]
    to 1.0.
  }
    procedure setDenominator(ACoefficientCount: Integer;
      ACoefficients: PDAVSingleFixedArray);

    // Input one sample to the filter and return one output.
    function Tick(const Input: Single): Single; overload; virtual;
    procedure Tick(const Input, Output: PDAVSingleFixedArray;
      const SampleFrames: Integer); overload; virtual;

    property LastOutput: Single read GetLastOutput; // Return the last computed output value.
    property Gain: Single read FGain write SetGain; // The gain is applied at the filter input and does not affect the coefficient values.
  end;

implementation

uses
  SysUtils;

resourcestring
  RCStrNoACoeffs = 'At least one A coefficient is mandatory!';
  RCStrNoBCoeffs = 'At least one B coefficient is mandatory!';
  RCStrFirstACoeffZero = 'The first A coeeficient must not be zero!';

{ TStkFilter }

constructor TStkFilter.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);

  // default constructor should setup for pass-through
  FnumB := 1;
  FnumA := 1;
  GetMem(FB, FnumB * SizeOf(Single));
  GetMem(FA, FnumA * SizeOf(Single));
  GetMem(FInputs, FnumB * SizeOf(Single));
  GetMem(FOutputs, FnumA * SizeOf(Single));
  FGain  := 1.0;
  FB^[0] := 1.0;
  FA^[0] := 1.0;

  Clear;
end;

constructor TStkFilter.Create(const SampleRate: Single;
  const BCoefficientCount: Integer; BCoefficients: PDAVSingleFixedArray;
  const ACoefficientCount: Integer; ACoefficients: PDAVSingleFixedArray);
begin
  inherited Create(SampleRate);

  // check the arguments
  if (ACoefficientCount < 1)
   then raise Exception.Create(RCStrNoACoeffs);

  if (BCoefficientCount < 1)
   then raise Exception.Create(RCStrNoBCoeffs);

  if (ACoefficients^[0] = 0)
   then raise Exception.Create(RCStrFirstACoeffZero);

  FGain := 1.0;
  FnumB := BCoefficientCount;
  FnumA := ACoefficientCount;
  GetMem(FB, FnumB * SizeOf(Single));
  GetMem(FA, FnumA * SizeOf(Single));
  GetMem(FInputs, FnumB * SizeOf(Single));
  GetMem(FOutputs, FnumA * SizeOf(Single));
  Clear;
  SetCoefficients(FnumB, BCoefficients, FnumA, aCoefficients);
end;

destructor TStkFilter.Destroy;
begin
  inherited Destroy;
  FreeMem(FB);
  FreeMem(FA);
  FreeMem(FInputs);
  FreeMem(FOutputs);
end;

procedure TStkFilter.Clear;
begin
 FillChar(FInputs^[0], FnumB * SizeOf(Single), 0);
 FillChar(FOutputs^[0], FnumA * SizeOf(Single), 0);
end;

function TStkFilter.GetLastOutput: Single;
begin
 Result := FOutputs^[0];
end;

procedure TStkFilter.SetCoefficients(const BCoefficientCount: Integer;
  BCoefficients: PDAVSingleFixedArray; const ACoefficientCount: Integer;
  ACoefficients: PDAVSingleFixedArray);
var
  i: Integer;
  t: Double;
begin
  // check the arguments
  if (ACoefficientCount < 1)
   then raise Exception.Create(RCStrNoACoeffs);

  if (BCoefficientCount < 1)
   then raise Exception.Create(RCStrNoBCoeffs);

  if (ACoefficients^[0] = 0)
   then raise Exception.Create(RCStrFirstACoeffZero);

  // test and reallocate B coefficients
  if (BCoefficientCount > FnumB) then
   begin
    ReallocMem(FB, FnumB * SizeOf(Single));
    ReallocMem(FInputs, FnumB * SizeOf(Single));
    FillChar(FInputs^[FnumB], (BCoefficientCount - FnumB) * SizeOf(Single), 0);
    FnumB := BCoefficientCount;
   end;
  if (BCoefficientCount < FnumB) then
   begin
    FnumB := BCoefficientCount;
    ReallocMem(FB, FnumB * SizeOf(Single));
    ReallocMem(FInputs, FnumB * SizeOf(Single));
   end;

  // test and reallocate A coefficients
  if (ACoefficientCount > FnumA) then
   begin
    ReallocMem(FA, FnumA * SizeOf(Single));
    ReallocMem(FOutputs, FnumA * SizeOf(Single));
    FillChar(FOutputs^[FnumA], (ACoefficientCount - FnumA) * SizeOf(Single), 0);
    FnumA := ACoefficientCount;
   end;
  if (ACoefficientCount < FnumA) then
   begin
    FnumA := ACoefficientCount;
    ReallocMem(FA, FnumA * SizeOf(Single));
    ReallocMem(FOutputs, FnumA * SizeOf(Single));
   end;

  Move(ACoefficients^[0], FA^[0], FnumA * SizeOf(Single));
  Move(BCoefficients^[0], FB^[0], FnumB * SizeOf(Single));

  // scale coefficients by FA[0] if necessary
  if (FA^[0] <> 1.0) then
   begin
    t := 1 / FA^[0];
    for i := 0 to FnumA - 1 do FA^[i] := FA^[i] * t;
    for i := 0 to FnumB - 1 do FB^[i] := FB^[i] * t;
   end;
end;

procedure TStkFilter.SetDenominator(ACoefficientCount: Integer;
  ACoefficients: PDAVSingleFixedArray);
var
  i: Integer;
  t: Double;
begin
  // check the arguments
  if (ACoefficientCount < 1)
   then raise Exception.Create(RCStrNoACoeffs);

  if (ACoefficients^[0] = 0)
   then raise Exception.Create(RCStrFirstACoeffZero);

  // test and reallocate A coefficients
  if (ACoefficientCount > FnumA) then
   begin
    ReallocMem(FA, FnumA * SizeOf(Single));
    ReallocMem(FOutputs, FnumA * SizeOf(Single));
    FillChar(FOutputs^[FnumA], (ACoefficientCount - FnumA) * SizeOf(Single), 0);
    FnumA := ACoefficientCount;
   end;
  if (ACoefficientCount < FnumA) then
   begin
    FnumA := ACoefficientCount;
    ReallocMem(FA, FnumA * SizeOf(Single));
    ReallocMem(FOutputs, FnumA * SizeOf(Single));
   end;

  Move(ACoefficients^[0], FA^[0], FnumA * SizeOf(Single));

  // scale coefficients by FA[0] if necessary
  if (FA^[0] <> 1.0) then
   begin
    t := 1 / FA^[0];
    for i := 0 to FnumA - 1 do FA^[i] := FA^[i] * t;
    for i := 0 to FnumB - 1 do FB^[i] := FB^[i] * t;
   end;
end;

procedure TStkFilter.SetGain(const AGain: Single);
begin
 if FGain <> AGain then
  begin
   FGain := AGain;
   GainChanged;
  end;
end;

procedure TStkFilter.GainChanged;
begin
 // nothing here yet
end;

procedure TStkFilter.setNumerator(BCoefficientCount: Integer;
  BCoefficients: PDAVSingleFixedArray);
begin
  // check the arguments
  if (BCoefficientCount < 1)
   then raise Exception.Create(RCStrNoBCoeffs);

  // test and reallocate B coefficients
  if (BCoefficientCount > FnumB) then
   begin
    ReallocMem(FB, FnumB * SizeOf(Single));
    ReallocMem(FInputs, FnumB * SizeOf(Single));
    FillChar(FInputs^[FnumB], (BCoefficientCount - FnumB) * SizeOf(Single), 0);
    FnumB := BCoefficientCount;
   end;
  if (BCoefficientCount < FnumB) then
   begin
    FnumB := BCoefficientCount;
    ReallocMem(FB, FnumB * SizeOf(Single));
    ReallocMem(FInputs, FnumB * SizeOf(Single));
   end;

  Move(BCoefficients^[0], FB^[0], FnumB * SizeOf(Single));
end;

procedure TStkFilter.Tick(const Input, Output: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Sample: integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Output^[Sample] := Tick(Input^[Sample])
end;

function TStkFilter.Tick(const Input: Single): Single;
var
  i: Integer;
begin
  FOutputs^[0] := 0.0;
  FInputs^[0] := FGain * Input;
  for i := FnumB - 1 downto 1 do
   begin
    FOutputs^[0] := FOutputs^[0] + FB^[i] * FInputs^[i];
    FInputs^[i] := FInputs^[i - 1];
   end;
  FOutputs^[0] := FOutputs^[0] + FB^[0] * FInputs^[0];

  for i := FnumA - 1 downto 1 do
   begin
    FOutputs^[0] := FOutputs^[0] - FA^[i] * FOutputs^[i];
    FOutputs^[i] := FOutputs^[i - 1];
   end;
  Result := FOutputs^[0];
end;

end.
