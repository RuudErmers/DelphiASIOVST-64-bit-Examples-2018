unit DAV_StkDelay;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{  TStkDelay
   STK non-interpolating TStkDelay line class.

   This protected filter subclass implements a non-interpolating digital
   delay-line. A fixed maximum length of 4095 and a delay of zero is set using
   the default constructor.
   Alternatively, the delay and maximum length can be set during instantiation
   with an overloaded constructor.

   A non-interpolating delay line is typically used in fixed delay-length
   applications, such as for reverberation.
}

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon, DAV_StkFilter;

type
  TStkDelay = class(TStkFilter)
  private
    function GetNextOut: Single; // only for delay settings greater than zero!
    function GetEnergy: Single; // calculate and return the signal Energy in the delay-line.
    procedure SetDelay(const Value: Integer); // The valid range for ADelay is from 0 to the maximum delay-line length.
    function GetDelay: Integer;
  protected
    FInPoint  : Integer;
    FOutPoint : Integer;
    FLength   : Integer;
    FDelay    : Single;
    procedure DelayChanged(const Value: Integer); virtual;
  public
    // Default constructor creates a delay-line with maximum FLength of 4095 samples and zero TStkDelay.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which specifies the current and maximum delay-line lengths.
    constructor Create(const SampleRate, ADelay: Single; const AMaxDelay: Integer); overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the delay line.
    procedure Clear; override;

    // Return the value at \e tapDelay samples from the delay-line input.
  {
    The valid range for \e tapDelay is 1 to the delay-line FLength.
  }
    function ContentsAt(const TapDelay: Integer): Single;

    // Input one sample to the delay-line and return one output.
    function Tick(const Input: Single): Single; overload; override;
    procedure Tick(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer); overload; override;

    property NextOut: Single read GetNextOut;
    property Energy: Single read GetEnergy;
    property Delay: Integer read GetDelay write SetDelay;
    property Length: Integer read FLength;
  end;

implementation

{ TStkDelay }

constructor TStkDelay.Create(const SampleRate: Single);
begin
  Create(SampleRate, 4095, 4095);
end;

constructor TStkDelay.Create(const SampleRate, ADelay: Single; const AMaxDelay: Integer);
begin
  inherited Create(SampleRate);
  // Writing before reading allows delays from 0 to FLength - 1.
  // If we want to allow a TStkDelay of MaxDelay, we need a
  // delay-line of FLength := maxDelay + 1!
  FLength := AMaxDelay + 1;

  // We need to reallocate Inputs.
  ReallocMem(FInputs, FLength * SizeOf(Single));
  Clear;

  FInPoint := 0;
  Delay := Round(ADelay);
end;

procedure TStkDelay.DelayChanged(const Value: Integer);
begin
  if (Value > FLength - 1) then
   begin // The value is too big.
    // Force TStkDelay to maxLength.
    FOutPoint := FInPoint + 1;
    FDelay := FLength - 1;
   end
  else if (Value < 0) then
   begin
    FOutPoint := FInPoint;
    FDelay := 0;
   end
  else
   begin
    FOutPoint := FInPoint - Round(Value);  // read chases write
    FDelay := Value;
   end;

  while (FOutPoint < 0)
   do FOutPoint := FOutPoint + FLength;  // modulo maximum FLength
end;

destructor TStkDelay.Destroy;
begin
  inherited;
end;

procedure TStkDelay.Clear;
begin
 FillChar(FInputs^[0], FLength * SizeOf(Single), 0);
 if Assigned(FOutputs)
  then FOutputs^[0] := 0;
end;

function TStkDelay.ContentsAt(const TapDelay: Integer): Single;
var
  Tap, Index: Integer;
begin
  Index := tapDelay;
  if (Index > FDelay)
   then Index := Round(FDelay);
  Tap := FInPoint - Index;
  if (Tap < 0)
   then Tap := Tap + FLength; // Check for wraparound.
  Result := FInputs^[Tap];
end;

function TStkDelay.GetEnergy: Single;
var
  Index: Integer;
  t, e : Single;
begin
  e := 0;
  if (FInPoint >= FOutPoint) then
   for Index := FOutPoint to FInPoint - 1 do
    begin
     t := FInputs^[Index];
     e := e + t * t;
    end
  else
   begin
    for Index := FOutPoint to FLength - 1 do
     begin
      t := FInputs^[Index];
      e := e + t * t;
     end;
    for Index := 0 to FInPoint - 1 do
     begin
      t := FInputs^[Index];
      e := e + t * t;
     end;
   end;
  Result := e;
end;

function TStkDelay.GetDelay: Integer;
begin
  Result := Round(FDelay);
end;

function TStkDelay.GetNextOut: Single;
begin
  Result := FInputs^[FOutPoint];
end;

procedure TStkDelay.SetDelay(const Value: Integer);
begin
 if Value <> Delay
  then DelayChanged(Value);
end;

function TStkDelay.Tick(const Input: Single): Single;
begin
 FInputs^[FInPoint] := Input;
 Inc(FInPoint);

 // Check for end condition
 if (FInPoint = FLength)
  then FInPoint := FInPoint - FLength;

 // Read out next value
 FOutputs^[0] := FInputs^[FOutPoint];
 inc(FOutPoint);
 if (FOutPoint >= FLength)
  then FOutPoint := FOutPoint - FLength;
 Result := FOutputs^[0];
end;

procedure TStkDelay.Tick(const Input, Output: PDAVSingleFixedArray;
  const SampleFrames: Integer);
var
  Index: integer;
begin
  for Index := 0 to SampleFrames - 1
   do Output^[Index] := Tick(Input^[Index])
end;

end.
