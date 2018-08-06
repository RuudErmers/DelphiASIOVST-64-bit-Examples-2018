unit DAV_StkDelayA;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK allpass interpolating delay line class.

   This delay subclass implements a fractional-length digital delay-line using
   a first-order allpass filter. A fixed maximum length of 4095 and a delay of
   0.5 is set using the default constructor.  Alternatively, the delay and
   maximum length can be set during instantiation with an overloaded
   constructor.

   An allpass filter has unity magnitude gain but variable phase delay
   properties, making it useful in achieving fractional delays without
   affecting a signal's frequency magnitude response.  In order to achieve a
   maximally flat phase delay response, the minimum delay possible in this
   implementation is limited to a value of 0.5.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkDelay;

type
  TStkDelayA = class(TStkDelay)
  private
    // Set the delay-line length
    // The valid range for the value is from 0.5 to the maximum delay-line length.
    procedure SetDelay(const Value: Single);

    // Return the current delay-line length.
    function GetDelay: Single;
  protected
    FAlpha      : Single;
    FCoeff      : Single;
    FApInput    : Single;
    FNextOutput : Single;
    FDoNextOut  : Boolean;
  public
    // Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which specifies the current and maximum delay-line lengths.
    constructor Create(const SampleRate, ADelay: Single; const AMaxDelay: longint); overload; override;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the delay line.
    procedure Clear; override;

    // Return the value which will be output by the next call to tick().
    // This method is valid only for delay settings greater than zero!
    function NextOut: Single;

    // Input one sample to the delay-line and return one output.
    function Tick(const Sample: Single): Single; override;
  published
    property Delay: Single read GetDelay write SetDelay;
  end;

implementation

{ TStkDelayA }

procedure TStkDelayA.Clear;
begin
  inherited Clear;
  FApInput := 0.0;
end;

constructor TStkDelayA.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  setDelay(0.5);
  FApInput := 0.0;
  FDoNextOut := True;
end;

constructor TStkDelayA.Create(const SampleRate, ADelay: Single; const AMaxDelay: Integer);
begin
  inherited Create(SampleRate);
   // Writing before reading allows delays from 0 to length-1.
  FLength := AMaxDelay + 1;

  if (FLength > 4096) then
   begin
    // We need to delete the previously allocated inputs.
    Dispose(FInputs);
    GetMem(FInputs, SizeOf(Single) * FLength);
    Clear;
   end;

  FInPoint := 0;
  setDelay(ADelay);
  FDoNextOut := True;
end;

destructor TStkDelayA.Destroy;
begin
  inherited Destroy;
end;

function TStkDelayA.GetDelay: Single;
begin
 Result := inherited Delay;
end;

function TStkDelayA.nextOut: Single;
begin
  if (FDoNextOut) then
   begin
    // Do allpass interpolation delay.
    FNextOutput := -FCoeff * FOutputs^[0];
    FNextOutput := FNextOutput + FApInput + (FCoeff * FInputs[FOutPoint]);
    FDoNextOut := False;
   end;
  Result := FNextOutput;
end;

procedure TStkDelayA.SetDelay(const Value: Single);
var
  OutPointer: Single;
begin
 if (Value > length - 1) then
  begin
   // Force delay to maxLength
   OutPointer := FInPoint + 1.0;
   Delay := length - 1;
  end
 else if (Value < 0.5) then
  begin
   OutPointer := FInPoint + 0.4999999999;
   Delay := 0.5;
  end
 else
  begin
   OutPointer := FInPoint - Value + 1.0;     // OutPoint chases inpoint
   Delay := Value;
  end;

  if (OutPointer < 0)
   then OutPointer := OutPointer + length;  // modulo maximum length

  FOutPoint := round(OutPointer);        // integer part
  FAlpha := 1.0 + FOutPoint - OutPointer; // fractional part

  if (FAlpha < 0.5) then
   begin
    // The optimal range for FAlpha is about 0.5 - 1.5 in order to
    // achieve the flattest phase delay response.
    Inc(FOutPoint);
    if (FOutPoint >= length)
     then FOutPoint := FOutPoint - length;
    FAlpha := FAlpha + 1;
   end;
  FCoeff := (1 - FAlpha) / (1 + FAlpha);         // coefficient for all pass
end;

function TStkDelayA.Tick(const Sample: Single): Single;
begin
  FInputs^[FInPoint] := Sample;
  Inc(FInPoint);

 // Increment input pointer modulo length.
  if (FInPoint = length) then
    FInPoint := FInPoint - length;

  FOutputs^[0] := nextOut;
  FDoNextOut := True;

 // Save the allpass input and increment modulo length.
  FApInput := FInputs[FOutPoint];
  if (FOutPoint = length) then FOutPoint := FOutPoint - length;
  Result := FOutputs^[0];
  Inc(FOutPoint);
end;

end.
