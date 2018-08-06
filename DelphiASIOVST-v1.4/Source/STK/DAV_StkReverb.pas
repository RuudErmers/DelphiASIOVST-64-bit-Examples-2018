unit DAV_StkReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_StkCommon;

type
  TStkReverb = class(TStk)
  private
    procedure SetEffectMix(const Value: Single);
    function GetLastOutput: Single;
  protected
    FLastOutput : array [0..1] of Single;
    FEffectMix  : Single;
    function IsPrime(const Number: Integer): Boolean;
    procedure EffectMixChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear; virtual; abstract;

    // Abstract Tick function ... must be implemented in subclasses.
    function Tick(const Input: Single): Single; overload; virtual;

    // Take VectorSize inputs, compute the same Number of outputs and return them in \e Vector.
    procedure Tick(const Input: PDAVSingleFixedArray; out Output: PDAVSingleFixedArray; const SampleFrames: Cardinal); overload; virtual;


    property EffectMix: Single read FEffectMix write SetEffectMix; // (0.0 = input only, 1.0 = reverb only).
    property LastOutputLeft: Single read FLastOutput[0];  // Return the last left output value.
    property LastOutputRight: Single read FLastOutput[1]; // Return the last right output value.
    property LastOutput: Single read GetLastOutput;       // Return the last output value.
  end;

implementation

constructor TStkReverb.Create(const SampleRate: Single = 44100);
begin
  inherited Create(SampleRate);
end;

destructor TStkReverb.Destroy;
begin
  inherited Destroy;
end;

procedure TStkReverb.SetEffectMix(const Value: Single);
begin
 if EffectMix <> Value then
  begin
   FEffectMix := Value;
   EffectMixChanged;
  end;
end;

procedure TStkReverb.EffectMixChanged;
begin
 // nothing in here yet...
end;

function TStkReverb.GetLastOutput: Single;
begin
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkReverb.IsPrime(const Number: Integer): Boolean;
var
  i: Integer;
begin
 Result := False;
 if (Number = 2) then
  begin
   Result := True;
   Exit;
  end;
 if (Number and 1 > 0) then
  begin
   i := 3;
   repeat
     if ((Number mod i) = 0) then Exit;
     i := i + 2;
   until (i >= Round(Sqrt(Number) + 1));
   Result := True;
  end;
end;

function TStkReverb.Tick(const Input: Single): Single;
begin
  Result := 0;
end;

procedure TStkReverb.Tick(const Input: PDAVSingleFixedArray;
  out Output: PDAVSingleFixedArray; const SampleFrames: Cardinal);
var
  i: Integer;
begin
 for i := 0 to SampleFrames - 1
  do Output^[i] := Tick(Input^[i]);
end;

end.
