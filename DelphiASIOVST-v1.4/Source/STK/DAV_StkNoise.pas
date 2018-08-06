unit DAV_StkNoise;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK noise generator.

   Generic random number generation using the random function.
   The quality of the random function varies from one OS to another.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon;

type
  TStkNoise = class(TStk)
  protected
    FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Return a random number between -1.0 and 1.0 using rand().
    function Tick: Single; overload; virtual;

    // Return VectorSize random numbers between -1.0 and 1.0 in \ Vector.
    procedure Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer); overload;

    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TStkNoise.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FLastOutput := 0.0;
end;

destructor TStkNoise.Destroy;
begin
  inherited Destroy;
end;

function TStkNoise.Tick: Single;
begin
  FLastOutput := (2.0 * random) - 1;
  Result := FLastOutput;
end;

procedure TStkNoise.Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Data^[Sample] := Tick; 
end;

end.
