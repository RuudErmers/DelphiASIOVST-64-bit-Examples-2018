unit DAV_StkInstrument;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK instrument abstract base class.

   This class provides a common interface for all STK instruments.
}
interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Types, DAV_StkCommon;

type
  TStkInstrument = class(TStk)
  protected
    FLastOutput: Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Value: Single); virtual; abstract;
    function GetFrequency: Single; virtual; abstract;
  public
    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); virtual; abstract;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); virtual; abstract;

    // Compute one output sample.
    function Tick: Single; overload; virtual;

    // Computer VectorSize outputs and return them in Vector.
    procedure Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer); overload; virtual;

    property LastOutput: Single read FLastOutput;
    property Frequency: Single read GetFrequency write SetFrequency;
  end;

  TStkControlableInstrument = class(TStkInstrument)
  public
    // Perform the control change specified by number and value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); virtual; abstract;
  end;

implementation

function TStkInstrument.Tick: Single;
begin
  Result := 0;
end;

procedure TStkInstrument.Tick(const Data: PDavSingleFixedArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Data^[Sample] := Tick; 
end;

end.
