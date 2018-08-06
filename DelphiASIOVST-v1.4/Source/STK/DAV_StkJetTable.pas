unit DAV_StkJetTable;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK Jet Table class.

   This class implements a flue jet non-linear function, computed by a
   polynomial calculation. Contrary to the name, this is not a "table".

   Consult Fletcher and Rossing, Karjalainen, Cook, and others for more
   information.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon;

type
  TStkJetTable = class(TStk)
  protected
    FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    function Tick(const Input: Single): Single; overload;
    function Tick(Vector: PSingle; VectorSize: Integer): PSingle; overload;

    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TStkJetTable.Create;
begin
  inherited Create(SampleRate);
  FLastOutput := 0.0;
end;

destructor TStkJetTable.Destroy;
begin
  inherited Destroy;
end;

function TStkJetTable.Tick(const Input: Single): Single;
begin
  // Perform "table lookup" using a polynomial
  // calculation (x^3 - x), which approximates
  // the jet sigmoid behavior.
  FLastOutput := Input * (Input * Input - 1.0);

  // Saturate at +/- 1.0.
  if (FLastOutput > 1.0) then
    FLastOutput := 1.0;
  if (FLastOutput < -1.0) then
    FLastOutput := -1.0;
  Result := FLastOutput;
end;

function TStkJetTable.Tick(Vector: PSingle; VectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := Vector;
  for i := 0 to VectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := Vector;
end;

end.

