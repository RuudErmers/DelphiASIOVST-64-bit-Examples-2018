unit AdditionalChunks;

{$I DAV_Compiler.inc}

interface

uses
  Classes, Graphics, DAV_Types, DAV_ChunkClasses, DAV_DifferentialEvolution,
  DAV_FixedPoint;

type
  TCircleChunk = class(TDefinedChunk)
  protected
    FAlpha   : Byte;
    FCenterX : TFixed24Dot8;
    FCenterY : TFixed24Dot8;
    FColor   : TColor;
    FRadius  : TFixed24Dot8;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property Radius: TFixed24Dot8 read FRadius write FRadius;
    property CenterX: TFixed24Dot8 read FCenterX write FCenterX;
    property CenterY: TFixed24Dot8 read FCenterY write FCenterY;
    property Color: TColor read FColor write FColor;
    property Alpha: Byte read FAlpha write FAlpha;
  end;

  TCircleChunkContainer = class(TChunkContainer)
  private
    function GetCircle(Index: Integer): TCircleChunk;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;

    property Circle[Index: Integer]: TCircleChunk read GetCircle; default;
  end;

  TRoundedRectangleChunk = class(TDefinedChunk)
  private
    FRight: TFixed24Dot8;
    FBottom: TFixed24Dot8;
    FTop: TFixed24Dot8;
    FLeft: TFixed24Dot8;
  protected
    FAlpha   : Byte;
    FColor   : TColor;
    FRadius  : TFixed24Dot8;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property Radius: TFixed24Dot8 read FRadius write FRadius;
    property Left: TFixed24Dot8 read FLeft write FLeft;
    property Top: TFixed24Dot8 read FTop write FTop;
    property Right: TFixed24Dot8 read FRight write FRight;
    property Bottom: TFixed24Dot8 read FBottom write FBottom;
    property Color: TColor read FColor write FColor;
    property Alpha: Byte read FAlpha write FAlpha;
  end;

  TRoundedRectangleChunkContainer = class(TChunkContainer)
  private
    function GetRoundedRectangle(Index: Integer): TRoundedRectangleChunk;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;

    property RoundedRectangle[Index: Integer]: TRoundedRectangleChunk read GetRoundedRectangle; default;
  end;

  TPopulationChunk = class(TDefinedChunk)
  private
    function GetVariableCount: Cardinal;
    function GetVariable(Index: Integer): Double;
    procedure SetVariable(Index: Integer; const Value: Double);
    procedure SetVariableCount(const Value: Cardinal);
  protected
    FData : TDifferentialEvolutionPopulation;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property VariableCount: Cardinal read GetVariableCount write SetVariableCount;
    property Variable[Index: Integer]: Double read GetVariable write SetVariable;
  end;

  TPopulationChunkContainer = class(TChunkContainer)
  private
    function GetPopulation(Index: Integer): TPopulationChunk;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;

    property Population[Index: Integer]: TPopulationChunk read GetPopulation; default;
  end;

implementation

uses
  SysUtils;

{ TCircleChunk }

constructor TCircleChunk.Create;
begin
 inherited;
 FChunkSize := 3 * SizeOf(TFixed24Dot8) + SizeOf(TColor) + SizeOf(Byte);
end;

class function TCircleChunk.GetClassChunkName: TChunkName;
begin
 Result := 'circ';
end;

procedure TCircleChunk.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Assert(Stream.Size >= FChunkSize);
   Read(FRadius, SizeOf(TFixed24Dot8));
   Read(FCenterX, SizeOf(TFixed24Dot8));
   Read(FCenterY, SizeOf(TFixed24Dot8));
   Read(FAlpha, SizeOf(Byte));
   Read(FColor, SizeOf(TColor));
  end;
end;

procedure TCircleChunk.SaveToStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Write(FRadius, SizeOf(TFixed24Dot8));
   Write(FCenterX, SizeOf(TFixed24Dot8));
   Write(FCenterY, SizeOf(TFixed24Dot8));
   Write(FAlpha, SizeOf(Byte));
   Write(FColor, SizeOf(TColor));
  end;
end;


{ TCircleChunkContainer }

constructor TCircleChunkContainer.Create;
begin
 RegisterChunkClass(TCircleChunk);
 inherited;
end;

function TCircleChunkContainer.GetCircle(Index: Integer): TCircleChunk;
begin
 if (Index >= 0) and (Index < Count) and
  (FChunkList[Index] is TCircleChunk)
  then Result := TCircleChunk(FChunkList[Index])
  else Result := nil;
end;

class function TCircleChunkContainer.GetClassChunkName: TChunkName;
begin
 Result := 'Crcl';
end;


{ TPopulationChunk }

constructor TPopulationChunk.Create;
begin
 inherited;
 FChunkSize := 0;
end;

class function TPopulationChunk.GetClassChunkName: TChunkName;
begin
 Result := 'popu';
end;

function TPopulationChunk.GetVariable(Index: Integer): Double;
begin
 if (Index >= 0) and (Index < Length(FData))
  then Result := FData[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TPopulationChunk.GetVariableCount: Cardinal;
begin
 Result := Length(FData);
end;

procedure TPopulationChunk.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if (ChunkSize > Size) or ((ChunkSize mod SizeOf(Double)) <> 0)
    then raise Exception.Create('Corrupt data!');

   SetLength(FData, ChunkSize div SizeOf(Double));

   // actually read the data
   Read(FData[0], ChunkSize);
  end;
end;

procedure TPopulationChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FData) * SizeOf(Double);
 inherited;

 // write the data
 Stream.Write(FData[0], ChunkSize);
end;


procedure TPopulationChunk.SetVariable(Index: Integer; const Value: Double);
begin
 if (Index >= 0) and (Index < Length(FData))
  then FData[Index] := Value
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TPopulationChunk.SetVariableCount(const Value: Cardinal);
begin
 SetLength(FData, Value);
end;


{ TRoundedRectangleChunk }

constructor TRoundedRectangleChunk.Create;
begin
 inherited;
 FChunkSize := 5 * SizeOf(TFixed24Dot8) + SizeOf(TColor) + SizeOf(Byte);
end;

class function TRoundedRectangleChunk.GetClassChunkName: TChunkName;
begin
 Result := 'rore';
end;

procedure TRoundedRectangleChunk.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Assert(Stream.Size >= FChunkSize);
   Read(FRadius, SizeOf(TFixed24Dot8));
   Read(FLeft, SizeOf(TFixed24Dot8));
   Read(FTop, SizeOf(TFixed24Dot8));
   Read(FRight, SizeOf(TFixed24Dot8));
   Read(FBottom, SizeOf(TFixed24Dot8));
   Read(FAlpha, SizeOf(Byte));
   Read(FColor, SizeOf(TColor));
  end;
end;

procedure TRoundedRectangleChunk.SaveToStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Write(FRadius, SizeOf(TFixed24Dot8));
   Write(FLeft, SizeOf(TFixed24Dot8));
   Write(FTop, SizeOf(TFixed24Dot8));
   Write(FRight, SizeOf(TFixed24Dot8));
   Write(FBottom, SizeOf(TFixed24Dot8));
   Write(FAlpha, SizeOf(Byte));
   Write(FColor, SizeOf(TColor));
  end;
end;

{ TRoundedRectangleChunkContainer }

constructor TRoundedRectangleChunkContainer.Create;
begin
 RegisterChunkClass(TRoundedRectangleChunk);
 inherited;
end;

function TRoundedRectangleChunkContainer.GetRoundedRectangle(
  Index: Integer): TRoundedRectangleChunk;
begin
 if (Index >= 0) and (Index < Count) and
  (FChunkList[Index] is TRoundedRectangleChunk)
  then Result := TRoundedRectangleChunk(FChunkList[Index])
  else Result := nil;
end;

class function TRoundedRectangleChunkContainer.GetClassChunkName: TChunkName;
begin
 Result := 'Rore';
end;


{ TPopulationChunkContainer }

constructor TPopulationChunkContainer.Create;
begin
 RegisterChunkClass(TPopulationChunk);
 inherited;
end;

class function TPopulationChunkContainer.GetClassChunkName: TChunkName;
begin
 Result := 'POPS';
end;

function TPopulationChunkContainer.GetPopulation(
  Index: Integer): TPopulationChunk;
begin
 if (Index >= 0) and (Index < Count) and
  (FChunkList[Index] is TPopulationChunk)
  then Result := TPopulationChunk(FChunkList[Index])
  else Result := nil;
end;

end.
