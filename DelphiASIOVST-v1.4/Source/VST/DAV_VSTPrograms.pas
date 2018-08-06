unit DAV_VSTPrograms;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_VSTBasicModule;

type
  TChunkEvent = procedure(Sender: TObject; const Index : Integer; const isPreset : Boolean) of object;

  TCustomVstProgram = class(TCollectionItem)
  private
    FDisplayName      : string;
    FVSTModule        : TBasicVSTModule;
    FOnInitialize     : TNotifyEvent;
    FOnStoreChunk     : TChunkEvent;
    FOnLoadChunk      : TChunkEvent;

    procedure SetParameter(AIndex: Integer; AValue: Single);
    function GetParameter(AIndex: Integer): Single;
  protected
    FParameter        : array of Single;
    FChunkData        : TMemoryStream;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
    function ParameterCount: Integer;
    procedure SetParameterCount(const Value: Integer);
    procedure SetParameters(const Parameters: array of Single);
    procedure CopyParameters(const ProgramNr: Integer);
    property Parameter[AIndex: Integer]: Single read GetParameter write SetParameter;
    property Chunk: TMemoryStream read FChunkData write FChunkData;
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnInitialize: TNotifyEvent read FOnInitialize write FOnInitialize;
    property OnLoadChunk: TChunkEvent read FOnLoadChunk write FOnLoadChunk;
    property OnStoreChunk: TChunkEvent read FOnStoreChunk write FOnStoreChunk;
  end;

  TVstProgram = class(TCustomVstProgram)
    property DisplayName;
    property VSTModule;
    property OnInitialize;
    property OnLoadChunk;
    property OnStoreChunk;
  end;

  TCustomVstPrograms = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TVstProgram;
    procedure SetItem(Index: Integer; const Value: TVstProgram);
    property Items[Index: Integer]: TVstProgram read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TVstProgram;
    function Insert(Index: Integer): TVstProgram;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TVSTPrograms = TCustomVstPrograms;

implementation

uses
  SysUtils, DAV_VSTEffect, DAV_VSTModuleWithPrograms;

resourcestring
  RCStrParameterMismatch = 'Parameter mismatch (%d)';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{$IFDEF FPC}
constructor TCustomVstProgram.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstProgram.Create(Collection: TCollection);
{$ENDIF}
begin
 inherited;
 FDisplayName := 'Init';
 FVSTModule := (Collection As TCustomVstPrograms).VSTModule;
 VSTModule.Effect^.numPrograms := Collection.Count;
 with TVSTModuleWithPrograms(VSTModule) do
  begin
// RE <    if not (effFlagsProgramChunks in VSTModule.Effect^.EffectFlags)
// RE <     then SetLength(FParameter, numParams)
// RE <     else FChunkData := TMemoryStream.Create;
   SetLength(FParameter, numParams);
   if (effFlagsProgramChunks in VSTModule.Effect^.EffectFlags) then FChunkData := TMemoryStream.Create;

   if CurrentProgram < 0 then CurrentProgram := 0;
  end;
end;

destructor TCustomVstProgram.Destroy;
begin
 try
  SetLength(FParameter, 0);
  FreeAndNil(FChunkData);
 finally
  inherited;
 end;
end;

function TCustomVstProgram.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstProgram.SetDisplayName(const AValue: string);
begin
 FDisplayName := Copy(AValue, 0, 50);
end;

procedure TCustomVstProgram.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
 if Dest is TCustomVstProgram then
  with TCustomVstProgram(Dest) do
   begin
    if Length(Self.FParameter) > 0 then
     begin
      SetLength(FParameter, Length(Self.FParameter));
      for i := 0 to Length(Self.FParameter) - 1
       do Parameter[i] := Self.Parameter[i];
     end;

    if Self.FChunkData.Size > 0 then
     begin
      FChunkData.Size := Self.FChunkData.Size;
      Move(Self.FChunkData.Memory^, FChunkData.Memory^, FChunkData.Size);
      FChunkData.Position := Self.FChunkData.Position;
     end;

    OnInitialize := Self.OnInitialize;
    OnStoreChunk := Self.OnStoreChunk;
    OnLoadChunk  := Self.OnLoadChunk;
    DisplayName  := Self.DisplayName;
   end
  else inherited;
end;

procedure TCustomVstProgram.CopyParameters(const ProgramNr: Integer);
begin
 with TVSTModuleWithPrograms(FVSTModule)
  do SetParameters(Programs[ProgramNr].FParameter);
end;

procedure TCustomVstProgram.SetParameter(AIndex: Integer; AValue: Single);
begin
 Assert(FVSTModule is TVSTModuleWithPrograms);
 with TVSTModuleWithPrograms(FVSTModule) do
  begin
// RE <   if effFlagsProgramChunks in Flags then exit;
   if (AIndex >= 0) and (AIndex < numParams)
    then FParameter[AIndex] := AValue
//    else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [AIndex]);
  end;
end;

function TCustomVstProgram.GetParameter(AIndex: Integer): Single;
begin
 Assert(FVSTModule is TVSTModuleWithPrograms);
 if (AIndex >= 0) and (AIndex < TVSTModuleWithPrograms(FVSTModule).numParams)
  then Result := FParameter[AIndex] else
   begin
    Result := 0;
//    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [AIndex]);
   end;
end;

procedure TCustomVstProgram.SetParameterCount(const Value: Integer);
begin
 SetLength(FParameter, Value);
end;

procedure TCustomVstProgram.SetParameters(const Parameters: array of Single);
var
  Index : Integer;
begin
 // check for no parameters
 if Length(Parameters) = 0 then Exit;

 // check for parameter mismatch
 if Length(Parameters) > ParameterCount
  then raise Exception.CreateFmt(RCStrParameterMismatch, [Length(Parameters)]);

 // update parameters
 for Index := 0 to Length(Parameters) - 1
  do Parameter[Index] := Parameters[Index];
end;

function TCustomVstProgram.ParameterCount: Integer;
begin
 Result := Length(FParameter);
end;


{ TCustomVstPrograms }

constructor TCustomVstPrograms.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TVstProgram);
 FVSTModule := TVSTModuleWithPrograms(AOwner);
end;

destructor TCustomVstPrograms.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomVstPrograms.Add: TVstProgram;
begin
  Result := TVstProgram(inherited Add);
end;

function TCustomVstPrograms.GetItem(Index: Integer): TVstProgram;
begin
 Result := TVstProgram(inherited GetItem(Index));
end;

function TCustomVstPrograms.Insert(Index: Integer): TVstProgram;
begin
 Result := TVstProgram(inherited Insert(Index));
end;

procedure TCustomVstPrograms.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TCustomVstPrograms.SetItem(Index: Integer; const Value: TVstProgram);
begin
 inherited SetItem(Index, Value);
end;

end.
