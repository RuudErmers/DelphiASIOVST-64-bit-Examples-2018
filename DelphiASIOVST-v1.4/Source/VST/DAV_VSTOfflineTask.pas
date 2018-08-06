unit DAV_VSTOfflineTask;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_VSTEffect;

type
  TCustomVstOfflineTask = class(TPersistent)
  private
    function GetInputBuffer: Pointer;
    function GetNumFramesToProcess: Double;
    function GetOutputBuffer: Pointer;
    function GetOutputText: string;
    function GetPositionToProcessFrom: Double;
    function GetProcessName: string;
    function GetProgressText: string;
    function GetReadCount: LongInt;
    function GetReadPosition: Double;
    function GetSizeInputBuffer: LongInt;
    function GetSizeOutputBuffer: LongInt;
    function GetWriteCount: LongInt;
    function GetWritePosition: Double;
    procedure SetDestinationSampleRate(Value: Double);
    procedure SetInputBuffer(const Value: Pointer);
    procedure SetMaxFramesToWrite(Value: Double);
    procedure SetNumDestinationChannels(Value: LongInt);
    procedure SetNumFramesInSourceFile(Value: Double);
    procedure SetNumFramesToProcess(Value: Double);
    procedure SetNumSourceChannels(Value: LongInt);
    procedure SetOutputBuffer(const Value: Pointer);
    procedure SetOutputText(const Value: string);
    procedure SetProcessName(const Value: string);
    procedure SetProgressText(const Value: string);
    procedure SetReadCount(const Value: LongInt);
    procedure SetReadPosition(const Value: Double);
    procedure SetSizeInputBuffer(Value: LongInt);
    procedure SetSizeOutputBuffer(Value: LongInt);
    procedure SetSourceSampleRate(Value: Double);
    procedure SetWriteCount(const Value: LongInt);
    procedure SetWritePosition(const Value: Double);
    procedure SetPositionToProcessFrom(const Value: Double);
    function GetMaxFramesToWrite: Double;
    function GetExtraBuffer: Pointer;
    procedure SetExtraBuffer(const Value: Pointer);
    function GetIndex: LongInt;
    function GetValue: LongInt;
    procedure SetValue(const Value: LongInt);
    procedure SetIndex(const Value: LongInt);
    function GetNumFramesInSourceFile: Double;
    function GetSourceSampleRate: Double;
    function GetDestinationSampleRate: Double;
    function GetNumSourceChannels: LongInt;
    function GetNumDestinationChannels: LongInt;
    function GetSourceFormat: LongInt;
    procedure SetSourceFormat(const Value: LongInt);
    function GetDestinationFormat: LongInt;
    procedure SetDestinationFormat(const Value: LongInt);
    function GetProgress: Double;
    function GetProgressMode: LongInt;
    function GetFlags: TVstOfflineTaskFlags;
    function GetHostOwned: Pointer;
    function GetPlugOwned: Pointer;
    procedure SetHostOwned(const Value: Pointer);
    procedure SetPlugOwned(const Value: Pointer);
    procedure SetFlags(const Value: TVstOfflineTaskFlags);
    procedure SetProgressMode(const Value: LongInt);
    procedure SetProgress(const Value: Double);
  protected
    FVstOfflineTaskRecord : PVstOfflineTaskRecord;
    FIsExternal           : Boolean;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; overload; virtual;
    constructor Create(const ExternalVstOfflineTaskRecord : PVstOfflineTaskRecord); overload; virtual;
    destructor Destroy; override;
    property ProcessName: string read GetProcessName write SetProcessName;
    property ReadPosition: Double read GetReadPosition write SetReadPosition;
    property WritePosition: Double  read GetWritePosition write SetWritePosition;
    property ReadCount: LongInt read GetReadCount write SetReadCount;
    property WriteCount: LongInt read GetWriteCount write SetWriteCount;
    property SizeInputBuffer: LongInt read GetSizeInputBuffer write SetSizeInputBuffer;
    property SizeOutputBuffer: LongInt read GetSizeOutputBuffer write SetSizeOutputBuffer;
    property InputBuffer: Pointer read GetInputBuffer write SetInputBuffer;
    property OutputBuffer: Pointer read GetOutputBuffer write SetOutputBuffer;
    property PositionToProcessFrom: Double read GetPositionToProcessFrom write SetPositionToProcessFrom;
    property NumFramesToProcess: Double read GetNumFramesToProcess write SetNumFramesToProcess;
    property MaxFramesToWrite: Double read GetMaxFramesToWrite write SetMaxFramesToWrite;
    property ExtraBuffer: Pointer read GetExtraBuffer write SetExtraBuffer;
    property Value: LongInt read GetValue write SetValue;
    property DataIndex: LongInt read GetIndex write SetIndex;
    property NumFramesInSourceFile: Double read GetNumFramesInSourceFile write SetNumFramesInSourceFile;
    property SourceSampleRate: Double read GetSourceSampleRate write SetSourceSampleRate;
    property DestinationSampleRate: Double read GetDestinationSampleRate write SetDestinationSampleRate;
    property NumSourceChannels: LongInt read GetNumSourceChannels write SetNumSourceChannels;
    property NumDestinationChannels: LongInt read GetNumDestinationChannels write SetNumDestinationChannels;
    property SourceFormat: LongInt read GetSourceFormat write SetSourceFormat;
    property DestinationFormat: LongInt read GetDestinationFormat write SetDestinationFormat;
    property OutputText: string read GetOutputText write SetOutputText;
    property Progress: Double read GetProgress write SetProgress;
    property ProgressMode: LongInt read GetProgressMode write SetProgressMode;
    property ProgressText: string read GetProgressText write SetProgressText;
    property Flags: TVstOfflineTaskFlags read GetFlags write SetFlags;
    property HostOwned: Pointer read GetHostOwned write SetHostOwned;
    property PlugOwned: Pointer read GetPlugOwned write SetPlugOwned;
  end;

  { TVstOfflineTask }

  TVstOfflineTask = class(TCustomVstOfflineTask)
  private
    function GetVstOfflineTaskRecord: TVstOfflineTaskRecord;
  public
    property VstOfflineTaskRecord: TVstOfflineTaskRecord read GetVstOfflineTaskRecord;
  published
    property ProcessName;            // set by plug
    property ReadPosition;           // set by plug/host
    property WritePosition;          // set by plug/host
    property ReadCount;              // set by plug/host
    property WriteCount;             // set by plug
    property SizeInputBuffer;        // set by host
    property SizeOutputBuffer;       // set by host
    property PositionToProcessFrom;  // set by host
    property NumFramesToProcess;     // set by host
    property MaxFramesToWrite;       // set by plug

    // other data access
    property Value;                  // set by host or plug
    property DataIndex;              // set by host or plug

    // file attributes
    property NumFramesInSourceFile;  // set by host
    property SourceSampleRate;       // set by host or plug
    property DestinationSampleRate;  // set by host or plug
    property NumSourceChannels;      // set by host or plug
    property NumDestinationChannels; // set by host or plug
    property SourceFormat;           // set by host
    property DestinationFormat;      // set by plug
    property OutputText;             // set by plug or host

    // progress notification
    property Progress;               // set by plug
    property ProgressMode;           // reserved for future
    property ProgressText;           // set by plug

    property Flags;                  // set by host and plug; see TVstOfflineTaskFlags
  end;

  TVstOfflineTaskCollectionItem = class(TCollectionItem)
  protected
    FVstOfflineTask: TVstOfflineTask;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property VstOfflineTask: TVstOfflineTask read FVstOfflineTask write FVstOfflineTask;
    property DisplayName;
  end;

implementation

{ TCustomVstOfflineTask }

procedure TCustomVstOfflineTask.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstOfflineTask
  then TCustomVstOfflineTask(Dest).FVstOfflineTaskRecord := FVstOfflineTaskRecord
  else inherited;
end;

constructor TCustomVstOfflineTask.Create;
begin
 inherited;
 FVstOfflineTaskRecord := nil;
 GetMem(FVstOfflineTaskRecord, SizeOf(TVstOfflineTaskRecord));
 FIsExternal := False;
end;

constructor TCustomVstOfflineTask.Create(const ExternalVstOfflineTaskRecord: PVstOfflineTaskRecord);
begin
 inherited Create;
 FVstOfflineTaskRecord := ExternalVstOfflineTaskRecord;
 FIsExternal := True;
end;

destructor TCustomVstOfflineTask.Destroy;
begin
 if not FIsExternal
  then Dispose(FVstOfflineTaskRecord);
 inherited;
end;

function TCustomVstOfflineTask.GetDestinationFormat: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.DestinationFormat;
end;

function TCustomVstOfflineTask.GetDestinationSampleRate: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.DestinationSampleRate;
end;

function TCustomVstOfflineTask.GetExtraBuffer: Pointer;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.ExtraBuffer;
end;

function TCustomVstOfflineTask.GetFlags: TVstOfflineTaskFlags;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.Flags;
end;

function TCustomVstOfflineTask.GetHostOwned: Pointer;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.HostOwned;
end;

function TCustomVstOfflineTask.GetIndex: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.Index;
end;

function TCustomVstOfflineTask.GetInputBuffer: Pointer;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.InputBuffer;
end;

function TCustomVstOfflineTask.GetMaxFramesToWrite: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.MaxFramesToWrite;
end;

function TCustomVstOfflineTask.GetNumDestinationChannels: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.NumDestinationChannels;
end;

function TCustomVstOfflineTask.GetNumFramesInSourceFile: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.NumFramesInSourceFile;
end;

function TCustomVstOfflineTask.GetNumFramesToProcess: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.NumFramesToProcess;
end;

function TCustomVstOfflineTask.GetNumSourceChannels: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.NumSourceChannels;
end;

function TCustomVstOfflineTask.GetOutputBuffer: Pointer;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.OutputBuffer;
end;

function TCustomVstOfflineTask.GetOutputText: string;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := string(FVstOfflineTaskRecord.OutputText);
end;

function TCustomVstOfflineTask.GetPlugOwned: Pointer;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.PlugOwned;
end;

function TCustomVstOfflineTask.GetPositionToProcessFrom: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.PositionToProcessFrom;
end;

function TCustomVstOfflineTask.GetProcessName: string;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := string(FVstOfflineTaskRecord.ProcessName);
end;

function TCustomVstOfflineTask.GetProgress: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.Progress;
end;

function TCustomVstOfflineTask.GetProgressMode: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.ProgressMode;
end;

function TCustomVstOfflineTask.GetProgressText: string;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := string(FVstOfflineTaskRecord.ProgressText);
end;

function TCustomVstOfflineTask.GetReadCount: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.ReadCount;
end;

function TCustomVstOfflineTask.GetReadPosition: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.ReadPosition;
end;

function TCustomVstOfflineTask.GetSizeInputBuffer: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.SizeInputBuffer;
end;

function TCustomVstOfflineTask.GetSizeOutputBuffer: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.SizeOutputBuffer;
end;

function TCustomVstOfflineTask.GetSourceFormat: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.SourceFormat;
end;

function TCustomVstOfflineTask.GetSourceSampleRate: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.SourceSampleRate;
end;

function TCustomVstOfflineTask.GetValue: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.Value;
end;

function TCustomVstOfflineTask.GetWriteCount: LongInt;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.WriteCount;
end;

function TCustomVstOfflineTask.GetWritePosition: Double;
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 Result := FVstOfflineTaskRecord.WritePosition;
end;

procedure TCustomVstOfflineTask.SetDestinationFormat(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.DestinationFormat := Value;
end;

procedure TCustomVstOfflineTask.SetDestinationSampleRate(Value: Double);
begin
 if Value < 0 then Value := 0;
 Assert(Assigned(FVstOfflineTaskRecord));
 if FVstOfflineTaskRecord.DestinationSampleRate <> Value
  then FVstOfflineTaskRecord.DestinationSampleRate := Value;
end;

procedure TCustomVstOfflineTask.SetExtraBuffer(const Value: Pointer);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.ExtraBuffer := Value;
end;

procedure TCustomVstOfflineTask.SetFlags(const Value: TVstOfflineTaskFlags);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.Flags := Value;
end;

procedure TCustomVstOfflineTask.SetHostOwned(const Value: Pointer);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.HostOwned := Value;
end;

procedure TCustomVstOfflineTask.SetIndex(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.Index := Value;
end;

procedure TCustomVstOfflineTask.SetInputBuffer(const Value: Pointer);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.InputBuffer := Value;
end;

procedure TCustomVstOfflineTask.SetMaxFramesToWrite(Value: Double);
begin
 if Value < -1 then Value := -1;
 Assert(Assigned(FVstOfflineTaskRecord));
 if FVstOfflineTaskRecord.MaxFramesToWrite <> Value
  then FVstOfflineTaskRecord.MaxFramesToWrite := Value;
end;

procedure TCustomVstOfflineTask.SetNumDestinationChannels(Value: LongInt);
begin
 if Value < -1 then Value := -1;
 Assert(Assigned(FVstOfflineTaskRecord));
 if FVstOfflineTaskRecord.NumDestinationChannels <> Value
  then FVstOfflineTaskRecord.NumDestinationChannels := Value;
end;

procedure TCustomVstOfflineTask.SetNumFramesInSourceFile(Value: Double);
begin
 if Value < -1 then Value := -1;
 Assert(Assigned(FVstOfflineTaskRecord));
 if FVstOfflineTaskRecord.NumFramesInSourceFile <> Value
  then FVstOfflineTaskRecord.NumFramesInSourceFile := Value;
end;

procedure TCustomVstOfflineTask.SetNumFramesToProcess(Value: Double);
begin
 if Value < -1 then Value := -1;
 Assert(Assigned(FVstOfflineTaskRecord));
 if FVstOfflineTaskRecord.NumFramesToProcess <> Value
  then FVstOfflineTaskRecord.NumFramesToProcess := Value;
end;

procedure TCustomVstOfflineTask.SetNumSourceChannels(Value: LongInt);
begin
 if Value < -1 then Value := -1;
 Assert(Assigned(FVstOfflineTaskRecord));
 if FVstOfflineTaskRecord.NumSourceChannels <> Value
  then FVstOfflineTaskRecord.NumSourceChannels := Value;
end;

procedure TCustomVstOfflineTask.SetOutputBuffer(const Value: Pointer);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.OutputBuffer := Value;
end;

procedure TCustomVstOfflineTask.SetOutputText(const Value: string);
var
  TextSize : Integer;
begin
 TextSize := Length(Value);
 if TextSize > 512 then TextSize := 512;
 Move(Value[1], FVstOfflineTaskRecord.OutputText[0], TextSize);

 // fill rest with zero
 if TextSize < 512
  then FillChar(FVstOfflineTaskRecord.OutputText[TextSize], 512 - TextSize, 0);
end;

procedure TCustomVstOfflineTask.SetPlugOwned(const Value: Pointer);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.PlugOwned := Value;
end;

procedure TCustomVstOfflineTask.SetPositionToProcessFrom(const Value: Double);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.PositionToProcessFrom := Value;
end;

procedure TCustomVstOfflineTask.SetProcessName(const Value: string);
var
  TextSize : Integer;
begin
 TextSize := Length(Value);
 if TextSize > 96 then TextSize := 96;
 Move(Value[1], FVstOfflineTaskRecord.ProcessName[0], TextSize);

 // fill rest with zero
 if TextSize < 96
  then FillChar(FVstOfflineTaskRecord.ProcessName[TextSize], 96 - TextSize, 0);
end;

procedure TCustomVstOfflineTask.SetProgress(const Value: Double);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.Progress := Value;
end;

procedure TCustomVstOfflineTask.SetProgressMode(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.ProgressMode := Value;
end;

procedure TCustomVstOfflineTask.SetProgressText(const Value: string);
var
  TextSize : Integer;
begin
 TextSize := Length(Value);
 if TextSize > 100 then TextSize := 100;
 Move(Value[1], FVstOfflineTaskRecord.ProgressText[0], TextSize);

 // fill rest with zero
 if TextSize < 100
  then FillChar(FVstOfflineTaskRecord.ProgressText[TextSize], 100 - TextSize, 0);
end;

procedure TCustomVstOfflineTask.SetReadCount(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.ReadCount := Value;
end;

procedure TCustomVstOfflineTask.SetReadPosition(const Value: Double);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.ReadPosition := Value;
end;

procedure TCustomVstOfflineTask.SetSizeInputBuffer(Value: LongInt);
begin
 if Value < -1 then Value := -1;
 if FVstOfflineTaskRecord.SizeInputBuffer <> Value
  then FVstOfflineTaskRecord.SizeInputBuffer := Value;
end;

procedure TCustomVstOfflineTask.SetSizeOutputBuffer(Value: LongInt);
begin
 if Value < -1 then Value := -1;
 if FVstOfflineTaskRecord.SizeOutputBuffer <> Value
  then FVstOfflineTaskRecord.SizeOutputBuffer := Value;
end;

procedure TCustomVstOfflineTask.SetSourceFormat(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.SourceFormat := Value;
end;

procedure TCustomVstOfflineTask.SetSourceSampleRate(Value: Double);
begin
 if Value < 0 then Value := 0;
 if FVstOfflineTaskRecord.SourceSampleRate <> Value
  then FVstOfflineTaskRecord.SourceSampleRate := Value;
end;

procedure TCustomVstOfflineTask.SetValue(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.Value := Value;
end;

procedure TCustomVstOfflineTask.SetWriteCount(const Value: LongInt);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.WriteCount := Value;
end;

procedure TCustomVstOfflineTask.SetWritePosition(const Value: Double);
begin
 Assert(Assigned(FVstOfflineTaskRecord));
 FVstOfflineTaskRecord.WritePosition := Value;
end;

{ TVstOfflineTaskCollectionItem }

constructor TVstOfflineTaskCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FVstOfflineTask := TVstOfflineTask.Create;
// DisplayName := 
end;

destructor TVstOfflineTaskCollectionItem.Destroy;
begin
 FreeAndNil(FVstOfflineTask);
 inherited;
end;

{ TVstOfflineTask }

function TVstOfflineTask.GetVstOfflineTaskRecord: TVstOfflineTaskRecord;
begin
 result := FVstOfflineTaskRecord^;
end;

end.
