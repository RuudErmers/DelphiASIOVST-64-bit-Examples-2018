unit DAV_ModularManager;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types, DAV_Classes, DAV_ModularBase,
  DAV_ModularContainer, DAV_ModularPin;

type
  TCustomModularManager = class(TComponent)
  private
    FModularContainer : TModularContainer;
    FChanged          : Boolean;
    FUpdateCount      : Integer;
    FOnChange         : TNotifyEvent;
    function GetModule(Index: Integer): TCustomModularBase;
    function GetModuleCount: Integer;
    function GetModuleItem(Index: Integer): TCustomModularItem;
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Clear;
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);

    function AddModule(Module: TCustomModularBase): TCustomModularItem; overload;
    function AddModule(Module: TCustomModularBase; X, Y: Integer): TCustomModularItem; overload;
    procedure RemoveModule(Module: TCustomModularBase);

    procedure Process(const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray;
      SampleFrames: Integer);

    property ModuleCount: Integer read GetModuleCount;
    property Module[Index: Integer]: TCustomModularBase read GetModule;
    property ModuleItem[Index: Integer]: TCustomModularItem read GetModuleItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TModularManager = class(TCustomModularManager)
  published
    property ModuleCount;
    property OnChange;
  end;

implementation

uses
  Math;

resourcestring
  RCStrUnpairedUpdate = 'Please call BeginUpdate before calling EndUpdate';

{ TCustomModularManager }

constructor TCustomModularManager.Create(AOwner: TComponent);
begin
 inherited;
 FModularContainer := TModularContainer.Create;

 // initialize some variables
 FUpdateCount := 0;
end;

destructor TCustomModularManager.Destroy;
begin
 FreeAndNil(FModularContainer);
 inherited;
end;

procedure TCustomModularManager.Changed;
begin
 FChanged := True;
 if (FUpdateCount = 0) and assigned(FOnChange)
  then FOnChange(Self);
end;

procedure TCustomModularManager.BeginUpdate;
begin
 inc(FUpdateCount);
end;

procedure TCustomModularManager.EndUpdate;
begin
 if FUpdateCount = 0
  then raise Exception.Create(RCStrUnpairedUpdate);

 dec(FUpdateCount);

 if (FUpdateCount = 0) and FChanged then
  begin
   if assigned(FOnChange)
    then FOnChange(Self);

   FChanged := False;
  end;
end;

function TCustomModularManager.AddModule(Module: TCustomModularBase): TCustomModularItem;
begin
 Result := FModularContainer.AddModule(Module);
 Changed;
end;

function TCustomModularManager.AddModule(Module: TCustomModularBase; X, Y: Integer): TCustomModularItem;
begin
 Result := FModularContainer.AddModule(Module);
 with Result do
  begin
   Result.Left := X;
   Result.Top := Y;
  end;
end;

procedure TCustomModularManager.RemoveModule(Module: TCustomModularBase);
begin
 FModularContainer.RemoveModule(Module);
 Changed;
end;

procedure TCustomModularManager.Clear;
begin
 // yet todo
end;

procedure TCustomModularManager.LoadFromFile(FileName: TFileName);
begin
 // yet todo
end;

procedure TCustomModularManager.SaveToFile(FileName: TFileName);
begin
 // yet todo
end;

function TCustomModularManager.GetModule(Index: Integer): TCustomModularBase;
begin
 if (Index >= 0) and (Index < ModuleCount)
  then result := FModularContainer[Index].Module
  else result := nil;
end;

function TCustomModularManager.GetModuleCount: Integer;
begin
 result := FModularContainer.ModuleCount;
end;

function TCustomModularManager.GetModuleItem(Index: Integer): TCustomModularItem;
begin
 if (Index >= 0) and (Index < ModuleCount)
  then result := FModularContainer[Index]
  else result := nil;
end;

procedure TCustomModularManager.Process(const InBuffer,
  OutBuffer: TDAVArrayOfSingleDynArray; SampleFrames: Integer);
var
  Channel: Integer;
begin
 with FModularContainer do
  begin
   // copy input channel data
   for Channel := 0 to min(InputPinCount, Length(InBuffer)) - 1 do
    begin
     PinInput[Channel].BufferSize := SampleFrames;
     Move(InBuffer[Channel, 0], PinInput[Channel].BufferAsSingleArray[0], SampleFrames);
    end;

   // copy output channel data
   for Channel := 0 to min(FModularContainer.OutputPinCount, Length(OutBuffer)) - 1 do
    begin
     PinOutput[Channel].BufferSize := SampleFrames;
     Move(OutBuffer[Channel, 0], PinOutput[Channel].BufferAsSingleArray[0], SampleFrames);
    end;

   // now process the module container
   FModularContainer.ProcessModule;
  end;
end;

end.
