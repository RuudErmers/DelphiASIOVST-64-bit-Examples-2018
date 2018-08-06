unit DAV_ModularPin;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Variants, DAV_Types, DAV_Classes;

type
  TModularPinDataType = (mdtInteger, mdtBoolean, mdtSingle, mdtDouble);

  TCustomModularPin = class(TCollectionItem)
  private
    FDisplayName            : string;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const AValue: string); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure DisplayNameChanged; virtual;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}

    property DisplayName{$IFNDEF FPC}: string read FDisplayName write SetDisplayName{$ENDIF};
  end;

  TManagedModularIOPin = class(TCustomModularPin)
  published
    property DisplayName;
  end;

  TManagedModularIOPins = class(TOwnedCollection)
  protected
    FOnPinCountChange: TNotifyEvent;
    function IndexOf(Value: TManagedModularIOPin): Integer;
    function GetItem(Index: Integer): TManagedModularIOPin; virtual;
    procedure SetItem(Index: Integer; const Value: TManagedModularIOPin); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TManagedModularIOPin read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    function Add: TManagedModularIOPin;
    function Insert(const Index: Integer): TManagedModularIOPin;
    procedure Delete(const Index: Integer);

    property Count;
    property OnPinCountChange: TNotifyEvent read FOnPinCountChange write FOnPinCountChange;
  end;

  TCustomModularConnectPin = class(TCustomModularPin)
  private
    FOnPinConnectionChanged : TNotifyEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure PinConnected; virtual; abstract;
    procedure PinDisconnected; virtual; abstract;
    procedure PinConnectionChanged; virtual; abstract;
  public
    property OnPinConnectionChanged: TNotifyEvent read FOnPinConnectionChanged write FOnPinConnectionChanged;
  end;

  TCustomModularIOPin = class(TCustomModularConnectPin)
  private
    FDataType   : TModularPinDataType;
    FBuffer     : Pointer;
    FBufferSize : Integer;
    FSparePin   : Boolean;
    procedure SetDataType(const Value: TModularPinDataType);
    procedure SetBufferSize(const Value: Integer);
    procedure SetSparePin(const Value: Boolean);
    function GetBufferAsDoubleArray: PDAVDoubleFixedArray;
    function GetBufferAsSingleArray: PDAVSingleFixedArray;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure AllocateBuffer; virtual;
    procedure BufferSizeChanged; virtual;
    procedure DataTypeChanged; virtual;
    procedure DisplayNameChanged; virtual;
    procedure SparePinChanged; virtual;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;

    property BufferAsSingleArray: PDAVSingleFixedArray read GetBufferAsSingleArray;
    property BufferAsDoubleArray: PDAVDoubleFixedArray read GetBufferAsDoubleArray;
    property BufferSize: Integer read FBufferSize write SetBufferSize default 1;
    property Datatype: TModularPinDataType read FDataType write SetDataType default mdtSingle;
    property SparePin: Boolean read FSparePin write SetSparePin;
  end;

  TCustomModularIOPins = class(TCollection)
  private
    procedure SetSparePins(const Value: Boolean);
    procedure PinCountChanged;
  protected
    FSparePins        : Boolean;
    FOnPinCountChange : TNotifyEvent;
    function IndexOf(Value: TCustomModularIOPin): Integer;
    function GetItem(Index: Integer): TCustomModularIOPin; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomModularIOPin); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TCustomModularIOPin read GetItem write SetItem; default;
  public
    destructor Destroy; override;
    function Add: TCustomModularIOPin;
    function Insert(const Index: Integer): TCustomModularIOPin;
    procedure Delete(const Index: Integer);

    property Count;
    property SparePins: Boolean read FSparePins write SetSparePins;
    property OnPinCountChange: TNotifyEvent read FOnPinCountChange write FOnPinCountChange;
  end;

  TModularInputPins = class(TCustomModularIOPins)
  public
    constructor Create; virtual;
  end;

  TModularOutputPins = class(TCustomModularIOPins)
  public
    constructor Create; virtual;
  end;

  TCustomModularPinInput = class;
  TCustomModularPinOutput = class;

  TCustomModularPinInput = class(TCustomModularIOPin)
  protected
    FOutputPins : array of TCustomModularPinOutput;
    procedure AddOutputPin(Pin: TCustomModularPinOutput);
    procedure RemoveOutputPin(Pin: TCustomModularPinOutput);
    function IndexOf(Pin: TCustomModularPin): Integer;

    procedure PinConnected; override;
    procedure PinDisconnected; override;
    procedure PinConnectionChanged; override;
  public
    procedure Connect(Pin: TCustomModularPinOutput); virtual;
    procedure Disconnect(Pin: TCustomModularPinOutput); virtual;
  end;

  TCustomModularPinOutput = class(TCustomModularIOPin)
  private
    procedure DisconnectPin; virtual;
  protected
    FInputPin : TCustomModularPinInput;
    procedure PinConnected; override;
    procedure PinDisconnected; override;
    procedure PinConnectionChanged; override;
  public
    procedure Connect(Pin: TCustomModularPinInput); virtual;
    procedure Disconnect; virtual;

    property InputPin: TCustomModularPinInput read FInputPin;
  end;

  TModularPinInput = class(TCustomModularPinInput)
  published
    property DisplayName;
  end;

  TModularPinOutput = class(TCustomModularPinOutput)
  published
    property DisplayName;
  end;

implementation

resourcestring
  RCStrPinAlreadyRemoved = 'Pin already removed';
  RCStrOutputPinAlreadyConnected = 'Output pin already connected!';
  RCStrDataTypeUndefined = 'Data type is yet undefined!';
  RCStrDataTypeMismatch = 'Data type mismatch!';
  RCStrTriggerTypeMismatch = 'Process type mismatch';

{ TCustomModularPin }

constructor TCustomModularPin.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := 'Pin ' + IntToStr(Collection.Count);
end;

procedure TCustomModularPin.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomModularPin then
  with TCustomModularPin(Dest) do
   try
    DisplayName := Self.DisplayName;
   except
    inherited;
   end
  else inherited;
end;

function TCustomModularPin.GetDisplayName: string;
begin
 result := FDisplayName;
end;

procedure TCustomModularPin.SetDisplayName(const AValue: string);
begin
 if AValue <> FDisplayName then
  begin
   FDisplayName := AValue;
   DisplayNameChanged;
  end;
 inherited;
end;

procedure TCustomModularPin.DisplayNameChanged;
begin

end;


{ TManagedModularIOPins }

constructor TManagedModularIOPins.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner, TManagedModularIOPin);
end;

destructor TManagedModularIOPins.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TManagedModularIOPins.Add: TManagedModularIOPin;
begin
 Result := TManagedModularIOPin(inherited Add);
end;

procedure TManagedModularIOPins.Delete(const Index: Integer);
begin
 inherited Delete(Index);
end;

function TManagedModularIOPins.GetItem(Index: Integer): TManagedModularIOPin;
begin
 Result := TManagedModularIOPin(inherited GetItem(Index));
end;

function TManagedModularIOPins.IndexOf(Value: TManagedModularIOPin): Integer;
var
  i : Integer;
begin
 result := -1;
 for i := 0 to Count - 1 do
  if Items[i] = Value then
   begin
    result := i;
    exit;
   end;
end;

function TManagedModularIOPins.Insert(const Index: Integer): TManagedModularIOPin;
begin
 Result := TManagedModularIOPin(inherited Insert(Index));
end;

procedure TManagedModularIOPins.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 if assigned(FOnPinCountChange)
  then FOnPinCountChange(Self);
end;

procedure TManagedModularIOPins.SetItem(Index: Integer; const Value: TManagedModularIOPin);
begin
 inherited SetItem(Index, Value);
end;


{ TCustomModularConnectPin }

procedure TCustomModularConnectPin.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomModularConnectPin then
  with TCustomModularConnectPin(Dest) do
   begin
    FOnPinConnectionChanged := Self.FOnPinConnectionChanged
   end;
end;


{ TCustomModularIOPin }

constructor TCustomModularIOPin.Create(Collection: TCollection);
begin
 inherited;
 FDataType    := mdtSingle;
 FBufferSize  := 1;
 FBuffer      := nil;
end;

destructor TCustomModularIOPin.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function CalculateByteSizeByDataType(DataType: TModularPinDataType): Byte;
begin
 case DataType of
  mdtInteger,
  mdtSingle   : result := 4;
  mdtDouble   : result := 8;
  else raise Exception.Create(RCStrDataTypeUndefined);
 end;
end;

procedure TCustomModularIOPin.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomModularPin then
  with TCustomModularPin(Dest) do
   begin
    FDataType   := Self.FDataType;
    FBuffer     := Self.FBuffer;
    FBufferSize := Self.FBufferSize;
   end;
end;

procedure TCustomModularIOPin.AllocateBuffer;
begin
 if FSparePin
  then if assigned(FBuffer) then Dispose(FBuffer) else
  else ReallocMem(FBuffer, FBufferSize * CalculateByteSizeByDataType(FDataType));
end;

procedure TCustomModularIOPin.BufferSizeChanged;
begin
 AllocateBuffer;
end;

procedure TCustomModularIOPin.DataTypeChanged;
begin
 AllocateBuffer;
end;

procedure TCustomModularIOPin.DisplayNameChanged;
begin
 // nothing in here yet
end;

function TCustomModularIOPin.GetBufferAsDoubleArray: PDAVDoubleFixedArray;
begin
 result := PDAVDoubleFixedArray(FBuffer);
end;

function TCustomModularIOPin.GetBufferAsSingleArray: PDAVSingleFixedArray;
begin
 result := PDAVSingleFixedArray(FBuffer);
end;

procedure TCustomModularIOPin.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TCustomModularIOPin.SetDataType(const Value: TModularPinDataType);
begin
 if FDataType <> Value then
  begin
   FDataType := Value;
   DataTypeChanged;
  end;
end;

procedure TCustomModularIOPin.SetSparePin(const Value: Boolean);
begin
 if FSparePin <> Value then
  begin
   FSparePin := Value;
   SparePinChanged;
  end;
end;

procedure TCustomModularIOPin.SparePinChanged;
begin
 AllocateBuffer;
end;


{ TCustomModularIOPins }

destructor TCustomModularIOPins.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomModularIOPins.Add: TCustomModularIOPin;
begin
 Result := TCustomModularIOPin(inherited Add);
end;

procedure TCustomModularIOPins.Delete(const Index: Integer);
begin
 inherited Delete(Index);
end;

function TCustomModularIOPins.GetItem(Index: Integer): TCustomModularIOPin;
begin
 Result := TCustomModularIOPin(inherited GetItem(Index));
end;

function TCustomModularIOPins.IndexOf(Value: TCustomModularIOPin): Integer;
var
  i : Integer;
begin
 result := -1;
 for i := 0 to Count - 1 do
  if Items[i] = Value then
   begin
    result := i;
    exit;
   end;
end;

function TCustomModularIOPins.Insert(const Index: Integer): TCustomModularIOPin;
begin
 Result := TCustomModularIOPin(inherited Insert(Index));
end;

procedure TCustomModularIOPins.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 PinCountChanged;
end;

procedure TCustomModularIOPins.PinCountChanged;
begin
 if assigned(FOnPinCountChange) and assigned(Self)
  then FOnPinCountChange(Self);
end;

procedure TCustomModularIOPins.SetItem(Index: Integer; const Value: TCustomModularIOPin);
begin
 inherited SetItem(Index, Value);
end;

procedure TCustomModularIOPins.SetSparePins(const Value: Boolean);
begin
 if FSparePins <> Value then
  begin
   FSparePins := Value;
   PinCountChanged;
  end;
end;


{ TModularInputPins }

constructor TModularInputPins.Create;
begin
 inherited Create(TModularPinInput);
end;


{ TModularOutputPins }

constructor TModularOutputPins.Create;
begin
 inherited Create(TModularPinOutput);
end;


{ TCustomModularPinOutput }

procedure TCustomModularPinOutput.Connect(Pin: TCustomModularPinInput);
begin
 // make sure everything is as it seems
 assert(Pin is TCustomModularPinInput);

 // check whether the data types match
 if Pin.Datatype <> Datatype
  then raise Exception.Create(RCStrDataTypeMismatch);

 if FInputPin <> Pin then
  begin
   DisconnectPin;
   FInputPin := Pin;
   PinConnected;
  end;
end;

procedure TCustomModularPinOutput.Disconnect;
begin
 DisconnectPin;
 PinDisconnected;
end;

procedure TCustomModularPinOutput.DisconnectPin;
begin
 if assigned(FInputPin)
  then FInputPin.Disconnect(Self);
 FInputPin := nil;
end;

procedure TCustomModularPinOutput.PinConnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinOutput.PinDisconnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinOutput.PinConnectionChanged;
begin
 if assigned(FOnPinConnectionChanged)
  then FOnPinConnectionChanged(Self);
end;

{ TCustomModularPinInput }

function TCustomModularPinInput.IndexOf(Pin: TCustomModularPin): Integer;
var
  i : Integer;
begin
 result := -1;
 for i := 0 to Length(FOutputPins) - 1 do
  if FOutputPins[i] = Pin then
   begin
    result := i;
    exit;
   end;
end;

procedure TCustomModularPinInput.AddOutputPin(Pin: TCustomModularPinOutput);
begin
 SetLength(FOutputPins, Length(FOutputPins) + 1);
 FOutputPins[Length(FOutputPins) - 1] := Pin;
end;

procedure TCustomModularPinInput.RemoveOutputPin(Pin: TCustomModularPinOutput);
var
  PinIndex : Integer;
begin
 PinIndex := IndexOf(Pin);
 if PinIndex < 0
  then raise Exception.Create(RCStrPinAlreadyRemoved);

 if PinIndex < Length(FOutputPins) - 2
  then Move(FOutputPins[PinIndex + 1], FOutputPins[PinIndex], (Length(FOutputPins) - 1 - PinIndex) * SizeOf(TCustomModularPinOutput));
 SetLength(FOutputPins, Length(FOutputPins) - 1);
end;

procedure TCustomModularPinInput.Connect(Pin: TCustomModularPinOutput);
begin
 // make sure everything is as it seems
 assert(Pin is TCustomModularPinOutput);

 // check if the pin is already connected
 if IndexOf(Pin) >= 0
  then raise Exception.Create(RCStrOutputPinAlreadyConnected);

 // check whether the data types match
 if Pin.Datatype <> Datatype
  then raise Exception.Create(RCStrDataTypeMismatch);

 // finally add output pin
 AddOutputPin(Pin);
 PinConnected;
end;

procedure TCustomModularPinInput.Disconnect(Pin: TCustomModularPinOutput);
begin
 // make sure everything is as it seems
 assert(Pin is TCustomModularPinOutput);

 // disconnect if the pin is connected
 if IndexOf(Pin) >= 0 then
  begin
   RemoveOutputPin(Pin);
   Pin.Disconnect;
   PinDisconnected;
  end;
end;

procedure TCustomModularPinInput.PinConnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinInput.PinDisconnected;
begin
 PinConnectionChanged;
end;

procedure TCustomModularPinInput.PinConnectionChanged;
begin
 if assigned(FOnPinConnectionChanged)
  then FOnPinConnectionChanged(Self);
end;

end.
