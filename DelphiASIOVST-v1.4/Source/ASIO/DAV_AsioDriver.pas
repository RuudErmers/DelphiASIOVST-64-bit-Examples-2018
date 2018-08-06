unit DAV_ASIODriver;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Windows, Forms, ActiveX, ComObj, DAV_ASIO;

type
  {$IFDEF DELPHI10_UP} {$region 'Driver interface declaration'} {$ENDIF}
  IDavASIODriverInterface = interface(IUnknown)
    // never ever change the order of the functions!!!
    procedure Init;
    procedure GetDriverName;
    procedure GetDriverVersion;
    procedure GetErrorMessage;
    procedure Start;
    procedure Stop;
    procedure GetChannels;
    procedure GetLatencies;
    procedure GetBufferSize;
    procedure CanSampleRate;
    procedure GetSampleRate;
    procedure SetSampleRate;
    procedure GetClockSources;
    procedure SetClockSource;
    procedure GetSamplePosition;
    procedure GetChannelInfo;
    procedure CreateBuffers;
    procedure DisposeBuffers;
    procedure ControlPanel;
    procedure Future;
    procedure OutputReady;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Driver interface declaration'} {$ENDIF}

  TDavASIODriver = class;
  TTDavASIODriver = class of TDavASIODriver;

  {$IFDEF DELPHI10_UP} {$region 'Control panel declaration'} {$ENDIF}
  TDavASIODriverCP = class(TForm)
  public
    Driver: TDavASIODriver;
    constructor Create(AOwner: TComponent); override;
    procedure PanelLoaded; virtual;
  end;

  TTDavASIODriverCP = class of TDavASIODriverCP;
  {$IFDEF DELPHI10_UP} {$endregion 'Control panel declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Thiscall wrapper declaration'} {$ENDIF}
  TDavASIOTCWrapper = class(TComObject)
  private
    FDestinationClass: TDavASIODriver;
  protected
    function GetDriverClass: TTDavASIODriver; virtual; abstract;
  public
    procedure Initialize; override;
    destructor Destroy; override;

    procedure Init;
    procedure GetDriverName;
    procedure GetDriverVersion;
    procedure GetErrorMessage;
    procedure Start;
    procedure Stop;
    procedure GetChannels;
    procedure GetLatencies;
    procedure GetBufferSize;
    procedure CanSampleRate;
    procedure GetSampleRate;
    procedure SetSampleRate;
    procedure GetClockSources;
    procedure SetClockSource;
    procedure GetSamplePosition;
    procedure GetChannelInfo;
    procedure CreateBuffers;
    procedure DisposeBuffers;
    procedure ControlPanel;
    procedure Future;
    procedure OutputReady;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Thiscall wrapper declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Asio driver class declaration'} {$ENDIF}
  TDavASIODriver = class
  protected
    fTCWrapper: TDavASIOTCWrapper;
    fInterfaceGUID: TGuid;
    fControlPanelClass: TTDavASIODriverCP;
    fParentWindowHandle: HWND;

    procedure SetControlPanelClass(cp: TTDavASIODriverCP);
    procedure InitControlPanel; virtual;
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid); virtual;

    function Init(SysHandle: HWND): boolean; virtual;
    function GetDriverName: string; virtual;
    function GetDriverVersion: LongInt; virtual;
    function GetErrorMessage: string; virtual;
    function Start: TASIOError; virtual;
    function Stop: TASIOError; virtual;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; virtual;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; virtual;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; virtual;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; virtual;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; virtual;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; virtual;
    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError; virtual;
    function SetClockSource(Reference: LongInt): TASIOError; virtual;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; virtual;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; virtual;
    function CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; virtual;
    function DisposeBuffers: TASIOError; virtual;
    function ControlPanel: TASIOError; virtual;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; virtual;
    function OutputReady: TASIOError; virtual;

    function AsioInit(SysHandle: HWND): TASIOBool;
    procedure AsioGetDriverName(Name: PAnsiChar);
    function AsioGetDriverVersion: Longint;  
    procedure AsioGetErrorMessage(Msg: PAnsiChar);
    function AsioStart: TASIOError;
    function AsioStop: TASIOError;
    function AsioGetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
    function AsioGetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
    function AsioGetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError;
    function AsioCanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function AsioGetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
    function AsioSetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function AsioGetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError;
    function AsioSetClockSource(Reference: LongInt): TASIOError;
    function AsioGetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
    function AsioGetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
    function AsioCreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
    function AsioDisposeBuffers: TASIOError;
    function AsioControlPanel: TASIOError;
    function AsioFuture(Selector: LongInt; Opt: Pointer): TASIOError;
    function AsioOutputReady: TASIOError;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Asio driver class declaration'} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Asio driver factory declaration'} {$ENDIF}
  TDavAsioDriverFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'Asio driver factory declaration'} {$ENDIF}

  var GlobalDriverControlPanel: TDavASIODriverCP;

implementation

uses sysutils;

const DavASIOInterfaceOffset = $24;


{ TDavASIOInterceptorCP }

{$IFDEF DELPHI10_UP} {$region 'Control panel implementation'} {$ENDIF}

constructor TDavASIODriverCP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Driver := nil;
end;

procedure TDavASIODriverCP.PanelLoaded;
begin end;

{$IFDEF DELPHI10_UP} {$endregion 'Control panel implementation'} {$ENDIF}


{ TDavASIOTCWrapper }

{$IFDEF DELPHI10_UP} {$region 'Thiscall wrapper implementation'} {$ENDIF}

procedure TDavASIOTCWrapper.Initialize;
begin
  inherited;
  Assert(DavASIOInterfaceOffset = GetInterfaceTable^.Entries[0].IOffset);
  FDestinationClass:=GetDriverClass.Create(self, GetInterfaceTable^.Entries[0].IID);
end;

destructor TDavASIOTCWrapper.Destroy;
begin
  FDestinationClass.Free;
  FDestinationClass:=nil;
  inherited;
end;  

procedure TDavASIOTCWrapper.Init;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioInit-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetDriverName;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetDriverName-FDestinationClass
end;  

procedure TDavASIOTCWrapper.GetDriverVersion;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetDriverVersion-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetErrorMessage;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetErrorMessage-FDestinationClass
end;

procedure TDavASIOTCWrapper.Start;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioStart-FDestinationClass
end;

procedure TDavASIOTCWrapper.Stop;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioStop-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetChannels;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetChannels-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetLatencies;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetLatencies-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetBufferSize;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8]   // get second parameter

  mov edx, [esp + 16]  // get fourth parameter
  mov [esp + 8], edx   // set fourth parameter

  // move return address on the stack position of the incoming fourth parameter
  mov edx, [esp]
  mov [esp + 16], edx

  mov edx, [esp + 4]   // get first parameter

  // move stack pointer to the new fourth parameter
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetBufferSize-FDestinationClass
end;

procedure TDavASIOTCWrapper.CanSampleRate;
asm
  // double uses 2 Words, they come in on the stack,
  // and delphi function calls use the same method
  // so we just move the return address to stack positions up
  // and are done

  mov edx,[esp]   // backup return address

  mov eax,[esp + 4]
  mov [esp], eax
  mov eax,[esp + 8]
  mov [esp+4], eax

  mov [esp+8],edx    // set return address

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioCanSampleRate-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetSampleRate;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetSampleRate-FDestinationClass  
end;

procedure TDavASIOTCWrapper.SetSampleRate;
asm
  // double uses 2 Words, they come in on the stack,
  // and delphi function calls use the same method
  // so we just move the return address to stack positions up
  // and are done

  mov edx,[esp]   // backup return address

  mov eax,[esp + 4]
  mov [esp], eax
  mov eax,[esp + 8]
  mov [esp+4], eax

  mov [esp+8],edx    // set return address

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioSetSampleRate-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetClockSources;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetClockSources-FDestinationClass
end;

procedure TDavASIOTCWrapper.SetClockSource;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset    

  // move stack pointer to the return address position
  add esp, 4

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioSetClockSource-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetSamplePosition;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetSamplePosition-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetChannelInfo;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset    

  // move stack pointer to the return address position
  add esp, 4

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetChannelInfo-FDestinationClass
end;

procedure TDavASIOTCWrapper.CreateBuffers;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8]   // get second parameter

  mov edx, [esp + 16]  // get fourth parameter
  mov [esp + 8], edx   // set fourth parameter

  // move return address on the stack position of the incoming fourth parameter
  mov edx, [esp]
  mov [esp + 16], edx

  mov edx, [esp + 4]   // get first parameter

  // move stack pointer to the new fourth parameter
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioCreateBuffers-FDestinationClass
end;

procedure TDavASIOTCWrapper.DisposeBuffers;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioDisposeBuffers-FDestinationClass
end;

procedure TDavASIOTCWrapper.ControlPanel;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioControlPanel-FDestinationClass
end;

procedure TDavASIOTCWrapper.Future;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioFuture-FDestinationClass
end;

procedure TDavASIOTCWrapper.OutputReady;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioOutputReady-FDestinationClass
end;

{$IFDEF DELPHI10_UP} {$endregion 'Thiscall wrapper implementation'} {$ENDIF}


{ TDavASIODriver }

{$IFDEF DELPHI10_UP} {$region 'Asio driver class implementation'} {$ENDIF}

constructor TDavASIODriver.Create(TCWrapper: TDavASIOTCWrapper; InterfaceGUID: TGuid);
begin
  fTCWrapper := TCWrapper;
  fInterfaceGUID := InterfaceGUID;
  fControlPanelClass := nil;
  fParentWindowHandle := 0;
end;

procedure TDavASIODriver.SetControlPanelClass(cp: TTDavASIODriverCP);
begin
  fControlPanelClass := cp;
end;  

procedure TDavASIODriver.InitControlPanel;
begin
  if assigned(fControlPanelClass) then
  begin
    if not assigned(GlobalDriverControlPanel) then GlobalDriverControlPanel := fControlPanelClass.Create(nil);
    GlobalDriverControlPanel.Driver := self;
    GlobalDriverControlPanel.PanelLoaded;
    if GlobalDriverControlPanel.Visible then GlobalDriverControlPanel.BringToFront;
  end;
end;

function TDavASIODriver.Init(SysHandle: HWND): boolean;
begin
  fParentWindowHandle := SysHandle;
  Result := true;
end;

function TDavASIODriver.GetDriverName: string;
begin
  Result := 'DAV Abstract Driver';
end;

function TDavASIODriver.GetDriverVersion: LongInt;
begin
  Result := 0;
end;

function TDavASIODriver.GetErrorMessage: string;
begin
  Result := '';
end;

function TDavASIODriver.Start: TASIOError;
begin
  Result := ASE_OK;
end;

function TDavASIODriver.Stop: TASIOError;
begin
  Result := ASE_OK;
end;

function TDavASIODriver.GetChannels(out NumInputChannels, NumOutputChannels: Integer): TASIOError;
begin
  NumInputChannels  := 1;
  NumOutputChannels := 1;
  Result := ASE_OK;
end;

function TDavASIODriver.GetLatencies(out InputLatency, OutputLatency: Integer): TASIOError;
begin
  InputLatency  := 0;
  OutputLatency := 0; 
  Result := ASE_OK;
end;

function TDavASIODriver.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin
  MinSize       := 256;
  MaxSize       := 1024;
  PreferredSize := 512;
  Granularity   := -1;
  Result := ASE_OK;
end;


function TDavASIODriver.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  Result := ASE_OK;
end;

function TDavASIODriver.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
  SampleRate := 44100;
  Result := ASE_OK;
end;

function TDavASIODriver.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  Result := ASE_OK;
end;


function TDavASIODriver.GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError;
begin
  with Clocks^[0] do
  begin
    Index := 0;
    AssociatedChannel := -1;
    AssociatedGroup := -1;
    IsCurrentSource := CAsioTrue;
    StrCopy(Name, 'Internal');
  end;
  NumSources := 1;  
  Result := ASE_OK;
end;

function TDavASIODriver.SetClockSource(Reference: LongInt): TASIOError;
begin
  Result := ASE_OK;
end;

function TDavASIODriver.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  SamplePosition.Hi := 0;
  SamplePosition.Lo := 0;
  TimeStamp.Hi := 0;
  TimeStamp.Lo := 0;
  Result := ASE_OK;
end;

function TDavASIODriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
begin
  if (Info.Channel <> 0) then
  begin
    Result := ASE_InvalidParameter;
    Exit;
  end;

  Info.SampleType := CAsioSTFloat32LSB;
  Info.ChannelGroup := 0;
  Info.IsActive := CAsioFalse;
  StrPCopy(Info.Name, 'Default channel');

  Result := ASE_OK;
end;

function TDavASIODriver.CreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
begin
  Result := ASE_NotPresent; // doesn't allocate anything right now
end;


function TDavASIODriver.DisposeBuffers: TASIOError;
begin
  Result := ASE_OK;
end;

function TDavASIODriver.ControlPanel: TASIOError;
var r: TRect;
begin
  Result:=ASE_NotPresent;
  if Assigned(GlobalDriverControlPanel) then
  begin
    // Hardcore centering ;)
    with GlobalDriverControlPanel do
    begin
      r := Rect(0,0,0,0);
      if fParentWindowHandle<>0 then
        GetWindowRect(fParentWindowHandle, r);

      if (r.Right-r.Left<Width) or (r.Bottom-r.Top<Height) then
        GetWindowRect(GetDesktopWindow, r);

      left := r.Left + round(((r.Right-r.Left)-Width)*0.5);
      top  := r.Top + round(((r.Bottom-r.Top)-Height)*0.5);
      ShowModal;
    end;

    Result := ASE_OK;
    exit;
  end;
end;

function TDavASIODriver.Future(Selector: LongInt; Opt: Pointer): TASIOError;
begin
  Result := ASE_NotPresent;
end;

function TDavASIODriver.OutputReady: TASIOError;
begin
  Result := ASE_OK;
end;


// methods used by the Asio-Wrapper, they are forwarded to local
// functions that can get overwritten, don't overwrite these functions
// the wrapper would never call them.

function TDavASIODriver.AsioInit(SysHandle: HWND): TASIOBool;
begin
 Result := TASIOBool(Init(SysHandle));
end;

procedure TDavASIODriver.AsioGetDriverName(Name: PAnsiChar);
begin
 StrPCopy(Name, AnsiString(Copy(GetDriverName, 0, 32)));
end;

function TDavASIODriver.AsioGetDriverVersion: Longint;
begin
 Result := GetDriverVersion;
end;

procedure TDavASIODriver.AsioGetErrorMessage(Msg: PAnsiChar);
begin
 StrPCopy(Msg, AnsiString(Copy(GetErrorMessage, 0, 124)));
end;

function TDavASIODriver.AsioStart: TASIOError;
begin
  Result := Start;
end;

function TDavASIODriver.AsioStop: TASIOError;
begin
  Result := Stop;
end;

function TDavASIODriver.AsioGetChannels(out NumInputChannels, NumOutputChannels: Integer): TASIOError;
begin
  Result := GetChannels(NumInputChannels, NumOutputChannels);
end;

function TDavASIODriver.AsioGetLatencies(out InputLatency, OutputLatency: Integer): TASIOError;
begin
  Result := GetLatencies(InputLatency, OutputLatency);
end;

function TDavASIODriver.AsioGetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin
  Result := GetBufferSize(MinSize, MaxSize, PreferredSize, Granularity);
end;

function TDavASIODriver.AsioCanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  Result := CanSampleRate(SampleRate);
end;

function TDavASIODriver.AsioGetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
  Result := GetSampleRate(SampleRate);
end;

function TDavASIODriver.AsioSetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  Result := SetSampleRate(SampleRate);
end;

function TDavASIODriver.AsioGetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt): TASIOError;
begin
  Result := GetClockSources(Clocks, NumSources);
end;

function TDavASIODriver.AsioSetClockSource(Reference: LongInt): TASIOError;
begin
  Result := SetClockSource(Reference);
end;

function TDavASIODriver.AsioGetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  Result := GetSamplePosition(SamplePosition, TimeStamp);
end;

function TDavASIODriver.AsioGetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
begin
  Result := GetChannelInfo(Info);
end;

function TDavASIODriver.AsioCreateBuffers(BufferInfos: PASIOBufferInfos; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
begin
  Result := CreateBuffers(BufferInfos, NumChannels, BufferSize, Callbacks);
end;

function TDavASIODriver.AsioDisposeBuffers: TASIOError;
begin
  Result := DisposeBuffers;
end;

function TDavASIODriver.AsioControlPanel: TASIOError;
begin
  Result := ControlPanel;
end;

function TDavASIODriver.AsioFuture(Selector: LongInt; Opt: Pointer): TASIOError;
begin
  Result := Future(Selector, Opt);
end;

function TDavASIODriver.AsioOutputReady: TASIOError;
begin
  Result := OutputReady;
end;

{$IFDEF DELPHI10_UP} {$endregion 'Asio driver class implementation'} {$ENDIF}


{ TDavAsioDriverFactory }

{$IFDEF DELPHI10_UP} {$region 'Asio driver factory implementation'} {$ENDIF}

procedure TDavAsioDriverFactory.UpdateRegistry(Register: Boolean);
begin
 inherited UpdateRegistry(Register);

 if Register then
  begin
   CreateRegKey('CLSID\' + GUIDToString(ClassID) + '\' + ComServer.ServerKey, 'ThreadingModel', 'Apartment');
   CreateRegKey('SOFTWARE\ASIO\' + Description, 'CLSID', GUIDToString(ClassID), HKEY_LOCAL_MACHINE);
   CreateRegKey('SOFTWARE\ASIO\' + Description, 'Description', Description, HKEY_LOCAL_MACHINE);
  end
 else DeleteRegKey('SOFTWARE\ASIO\' + Description, HKEY_LOCAL_MACHINE);
end;

{$IFDEF DELPHI10_UP} {$endregion 'Asio driver factory implementation'} {$ENDIF}

initialization

GlobalDriverControlPanel:=nil;

end.
