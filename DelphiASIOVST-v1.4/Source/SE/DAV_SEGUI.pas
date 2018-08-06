unit DAV_SEGUI;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, DAV_SECommon;

type
  /////////////////////////////////
  // Plugin Module opCodes (GUI) //
  /////////////////////////////////

  TSEGuiPluginOpcodes = (
    seGuiInitialise = 0,       // initialise
    seGuiClose,                // exit, release all memory and other resources!
    seGuiPaint,
    seGuiLButtonDown,
    seGuiLButtonUp,
    seGuiMouseMove,
    seGuiOnModuleMessage,      // OnModuleMsg test
    seGuiOnGuiPlugValueChange,
    seGuiOnWindowOpen,
    seGuiOnWindowClose,
    seGuiOnIdle,
    seGuiOnNewConnection,
    seGuiOnDisconnect,
    seGuiDoNotUseOrRemoveThis = $7FFFFFFF
    );

  ////////////////////////
  // Host opCodes (GUI) //
  ////////////////////////

  TSEGuiHostOpcodes = (
    seGuiHostRequestRepaint = 0,  //
    seGuiHostGetHandle,
    seGuiHostSendStringToAudio,   // SendStringToAudio test
    seGuiHostSetWindowSize,
    seGuiHostSetWindowSizeable,
    seGuiHostGetTotalPinCount,
    seGuiHostPlugSetValText,
    seGuiHostPlugGetValText,
    seGuiHostAddGuiPlug,
    seGuiRegisterPatchParameter,  // Obsolete, use IO_PATCH_STORE or IO_UI_COMMUNICATION_DUAL flags instead. Will crash module on destruction (mayby need Unregister Opcode to fix this)
    seGuiHostGetFontInfo,
    seGuiHostSetWindowType,       // pass 1 to provide your GuiModule with a 'real' HWND (else SE draws your module on the parent window)
    seGuiHostGetWindowHandle,
    (* example code... (WI is a SEWndInfo pointer )
      result := HWND(CallHost(seGuiHostGetWindowHandle, WI.context_handle));
    *)
    seGuiHostSetWindowFlags,
    seGuiHostPlugGetVal,          // query host value
    seGuiHostPlugSetVal,          // set host value
    seGuiHostPlugSetExtraData,    // sets enum list or file extension (depending on datatype)
    (* example code...
      // pass pin number and new list
      CallHost(seGuiHostPlugSetExtraData, 4, 0, 'moose, cat, dog', 0);
    *)
    seGuiHostPlugGetExtraData,    // gets enum list or file extension (depending on datatype). Easier to use SeGuiPin.getExtraData
    (* example code...
      var
        string_length : Integer;
        dest          : ^wchar_t;
        ascii_text    : PChar;
      begin
       string_length := CallHost(seGuiHostPlugGetExtraData, getIndex, 0, 0);

       // Destination is UNICODE (two-byte) character string
       dest := new wchar_t[string_length];

       CallHost(seGuiHostPlugGetExtraData, PN_ENUM_OUT, string_length, @dest);

       // to convert to ascii
       ascii_text := new char[string_length];
       WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, nil, nil);

       // clean up
       Dispose(dest);
       Dispose(ascii_text);
      end;
    *)
    seGuiHostSetCapture,          // see SEGUI_base::SetCapture(...)
    seGuiHostReleaseCapture,
    seGuiHostGetCapture,
    seGuiHostCallVstHost,         // pass se_call_vst_host_params structure in Ptr
    seGuiHostSetIdle,             // pass 1 to receive regular calls to OnIdle(), pass zero to cancel
    seGuiHostGetModuleFilename,   // returns full module path
    (* example code...
      const
        MAX_STRING_LENGTH : Integer = 300;
      var
        dest       : array [0..MAX_STRING_LENGTH-1] of ShortInt;
        ascii_text : array [0..MAX_STRING_LENGTH-1] of Char;
      begin
       // Destination is UNICODE (two-byte) character string
       CallHost(seGuiHostGetModuleFilename, 0, MAX_STRING_LENGTH, @dest);

       // to convert to ascii
       WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, nil, nil);
      end;
    *)
    seGuiHostResolveFilename,     // returns full module path
    (* example code...
      const
        MAX_STRING_LENGTH : Integer = 300;
      var
        dest       : array [0..MAX_STRING_LENGTH-1] of ShortInt;
        ascii_text : array [0..MAX_STRING_LENGTH-1] of Char;
      begin
       // Destination is UNICODE (two-byte) character string
       // convert filename to UNICODE
       MultiByteToWideChar(CP_ACP, 0, "test.wav", -1, LPWSTR(@dest), MAX_STRING_LENGTH);

       // query full filename (SE concatenates default path for that type of file, depending on file extension)
       CallHost(seGuiHostResolveFilename, 0, MAX_STRING_LENGTH, @dest);

       // to convert to ascii
       WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_text, MAX_STRING_LENGTH, nil, nil);
      end;
    *)
    seGuiHostGetHostType,         // return code 0 = unsuported, 1 = module is running in SynthEdit, 2 = Module is in a VST plugin (made with SE)
    seGuiHostRemoveGuiPlug,
    seGuiHostGetParentContext,    // Get 'handle' of parent window.  This is an SE handle, not an HWND. Use seGuiHostGetWindowHandle to convert.
    seGuiHostMapWindowPoints,     // map a point on one window to the co-ordinate system of a 2nd window
    (*
      var
        parent_context : Integer;
        h              : HWND;
      begin
        // Example: getting parent HWND, and your position relative to it
        parent_context := WI.context_handle;
        h := 0;
        while h = 0 do
         begin
          parent_context = CallHost(seGuiHostGetParentContext, parent_context);
          h := HWND(CallHost(seGuiHostGetWindowHandle, parent_context));
         end;

        sepoint offset(0,0);
        CallHost(seGuiHostMapWindowPoints, WI.context_handle, parent_context, @offset, 0);
      end;
    *)
    seGuiHostMapClientPointToScreen, // maps a point on your gui to the system screen (absolute co-ords)
    (*
      // Example: converting a point on your GUI to an absolute co-ordinate. Useful for pop-up menus
      var
        offset : TSEPoint;
      begin
        offset.x := 0;
        offset.y := 0;
        CallHost(seGuiHostMapClientPointToScreen, WI.context_handle, 0, @offset, 0);
      end;
    *)
    seGuiHostInvalidateRect,      // invlalidate (cause redraw) of any SE window
    (*
      var
        n: TRect;
      begin
        n.top = 0;
        n.bottom = 1;
        n.left = 2;
        n.right = 20;
        CallHost(seGuiHostInvalidateRect, WI.context_handle, 0, @n, 0);
      end;
    *)
    seGuiHostIsGraphInitialsed,   // test if pin updates are due to file loading, or from user.
    seGuiHostIsInteger = $7FFFFFFF);

  PSEGUIStructBase = ^TSEGUIStructBase;
  TSEGUIBase = class;

  TSEGuiCallback = function(Effect: PSEGUIStructBase; Opcode: TSEGuiHostOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSEGuiDispatcher = function(Effect: PSEGUIStructBase; Opcode: TSEGuiPluginOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;

  TSEGUIStructBase = record
    Magic      : Integer;              // magic number
    Version    : Integer;
    Dispatcher : TSEGuiDispatcher;
    HostPtr    : Pointer;              // reserved for host use, must be 0
    SEGUIBase  : TSEGUIBase;           // for class access
    User       : Pointer;              // user access
    Future     : array[0..15] of Char; // pls zero
  end;

  TSEHostWindowFlag = (hwfResizable = 1, hwfNoCustomGfxOnStructure = 2);
  TSEHostWindowFlags = set of TSEHostWindowFlag;

  // painting info
  TSEpoint = TPoint;

  PSEWndInfo = ^TSEWndInfo;
  TSEWndInfo = record
    Width         : Integer;
    Height        : Integer;
    ContextHandle : THandle;
  end;

  TSEFontInfo = record
    Size            : Integer;
    Color           : Integer;
    ColorBackground : Integer;
    Flags           : Integer; // alignment etc
    FontHeight      : Integer;
//    Category        : array[0..19] of Char;
//    Facename        : array[0..49] of Char;
    Future          : array[0..99] of Char;
  end;

  TSEGuiPin = class(TObject)
  private
    FIndex  : Integer;
    FModule : TSEGUIBase;
    function GetValueBool: Boolean;
    function GetValueFloat: Single;
    function GetValueInt: Integer;         // int, bool, and list type values
    function GetValueText: TSeSdkString;
    procedure SetValueBool(const Value: Boolean);
    procedure SetValueFloat(Value: Single);
    procedure SetValueInt(Value: Integer);
    procedure SetValueAsString(const Value: TSeSdkString);
  protected
    property Module: TSEGUIBase read FModule;
  public
    constructor Create; overload; virtual;
    constructor Create(const AIndex: Integer; const AModule: TSEGUIBase); overload; virtual;
    procedure Init(const AIndex: Integer; const AModule: TSEGUIBase); virtual;
    function GetExtraData: TSeSdkString2;
    procedure SetValueText(var Value: TSeSdkString);

    property PinIndex: Integer read FIndex;
    property ValueAsInteger: Integer read GetValueInt write SetValueInt;
    property ValueAsBoolean: Boolean read GetValueBool write SetValueBool;
    property ValueAsSingle: Single read GetValueFloat write SetValueFloat;
    property ValueAsString: TSeSdkString read GetValueText write SetValueAsString;
  end;

  TSEGuiPins = array of TSeGuiPin;

  TSEGuiPinEvent = procedure(Sender: TObject; Pin: TSeGuiPin) of object;
  TSEGuiPinIndexEvent = procedure(Sender: TObject; PinIndex: Integer) of object;
  TSEGUIBase = class(TObject)
  private
    FOnIdle           : TNotifyEvent;
    FOnPinValueChange : TSEGuiPinEvent;
    FOnDisconnect     : TSEGuiPinIndexEvent;

    FOnLButtonDown    : TNotifyEvent;
    FOnLButtonUp      : TNotifyEvent;
    FOnMouseMove      : TNotifyEvent;
    FOnNewConnection  : TNotifyEvent;
    FOnWindowClose    : TNotifyEvent;
    FOnWindowOpen     : TNotifyEvent;
    FOnPaint          : TNotifyEvent;

//    FPins   : array of TSeGuiPin;
    procedure SetupPins;
    function GetPin(Index: Integer): TSEGuiPin;
  protected
    FAudioMaster : TSEGuiCallback;
    FStructBase  : TSEGUIStructBase;
    function GetSEGUIStructBase: PSEGUIStructBase;
    function GuiIdle: Boolean; virtual;
    procedure GuiDisconnect(PinIndex: Integer); virtual;
    procedure GuiPinValueChange(CurrentPin: TSeGuiPin); virtual;
    procedure GuiLButtonDown(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); virtual;
    procedure GuiLButtonUp(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); virtual;
    procedure GuiModuleMsg(UserMsg_id: Integer; MsgLength: Integer; MsgData: Pointer); virtual;
    procedure GuiMouseMove(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint); virtual;
    procedure GuiNewConnection(PinIndex: Integer); virtual;
    procedure GuiWindowClose(WI: PSEWndInfo); virtual;
    procedure GuiWindowOpen(WI: PSEWndInfo); virtual;
    procedure GuiPaint(hDC: HDC; WI: PSEWndInfo); virtual;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); virtual;
    destructor Destroy; override;

    // called from audio master
    function CallHost(Opcode: TSEGuiHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
    function Dispatcher(Opcode: TSEGuiPluginOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; virtual;
    function GetCapture(const WI: PSEWndInfo): Boolean;
    procedure AddGuiPlug(ADatatype: TSEPlugDataType; ADirection: TSEDirection; const AName: Pchar);
    procedure Close; virtual;
    procedure Initialise(const LoadedFromFile: Boolean); virtual;
    procedure ReleaseCapture(const WI: PSEWndInfo);
    procedure SetCapture(const WI: PSEWndInfo);

    property SEGUIStructBase: PSEGUIStructBase read GetSEGUIStructBase;
    property Pin[Index: Integer]: TSEGuiPin read GetPin;

    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnDisconnect: TSEGuiPinIndexEvent read FOnDisconnect write FOnDisconnect;
    property OnPinValueChange: TSEGuiPinEvent read FOnPinValueChange write FOnPinValueChange;
  end;

implementation

uses
  SysUtils, Types;

var
  StaticPin : TSEGuiPin;

{ TSeGuiPin }

constructor TSEGuiPin.Create;
begin
 inherited;
end;

constructor TSEGuiPin.Create(const AIndex: Integer; const AModule: TSEGUIBase);
begin
 Create;
 Init(AIndex, AModule);
end;

procedure TSeGuiPin.SetValueText(var Value: TSeSdkString);
begin
 FModule.CallHost(seGuiHostPlugSetValText, FIndex, 0, @Value);
end;

function TSeGuiPin.GetValueText: TSeSdkString;
begin
 // warning, unstable over 2000 bytes  ( that's 1000 UNICODE characters )
 result := PChar(FModule.CallHost(seGuiHostPlugGetValText, FIndex, 0, nil));
end;

procedure TSEGuiPin.Init(const AIndex: Integer; const AModule: TSEGUIBase);
begin
 FIndex  := AIndex;
 FModule := AModule;
end;

function TSeGuiPin.GetValueInt: Integer; // int, bool, and list type values
begin
 FModule.CallHost(seGuiHostPlugGetVal, FIndex, 0, @result);
end;

function TSEGuiPin.GetValueBool: Boolean;
begin
 FModule.CallHost(seGuiHostPlugGetVal, FIndex, 0, @result);
end;

function TSeGuiPin.GetValueFloat: Single;
begin
 FModule.CallHost(seGuiHostPlugGetVal, FIndex, 0, @result);
end;

procedure TSEGuiPin.SetValueAsString(const Value: TSeSdkString);
begin
 FModule.CallHost(seGuiHostPlugSetValText, FIndex, 0, @Value);
end;

procedure TSEGuiPin.SetValueBool(const Value: Boolean);
begin
 FModule.CallHost(seGuiHostPlugSetVal, FIndex, Integer(Value), nil);
end;

procedure TSeGuiPin.setValueFloat(Value: Single);
begin
 FModule.CallHost(seGuiHostPlugSetVal, FIndex, 0, nil, Value);
end;

procedure TSeGuiPin.setValueInt(Value: Integer);
begin
 FModule.CallHost(seGuiHostPlugSetVal, FIndex, Value, nil);
end;

function TSeGuiPin.getExtraData: TSeSdkString2;
var
  StringLength : Integer;
  Temp         : PWideChar;
begin
 StringLength := FModule.CallHost(seGuiHostPlugGetExtraData, FIndex, 0, nil);
 GetMem(Temp, StringLength * 2);
 try
  FModule.CallHost(seGuiHostPlugGetExtraData, FIndex, StringLength, temp);
  Result := temp;
 finally
  Dispose(temp);
 end;
end;

function DispatchEffectClass(Effect: PSEGUIStructBase; Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
 if assigned(Effect) then
  begin
   assert(assigned(Effect.SEGUIBase));
   result := Effect.SEGUIBase.Dispatcher(TSEGuiPluginOpcodes(Opcode), Index, Value, Ptr, Opt);
  end else result := 0;
end;


{ TSEGUIBase }

constructor TSEGUIBase.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 FAudioMaster := SEGuiCallback;
 FillChar(FStructBase, SizeOf(TSEGUIStructBase), 0);
 with FStructBase do
  begin
   Magic      := CSepMagic2;
   Dispatcher := @DispatchEffectClass;
   SEGUIBase  := Self;
   HostPtr    := AHostPtr;
   Version    := 1;
  end;
end;

destructor TSEGUIBase.Destroy;
begin
 inherited;
end;

function TSEGUIBase.Dispatcher(Opcode: TSEGuiPluginOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
var
  pnt : TSEPoint;
begin
 result := 0;
 case Opcode of
  seGuiInitialise: Initialise(Index = 1);
  seGuiClose:
   begin
    Close;
    Free;
    result := 1; // this object now deleted, must do nothing more.
   end;
  seGuiPaint: GuiPaint(HDC(Index), PSEWndInfo(Ptr));
  seGuiLButtonDown:
   begin
    pnt := Point(Index, Value);
    GuiLButtonDown(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
   end;
  seGuiLButtonUp:
   begin
    pnt := Point(Index, Value);
    GuiLButtonUp(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
   end;
  seGuiMouseMove:
   begin
    pnt := Point(Index, Value);
    GuiMouseMove(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
   end;
  seGuiOnModuleMessage: GuiModuleMsg(Value, Index, Ptr);
  seGuiOnGuiPlugValueChange:
   begin
    StaticPin.Init(Index, Self);
    GuiPinValueChange(StaticPin);
   end;
  seGuiOnWindowOpen    : GuiWindowOpen(PSEWndInfo(Ptr));
  seGuiOnWindowClose   : GuiWindowClose(PSEWndInfo(Ptr));
  seGuiOnIdle          : result := Integer(GuiIdle);
  seGuiOnNewConnection : GuiNewConnection(Index);
  seGuiOnDisconnect    : GuiDisconnect(Index);
 end;
end;

function TSEGUIBase.CallHost(Opcode: TSEGuiHostOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
begin
 assert(assigned(FAudioMaster));
 result := FAudioMaster(@FStructBase, Opcode, Index, Value, Ptr, Opt);
end;

procedure TSEGUIBase.Close;
begin
 // do nothing yet
end;

procedure TSEGUIBase.Initialise(const LoadedFromFile: Boolean);
begin
 SetupPins;
end;

procedure TSEGUIBase.GuiDisconnect(PinIndex: Integer);
begin
 if assigned(FOnDisconnect)
  then FOnDisconnect(Self, PinIndex);
end;

procedure TSEGUIBase.GuiPinValueChange(CurrentPin: TSeGuiPin);
begin
 if assigned(FOnPinValueChange)
  then FOnPinValueChange(Self, CurrentPin);
end;

function TSEGUIBase.GuiIdle: Boolean;
begin
 if assigned(FOnIdle)
  then FOnIdle(Self);
 result := assigned(FOnIdle);
end;

procedure TSEGUIBase.GuiLButtonDown(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 if assigned(FOnLButtonDown)
  then FOnLButtonDown(Self);
end;

procedure TSEGUIBase.GuiLButtonUp(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 if assigned(FOnLButtonUp)
  then FOnLButtonUp(Self);
end;

procedure TSEGUIBase.GuiModuleMsg(UserMsg_id, MsgLength: Integer; MsgData: Pointer);
begin
 if assigned(FOnLButtonUp)
  then FOnLButtonUp(Self);
end;

procedure TSEGUIBase.GuiMouseMove(WI: PSEWndInfo; nFlags: Cardinal; Point: TSEPoint);
begin
 if assigned(FOnMouseMove)
  then FOnMouseMove(Self);
end;

procedure TSEGUIBase.GuiNewConnection(PinIndex: Integer);
begin
 if assigned(FOnNewConnection)
  then FOnNewConnection(Self);
end;

procedure TSEGUIBase.GuiWindowClose(WI: PSEWndInfo);
begin
 if assigned(FOnWindowClose)
  then FOnWindowClose(Self);
end;

procedure TSEGUIBase.GuiWindowOpen(WI: PSEWndInfo);
begin
 if assigned(FOnWindowOpen)
  then FOnWindowOpen(Self);
end;

procedure TSEGUIBase.GuiPaint(hDC: HDC; WI: PSEWndInfo);
begin
 if assigned(FOnPaint)
  then FOnPaint(Self);
end;

procedure TSEGUIBase.AddGuiPlug(ADatatype: TSEPlugDataType; ADirection: TSEDirection; const AName: Pchar);
begin
 CallHost(seGuiHostAddGuiPlug, Integer(ADatatype), Integer(ADirection), AName);
 SetupPins;
end;

procedure TSEGUIBase.SetupPins;
(*
var
  i, ActualPlugCount: Integer;
*)
begin
 // commented out, may need reinstating if pins ever gain state
(*
 // get actual number of pins used (may be more or less if auto-duplicating plugs used)
 ActualPlugCount := CallHost(seGuiHostGetTotalPinCount);

 FPins.Resize(ActualPlugCount);

 for i := 0 to ActualPlugCount - 1
  do FPins[i].Init(i, self);
*)
end;

function TSEGUIBase.GetPin(Index: Integer): TSeGuiPin;
begin
  // there are no pins.
  // pins currently hold no state, implement them as a flyweight (saves having
  // to track pin add/remove, we're not notified of autoduplicate add/remove anyhow)
  StaticPin.Init(Index, Self);
  result := StaticPin;

 //{ return &m_pins[Index];}
end;

// capture mouse movement
procedure TSEGUIBase.SetCapture(const WI: PSEWndInfo);
begin
  CallHost(seGuiHostSetCapture, 0, 0, WI);
end;

// release capture mouse movement
procedure TSEGUIBase.ReleaseCapture(const WI: PSEWndInfo);
begin
  CallHost(seGuiHostReleaseCapture, 0, 0, WI);
end;

// query mouse capture state
function TSEGUIBase.GetSEGUIStructBase: PSEGUIStructBase;
begin
 result := @FStructBase;
end;

function TSEGUIBase.GetCapture(const WI: PSEWndInfo): Boolean;
begin
  result := CallHost(seGuiHostGetCapture, 0, 0, WI) <> 0;
end;

initialization
  StaticPin := TSEGuiPin.Create;

finalization
  FreeAndNil(StaticPin);

end.
