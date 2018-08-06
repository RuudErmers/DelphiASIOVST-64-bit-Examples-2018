unit DAV_SEModule;

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
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils, 
  DAV_Types, DAV_SECommon;

type
  TUgFlag = (
    ugfVoiceMonIgnore = 2,      // DON'T WANT VOICE HELD OPEN BECAUSE A SCOPE IS CONNECTED
    ugfPolyphonicAgregator = 6, // A ug that always combines voices

    // read only
    ugfSuspend = 7,
    ugfOpen = 8,

    // normally when a voice has faded out, SE shuts all that voice's modules off
    // in very special cases you can prevent SE from shutting off your module
    ugfNeverSuspend = 9,
    ugfClone = 11,
    ugfSendTimeInfoToHost = 17,
    ugfDoNeitherUseNorRemove = 31 // necessary so that TGuiFlags is an integer
  );
  TUgFlags = set of TUgFlag;

  TGuiFlag = (
    gfControlView = 7,            // visible in PanelEdit mode
    gfStructureView = 8,          // visible in structure mode
    gfDoNeitherUseNorRemove = 31  // necessary so that TGuiFlags is an integer
  );
  TGuiFlags = set of TGuiFlag;

  TSEIOFlag =
    (iofPolyphonicActive,    // midi channel selection etc should ignore patch changes
     iofIgnorePatchChange,   // auto-rename on new connection
     iofRename,              // plugs which are automaticly duplicated (like a container's 'spare' plug)
     iofAutoDuplicate,
     iofFilename,
     iofSetableOutput,       // ALLOW USER TO SET THE VALUE OF THIS OUTPUT eg on 'constant value' ug
     iofCustomisable,        // plugs which can be duplicated/deleted by CUG
     iofAdder,               // plugs which handle multiple inputs, must belong to an Adder ug
     iofHidePin,             // plugs which are private or obsolete, but are enabled on load if connected somewhere
     iofLinearInput,         // set this if this input can handle more that one polyphonic voice
     iofUICommunication,
     iofAutoEnum,
     iofHideWhenLocked,
     iofParameterScreenOnly, // DON'T EXPOSE AS PLUG (TO SAVE SPACE)
     iofDoNotCheckEnum,
     iofUIDualFlag,          // don't use iofUIDualFlag by itself, use iofUICommunication
     iofPatchStore,          // Patch store is similar to dual but for DSP output plugs that appear as input plugs on UI (Output paramters) could consolodate?
     iofParamPrivate,        // Private parameter (not exposed to user of VST plugin)
     iofMinimized,           // minimised (not exposed on structure view (only properties window)
     iofDisableIfPos = iofHidePin,
     iofPrivate = iofHidePin);
  TSEIOFlags = set of TSEIOFlag;
  //  iofCommunicationDual = iofUIDualFlag or iofUICommunication; // obsolete, use iofPatchStore instead

  // state type
  TSEStateType = (stStop, stOneOff, stStatic = 1, stRun); // stOneOff obsolete. Use stStatic or stRun!

  // Order is important for comparisons
  TUGEventType = (uetStatChange, uetSuspend, uetMIDI, uetRunFunction,
    uetIOFunc, uetUINotify, uetProgChange, uetNoteOn, uetNoteOff,
    uetNoteMute, uetPitchBend, uetAftertouch, uetStartPortamento,
    uetWSTableChange, uetDelayedGate, uetParamAutomation,
    uetNoteOffHeld, uetHeldNotesOff, uetNull, uetGeneric1,
    uetSetOutputPin, uetRunFunction2);

  PSEFloatSample = ^TSEFloatSample;
  TSEFloatSample = Single;

  PSEFloatSampleFixedArray = ^TSEFloatSampleFixedArray;
  TSEFloatSampleFixedArray = Array [0..0] of TSEFloatSample;

  ///////////////////////////
  // Plugin Module Opcodes //
  ///////////////////////////

  TSEPluginModuleOpcodes =
    (seffOpen  = 0,       // initialise
     seffClose = 1,       // exit, release all memory and other resources!

     seffSetSampleRate,   // in Opt (float)
     seffSetBlockSize,    // in Value
     seffGetUniqueId,
     seffGetEffectName,
     seffGetPinProperties,
     seffAddEvent,
     seffResume,
     seffGetModuleProperties,
     seffIsEventListEmpty,
     seffGetSdkVersion,
     seffGuiNotify,
     seffQueryDebugInfo); // allows to host to determin compiler settings etc


  //////////////////
  // Host Opcodes //
  //////////////////

  TSEHostOpcodes = (
    SEAudioMasterSetPinStatus = 0,
    SEAudioMasterIsPinConnected,     //
    SEAudioMasterGetPinInputText,    // gets pointer to plugs input string (DT_TEXT only)
    SEAudioMasterGetSampleClock,     // get current sampleclock at block start
    SEAudioMasterSendMIDI,           // send short MIDI msg
    SEAudioMasterGetInputPinCount,   // total AUDIO ins
    SEAudioMasterGetOutputPinCount,  // total AUDIO outs
    SEAudioMasterGetPinVarAddress,
    SEAudioMasterGetBlockStartClock,
    SEAudioMasterGetTime,
    SEAudioMasterSleepMode,
    SEAudioMasterGetRegisteredName,  // limited to 50 characters or less
    (* EXAMPLE CALLING CODE
      name : array [0..49] of Char;
      CallHost(SEAudioMasterGetRegisteredName, 0, 0, @name[0]);
    *)
    SEAudioMasterGetFirstClone,
    SEAudioMasterGetNextClone,
    (* EXAMPLE CALLING CODE

      procedure IterateThroughAllClones;
      var
        CloneStruct : PSE2ModStructBase;
        Clone       : PModule;
      begin
        // get first one
        CallHost(SEAudioMasterGetFirstClone, 0, 0, CloneStruct);

        while (clone_struct <> 0)
         begin
          // convert host's clone pointer to a 'Module' object
          Clone := PModule(CloneStruct.Object);

          // Access each clone here

          // step to Next clone
          Clone.CallHost(SEAudioMasterGetNextClone, 0, 0, CloneStruct);
         end;
      end;
    *)
    SEAudioMasterGetTotalPinCount,   // Total pins of all types
    SEAudioMasterCallVstHost,        // Call VST Host direct (see TSECallVstHostParams)
    SEAudioMasterResolveFilename,    // get full path from a short filename, (int pin_idx, float max_characters, Char *destination)
    SEAudioMasterSendStringToGui,    // Reserved for Experimental use (by Jef)
    SEAudioMasterGetModuleHandle,    // Reserved for Experimental use (by Jef)
    SEAudioMasterAddEvent,           // pass PSeEvent, host will copy data from struct. Safe to discard after call.
    SEAudioMasterCreateSharedLookup,
    SEAudioMasterSetPinOutputText,   // sets plug's output string (DT_TEXT only)
    SEAudioMasterSetProcessFunction, // sets the current SubProcess function
    SEAudioMasterResolveFilename2,   // get full path from a short filename - UNICODE
    (* EXAMPLE CALLING CODE
      uses windows;  //for WideCharToMultiByte

      // get the full path of an imbedded file when you only know it's short name
      const
        MAX_FILENAME_LENGTH : Integer = 300;

      // Both source and destination are UNICODE (two-byte) character strings
      unsigned short *source = L"test.txt";
      unsigned short dest[MAX_FILENAME_LENGTH];

      CallHost(SEAudioMasterResolveFilename2, Integer(source), MAX_FILENAME_LENGTH, &dest);

      // to convert to ascii (optional)
      Char ascii_filename[MAX_FILENAME_LENGTH];
      WideCharToMultiByte(CP_ACP, 0, dest, -1, ascii_filename, MAX_FILENAME_LENGTH, NULL, NULL);
    *)
    SEAudioMasterGetSeVersion,       // returns SE Version number times 100,000 ( e.g. 120000 is V 1.2 )
    (* EXAMPLE CALLING CODE
      int v = CallHost(SEAudioMasterGetSeVersion, 0, 0, 0);
    *)
    SEAudioMasterIsInteger = $7FFFFFFF
  );


  //////////////////////////
  // forward declarations //
  //////////////////////////

  PSE1ModStructBase  = ^TSE1ModStructBase;
  PSE2ModStructBase  = ^TSE2ModStructBase;
  PSEEvent           = ^TSEEvent;
  TSEModuleBase      = class;
  TSEPin             = class;
  TSEModuleBaseClass = class of TSEModuleBase;

  ////////////////////
  // function types //
  ////////////////////

  TSE1Dispatcher = function(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSE2Dispatcher = function(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSE1Process = procedure(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
  TSE2Process = procedure(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
  TSE2Event = procedure(ModuleBase: TSEModuleBase; Event: PSEEvent); cdecl;

  TSESetParameter = procedure(Effect: PSE1ModStructBase; Index: Integer; Parameter: Single);
  TSEGetParameter = function(Effect: PSE1ModStructBase; Index: Integer): Single;

  TSE2EventEvent = function(Event: PSEEvent): Pointer of object;
  TSE2DispatcherEvent = function(Opcode: Integer; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer of object;
  TSE2ProcessEvent = procedure(const BufferOffset, SampleFrames: Integer) of object;
  TSEPlugStateChangeEvent = procedure(Sender: TObject; Pin: TSEPin) of object;
  TSEPinStatusUpdateEvent = procedure(Sender: TObject; AStatus: TSEStateType) of object;
  TSEMidiDataEvent = procedure(Sender: TObject; AClock, AMidiMsg: Cardinal; PinID: Integer) of object;
  TSEInputStateChangedEvent = procedure(Sender: TObject; PlugIndex: Integer; NewState: TSEStateType) of object;
  TSEGuiNotifyEvent = procedure(Sender: TObject; AUserMsgID, ASize: Integer; AData: Pointer) of object;
  TSEVoiceResetEvent = procedure(Sender: TObject; Future: Integer);

  TSE1AudioMasterCallback = function (Effect: PSE1ModStructBase; Opcode: TSEHostOpcodes; Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;
  TSE2AudioMasterCallback = function (Effect: PSE2ModStructBase; Opcode: TSEHostOpcodes; Index, Value: Integer; Ptr : Pointer; Opt : Single): Integer; cdecl;

  ////////////////
  // structures //
  ////////////////

  {$A4} // align all records to 4 byte

  TSE1ModStructBase = record
    Magic            : Integer;              // the 'magic number' that identifies a SynthEdit module (spells SEPL)
    Flags            : Integer;              // see constants
    ReservedA        : Pointer;              // reserved for host use, must be 0
    ReservedB        : Integer;              // reserved, must be 0
    SEModule         : TSEModuleBase;        // for class access, MUST be 0 else!
    User             : Pointer;              // user access
    Version          : Integer;              //
    ProcessReplacing : TSE1Process;          // see TSE1Process
    Future           : array[0..59] of Char; // pls zero
  end;

  TSE2ModStructBase = record
    Magic            : Integer;              // the 'magic number' that identifies a SynthEdit module (spells SEPL)
    Version          : Integer;              //
    HostPtr          : Pointer;              // reserved for host use, must be 0
    SEModule         : TSEModuleBase;        // for class access, MUST be 0 else!
    User             : Pointer;              // User access
    Dispatcher       : TSE2Dispatcher;
    SubProcessPtr    : TSE2Process;
    EventHandlerPtr  : TSE2Event;
    Future           : array[0..15] of Char; // pls zero
  end;

  // fill in this structure when using Opcode SEAudioMasterCallVstHost
  PSECallVstHostParams = ^TSECallVstHostParams;
  TSECallVstHostParams = record
    Opcode : Integer;
    Index  : Integer;
    Value  : Integer;
    Ptr    : Pointer;
    Opt    : Single;
  end;

  TSEEvent = record // a generic timestamped event
    TimeStamp : Cardinal;
    EventType : TUGEventType;
    IntParamA : Integer;
    IntParamB : Integer;
    PtrParam  : Pointer;
    Next      : PSEEvent; // Next in list (not used)
  end;

  PSEModuleProperties = ^TSEModuleProperties;
  TSEModuleProperties = record
    Name       : PAnsiChar;
    ID         : PAnsiChar; // max. 32 chars
    About      : PAnsiChar;
    Flags      : TUgFlags;
    GuiFlags   : TGuiFlags;
    SdkVersion : Integer;
  end;

  PSEPinProperties = ^TSEPinProperties;
  TSEPinProperties = record
    VariableAddress  : Pointer;
    Direction        : TSEDirection;
    Datatype         : TSEPlugDataType;
    Name             : PAnsiChar;
    DefaultValue     : PAnsiChar;
    DatatypeExtra    : PAnsiChar;
    Flags            : TSEIOFlags;
    Spare            : Integer;
  end;

  TAutoduplicatePlugData = record
    case Integer of
     0 : (FloatPtr   : PSingle);
     1 : (EnumValue  : Smallint);
     2 : (BoolValue  : Boolean);
     3 : (FloatValue : Single);
     4 : (TextPtr    : PPAnsiChar);
     5 : (IntValue   : Integer);
    end;

  TSEPin = class
  private
    FModule               : TSEModuleBase;
    FPinIndex             : Integer;
    FStatus               : TSEStateType;
    FDataType             : TSEPlugDataType;
    FVariablePtr          : Pointer;
    FAutoDuplicatePlugVar : TAutoduplicatePlugData; // Holds pointer to buffer (auto duplicate plugs only)
    FOnStatusUpdate       : TSEPinStatusUpdateEvent;
    function GetIsConnected: Boolean;
    function GetValue: Single;
  protected
    procedure StatusUpdate(AStatus: TSEStateType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Init(Module: TSEModuleBase; PinIndex: Integer; DataType: TSEPlugDataType; VariablePtr: Pointer);
    procedure TransmitStatusChange(SampleClock: Cardinal; NewState: TSEStateType);
    procedure TransmitMIDI(SampleClock, Msg: Cardinal);

    property IsConnected: Boolean read GetIsConnected;
    property Module: TSEModuleBase read FModule;
    property Value: Single read GetValue;
    property VariableAddress: Pointer read FVariablePtr;

    property DataType: TSEPlugDataType read FDataType;
    property PinID: Integer read FPinIndex;
    property Status: TSEStateType read FStatus;

    // for audio plugs only
    property ValueNonAudio: TAutoduplicatePlugData read FAutoDuplicatePlugVar;

    property OnStatusupdate: TSEPinStatusUpdateEvent read FOnStatusUpdate write FOnStatusUpdate;
  end;

  TSEPins = array of TSEPin;

  TUgFunc = procedure of object;

  TSEModuleBase = class(TObject)
  private
    FOnOpen                  : TNotifyEvent;
    FOnClose                 : TNotifyEvent;
    FOnResume                : TNotifyEvent;
    FOnSampleRateChangeEvent : TNotifyEvent;
    FOnBlockSizeChangeEvent  : TNotifyEvent;
    FOnPlugStateChangeEvent  : TSEPlugStateChangeEvent;
    FOnProcessEvent          : TSE2ProcessEvent;
    FOnEventEvent            : TSE2EventEvent;
    FOnMidiData              : TSEMidiDataEvent;
    FOnInputStatusChange     : TSEInputStateChangedEvent;
    FOnGuiNotify             : TSEGuiNotifyEvent;
    FOnVoiceReset            : TSEVoiceResetEvent;
    FOnProgramChange         : TNotifyEvent;

    function GetEffect: PSE2ModStructBase;
    function GetSampleClock: Cardinal;
    function GetPin(Index: Integer): TSEPin;
    function GetPinPropertiesClean(const Index: Integer; Properties: PSEPinProperties): Boolean;
    function GetTotalPinCount: Integer;
    function GetInputPinCount: Integer;
    function GetOutputPinCount: Integer;
    procedure SetProcess(const Value: TSE2ProcessEvent);
    procedure SetSampleRate(const Value: Single);
    procedure SetBlockSize(const Value: Integer);
    procedure ProcessIdle(const BufferPos, SampleFrames: Integer);
  protected
    FSEAudioMaster : TSE2AudioMasterCallback;
    FEffect        : TSE2ModStructBase;
    FSampleRate    : Single;
    FBlockSize     : Integer;
    FPins          : TSEPins;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; virtual;
    function GetName(Name: PAnsiChar): Boolean; virtual;     // name max 32 Char
    function GetUniqueId(Name: PAnsiChar): Boolean; virtual; // id max 32 Char

    procedure Open; virtual;
    procedure Close; virtual;
    procedure Resume; virtual;
    procedure VoiceReset(Future: Integer); virtual;
    procedure SampleRateChanged; virtual;
    procedure BlockSizeChanged; virtual;
    procedure PlugStateChange(const CurrentPin: TSEPin); virtual;
    procedure InputStatusChange(PlugIndex: Integer; NewState: TSEStateType); virtual;
    procedure MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer); virtual;
    procedure GuiNotify(AUserMsgID: Integer; ASize: Integer; AData: Pointer); virtual;

    { divert to virtual function }
    procedure HandleEvent(Event: PSEEvent); virtual;
    function Dispatcher(Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; virtual;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); virtual;
    destructor Destroy; override;

    function ResolveFileName(const Pin: Integer): TFileName; overload;
    function ResolveFileName(const FileName: TFileName): TFileName; overload;

    procedure AddEvent(Event: TSEEvent);
    procedure RunDelayed(SampleClock: Cardinal; Func: TUgFunc);

    { inquiry }
    function CallHost(Opcode: TSEHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); virtual;

    property Effect: PSE2ModStructBase read GetEffect;
    property Pin[Index: Integer]: TSEPin read GetPin;
    property SampleClock: Cardinal read GetSampleClock;
    property TotalPinCount: Integer read GetTotalPinCount;
    property InputPinCount: Integer read GetInputPinCount;
    property OutputPinCount: Integer read GetOutputPinCount;

    property SampleRate: Single read FSampleRate;
    property BlockSize: Integer read FBlockSize;

    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnProcess: TSE2ProcessEvent read FOnProcessEvent write SetProcess;
    property OnEvent: TSE2EventEvent read FOnEventEvent write FOnEventEvent;
    property OnMidiData: TSEMidiDataEvent read FOnMidiData write FOnMidiData;
    property OnProgramChange: TNotifyEvent read FOnProgramChange write FOnProgramChange;
    property OnSampleRateChange: TNotifyEvent read FOnSampleRateChangeEvent write FOnSampleRateChangeEvent;
    property OnBlockSizeChange: TNotifyEvent read FOnBlockSizeChangeEvent write FOnBlockSizeChangeEvent;
    property OnPlugStateChange: TSEPlugStateChangeEvent read FOnPlugStateChangeEvent write FOnPlugStateChangeEvent;
    property OnInputStateChanged: TSEInputStateChangedEvent read FOnInputStatusChange write FOnInputStatusChange;
  end;

//////////////////////
// static functions //
//////////////////////

function SE1Dispatcher(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
function SE2Dispatcher(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
procedure SE1Process(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
procedure SE2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
procedure SE2Event(ModuleBase: TSEModuleBase; Event: PSEEvent); cdecl;

// handy function to fix denormals.
procedure KillDenormal(var Sample: Single);

function IOFlagToString(IOFlag: TSEIOFlag): string;
function IOFlagsToString(IOFlags: TSEIOFlags): string;
function PropertyFlagsToString(Flags: TUgFlags): string;
function PropertyGUIFlagsToString(Flags: TGuiFlags): string;

implementation

uses
  AnsiStrings;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

constructor TSEPin.Create;
begin
 // nothing in here yet!
end;

destructor TSEPin.Destroy;
begin
 inherited;
end;

procedure TSEPin.Init(Module: TSEModuleBase; PinIndex: Integer; DataType: TSEPlugDataType; VariablePtr: Pointer);
begin
 FModule       := Module;
 FPinIndex     := PinIndex;
 FDataType     := DataType;
 FVariablePtr  := VariablePtr;
 FStatus       := stStop;

 // auto-duplicate pins hold their own pointer to the audio buffer
 if (FVariablePtr = nil) then
  begin
   case FDataType of
    dtFSample:
     begin
      FAutoDuplicatePlugVar.FloatPtr := PSingle(FModule.CallHost(SEAudioMasterGetPinVarAddress, FPinIndex));
      FVariablePtr := @FAutoDuplicatePlugVar.FloatPtr;
     end;
    dtEnum:
     begin
      FAutoDuplicatePlugVar.EnumValue := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.EnumValue;
     end;
    dtBoolean:
     begin
      FAutoDuplicatePlugVar.BoolValue := False;
      FVariablePtr := @FAutoDuplicatePlugVar.BoolValue;
     end;
    dtSingle:
     begin
      FAutoDuplicatePlugVar.FloatValue := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.FloatValue;
     end;
    dtInteger:
     begin
      FAutoDuplicatePlugVar.IntValue := 0;
      FVariablePtr := @FAutoDuplicatePlugVar.IntValue;
     end;
    dtText:
     begin
      (* don't work
      // FAutoDuplicatePlugVar.TextPtr := 0;
      FAutoDuplicatePlugVar.TextPtr := PPAnsiChar(FModule.CallHost(SEAudioMasterGetPinVarAddress, FPinIndex));
      FVariablePtr := @FAutoDuplicatePlugVar.TextPtr;
      *)
     end;
  end;
 end else FAutoDuplicatePlugVar.FloatPtr := nil;
end;

function TSEPin.GetIsConnected: Boolean;
begin
 Result := FModule.CallHost(SEAudioMasterIsPinConnected, FPinIndex, 0, nil) <> 0;
end;

procedure TSEPin.StatusUpdate(AStatus: TSEStateType);
begin
 if Assigned(FOnStatusUpdate) then FOnStatusUpdate(Self, AStatus);
 FStatus := AStatus;
 Module.PlugStateChange(Self);
 if AStatus = stOneOff // one-offs need re-set once processed
  then FStatus := stStop;
end;

function TSEPin.GetValue: Single;
var
  BlockPos    : Cardinal;
  SampleClock : Cardinal;
begin
 Assert(FDataType = dtFSample);
 Assert(Assigned(FModule));
 BlockPos := FModule.CallHost(SEAudioMasterGetBlockStartClock, FPinIndex, 0, nil);
 SampleClock := FModule.SampleClock;
 Result := PDAVSingleFixedArray(FVariablePtr^)[(SampleClock - BlockPos)];
end;

procedure TSEPin.TransmitStatusChange(SampleClock: Cardinal; NewState: TSEStateType);
begin
 if Assigned(FModule)
  then FModule.CallHost(SEAudioMasterSetPinStatus, FPinIndex, Integer(NewState), Pointer(SampleClock));
end;

procedure TSEPin.TransmitMIDI(SampleClock: Cardinal; Msg: Cardinal);
begin
 // MIDI data must allways be timestamped at or after the current 'clock'.
 if Assigned(FModule) then
  begin
   Assert(SampleClock >= FModule.SampleClock);
   FModule.CallHost(SEAudioMasterSendMIDI, FPinIndex, Msg, Pointer(SampleClock));
  end;
end;

{ TSEModuleBase }

constructor TSEModuleBase.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 FPins := nil;
 FSEAudioMaster := AudioMaster;

 FillChar(FEffect, SizeOf(TSE2ModStructBase), 0);

 FOnProcessEvent := ProcessIdle;
 with FEffect do
  begin
   Magic           := CSepMagic;
   Version         := 1;
   HostPtr         := Reserved;
   SEModule        := Self;
   Dispatcher      := SE2Dispatcher;
   EventHandlerPtr := SE2Event;
   SubProcessPtr   := SE2Process;
  end;

 FSampleRate := 44100;
 FBlockSize := 1024;
end;

destructor TSEModuleBase.Destroy;
var
  i : Integer;
begin
 for i := 0 to Length(FPins) - 1
  do FreeAndNil(FPins[i]);
 SetLength(FPins, 0); 
 inherited;
end;


procedure TSEModuleBase.Open;
var
  i, ActualPlugCount   : Integer;
  PlugDescriptionIndex : Integer;
  Properties           : TSEPinProperties;
begin
 // set sampleclock
 //  SetSampleClock(CallHost(SEAudioMasterGetSampleClock, 0, 0, 0));

 // get actual number of pins used (may be more or less if auto-duplicating plugs used)
 ActualPlugCount := TotalPinCount;

 if ActualPlugCount > 0 then
  begin
   SetLength(FPins, ActualPlugCount);
   for i := 0 to ActualPlugCount - 1
    do FPins[i] := TSEPin.Create;
  end;

 // Set up standard plugs
 PlugDescriptionIndex := 0;
 i := 0;

 while (GetPinPropertiesClean(PlugDescriptionIndex, @Properties)) and (i < ActualPlugCount) do
  begin
   if (not (iofUICommunication in Properties.Flags)) or
      (          iofUIDualFlag in Properties.Flags) then // skip GUI plugs
    begin
     FPins[i].Init(Self, I, TSEPlugDataType(Properties.DataType), Properties.VariableAddress);
     Inc(i);
    end;
   Inc(PlugDescriptionIndex);
  end;

 // now set up any additional 'autoduplicate' plugs
 // Assumed they are the last plug described in GetPinProperties

 // Get the properites of last pin
 GetPinPropertiesClean(PlugDescriptionIndex - 1, @Properties);

 if (not (iofUICommunication in Properties.Flags)) or
    (          iofUIDualFlag in Properties.Flags) then // skip GUI plugs
   if (iofAutoDuplicate in Properties.Flags) then
    while (i < ActualPlugCount) do
     begin
      FPins[i].Init(Self, i, TSEPlugDataType(Properties.DataType), nil);
      inc(i);
     end;
 if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TSEModuleBase.Close;
begin
 if Assigned(FOnClose) then FOnClose(Self);
end;


function TSEModuleBase.Dispatcher(opCode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
var
  Event : TSEEvent;
begin
 Result := 0;
 case opCode of
  seffOpen  : Open;
  seffClose : begin
               Close;
               Free;
               Result := 1;
              end;
  seffSetSampleRate: setSampleRate(Opt);
  seffSetBlockSize: setBlockSize(Value);
  seffGetPinProperties:
   begin
    Result := Integer(GetPinProperties(Index, PSEPinProperties(Ptr)));
(*
    // check for illegal flag combinations
    // 'Dual' Input plugs must be private (GuiModule can set the Value, but User must not be able to)
    Assert((not iofUICommunicationDual in SEPinProperties(Ptr).Flags) or
               (iofPrivate in PSEPinProperties(Ptr).Flags) or
               (PSEPinProperties(Ptr).direction = drOut);
*)

    // 'Patch Store' Input plugs must be private, or GuiModule
    Assert((not (iofPatchStore in PSEPinProperties(Ptr).Flags)) or
                (iofHidePin in PSEPinProperties(Ptr).Flags) or
                (PSEPinProperties(Ptr).Direction = drOut) or
                (iofUICommunication in PSEPinProperties(Ptr).Flags));
   end;
  seffGetModuleProperties:
   begin
    // obsolete
    // getModuleProperties ( (SEModuleProperties * )Ptr) ? 1 : 0;
    Result := 0;
   end;
  seffGetEffectName:
   begin
//    StrCopy(Ptr, PAnsiChar('obsolete'));

    Result := 0;
   end;
  seffGetUniqueId:
   begin
//    StrCopy(Ptr, PAnsiChar('obsolete'));
    Result := 0;
   end;
  seffAddEvent:
   begin
    Assert(False); // not used in SDK2
    Event := PSEEvent(Ptr)^; // can't directly use object allocated by host, make a copy
//    AddEvent(Event);
   end;
  seffResume:
   begin
    // Index = 0 - Module initiated sleep
    // Index = 1 - Voice initiated sleep. Indicates new note.
    Resume;
    if (Index > 0) then VoiceReset(Index);
   end;
  seffIsEventListEmpty:
   begin
    Assert(false);  // not used in SDK2
//      return events == 0 ? 1 : 0;
   end;
  seffGetSdkVersion: Result := CSeSdkVersion;
  seffGuiNotify: GuiNotify(Value, Index, Ptr);
  seffQueryDebugInfo:
   begin
(*
    static int info[4];
    info[0] = 1; // Version number
    info[1] = sizeof(process_function_ptr2);
    info[2] = (int) &m_process_function_pointer.pointer;
    info[3] = 0;
    return (long) info;
*)
   end;
  end;
end;

function TSEModuleBase.GetPin(Index: Integer): TSEPin;
begin
 if (Length(FPins) > Index) and (Index >= 0)
  then Result := FPins[Index]
  else Result := nil;
end;

function TSEModuleBase.CallHost(Opcode: TSEHostOpcodes; Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
begin
 if Assigned(FSEAudioMaster)
  then Result := FSEAudioMaster(@FEffect, Opcode, Index, Value, Ptr, Opt)
  else Result := 0;
end;

function TSEModuleBase.GetSampleClock: Cardinal;
begin
  Result := CallHost(SEAudioMasterGetSampleClock);
end;

function TSEModuleBase.GetTotalPinCount: Integer;
begin
 Result := CallHost(SEAudioMasterGetTotalPinCount);
end;

procedure TSEModuleBase.GuiNotify(AUserMsgID, ASize: Integer; AData: Pointer);
begin
 if Assigned(FOnGuiNotify)
  then FOnGuiNotify(Self, AUserMsgID, ASize, AData);
end;

procedure TSEModuleBase.InputStatusChange(PlugIndex: Integer;
  NewState: TSEStateType);
begin
 if Assigned(FOnInputStatusChange)
  then FOnInputStatusChange(Self, PlugIndex, NewState);
end;

procedure TSEModuleBase.MidiData(AClock, AMidiMsg: Cardinal; PinID: Integer);
begin
 if Assigned(FOnMidiData)
  then FOnMidiData(Self, AClock, AMidiMsg, PinID);
end;

procedure TSEModuleBase.PlugStateChange(const CurrentPin: TSEPin);
begin
 if Assigned(FOnPlugStateChangeEvent)
  then FOnPlugStateChangeEvent(Self, CurrentPin);
end;

procedure TSEModuleBase.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TSEModuleBase.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TSEModuleBase.BlockSizeChanged;
begin
 if Assigned(FOnBlockSizeChangeEvent)
  then FOnBlockSizeChangeEvent(Self);
end;

procedure TSEModuleBase.SampleRateChanged;
begin
 if Assigned(FOnSampleRateChangeEvent)
  then FOnSampleRateChangeEvent(Self);
end;

procedure TSEModuleBase.SetProcess(const Value: TSE2ProcessEvent);
begin
 if @FOnProcessEvent <> @Value then
  if Assigned(Value)
   then FOnProcessEvent := Value
   else FOnProcessEvent := ProcessIdle;
end;

procedure TSEModuleBase.VoiceReset(Future: Integer);
begin
 if Assigned(FOnVoiceReset)
  then FOnVoiceReset(Self, Future);
end;

// gets a pins properties, after clearing the structure (prevents garbage getting in)
function TSEModuleBase.GetEffect: PSE2ModStructBase;
begin
 Result := @FEffect;
end;

class procedure TSEModuleBase.GetModuleProperties(Properties: PSEModuleProperties);
var
  ModuleFileName : array[0..MAX_PATH] of AnsiChar;
begin
 // describe the plugin, this is the name the end-user will see.
 with Properties^ do
  begin
   Name := 'Delphi ASIO & VST Packages';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   try
    GetModuleFileNameA(HInstance, ModuleFileName, SizeOf(ModuleFileName));
//    FileName := ;
    ID := PAnsiChar(ExtractFileName(ModuleFileName));
   except
    ID := 'Delphi ASIO & VST Packages';
   end;

   // Info, may include Author, Web page whatever
   About := 'Delphi ASIO & VST Packages';
  end;
end;

function TSEModuleBase.GetName(Name: PAnsiChar): Boolean;
begin
 Result := False;
end;

function TSEModuleBase.GetInputPinCount: Integer;
begin
 Result := CallHost(SEAudioMasterGetInputPinCount);
end;

function TSEModuleBase.GetOutputPinCount: Integer;
begin
 Result := CallHost(SEAudioMasterGetOutputPinCount);
end;

function TSEModuleBase.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 {return m_sample_clock;}
 Result := False;
end;

function TSEModuleBase.GetPinPropertiesClean(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 FillChar(Properties^, SizeOf(TSEPinProperties), 0); // clear structure
 Result := GetPinProperties(Index, Properties);
end;

function TSEModuleBase.GetUniqueId(Name: PAnsiChar): Boolean;
begin
 Result := False;
end;

procedure TSEModuleBase.HandleEvent(Event: PSeEvent);
begin
 Assert(Event.TimeStamp = SampleClock);
 case Event.EventType of
  uetStatChange: GetPin(Integer(Event.PtrParam)).StatusUpdate(TSEStateType(Event.IntParamA));
(* not used anymore
    case UET_RUN_FUNCTION: // buggy ( dll can't allocate mem and attach to event, causes crash when SE trys to free it)
      begin
        // PtrParam points to a ug_func pointer (allocated)
        function_pointer *fp = (function_pointer* ) Event.PtrParam;
        fp.Run;

        // TODO!!!!would be better to perform deletion in event detructor
        // will prevent mem leaks on events that are deleted without being used (due to power-off situation)
        delete fp;
        Event.PtrParam = NULL;
      end;
      break;
*)
  uetRunFunction2:
   begin
(* TODO!!!
//    ug_func Func = 0; // important to initialise (code below is a hack)
//    *(PInteger(@Func) = *(PInteger(@(Event.PtrParam));
//    (this.*(Func));
        my_delegate<ug_func> temp_delegate(Event.PtrParam);
//        temp_delegate.pointer.native = 0;
//        temp_delegate.pointer.raw_pointer = Event.PtrParam;
        (this.*(temp_delegate.pointer.native));
*)
   end;
(*  case UET_UI _NOTIFY2:
      GuiNotify( Event.IntParamA, (void * ) Event.IntParamB );
      free( (void * ) Event.IntParamB ); // free memory block
      break;*)
  uetProgChange: if Assigned(FOnProgramChange)
                  then FOnProgramChange(Self);
  uetMIDI : MidiData(Event.TimeStamp, Event.IntParamA, Event.IntParamB);
  else; // Assert(false); // un-handled event
 end;
 if Assigned(OnEvent)
  then OnEvent(Event)
end;

procedure TSEModuleBase.RunDelayed(SampleClock: Cardinal; Func: TUgFunc);
begin
//  my_delegate<ug_func> temp_delegate(Func);

//  temp_delegate.pointer.native = Func;
(*
  // NO, can't allocate events here (in dll)
//  function_pointer *fp = new function_pointer( this, Func );
  void *function_address;
  *( (int* ) &(function_address)) = *( (int* ) &Func); // copy first 32 bits of function pointer
*)

// AddEvent(SampleClock, Integer(uetRunFunction2), 0, 0, temp_delegate.RawPointer);
end;

// insert event sorted.  Pass pointer to tempory event structure(event data will be copied by SE)
procedure TSEModuleBase.AddEvent(Event: TSEEvent);
begin
 Assert(Event.TimeStamp >= SampleClock);
 CallHost(SEAudioMasterAddEvent, 0, 0, @Event);

//  delete p_event;

(*
  unsigned long TimeStamp = p_event.TimeStamp;

  SeEvent *e = events;
  SeEvent *prev = 0;
  while(true)
  begin
    if( e == 0 || e.TimeStamp > TimeStamp ) // events with same time must be added in same order received (for stat changes to work properly)
    begin
      p_event.Next = e;
      if( prev )
      begin
        prev.Next = p_event;
      end;
      else
      begin
        events = p_event;
      end;
      return;
    end;
    prev = e;
    e = e.Next;
  end;*)
end;

function TSEModuleBase.ResolveFileName(const FileName: TFileName): TFileName;
{$IFDEF SUPPORTS_UNICODE}
var
  AnsiFileName : AnsiString;
  AnsiResult   : AnsiString;
{$ENDIF}
begin
 SetLength(Result, 256);
{$IFDEF SUPPORTS_UNICODE}
 AnsiFileName := AnsiString(FileName);
 CallHost(SEAudioMasterResolveFilename2, Integer(PAnsiChar(AnsiFileName)),
   Length(Result), PAnsiChar(AnsiResult));
 Result := string(AnsiResult);
{$ELSE}
 CallHost(SEAudioMasterResolveFilename2, Integer(PAnsiChar(FileName)),
   Length(Result), PAnsiChar(Result));
{$ENDIF}
end;

function TSEModuleBase.ResolveFileName(const Pin: Integer): TFileName;
var
  str: array[0..1023] of char;
begin
 CallHost(SEAudioMasterResolveFilename, Pin, Length(str), @str[0]);
 Result := StrPas(str);
end;

procedure TSEModuleBase.Resume; // from either sleep or suspend
(*
var
  e : PSeEvent;
*)
begin
 // not used, see original SDK
(*
 SetSampleClock(CallHost(SEAudioMasterGetSampleClock, 0, 0, 0 ));

 // update the time on any pending events
 // this applies to resume from suspend only
 e := events;
 while (e <> nil) do
  begin
   if e.TimeStamp < SampleClock
    then e.TimeStamp := SampleClock;
   e = e.Next;
  end;
*)
 if Assigned(FOnResume) then FOnResume(Self);
end;

procedure TSEModuleBase.ProcessIdle(const BufferPos, SampleFrames: Integer);
begin
 // do nothing here!
end;


//////////////////////
// static functions //
//////////////////////

function SE1Dispatcher(Effect: PSE1ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
 Result := Effect.SEModule.Dispatcher(opCode, Index, Value, Ptr, Opt);
end;

function SE2Dispatcher(Effect: PSE2ModStructBase; Opcode: TSEPluginModuleOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
 Result := Effect.SEModule.Dispatcher(opCode, Index, Value, Ptr, Opt);
end;

procedure SE1Process(Effect: PSE1ModStructBase; inputs, outputs: PDAVArrayOfSingleFixedArray; SampleFrames: Integer); cdecl;
begin
 // not yet supported
end;

procedure SE2Process(ModuleBase: TSEModuleBase; BufferOffset: Integer; SampleFrames: Integer); cdecl;
begin
 with ModuleBase do
  if Assigned(OnProcess)
   then OnProcess(BufferOffset, SampleFrames);
end;

procedure SE2Event(ModuleBase: TSEModuleBase; Event: PSEEvent); cdecl;
begin
 with ModuleBase do HandleEvent(Event);
end;

procedure KillDenormal(var Sample: Single);
const
  CAntiDenormal: Single = 1E-18;
begin
 Sample := Sample + CAntiDenormal;
 Sample := Sample - CAntiDenormal;
end;

function IOFlagToString(IOFlag: TSEIOFlag): string;
begin
 case IOFlag of
  iofPolyphonicActive    : Result := 'Polyphonic Active';
  iofIgnorePatchChange   : Result := 'Ignore Patch Change';
  iofRename              : Result := 'Rename';
  iofAutoDuplicate       : Result := 'Auto Duplicate';
  iofFilename            : Result := 'Filename';
  iofSetableOutput       : Result := 'Setable Output';
  iofCustomisable        : Result := 'Customisable';
  iofAdder               : Result := 'Adder';
  iofHidePin             : Result := 'Hide Pin';
  iofLinearInput         : Result := 'Linear Input';
  iofUICommunication     : Result := 'UI Communication';
  iofAutoEnum            : Result := 'Auto Enum';
  iofHideWhenLocked      : Result := 'Hide When Locked';
  iofParameterScreenOnly : Result := 'Parameter Screen Only';
  iofDoNotCheckEnum      : Result := 'Do Not Check Enum';
  iofUIDualFlag          : Result := 'UI Dual Flag';
  iofPatchStore          : Result := 'Patch Store';
  iofParamPrivate        : Result := 'Parameter Private';
  iofMinimized           : Result := 'Minimized';
  else                     Result := '';
 end;
end;

function IOFlagsToString(IOFlags: TSEIOFlags): string;
begin
 Result := '';
 if iofPolyphonicActive in IOFlags then Result := 'Polyphonic Active';
 if iofIgnorePatchChange in IOFlags then Result := Result + 'Ignore Patch Change,';
 if iofRename in IOFlags then Result := Result + 'Rename,';
 if iofAutoDuplicate in IOFlags then Result := Result + 'Auto Duplicate,';
 if iofFilename in IOFlags then Result := Result + 'Filename,';
 if iofSetableOutput in IOFlags then Result := Result + 'Setable Output,';
 if iofCustomisable in IOFlags then Result := Result + 'Customisable,';
 if iofAdder in IOFlags then Result := Result + 'Adder,';
 if iofHidePin in IOFlags then Result := Result + 'Hide Pin,';
 if iofLinearInput in IOFlags then Result := Result + 'Linear Input,';
 if iofUICommunication in IOFlags then Result := Result + 'UI Communication,';
 if iofAutoEnum in IOFlags then Result := Result + 'Auto Enum,';
 if iofHideWhenLocked in IOFlags then Result := Result + 'Hide When Locked,';
 if iofParameterScreenOnly in IOFlags then Result := Result + 'Parameter Screen Only,';
 if iofDoNotCheckEnum in IOFlags then Result := Result + 'Do Not Check Enum,';
 if iofUIDualFlag in IOFlags then Result := Result + 'UI Dual Flag,';
 if iofPatchStore in IOFlags then Result := Result + 'Patch Store,';
 if iofParamPrivate in IOFlags then Result := Result + 'Parameter Private,';
 if iofMinimized in IOFlags then Result := Result + 'Minimized,';
 if Length(Result) > 0 then SetLength(Result, Length(Result) - 1); 
end;

function PropertyFlagsToString(Flags: TUgFlags): string;
begin
 Result := '';
 if ugfVoiceMonIgnore in Flags then Result := Result + 'Voice Monitor Ignore, ';
 if ugfPolyphonicAgregator in Flags then Result := Result + 'Polyphonic Agregator, ';
 if ugfSuspend in Flags then Result := Result + 'Suspended, ';
 if ugfOpen in Flags then Result := Result + 'Open, ';
 if ugfNeverSuspend in Flags then Result := Result + 'Never Suspend, ';
 if ugfClone in Flags then Result := Result + 'Clone, ';
 if ugfSendTimeinfoToHost in Flags then Result := Result + 'Send TimeInfo to Host';
 if Result = '' then Result := '-'
end;

function PropertyGUIFlagsToString(Flags: TGuiFlags): string;
begin
 Result := '';
 if gfControlView in Flags then Result := Result + 'Control View, ';
 if gfStructureView in Flags then Result := Result + 'Structure View';
 if Result = '' then Result := '-'
end;

end.
