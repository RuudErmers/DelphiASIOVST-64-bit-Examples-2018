unit DAV_VSTBasicModule;

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
//  The initial developer of this code is Christian-W. Budde, additional      //
//  coding and refactoring done by Maik Menz                                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// This unit implements the basic VST-Plugin <--> Host communications
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Forms, Sysutils, DAV_Types, DAV_VSTEffect, DAV_WinAmp;

{$IFDEF DebugLog}
const
  CDebugLogFile : string = 'C:\Temp\Debug.log';
{$ENDIF}

type
  TBasicVSTModuleClass = class of TBasicVSTModule;

  { TBasicVSTModule }

  TBasicVSTModule = class({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  private
    procedure ConvertDummy(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertFloatToInterleaved(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertFloatToInterleaved16bit(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertFloatToInterleaved24bit(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertFloatToInterleaved8bit(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertInterleaved16bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertInterleaved24bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertInterleaved8bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
    procedure ConvertInterleavedToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Cardinal);
  protected
    FEffect             : TVSTEffect;
    FAudioMaster        : TAudioMasterCallbackFunc;

    {$IFNDEF UseAudioEffectPtr}
    FUseAudioEffectPtr  : Boolean;
    {$ENDIF}

    {$IFNDEF UseDelphi}
    FOnCreate           : TNotifyEvent;
    FOnDestroy          : TNotifyEvent;
    {$ENDIF}

    // WinAmp
    FWinAmpDSPModule    : PWinampDSPModule;
    FWinAmpEditorForm   : TForm;
    FWinAmpBypass       : Boolean;
    FWinAmpInputBuffer  : array of PDAVSingleFixedArray;
    FWinAmpOutputBuffer : array of PDAVSingleFixedArray;
    FWinAmpNrChannels   : Integer;
    FWinAmpSampleRate   : Integer;
    FWinAmpSampleFrames : Integer;

    FWinAmpConvertIn    : TWinAmpConvert;
    FWinAmpConvertOut   : TWinAmpConvert;

    {$IFDEF DebugLog}
    FLog                : TStringList;
    FTmStmp             : TDateTime;
    procedure AddLogMessage(const Text: string);
    {$ENDIF}

    function GetEffect: PVSTEffect; virtual;

    procedure SetAudioMaster(const AM: TAudioMasterCallbackFunc); virtual;
    function CallAudioMaster(const Opcode: TAudioMasterOpcode;
      const Index: Integer = 0; const Value: TVstIntPtr = 0;
      const PTR: Pointer = nil; const Opt: Single = 0): TVstIntPtr; virtual;

    procedure WinAmpConfig; virtual;
    procedure WinAmpQuit; virtual;
    function WinAmpModifySamples(const Samples: Pointer; const SampleFrames,
      BitPerSample, ChannelCount, SampleRate: Integer): Integer; virtual;

    function  GetMasterVersion: Integer; virtual;
    function  GetCurrentUniqueID: TChunkName; virtual;
    procedure MasterIdle; virtual;
    function  IsInputConnected(const Input: Integer): Boolean; virtual;
    function  IsOutputConnected(const Output: Integer): Boolean; virtual;

    procedure WantEvents(const Filter: TVstIntPtr);  // filter is currently ignored, midi channel data only (default) virtual void wantEvents (long filter = 1); default is 1 for this!
    function  GetTimeInfo(const Filter: TVstIntPtr): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported) filter should contain a mask indicating which fields are requested (see valid masks in aeffectx.h), as some items may require extensive conversions
    procedure SetTimeInfo(const Filter: TVstIntPtr; var VstTimeInfo: PVstTimeInfo); virtual;
    function  TempoAt(const Pos: TVstIntPtr): TVstIntPtr; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
    function  SendVstEventsToHost(var Events: TVstEvents): Boolean;  // True: success

    function  GetNumAutomatableParameters: Integer; virtual;
    procedure SetParameterAutomated(const Index: Integer; const Value: Single); virtual;
    function  GetParameterQuantization: TVstIntPtr; virtual; // returns the Integer Value for +1.0 representation, or 1 if full single float precision is maintained in automation. parameter Index in <Value> (-1: all, any)

    function  GetInputLatency: TVstIntPtr; virtual;
    function  GetOutputLatency: TVstIntPtr; virtual;
    function  GetPreviousPlug(const Input: TVstIntPtr): PVSTEffect; virtual;  // input can be -1 in which case the first found is returned
    function  GetNextPlug(const Output: TVstIntPtr): PVSTEffect; virtual;     // output can be -1 in which case the first found is returned

    function  WillProcessReplacing: Integer; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
    function  GetCurrentProcessLevel: Integer; virtual;  // returns: 0: not supported, 1: currently in user thread (gui) 2: currently in audio thread or irq (where Process is called) 3: currently in 'sequencer' thread or irq (midi, timer etc) 4: currently offline Processing and thus in user thread other: not defined, but probably pre-empting user thread.
    function  GetAutomationState: Integer; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

    function  OfflineRead(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption; const ReadSource: Boolean): Boolean; virtual;
    function  OfflineWrite(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption): Boolean; virtual;
    function  OfflineStart(var AudioFile: TVstAudioFile; const numAudioFiles: Integer; const numNewAudioFiles: TVstIntPtr): Boolean; virtual;
    function  OfflineGetCurrentPass: Integer; virtual;
    function  OfflineGetCurrentMetaPass: Integer; virtual;

    procedure SetOutputSampleRate(const Samplerate: Single); virtual;

    function  GetHostVendorString(const Text: PAnsiChar): Boolean; virtual;  // fills <Text> with a string identifying the vendor (max 64 char)
    function  GetHostProductString(const Text: PAnsiChar): Boolean; virtual; // fills <Text> with a string with product name (max 64 char)
    function  GetHostVendorVersion: Integer; virtual;  // returns vendor-specific version
    function  HostVendorSpecific(const Index: Integer; const Value: TVstIntPtr; const ptrArg: Pointer; const floatArg: Single): Integer; virtual;  // no definition
    function  GetCanHostDo(Text: AnsiString): Integer; virtual;  // see 'hostCanDos' in audioeffectx.cpp returns 0: don't know (default), 1: yes, -1: no
    function  GetHostLanguage: Integer; virtual;   // returns VstHostLanguage
    function  OpenWindow(const aWindow: PVstWindow): Pointer; virtual;  // create new window
    function  CloseWindow(const aWindow: PVstWindow): Boolean; virtual; // close a newly created window
    function  GetDirectory: Pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*

    function  UpdateDisplay: Boolean; virtual; // something has changed, update 'multi-fx' display returns True if supported
    function  IOChanged: Boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
    function  NeedIdle: Boolean; virtual;    // plug needs idle calls (outside its editor window)
    function  SizeWindow(const Width, Height: Integer): Boolean; virtual;

    function  BeginEdit(const Index: Integer): Boolean; virtual; // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
    function  EndEdit(const Index: Integer): Boolean; virtual;   // to be called after a setParameterAutomated (on Mouse Up)

    function  OpenFileSelector(var VstFileSelect: TVstFileSelect): Boolean; virtual;
    function  CloseFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
    function  GetChunkFile(const NativePath: Pointer): Boolean;

    // HostCalls, protected methods that can be overwritten, but shall remain
    // hidden, since the user should not be able to CALL them directly!
    function HostCallOpen                      (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallClose                     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetProgram                (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProgram                (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetProgramName            (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProgramName            (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParamLabel             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParamDisplay           (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParamName              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVu                     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetSampleRate             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetBlockSize              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallMainsChanged              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditGetRect               (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditOpen                  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditClose                 (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditDraw                  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditMouse                 (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditKey                   (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditIdle                  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditTop                   (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditSleep                 (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallIdentify                  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetChunk                  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetChunk                  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallProcessEvents             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallCanBeAutomated            (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallString2Parameter          (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetNumProgramCategories   (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProgramNameIndexed     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallCopyProgram               (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallConnectInput              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallConnectOutput             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetInputProperties        (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetOutputProperties       (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetPlugCategory           (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetCurrentPosition        (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetDestinationBuffer      (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallOfflineNotify             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallOfflinePrepare            (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallOfflineRun                (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallProcessVarIo              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetSpeakerArrangement     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetBlockSizeAndSampleRate (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetBypass                 (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetEffectName             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetErrorText              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVendorString           (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetProductString          (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVendorVersion          (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallVendorSpecific            (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallCanDo                     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetTailSize               (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallIdle                      (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetIcon                   (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetViewPosition           (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetParameterProperties    (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallKeysRequired              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetVstVersion             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditKeyDown               (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEditKeyUp                 (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetEditKnobMode           (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetMidiProgramName        (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetCurrentMidiProgram     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetMidiProgramCategory    (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallHasMidiProgramsChanged    (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetMidiKeyName            (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallBeginSetProgram           (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallEndSetProgram             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetSpeakerArrangement     (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallShellGetNextPlugin        (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallStartProcess              (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallStopProcess               (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetTotalSampleToProcess   (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetPanLaw                 (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallBeginLoadBank             (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallBeginLoadProgram          (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallSetProcessPrecision       (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetNumMidiInputChannels   (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function HostCallGetNumMidiOutputChannels  (const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;

    procedure HostCallProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); virtual; abstract;
    procedure HostCallProcess32Replacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal); virtual; abstract;
    procedure HostCallProcess64Replacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal); virtual; abstract;

    function  HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; virtual;
    function  HostCallGetParameter(const Index: Integer): Single; virtual; abstract;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); virtual; abstract;

    function  UpdateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
    function  UpdateBlockSize: Integer; virtual;  // same for block size

    function  GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
    function  GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetStaticDescription: string; virtual;

    property Effect: PVSTEffect read GetEffect;
    property AudioMaster: TAudioMasterCallbackFunc read FAudioMaster write SetAudioMaster;
    {$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    {$ENDIF}

    {$IFDEF DebugLog} property DebugLog: TStringList read FLog; {$ENDIF}
  end;

  EVstError = class(Exception);

function DispatchEffectFuncAudioEffectPtr(Effect: PVSTEffect; OpCode : TDispatcherOpCode;
  const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer;
  const opt: Single): TVstIntPtr; cdecl;
function GetParameterFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Index: Integer): Single; cdecl;
procedure SetParameterFuncAudioEffectPtr(const Effect: PVSTEffect; const Index: Integer;
  const Value: Single); cdecl;

{$IFNDEF UseAudioEffectPtr}
function DispatchEffectFuncUserPtr(Effect: PVSTEffect; OpCode : TDispatcherOpCode;
  const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer;
  const opt: Single): TVstIntPtr; cdecl;
function GetParameterFuncUserPtr(const Effect: PVSTEffect;
  const Index: Integer): Single; cdecl;
procedure SetParameterFuncUserPtr(const Effect: PVSTEffect; const Index: Integer;
  const Value: Single); cdecl;
{$ENDIF}

// checks (just in case)
procedure ProcessFuncCheck(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
procedure Process32ReplacingFuncCheck(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
procedure Process64ReplacingFuncCheck(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer); cdecl;

procedure ProcessFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
procedure Process32ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
procedure Process64ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer); cdecl;

{$IFNDEF UseAudioEffectPtr}
procedure ProcessFuncUserPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
procedure Process32ReplacingFuncUserPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
procedure Process64ReplacingFuncUserPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer); cdecl;
{$ENDIF}

// Dummy calls
function GetParameterFuncDummy(const Effect: PVSTEffect;
  const Index: Integer): Single; cdecl;
procedure SetParameterFuncDummy(const Effect: PVSTEffect;
  const Index: Integer; const Value: Single); cdecl;
procedure ProcessFuncDummy(const Effect: PVSTEffect;
  const Inputs, Outputs: Pointer; const SampleFrames: Cardinal); cdecl;

// WinAmp
function Init(const WinAmpDSPModule: PWinAmpDSPModule): Integer; cdecl;
procedure Config(const WinAmpDSPModule: PWinAmpDSPModule); cdecl;
function ModifySamples(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
function ModifySamplesDummy(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
procedure Quit(const WinAmpDSPModule: PWinAmpDSPModule); cdecl;

function VstModuleMain(AudioMasterCallback: TAudioMasterCallbackFunc;
  const BasicVSTModuleClass: TBasicVSTModuleClass): PVSTEffect;
function WinampDSPModuleHeader(const BasicVSTModuleClass: TBasicVSTModuleClass): PWinAmpDSPHeader;
function GetWinampModule(const Which : Integer): PWinAmpDSPModule; cdecl;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses
  Math, Contnrs, DAV_Common;

var
  GVstInstanceList : TObjectList;

{$IFNDEF PUREPASCAL}
const
  {$IFDEF UseDelphi}
  CHostCallProcessOffset = $00000244;
  CHostCallProcess32ReplacingOffset = $00000248;
  CHostCallProcess64ReplacingOffset = $0000024C;
  {$ELSE}
  CHostCallProcessOffset = $00000238; // $0000022C;
  CHostCallProcess32ReplacingOffset = $0000023C; // $00000230;
  CHostCallProcess64ReplacingOffset = $00000240; // $00000234;
  {$ENDIF}
{$ENDIF}

function VstModuleMain(AudioMasterCallback: TAudioMasterCallbackFunc;
  const BasicVSTModuleClass: TBasicVSTModuleClass): PVSTEffect;
var
  BasicVstModule : TBasicVSTModule;
begin
 try
  BasicVstModule := BasicVSTModuleClass.Create(Application);
  with BasicVstModule do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
  GVstInstanceList.Add(BasicVstModule);
 except
  Result := nil;
 end;
end;

var
  WADSPHeader  : TWinAmpDSPheader;
  WAVstModule  : TBasicVSTModuleClass;

function GetWinampModule(const Which: Integer): PWinAmpDSPModule; cdecl;
begin
 case Which of
   0 : begin
        GetMem(Result, SizeOf(TWinAmpDSPModule));
        Result^.Description := PAnsiChar(AnsiString(WAVstModule.GetStaticDescription));
        Result^.Init := Init;
        Result^.Config := Config;
        Result^.ModifySamples := ModifySamplesDummy;
        Result^.Quit := Quit;
       end
 else
  Result := nil;
 end;
end;

function WinampDSPModuleHeader(const BasicVSTModuleClass: TBasicVSTModuleClass): PWinAmpDSPHeader;
begin
 WAVstModule := BasicVSTModuleClass;
 try
  with WADSPHeader do
   begin
    Version     := $20;
    Key         := $21;
    Description := PAnsiChar(AnsiString(WAVstModule.GetStaticDescription));
    GetModule   := GetWinampModule;
   end;
  Result := @WADSPHeader;
 except
  Result := nil;
 end;
end;


{ TBasicVSTModule }

constructor TBasicVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF DebugLog}
 FLog := TStringList.Create;
 FTmStmp := Now;
 FLog.Add('Create: ' + TimeToStr(FTmStmp));
 if not (csDesigning in ComponentState) then
  try
   FLog.SaveToFile(CDebugLogFile);
  except
  end;
 {$ENDIF}

 {$IFDEF UseDelphi} inherited CreateNew(AOwner); {$ENDIF}
 with FEffect do
  begin
   Magic           := 'PtsV';
   EffectFlags     := [effFlagsCanReplacing];
   ReservedForHost := nil;
   Resvd2          := nil;
   {$IFDEF UseAudioEffectPtr}
   AudioEffectPtr  := Self;
   User            := nil;
   {$ELSE}
   AudioEffectPtr  := nil;
   User            := Self;
   {$ENDIF}
   uniqueID        := 'fEoN';
   ioRatio         := 1;
   numParams       := 0;
   numPrograms     := 0;
   numInputs       := 2;
   numOutputs      := 2;

   {$IFNDEF UseAudioEffectPtr}
   Dispatcher         := @DispatchEffectFuncUserPtr;
   SetParameter       := @SetParameterFuncUserPtr;
   GetParameter       := @GetParameterFuncUserPtr;
   {$ELSE}
   Dispatcher         := @DispatchEffectFuncAudioEffectPtr;
   SetParameter       := @SetParameterFuncAudioEffectPtr;
   GetParameter       := @GetParameterFuncAudioEffectPtr;
   {$ENDIF}
   Process            := @ProcessFuncCheck;
   Process32Replacing := @Process32ReplacingFuncCheck;
   Process64Replacing := @Process64ReplacingFuncCheck;
  end;

 FWinAmpDspModule    := nil;
 FWinAmpSampleRate   := 44100;
 FWinAmpSampleFrames := 0;
 FWinAmpNrChannels   := 0;
 FWinAmpBypass       := False;

 {$IFNDEF UseAudioEffectPtr}
 FUseAudioEffectPtr  := False;
 {$ENDIF}
end;

destructor TBasicVSTModule.Destroy;
var
  Index : Integer;
begin
 try
  {$IFDEF DebugLog}
  AddLogMessage('TBasicVSTModule.Destroy');
  if Assigned(FLog) then FLog.SaveToFile(CDebugLogFile);
  {$ENDIF}
  for Index := 0 to Length(FWinAmpInputBuffer)  - 1 do Dispose(FWinAmpInputBuffer[Index]);
  for Index := 0 to Length(FWinAmpOutputBuffer) - 1 do Dispose(FWinAmpOutputBuffer[Index]);
 finally
   inherited;
  {$IFDEF DebugLog} if Assigned(FLog) then FreeAndNil(FLog); {$ENDIF}
 end;
end;

{$IFDEF DebugLog}
procedure TBasicVSTModule.AddLogMessage(const Text: string);
begin
 if Assigned(FLog) then
  try
   FLog.Add(TimeToStr(Now - FTmStmp) + ' | ' + Text);
   if not (csDesigning in ComponentState) then
    begin
     if FileExists(CDebugLogFile + '.bak')
      then DeleteFile(CDebugLogFile + '.bak');
     if FileExists(CDebugLogFile)
      then RenameFile(CDebugLogFile, CDebugLogFile + '.bak');
     FLog.SaveToFile(CDebugLogFile);
    end;
  except
  end;
end;
{$ENDIF}

function TBasicVSTModule.GetEffect: PVSTEffect;
begin
 Result := @FEffect;
end;

procedure TBasicVSTModule.SetAudioMaster(const AM :TAudioMasterCallbackFunc);
{$IFNDEF UseAudioEffectPtr}
var
  HostProduct : PAnsiChar;
{$ENDIF}  
begin
 {$IFDEF DebugLog} AddLogMessage('TBasicVSTModule.SetAudioMaster'); {$ENDIF}

 FAudioMaster := AM;
 if CallAudioMaster(amVersion) = 0
  then raise EVstError.Create('AudioMaster Error');

 {$IFDEF DebugLog} AddLogMessage('-> After check audiomaster version'); {$ENDIF}

 {$IFNDEF UseAudioEffectPtr}
 GetMem(HostProduct, 256);
 FillChar(HostProduct^, 256, 0);
 try
  GetHostProductString(HostProduct);
  FUseAudioEffectPtr := (StrPos('energyXT', HostProduct) <> nil) or
    (StrPos('Sound Forge Pro 10.0', HostProduct) <> nil);

  {$IFDEF DebugLog} AddLogMessage('-> Get Host Product String'); {$ENDIF}

  if FUseAudioEffectPtr then
   with FEffect do
    begin
     AudioEffectPtr := Self;
     User           := nil;
     Dispatcher     := @DispatchEffectFuncAudioEffectPtr;
     SetParameter   := @SetParameterFuncAudioEffectPtr;
     GetParameter   := @GetParameterFuncAudioEffectPtr;
    end;
 finally
  Dispose(HostProduct);
 end;
 {$ENDIF}
end;

function TBasicVSTModule.CallAudioMaster(const Opcode: TAudioMasterOpcode;
  const Index: Integer = 0; const Value: TVstIntPtr = 0;
  const PTR: Pointer = nil; const Opt: Single = 0): TVstIntPtr;
begin
 {$IFDEF DebugLog}
 AddLogMessage('TBasicVSTModule.CallAudioMaster; Opcode: ' +
   IntToStr(Integer(Opcode)));
 {$ENDIF}

 if Assigned(FAudioMaster)
  then Result := FAudioMaster(@FEffect, Opcode, Index, Value, PTR, Opt)
  else Result := 0;
end;


class function TBasicVSTModule.GetStaticDescription: string;
begin
 Result := 'Delphi ASIO & VST Package Plugin';
end;


// ------------------------------------------------------------------
// Calls to the Host
// ------------------------------------------------------------------

function TBasicVSTModule.GetMasterVersion: Integer;
begin
 Result := CallAudioMaster(amVersion);
 if Result = 0 then Result := 1;
end;

function TBasicVSTModule.GetCurrentUniqueId: TChunkName;
begin
 if Assigned(FAudioMaster)
  then Result := TChunkName(Integer(CallAudioMaster(amCurrentId)))
  else Result := #0#0#0#0;
end;

procedure TBasicVSTModule.MasterIdle;
begin
 CallAudioMaster(amIdle);
end;

function TBasicVSTModule.IsInputConnected(const Input: Integer): Boolean;
begin
 Result := (CallAudioMaster(amPinConnected, Input) = 0);
end;

function TBasicVSTModule.IsOutputConnected(const Output: Integer): Boolean;
begin
 Result := (CallAudioMaster(amPinConnected, Output, 1) = 0);
end;

procedure TBasicVSTModule.WantEvents(const Filter: TVstIntPtr);
begin
 CallAudioMaster(amWantMidi, 0, Filter);
end;

function TBasicVSTModule.GetTimeInfo(const Filter: TVstIntPtr): PVstTimeInfo;
begin
 Result := PVstTimeInfo(CallAudioMaster(amGetTime, 0, Filter));
end;

procedure TBasicVSTModule.SetTimeInfo(const Filter: TVstIntPtr; var VstTimeInfo: PVstTimeInfo);
begin
 CallAudioMaster(amSetTime, 0, Filter, @VstTimeInfo);
end;

function TBasicVSTModule.TempoAt(const Pos: TVstIntPtr): TVstIntPtr;
begin
 Result := CallAudioMaster(amTempoAt, 0, Pos);
end;

function TBasicVSTModule.SendVstEventsToHost(var Events: TVstEvents): Boolean;
begin
 Result := CallAudioMaster(amProcessEvents, 0, 0, @Events) = 1;
end;

function TBasicVSTModule.GetNumAutomatableParameters: Integer;
begin
 Result := CallAudioMaster(amGetNumAutomatableParameters);
end;

procedure TBasicVSTModule.SetParameterAutomated(const Index: Integer; const Value: Single);
begin
 CallAudioMaster(amAutomate, Index, 0, nil, Value);
end;

function TBasicVSTModule.GetParameterQuantization: TVstIntPtr;
begin
 Result := CallAudioMaster(amGetParameterQuantization);
end;

function TBasicVSTModule.GetInputLatency: TVstIntPtr;
begin
 Result := CallAudioMaster(amGetInputLatency);
end;

function TBasicVSTModule.GetOutputLatency: TVstIntPtr;
begin
 Result := CallAudioMaster(amGetOutputLatency);
end;

function TBasicVSTModule.GetPreviousPlug(const Input: TVstIntPtr): PVSTEffect;
begin
 Result := PVSTEffect(CallAudioMaster(amGetPreviousPlug, 0, Input));
end;

function TBasicVSTModule.GetNextPlug(const Output: TVstIntPtr): PVSTEffect;
begin
 Result := PVSTEffect(CallAudioMaster(amGetNextPlug, 0, Output));
end;

function TBasicVSTModule.WillProcessReplacing: Integer;
begin
 Result := CallAudioMaster(amWillReplaceOrAccumulate);
end;

function TBasicVSTModule.GetCurrentProcessLevel: Integer;
begin
 Result := CallAudioMaster(amGetCurrentProcessLevel);
end;

function TBasicVSTModule.GetAutomationState: Integer;
begin
 Result := CallAudioMaster(amGetAutomationState);
end;

function TBasicVSTModule.OfflineRead(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption; const ReadSource: Boolean): Boolean;
begin
 Result := CallAudioMaster(amOfflineRead, Integer(ReadSource), TVstIntPtr(Option), @Offline) <> 0;
end;

function TBasicVSTModule.OfflineWrite(var Offline: TVstOfflineTaskRecord; const Option: TVstOfflineOption): Boolean;
begin
 Result := CallAudioMaster(amOfflineWrite, 0, TVstIntPtr(Option), @Offline) <> 0;
end;

function TBasicVSTModule.OfflineStart(var AudioFile: TVstAudioFile;
  const numAudioFiles: Integer; const numNewAudioFiles: TVstIntPtr): Boolean;
begin
 Result := CallAudioMaster(amOfflineStart, numNewAudioFiles, numAudioFiles, @AudioFile) <> 0;
end;

function TBasicVSTModule.OfflineGetCurrentPass: Integer;
begin
 Result := CallAudioMaster(amOfflineGetCurrentPass);
end;

function TBasicVSTModule.OfflineGetCurrentMetaPass: Integer;
begin
 Result := CallAudioMaster(amOfflineGetCurrentMetaPass);
end;

procedure TBasicVSTModule.SetOutputSampleRate(const SampleRate: Single);
begin
 CallAudioMaster(amSetOutputSampleRate, 0, 0, nil, SampleRate);
end;

function TBasicVSTModule.GetHostVendorString(const Text: PAnsiChar): Boolean;
begin
 Result := CallAudioMaster(amGetVendorString, 0, 0, Text) <> 0;
end;

function TBasicVSTModule.GetHostProductString(const Text: PAnsiChar): Boolean;
begin
 {$IFDEF DebugLog}
 AddLogMessage('TBasicVSTModule.GetHostProductString;');
 AddLogMessage('--> Before AudioMaster Call');
 {$ENDIF}

 Result := CallAudioMaster(amGetProductString, 0, 0, Text) <> 0;

 {$IFDEF DebugLog} AddLogMessage('--> After AudioMaster Call'); {$ENDIF}
end;

function TBasicVSTModule.GetHostVendorVersion: Integer;
begin
 Result := CallAudioMaster(amGetVendorVersion);
end;

function TBasicVSTModule.HostVendorSpecific(const Index: Integer;
  const Value: TVstIntPtr; const PtrArg: Pointer;
  const FloatArg: Single): Integer;
begin
 Result := CallAudioMaster(amVendorSpecific, Index, Value, ptrArg, floatArg);
end;

function TBasicVSTModule.GetCanHostDo(Text: AnsiString): Integer;
begin
 Result := CallAudioMaster(amCanDo, 0, 0, PAnsiChar(Text));
end;

function TBasicVSTModule.GetHostLanguage: Integer;
begin
 Result := CallAudioMaster(amGetLanguage);
end;

function TBasicVSTModule.OpenWindow(const aWindow: PVstWindow): Pointer;
begin
 Result := Pointer(CallAudioMaster(amOpenWindow, 0, 0, aWindow));
end;

function TBasicVSTModule.CloseWindow(const aWindow: PVstWindow): Boolean;
begin
 Result := CallAudioMaster(amCloseWindow, 0, 0, aWindow) <> 0;
end;

function TBasicVSTModule.GetDirectory: Pointer;
begin
 Result := Pointer(CallAudioMaster(amGetDirectory));
end;

function TBasicVSTModule.UpdateDisplay: Boolean;
begin
 Result := CallAudioMaster(amUpdateDisplay) <> 0;
end;

function TBasicVSTModule.IOChanged: Boolean;
begin
 Result := CallAudioMaster(amIOChanged) <> 0;
end;

function TBasicVSTModule.NeedIdle: Boolean;
begin
 Result := CallAudioMaster(amNeedIdle) <> 0;
end;

function TBasicVSTModule.SizeWindow(const Width, Height: Integer): Boolean;
begin
 Result := CallAudioMaster(amSizeWindow, Width, Height) <> 0;
end;

function TBasicVSTModule.BeginEdit(const Index: Integer): Boolean;
begin
 Result := CallAudioMaster(amBeginEdit, Index) <> 0;
end;

function TBasicVSTModule.EndEdit(const Index: Integer): Boolean;
begin
 Result := CallAudioMaster(amEndEdit, Index) <> 0;
end;

function TBasicVSTModule.OpenFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
begin
 Result := CallAudioMaster(amOpenFileSelector, 0, 0, @VstFileSelect) <> 0;
end;

function TBasicVSTModule.CloseFileSelector(var VstFileSelect: TVstFileSelect): Boolean;
begin
 Result := CallAudioMaster(amCloseFileSelector, 0, 0, @VstFileSelect) <> 0;
end;

function TBasicVSTModule.GetChunkFile(const NativePath: Pointer): Boolean;
begin
 Result := CallAudioMaster(amGetChunkFile, 0, 0, NativePath) <> 0;
end;

function TBasicVSTModule.UpdateSampleRate: Double;
begin
 Result := CallAudioMaster(amGetSampleRate);
end;

function TBasicVSTModule.UpdateBlockSize: Integer;
begin
 Result := CallAudioMaster(amGetBlockSize);
end;

function TBasicVSTModule.GetInputSpeakerArrangement: PVstSpeakerArrangement;
begin
 Result := PVstSpeakerArrangement(CallAudioMaster(amGetInputSpeakerArrangement));
end;

function TBasicVSTModule.GetOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
 Result := PVstSpeakerArrangement(CallAudioMaster(amGetOutputSpeakerArrangement));
end;

function TBasicVSTModule.WinAmpModifySamples(const Samples: Pointer;
  const SampleFrames, BitPerSample, ChannelCount, SampleRate: Integer): Integer;
var
 i : Integer;
begin
 Result := SampleFrames;

 // TEST if maximum blocksize changed
 if SampleFrames > FWinAmpSampleRate then
  begin
   FWinAmpSampleRate := SampleFrames;
//   SetBlockSize(SampleFrames);

   // reallocate input VST buffers
   for i := 0 to Length(FWinAmpInputBuffer) - 1
    do ReallocMem(FWinAmpInputBuffer[i], SampleFrames * SizeOf(Single));

   // reallocate output VST buffers
   for i := 0 to Length(FWinAmpOutputBuffer) - 1
    do ReallocMem(FWinAmpOutputBuffer[i], SampleFrames * SizeOf(Single));
  end;

 // TEST if samplerate changed
 if SampleRate <> FWinAmpSampleRate then
  begin
   FWinAmpSampleRate := SampleRate;
//   SetSampleRate(SampleRate);
  end;

 case BitPerSample of
   8: begin
       FWinAmpConvertIn  := ConvertInterleaved8bitToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved8bit;
      end;
  16: begin
       FWinAmpConvertIn  := ConvertInterleaved16bitToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved16bit;
      end;
  24: begin
       FWinAmpConvertIn  := ConvertInterleaved24bitToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved24bit;
      end;
  32: begin
       FWinAmpConvertIn  := ConvertInterleavedToFloat;
       FWinAmpConvertOut := ConvertFloatToInterleaved;
      end;
  else
   begin
    FWinAmpConvertIn  := ConvertDummy;
    FWinAmpConvertOut := ConvertDummy;
   end;
 end;

 // convert interleaved to float data
 FWinAmpConvertIn(Samples, ChannelCount, SampleFrames);

 // process VST plugin
 FEffect.Process32Replacing(@FEffect, @FWinAmpInputBuffer[0],
   @FWinAmpOutputBuffer[0], SampleFrames);

 // convert float to interleaved data
 FWinAmpConvertOut(Samples, ChannelCount, SampleFrames);
end;

procedure TBasicVSTModule.WinAmpConfig;
begin
 if not Assigned(FWinAmpEditorForm) then
  begin
   FWinAmpEditorForm := TForm.Create(Self);
  end;
 FWinAmpEditorForm.Show;
end;

procedure TBasicVSTModule.WinAmpQuit;
begin
 FWinAmpBypass := True;
end;

procedure TBasicVSTModule.ConvertDummy(const Data: Pointer; const ChannelCount,
  SampleFrames: Cardinal);
begin
 // do nothing (dummy)
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved8bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  I8           : PShortIntArray absolute Data;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
const
  MulFakDith8 : Single = $7E;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpOutputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do I8^[SampleIndex * ChannelCount + ChannelIndex] := Round(FWinAmpOutputBuffer[ChannelIndex]^[SampleIndex] * MulFakDith8 + random - random);
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved16bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  I16          : PSmallIntArray absolute Data;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
const
  MulFakDith16 : Single = $7FFE;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpOutputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do I16^[SampleIndex * ChannelCount + ChannelIndex] := Round(Limit(FWinAmpOutputBuffer[ChannelIndex]^[SampleIndex]) * MulFakDith16 + Random - Random);
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved24bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  I24           : P3ByteArray absolute Data;
  ChannelIndex  : Cardinal;
  SampleIndex   : Cardinal;
  TempData      : Integer;
  TempDataBytes : array [0..3] of Byte absolute TempData;
const
  MulFakDith24 : Single = 1 / $7FFFFE;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpOutputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    TempData := Round(Limit(FWinAmpOutputBuffer[ChannelIndex]^[SampleIndex]) * MulFakDith24 + random - random);
    Move(TempDataBytes[1], I24^[SampleIndex * ChannelCount + ChannelIndex], 3);
   end;
end;

procedure TBasicVSTModule.ConvertFloatToInterleaved(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  Interleaved  : PDAVSingleFixedArray absolute Data;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[ChannelIndex]^[SampleIndex] := Interleaved^[SampleIndex * ChannelCount + ChannelIndex];
end;

procedure TBasicVSTModule.ConvertInterleaved8bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  I8           : PShortIntArray absolute Data;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
const
  DivFak8 : Single = 1 / $80;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[ChannelIndex]^[SampleIndex] := I8^[SampleIndex * ChannelCount + ChannelIndex] * DivFak8;
end;

procedure TBasicVSTModule.ConvertInterleaved16bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  I16          : PSmallIntArray absolute Data;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
const
  DivFak16 : Single = 1 / $8000;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[ChannelIndex]^[SampleIndex] := I16^[SampleIndex * ChannelCount + ChannelIndex] * DivFak16;
end;

procedure TBasicVSTModule.ConvertInterleaved24bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  I24          : P3ByteArray absolute Data;
  TempData     : Integer;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
const
  DivFak24 : Single = 1 / $800000;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    TempData := (ShortInt(I24^[SampleIndex * ChannelCount + ChannelIndex][2]) shl 16) +
                         (I24^[SampleIndex * ChannelCount + ChannelIndex][1]  shl 8)  +
                         (I24^[SampleIndex * ChannelCount + ChannelIndex][0]);
    FWinAmpInputBuffer[ChannelIndex]^[SampleIndex] := TempData * DivFak24;
   end;
end;

procedure TBasicVSTModule.ConvertInterleavedToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Cardinal);
var
  Interleaved  : PDAVSingleFixedArray absolute Data;
  ChannelIndex : Cardinal;
  SampleIndex  : Cardinal;
begin
 for ChannelIndex := 0 to Min(Length(FWinAmpInputBuffer), ChannelCount) - 1 do
  for SampleIndex := 0 to SampleFrames - 1
   do FWinAmpInputBuffer[ChannelIndex]^[SampleIndex] :=
     Interleaved^[SampleIndex * ChannelCount + ChannelIndex];
end;



// ------------------------------------------------------------------
// Calls from the host
// ------------------------------------------------------------------

function TBasicVSTModule.HostCallOpen(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin
 Result := 0;
 {$IFNDEF UseAudioEffectPtr}
 if not FUseAudioEffectPtr then
  begin
   Effect^.GetParameter       := @GetParameterFuncUserPtr;
   Effect^.SetParameter       := @SetParameterFuncUserPtr;
   Effect^.Process            := @ProcessFuncUserPtr;
   Effect^.Process32Replacing := @Process32ReplacingFuncUserPtr;
   Effect^.Process64Replacing := @Process64ReplacingFuncUserPtr;
  end
 else
 {$ELSE}
  begin
   Effect^.GetParameter       := @GetParameterFuncAudioEffectPtr;
   Effect^.SetParameter       := @SetParameterFuncAudioEffectPtr;
   Effect^.Process            := @ProcessFuncAudioEffectPtr;
   Effect^.Process32Replacing := @Process32ReplacingFuncAudioEffectPtr;
   Effect^.Process64Replacing := @Process64ReplacingFuncAudioEffectPtr;
  end;
 {$ENDIF}
end;

function TBasicVSTModule.HostCallClose(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin
 try
  Effect^.GetParameter := @GetParameterFuncDummy;
  Effect^.SetParameter := @SetParameterFuncDummy;
  Effect^.Process := @ProcessFuncDummy;
  Effect^.Process32Replacing := @ProcessFuncDummy;
  Effect^.Process64Replacing := @ProcessFuncDummy;
  {$IFDEF UseAudioEffectPtr}
  Effect^.AudioEffectPtr := nil;
  {$ELSE}
  if FUseAudioEffectPtr
   then Effect^.AudioEffectPtr := nil
   else Effect^.User := nil;
  {$ENDIF}

  {$IFNDEF FPC}
  Free;
  {$ENDIF}

  Result := 1;
 except
  Result := 0;
 end;
end;

function TBasicVSTModule.HostCallSetProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProgramName(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramName(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamLabel(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamDisplay(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParamName(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVu(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSampleRate(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSize(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallMainsChanged(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditGetRect(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditOpen(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditClose(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditDraw(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditMouse(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKey(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditIdle(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditTop(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditSleep(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdentify(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
var
  ChunkName : TChunkName;
begin
 ChunkName := 'fEvN';
 Result := Integer(ChunkName);
end;

function TBasicVSTModule.HostCallGetChunk(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetChunk(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessEvents(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanBeAutomated(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallString2Parameter(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumProgramCategories(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProgramNameIndexed(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallCopyProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode; const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin
 case OpCode of
  effOpen:                      Result := HostCallOpen(Index, Value, PTR, opt);
  effClose:                     Result := HostCallClose(Index, Value, PTR, opt);
  effSetProgram:                Result := HostCallSetProgram(Index, Value, PTR, opt);
  effGetProgram:                Result := HostCallGetProgram(Index, Value, PTR, opt);
  effSetProgramName:            Result := HostCallSetProgramName(Index, Value, PTR, opt);
  effGetProgramName:            Result := HostCallGetProgramName(Index, Value, PTR, opt);
  effGetParamLabel:             Result := HostCallGetParamLabel(Index, Value, PTR, opt);
  effGetParamDisplay:           Result := HostCallGetParamDisplay(Index, Value, PTR, opt);
  effGetParamName:              Result := HostCallGetParamName(Index, Value, PTR, opt);
  effGetVu:                     Result := HostCallGetVu(Index, Value, PTR, opt);
  effSetSampleRate:             Result := HostCallSetSampleRate(Index, Value, PTR, opt);
  effSetBlockSize:              Result := HostCallSetBlockSize(Index, Value, PTR, opt);
  effMainsChanged:              Result := HostCallMainsChanged(Index, Value, PTR, opt);
  effEditGetRect:               Result := HostCallEditGetRect(Index, Value, PTR, opt);
  effEditOpen:                  Result := HostCallEditOpen(Index, Value, PTR, opt);
  effEditClose:                 Result := HostCallEditClose(Index, Value, PTR, opt);
  effEditDraw:                  Result := HostCallEditDraw(Index, Value, PTR, opt);
  effEditMouse:                 Result := HostCallEditMouse(Index, Value, PTR, opt);
  effEditKey:                   Result := HostCallEditKey(Index, Value, PTR, opt);
  effEditIdle:                  Result := HostCallEditIdle(Index, Value, PTR, opt);
  effEditTop:                   Result := HostCallEditTop(Index, Value, PTR, opt);
  effEditSleep:                 Result := HostCallEditSleep(Index, Value, PTR, opt);
  effIdentify:                  Result := HostCallIdentify(Index, Value, PTR, opt);
  effGetChunk:                  Result := HostCallGetChunk(Index, Value, PTR, opt);
  effSetChunk:                  Result := HostCallSetChunk(Index, Value, PTR, opt);
  effProcessEvents:             Result := HostCallProcessEvents(Index, Value, PTR, opt);
  effCanBeAutomated:            Result := HostCallCanBeAutomated(Index, Value, PTR, opt);
  effString2Parameter:          Result := HostCallString2Parameter(Index, Value, PTR, opt);
  effGetNumProgramCategories:   Result := HostCallGetNumProgramCategories(Index, Value, PTR, opt);
  effGetProgramNameIndexed:     Result := HostCallGetProgramNameIndexed(Index, Value, PTR, opt);
  effCopyProgram:               Result := HostCallCopyProgram(Index, Value, PTR, opt);
  effConnectInput:              Result := HostCallConnectInput(Index, Value, PTR, opt);
  effConnectOutput:             Result := HostCallConnectOutput(Index, Value, PTR, opt);
  effGetInputProperties:        Result := HostCallGetInputProperties(Index, Value, PTR, opt);
  effGetOutputProperties:       Result := HostCallGetOutputProperties(Index, Value, PTR, opt);
  effGetPlugCategory:           Result := HostCallGetPlugCategory(Index, Value, PTR, opt);
  effGetCurrentPosition:        Result := HostCallGetCurrentPosition(Index, Value, PTR, opt);
  effGetDestinationBuffer:      Result := HostCallGetDestinationBuffer(Index, Value, PTR, opt);
  effOfflineNotify:             Result := HostCallOfflineNotify(Index, Value, PTR, opt);
  effOfflinePrepare:            Result := HostCallOfflinePrepare(Index, Value, PTR, opt);
  effOfflineRun:                Result := HostCallOfflineRun(Index, Value, PTR, opt);
  effProcessVarIo:              Result := HostCallProcessVarIo(Index, Value, PTR, opt);
  effSetSpeakerArrangement:     Result := HostCallSetSpeakerArrangement(Index, Value, PTR, opt);
  effSetBlockSizeAndSampleRate: Result := HostCallSetBlockSizeAndSampleRate(Index, Value, PTR, opt);
  effSetBypass:                 Result := HostCallSetBypass(Index, Value, PTR, opt);
  effGetEffectName:             Result := HostCallGetEffectName(Index, Value, PTR, opt);
  effGetErrorText:              Result := HostCallGetErrorText(Index, Value, PTR, opt);
  effGetVendorString:           Result := HostCallGetVendorString(Index, Value, PTR, opt);
  effGetProductString:          Result := HostCallGetProductString(Index, Value, PTR, opt);
  effGetVendorVersion:          Result := HostCallGetVendorVersion(Index, Value, PTR, opt);
  effVendorSpecific:            Result := HostCallVendorSpecific(Index, Value, PTR, opt);
  effCanDo:                     Result := HostCallCanDo(Index, Value, PTR, opt);
  effGetTailSize:               Result := HostCallGetTailSize(Index, Value, PTR, opt);
  effIdle:                      Result := HostCallIdle(Index, Value, PTR, opt);
  effGetIcon:                   Result := HostCallGetIcon(Index, Value, PTR, opt);
  effSetViewPosition:           Result := HostCallSetViewPosition(Index, Value, PTR, opt);
  effGetParameterProperties:    Result := HostCallGetParameterProperties(Index, Value, PTR, opt);
  effKeysRequired:              Result := HostCallKeysRequired(Index, Value, PTR, opt);
  effGetVstVersion:             Result := HostCallGetVstVersion(Index, Value, PTR, opt);
  effEditKeyDown:               Result := HostCallEditKeyDown(Index, Value, PTR, opt);
  effEditKeyUp:                 Result := HostCallEditKeyUp(Index, Value, PTR, opt);
  effSetEditKnobMode:           Result := HostCallSetEditKnobMode(Index, Value, PTR, opt);
  effGetMidiProgramName:        Result := HostCallGetMidiProgramName(Index, Value, PTR, opt);
  effGetCurrentMidiProgram:     Result := HostCallGetCurrentMidiProgram(Index, Value, PTR, opt);
  effGetMidiProgramCategory:    Result := HostCallGetMidiProgramCategory(Index, Value, PTR, opt);
  effHasMidiProgramsChanged:    Result := HostCallHasMidiProgramsChanged(Index, Value, PTR, opt);
  effGetMidiKeyName:            Result := HostCallGetMidiKeyName(Index, Value, PTR, opt);
  effBeginSetProgram:           Result := HostCallBeginSetProgram(Index, Value, PTR, opt);
  effEndSetProgram:             Result := HostCallEndSetProgram(Index, Value, PTR, opt);
  effGetSpeakerArrangement:     Result := HostCallGetSpeakerArrangement(Index, Value, PTR, opt);
  effShellGetNextPlugin:        Result := HostCallShellGetNextPlugin(Index, Value, PTR, opt);
  effStartProcess:              Result := HostCallStartProcess(Index, Value, PTR, opt);
  effStopProcess:               Result := HostCallStopProcess(Index, Value, PTR, opt);
  effSetTotalSampleToProcess:   Result := HostCallSetTotalSampleToProcess(Index, Value, PTR, opt);
  effSetPanLaw:                 Result := HostCallSetPanLaw(Index, Value, PTR, opt);
  effBeginLoadBank:             Result := HostCallBeginLoadBank(Index, Value, PTR, opt);
  effBeginLoadProgram:          Result := HostCallBeginLoadProgram(Index, Value, PTR, opt);
  effSetProcessPrecision:       Result := HostCallSetProcessPrecision(Index, Value, PTR, opt);
  effGetNumMidiInputChannels:   Result := HostCallGetNumMidiInputChannels(Index, Value, PTR, opt);
  effGetNumMidiOutputChannels:  Result := HostCallGetNumMidiOutputChannels(Index, Value, PTR, opt);
  else
   try
     raise EVstError.Create('Unknown OpCode');
   except
     Result := 0;
   end;
  end;
end;

function TBasicVSTModule.HostCallConnectInput(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallConnectOutput(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetInputProperties(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetOutputProperties(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetPlugCategory(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentPosition(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetDestinationBuffer(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineNotify(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflinePrepare(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallOfflineRun(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallProcessVarIo(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetSpeakerArrangement(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBlockSizeAndSampleRate(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetBypass(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetEffectName(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetErrorText(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorString(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetProductString(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVendorVersion(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallVendorSpecific(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallCanDo(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetTailSize(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallIdle(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetIcon(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetViewPosition(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetParameterProperties(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallKeysRequired(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetVstVersion(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyDown(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEditKeyUp(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetEditKnobMode(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramName(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetCurrentMidiProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiProgramCategory(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallHasMidiProgramsChanged(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetMidiKeyName(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginSetProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallEndSetProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetSpeakerArrangement(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallShellGetNextPlugin(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallStartProcess(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallStopProcess(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetTotalSampleToProcess(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetPanLaw(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadBank(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallBeginLoadProgram(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallSetProcessPrecision(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiInputChannels(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;

function TBasicVSTModule.HostCallGetNumMidiOutputChannels(const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr;
begin Result := 0; end;



// effect functions

function DispatchEffectFuncAudioEffectPtr(Effect: PVSTEffect; OpCode: TDispatcherOpCode; const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; cdecl;
begin
 if Assigned(Effect) and (TObject(Effect^.AudioEffectPtr) is TBasicVSTModule)
   then Result := TBasicVSTModule(Effect^.AudioEffectPtr).HostCallDispatchEffect(OpCode, Index, Value, PTR, opt)
 else Result := 0;
end;

function GetParameterFuncAudioEffectPtr(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then Result := TBasicVSTModule(Effect^.AudioEffectPtr).HostCallGetParameter(Index)
  else Result := 0;
end;

procedure SetParameterFuncAudioEffectPtr(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallSetParameter(Index, Value);
end;

{$IFNDEF UseAudioEffectPtr}
function DispatchEffectFuncUserPtr(Effect: PVSTEffect; OpCode: TDispatcherOpCode; const Index: Integer; const Value: TVstIntPtr; const PTR: Pointer; const opt: Single): TVstIntPtr; cdecl;
begin
 if Assigned(Effect) and // (Effect^.Magic <> #0#0#0#0) and
  (TObject(Effect^.User) is TBasicVSTModule)
   then Result := TBasicVSTModule(Effect^.User).HostCallDispatchEffect(OpCode, Index, Value, PTR, opt)
 else Result := 0;
end;

function GetParameterFuncUserPtr(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.User) is TBasicVSTModule
  then Result := TBasicVSTModule(Effect^.User).HostCallGetParameter(Index)
  else Result := 0;
end;

procedure SetParameterFuncUserPtr(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
 Assert(Assigned(Effect));
 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallSetParameter(Index, Value);
end;
{$ENDIF}

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

procedure ProcessFuncCheck(const Effect: PVSTEffect; const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
begin
 if Assigned(Effect) then
  {$IFDEF UseAudioEffectPtr}
  if Addr(Effect^.Process) = Addr(ProcessFuncAudioEffectPtr)
   then ProcessFuncAudioEffectPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ELSE}
  if Addr(Effect^.Process) = Addr(ProcessFuncUserPtr)
   then ProcessFuncUserPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ENDIF}
end;

procedure Process32ReplacingFuncCheck(const Effect: PVSTEffect; const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
begin
 if Assigned(Effect) then
  {$IFDEF UseAudioEffectPtr}
  if Addr(Effect^.Process32Replacing) = Addr(Process32ReplacingFuncAudioEffectPtr)
   then Process32ReplacingFuncAudioEffectPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ELSE}
  if Addr(Effect^.Process32Replacing) = Addr(Process32ReplacingFuncUserPtr)
   then Process32ReplacingFuncUserPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ENDIF}
end;

procedure Process64ReplacingFuncCheck(const Effect: PVSTEffect; const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer); cdecl;
begin
 if Assigned(Effect) then
  {$IFDEF UseAudioEffectPtr}
  if Addr(Effect^.Process64Replacing) = Addr(Process64ReplacingFuncAudioEffectPtr)
   then Process64ReplacingFuncAudioEffectPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ELSE}
  if Addr(Effect^.Process64Replacing) = Addr(Process64ReplacingFuncUserPtr)
   then Process64ReplacingFuncUserPtr(Effect, Inputs, Outputs, SampleFrames);
  {$ENDIF}
end;

{$IFDEF CPUx86_64}
{$DEFINE PUREPASCAL}
{$ENDIF}

procedure ProcessFuncAudioEffectPtr(const Effect: PVSTEffect; const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames <= 0) or ((Inputs = nil) and (Outputs = nil))
  then Exit;

 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallProcess(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
    PUSH    EBX

    // TEST SampleFrames <> 0
    MOV     EAX, SampleFrames
    TEST    EAX, EAX
    JZ      @End

    // TEST Effect <> 0
    MOV     EBX, Effect
    TEST    EBX, EBX
    JZ      @End

    // TEST Effect^.AudioEffectPtr <> 0
    MOV     EBX, [EBX + $40]
    TEST    EBX, EBX
    JZ      @End

    // TEST Outputs <> 0 (if so, processing can take place)
    MOV     ECX, [Outputs]
    TEST    ECX, ECX
    JZ      @Process

    // TEST Inputs <> 0 (only if outputs is also nil)
    MOV     EDX, [Inputs]
    TEST    EDX, EDX
    JZ      @End

@Process:
    // PUSH SampleFrames on stack
    PUSH    EAX
    MOV     EAX, EBX
    MOV     EBX, [EAX]

    // CALL HostCallProcess
    CALL    DWORD PTR [EBX + CHostCallProcessOffset] // [TBasicVSTModule(EBX).HostCallProcess]

@End:
    POP EBX
end;
{$ENDIF}

procedure Process32ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect; const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames <= 0) or ((Inputs = nil) and (Outputs = nil))
  then Exit;

 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallProcess32Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
    PUSH    EBX

    // TEST SampleFrames <> 0
    MOV     EAX, SampleFrames
    TEST    EAX, EAX
    JZ      @End

    // TEST Effect <> 0
    MOV     EBX, Effect
    TEST    EBX, EBX
    JZ      @End

    // TEST Effect^.AudioEffectPtr <> 0
    MOV     EBX, [EBX + $40]
    TEST    EBX, EBX
    JZ      @End

    // TEST Outputs <> 0 (if so, processing can take place)
    MOV     ECX, [Outputs]
    TEST    ECX, ECX
    JZ      @Process

    // TEST Inputs <> 0 (only if outputs is also nil)
    MOV     EDX, [Inputs]
    TEST    EDX, EDX
    JZ      @End

@Process:
    // PUSH SampleFrames on stack
    PUSH    EAX
    MOV     EAX, EBX
    MOV     EBX, [EAX]

    // CALL HostCallProcess32Replacing (damn hack!!!)
    CALL    DWORD PTR [EBX + CHostCallProcess32ReplacingOffset] // [TBasicVSTModule(EBX).HostCallProcess32Replacing]

@End:
    POP EBX
end;
{$ENDIF}

procedure Process64ReplacingFuncAudioEffectPtr(const Effect: PVSTEffect;
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames <= 0) or ((Inputs = nil) and (Outputs = nil))
  then Exit;

 if TObject(Effect^.AudioEffectPtr) is TBasicVSTModule
  then TBasicVSTModule(Effect^.AudioEffectPtr).HostCallProcess64Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
    PUSH    EBX

    // TEST SampleFrames <> 0
    MOV     EAX, SampleFrames
    TEST    EAX, EAX
    JZ      @End

    // TEST Effect <> 0
    MOV     EBX, Effect
    TEST    EBX, EBX
    JZ      @End

    // TEST Effect^.AudioEffectPtr <> 0
    MOV     EBX, [EBX + $40]
    TEST    EBX, EBX
    JZ      @End

    // TEST Outputs <> 0 (if so, processing can take place)
    MOV     ECX, [Outputs]
    TEST    ECX, ECX
    JZ      @Process

    // TEST Inputs <> 0 (only if outputs is also nil)
    MOV     EDX, [Inputs]
    TEST    EDX, EDX
    JZ      @End

@Process:
    // PUSH SampleFrames on stack
    PUSH    EAX
    MOV     EAX, EBX
    MOV     EBX, [EAX]

    // CALL HostCallProcess64Replacing (damn hack!!!)
    CALL    DWORD PTR [EBX + CHostCallProcess64ReplacingOffset] // [TBasicVSTModule(EBX).HostCallProcess64Replacing]

@End:
    POP     EBX
end;
{$ENDIF}

{$IFNDEF UseAudioEffectPtr}
procedure ProcessFuncUserPtr(const Effect: PVSTEffect; const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames <= 0) or ((Inputs = nil) and (Outputs = nil))
  then Exit;

 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallProcess(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
    PUSH    EBX

    // TEST SampleFrames <> 0
    MOV     EAX, SampleFrames
    TEST    EAX, EAX
    JZ      @End

    // TEST Effect <> 0
    MOV     EBX, Effect
    TEST    EBX, EBX
    JZ      @End

    // TEST Effect^.User <> 0
    MOV     EBX, [EBX + $44]
    TEST    EBX, EBX
    JZ      @End

    // TEST Outputs <> 0 (if so, processing can take place)
    MOV     ECX, [Outputs]
    TEST    ECX, ECX
    JZ      @Process

    // TEST Inputs <> 0 (only if outputs is also nil)
    MOV     EDX, [Inputs]
    TEST    EDX, EDX
    JZ      @End

@Process:
    // PUSH SampleFrames on stack
    PUSH    EAX
    MOV     EAX, EBX
    MOV     EBX, [EAX]

    // CALL HostCallProcess
    CALL    DWORD PTR [EBX + CHostCallProcessOffset] // [TBasicVSTModule(EBX).HostCallProcess]

@End:
    POP EBX
end;
{$ENDIF}

procedure Process32ReplacingFuncUserPtr(const Effect: PVSTEffect; const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames <= 0) or ((Inputs = nil) and (Outputs = nil))
  then Exit;

 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallProcess32Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
    PUSH    EBX

    // TEST SampleFrames <> 0
    MOV     EAX, SampleFrames
    TEST    EAX, EAX
    JZ      @End

    // TEST Effect <> 0
    MOV     EBX, Effect
    TEST    EBX, EBX
    JZ      @End

    // TEST Effect^.User <> 0
    MOV     EBX, [EBX + $44]
    TEST    EBX, EBX
    JZ      @End

    // TEST Outputs <> 0 (if so, processing can take place)
    MOV     ECX, [Outputs]
    TEST    ECX, ECX
    JZ      @Process

    // TEST Inputs <> 0 (only if outputs is also nil)
    MOV     EDX, [Inputs]
    TEST    EDX, EDX
    JZ      @End

@Process:
    // PUSH SampleFrames on stack
    PUSH    EAX
    MOV     EAX, EBX
    MOV     EBX, [EAX]

    CALL    DWORD PTR [EBX + CHostCallProcess32ReplacingOffset] // [TBasicVSTModule(EBX).HostCallProcess32Replacing]

@End:
    POP EBX
end;
{$ENDIF}

procedure Process64ReplacingFuncUserPtr(const Effect: PVSTEffect; const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Integer); cdecl;
{$IFDEF PUREPASCAL}
begin
 // check consistency
 if not Assigned(Effect) or (SampleFrames <= 0) or ((Inputs = nil) and (Outputs = nil))
  then Exit;

 if TObject(Effect^.User) is TBasicVSTModule
  then TBasicVSTModule(Effect^.User).HostCallProcess64Replacing(Inputs, Outputs, SampleFrames);
end;
{$ELSE}
asm
    PUSH    EBX

    // TEST SampleFrames <> 0
    MOV     EAX, SampleFrames
    TEST    EAX, EAX
    JZ      @End

    // TEST Effect <> 0
    MOV     EBX, Effect
    TEST    EBX, EBX
    JZ      @End

    // TEST Effect^.User <> 0
    MOV     EBX, [EBX + $44]
    TEST    EBX, EBX
    JZ      @End

    // TEST Outputs <> 0 (if so, processing can take place)
    MOV     ECX, [Outputs]
    TEST    ECX, ECX
    JZ      @Process

    // TEST Inputs <> 0 (only if outputs is also nil)
    MOV     EDX, [Inputs]
    TEST    EDX, EDX
    JZ      @End

@Process:
    // PUSH SampleFrames on stack
    PUSH    EAX
    MOV     EAX, EBX
    MOV     EBX, [EAX]

    // CALL HostCallProcess64Replacing (damn hack!!!)
    CALL    DWORD PTR [EBX + CHostCallProcess64ReplacingOffset] // [TBasicVSTModule(EBX).HostCallProcess64Replacing]

@End:
    POP EBX
end;
{$ENDIF}
{$ENDIF}

// Dummy Functions

function GetParameterFuncDummy(const Effect: PVSTEffect; const Index: Integer): Single; cdecl;
begin
 Result := 0;
end;

procedure SetParameterFuncDummy(const Effect: PVSTEffect; const Index: Integer; const Value: Single); cdecl;
begin
end;

procedure ProcessFuncDummy(const Effect: PVSTEffect; const Inputs, Outputs: Pointer; const SampleFrames: Cardinal); cdecl;
begin
end;


// WinAmp Stuff

function Init(const WinAmpDSPModule: PWinAmpDSPModule): Integer; cdecl;
begin
 // make sure a pointer to the TWinAmpDSPModule exists
 if not Assigned(WinAmpDSPModule) then
  begin
   Result := 1;
   Exit;
  end;

 // assert that no other instance exists already
 Assert(WinAmpDSPModule^.UserData = nil);

 try
  // instanciate TWinAmpObject
  WinAmpDSPModule^.UserData := WAVstModule.Create(Application);
  TBasicVSTModule(WinAmpDSPModule^.UserData).FWinAmpDSPModule := WinAmpDSPModule;
  WinAmpDSPModule^.ModifySamples := ModifySamples;
  Result := 0;
 except
  Result := 1;
 end;
end;

procedure Config(const WinAmpDSPModule: PWinAmpDSPModule); cdecl;
begin
 // assert that a pointer to the TWinAmpDSPModule exists
 Assert(Assigned(WinAmpDSPModule));

 // open config dialog
 if Assigned(WinAmpDSPModule^.UserData) then
  try
   TBasicVSTModule(WinAmpDSPModule^.UserData).WinAmpConfig;
  except
  end;
end;

function ModifySamples(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
begin
 Result := SampleFrames;

 // make sure a pointer to the TWinAmpDSPModule exists
 if not Assigned(WinAmpDSPModule) then Exit;

 // make sure a TWinAmpObject instance exists
 if not Assigned(WinAmpDSPModule^.UserData) then Exit;

 // CALL the objects 'ModifySamples'
 Result := TBasicVSTModule(WinAmpDSPModule^.UserData).WinAmpModifySamples(Samples,
   SampleFrames, BitPerSample, ChannelCount, SampleRate);
end;

function ModifySamplesDummy(const WinAmpDSPModule: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
begin
 Result := SampleFrames;
end;

procedure Quit(const WinAmpDSPModule: PWinAmpDSPModule);
begin
 // assert that a pointer to the TWinAmpDSPModule exists
 Assert(Assigned(WinAmpDSPModule));
 try
  WinAmpDSPModule^.ModifySamples := ModifySamplesDummy;
  sleep(5);
  TBasicVSTModule(WinAmpDSPModule^.UserData).WinAmpQuit;
 finally
  FreeAndNil(WinAmpDSPModule^.UserData);
 end;
end;

initialization
  GVstInstanceList := TObjectList.Create(False);

finalization
  FreeAndNil(GVstInstanceList);

end.
