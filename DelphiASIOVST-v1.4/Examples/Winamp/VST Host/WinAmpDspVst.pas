unit WinAmpDspVst;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Classes, Forms, SysUtils,
  {$IFDEF UseCriticalSection}SyncObjs, {$ENDIF} DAV_Types, DAV_VstHost,
  DAV_DspPolyphaseUpsampler, DAV_DspPolyphaseDownsampler, WinAmpDspVstGui;

type
  TSmallIntArray = array [0..40000] of Smallint;
  PSmallIntArray = ^TSmallIntArray;
  TShortIntArray = array [0..40000] of ShortInt;
  PShortIntArray = ^TShortIntArray;
  T3Bytes = array [0..2] of Byte;
  P3Bytes = ^T3Bytes;
  T3ByteArray = array [0..40000] of T3Bytes;
  P3ByteArray = ^T3ByteArray;
  PWinampDSPModule = ^TWinampDSPModule;
  PWinAmpDSPHeader = ^TWinAmpDSPheader;
  TWinAmpObject = class;

  TWAGetHeader = function: PWinAmpDSPHeader; cdecl;
  TWAGetModule = function(const Which: Integer): PWinAmpDSPModule; cdecl;
  TWAConfig = procedure(const This_Mod: PWinAmpDSPModule); cdecl;
  TWAInit = function(const This_Mod: PWinAmpDSPModule) : Integer; cdecl;
  TWAQuit = procedure(const This_Mod: PWinAmpDSPModule); cdecl;
  TWAModifySamples = function(const This_Mod: PWinAmpDSPModule;
    const Samples: Pointer; const SamplesFrame, BitPerSample, ChannelCount,
    SampleRate: Integer): Integer; cdecl;

  TWinampDSPModule = record
                      Description   : PAnsiChar;
                      HwndParent    : Hwnd;
                      HDLLinstance  : Hinst;
                      Config        : TWAConfig;
                      Init          : TWAInit;
                      ModifySamples : TWAModifySamples;
                      Quit          : TWAQuit;
                      UserData      : TWinAmpObject;
                     end;

  TWinAmpDSPHeader = record
                      Version      : Integer;
                      Description  : PAnsiChar;
                      GetModule    : TWAGetModule;
                      Key          : Integer;
                     end;

  TWinAmpConvert = procedure (const Data: Pointer; const ChannelCount, SampleFrames: Integer) of object;

  TWinAmpObject = class(TDataModule)
  private
    FBypass        : Boolean;
    FUpsampler     : array of TPolyphaseUpsampler32;
    FDownsampler   : array of TPolyphaseDownsampler32;

    FInputBuffer   : array of PDAVSingleFixedArray;
    FOutputBuffer  : array of PDAVSingleFixedArray;

    FNrChannels    : Integer;
    FEnhanceFak    : Integer;
    FSampleRate    : Integer;
    FSampleFrames  : Integer;
    FEditorForm    : TFmWinAmpVST;
    FRegistryKey   : string;
    FFxpName       : TFileName;
{
    FRealDelay     : Integer;
    FPDCBuffer     : TArrayOfSingleArray;
}
    procedure AudioMasterUpdateDisplay(Sender: TObject);
    function GetWinampDSPModule: TWinampDSPModule;
    function GetEnhanced: Boolean;
    procedure SetEnhanced(const Value: Boolean);
    procedure ConvertDummy(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved8bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved16bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved24bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved32bit(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved8bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved16bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved24bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved32bitToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    {$IFDEF UseFloatConvert}
    procedure ConvertFloatToInterleaved(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleavedToFloat(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleavedOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleavedToFloatOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    {$ENDIF}
    procedure ConvertFloatToInterleaved8bitOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved16bitOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved24bitOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertFloatToInterleaved32bitOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved8bitToFloatOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved16bitToFloatOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved24bitToFloatOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure ConvertInterleaved32bitToFloatOversampled(const Data: Pointer; const ChannelCount, SampleFrames: Integer);
    procedure UpdateVSTPlugin;
  protected
    {$IFDEF UseCriticalSection}
    FCriticalSection  : TCriticalSection;
    {$ENDIF}

    FVstHost          : TVstHost;
    FWinAmpDspModule  : PWinampDSPModule;
    FWinAmpConvertIn  : TWinAmpConvert;
    FWinAmpConvertOut : TWinAmpConvert;
  public
    constructor CreateNew(AOwner: TComponent; const AWinAmpDspModule: PWinampDSPModule); reintroduce; virtual;
    destructor Destroy; override;
    procedure Config;
    procedure Quit;
    procedure LoadVSTDLL(const VSTDLL: TFileName);
    function ModifySamples(const Samples: Pointer; const SampleFrames,
      BitPerSample, ChannelCount, SampleRate: Integer): Integer;
  published
    property WinAmpDspModule: TWinampDSPModule read GetWinampDSPModule;
    property RegistryKey: string read FRegistryKey;
    property FxpName: TFileName read FFxpName;
    property VstHost: TVstHost read FVstHost;
    property Bypass: Boolean read FBypass write FBypass;
    property Enhanced: Boolean read GetEnhanced write SetEnhanced;
  end;

function winampDSPGetHeader2: PWinAmpDSPHeader; cdecl; export;
function GetModule(const Which: Integer) : PWinAmpDSPModule; cdecl;
procedure Config(const This_Mod: PWinAmpDSPModule); cdecl;
function Init(const This_Mod: PWinAmpDSPModule) : Integer; cdecl;
function ModifySamples(const This_Mod: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
function ModifySamplesDummy(const This_Mod: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
procedure Quit(const This_Mod: PWinAmpDSPModule); cdecl;

implementation

uses
  Dialogs, Registry, Math, DAV_VSTEffect, DAV_Common;

var
  WADSPHeader      : TWinAmpDSPheader =
                     (Version : $20;
                      Description : 'VST Host DSP v1.1.5 for WinAmp'#0;
                      GetModule : GetModule;
                      Key : $21);

  WADSPModule      : TWinAmpDSPModule =
                      (Description : 'VST Host DSP v1.1.5 for WinAmp'#0;
                       HwndParent : 0;
                       hDLLinstance : 0;
                       Config : Config;
                       Init : Init;
                       ModifySamples : ModifySamplesDummy;
                       Quit : Quit;
                       UserData : nil);

exports winampDSPGetHeader2;

function winampDSPGetHeader2 : PWinAmpDSPHeader; cdecl;
begin
 Result := @WADSPHeader;
end;

function GetModule(const Which: Integer): PWinAmpDSPModule;
begin
 case Which of
   0 : Result := @WADSPModule;
 else
  Result := nil;
 end;
end;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

procedure ScanResources;
var
  ContainedVSTPlugins : TStringList;
begin
 ContainedVSTPlugins := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, LongWord(ContainedVSTPlugins));
  if (ContainedVSTPlugins.Count > 0) and (ContainedVSTPlugins[0] <> 'DLL') then
   begin
    WADSPHeader.Description := PAnsiChar(AnsiString(ContainedVSTPlugins[0]));
    WADSPModule.Description := PAnsiChar(AnsiString(ContainedVSTPlugins[0]));
   end;
 finally
  FreeAndNil(ContainedVSTPlugins);
 end;
end;

// stuff Delphi might not know...
const
  FILE_READ_DATA            = $0001; // file & pipe
  FILE_LIST_DIRECTORY       = $0001; // directory
  FILE_WRITE_DATA           = $0002; // file & pipe
  FILE_ADD_FILE             = $0002; // directory
  FILE_APPEND_DATA          = $0004; // file
  FILE_ADD_SUBDIRECTORY     = $0004; // directory
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe
  FILE_READ_EA              = $0008; // file & directory
  FILE_WRITE_EA             = $0010; // file & directory
  FILE_EXECUTE              = $0020; // file
  FILE_TRAVERSE             = $0020; // directory
  FILE_DELETE_CHILD         = $0040; // directory
  FILE_READ_ATTRIBUTES      = $0080; // all
  FILE_WRITE_ATTRIBUTES     = $0100; // all
  FILE_ALL_ACCESS           = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  FILE_GENERIC_READ         = STANDARD_RIGHTS_READ or FILE_READ_DATA or
                              FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
  FILE_GENERIC_WRITE        = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or
                              FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or
                              FILE_APPEND_DATA or SYNCHRONIZE;
  FILE_GENERIC_EXECUTE      = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or
                              FILE_EXECUTE or SYNCHRONIZE;

function CheckAccessToFile(DesiredAccess: DWORD; const FileName: WideString): Boolean;
const
  GenericFileMapping     : TGenericMapping = (
    GenericRead: FILE_GENERIC_READ;
    GenericWrite: FILE_GENERIC_WRITE;
    GenericExecute: FILE_GENERIC_EXECUTE;
    GenericAll: FILE_ALL_ACCESS
    );
var
  LastError              : DWORD;
  LengthNeeded           : DWORD;
  SecurityDescriptor     : PSecurityDescriptor;
  ClientToken            : THandle;
  AccessMask             : DWORD;
  PrivilegeSet           : TPrivilegeSet;
  PrivilegeSetLength     : DWORD;
  GrantedAccess          : DWORD;
  AccessStatus           : BOOL;
begin
  Result := False;
  LastError := GetLastError;
  LengthNeeded := 0;
  if not GetFileSecurityW(PWideChar(FileName), OWNER_SECURITY_INFORMATION or
    GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION, nil, 0,
    LengthNeeded) and (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then Exit;
  SetLastError(LastError);
  Inc(LengthNeeded, $1000);
  SecurityDescriptor := PSecurityDescriptor(LocalAlloc(LPTR, LengthNeeded));
  if not Assigned(SecurityDescriptor) then
    Exit;
  try
    if not GetFileSecurityW(PWideChar(FileName), OWNER_SECURITY_INFORMATION or
      GROUP_SECURITY_INFORMATION or DACL_SECURITY_INFORMATION,
      SecurityDescriptor, LengthNeeded, LengthNeeded) then
      Exit;
    if not ImpersonateSelf(SecurityImpersonation) then 
      Exit; 
    try 
      if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY or 
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE, False, ClientToken) then 
        Exit;
      try 
        AccessMask := DesiredAccess; 
        MapGenericMask(AccessMask, GenericFileMapping); 
        PrivilegeSetLength := SizeOf(TPrivilegeSet); 
        if AccessCheck(SecurityDescriptor, ClientToken, AccessMask, 
          GenericFileMapping, PrivilegeSet, PrivilegeSetLength, GrantedAccess, 
          AccessStatus) then
          Result := AccessStatus; 
      finally 
        CloseHandle(ClientToken); 
      end; 
    finally 
      RevertToSelf; 
    end; 
  finally
    LocalFree(HLOCAL(SecurityDescriptor));
  end;
end;

function Init(const This_Mod: PWinAmpDSPModule): Integer;
begin
 // make sure a pointer to the TWinAmpDSPModule exists
 if not Assigned(This_Mod) or Assigned(This_Mod^.UserData) then
  begin
   Result := 1;
   exit;
  end;

 // assert that no other instance exists already
 Assert(This_Mod^.UserData = nil);

 DontRaiseExceptionsAndSetFPUcodeword;

 try
  // instanciate TWinAmpObject
  This_Mod^.UserData := TWinAmpObject.CreateNew(nil, This_Mod);
  This_Mod^.ModifySamples := ModifySamples;
  Result := 0;
 except
  Result := 1;
 end;
end;

procedure Config(const This_Mod: PWinAmpDSPModule);
begin
 // assert that a pointer to the TWinAmpDSPModule exists
 Assert(Assigned(This_Mod));

 // open config dialog
 if Assigned(This_Mod^.UserData)
  then This_Mod^.UserData.Config;
end;

function ModifySamples(const This_Mod: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
begin
 Result := SampleFrames;

 // make sure a pointer to the TWinAmpDSPModule exists
 if not Assigned(This_Mod) then exit;

 // make sure a TWinAmpObject instance exists
 if not Assigned(This_Mod^.UserData) then exit;

 // call the objects 'ModifySamples'
 Result := This_Mod^.UserData.ModifySamples(Samples, SampleFrames,
   BitPerSample, ChannelCount, SampleRate);
end;

function ModifySamplesDummy(const This_Mod: PWinAmpDSPModule;
  const Samples: Pointer; const SampleFrames, BitPerSample, ChannelCount,
  SampleRate: Integer): Integer; cdecl;
begin
 Result := SampleFrames;
end;

procedure Quit(const This_Mod: PWinAmpDSPModule);
begin
 // assert that a pointer to the TWinAmpDSPModule exists
 if Assigned(This_Mod) and Assigned(This_Mod^.UserData) then
  try
   DontRaiseExceptionsAndSetFPUcodeword;
   This_Mod^.ModifySamples := ModifySamplesDummy;
   sleep(5);
   if Assigned(Application)
    then Application.ProcessMessages;
   This_Mod^.UserData.Quit;
  finally
   try
    FreeAndNil(This_Mod^.UserData);
   except
   end;
  end;
end;


{ TWinAmpObject }

constructor TWinAmpObject.CreateNew(AOwner: TComponent; const AWinAmpDspModule: PWinampDSPModule);
var
  ContainedVSTPlugins : TStringList;
  RS                  : TResourceStream;
begin
 inherited CreateNew(AOwner);
 FWinAmpDspModule := AWinAmpDspModule;
 FSampleRate      := 44100;
 FSampleFrames    := 0;
 FNrChannels      := 0;
 FEnhanceFak      := 1;
 FBypass          := False;

 {$IFDEF UseCriticalSection}
 FCriticalSection := TCriticalSection.Create;
 {$ENDIF}

 try
  SetLength(FFxpName, 256);

  Assert(HInstance = AWinAmpDspModule.hDLLinstance);
  
//  GetModuleFileName(HInstance, @FFxpName[1], 254);
  GetModuleFileName(AWinAmpDspModule.hDLLinstance, @FFxpName[1], 254);
  FFxpName := StrPas(PChar(@FFxpName[1]));
  FRegistryKey := ExtractFileName(FFxpName);
  if FRegistryKey = 'dsp_vst.dll'                          
   then FRegistryKey := 'Software\WinAmp\VST Host DSP Plugin'
   else FRegistryKey := 'Software\WinAmp\' + Copy(FRegistryKey, 1, Pos('.dll', FRegistryKey) - 1);
  FFxpName := ExpandUNCFileName(Copy(FxpName, 1, Pos('.dll', FFxpName) - 1) + '.fxp');
 except
  FRegistryKey := 'Software\WinAmp\VST Host DSP Plugin';
  FFxpName := 'dsp_vst.fxp';
 end;

 FVstHost := TVstHost.Create(Self);
 with FVstHost do
  begin
   CanDos := [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo,
     hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo,
     hcdAsyncProcessing, hcdSupplyIdle, hcdEditFile, hcdStartStopProcess];
   ManageIdleAutomaticly := False;
   Tempo := 120.0;
   VendorVersion := 0;
   VstTimeInfo.SampleRate := 44100.0;
   VstTimeInfo.Tempo := 120.0;
   VstTimeInfo.Flags := [vtiNanosValid, vtiPpqPosValid, vtiTempoValid,
     vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid,
     vtiClockValid];
   VstVersion := 2300;
   with VstPlugIns.Add
    do OnAudioMasterUpdateDisplay := AudioMasterUpdateDisplay;
  end;

 ContainedVSTPlugins := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, LongWord(ContainedVSTPlugins));

  if ContainedVSTPlugins.Count > 0 then
   begin
    RS := TResourceStream.Create(HInstance, ContainedVSTPlugins[0], 'DLL');
    try
     VstHost[0].LoadFromStream(RS);
     VstHost[0].Active := True;
     UpdateVSTPlugin;
     if FileExists(FxpName)
      then VstHost[0].LoadPreset(FxpName);
    finally
     FreeAndNil(RS);
    end;
   end
  else
   with TRegistry.Create do
    try
     if OpenKeyReadOnly(RegistryKey) then
      begin
       if ValueExists('Last Plugin') then
        begin
         LoadVSTDLL(ReadString('Last Plugin'));
         if FileExists(FxpName) then
          try
           VstHost[0].LoadPreset(FxpName);
          finally
           if not ValueExists('Dispose Preset') or ReadBool('Dispose Preset') then
            if CheckAccessToFile(GENERIC_WRITE, FFxpName)
             then DeleteFile(FxpName);
          end;
        end;
      end;
    finally
     CloseKey;
     Free;
    end;

 finally
  FreeAndNil(ContainedVSTPlugins);
 end;

end;

destructor TWinAmpObject.Destroy;
var
  i : Integer;
begin
 try
  if FVstHost[0].EditVisible then
   try
    FVstHost[0].CloseEdit;
    FEditorForm.Close;
    FreeAndNil(FEditorForm);
    if Assigned(FEditorForm)
     then FreeAndNil(FEditorForm);
   except
   end;
  FVstHost[0].Active := False;

  for i := 0 to Length(FDownsampler) - 1 do
   if Assigned(FDownsampler[i]) then FreeAndNil(FDownsampler[i]);
  for i := 0 to Length(FUpsampler) - 1 do
   if Assigned(FUpsampler[i]) then FreeAndNil(FUpsampler[i]);
  SetLength(FDownsampler, 0);
  SetLength(FUpsampler, 0);
  for i := 0 to Length(FInputBuffer)  - 1 do Dispose(FInputBuffer[i]);
  for i := 0 to Length(FOutputBuffer) - 1 do Dispose(FOutputBuffer[i]);
  FreeAndNil(FVstHost);

  {$IFDEF UseCriticalSection}
  FreeAndNil(FCriticalSection);
  {$ENDIF UseCriticalSection}
 finally
   inherited;
 end;
end;

function TWinAmpObject.GetEnhanced: Boolean;
begin
 Result := FEnhanceFak > 1;
end;

function TWinAmpObject.GetWinampDSPModule: TWinampDSPModule;
begin
 Assert(Assigned(FWinAmpDspModule));
 Result := FWinAmpDspModule^;
end;

procedure TWinAmpObject.AudioMasterUpdateDisplay(Sender: TObject);
begin
 if Assigned(FEditorForm)
  then FEditorForm.CBPreset.ItemIndex := FVstHost[0].CurrentProgram;
end;

procedure TWinAmpObject.Config;
begin
  {$IFDEF UseCriticalSection}
  // Acquire critical section
  FCriticalSection.Acquire;
  try
  {$ENDIF}
   if not Assigned(FEditorForm)
    then FEditorForm := TFmWinAmpVST.Create(Self);

   if FVSTHost[0].Active then
    try
     FVSTHost[0].ShowEdit(FEditorForm.PnGUI);
     FVSTHost[0].Idle;
     FVSTHost[0].EditIdle;
    except
     raise
    end;
  {$IFDEF UseCriticalSection}
  finally
   // leave critical section
   FCriticalSection.Release;
  end;
  {$ENDIF}

 FEditorForm.Show;
end;

procedure TWinAmpObject.Quit;
begin
  {$IFDEF UseCriticalSection}
  FCriticalSection.Acquire;
  try
  {$ENDIF}
   FBypass := True;
   with FVstHost[0] do
    try
     if FileExists(FFxpName) then
      if CheckAccessToFile(GENERIC_WRITE, FFxpName)
       then SavePreset(FFxpName) else else
      if CheckAccessToFile(GENERIC_WRITE, ExtractFilePath(FFxpName))
       then SavePreset(FFxpName);

     if EditVisible
      then CloseEdit;
     FEditorForm.Close;
     FreeAndNil(FEditorForm);

     Active := False;
//     Unload;
    except
    end;

  {$IFDEF UseCriticalSection}
  finally
   FCriticalSection.Release;
  end;
  {$ENDIF}
end;

procedure TWinAmpObject.ConvertInterleaved8bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I8              : PShortIntArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak8 : Single = 1 / $80;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FInputBuffer[Channel]^[Sample] := I8^[Sample * ChannelCount + Channel] * DivFak8;
end;

procedure TWinAmpObject.ConvertInterleaved8bitToFloatOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I8              : PShortIntArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak8 : Single = 1 / $80;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FUpsampler[Channel].ProcessSample(I8^[Sample * ChannelCount + Channel] * DivFak8, PDAV2SingleArray(@FInputBuffer[Channel]^[2 * Sample])^);
end;

procedure TWinAmpObject.ConvertInterleaved16bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I16             : PSmallIntArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak16 : Single = 1 / $8000;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FInputBuffer[Channel]^[Sample] := I16^[Sample * ChannelCount + Channel] * DivFak16;
end;

procedure TWinAmpObject.ConvertInterleaved16bitToFloatOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I16             : PSmallIntArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak16 : Single = 1 / $8000;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FUpsampler[Channel].ProcessSample(I16^[Sample * ChannelCount + Channel] * DivFak16, PDAV2SingleArray(@FInputBuffer[Channel]^[2 * Sample])^);
end;

procedure TWinAmpObject.ConvertInterleaved24bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I24             : P3ByteArray absolute Data;
  Channel, Sample : Integer;
  TempData        : Integer;
const
  DivFak24 : Single = 1 / $800000;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    TempData := (ShortInt(I24^[Sample * ChannelCount + Channel][2]) shl 16) +
                         (I24^[Sample * ChannelCount + Channel][1]  shl 8)  +
                         (I24^[Sample * ChannelCount + Channel][0]);
    FInputBuffer[Channel]^[Sample] := TempData * DivFak24;
   end;
end;

procedure TWinAmpObject.ConvertInterleaved24bitToFloatOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I24             : P3ByteArray absolute Data;
  Channel, Sample : Integer;
  TempData        : Integer;
const
  DivFak24 : Single = 1 / $800000;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    TempData := (ShortInt(I24^[Sample * ChannelCount + Channel][2]) shl 16) +
                         (I24^[Sample * ChannelCount + Channel][1]  shl 8)  +
                         (I24^[Sample * ChannelCount + Channel][0]);
    FUpsampler[Channel].ProcessSample(TempData, PDAV2SingleArray(@FInputBuffer[Channel]^[2 * Sample])^);
   end;
end;

procedure TWinAmpObject.ConvertInterleaved32bitToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I32             : PIntegerArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak32 : Single = 1 / $80000000;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FInputBuffer[Channel]^[Sample] := I32^[Sample * ChannelCount + Channel] * DivFak32;
end;

procedure TWinAmpObject.ConvertInterleaved32bitToFloatOversampled(
  const Data: Pointer; const ChannelCount, SampleFrames: Integer);
var
  I32             : PIntegerArray absolute Data;
  Channel, Sample : Integer;
const
  DivFak32 : Single = 1 / $80000000;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FUpsampler[Channel].ProcessSample(I32^[Sample * ChannelCount + Channel], PDAV2SingleArray(@FInputBuffer[Channel]^[2 * Sample])^);
end;

procedure TWinAmpObject.ConvertDummy(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
begin
 // do nothing (dummy)
end;

{$IFDEF UseFloatConvert}
procedure TWinAmpObject.ConvertInterleavedToFloat(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  Interleaved     : PDAVSingleFixedArray absolute Data;
  Channel, Sample : Integer;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FInputBuffer[Channel]^[Sample] := Interleaved^[Sample * ChannelCount + Channel];
end;

procedure TWinAmpObject.ConvertInterleavedToFloatOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  Interleaved     : PDAVSingleFixedArray absolute Data;
  Channel, Sample : Integer;
begin
 for Channel := 0 to min(Length(FInputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do FUpsampler[Channel].ProcessSample(Interleaved^[Sample * ChannelCount + Channel], PDAV2SingleArray(@FInputBuffer[Channel]^[2 * Sample])^);
end;
{$ENDIF}

procedure TWinAmpObject.ConvertFloatToInterleaved8bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I8 : PShortIntArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith8 : Single = $7E;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I8^[Sample * ChannelCount + Channel] := round(FOutputBuffer[Channel]^[Sample] * MulFakDith8 + random - random);
end;

procedure TWinAmpObject.ConvertFloatToInterleaved8bitOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I8 : PShortIntArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith8 : Single = $7E;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I8^[Sample * ChannelCount + Channel] := Round(Limit(FDownsampler[Channel].ProcessSample(PDAV2SingleArray(@FOutputBuffer[Channel]^[2 * Sample])^)) * MulFakDith8 + random - random);
end;

procedure TWinAmpObject.ConvertFloatToInterleaved16bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I16             : PSmallIntArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith16 : Single = $7FFE;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I16^[Sample * ChannelCount + Channel] := round(Limit(FOutputBuffer[Channel]^[Sample]) * MulFakDith16 + random - random);
end;

procedure TWinAmpObject.ConvertFloatToInterleaved16bitOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I16             : PSmallIntArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith16 : Single = $7FFE;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I16^[Sample * ChannelCount + Channel] := Round(Limit(FDownsampler[Channel].ProcessSample(PDAV2SingleArray(@FOutputBuffer[Channel]^[2 * Sample])^)) * MulFakDith16 + random - random);
end;

procedure TWinAmpObject.ConvertFloatToInterleaved24bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I24             : P3ByteArray absolute Data;
  Channel, Sample : Integer;
  TempData        : Integer;
  TempDataBytes   : array [0..3] of Byte absolute TempData;
const
  MulFakDith24 : Single = $7FFFFE;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    TempData := round(Limit(FOutputBuffer[Channel]^[Sample]) * MulFakDith24 + random - random);
    Move(TempDataBytes[0], I24^[Sample * ChannelCount + Channel], 3);
   end;
end;

procedure TWinAmpObject.ConvertFloatToInterleaved24bitOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I24             : P3ByteArray absolute Data;
  Channel, Sample : Integer;
  TempData        : Integer;
  TempDataBytes   : array [0..3] of Byte absolute TempData;
const
  MulFakDith24 : Single = $7FFFFE;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1 do
   begin
    TempData := round(Limit(FDownsampler[Channel].ProcessSample(PDAV2SingleArray(@FOutputBuffer[Channel]^[2 * Sample])^)) * MulFakDith24 + random - random);
    Move(TempDataBytes[0], I24^[Sample * ChannelCount + Channel], 3);
   end;
end;

procedure TWinAmpObject.ConvertFloatToInterleaved32bit(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I32             : PIntegerArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith32 : Single = $7FFFFFFE;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I32^[Sample * ChannelCount + Channel] := round(Limit(FOutputBuffer[Channel]^[Sample]) * MulFakDith32 + random - random);
end;

procedure TWinAmpObject.ConvertFloatToInterleaved32bitOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  I32             : PIntegerArray absolute Data;
  Channel, Sample : Integer;
const
  MulFakDith32 : Single = $7FFFFFFE;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do I32^[Sample * ChannelCount + Channel] := round(Limit(FDownsampler[Channel].ProcessSample(PDAV2SingleArray(@FOutputBuffer[Channel]^[2 * Sample])^)) * MulFakDith32 + random - random);
end;

{$IFDEF UseFloatConvert}
procedure TWinAmpObject.ConvertFloatToInterleaved(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  Interleaved     : PDAVSingleFixedArray absolute Data;
  Channel, Sample : Integer;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Interleaved^[Sample * ChannelCount + Channel] := Limit(FOutputBuffer[Channel]^[Sample]);
end;

procedure TWinAmpObject.ConvertFloatToInterleavedOversampled(const Data: Pointer;
  const ChannelCount, SampleFrames: Integer);
var
  Interleaved     : PDAVSingleFixedArray absolute Data;
  Channel, Sample : Integer;
begin
 for Channel := 0 to min(Length(FOutputBuffer), ChannelCount) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Interleaved^[Sample * ChannelCount + Channel] := Limit(FDownsampler[Channel].ProcessSample(PDAV2SingleArray(@FOutputBuffer[Channel]^[2 * Sample])^));
end;
{$ENDIF}

function TWinAmpObject.ModifySamples(const Samples: Pointer; const SampleFrames,
  BitPerSample, ChannelCount, SampleRate: Integer): Integer;
var
 i : Integer;
begin
 Result := SampleFrames;

 if not Assigned(FVstHost) or not FVstHost[0].Active or FBypass
  then exit;

  {$IFDEF UseCriticalSection}
  FCriticalSection.Acquire;
  try
  {$ENDIF}

(*
   if Length(fPDCBuffer) < nCh then SetLength(fPDCBuffer, nCh);
   for i := 0 to nCh - 1 do
    if Length(fPDCBuffer[i]) <> VstHost[0].InitialDelay
     then SetLength(fPDCBuffer[i],VstHost[0].InitialDelay);
*)

   // test if maximum blocksize changed
   if FEnhanceFak * SampleFrames > FSampleFrames then
    begin
     FSampleFrames := FEnhanceFak * SampleFrames;
     FVstHost[0].SetBlockSize(FEnhanceFak * SampleFrames);

     // reallocate input VST buffers
     for i := 0 to Length(FInputBuffer) - 1
      do ReallocMem(FInputBuffer[i], FEnhanceFak * SampleFrames * SizeOf(Single));

     // reallocate output VST buffers
     for i := 0 to Length(FOutputBuffer) - 1
      do ReallocMem(FOutputBuffer[i], FEnhanceFak * SampleFrames * SizeOf(Single));
    end;

   // test if samplerate changed
   if FEnhanceFak * SampleRate <> FSampleRate then
    begin
     FSampleRate := FEnhanceFak * SampleRate;
     FVstHost[0].SetSampleRate(FEnhanceFak * SampleRate);
    end;

   case BitPerSample of
     8: if FEnhanceFak > 1 then
         begin
          FWinAmpConvertIn  := ConvertInterleaved8bitToFloatOversampled;
          FWinAmpConvertOut := ConvertFloatToInterleaved8bitOversampled;
         end
        else
         begin
          FWinAmpConvertIn  := ConvertInterleaved8bitToFloat;
          FWinAmpConvertOut := ConvertFloatToInterleaved8bit;
         end;
    16: if FEnhanceFak > 1 then
         begin
          FWinAmpConvertIn  := ConvertInterleaved16bitToFloatOversampled;
          FWinAmpConvertOut := ConvertFloatToInterleaved16bitOversampled;
         end
        else
         begin
          FWinAmpConvertIn  := ConvertInterleaved16bitToFloat;
          FWinAmpConvertOut := ConvertFloatToInterleaved16bit;
         end;
    24: if FEnhanceFak > 1 then
         begin
          FWinAmpConvertIn  := ConvertInterleaved24bitToFloatOversampled;
          FWinAmpConvertOut := ConvertFloatToInterleaved24bitOversampled;
         end
        else
         begin
          FWinAmpConvertIn  := ConvertInterleaved24bitToFloat;
          FWinAmpConvertOut := ConvertFloatToInterleaved24bit;
         end;
    32: if FEnhanceFak > 1 then
         begin
          FWinAmpConvertIn  := ConvertInterleaved32bitToFloatOversampled; //ConvertInterleavedToFloatOversampled;
          FWinAmpConvertOut := ConvertFloatToInterleaved32bitOversampled; //ConvertFloatToInterleavedOversampled;
         end
        else
         begin
          FWinAmpConvertIn  := ConvertInterleaved32bitToFloat; //ConvertInterleavedToFloat;
          FWinAmpConvertOut := ConvertFloatToInterleaved32bit; //ConvertFloatToInterleaved;
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
   FVstHost[0].Process32Replacing(@FInputBuffer[0], @FOutputBuffer[0], FEnhanceFak * SampleFrames);

   // convert float to interleaved data
   FWinAmpConvertOut(Samples, ChannelCount, SampleFrames);

  {$IFDEF UseCriticalSection}
  finally
   FCriticalSection.Release;
  end;
 {$ENDIF}
end;

procedure TWinAmpObject.LoadVSTDLL(const VSTDLL: TFileName);
begin
  {$IFDEF UseCriticalSection}
  FCriticalSection.Acquire;
  try
  {$ENDIF}
   with FVstHost[0] do
    begin
     try
      if EditVisible then CloseEdit;
     except
     end;
     Active := False;
     try
      Unload;
     except
     end;

     DLLFileName := VSTDLL;
   //   FRealDelay := 0;
     Active := True;

     UpdateVSTPlugin;
    end;
  {$IFDEF UseCriticalSection}
  finally
   FCriticalSection.Release;
  end;
  {$ENDIF}

 if Assigned(FEditorForm)
  then FEditorForm.UpdatePluginInformation;
end;

procedure TWinAmpObject.UpdateVSTPlugin;
var
  i : Integer;
begin
 // allocate and clear VST input buffer
 if FVstHost[0].numInputs > Length(FInputBuffer) then
  begin
   SetLength(FInputBuffer, FVstHost[0].numInputs);
   SetLength(FUpsampler, FVstHost[0].numInputs);
   for i := 0 to Length(FInputBuffer) - 1 do
    begin
     ReallocMem(FInputBuffer[i], FSampleFrames * SizeOf(Single));
     FillChar(FInputBuffer[i]^[0], FSampleFrames * SizeOf(Single), 0);
     FUpsampler[i] := TPolyphaseUpsampler32.Create;
     FUpsampler[i].SetCoefficients(5, 0.1);
    end;
  end;

 // allocate and clear VST output buffer
 if FVstHost[0].numOutputs > Length(FOutputBuffer) then
  begin
   SetLength(FOutputBuffer, FVstHost[0].numOutputs);
   SetLength(FDownsampler, FVstHost[0].numInputs);
   for i := 0 to Length(FOutputBuffer) - 1 do
    begin
     ReallocMem(FOutputBuffer[i], FSampleFrames * SizeOf(Single));
     FillChar(FOutputBuffer[i]^[0], FSampleFrames * SizeOf(Single), 0);
     FDownsampler[i] := TPolyphaseDownsampler32.Create;
     FDownsampler[i].SetCoefficients(5, 0.1);
    end;
  end;

 if Assigned(FEditorForm) then
  try
   FVstHost[0].ShowEdit(FEditorForm.PnGUI);
   FVstHost[0].Idle;
   FVstHost[0].EditIdle;
  except
   raise
  end;
end;

procedure TWinAmpObject.SetEnhanced(const Value: Boolean);
var
  i : Integer;
begin
 if Value <> Enhanced then
  begin
   if Value then
    begin
     MessageDlg('This is an unregistered copy! To disable/remember this setting the' +
       #13#10 + 'next time the VST bridge starts, please consider a donation.',
       mtInformation, [mbOK], 0);
     FEnhanceFak := 2;
     for i := 0 to Length(FDownsampler) - 1 do
      if Assigned(FDownsampler[i]) then FDownsampler[i].ClearBuffers;
     for i := 0 to Length(FUpsampler) - 1 do
      if Assigned(FUpsampler[i]) then FUpsampler[i].ClearBuffers;
    end else FEnhanceFak := 1;
  end;
end;

initialization
 ScanResources; 

end.
