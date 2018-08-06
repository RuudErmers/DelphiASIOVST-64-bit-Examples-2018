unit DAV_VSTModule;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF} Classes, DAV_VSTModuleWithDsp;

type
  TVSTModule = class(TDspVSTModule)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Flags;
    property About;
    property Version;
    property EditorFormClass;
    property EffectName;
    property ProductName;
    property VendorName;
    property VersionMajor;
    property VersionMinor;
    property VersionRelease;
    property PlugCategory;
    property Tempo;
    property TailSize;
    property CanDos;
    property SampleRate;
    property BlockSize;
    property numInputs;
    property numOutputs;
    property numParams;
    property numPrograms;
    property numCategories;
    property CurrentProgram;
    property CurrentProgramName;
    property ProcessingMode;
    property BlockModeSize;
    property BlockModeOverlap;
    property InitialDelay;
    property KeysRequired;
    property RealQualities;
    property OffQualities;
    property IORatio;
    property UniqueID;
    property ShellPlugins;
    property TruncateStrings;
    property Programs;
    property ParameterProperties;
    property ParameterCategories;
    property ProcessPrecisition;
    property OnGetChunkParameter;
    property OnOpen;
    property OnClose;
    property OnEditOpen;
    property OnEditClose;
    property OnEditIdle;
    property OnEditTop;
    property OnEditGetSize;
    property OnEditSleep;
    property OnEditorKeyUp;
    property OnEditorKeyDown;
    property OnEditorKnobMode;
    property OnAfterProgramChange;
    property OnBeforeProgramChange;
    property OnBeginLoadBank;
    property OnBeginLoadProgram;
    property OnBeginSetProgram;
    property OnBlockSizeChange;
    property OnCanDo;
    property OnCheckKey;
    property OnDispatcher;
    property OnEndSetProgram;
    property OnGetVU;
    property OnInConnected;
    property OnInitialize;
    property OnInputProperties;
    property OnOfflineNotify;
    property OnOfflinePrepare;
    property OnOfflineRun;
    property OnOutConnected;
    property OnSpeakerArrangementChanged;
    property OnOutputProperties;
    property OnParameterChange;
    property OnParameterSizeFailed;
    property OnProcess;
    property OnProcess32Replacing;
    property OnProcess64Replacing;
    property OnProcessEvents;
    property OnProcessEvent;
    property OnProcessMidi;
    property OnProcessMidiSysEx;
    property OnProcessVarIO;
    property OnResume;
    property OnSampleRateChange;
    property OnSetPanLaw;
    property OnSoftBypass;
    property OnStartProcess;
    property OnStopProcess;
    property OnSuspend;
    property OnVendorSpecific;

    {$IFDEF UseDelphi}
    property OldCreateOrder default True;
    {$ENDIF}
  end;


{$IFDEF FPC}
//function InitResourceComponent(Instance: TComponent; RootAncestor: TClass):Boolean;
{$ENDIF}

implementation

uses
  Forms {$IFNDEF FPC} , RtlConsts {$ENDIF};

{ TVSTModule }
    
{$IFNDEF FPC}

constructor TVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF UseDelphi}
 inherited Create(AOwner);
 if (ClassType <> TVSTModule) and not (csDesigning in ComponentState) then
  try
   if not InitInheritedComponent(Self, TDspVSTModule) then
     raise EResNotFound.CreateFmt('Resource %s not found', [ClassName]);
   try
    if Assigned(OnCreate) and OldCreateOrder then OnCreate(Self);
   except
    Forms.Application.HandleException(Self);
   end;
  except
  end;
 {$ELSE}
 inherited Create(AOwner);
 if Assigned(OnCreate) then OnCreate(Self);
 {$ENDIF}
end;

{$ELSE}

constructor TVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF UseDelphi}
 inherited Create(AOwner);
 if (ClassType <> TVSTModule) and not (csDesigning in ComponentState) then
  try
   if not InitInheritedComponent(Self, TDspVSTModule)
    then raise EStreamError.CreateFmt('Resource %s not found', [ClassName]);
   if OldCreateOrder then DoCreate;
  except
  end;
 {$ELSE}
 inherited Create(AOwner);
 {$ENDIF}
 {$IFDEF DebugLog} AddLogMessage('After TVSTModule Create'); {$ENDIF}
end;

{$ENDIF}

destructor TVSTModule.Destroy;
begin
 {$IFNDEF UseDelphi}
 if Assigned(FOnDestroy) then FOnDestroy(Self);
 {$ENDIF}
 inherited;
end;

{$IFDEF FPC}
(*
function InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
begin
 Result := InitLazResourceComponent(Instance, RootAncestor);
end;

initialization
//  Set8087CW(Default8087CW or $3F);
  RegisterInitComponentHandler(TVSTModule, @InitResourceComponent);
*)
{$ENDIF}

end.

