{******************************************************************************}
{                                                                              }
{ Code to generate the VSTModule-derived Data Module unit, where the audio     }
{ processing code will reside.                                                 }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTModuleCreator;

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_VSTPluginConfig;

type
  TVSTModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FConfig: TConfig;
  public
    constructor Create(Config: TConfig);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string):
      IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string):
      IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

uses
  SysUtils,
  DAV_VSTCustomModule, DAV_VSTModule, DAV_VSTEffect, DAV_OpenToolsUtils;

const
  CRLF               = #13#10;
  CAnchestorName     = 'VSTModule';
  CNumInputsEffect   = 2;
  CNumOutputsEffect  = 2;
  CNumInputsSynth    = 0;
  CNumOutputsSynth   = 2;

constructor TVSTModuleCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TVSTModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  NativeFormEditor: INTAFormEditor;
begin
  with TVSTModule(INTAComponent(FormEditor.GetRootComponent).GetComponent) do
  begin
    UniqueID       := FConfig.UniqueID;
    EffectName     := FConfig.EffectName;
    VersionMajor   := FConfig.VersionMajor;
    VersionMinor   := FConfig.VersionMinor;
    VersionRelease := FConfig.VersionRelease;
    Version        := AnsiString(IntToStr(VersionMajor) + '.' + IntToStr(VersionMinor));
    VendorName     := FConfig.VendorName;
    ProductName    := FConfig.ProductName;

    if FConfig.IsSynth then
    begin
      NumInputs    := CNumInputsSynth;
      NumOutputs   := CNumOutputsSynth;
      PlugCategory := vpcSynth;
      CanDos := CanDos + [vcdReceiveVstEvents, vcdReceiveVstMidiEvent];
      Flags := Flags + [effFlagsIsSynth];
    end
    else
    begin
      NumInputs    := CNumInputsEffect;
      NumOutputs   := CNumOutputsEffect;
      PlugCategory := vpcEffect;
      if (NumInputs = 1) then
      begin
        if (NumOutputs = 1) then CanDos := CanDos + [vcd1in1out] else
        if (NumOutputs = 2) then CanDos := CanDos + [vcd1in2out];
      end else
      if (NumInputs = 2) then
      begin
        if (NumOutputs = 1) then CanDos := CanDos + [vcd2in1out] else
        if (NumOutputs = 2) then CanDos := CanDos + [vcd2in2out] else
        if (NumOutputs = 4) then CanDos := CanDos + [vcd2in4out];
      end else
      if (NumInputs = 4) then
      begin
        if (NumOutputs = 2) then CanDos := CanDos + [vcd4in2out] else
        if (NumOutputs = 4) then CanDos := CanDos + [vcd4in4out] else
        if (NumOutputs = 8) then CanDos := CanDos + [vcd4in8out];
      end else
      if (NumInputs = 8) then
      begin
        if (NumOutputs = 4) then CanDos := CanDos + [vcd8in4out] else
        if (NumOutputs = 8) then CanDos := CanDos + [vcd8in8out];
      end else
      if (NumInputs = 0) then PlugCategory := vpcGenerator;
      if (NumOutputs = 0) then PlugCategory := vpcAnalysis;
    end;

    if FConfig.UseEditor then
    begin
      Flags := Flags + [effFlagsHasEditor];
      // wire the OnEditOpen event handler to our VSTModuleEditOpen method
      if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
      begin
        if NativeFormEditor.FormDesigner <> nil then
        begin
          DoCreateMethod(NativeFormEditor.FormDesigner,
            NativeFormEditor.FormDesigner.GetRoot, 'Open',
            'VSTModuleOpen');
        end;
      end;
    end;
  end;
end;

function TVSTModuleCreator.GetAncestorName: string;
begin
  Result := CAnchestorName;
end;

function TVSTModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTModuleCreator.GetFormName: string;
begin
  Result := FConfig.PluginFormName;
end;

function TVSTModuleCreator.GetImplFileName: string;
begin
  //Result := '';
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.PluginUnitName + '.pas';
end;

function TVSTModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTModuleCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  Result := nil;
end;

function TVSTModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s: string;
begin
  // Can use either FConfig.PluginFormName or FormIdent here, as they are set to
  // to the same value in TVSTModuleCreator.GetFormName
  s :=
    'unit ' + ModuleIdent + ';' + CRLF +
    CRLF +
    'interface' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, ' + CRLF +
    '  DAV_Types, DAV_VSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF;

  if FConfig.UseEditor then
  begin
    s := s +
      '    procedure VSTModuleOpen(Sender: TObject);' +
      CRLF;
  end;

  s := s +
    '  private' + CRLF +
    '  public' + CRLF +
    '  end;' + CRLF +
    CRLF +
    'implementation' + CRLF +
    CRLF +
    '{$R *.DFM}' + CRLF +
    CRLF;

  if FConfig.UseEditor then
  begin
    s := s +
      'uses' + CRLF +
      '  ' + FConfig.EditorUnitName + ';' + CRLF +
      CRLF +
      'procedure T' + FormIdent + '.VSTModuleOpen(Sender: TObject);' + CRLF +
      'begin' + CRLF +
      '  EditorFormClass := T' + FConfig.EditorFormName + ';' + CRLF +
      'end;' + CRLF +
      CRLF;
  end;

  s := s + 'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
