unit DAV_AsioDriverModuleCreator;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Code to generate the VSTModule-derived Data Module unit, where the audio  //
//  processing code will reside.                                              //
//                                                                            //
//  Part of the ASIO Driver Framework by Christian Budde and Tobybear.         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  DAV_AsioDriverConfig;

type
  TAsioDriverModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
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
  SysUtils, DAV_AsioDriverBasicModule, DAV_AsioDriverModule,
  DAV_OpenToolsUtils;

const
  CRLF               = #13#10;
  CAnchestorName     = 'AsioDriverModule';
  CNumInputsEffect   = 2;
  CNumOutputsEffect  = 2;
  CNumInputsSynth    = 0;
  CNumOutputsSynth   = 2;

constructor TAsioDriverModuleCreator.Create(Config: TConfig);
begin
  FConfig := Config;
end;

procedure TAsioDriverModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  NativeFormEditor: INTAFormEditor;
begin
  with TAsioDriverModule(INTAComponent(FormEditor.GetRootComponent).GetComponent) do
  begin
(*
    UniqueID       := FConfig.UniqueID;
    EffectName     := FConfig.EffectName;
    VersionMajor   := FConfig.VersionMajor;
    VersionMinor   := FConfig.VersionMinor;
    VersionRelease := FConfig.VersionRelease;
    Version        := IntToStr(VersionMajor) + '.' + IntToStr(VersionMinor);
    VendorName     := FConfig.VendorName;
    ProductName    := FConfig.ProductName;

    if FConfig.IsSynth then
    begin
      NumInputs    := CNumInputsSynth;
      NumOutputs   := CNumOutputsSynth;
      PlugCategory := vpcSynth;
      CanDos := CanDos + [vcdReceiveAsioDriverEvents, vcdReceiveAsioDriverMidiEvent];
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
      // wire the OnEditOpen event handler to our AsioDriverModuleEditOpen method
      if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
      begin
        if NativeFormEditor.FormDesigner <> nil then
        begin
          DoCreateMethod(NativeFormEditor.FormDesigner,
            NativeFormEditor.FormDesigner.GetRoot, 'EditOpen',
            'AsioDriverModuleEditOpen');
        end;
      end;
    end;
*)
  end;
end;

function TAsioDriverModuleCreator.GetAncestorName: string;
begin
  Result := CAnchestorName;
end;

function TAsioDriverModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TAsioDriverModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TAsioDriverModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TAsioDriverModuleCreator.GetFormName: string;
begin
  Result := FConfig.AsioDriverFormName;
end;

function TAsioDriverModuleCreator.GetImplFileName: string;
begin
  //Result := '';
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.AsioDriverUnitName + '.pas';
end;

function TAsioDriverModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TAsioDriverModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TAsioDriverModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TAsioDriverModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TAsioDriverModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TAsioDriverModuleCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TAsioDriverModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  Result := nil;
end;

function TAsioDriverModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s: string;
begin
  // Can use either FConfig.DriverFormName or FormIdent here, as they are set to
  // to the same value in TAsioDriverModuleCreator.GetFormName
  s :=
    'unit ' + ModuleIdent + ';' + CRLF +
    CRLF +
    'interface' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, ' + CRLF +
    '  DAV_Common, DAV_AsioDriverModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF;

  if FConfig.UseControlPanel then
  begin
    s := s +
      '    procedure AsioDriverModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);' +
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

  if FConfig.UseControlPanel then
  begin
    s := s +
      'uses' + CRLF +
      '  ' + FConfig.ControlPanelUnitName + ';' + CRLF +
      CRLF +
      'procedure T' + FormIdent + '.AsioDriverModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);' + CRLF +
      'begin' + CRLF +
      '  GUI := T' + FConfig.ControlPanelFormName + '.Create(Self);' + CRLF +
      'end;' + CRLF +
      CRLF;
  end;

  s := s + 'end.';

  Result := StringToIOTAFile(s);
end;

function TAsioDriverModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
