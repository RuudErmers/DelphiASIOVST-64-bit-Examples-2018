{******************************************************************************}
{                                                                              }
{ Code to generate the VSTModule-derived Data Module unit, where the audio     }
{ processing code will reside.                                                 }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTClonedModuleCreator;

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI, DAV_VSTHost, DAV_VSTCustomModule, DAV_VSTModule,
  DAV_VSTPluginCloneConfig;

type
  TVSTClonedModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FConfig: TConfig;
    procedure ClonePlugin(const VstModule: TVSTModule; VstPlugin: TCustomVstPlugIn);
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
  SysUtils, Classes, {$IFDEF DELPHI14_UP} AnsiStrings,{$ENDIF} DAV_VSTEffect,
  DAV_OpenToolsUtils, DAV_VSTParameters;

const
  CRLF          = #13#10;
  CAncestorName = 'VSTModule';

constructor TVSTClonedModuleCreator.Create(Config: TConfig);
begin
 FConfig := Config;
end;

procedure TVSTClonedModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  NativeFormEditor : INTAFormEditor;
  VstModule        : TVSTModule;
begin
 VstModule := TVSTModule(INTAComponent(FormEditor.GetRootComponent).GetComponent);
 with VstModule do
  begin
   ClonePlugin(VstModule, FConfig.VSTPlugin);

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

   if FConfig.ContainsGui then
    begin
     // wire the OnEditOpen event handler to our VSTModuleEditOpen method
     if Supports(FormEditor, INTAFormEditor, NativeFormEditor) then
      begin
       if NativeFormEditor.FormDesigner <> nil then
        begin
         DoCreateMethod(NativeFormEditor.FormDesigner,
           NativeFormEditor.FormDesigner.GetRoot, 'EditOpen',
           'VSTModuleEditOpen');
        end;
      end;
    end
   else Flags := Flags + [effFlagsHasEditor];
  end;
end;

procedure TVSTClonedModuleCreator.ClonePlugin(const VstModule: TVSTModule;
  VstPlugin: TCustomVstPlugIn);
var
  i                  : Integer;
  MinParam, MaxParam : Single;
  CurrentValue       : Single;
  OriginalParameter  : Single;
  ParamProperty      : TVstParameterPropertyRecord;
begin
 with VstModule do
  begin
   UniqueID       := VSTPlugin.UniqueID;
   EffectName     := VSTPlugin.EffectName;
   if VSTPlugin.Version < 10 then
    begin
     VersionRelease := VSTPlugin.Version;
     Version := AnsiString(IntToStr(VSTPlugin.Version));
    end
   else
    begin
     VersionMajor   := VSTPlugin.Version div 1000;
     VersionMinor   := (VSTPlugin.Version - VersionMajor * 1000) div 100;
     VersionRelease := (VSTPlugin.Version - VersionMajor * 1000 - VersionMinor * 100);
     Version        := AnsiString(IntToStr(VersionMajor) + '.' + IntToStr(VersionMinor));
    end;
   VendorName     := VSTPlugin.VendorString;
   ProductName    := VSTPlugin.ProductString;
   Flags          := VSTPlugin.EffectOptions;

   numInputs      := VSTPlugin.numInputs;
   numOutputs     := VSTPlugin.numOutputs;
   InitialDelay   := VSTPlugin.InitialDelay;
   RealQualities  := VSTPlugin.RealQualities;
   OffQualities   := VSTPlugin.OffQualities;
   IORatio        := VSTPlugin.IORatio;
   PlugCategory   := VSTPlugin.PlugCategory;
   CanDos         := VSTPlugin.VSTCanDos;

   for i := 0 to VSTPlugin.numParams - 1 do
    with ParameterProperties.Add do
     begin
      DisplayName := string(VSTPlugin.ParameterName[i]);
      Units := VSTPlugin.ParameterLabel[i];

      try
       if VSTPlugin.GetParameterProperties(i, ParamProperty) then
        begin
         StepFloat        := ParamProperty.StepFloat;
         SmallStepFloat   := ParamProperty.SmallStepFloat;
         LargeStepFloat   := ParamProperty.LargeStepFloat;
         Flags            := ParamProperty.Flags;
         MinInteger       := ParamProperty.MinInteger;
         MaxInteger       := ParamProperty.MaxInteger;
         StepInteger      := ParamProperty.StepInteger;
         LargeStepInteger := ParamProperty.LargeStepInteger;
         Category         := ParamProperty.CategoryLabel;
         ShortLabel       := ParamProperty.ShortLabel;
        end;
      except
      end;

      CanBeAutomated := VstPlugin.CanBeAutomated(i) > 0;
      OriginalParameter := VSTPlugin.Parameter[i];

      // check parameter minimum
      try
       Parameter[i] := 0;
       MinParam := StrToFloat(string(VSTPlugin.ParameterDisplay[i]));
      except
       MinParam := 0;
      end;

      // check parameter maximum
      try
       Parameter[i] := 1;
       MaxParam := StrToFloat(string(VSTPlugin.ParameterDisplay[i]));
      except
       MaxParam := 1;
      end;

      // eventually set min/max
      if MinParam < MaxParam then
       try
        Parameter[i] := 0.5;
        CurrentValue := StrToFloat(string(VSTPlugin.ParameterDisplay[i]));
        if (CurrentValue > MinParam) and (CurrentValue < MaxParam) then
         begin
          Min := MinParam;
          Max := MaxParam;
         end;
       except
       end
      else
       begin
        // ToDo
       end; 

      VSTPlugin.Parameter[i] := OriginalParameter;
     end;

   for i := 0 to VSTPlugin.numPrograms - 1 do
    with Programs.Add do
     begin
      VSTPlugin.CurrentProgram := i;
      DisplayName := string(VSTPlugin.ProgramName);
     end;
  end;
end;

function TVSTClonedModuleCreator.GetAncestorName: string;
begin
  Result := CAncestorName;
end;

function TVSTClonedModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TVSTClonedModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TVSTClonedModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TVSTClonedModuleCreator.GetFormName: string;
begin
  Result := FConfig.PluginFormName;
end;

function TVSTClonedModuleCreator.GetImplFileName: string;
begin
  //Result := '';
  Result := IncludeTrailingPathDelimiter(FConfig.ProjectPath) +
    FConfig.PluginUnitName + '.pas';
end;

function TVSTClonedModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TVSTClonedModuleCreator.GetMainForm: Boolean;
begin
  Result := True;
end;

function TVSTClonedModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetModuleOwner;
end;

function TVSTClonedModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TVSTClonedModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TVSTClonedModuleCreator.GetUnnamed: Boolean;
begin
  Result := False; // False still queries for a project name!
end;

function TVSTClonedModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string):
  IOTAFile;
begin
  Result := nil;
end;

function TVSTClonedModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
var
  s    : string;
  flt  : Single;
  i, j : Integer;
begin
  // Can use either FConfig.PluginFormName or FormIdent here, as they are set to
  // to the same value in TVSTClonedModuleCreator.GetFormName
  s :=
    'unit ' + ModuleIdent + ';' + CRLF +
    CRLF +
    'interface' + CRLF +
    CRLF +
    'uses ' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Forms, ' + CRLF +
    '  DAV_Common, DAV_VSTModule;' + CRLF +
    CRLF +
    'type' + CRLF +
    '  T' + FormIdent + ' = class(T' + AncestorIdent + ')' + CRLF;

  s := s + '    procedure VSTModuleOpen(Sender: TObject);' + CRLF;

  if FConfig.ContainsGui then
   begin
    s := s +
      '    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);' +
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

  // add uses
  if FConfig.ContainsGui then
   begin
    s := s +
      'uses' + CRLF +
      '  ' + FConfig.EditorUnitName + ';' + CRLF +
      CRLF;
   end;

  // initialize programs
  if FConfig.VSTPlugin.numPrograms > 0 then
   begin
    s := s +
      'procedure T' + FormIdent + '.VSTModuleOpen(Sender: TObject);' + CRLF +
      'begin' + CRLF;

    for i := 0 to FConfig.VSTPlugin.numPrograms - 1 do
     begin
      FConfig.VSTPlugin.CurrentProgram := i;
      s := s + ' with Programs[' + IntToStr(i) + '] do'  + CRLF +
        '  begin' + CRLF;

      // scan programs
      for j := 0 to FConfig.VSTPlugin.numParams - 1 do
       begin
        try
         if TryStrToFloat(string(FConfig.VSTPlugin.ParameterDisplay[j]), flt)
          then s := s + '   Parameter[' + IntToStr(j) + '] := ' +
            string(FConfig.VSTPlugin.ParameterDisplay[j]) + ';' + CRLF;
        except
        end;
       end;

      s := s +
        '  end;' + CRLF;
     end;

    s := s +
      'end;' + CRLF +
      CRLF;
   end;

  // add Editor Open procedure
  if FConfig.ContainsGui then
   begin
    s := s +
      'procedure T' + FormIdent + '.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);' + CRLF +
      'begin' + CRLF +
      '  GUI := T' + FConfig.EditorFormName + '.Create(Self);' + CRLF +
      'end;' + CRLF +
      CRLF;
   end;

  s := s + 'end.';

  Result := StringToIOTAFile(s);
end;

function TVSTClonedModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.

