{******************************************************************************}
{                                                                              }
{ Support routines that simplify use of the Open Tools API.                    }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_OpenToolsUtils;

interface

{$I DAV_Compiler.inc}

uses
  Classes, TypInfo,
  {$IFDEF DELPHI6_UP}
  DesignIntf, // DsgnIntf renamed to DesignIntf from Delphi 6
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  ToolsAPI;

type
  {$IFDEF DELPHI6_UP}
  FormDesignerInterface = IDesigner;
  {$ELSE}
  FormDesignerInterface = IFormDesigner;
  {$ENDIF}

function GetCurrentProjectGroup: IOTAProjectGroup;
function GetCurrentProject: IOTAProject;
function GetModuleOwner: IOTAModule;
function GetUniqueProjectName: string;
function ProjectExists(ProjectName: string): Boolean;
procedure SetProjectOption(OptionName: string; Value: Variant);
procedure DoCreateMethod(FormDesigner: FormDesignerInterface;
  Persistent: TPersistent; MethodShortName, MethodSourceName: string);

implementation

uses
  SysUtils {$IFDEF DELPHI6_UP}, StrUtils{$ENDIF};

function GetCurrentProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = 0 then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
end;

function GetCurrentProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
  ProjectGroup := GetCurrentProjectGroup;

  if Assigned(ProjectGroup) then
    if ProjectGroup.ProjectCount > 0 then
      Result := ProjectGroup.ActiveProject
end;

function GetModuleOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Module <> nil then
  begin
    if Module.QueryInterface(IOTAProject, NewModule) = 0 then
    begin
      Result := NewModule;
    end
    {$IFDEF DELPHI5}
    else if Module.GetOwnerCount > 0 then
    begin
      NewModule := Module.GetOwner(0);
    {$ELSE}
    else if Module.OwnerModuleCount > 0 then
    begin
      NewModule := Module.OwnerModules[0];
    {$ENDIF}
      if NewModule <> nil then
        if NewModule.QueryInterface(IOTAProject, Result) <> 0 then
          Result := nil;
    end;
  end;
end;

function GetUniqueProjectName: string;
var
  i: Integer;
begin
  i := 0;
  repeat
    Inc(i);
    Result := Format('Project%d', [i]);
  until not ProjectExists(Result);
end;

function ProjectExists(ProjectName: string): Boolean;
var
  ProjGroup: IOTAProjectGroup;
  i: Integer;
  TestProjectName: string;
begin
  Result := False;
  ProjGroup := GetCurrentProjectGroup;
  if ProjGroup <> nil then
  begin
    for i := 0 to ProjGroup.ProjectCount - 1 do
    begin
      // get the project name (without path or extension)
      TestProjectName :=
        ChangeFileExt(ExtractFileName(ProjGroup.Projects[i].FileName), '');
      if AnsiCompareText(TestProjectName, ProjectName) = 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

// e.g. SetProjectOption('Optimization', 1);
procedure SetProjectOption(OptionName: string; Value: Variant);
var
  po: IOTAOptions;
begin
  po := GetCurrentProject.ProjectOptions;
  po.Values[OptionName] := Value;
end;

// Use to create a new method in the source and assign it to a TPersistent
procedure DoCreateMethod(FormDesigner: FormDesignerInterface;
  Persistent: TPersistent; MethodShortName, MethodSourceName: string);
var
  Method: TMethod;
  PropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
begin
  TypeInfo := PTypeInfo(Persistent.ClassInfo);
  PropInfo := GetPropInfo(TypeInfo, 'On' + MethodShortName); // Ex: MethodName := "Click"
  Method := FormDesigner.CreateMethod(MethodSourceName,
    GetTypeData(PropInfo^.PropType^));
  SetMethodProp(Persistent, PropInfo, Method);
end;

end.

