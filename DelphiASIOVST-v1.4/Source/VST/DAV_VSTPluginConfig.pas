{******************************************************************************}
{                                                                              }
{ Class used to hold the configuration information entered on the Wizard form  }
{ and enable it to be accessed by the code generation process.                 }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTPluginConfig;

interface

{$I ..\DAV_Compiler.inc}

type
  TConfig = class(TObject)
  private
    function RandomLetter: Char;
    function RandomUniqueID: string;
  public
    ProjectPath    : string;
    ProjectName    : string;
    PluginUnitName : string;
    PluginFormName : string;
    EditorUnitName : string;
    EditorFormName : string;
    UseEditor      : Boolean;
    UniqueID       : AnsiString;
    EffectName     : AnsiString;
    IsSynth        : Boolean;
    VersionMajor   : Integer;
    VersionMinor   : Integer;
    VersionRelease : Integer;
    VendorName     : AnsiString;
    ProductName    : AnsiString;
    SaveWhenDone   : Boolean;
    constructor Create;
  end;

implementation

uses
  Math, SysUtils, Registry, DAV_OpenToolsUtils;

const
  CDefaultUseEditor      = True;
  CDefaultIsSynth        = False;
  CDefaultVersionMajor   = 1;
  CDefaultVersionMinor   = 0;
  CDefaultVersionRelease = 0;

resourcestring
  RCDefaultPluginUnitName = 'PluginDM';
  RCDefaultPluginFormName = 'PluginDataModule';
  RCDefaultEditorUnitName = 'EditorFrm';
  RCDefaultEditorFormName = 'EditorForm';
  RCDefaultEffectName     = 'My Plugin';
  RCDefaultVendorName     = 'My Company';
  RCDefaultProductName    = 'My Product';

constructor TConfig.Create;
begin
  ProjectPath    := GetCurrentDir;
  ProjectName    := GetUniqueProjectName;
  PluginUnitName := RCDefaultPluginUnitName;
  PluginFormName := RCDefaultPluginFormName;
  EditorUnitName := RCDefaultEditorUnitName;
  EditorFormName := RCDefaultEditorFormName;
  UseEditor      := CDefaultUseEditor;
  UniqueID       := AnsiString(RandomUniqueID);
  EffectName     := AnsiString(RCDefaultEffectName);
  IsSynth        := CDefaultIsSynth;
  VersionMajor   := CDefaultVersionMajor;
  VersionMinor   := CDefaultVersionMinor;
  VersionRelease := CDefaultVersionRelease;
  VendorName     := AnsiString(RCDefaultVendorName);
  ProductName    := AnsiString(RCDefaultProductName);
end;

function TConfig.RandomLetter: Char;
begin
 {$IFDEF Delphi5}
 Result := Char(Ord('A') + Random(Ord('Z') - Ord('A')) + Random(2) * 32);
 {$ELSE}
 Result := Char(RandomRange(Ord('A'), Ord('Z')) + Random(2) * 32);
 {$ENDIF}
end;

function TConfig.RandomUniqueID: string;
begin
  Result := RandomLetter + RandomLetter + RandomLetter + RandomLetter;
end;

end.
