unit DAV_AsioDriverConfig;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Class used to hold the configuration information entered on the Wizard    //
//  form and enable it to be accessed by the code generation process.         //
//                                                                            //
//  Part of the ASIO Driver Framework by Christian Budde and Tobybear.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

type
  TConfig = class(TObject)
  public
    ProjectPath          : string;
    ProjectName          : string;
    AsioDriverUnitName   : string;
    AsioDriverFormName   : string;
    ControlPanelUnitName : string;
    ControlPanelFormName : string;
    UseControlPanel      : Boolean;
    GUIDstring           : string;
    DriverName           : string;
    SaveWhenDone         : Boolean;
    constructor Create;
  end;

implementation

uses
  Math, SysUtils,
  DAV_OpenToolsUtils;

const
  CDefaultUseEditor      = True;
  CDefaultIsSynth        = False;
  CDefaultVersionMajor   = 1;
  CDefaultVersionMinor   = 0;
  CDefaultVersionRelease = 0;

resourcestring
  RCDefaultDriverUnitName       = 'PluginDM';
  RCDefaultDriverFormName       = 'PluginDataModule';
  RCDefaultControlPanelUnitName = 'EditorFrm';
  RCDefaultControlPanelFormName = 'EditorForm';
  RCDefaultDriverName           = 'My Driver';

constructor TConfig.Create;
var
  GUID : TGUID;
begin
  ProjectPath          := GetCurrentDir;
  ProjectName          := GetUniqueProjectName;
  AsioDriverUnitName   := RCDefaultDriverUnitName;
  AsioDriverFormName   := RCDefaultDriverFormName;
  ControlPanelUnitName := RCDefaultControlPanelUnitName;
  ControlPanelFormName := RCDefaultControlPanelFormName;
  UseControlPanel      := CDefaultUseEditor;
  DriverName           := RCDefaultDriverName;
  CreateGUID(GUID);
  GUIDstring           := GUIDToString(GUID);
end;

end.
