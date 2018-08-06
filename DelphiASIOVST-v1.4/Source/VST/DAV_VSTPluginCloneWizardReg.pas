{******************************************************************************}
{                                                                              }
{ Registers the plugin with the Delphi IDE.                                    }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTPluginCloneWizardReg;

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  {$IFDEF DELPHI6_UP}
  DesignIntf,    // DsgnIntf renamed to DesignIntf from Delphi 6
//DesignEditors, // TCustomModule moved to DesignEditors from Delphi 6
  DMForm,
  {$ELSE}
  DsgnIntf,
  DMDesigner,
  {$ENDIF}
  DAV_VSTPluginCloneWizard;

procedure Register;

implementation

procedure Register;
begin
  // Register our Wizard to add the new module to the Object Repository
  RegisterPackageWizard(TVSTPluginCloneWizard.Create);
end;

end.

