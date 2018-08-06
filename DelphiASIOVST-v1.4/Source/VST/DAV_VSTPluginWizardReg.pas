{******************************************************************************}
{                                                                              }
{ Registers the plugin with the Delphi IDE.                                    }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

unit DAV_VSTPluginWizardReg;

interface

{$I ..\DAV_Compiler.inc}

uses
  ToolsAPI,
  {$IFDEF DELPHI6_UP}
  DesignIntf,    // DsgnIntf renamed to DesignIntf from Delphi 6
  DMForm,
  {$ELSE}
  DsgnIntf,
  DMDesigner,
  {$ENDIF}
  DAV_VSTModule, DAV_VSTPluginWizard;

procedure Register;

implementation

procedure Register;
begin
  // Register our DataModule Descendant
  {$IFDEF DELPHI5}
  RegisterCustomModule(TVSTModule, TDataModuleDesignerCustomModule);
  {$ELSE}
  RegisterCustomModule(TVSTModule, TDataModuleCustomModule);
  {$ENDIF}

  // Register our Wizard to add the new module to the Object Repository
  RegisterPackageWizard(TVSTPluginWizard.Create);
end;

end.
