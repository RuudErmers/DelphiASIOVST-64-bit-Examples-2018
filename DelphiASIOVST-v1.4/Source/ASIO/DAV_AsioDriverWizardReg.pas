unit DAV_AsioDriverWizardReg;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Registers the plugin with the Delphi IDE.                                 //
//                                                                            //
//  Part of the ASIO Driver Framework by Christian Budde and Tobybear.         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

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
  DAV_AsioDriverModule,
  DAV_AsioDriverWizard;

procedure Register;

implementation

procedure Register;
begin
  // Register our DataModule Descendant
  {$IFDEF DELPHI5}
  RegisterCustomModule(TAsioDriverModule, TDataModuleDesignerCustomModule);
  {$ELSE}
  RegisterCustomModule(TAsioDriverModule, TDataModuleCustomModule);
  {$ENDIF}

  // Register our Wizard to add the new module to the Object Repository
  RegisterPackageWizard(TAsioDriverWizard.Create);
end;

end.
