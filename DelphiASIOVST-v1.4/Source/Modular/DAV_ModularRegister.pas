unit DAV_ModularRegister;

interface

{$I ..\DAV_Compiler.inc}

procedure Register;

implementation

{$IFNDEF FPC}{$R '..\..\Resources\DAV_ModularRegister.res'}{$ENDIF}

uses
  Classes, DAV_ModularManager, DAV_ModularBase, DAV_ModularPin,
  DAV_ModularEnvelopeFollower, DAV_ModularVoiceController, DAV_ModularOscSine,
  DAV_ModularOscSaw, DAV_ModularOscRamp, DAV_ModularOscSquare,
  DAV_ModularOscNoise, DAV_ModularOscAbsSine, DAV_ModularEnvelope,
  {$IFNDEF FPC}
  ToolsAPI,
   {$IFDEF DELPHI6_UP}
   DesignIntf,    // DsgnIntf renamed to DesignIntf from Delphi 6
// DesignEditors, // TCustomModule moved to DesignEditors from Delphi 6
   DMForm,
   {$ELSE}
   DsgnIntf,
   DMDesigner,
   {$ENDIF}
  {$ENDIF}
  DAV_ModularVoice;

procedure Register;
begin
  RegisterComponents('ASIO/VST Modular', [TModularManager,
                                          TDspVoiceController,
                                          TDspOscSine,
                                          TDspOscSaw,
                                          TDspOscRamp,
                                          TDspOscSquare,
                                          TDspOscNoise,
                                          TDspOscAbsSine,
                                          TDspEnvelope]);
  {$IFNDEF FPC}
   {$IFDEF DELPHI5}
   RegisterCustomModule(TDspVoice, TDataModuleDesignerCustomModule);
   {$ELSE}
   RegisterCustomModule(TDspVoice, TDataModuleCustomModule);
   {$ENDIF}
  {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_ModularRegister.lrs}
{$ENDIF}

end.
