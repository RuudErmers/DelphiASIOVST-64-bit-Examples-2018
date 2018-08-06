library SEConvolution;

{$I DAV_Compiler.inc}

{-$R 'IR.res' 'IR.rc'}

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEConvolutionModule in 'SEConvolutionModule.pas',
  SELowLatencyConvolutionModule in 'SELowLatencyConvolutionModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSEConvolutionModule.GetModuleProperties(Properties);
  1: TSELowLatencyConvolutionModule.GetModuleProperties(Properties);
  else Result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;
 if (ProcessType = 1) then
  case Index of
   0: SEModuleBase := TSEConvolutionModule.Create(SEAudioMaster, Reserved);
   1: SEModuleBase := TSELowLatencyConvolutionModule.Create(SEAudioMaster, Reserved);
  end;
 if Assigned(SEModuleBase)
  then Result := SEModuleBase.Effect
  else Result := nil;
end;

exports
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
