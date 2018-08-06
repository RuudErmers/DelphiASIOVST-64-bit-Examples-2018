library SETrigoMath;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SETrigoMathModule in 'SETrigoMathModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..49] of TSETrigoMathModuleClass = (
    TSEArcCosFloatModule, TSEArcCosDoubleModule, TSEArcSinFloatModule,
    TSEArcSinDoubleModule, TSESinFloatModule, TSESinDoubleModule,
    TSECosFloatModule, TSECosDoubleModule, TSETanFloatModule,
    TSETanDoubleModule, TSECotanFloatModule, TSECotanDoubleModule,
    TSESecantFloatModule, TSESecantDoubleModule, TSECosecantFloatModule,
    TSECosecantDoubleModule, TSESinhFloatModule, TSESinhDoubleModule,
    TSECoshFloatModule, TSECoshDoubleModule, TSETanhFloatModule,
    TSETanhDoubleModule, TSECotHFloatModule, TSECotHDoubleModule,
    TSESecHFloatModule, TSESecHDoubleModule, TSECscHFloatModule,
    TSECscHDoubleModule, TSEArcCotFloatModule, TSEArcCotDoubleModule,
    TSEArcSecFloatModule, TSEArcSecDoubleModule, TSEArcCscFloatModule,
    TSEArcCscDoubleModule, TSEArcCoshFloatModule, TSEArcCoshDoubleModule,
    TSEArcSinhFloatModule, TSEArcSinhDoubleModule, TSEArcTanhFloatModule,
    TSEArcTanhDoubleModule, TSEArcCotHFloatModule, TSEArcCotHDoubleModule,
    TSEArcSecHFloatModule, TSEArcSecHDoubleModule, TSEArcCscHFloatModule,
    TSEArcCscHDoubleModule, TSELog10FloatModule, TSELog10HDoubleModule,
    TSELog2FloatModule, TSELog2HDoubleModule);

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(CModuleClasses)) then
  begin
   CModuleClasses[Index].GetModuleProperties(Properties);
   Result := True;
  end
 else Result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(CModuleClasses)) and (ProcessType = 1)
  then Result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect
  else Result := nil;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.