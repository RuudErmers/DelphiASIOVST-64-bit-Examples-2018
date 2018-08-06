library SEMerger;

{.$R 'Test.res' 'Test.rc'}

uses
  FastMM4,
//  FastMove,
//  RTLVCLOptimize,
  SysUtils,
  Classes,
  SEwrapper in 'SEwrapper.pas';

{$E sem}
{$R *.res}

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.