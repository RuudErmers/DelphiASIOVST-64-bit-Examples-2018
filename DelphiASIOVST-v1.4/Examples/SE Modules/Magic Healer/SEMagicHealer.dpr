library SEMagicHealer;

uses
  SysUtils,
  Classes,
  SEwrapper in 'SEwrapper.pas';

{$E sem}
{$R *.res}

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.