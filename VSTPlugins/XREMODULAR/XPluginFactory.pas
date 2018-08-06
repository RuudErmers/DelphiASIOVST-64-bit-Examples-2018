unit XPluginFactory;

interface

uses XPluginREModular,UIXPlugin,DAV_VSTModule;

const UniqueID:string = 'XMLR';
const ProductName:string = 'XREModular';
function CreateObject(vstModule:TvstModule;samplerate:single): IXPlugin;

implementation

function CreateObject(vstModule: TvstModule;  samplerate: single): IXPlugin;
begin
  result:=TXPluginREModular.Create(vstModule, samplerate);
end;

end.


