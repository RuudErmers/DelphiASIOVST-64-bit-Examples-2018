unit XPluginFactory;

interface

uses XPluginRECDS2,UIXPlugin,DAV_VSTModule;

const UniqueID:string = 'XDS2';
const ProductName:string = 'XRECDS2';
function CreateObject(vstModule:TvstModule;samplerate:single): IXPlugin;

implementation

function CreateObject(vstModule: TvstModule;  samplerate: single): IXPlugin;
begin
  result:=TXPluginRECDS2.Create(vstModule, samplerate);
end;

end.
