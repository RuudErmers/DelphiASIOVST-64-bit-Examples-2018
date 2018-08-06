unit XPluginFactory;

interface

uses XPluginRESyntorchestra,UIXPlugin,DAV_VSTModule;

const UniqueID:string = 'XRSO';
const ProductName:string = 'XRESyntOrchestra';
function CreateObject(vstModule:TvstModule;samplerate:single): IXPlugin;

implementation

function CreateObject(vstModule: TvstModule;  samplerate: single): IXPlugin;
begin
  result:=TXPluginRESyntorchestra.Create(vstModule, samplerate);
end;

end.

