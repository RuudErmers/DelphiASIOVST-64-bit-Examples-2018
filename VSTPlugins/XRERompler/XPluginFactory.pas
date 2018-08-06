unit XPluginFactory;

interface

uses XPluginRERompler,UIXPlugin,DAV_VSTModule;

const UniqueID:string = 'XROM';
const ProductName:string = 'XRERompler';
function CreateObject(vstModule:TvstModule;samplerate:single): IXPlugin;

implementation

function CreateObject(vstModule: TvstModule;  samplerate: single): IXPlugin;
begin
  result:=TXPluginRERompler.Create(vstModule, samplerate);
end;

end.

