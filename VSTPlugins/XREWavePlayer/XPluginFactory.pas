unit XPluginFactory;

interface

uses XPluginREWavePlayer,UIXPlugin,DAV_VSTModule;

const UniqueID:string = 'XWVP';
const ProductName:string = 'XREWavePlayer';
function CreateObject(vstModule:TvstModule;samplerate:single): IXPlugin;

implementation

function CreateObject(vstModule: TvstModule;  samplerate: single): IXPlugin;
begin
  result:=TXPluginREWavePlayer.Create(vstModule, samplerate);
end;

end.

