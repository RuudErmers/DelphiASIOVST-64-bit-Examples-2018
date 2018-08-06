program ChunkParser;

uses
  FastMM4,
  Forms,
  CPmain in 'CPmain.pas' {FmChunkParser};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmChunkParser, FmChunkParser);
  Application.Run;
end.
