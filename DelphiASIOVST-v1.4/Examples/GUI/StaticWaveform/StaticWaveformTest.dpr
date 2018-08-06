program StaticWaveformTest;

uses
  Forms,
  StaticWaveformTestMain in 'StaticWaveformTestMain.pas' {FmStaticWaveformTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmStaticWaveformTest, FmStaticWaveformTest);
  Application.Run;
end.
