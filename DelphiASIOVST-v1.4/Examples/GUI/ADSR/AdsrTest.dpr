program AdsrTest;

uses
  FastMM4,
  Forms,
  AdsrTestMain in 'AdsrTestMain.pas' {FmAdsrTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAdsrTest, FmAdsrTest);
  Application.Run;
end.
