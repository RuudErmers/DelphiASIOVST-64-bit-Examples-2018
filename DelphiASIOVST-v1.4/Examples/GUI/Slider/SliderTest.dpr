program SliderTest;

uses
  Forms,
  SliderTestMain in 'SliderTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSliderTest, FmSliderTest);
  Application.Run;
end.
