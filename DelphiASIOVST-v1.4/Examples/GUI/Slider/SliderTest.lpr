program SliderTest;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  SliderTestMain in 'SliderTestMain.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TFmSliderTest, FmSliderTest);
  Application.Run;
end.
