program FloatMix;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmFloatMix};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmFloatMix, FmFloatMix);
  Application.Run;
end.

