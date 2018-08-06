program ASIODemo;

uses
  FMX.Forms,
  ASIODemoForm in 'ASIODemoForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.

