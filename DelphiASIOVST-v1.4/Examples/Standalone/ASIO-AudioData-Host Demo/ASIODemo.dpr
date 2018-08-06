program ASIODemo;

uses
  Forms,
  ASIODemoForm in 'ASIODemoForm.pas' {FmASIO},
  DAV_ASIOHostAudioData in '..\..\..\Source\ASIO\DAV_ASIOHostAudioData.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
