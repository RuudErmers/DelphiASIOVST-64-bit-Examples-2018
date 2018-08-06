program prjTestMidiControls;

uses
  Vcl.Forms,
  URMCSimpleTest in 'URMCSimpleTest.pas' {Form1},
  URMCPropertyForm in 'URMCPropertyForm.pas' {RMCPropertyForm},
  UMyScreen in '..\..\..\Common\UMyScreen.pas',
  URMCControls in 'URMCControls.pas',
  UTickCount in '..\..\..\Common\UTickCount.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TRMCPropertyForm, RMCPropertyForm);
  Application.Run;
end.
