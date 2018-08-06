program prjTestPlugin;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1},
  MidiBase in '..\..\..\Common\MidiBase.pas',
  UMidiEvent in '..\..\..\Common\UMidiEvent.pas',
  UMidiNrpn in '..\..\..\Common\UMidiNrpn.pas',
  UMidiPorts in '..\..\..\Common\UMidiPorts.pas',
  UTickCount in '..\..\..\..\Common\UTickCount.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
