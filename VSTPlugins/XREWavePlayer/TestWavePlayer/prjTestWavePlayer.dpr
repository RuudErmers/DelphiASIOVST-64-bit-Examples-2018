program prjTestWavePlayer;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1},
  UMidiPorts in '..\..\..\Common\UMidiPorts.pas',
  UMidiEvent in '..\..\..\Common\UMidiEvent.pas',
  MidiBase in '..\..\..\Common\MidiBase.pas',
  UMidiNrpn in '..\..\..\Common\UMidiNrpn.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
