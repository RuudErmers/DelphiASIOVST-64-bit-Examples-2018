program MidiGui;

uses
  Forms,
  MidiGuiU in 'MidiGuiU.pas' {KbDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TKbDemoForm, KbDemoForm);
  Application.Run;
end.
