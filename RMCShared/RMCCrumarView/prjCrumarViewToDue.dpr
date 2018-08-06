program prjCrumarViewToDue;

uses
  Vcl.Forms,
  UCrumarViewToDueMain in 'UCrumarViewToDueMain.pas' {Form1},
  UCrumarView in 'UCrumarView.pas',
  UCrumarViewFrame in 'UCrumarViewFrame.pas' {CrumarViewFrame: TFrame},
  UAckedSerialRMSProtocol in '..\..\Super52\MidiHWDevices\UAckedSerialRMSProtocol.pas',
  UAckedSerial in '..\..\Super52\MidiHWDevices\UAckedSerial.pas',
  UTickCount in '..\..\..\Common\UTickCount.pas',
  URegVeryEasy in '..\..\..\Common\URegVeryEasy.pas',
  UMidiEvent in '..\..\Common\UMidiEvent.pas',
  UVirtCC in '..\..\Common\UVirtCC.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
