unit VoiceTestModuleU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule, DAVDProcessingComponent, DDspBaseComponent,
  DDspVoiceController, DDspVoice, DDSPBaseOsc, DDSPOscSine;

type
  TVoiceTestModule = class(TVSTModule)
    DspVoiceController1: TDspVoiceController;
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure DspVoiceController1CreateVoice(Sender: TDspVoiceController;
      MidiEvent: TAVDMidiEvent; var NewVoice: TDspVoice);
    procedure DspVoiceController1VoiceCountChanged(
      Sender: TDspVoiceController; ActiveVoices, AllVoices: Integer);
  end;

implementation

{$R *.DFM}

uses
  VoiceTestFormU, VoiceTestVoiceU, DVSTCustomModule;

procedure TVoiceTestModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TVoiceTestForm.Create(Self);
end;

procedure TVoiceTestModule.DspVoiceController1CreateVoice(
  Sender: TDspVoiceController; MidiEvent: TAVDMidiEvent;
  var NewVoice: TDspVoice);
var x: TDspVoiceInfo;
begin
  x := TDspVoiceInfo.Create(MidiEvent);
  NewVoice := TVoiceTestVoice.Create(Sender, x);
end;

procedure TVoiceTestModule.DspVoiceController1VoiceCountChanged(
  Sender: TDspVoiceController; ActiveVoices, AllVoices: Integer);
begin
  // just for debugging / this is not a good way to display voice count
  // because it's too slow and makes some bugs
  if assigned(EditorForm) then with EditorForm as TVoiceTestForm do
  begin
    lblAllV.Caption:=inttostr(AllVoices);
    lblActiveV.Caption:=inttostr(ActiveVoices);
  end;
end;

end.
