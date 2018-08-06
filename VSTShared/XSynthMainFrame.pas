unit XSynthMainFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DAV_GuiBaseControl,  DAV_GuiMidiKeys;

type
  TSynthMainFrame = class(TForm)
    MidiKeys: TGuiMidiKeys;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OnNoteEvent(pitch: integer; _on: boolean);

  end;
{
var
  SynthMainFrame: TSynthMainFrame; }

implementation

{$R *.dfm}

procedure TSynthMainFrame.FormCreate(Sender: TObject);
begin
  Name:=Name;
end;

procedure TSynthMainFrame.OnNoteEvent(pitch: integer; _on: boolean);
begin
  MidiKeys.SetKeyPressed(pitch,_on);
end;

end.
