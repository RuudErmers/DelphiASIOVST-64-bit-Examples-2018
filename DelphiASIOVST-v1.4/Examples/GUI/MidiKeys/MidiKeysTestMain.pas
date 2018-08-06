unit MidiKeysTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiBaseControl, DAV_GuiMidiKeys, DAV_GuiLED;

type
  TForm1 = class(TForm)
    MidiKeysA: TGuiMidiKeys;
    MidiKeysB: TGuiMidiKeys;
    MidiKeysC: TGuiMidiKeys;
    MidiKeysD: TGuiMidiKeys;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 MidiKeysB.AntiAlias := gaaLinear2x;
 MidiKeysC.AntiAlias := gaaLinear3x;
 MidiKeysD.AntiAlias := gaaLinear4x;
end;

end.
