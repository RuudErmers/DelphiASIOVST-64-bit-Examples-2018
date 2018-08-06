unit UModularViewFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,DAV_GuiMidiKeys,
  URMCEmptyPanel,URMCBaseControlPanel,URMCControls,URMCConstants, Vcl.StdCtrls,
  DAV_GuiBaseControl;

type
  TModularViewFrame = class(TForm)
    Image1: TImage;
    MidiKeys: TGuiMidiKeys;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
