unit UFormRERompler;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DAV_GuiBaseControl,
  DAV_GuiMidiKeys, Vcl.Samples.Spin;

type
  TFormRERompler = class(TForm)
    ButtonLoadWave: TButton;
    MemoDebug: TMemo;
    MidiKeys: TGuiMidiKeys;
    ButtonEasy: TButton;
    SESampleRate: TSpinEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  FormRERompler: TFormRERompler;

implementation

{$R *.dfm}

end.
