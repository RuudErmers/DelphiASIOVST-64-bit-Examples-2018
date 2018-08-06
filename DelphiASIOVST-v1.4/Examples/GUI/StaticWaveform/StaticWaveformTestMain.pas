unit StaticWaveformTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_Common, DAV_Types, DAV_GuiBaseControl,
  DAV_GuiStaticWaveform;

type
  TFmStaticWaveformTest = class(TForm)
    StaticWaveformA: TGuiStaticWaveform;
    StaticWaveformB: TGuiStaticWaveform;
    StaticWaveformC: TGuiStaticWaveform;
    StaticWaveformD: TGuiStaticWaveform;
    CbTransparent: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmStaticWaveformTest: TFmStaticWaveformTest;

implementation

{$R *.dfm}

procedure TFmStaticWaveformTest.FormCreate(Sender: TObject);
var
  WaveData : TDAVSingleDynArray;
  Sample   : Integer;
begin
 SetLength(WaveData, 256);
 for Sample := 0 to Length(WaveData) - 1
  do WaveData[Sample] := 2 * random - 1;
 StaticWaveformA.SetWaveForm(WaveData);
 StaticWaveformB.SetWaveForm(WaveData);
 StaticWaveformC.SetWaveForm(WaveData);
 StaticWaveformD.SetWaveForm(WaveData);
end;

procedure TFmStaticWaveformTest.CbTransparentClick(Sender: TObject);
begin
 StaticWaveformA.Transparent := CbTransparent.Checked;
 StaticWaveformB.Transparent := CbTransparent.Checked;
 StaticWaveformC.Transparent := CbTransparent.Checked;
 StaticWaveformD.Transparent := CbTransparent.Checked;
end;

end.
