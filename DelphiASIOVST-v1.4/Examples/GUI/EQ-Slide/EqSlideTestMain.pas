unit EqSlideTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_DspFilter, DAV_DspFilterBasics, DAV_GuiCustomControl,
  DAV_GuiEQSlide;

type
  TFmEqSlideTest = class(TForm)
    GuiEQSlide1: TGuiEQSlide;
    GuiEQSlide2: TGuiEQSlide;
    GuiEQSlide3: TGuiEQSlide;
    GuiEQSlide4: TGuiEQSlide;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function GuiEQSlideGetColor(Sender: TObject; const Frequency: Single): TColor;
    function GuiEQSlideGetColorBW(Sender: TObject; const Frequency: Single): TColor;
  private
    FLowpass : TBasicLowpassFilter;
  public
    { Public-Deklarationen }
  end;

var
  FmEqSlideTest: TFmEqSlideTest;

implementation

{$R *.dfm}

uses
  DAV_Common, DAV_GuiCommon;

procedure TFmEqSlideTest.FormCreate(Sender: TObject);
begin
 FLowpass := TBasicLowpassFilter.Create;
end;

procedure TFmEqSlideTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FLowpass);
end;

function TFmEqSlideTest.GuiEQSlideGetColor(Sender: TObject;
  const Frequency: Single): TColor;
var
  Gain : Single;
begin
 Gain := FLowpass.MagnitudeSquared(Frequency);
 Result := HLSToRGB(0.5 - 0.25 * Gain, 0.3 + 0.1 * Gain, 0.3 + 0.3 * Gain);
end;

function TFmEqSlideTest.GuiEQSlideGetColorBW(Sender: TObject;
  const Frequency: Single): TColor;
var
  Gain : Single;
  Col  : Byte;
begin
 Gain := FLowpass.MagnitudeSquared(Frequency);
 Col := Round(Limit(250 * Gain, 0, 255));
 Result := Col shl 16 + Col shl 8 + Col;
end;

end.
