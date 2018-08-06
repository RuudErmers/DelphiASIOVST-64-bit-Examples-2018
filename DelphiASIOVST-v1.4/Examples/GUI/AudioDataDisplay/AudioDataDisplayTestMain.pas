unit AudioDataDisplayTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DAV_Classes, DAV_AudioData,
  DAV_GuiAudioDataDisplay, DAV_GuiPixelMap;

type
  TFmAudioDataDisplay = class(TForm)
    ADC: TAudioDataCollection32;
    ADD1: TGuiAudioDataDisplay;
    ADD2: TGuiAudioDataDisplay;
    ADD3: TGuiAudioDataDisplay;
    ADD4: TGuiAudioDataDisplay;
    Axis: TTabSheet;
    CbTransparent: TCheckBox;
    GuiAudioDataDisplay1: TGuiAudioDataDisplay;
    LbLineWidth: TLabel;
    PC: TPageControl;
    TbLineWidth: TTrackBar;
    TsBasic: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmAudioDataDisplay: TFmAudioDataDisplay;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmAudioDataDisplay.CbTransparentClick(Sender: TObject);
begin
 ADD1.Transparent := CbTransparent.Checked;
 ADD2.Transparent := CbTransparent.Checked;
 ADD3.Transparent := CbTransparent.Checked;
 ADD4.Transparent := CbTransparent.Checked;
end;

procedure TFmAudioDataDisplay.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;

 ADC.GenerateWhiteNoise(1);
 ADD1.Transparent := True;
 ADD2.Transparent := True;
 ADD3.Transparent := True;
 ADD4.Transparent := True;
end;

procedure TFmAudioDataDisplay.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmAudioDataDisplay.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmAudioDataDisplay.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * Random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmAudioDataDisplay.TbLineWidthChange(Sender: TObject);
begin
 ADD1.LineWidth := TbLineWidth.Position;
 ADD2.LineWidth := TbLineWidth.Position;
 ADD3.LineWidth := TbLineWidth.Position;
 ADD4.LineWidth := TbLineWidth.Position;
end;

end.
