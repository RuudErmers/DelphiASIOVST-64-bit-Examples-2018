unit AdsrTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiPixelMap, DAV_GuiCustomControl, DAV_GuiADSRGraph;

type
  TFmAdsrTest = class(TForm)
    ADSRGraph: TGuiADSRGraph;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmAdsrTest: TFmAdsrTest;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmAdsrTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmAdsrTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmAdsrTest.FormClick(Sender: TObject);
begin
 ADSRGraph.Transparent := not ADSRGraph.Transparent;
end;

procedure TFmAdsrTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmAdsrTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
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
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

end.
