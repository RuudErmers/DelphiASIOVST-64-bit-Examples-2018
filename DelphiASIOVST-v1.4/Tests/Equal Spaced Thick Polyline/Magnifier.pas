unit Magnifier;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_GuiByteMap, DAV_GuiPixelMap, DAV_GuiGraphicControl,
  DAV_GuiLabel, DAV_GuiSlider;

type
  TFmMagnifier = class(TForm)
    PaintBox: TPaintBox;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  private
    FPixelMap : TGuiCustomPixelMap;
  public
    procedure Magnify(Scale: Integer);
  end;

var
  FmMagnifier: TFmMagnifier;

implementation

{$R *.dfm}

uses
  Math, DAV_GuiCommon, MainUnit;

procedure TFmMagnifier.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
end;

procedure TFmMagnifier.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
end;

procedure TFmMagnifier.FormResize(Sender: TObject);
begin
 with FPixelMap do
  begin
   SetSize(PaintBox.Width, PaintBox.Height);
   Magnify(4);
  end;
end;

procedure TFmMagnifier.Magnify(Scale: Integer);
var
  x, y   : Integer;
  xs, ys : Integer;
  xc, yc : Integer;
  Temp   : TPixel32;
begin
 with FmESTP do
  begin
   for y := 0 to Min(PixelMap.Height, Trunc(FPixelMap.Height / Scale)) - 1 do
    begin
     ys := Scale * y;
     for x := 0 to Min(PixelMap.Width, Trunc(FPixelMap.Width / Scale)) - 1 do
      begin
       xs := Scale * x;
       Temp := PixelMap.Pixel[x, y];

       for yc := 0 to Scale - 1 do
        for xc := 0 to Scale - 1 do
         FPixelMap.Pixel[xs + xc, ys + yc] := Temp;
      end;
    end;
  end;
end;

procedure TFmMagnifier.PaintBoxPaint(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(PaintBox.Canvas);
end;

end.

