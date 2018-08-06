unit MainUnit;

{$I DAV_Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiPaintBox, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiGraphicControl;

type
  TFmPaintBoxTest = class(TForm)
    GuiPaintBox: TGuiPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmPaintBoxTest: TFmPaintBoxTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmPaintBoxTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmPaintBoxTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmPaintBoxTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmPaintBoxTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLne : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLne := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * random;
       Filter[0] := Filter[1];

       ScnLne[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLne[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLne[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

end.

