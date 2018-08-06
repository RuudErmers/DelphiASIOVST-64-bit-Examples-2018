unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiPixelMap;

type
  TFmSphereTest = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmSphereTest: TFmSphereTest;

implementation

{$R *.dfm}

uses
  DAV_GuiCommon;

procedure TFmSphereTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmSphereTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSphereTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSphereTest.FormResize(Sender: TObject);
var
  X, Y     : Integer;
  CenterX  : Single;
  CenterY  : Single;
  YValue   : Single;
  XValue   : Single;
  ScnLn    : PPixel32Array;
  FltValue : Single;
  Value    : Byte;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   CenterX := 0.5 * Width;
   CenterY := 0.5 * Height;

   for Y := 0 to Height - 1 do
    begin
     YValue := Abs((Y - CenterY) / CenterY);
     ScnLn := ScanLine[Y];
     for X := 0 to Width - 1 do
      begin
       XValue := Abs((X - CenterX) / CenterX);
       FltValue := (1 - Sqrt(Sqr(XValue) + Sqr(YValue)));
       if FltValue > 0
        then Value := Round(Sqrt(FltValue) * $FF)
        else Value := 0;
       ScnLn^[X].ARGB := (Value shl 24) or (Value shl 16) or (Value shl 8) or Value;
      end;
    end;
  end;
 Invalidate;
end;

end.

