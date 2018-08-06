unit MainUnit;

{$I DAV_Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiStitchedControls,
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList, DAV_GuiImageControl,
  DAV_GuiCustomControl;

type
  TFmDialTest = class(TForm)
    GuiStitchedDial0: TGuiStitchedDial;
    GuiStitchedDial1: TGuiStitchedDial;
    GuiStitchedDial2: TGuiStitchedDial;
    GuiStitchedDial3: TGuiStitchedDial;
    GuiStitchedPNGList: TGuiStitchedPNGList;
    LbValue: TLabel;
    LbCurrentValue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure GuiStitchedDialChange(Sender: TObject);
    procedure Dial0MappingChange(Sender: TObject);
    procedure GuiStitchedDial3Change(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmDialTest: TFmDialTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_GuiFileFormats, DAV_GuiPng;

procedure TFmDialTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;

 GuiStitchedDial0.ImageIndex := 0;
 GuiStitchedDial1.ImageIndex := 0;
 GuiStitchedDial2.ImageIndex := 0;
 GuiStitchedDial3.ImageIndex := 0;

 GuiStitchedDial0.LockCursor := True;
end;

procedure TFmDialTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmDialTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmDialTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  b      : ShortInt;
  ScnLn  : PPixel32Array;
begin
 if Assigned(FBackground) then
  with FBackground do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * (2 * Random - 1);
        b := Round($7F + $3F * s[1]);
        s[0] := s[1];
        ScnLn[x].B := b;
        ScnLn[x].G := b;
        ScnLn[x].R := b;
       end;
     end;
   end;
end;

procedure TFmDialTest.GuiStitchedDial3Change(Sender: TObject);
begin
 GuiStitchedDial0.Value := GuiStitchedDial3.Value;
 GuiStitchedDial2.Value := GuiStitchedDial3.Value;

 GuiStitchedDialChange(Sender);
end;

procedure TFmDialTest.GuiStitchedDialChange(Sender: TObject);
begin
 if Sender is TGuiStitchedDial then
  with TGuiStitchedDial(Sender)
   do LbValue.Caption := FloatToStrF(Value, ffGeneral, 4, 4);
end;

procedure TFmDialTest.Dial0MappingChange(Sender: TObject);
begin
 GuiStitchedDial0.CurveMapping := GuiStitchedDial1.Value;

 GuiStitchedDialChange(Sender);
end;

end.
