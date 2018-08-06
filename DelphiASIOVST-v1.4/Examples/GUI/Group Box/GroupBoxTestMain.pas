unit GroupBoxTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, DAV_GuiGroup, DAV_GuiPixelMap, DAV_GuiSlider,
  DAV_GuiCheckBox, DAV_GuiGraphicControl, DAV_GuiLabel;

type
  TFmGroupBoxTest = class(TForm)
    ColorDialog: TColorDialog;
    GroupA: TGuiGroup;
    GroupB: TGuiGroup;
    GroupC: TGuiGroup;
    GroupD: TGuiGroup;
    ShGroupColor: TShape;
    CbTransparent: TGuiControlsCheckBox;
    SlBorderWidth: TGuiSlider;
    SlRoundRadius: TGuiSlider;
    LbOutlineWidth: TGuiLabel;
    LbRoundRadius: TGuiLabel;
    LbColor: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SlRoundRadiusChange(Sender: TObject);
    procedure SlBorderWidthChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure ShGroupColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FBackground : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmGroupBoxTest: TFmGroupBoxTest;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmGroupBoxTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmGroupBoxTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmGroupBoxTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmGroupBoxTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLn[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLn[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmGroupBoxTest.ShGroupColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 with ColorDialog do
  begin
   Color := ShGroupColor.Brush.Color;
   if Execute then
    begin
     ShGroupColor.Brush.Color := Color;
     GroupA.GroupColor := Color;
     GroupB.GroupColor := Color;
     GroupC.GroupColor := Color;
     GroupD.GroupColor := Color;
    end;
  end;
end;

procedure TFmGroupBoxTest.SlBorderWidthChange(Sender: TObject);
begin
 GroupA.BorderWidth := SlBorderWidth.Value;
 GroupB.BorderWidth := SlBorderWidth.Value;
 GroupC.BorderWidth := SlBorderWidth.Value;
 GroupD.BorderWidth := SlBorderWidth.Value;
end;

procedure TFmGroupBoxTest.SlRoundRadiusChange(Sender: TObject);
begin
 GroupA.BorderRadius := SlRoundRadius.Value;
 GroupB.BorderRadius := SlRoundRadius.Value;
 GroupC.BorderRadius := SlRoundRadius.Value;
 GroupD.BorderRadius := SlRoundRadius.Value;
end;

procedure TFmGroupBoxTest.CbTransparentClick(Sender: TObject);
begin
 GroupA.Transparent := CbTransparent.Checked;
 GroupB.Transparent := CbTransparent.Checked;
 GroupC.Transparent := CbTransparent.Checked;
 GroupD.Transparent := CbTransparent.Checked;
 SlBorderWidth.Transparent := CbTransparent.Checked;
 SlRoundRadius.Transparent := CbTransparent.Checked;
 LbOutlineWidth.Transparent := CbTransparent.Checked;
 LbRoundRadius.Transparent := CbTransparent.Checked;
 Invalidate;
end;

end.
