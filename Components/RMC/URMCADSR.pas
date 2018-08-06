unit URMCADSR;

interface

uses
  System.SysUtils, System.Classes, Messages, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics,Types;

type TRMCADSR = class (TPanel)
  private
    a,d,s,r:integer;
    procedure Paint; override;
    procedure PaintIt(Canvas: TCanvas;width,height:integer);
  public
    procedure ADSR(a,d,s,r:integer);
    constructor Create(AOwner : TComponent); override;
end;

procedure Register;

implementation

uses Math,Windows;

procedure Register;
begin
  RegisterComponents('RMC', [TRMCADSR]);
end;

type   TVCLBitmap = Vcl.Graphics.TBitmap;

{ TRMCADSR }

procedure CopyAntiAliased(handlet:THandle; wt,ht:integer;handles:THandle; ws,hs:integer);
begin
  SetStretchBltMode(Handlet, HALFTONE);
  StretchBlt(Handlet, 0, 0, wt,ht,handles,0,0,ws,hs, SRCCOPY);
end;


constructor TRMCADSR.Create(AOwner: TComponent);
begin
  inherited;
  ADSR(64,64,64,64);
end;

procedure TRMCADSR.ADSR(a, d, s, r: integer);
begin
  if a<>-1 then self.a:=a;
  if d<>-1 then self.d:=d;
  if s<>-1 then self.s:=s;
  if r<>-1 then self.r:=r;
  Invalidate;
end;

procedure TRMCADSR.Paint;
VAR bmp:TVCLBitmap;
    rw,rh,r:single;
begin
  bmp:=TVCLBitmap.Create;  // 150 x 150   -> 400 x 400 oid
  rw:=Width / 600;      // 0.25
  rh:=Height / 400;     // 0.35
  r:=min(rw,rh);        // = rh   -> Height 400
  if r>1 then r:=1;
  if r<0.5 then r:=0.5;
  bmp.SetSize(round(Width / r),round(Height/r));
  PaintIt(bmp.Canvas,bmp.Width,bmp.height);
  CopyAntiAliased(Canvas.Handle, self.Width, self.Height, Bmp.Canvas.Handle,Bmp.Width, Bmp.Height);
  bmp.Free;
end;


procedure TRMCADSR.PaintIt(Canvas:TCanvas;width,height:integer);
VAR ir:TRect;
    procedure ToCoord(VAR x1,y1,x2,y2:integer);
    VAR w,h:integer;
    begin
      w:=ir.right-ir.left;
      h:=ir.bottom-ir.top;
      x1:=ir.left+x1*w DIV 4000;
      x2:=ir.left+x2*w DIV 4000;
      y1:=ir.bottom-y1* h DIV 4000;
      y2:=ir.bottom-y2* h DIV 4000;
    end;
    procedure Line(x1,y1,x2,y2:integer);
    begin
      ToCoord(x1,y1,x2,y2);
      Canvas.MoveTo(x1,y1);
      Canvas.LineTo(x2,y2);
    end;
    procedure Text(x1,y1,x2,y2:integer;s:string);
    VAR c:TSize;
        r:Trect;
    begin
      ToCoord(x1,y1,x2,y2);
      r:=rect(x1,y1,x2,y2);
      c:=Canvas.TextExtent(s);
      x1:=(r.Left+r.Right) DIV 2-c.cx DIV 2;
      y1:=(r.Bottom-4)-c.cy;
      Canvas.TextRect(r,x1,y1,s);
    end;
VAR n,ta,td,ts,tr,h,w,yd:integer;
    rr:Trect;
begin
//  if not visible then exit;
  with Canvas do
  begin
    Brush.Color:=Color;
    Brush.Style:=bsSolid;
    Pen.Style:=psSolid;
    Pen.Color:=Color;
    rr:=Rect(0,0,width,height);
    Rectangle(rr);
    Pen.Style:=psSolid;
    Pen.Color:=clWhite;
    Pen.Width:=1;
    Font.Height:=18;
  end;
  // randje van 1/10 aan alle zijden en een verhouding van 2:1. Rsultaat in ir
  if width>2 * height then
  begin
    h:=height - 2 *height DIV 10;
    ir:=Rect((width - 2*h) DIV 2,height DIV 10,(width+2*h) DIV 2 , height-height DIV 10);
  end
  else
  begin
    w:=width - 2 *width DIV 10;
    w:= w DIV 2;
    ir:=Rect(width DIV 10, (height - w) DIV 2,width - width DIV 10, (height+w) DIV 2);
  end;
  Line(0,0,4000,0);
  Line(4000,0,4000,4000);
  Line(4000,4000,0,4000);
  Line(0,4000,0,0);
  Canvas.Pen.Width:=3;

  ta:=125+a*875 DIV 127;
  td:=125+d*875 DIV 127 *(127-s) DIV 127;
  tr:=125+r*875 DIV 127 *(s) DIV 127;
  yd:=s*4000 DIV 127;
  n:= (ta+td+tr);
  ta:=3000 * ta DIV n;
  td:=3000 * td DIV n;
  tr:=3000 * tr DIV n;
  ts:=1000;
  Line(0,0,ta,4000);
  Line(ta,4000,ta+td,yd);
  Line(ta+td,yd,ta+td+ts,yd);
  Line(ta+td+ts,yd,ta+td+ts+tr,0);
  Canvas.Pen.Style:=psDot;
  Canvas.Pen.Width:=1;
  Line(ta,0,ta,4000);
  Line(ta+td,0,ta+td,yd);
  Line(ta+td+ts,0,ta+td+ts,yd);
  Line(0,yd,ta+td,yd);
  Canvas.Font.Color:=clWhite;
  Text(0,-200,ta,-600,'A='+Inttostr(a));
  Text(ta,-200,ta+td,-600,'D='+Inttostr(d));
  Text(ta+td+ts,-200,ta+td+ts+tr,-600,'R='+Inttostr(r));
  Text(-600,yd+100,-50,yd-300,'S='+Inttostr(s));
end;



end.
