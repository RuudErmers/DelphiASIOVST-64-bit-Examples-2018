unit SELabelGUI;

interface

uses
  Windows, Classes, Graphics, DAV_SEModule, DAV_SEGUI, SELabelModule;

type
  TRGB24 = packed record
    B, G, R: Byte;
  end;
  TRGB24Array = packed array [0..MaxInt div SizeOf(TRGB24) - 1] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  TCustomSELabelGui = class(TSEGUIBase)
  protected
    FText       : AnsiString;
  public  
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
  end;

  TSELabelDsp = class(TCustomSELabelGui)
  protected
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
  end;

  TSELabelGui = class(TCustomSELabelGui)
  protected
    FFontName   : Integer;
    FFontShadow : Integer;
    FBitmap     : TBitmap;
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
    procedure GuiWindowOpen(WI: PSEWndInfo); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, DAV_Types;

{ TCustomSELabelGui }

constructor TCustomSELabelGui.Create(SEGuiCallback: TSEGuiCallback;
  AHostPtr: Pointer);
begin
 inherited;
 CallHost(seGuiHostSetWindowSize, 64, 32);
 CallHost(seGuiHostSetWindowType, 0); // 0 = Draw on SE's window (default), 1 = HWND based

// CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE or HWF_NO_CUSTOM_GFX_ON_STRUCTURE));
 CallHost(seGuiHostSetWindowFlags, Integer(hwfResizable));
end;


{ TSELabelDsp }

procedure TSELabelDsp.GuiPaint(hDC: HDC; wi :PSEWndInfo);
begin
 if FText <> '' then
  begin
   with TCanvas.Create do
    try
     Handle := hDC;
     Brush.Style := bsClear;
     TextOut(0, 0, FText);
    finally
     Free;
    end;
  end ;
end;

procedure TSELabelDsp.GuiPinValueChange(CurrentPin: TSEGuiPin);
var
  NewText : AnsiString;
begin
 case CurrentPin.PinIndex of
  1 : begin
       NewText := CurrentPin.ValueAsString;
       if NewText <> FText then
        begin
         FText := NewText;
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
 end;
 inherited;
end;

{ TSELabelGui }

constructor TSELabelGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
 FBitmap := TBitmap.Create;
 FBitmap.PixelFormat := pf24bit;
 FBitmap.Canvas.Font.Height := 4 * 24;
end;

destructor TSELabelGui.Destroy;
begin
 FreeAndNil(FBitmap);
 inherited;
end;

procedure Downsample4xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  Line : Array [0..4] of PRGB24Array;
begin
 with Bitmap do
  begin
   // first stage
   for y := 0 to (Height div 4) - 1 do
    begin
     Line[0] := Scanline[y];
     Line[1] := Scanline[y * 4];
     Line[2] := Scanline[y * 4 + 1];
     Line[3] := Scanline[y * 4 + 2];
     Line[4] := Scanline[y * 4 + 3];
     for x := 0 to (Width  div 4) - 1 do
      begin
       Line[0, x].B := (Line[1, 4 * x].B + Line[1, 4 * x + 1].B + Line[1, 4 * x + 2].B + Line[1, 4 * x + 3].B +
                        Line[2, 4 * x].B + Line[2, 4 * x + 1].B + Line[2, 4 * x + 2].B + Line[2, 4 * x + 3].B +
                        Line[3, 4 * x].B + Line[3, 4 * x + 1].B + Line[3, 4 * x + 2].B + Line[3, 4 * x + 3].B +
                        Line[4, 4 * x].B + Line[4, 4 * x + 1].B + Line[4, 4 * x + 2].B + Line[4, 4 * x + 3].B) div 16;
       Line[0, x].G := (Line[1, 4 * x].G + Line[1, 4 * x + 1].G + Line[1, 4 * x + 2].G + Line[1, 4 * x + 3].G +
                        Line[2, 4 * x].G + Line[2, 4 * x + 1].G + Line[2, 4 * x + 2].G + Line[2, 4 * x + 3].G +
                        Line[3, 4 * x].G + Line[3, 4 * x + 1].G + Line[3, 4 * x + 2].G + Line[3, 4 * x + 3].G +
                        Line[4, 4 * x].G + Line[4, 4 * x + 1].G + Line[4, 4 * x + 2].G + Line[4, 4 * x + 3].G) div 16;
       Line[0, x].R := (Line[1, 4 * x].R + Line[1, 4 * x + 1].R + Line[1, 4 * x + 2].R + Line[1, 4 * x + 3].R +
                        Line[2, 4 * x].R + Line[2, 4 * x + 1].R + Line[2, 4 * x + 2].R + Line[2, 4 * x + 3].R +
                        Line[3, 4 * x].R + Line[3, 4 * x + 1].R + Line[3, 4 * x + 2].R + Line[3, 4 * x + 3].R +
                        Line[4, 4 * x].R + Line[4, 4 * x + 1].R + Line[4, 4 * x + 2].R + Line[4, 4 * x + 3].R) div 16;
      end;
    end;
  end;
end;

procedure Upsample4xBitmap24(var Bitmap: TBitmap);
var
  x, y : Integer;
  i, j : Integer;
  Line : Array [0..4] of PRGB24Array;
begin
 with Bitmap do
  for y := (Height div 4) - 1 downto 0 do
   begin
    assert(PixelFormat = pf24bit);

    Line[0] := Scanline[y];
    Line[1] := Scanline[y * 4];
    Line[2] := Scanline[y * 4 + 1];
    Line[3] := Scanline[y * 4 + 2];
    Line[4] := Scanline[y * 4 + 3];
    for x := (Width  div 4) - 1 downto 0 do
     for i := 1 to 4 do
      for j := 0 to 3 do
       begin
        Line[i, 4 * x + j].B := Line[0, x].B;
        Line[i, 4 * x + j].G := Line[0, x].G;
        Line[i, 4 * x + j].R := Line[0, x].R;
       end;
   end;
end;

procedure TSELabelGui.GuiPaint(hDC: HDC; wi :PSEWndInfo);
var
  OldColor : TColor;
begin
 if FText <> '' then
  begin
   if FBitmap.Width  <> 4 * wi.Width  then FBitmap.Width  := 4 * wi.Width;
   if FBitmap.Height <> 4 * wi.Height then FBitmap.Height := 4 * wi.Height;
   with TCanvas.Create do
    try
     Handle := hDC;
     Brush.Style := bsClear;
     BitBlt(FBitmap.Canvas.Handle, 0, 0, Wi.Width, Wi.Height, Handle, 0, 0, SRCCOPY);
     FBitmap.Canvas.Brush.Style := bsClear;
     Upsample4xBitmap24(FBitmap);
     if FFontShadow <> 0 then
      begin
       OldColor := FBitmap.Canvas.Font.Color;
       FBitmap.Canvas.Font.Color := clBlack;
       FBitmap.Canvas.TextOut(FFontShadow, FFontShadow, string(FText));
       FBitmap.Canvas.Font.Color := OldColor;
      end;
     FBitmap.Canvas.TextOut(0, 0, string(FText));
     Downsample4xBitmap24(FBitmap);
     Draw(0, 0, FBitmap);
    finally
     Free;
    end;
  end ;
end;

procedure TSELabelGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
var
  NewText : AnsiString;
  i       : Integer;
  b       : Boolean;
begin
 case CurrentPin.PinIndex of
  0 : begin
       NewText := AnsiString(CurrentPin.ValueAsString);
       if NewText <> FText then
        begin
         FText := NewText;
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
  1 : begin
       i := CurrentPin.ValueAsInteger;
       if FBitmap.Canvas.Font.Color <> TColor(i) then
        begin
         FBitmap.Canvas.Font.Color := TColor(i);
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
  2 : begin
       i := CurrentPin.ValueAsInteger;
       if i <> FFontName then
        begin
         FFontName := i;
         FBitmap.Canvas.Font.Name := CFontList[i];
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
  3 : begin
       i := CurrentPin.ValueAsInteger;
       if FBitmap.Canvas.Font.Size <> 4 * i then
        begin
         FBitmap.Canvas.Font.Size := 4 * i;
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
  4 : begin
       b := CurrentPin.ValueAsBoolean;
       if (fsBold in FBitmap.Canvas.Font.Style) <> b then
        begin
         if b
          then FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style + [fsBold]
          else FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style - [fsBold];
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
  5 : begin
       b := CurrentPin.ValueAsBoolean;
       if (fsItalic in FBitmap.Canvas.Font.Style) <> b then
        begin
         if b
          then FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style + [fsItalic]
          else FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style - [fsItalic];
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
  6 : begin
       i := CurrentPin.ValueAsInteger;
       if i <> FFontShadow then
        begin
         FFontShadow := i;
         CallHost(seGuiHostRequestRepaint);
        end;
      end;
 end;
 inherited;
end;

procedure TSELabelGui.GuiWindowOpen(WI: PSEWndInfo);
begin
 inherited;
 FText := AnsiString(Pin[0].ValueAsString);
 FBitmap.Canvas.Font.Color := TColor(Pin[1].ValueAsInteger);
 FBitmap.Canvas.Font.Name := CFontList[Pin[2].ValueAsInteger];
 FBitmap.Canvas.Font.Size := 4 * Pin[3].ValueAsInteger;
 if Pin[4].ValueAsBoolean
  then FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style + [fsBold]
  else FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style - [fsBold];
 if Pin[5].ValueAsBoolean
  then FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style + [fsItalic]
  else FBitmap.Canvas.Font.Style := FBitmap.Canvas.Font.Style - [fsItalic];
 FFontShadow := Pin[6].ValueAsInteger;
end;

end.
