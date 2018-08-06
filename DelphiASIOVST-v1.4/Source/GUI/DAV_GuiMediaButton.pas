unit DAV_GuiMediaButton;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, DAV_GuiCommon,
  DAV_GuiBaseControl, DAV_GuiPixelMap, DAV_GuiVectorPixelCircle;

type
  TMediaButtonState = (mbsPlay, mbsPause, mbsStop, mbsRecord, mbsFastBackward,
    mbsFastForward, mbsPrevious, mbsNext);

  TCustomGuiMediaButton = class(TCustomControl)
  private
    FBorderRadius : Single;
    FBorderWidth  : Single;
    FBorderColor  : TColor;
    FButtonColor  : TColor;
    FButtonState  : TMediaButtonState;
    FGlyphOffset  : Integer;
    FOnPaint      : TNotifyEvent;
    FOutlineWidth: Single;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Single);
    procedure SetBorderWidth(const Value: Single);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonState(const Value: TMediaButtonState);
    procedure SetOutlineWidth(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure ButtonStateChanged; virtual;
    procedure ControlChanged; virtual;
    procedure CalculateAbsoluteGlyphOffset;
    procedure OutlineWidthChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property OutlineWidth: Single read FOutlineWidth write SetOutlineWidth;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default $303030;
    property ButtonState: TMediaButtonState read FButtonState write SetButtonState default mbsPlay;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiMediaButton = class(TCustomGuiMediaButton)
  private
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;
    FTransparent      : Boolean;
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure BackBufferChanged; virtual;
    procedure BufferChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure ControlChanged; override;
    procedure RenderButton(PixelMap: TGuiCustomPixelMap);
    procedure RenderGlyph(PixelMap: TGuiCustomPixelMap);
    procedure RenderTriangle(PixelMap: TGuiCustomPixelMap; Rect: TRect;
      Color: TColor; Reverse: Boolean = False);
    procedure TransparentChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ButtonColor;
    property ButtonState;
    property OutlineWidth;

    property OnPaint;

    property Align;
    property Action;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

implementation

uses
  Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common, DAV_FixedPoint, DAV_GuiBlend,
  DAV_Approximations;


{ TCustomGuiMediaButton }

constructor TCustomGuiMediaButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := ControlStyle + [csDoubleClicks, csReplicatable, csOpaque] -
   [csFramed];

 // Ensure we're not a tab-stop
 TabStop := False;

 // initialize defaults
 Color := clBtnFace;
 FButtonColor  := $606060;
 FBorderColor  := $202020;
 FBorderWidth  := 1;
 FButtonState  := mbsPlay;
 FBorderRadius := 0;
 FOutlineWidth := 1;
 CalculateAbsoluteGlyphOffset;
end;

destructor TCustomGuiMediaButton.Destroy;
begin
 inherited Destroy;
end;

procedure TCustomGuiMediaButton.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiMediaButton then
  with TCustomGuiMediaButton(Dest) do
   begin
    FBorderRadius := Self.FBorderRadius;
    FBorderWidth  := Self.FBorderWidth;
    FBorderColor  := Self.FBorderColor;
    FButtonColor  := Self.FButtonColor;
    FButtonState  := Self.FButtonState;
    FOnPaint      := Self.FOnPaint;
   end;
end;

procedure TCustomGuiMediaButton.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetBorderRadius(const Value: Single);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetButtonState(const Value: TMediaButtonState);
begin
 if FButtonState <> Value then
  begin
   FButtonState := Value;
   ButtonStateChanged;
  end;
end;

procedure TCustomGuiMediaButton.SetOutlineWidth(const Value: Single);
begin
 if FOutlineWidth <> Value then
  begin
   FOutlineWidth := Value;
   OutlineWidthChanged;
  end;
end;

procedure TCustomGuiMediaButton.ButtonStateChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.OutlineWidthChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   ButtonColorChanged;
  end;
end;

procedure TCustomGuiMediaButton.BorderWidthChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.ControlChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMediaButton.BorderColorChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.BorderRadiusChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.ButtonColorChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiMediaButton.CalculateAbsoluteGlyphOffset;
begin
 FGlyphOffset := Max(3, Min(Width, Height) div 10);
end;


{ TGuiMediaButton }

constructor TGuiMediaButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FBuffer       := TGuiPixelMapMemory.Create;
 FBackBuffer   := TGuiPixelMapMemory.Create;

 // initiliaze defaults
 FTransparent  := False;
end;

destructor TGuiMediaButton.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited Destroy;
end;

procedure TGuiMediaButton.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiMediaButton then
  with TGuiMediaButton(Dest) do
   begin
    FTransparent := Self.FTransparent;
    FBuffer.Assign(Self.FBuffer);
    FBackBuffer.Assign(Self.FBackBuffer);
   end;
end;

procedure TGuiMediaButton.ControlChanged;
begin
 inherited;
end;

procedure TGuiMediaButton.Paint;
begin
 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FOnPaint)
  then FOnPaint(Self);

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);

 inherited;
end;

procedure TGuiMediaButton.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 ControlChanged;
end;

procedure TGuiMediaButton.BufferChanged;
begin
 FUpdateBuffer := True;
 ControlChanged;
end;

procedure TGuiMediaButton.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 if FTransparent then FBackBuffer.CopyParentImage(Self) else
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TGuiMediaButton.UpdateBuffer;
begin
 FUpdateBuffer := False;

 inherited;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 RenderButton(FBuffer);
 RenderGlyph(FBuffer);
end;

procedure TGuiMediaButton.RenderButton(PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  ButtonColor       : TPixel32;
  BorderColor       : TPixel32;
  CombColor         : TPixel32;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Temp              : Single;
begin
 with PixelMap do
  begin
   ButtonColor := ConvertColor(FButtonColor);
   if FBorderWidth > 0
    then BorderColor := ConvertColor(FBorderColor)
    else BorderColor := ButtonColor;

   // draw circle
   Radius := Min(Min(FBorderRadius, 0.5 * Width), 0.5 * Height) + 1;
   BorderWidth := Max(FBorderWidth, 1);

   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth - 1));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   for Y := 0 to Round(Radius) - 1  do
    begin
     SqrYDist := Sqr(Y - (Radius - 1));
     XStart := Sqr(Radius) - SqrYDist;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     for X := Round((Radius - 1) - XStart) to Round((Width - 1) - (Radius - 1) + XStart) do
      begin
       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (Width - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (Width - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist < SqrRadMinusBorder
        then CombColor := ButtonColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombinePixel(BorderColor, ButtonColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);
       EMMS;
      end;
    end;

   for Y := Round(Radius) to Height - 1 - Round(Radius) do
    begin
     ScnLne[0] := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       // check whether position is a border
       if (Y < BorderWidth - 1) or (Y > Height - 1 - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether position is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else

       // check whether position is a lower half border
       if (Y > Height - 1 - BorderWidth) then
        begin
         Temp := Y - (Height - 1 - BorderWidth);
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, ButtonColor, Round(Temp * $FF));
        end
       else CombColor := ButtonColor;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;
  end;
end;

procedure TGuiMediaButton.RenderGlyph(PixelMap: TGuiCustomPixelMap);
var
  Offset : Integer;
  IntOff : Integer;
  Wdth   : Integer;
  Y      : Integer;
const
  CBorderColor : TPixel32 = (ARGB : $AF000000);
begin
 with PixelMap do
  begin
   Offset := Round(FBorderWidth + FGlyphOffset);
   case FButtonState of
    mbsPlay :
     begin
       RenderTriangle(PixelMap, Rect(Offset, Offset, Width - Offset,
         Height - Offset), clLime);
(*
       Wdth := Round(0.5 * FOutlineWidth);
       RenderTriangle(PixelMap, Rect(Offset - Wdth, Offset - Wdth,
         Width - Offset + Wdth,
         Height - Offset + Wdth), clBlack);
       RenderTriangle(PixelMap, Rect(Offset + Wdth, Offset + Wdth,
         Width - Offset - Wdth,
         Height - Offset - Wdth), clLime);
*)
     end;
    mbsPause :
     begin
      IntOff := Trunc(FOutlineWidth) div 2;
      Wdth := Round(0.5 * (Width - 3 * Offset));
      if Wdth < FGlyphOffset then Wdth := FGlyphOffset;

      FillRect(Offset, Offset, Offset + Wdth,
        Height - Offset, pxGray32);

      FillRect(Width - Offset - Wdth, Offset,
        Width - Offset, Height - Offset, pxGray32);

      // draw frame
      for Y := 0 to Trunc(FOutlineWidth) - 1 do
       begin
        BlendPixelLine(CBorderColor, @ScanLine[Offset - IntOff + Y]^[Offset - IntOff],
          Wdth + Trunc(FOutlineWidth));
        BlendPixelLine(CBorderColor, @ScanLine[Height - Offset - IntOff + Y]^[Offset - IntOff],
          Wdth + Trunc(FOutlineWidth));
       end;
      for Y := Offset - IntOff + Trunc(FOutlineWidth) to Height - Offset - IntOff - 1 do
       begin
        BlendPixelLine(CBorderColor, @ScanLine[Y]^[Offset - IntOff],
          Trunc(FOutlineWidth));
        BlendPixelLine(CBorderColor, @ScanLine[Y]^[Offset + Wdth - IntOff],
          Trunc(FOutlineWidth));
       end;

      for Y := 0 to Trunc(FOutlineWidth) - 1 do
       begin
        BlendPixelLine(CBorderColor, @ScanLine[Offset - IntOff + Y]^[Width - 1 - Offset - Wdth - IntOff],
          Wdth + Trunc(FOutlineWidth));
        BlendPixelLine(CBorderColor, @ScanLine[Height - Offset - IntOff + Y]^[Width - 1 - Offset - Wdth - IntOff],
          Wdth + Trunc(FOutlineWidth));
       end;
      for Y := Offset - IntOff + Trunc(FOutlineWidth) to Height - Offset - IntOff - 1 do
       begin
        BlendPixelLine(CBorderColor, @ScanLine[Y]^[Width - 1 - (Offset + IntOff)],
          Trunc(FOutlineWidth));
        BlendPixelLine(CBorderColor, @ScanLine[Y]^[Width - 1 - (Offset + Wdth + IntOff)],
          Trunc(FOutlineWidth));
       end;
     end;
    mbsStop : FillRect(Offset, Offset, Width  - Offset, Height - Offset, pxBlack32);

    mbsFastForward :
     begin
      RenderTriangle(PixelMap, Rect(Offset, Offset, (Width - Offset) div 2,
        Height - Offset), clYellow);
      RenderTriangle(PixelMap, Rect((Width + Offset) div 2, Offset,
        Width - Offset, Height - Offset), clYellow);
     end;

    mbsFastBackward :
     begin
      RenderTriangle(PixelMap, Rect(Offset, Offset, (Width - Offset) div 2,
        Height - Offset), clYellow, True);
      RenderTriangle(PixelMap, Rect((Width + Offset) div 2, Offset,
        Width - Offset, Height - Offset), clYellow, True);
     end;

    mbsNext :
     begin
      RenderTriangle(PixelMap, Rect(Offset, Offset, (Width - Offset) div 2,
        Height - Offset), clBlue);
      FillRect(Round(0.5 * (Width + Offset)), Offset,
        Width - Offset, Height - Offset, pxBlue32);
     end;

    mbsPrevious :
     begin
      FillRect(Offset, Offset, (Width - Offset) div 2,
        Height - Offset, pxBlue32);
      RenderTriangle(PixelMap, Rect(Round(0.5 * (Width + Offset)), Offset,
        Width - Offset, Height - Offset), clBlue, True);
     end;

    mbsRecord :
     begin
      with TGuiPixelFilledCircle.Create do
       try
        Color := clRed;
        GeometricShape.CenterX := ConvertToFixed24Dot8(0.5 * Width);
        GeometricShape.CenterY := ConvertToFixed24Dot8(0.5 * Height);
        GeometricShape.Radius := ConvertToFixed24Dot8(0.5 * (Min(Width, Height) - FOutlineWidth) - Offset);
        Alpha := $FF;
        Draw(PixelMap);
       finally
        Free;
       end;
      with TGuiPixelFrameCircle.Create do
       try
        Color := clBlack;
        Alpha := $AF;
        LineWidth := ConvertToFixed24Dot8(FOutlineWidth);
        GeometricShape.CenterX := ConvertToFixed24Dot8(0.5 * Width);
        GeometricShape.CenterY := ConvertToFixed24Dot8(0.5 * Height);
        GeometricShape.Radius := ConvertToFixed24Dot8(0.5 * Min(Width, Height) - Offset);
        Draw(PixelMap);
       finally
        Free;
       end;
     end;
   end;

  end;
end;

procedure TGuiMediaButton.RenderTriangle(PixelMap: TGuiCustomPixelMap;
  Rect: TRect; Color: TColor; Reverse: Boolean = False);
var
  X, Y   : Integer;
  XFixed : TFixed24Dot8;
  XAdv   : TFixed24Dot8;
  ScnLn  : array [0..1] of PPixel32Array;
  Clr    : TPixel32;
  BrdClr : TPixel32;
begin
 with PixelMap do
  begin
   if (Rect.Right - Rect.Left) < 0 then Exit;

   // set color
   Clr := ConvertColor(Color);
   Clr.A := $FF;
   BrdClr.ARGB := $AF000000;

   // set start and advance
   XFixed := CFixed24Dot8Half;
   XAdv := ConvertToFixed24Dot8(2 * (Rect.Right - Rect.Left) / (Rect.Bottom - Rect.Top));
   if Reverse then
    for Y := 0 to ((Rect.Bottom - Rect.Top) div 2) do
     begin
      ScnLn[0] := ScanLine[Rect.Top + Y];
      ScnLn[1] := ScanLine[Rect.Bottom - Y];
      X := FixedFloor(XFixed);
      BlendPixelLine(Clr, @ScnLn[0]^[Rect.Right - X], X);
      BlendPixelInplace(ApplyAlpha(Clr, XFixed.Frac), ScnLn[0]^[Rect.Right - X - 1]);
      if ScnLn[0] <> ScnLn[1] then
       begin
        BlendPixelLine(Clr, @ScnLn[1]^[Rect.Right - X], X);
        BlendPixelInplace(ApplyAlpha(Clr, XFixed.Frac), ScnLn[1]^[Rect.Right - X - 1]);
       end else Break;
      XFixed := FixedAdd(XFixed, XAdv);
     end
   else
    for Y := 0 to ((Rect.Bottom - Rect.Top) div 2) do
     begin
      ScnLn[0] := ScanLine[Rect.Top + Y];
      ScnLn[1] := ScanLine[Rect.Bottom - Y];
      X := FixedFloor(XFixed);
      BlendPixelLine(Clr, @ScnLn[0]^[Rect.Left], X);
      BlendPixelInplace(ApplyAlpha(Clr, XFixed.Frac), ScnLn[0]^[Rect.Left + X]);
      if ScnLn[0] <> ScnLn[1] then
       begin
        BlendPixelLine(Clr, @ScnLn[1]^[Rect.Left], X);
        BlendPixelInplace(ApplyAlpha(Clr, XFixed.Frac), ScnLn[1]^[Rect.Left + X]);
       end else Break;

      XFixed := FixedAdd(XFixed, XAdv);
     end;
   EMMS;
  end;
end;

procedure TGuiMediaButton.Resize;
begin
 inherited;

 CalculateAbsoluteGlyphOffset;

 if Assigned(FBuffer)
  then FBuffer.SetSize(Self.Width, Self.Height);

 if Assigned(FBackBuffer)
  then FBackBuffer.SetSize(Self.Width, Self.Height);

 BackBufferChanged;
end;

procedure TGuiMediaButton.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiMediaButton.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiMediaButton.TransparentChanged;
begin
 BufferChanged;
end;

end.
