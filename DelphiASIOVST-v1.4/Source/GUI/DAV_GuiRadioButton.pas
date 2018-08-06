unit DAV_GuiRadioButton;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, StdCtrls, ExtCtrls,
  DAV_FixedPoint, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiVector,
  DAV_GuiFixedPoint, DAV_GuiFont, DAV_GuiShadow;

type
  TGuiControlsRadioButton = class(TRadioButton)
  private
    FRadioButtonRadius : Integer;
    FCanvas            : TCanvas;
    FUpdateBuffer      : Boolean;
    FUpdateBackBuffer  : Boolean;
    FTransparent       : Boolean;
    FFocused           : Boolean;
    FNative            : Boolean;
    FMouseIsDown       : Boolean;
    FMouseInControl    : Boolean;
    FFlatChecked       : Boolean;
    FTextChanged       : Boolean;
    FCircleChanged     : Boolean;
    FGroupIndex        : Integer;
    FFocusedColor      : TColor;
    FDownColor         : TColor;
    FDotColor          : TColor;
    FBorderColor       : TColor;
    FBackgroundColor   : TColor;
    FOnPaint           : TNotifyEvent;
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetShadow(const Value: TGUIShadow);
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetNative(const Value: Boolean);

  protected
    FBuffer     : TGuiCustomPixelMap;
    FBackBuffer : TGuiCustomPixelMap;
    FGuiFont    : TGuiOversampledGDIFont;

    procedure CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF}); message CM_PARENTCOLORCHANGED;
    procedure CMTextChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TWmNoParams{$ENDIF}); message CM_TEXTCHANGED;

    {$IFDEF FPC}
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    {$ELSE}
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}

    procedure DoEnter; override;
    procedure DoExit; override;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetBiDiMode(Value: TBiDiMode); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure BufferChanged(TextChanged: Boolean = True; CircleChanged: Boolean = True); virtual;
    procedure BackBufferChanged; virtual;
    procedure CalculateRadioButtonRadius; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure RenderCircle(Buffer: TGuiCustomPixelMap); virtual;
    procedure RenderText(Buffer: TGuiCustomPixelMap); virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;

    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseEnter;
    procedure MouseLeave;
  published
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Color default clBtnFace;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default clBtnHighlight;
    property ColorDown: TColor index 1 read FDownColor write SetColors default clBtnHighlight;
    property ColorDot: TColor index 2 read FDotColor write SetColors default clWindowText;
    property ColorBorder: TColor index 3 read FBorderColor write SetColors default clWindowText;
    property ColorBackground: TColor index 4 read FBackgroundColor write SetColors default clWindow;
    property Native: Boolean read FNative write SetNative default True;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    property Action;
    property Anchors;
    property AutoSize;
    property Caption;
    property Checked;
    property Enabled;
    property Font;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEndDock;
    property OnStartDock;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
   {$IFDEF MFC_COMPILER_4_UP}
    property BiDiMode write SetBidiMode;
   {$ENDIF}
  end;

implementation

uses
  ActnList, Math, DAV_Common, DAV_Complex, DAV_GuiBlend, DAV_Approximations;

constructor TGuiControlsRadioButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 ControlStyle  := ControlStyle + [csOpaque, csReplicatable];

 // create control canvas
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;

 // create buffers (& set size)
 FBuffer     := TGuiPixelMapMemory.Create;
 FBackBuffer := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(Width, Height);
 FBackBuffer.SetSize(Width, Height);

 // create font
 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 ParentColor       := True;
 ParentFont        := True;
 FFocusedColor     := clBtnHighlight;
 FDownColor        := clBtnHighlight;
 FDotColor         := clWindowText;
 FBorderColor      := clWindowText;
 FBackgroundColor  := clWindow;
 FNative           := False;
 FFlatChecked      := False;
 FGroupIndex       := 0;
 Enabled           := True;
 Visible           := True;
 FTextChanged      := True;
 FCircleChanged    := True;

 CalculateRadioButtonRadius;
end;

procedure TGuiControlsRadioButton.CreateParams(var Params: TCreateParams);
begin
 inherited;

 if not FNative then
  with Params do Style := (Style and not $1F) or BS_OWNERDRAW;
end;

destructor TGuiControlsRadioButton.Destroy;
begin
 FreeAndNil(FCanvas);
 FreeAndNil(FGuiFont);

 // dispose buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);

 inherited;
end;

procedure TGuiControlsRadioButton.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiControlsRadioButton.Resize;
begin
 inherited;
 CalculateRadioButtonRadius;

 // resize and update back buffer
 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   BackBufferChanged;
  end;

 // resize and update back buffer
 if Assigned(FBuffer) then
  begin
   FBuffer.SetSize(Width, Height);
   BufferChanged;
  end;
end;

procedure TGuiControlsRadioButton.CalculateRadioButtonRadius;
begin
 FRadioButtonRadius := Round(Abs(Font.Height * Font.PixelsPerInch / 144));
 if 2 * FRadioButtonRadius > Height - 2
  then FRadioButtonRadius := Round(0.5 * (Height - 2));
 if 2 * FRadioButtonRadius > Width - 2
  then FRadioButtonRadius := Round(0.5 * (Width - 2));
end;

procedure TGuiControlsRadioButton.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TGuiControlsRadioButton.BufferChanged(TextChanged: Boolean = True; CircleChanged: Boolean = True);
begin
 if TextChanged then FTextChanged := True;
 if CircleChanged then FCircleChanged := True;
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TGuiControlsRadioButton.CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;

 if not FTransparent
  then BackBufferChanged;
end;

procedure TGuiControlsRadioButton.CMMouseEnter(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 MouseEnter;
end;

procedure TGuiControlsRadioButton.CMMouseLeave(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 MouseLeave;
end;

procedure TGuiControlsRadioButton.CMEnabledChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;

 if not Enabled and FMouseInControl
  then FMouseIsDown := False;

 BufferChanged;
end;

procedure TGuiControlsRadioButton.CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FGuiFont.Font.Assign(Font);
end;

procedure TGuiControlsRadioButton.CMTextChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TWmNoParams{$ENDIF});
begin
 inherited;

 if not (csLoading in ComponentState)
   then BufferChanged(True, False);
end;

procedure TGuiControlsRadioButton.MouseEnter;
begin
 if Enabled and not FMouseInControl then
  begin
   FMouseInControl := True;
   if not FNative
    then BufferChanged(False);
  end;
end;

procedure TGuiControlsRadioButton.MouseLeave;
begin
 if Enabled and FMouseInControl and not FMouseIsDown then
  begin
   FMouseInControl := False;

   if not FNative
    then BufferChanged(False);
  end;
end;

procedure TGuiControlsRadioButton.WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF});
begin
 inherited;

 if Enabled then FFocused := True;
end;

{$IFDEF FPC}
procedure TGuiControlsRadioButton.WMKillFocus(var Message: TLMKillFocus);
{$ELSE}
procedure TGuiControlsRadioButton.WMKillFocus(var Message: TWMKillFocus);
{$ENDIF}
begin
 inherited;

 if Enabled then
  begin
   FMouseInControl := False;
   FFocused        := False;
  end;
end;

procedure TGuiControlsRadioButton.CMParentColorChanged(var Message: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF});
begin
 inherited;

 if ParentColor and not FTransparent
  then BackBufferChanged;
end;

procedure TGuiControlsRadioButton.DoEnter;
begin
 inherited DoEnter;

 if FMouseIsDown and FMouseInControl
  then Checked := True;
 FFocused := True;
 BufferChanged(False);
end;

procedure TGuiControlsRadioButton.DoExit;
begin
 inherited DoExit;

 FFocused := False;
 BufferChanged(False);
end;

procedure TGuiControlsRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   SetFocus;
   FMouseIsDown := True;
   inherited MouseDown(Button, Shift, X, Y);
   BufferChanged(False);
  end;
end;

procedure TGuiControlsRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   FMouseIsDown := false;
   if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) and not Checked
    then Checked := True;

   inherited MouseUp(Button, Shift, X, Y);
   BufferChanged(False);
  end;
end;

procedure TGuiControlsRadioButton.Paint;
begin
 inherited;

 if FUpdateBackBuffer
  then UpdateBackBuffer;

 if FUpdateBuffer
  then UpdateBuffer;

 if Assigned(FOnPaint)
  then FOnPaint(Self);

 if Assigned(FBuffer)
  then FBuffer.PaintTo(Canvas);
end;

procedure TGuiControlsRadioButton.PaintWindow(DC: HDC);
begin
 FCanvas.Lock;
 try
  FCanvas.Handle := DC;
  try
   {$IFNDEF FPC}
   TControlCanvas(FCanvas).UpdateTextFlags;
   {$ENDIF}
   Paint;
  finally
   FCanvas.Handle := 0;
  end;
 finally
  FCanvas.Unlock;
 end;
end;

(*
procedure TGuiControlsRadioButton.DrawRadio;
begin
 if Focused or FMouseInControl then
  if not FMouseIsDown then
   begin
    Brush.Color   := FFocusedColor;
    Pen.Color     := FBorderColor;
   end
  else
   begin
    Brush.Color   := FDownColor;
    Pen.Color     := FBorderColor;
   end
  else
   begin
    Brush.Color   := Color;
    Pen.Color     := FBorderColor;
   end;

  format := DT_WORDBREAK;
  if Alignment=taRightJustify then
   begin
    TextBounds := Rect(ClientRect.Left + 18, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
    format := format or DT_LEFT;
   end
  else if Alignment=taLeftJustify then
   begin
    TextBounds := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 16, ClientRect.Bottom - 1);
    format := format or DT_RIGHT;
   end;

  Brush.Style := bsClear;
  Font := Self.Font;
  if not Enabled then
   begin
    OffsetRect(TextBounds, 1, 1);
    Font.Color := FDisabledColor;
    DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, format);
    OffsetRect(TextBounds, -1, -1);
    Font.Color := FBackgroundColor;
    DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, format);
   end else
end;
*)

procedure TGuiControlsRadioButton.FontChangedHandler(Sender: TObject);
begin
 CalculateRadioButtonRadius;
 BufferChanged;
end;

function TGuiControlsRadioButton.GetChecked: Boolean;
begin
 if FNative
  then Result := FFlatChecked
  else Result := inherited GetChecked;
end;

{$IFDEF FPC}
procedure TGuiControlsRadioButton.WMSize(var Message: TLMSize);
{$ELSE}
procedure TGuiControlsRadioButton.WMSize(var Message: TWMSize);
{$ENDIF}
begin
 inherited;

 if FTransparent and (not (csLoading in ComponentState))
  then BackBufferChanged;
end;

procedure TGuiControlsRadioButton.WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF});
begin
 inherited;

 if FTransparent and (not (csLoading in ComponentState))
  then BackBufferChanged;
end;

procedure TGuiControlsRadioButton.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
begin
 if FNative
  then inherited
  else
   begin
    ControlState := ControlState + [csCustomPaint];
    inherited;
    ControlState := ControlState - [csCustomPaint];
   end;
end;

procedure TGuiControlsRadioButton.RenderCircle(Buffer: TGuiCustomPixelMap);
var
  Y, X1, X2         : Integer;
  ScnLne            : PPixel32Array;
  CombColor         : TPixel32;
  BorderColor       : TPixel32;
  DotColor          : TPixel32;
  BackColor         : TPixel32;
  DrawDot           : Boolean;
  Radius            : TFixed24Dot8;
  InnerOffset       : TFixed24Dot8;
  BorderWidth       : TFixed24Dot8;
  XStart            : TFixed24Dot8;
  Scale             : TFixed24Dot8;
  OffsetX           : TFixed24Dot8;
  OffsetY           : TFixed24Dot8;
  SqrYDist          : TFixed24Dot8;
  SqrDist           : TFixed24Dot8;
  SqrRadMinusOne    : TFixed24Dot8;
  SqrRadMinusBorder : TFixed24Dot8;
  RadMinusBorderOne : TFixed24Dot8;
  SqrRadMinusInner  : TFixed24Dot8;
  RadMinusInnerOne  : TFixed24Dot8;
  Temp              : TFixed24Dot8;
begin
 with Buffer do
  begin
   // assign local colors
   BorderColor := ConvertColor(FBorderColor);
   DotColor := ConvertColor(FDotColor);
   DrawDot := Checked or (FMouseIsDown and Enabled);

   if FMouseIsDown then
    begin
     BackColor := ConvertColor(FDownColor);
     if not Checked
      then DotColor.A := $7F;
    end else
   if FMouseInControl or Focused
    then BackColor := ConvertColor(FFocusedColor)
    else BackColor := ConvertColor(FBackgroundColor);

   BorderWidth := ConvertToFixed24Dot8(Max(2.5, 1 + 0.15 * FRadioButtonRadius));

   // draw circle
   Radius := ConvertToFixed24Dot8(FRadioButtonRadius);
   if Radius.Fixed <= 0 then Exit;

   InnerOffset := FixedAdd(BorderWidth, BorderWidth);

   Temp := FixedSub(Radius, CFixed24Dot8One);
   if Temp.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(Temp);

   Temp := FixedSub(Radius, BorderWidth);
   if Temp.Fixed < 0
    then SqrRadMinusBorder.Fixed := 0
    else SqrRadMinusBorder := FixedSqr(Temp);

   Temp := FixedAdd(Radius, CFixed24Dot8One);
   RadMinusBorderOne := FixedSub(Temp, BorderWidth);
   if RadMinusBorderOne.Fixed < 0
    then RadMinusBorderOne.Fixed := 0;

   Temp := FixedSub(Radius, InnerOffset);
   if Temp.Fixed < 0
    then SqrRadMinusInner.Fixed := 0
    else SqrRadMinusInner := FixedSqr(Temp);

   RadMinusInnerOne := FixedAdd(FixedSub(Radius, InnerOffset), CFixed24Dot8One);
   if RadMinusInnerOne.Fixed < 0
    then RadMinusInnerOne.Fixed := 0;

   {$IFDEF FPC}
   OffsetX := FixedAdd(Radius, CFixed24Dot8Half);
   {$ELSE}
   case Alignment of
    taLeftJustify  : OffsetX := FixedSub(FixedSub(ConvertToFixed24Dot8(Width), Radius), CFixed24Dot8Half);
    taRightJustify : OffsetX := FixedAdd(Radius, CFixed24Dot8Half);
    else raise Exception.Create('Unknown justify');
   end;
   {$ENDIF}
   OffsetY := FixedMul(ConvertToFixed24Dot8(Integer(Height - 1)), CFixed24Dot8Half);

   for Y := FixedRound(FixedSub(OffsetY, Radius)) to FixedRound(FixedAdd(OffsetY, Radius)) do
    begin
     // calculate squared vertical distance
     SqrYDist := FixedSqr(FixedSub(ConvertToFixed24Dot8(Y), OffsetY));

     XStart := FixedSub(FixedSqr(Radius), SqrYDist);
     if XStart.Fixed < 0
      then Continue
      else XStart := FixedSub(FixedSqrt(XStart), ConvertToFixed24Dot8(0.4999999));

     ScnLne := Scanline[Y];
     X1 := FixedRound(FixedSub(OffsetX, XStart));
     X2 := FixedRound(FixedAdd(OffsetX, XStart));
     while X1 < X2 do
      begin
       // calculate squared distance
       SqrDist := FixedAdd(FixedSqr(FixedSub(ConvertToFixed24Dot8(X1), OffsetX)), SqrYDist);

       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         if DrawDot then
          begin
           if (SqrDist.Fixed <= SqrRadMinusInner.Fixed) then
            begin
             CombColor := BlendPixel(DotColor, BackColor);
             if not Enabled then CombColor.A := CombColor.A shr 1;
             BlendPixelLine(CombColor, @ScnLne[X1], X2 - X1 + 1);
             Break;
            end else
           if (SqrDist.Fixed <= FixedSqr(RadMinusInnerOne).Fixed) then
            begin
             Scale := FixedSub(RadMinusInnerOne, FixedSqrt(SqrDist));
             CombColor := BlendPixel(DotColor, BackColor);
             Assert(Scale.Fixed >= 0);
             Assert(Scale.Fixed < $FF);
             CombColor := CombinePixel(CombColor, BackColor, Scale.Fixed);
             if not Enabled then CombColor.A := CombColor.A shr 1;

             BlendPixelInplace(CombColor, ScnLne[X1]);
             BlendPixelInplace(CombColor, ScnLne[X2]);
             Inc(X1);
             Dec(X2);
            end
           else
            begin
             CombColor := BackColor;
             if not Enabled then CombColor.A := CombColor.A shr 1;
             BlendPixelInplace(CombColor, ScnLne[X1]);
             BlendPixelInplace(CombColor, ScnLne[X2]);
             Inc(X1);
             Dec(X2);
            end;
          end
         else
          begin
           CombColor := BackColor;
           if not Enabled then CombColor.A := CombColor.A shr 1;
           BlendPixelLine(CombColor, @ScnLne[X1], X2 - X1 + 1);
           Break;
          end;
        end
       else
        begin
         if SqrDist.Fixed <= FixedSqr(RadMinusBorderOne).Fixed then
          begin
           Scale := FixedSub(RadMinusBorderOne, FixedSqrt(SqrDist));
           Assert(Scale.Fixed >= 0);
           Assert(Scale.Fixed < $FF);
           CombColor := CombinePixel(BackColor, BorderColor, Scale.Fixed);
          end else
         if SqrDist.Fixed < SqrRadMinusOne.Fixed
          then CombColor := BorderColor
          else
           begin
            CombColor := BorderColor;
            Scale := FixedSub(Radius, FixedSqrt(SqrDist));
            CombinePixelInplace(BackColor, CombColor, 0);
            CombColor.A := Scale.Fixed;
           end;

         if not Enabled then CombColor.A := CombColor.A shr 1;
         BlendPixelInplace(CombColor, ScnLne[X1]);
         BlendPixelInplace(CombColor, ScnLne[X2]);
         Inc(X1);
         Dec(X2);
        end;
      end;
    end;
  end;
end;

procedure TGuiControlsRadioButton.RenderText(Buffer: TGuiCustomPixelMap);
var
  TextSize    : TSize;
  OldAlpha    : Byte;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtent(Caption);
   {$IFDEF FPC}
   TextSize.cx := 2 * FRadioButtonRadius + 3;
   {$ELSE}
   case Alignment of
    taLeftJustify  : TextSize.cx := Width - TextSize.cx - 2 * FRadioButtonRadius - 3;
    taRightJustify : TextSize.cx := 2 * FRadioButtonRadius + 3;
   end;
   {$ENDIF}
   TextSize.cy := (Height - TextSize.cy) div 2;
   if not Enabled then
    begin
     OldAlpha := FGuiFont.Alpha;
     FGuiFont.Alpha := FGuiFont.Alpha shr 1;
     FGuiFont.TextOut(Caption, FBuffer, TextSize.cx, TextSize.cy);
     FGuiFont.Alpha := OldAlpha;
    end
   else FGuiFont.TextOut(Caption, FBuffer, TextSize.cx, TextSize.cy);
  end;
end;

procedure TGuiControlsRadioButton.SetColors(Index: Integer; Value: TColor);
begin
 case Index of
  0: FFocusedColor    := Value;
  1: FDownColor       := Value;
  2: FDotColor        := Value;
  3: FBorderColor     := Value;
  4: FBackgroundColor := Value;
 end;
 BufferChanged(False);
end;

procedure TGuiControlsRadioButton.SetNative(const Value: Boolean);
var
  OldMouseInControl : Boolean;
begin
 if FNative <> Value then
  begin
   OldMouseInControl := FMouseInControl;
   FNative := Value;
   {$IFDEF FPC}
   RecreateWnd(Self);
   {$ELSE}
   RecreateWnd;
   {$ENDIF}
   FMouseInControl := OldMouseInControl;
  end;
end;

procedure TGuiControlsRadioButton.SetOversampling(
  const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TGuiControlsRadioButton.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TGuiControlsRadioButton.SetBiDiMode(Value: TBiDiMode);
begin
 inherited;

 {$IFNDEF FPC}
 if BidiMode = bdRightToLeft
  then Alignment := taLeftJustify
  else Alignment := taRightJustify;
 {$ENDIF}
end;

procedure TGuiControlsRadioButton.SetChecked(Value: Boolean);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
   if Parent <> nil then
    with Parent do
     for I := 0 to ControlCount - 1 do
      begin
       Sibling := Controls[I];
       if (Sibling <> Self) and (Sibling is TGuiControlsRadioButton) then
        with TGuiControlsRadioButton(Sibling) do
         if GroupIndex = Self.GroupIndex then
          begin
           if Assigned(Action) and (Action is TCustomAction) and TCustomAction(Action).AutoCheck
            then TCustomAction(Action).Checked := False;
           SetChecked(False);
          end;
      end;
  end;

begin
 if not FNative then
  begin
   inherited SetChecked(Value);
   BufferChanged(False, True);
  end else
 if FFlatChecked <> Value then
  begin
   FFlatChecked := Value;
   TabStop := Value;
   {$IFNDEF FPC}
   if HandleAllocated
    then SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);
   {$ENDIF}

   if Value then
    begin
     TurnSiblingsOff;
     inherited Changed;
     if not ClicksDisabled then Click;
    end;
   FCircleChanged := True;
   BufferChanged(False);
  end;
end;

procedure TGuiControlsRadioButton.SetTransparent(const Value: Boolean);
begin
 FTransparent := Value;
 if not (csLoading in ComponentState)
  then Invalidate;
end;

function TGuiControlsRadioButton.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TGuiControlsRadioButton.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TGuiControlsRadioButton.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 FUpdateBackBuffer := False;

 {$IFNDEF FPC}
 if FTransparent then FBackBuffer.CopyParentImage(Self) else
 {$ENDIF}
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TGuiControlsRadioButton.UpdateBuffer;
var
  y       : Integer;
  SrcPtr  : PPixel32Array;
  DestPtr : PPixel32Array;
begin
 FUpdateBuffer := False;

 inherited;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 if FCircleChanged and FTextChanged then
  begin
   FCircleChanged := False;
   FTextChanged := False;

   // copy entire back buffer to buffer
   Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
     FBuffer.Width * SizeOf(TPixel32));

   RenderCircle(FBuffer);
   RenderText(FBuffer);
   Exit;
  end;

 // check whether only the circle changed;
 if FCircleChanged then
  begin
   FCircleChanged := False;

   // copy circle part of the back buffer to buffer
   SrcPtr := FBackBuffer.DataPointer;
   DestPtr := FBuffer.DataPointer;
   for y := 0 to FBuffer.Height - 1 do
    begin
     Move(SrcPtr^, DestPtr^, (2 * FRadioButtonRadius + 1) * SizeOf(TPixel32));
     SrcPtr := @SrcPtr^[FBuffer.Width];
     DestPtr := @DestPtr^[FBuffer.Width];
    end;

   // actually render circle
   RenderCircle(FBuffer);
   Exit;
  end;

 // check whether only the text changed;
 if FTextChanged then
  begin
   FTextChanged := False;

   // copy text part of the back buffer to buffer
   SrcPtr := @FBackBuffer.DataPointer[(2 * FRadioButtonRadius + 1)];
   DestPtr := @FBuffer.DataPointer[(2 * FRadioButtonRadius + 1)];
   for y := 0 to FBuffer.Height - 1 do
    begin
     Move(SrcPtr^, DestPtr^, (FBuffer.Width - (2 * FRadioButtonRadius + 1)) * SizeOf(TPixel32));
     SrcPtr := @SrcPtr^[FBuffer.Width];
     DestPtr := @DestPtr^[FBuffer.Width];
    end;

   // actually render text
   RenderText(FBuffer);
  end;
end;

end.
