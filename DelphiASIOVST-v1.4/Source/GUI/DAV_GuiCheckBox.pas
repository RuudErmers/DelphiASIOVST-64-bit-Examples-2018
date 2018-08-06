unit DAV_GuiCheckBox;

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
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiFixedPoint,
  DAV_GuiFont, DAV_GuiShadow;

type
  TGuiControlsCheckBox = class(TCheckBox)
  private
    FCheckBoxSize     : Integer;
    FCanvas           : TCanvas;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;
    FTransparent      : Boolean;
    FFocused          : Boolean;
    FNative           : Boolean;
    FMouseIsDown      : Boolean;
    FMouseInControl   : Boolean;
    FTextChanged      : Boolean;
    FBoxChanged       : Boolean;
    FGroupIndex       : Integer;
    FFocusedColor     : TColor;
    FDownColor        : TColor;
    FDotColor         : TColor;
    FBorderColor      : TColor;
    FBackgroundColor  : TColor;
    FOnPaint          : TNotifyEvent;
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
    procedure SetBiDiMode(Value: TBiDiMode); override;
    procedure SetChecked(Value: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure BackBufferChanged; virtual;
    procedure BufferChanged(TextChanged: Boolean = True; BoxChanged: Boolean = True); virtual;
    procedure CalculateCheckBoxRadius; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure NativeChanged; virtual;
    procedure RenderBox(Buffer: TGuiCustomPixelMap); virtual;
    procedure RenderText(Buffer: TGuiCustomPixelMap); virtual;
    procedure TransparentChanged; virtual;
    procedure UpdateBackBuffer; virtual;
    procedure UpdateBuffer; virtual;

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

constructor TGuiControlsCheckBox.Create(AOwner: TComponent);
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

 // initialize variables
 ParentColor       := True;
 ParentFont        := True;
 FFocusedColor     := clBtnHighlight;
 FDownColor        := clBtnHighlight;
 FDotColor         := clWindowText;
 FBorderColor      := clWindowText;
 FBackgroundColor  := clWindow;
 FNative           := False;
 FGroupIndex       := 0;
 Enabled           := True;
 Visible           := True;
 FTextChanged      := True;
 FBoxChanged       := True;

 CalculateCheckBoxRadius;
end;

procedure TGuiControlsCheckBox.CreateParams(var Params: TCreateParams);
begin
 inherited;

 if not FNative then
  with Params do Style := (Style and not $1F) or BS_OWNERDRAW;
end;

destructor TGuiControlsCheckBox.Destroy;
begin
 FreeAndNil(FCanvas);

 // create buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);

 FreeAndNil(FGuiFont);

 inherited;
end;

procedure TGuiControlsCheckBox.Loaded;
begin
 inherited;
 Resize;
end;

procedure TGuiControlsCheckBox.Resize;
begin
 inherited;
 CalculateCheckBoxRadius;

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

procedure TGuiControlsCheckBox.CalculateCheckBoxRadius;
begin
 FCheckBoxSize := Round(Abs(Font.Height * Font.PixelsPerInch / 72));
 if FCheckBoxSize > Height - 2
  then FCheckBoxSize := Height - 2;
 if FCheckBoxSize > Width - 2
  then FCheckBoxSize := Width - 2;
end;

procedure TGuiControlsCheckBox.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TGuiControlsCheckBox.BufferChanged(TextChanged: Boolean = True; BoxChanged: Boolean = True);
begin
 if TextChanged then FTextChanged := True;
 if BoxChanged then FBoxChanged := True;
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TGuiControlsCheckBox.CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;

 if not FTransparent
  then BackBufferChanged;
end;

procedure TGuiControlsCheckBox.CMMouseEnter(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 MouseEnter;
end;

procedure TGuiControlsCheckBox.CMMouseLeave(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 MouseLeave;
end;

procedure TGuiControlsCheckBox.CMEnabledChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;

 if not Enabled and FMouseInControl
  then FMouseIsDown := False;

 BufferChanged;
end;

procedure TGuiControlsCheckBox.CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FGuiFont.Font.Assign(Font);
end;

procedure TGuiControlsCheckBox.CMTextChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TWmNoParams{$ENDIF});
begin
 inherited;

 if not (csLoading in ComponentState)
   then BufferChanged(True, False);
end;

procedure TGuiControlsCheckBox.MouseEnter;
begin
 if Enabled and not FMouseInControl then
  begin
   FMouseInControl := True;

   if not FNative
    then BufferChanged(False);
  end;
end;

procedure TGuiControlsCheckBox.MouseLeave;
begin
 if Enabled and FMouseInControl and not FMouseIsDown then
  begin
   FMouseInControl := False;

   if not FNative
    then BufferChanged(False);
  end;
end;

procedure TGuiControlsCheckBox.WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF});
begin
 inherited;

 if Enabled then FFocused := True;
end;

{$IFDEF FPC}
procedure TGuiControlsCheckBox.WMKillFocus(var Message: TLMKillFocus);
{$ELSE}
procedure TGuiControlsCheckBox.WMKillFocus(var Message: TWMKillFocus);
{$ENDIF}
begin
 inherited;

 if Enabled then
  begin
   FMouseInControl := False;
   FFocused        := False;
  end;
end;

procedure TGuiControlsCheckBox.CMParentColorChanged(var Message: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF});
begin
 inherited;

 if ParentColor and not FTransparent
  then BackBufferChanged;
end;

procedure TGuiControlsCheckBox.DoEnter;
begin
 inherited DoEnter;

 if FMouseIsDown and FMouseInControl
  then Checked := True;
 FFocused := True;
 BufferChanged(False);
end;

procedure TGuiControlsCheckBox.DoExit;
begin
 inherited DoExit;

 FFocused := False;
 BufferChanged(False);
end;

procedure TGuiControlsCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   SetFocus;
   FMouseIsDown := True;
   inherited MouseDown(Button, Shift, X, Y);
   BufferChanged(False);
  end;
end;

procedure TGuiControlsCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbLeft) and Enabled then
  begin
   FMouseIsDown := False;
   inherited MouseUp(Button, Shift, X, Y);
   BufferChanged(False);
  end;
end;

procedure TGuiControlsCheckBox.Paint;
begin
 inherited;

 if not FNative then
  begin
   if FUpdateBackBuffer
    then UpdateBackBuffer;

   if FUpdateBuffer
    then UpdateBuffer;

   if Assigned(FOnPaint)
    then FOnPaint(Self);

   if Assigned(FBuffer)
    then FBuffer.PaintTo(Canvas);
  end;
end;

procedure TGuiControlsCheckBox.PaintWindow(DC: HDC);
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
procedure TGuiControlsCheckBox.DrawRadio;
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

procedure TGuiControlsCheckBox.FontChangedHandler(Sender: TObject);
begin
 CalculateCheckBoxRadius;
 BufferChanged;
end;

{$IFDEF FPC}
procedure TGuiControlsCheckBox.WMSize(var Message: TLMSize);
{$ELSE}
procedure TGuiControlsCheckBox.WMSize(var Message: TWMSize);
{$ENDIF}
begin
 inherited;

 if FTransparent and (not (csLoading in ComponentState))
  then BackBufferChanged;
end;

procedure TGuiControlsCheckBox.WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF});
begin
 inherited;

 if FTransparent and (not (csLoading in ComponentState))
  then BackBufferChanged;
end;

procedure TGuiControlsCheckBox.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
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

procedure TGuiControlsCheckBox.RenderBox(Buffer: TGuiCustomPixelMap);
var
  YOffset, X, Y     : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  CombColor         : TPixel32;
  BorderColor       : TPixel32;
  DotColor          : TPixel32;
  BackColor         : TPixel32;
  DrawDot           : Boolean;
  DotRect           : TRect;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Temp              : Single;
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

   Radius := 4;
   BorderWidth := Max(1.5, 0.5 + 0.1 * FCheckBoxSize);
   YOffset := (Height - FCheckBoxSize) div 2;

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
     ScnLne[0] := Scanline[Y + YOffset];
     ScnLne[1] := Scanline[FCheckBoxSize - 1 - Y + YOffset];

     for X := Round((Radius - 1) - XStart) to Round((FCheckBoxSize - 1) - (Radius - 1) + XStart) do
      begin
       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (FCheckBoxSize - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (FCheckBoxSize - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist < SqrRadMinusBorder
        then CombColor := BackColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombinePixel(BorderColor, BackColor, Round($FF - Temp * $FF));
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

   for Y := Round(Radius) to FCheckBoxSize - 1 - Round(Radius) do
    begin
     ScnLne[0] := Scanline[Y + YOffset];
     for X := 0 to FCheckBoxSize - 1 do
      begin
       // check whether position is a border
       if (Y < BorderWidth - 1) or (Y > FCheckBoxSize - 1 - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether position is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > FCheckBoxSize - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end else
         if (X > FCheckBoxSize - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - FCheckBoxSize + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end else

       // check whether position is a lower half border
       if (Y > FCheckBoxSize - 1 - BorderWidth) then
        begin
         Temp := Y - (FCheckBoxSize - 1 - BorderWidth);
         if (X < BorderWidth - 1) or (X > FCheckBoxSize - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end else
         if (X > FCheckBoxSize - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - FCheckBoxSize + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > FCheckBoxSize - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end else
       if (X > FCheckBoxSize - 1 - BorderWidth) then
        begin
         Temp := X - (FCheckBoxSize - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, BackColor, Round(Temp * $FF));
        end
       else CombColor := BackColor;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;

   if Checked then
    begin
     DotRect := Rect(4, YOffset + 4, FCheckBoxSize - 4, YOffset + FCheckBoxSize - 4);
     FillRect(DotRect, DotColor);
    end;

  end;
end;

procedure TGuiControlsCheckBox.RenderText(Buffer: TGuiCustomPixelMap);
var
  TextSize    : TSize;
  OldAlpha    : Byte;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtent(Caption);
   {$IFDEF FPC}
   TextSize.cx := FCheckBoxSize + 3;
   {$ELSE}
   case Alignment of
    taLeftJustify  : TextSize.cx := Width - TextSize.cx - FCheckBoxSize - 3;
    taRightJustify : TextSize.cx := FCheckBoxSize + 3;
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

procedure TGuiControlsCheckBox.NativeChanged;
var
  OldMouseInControl : Boolean;
begin
 OldMouseInControl := FMouseInControl;

 {$IFDEF FPC}
 RecreateWnd(Self);
 {$ELSE}
 RecreateWnd;
 {$ENDIF}
 FMouseInControl := OldMouseInControl;

 if not FNative
  then FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TGuiControlsCheckBox.SetColors(Index: Integer; Value: TColor);
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

procedure TGuiControlsCheckBox.SetNative(const Value: Boolean);
begin
 if FNative <> Value then
  begin
   FNative := Value;
   NativeChanged;
  end;
end;

procedure TGuiControlsCheckBox.SetOversampling(
  const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TGuiControlsCheckBox.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TGuiControlsCheckBox.SetBiDiMode(Value: TBiDiMode);
begin
 inherited;

 {$IFNDEF FPC}
 if BidiMode = bdRightToLeft
  then Alignment := taLeftJustify
  else Alignment := taRightJustify;
 {$ENDIF}
end;

procedure TGuiControlsCheckBox.SetChecked(Value: Boolean);
begin
 inherited SetChecked(Value);

 if not FNative
  then BufferChanged(False);
end;

procedure TGuiControlsCheckBox.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TGuiControlsCheckBox.TransparentChanged;
begin
 BackBufferChanged;
end;

function TGuiControlsCheckBox.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TGuiControlsCheckBox.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TGuiControlsCheckBox.UpdateBackBuffer;
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

procedure TGuiControlsCheckBox.UpdateBuffer;
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

 if FBoxChanged and FTextChanged then
  begin
   FBoxChanged := False;
   FTextChanged := False;

   // copy entire back buffer to buffer
   Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
     FBuffer.Width * SizeOf(TPixel32));

   RenderBox(FBuffer);
   RenderText(FBuffer);
   Exit;
  end;

 // check whether only the box changed;
 if FBoxChanged then
  begin
   FBoxChanged := False;

   // copy box part of the back buffer to buffer
   SrcPtr := FBackBuffer.DataPointer;
   DestPtr := FBuffer.DataPointer;
   for y := 0 to FBuffer.Height - 1 do
    begin
     Move(SrcPtr^, DestPtr^, (FCheckBoxSize + 1) * SizeOf(TPixel32));
     SrcPtr := @SrcPtr^[FBuffer.Width];
     DestPtr := @DestPtr^[FBuffer.Width];
    end;

   // actually render box
   RenderBox(FBuffer);
   Exit;
  end;

 // check whether only the text changed;
 if FTextChanged then
  begin
   FTextChanged := False;

   // copy text part of the back buffer to buffer
   SrcPtr := @FBackBuffer.DataPointer[(FCheckBoxSize + 1)];
   DestPtr := @FBuffer.DataPointer[(FCheckBoxSize + 1)];
   for y := 0 to FBuffer.Height - 1 do
    begin
     Move(SrcPtr^, DestPtr^, (FBuffer.Width - (FCheckBoxSize + 1)) * SizeOf(TPixel32));
     SrcPtr := @SrcPtr^[FBuffer.Width];
     DestPtr := @DestPtr^[FBuffer.Width];
    end;

   // actually render text
   RenderText(FBuffer);
  end;
end;

end.
