unit DAV_GuiButton;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$IFDEF MSWINDOWS} Windows,
  {$ENDIF} {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Controls, StdCtrls, Graphics, DAV_GuiCommon, DAV_GuiFont,
  DAV_GuiShadow, DAV_GuiPixelMap;

type
  TCustomGuiButton = class(TButtonControl)
  private
    FAlignment    : TAlignment;
    FBorderColor  : TColor;
    FBorderRadius : Single;
    FBorderWidth  : Single;
    FButtonColor  : TColor;
    FCanvas       : TCanvas;
    FCaption      : string;
    FGuiFont      : TGuiOversampledGDIFont;
    FTransparent  : Boolean;
    FOnPaint      : TNotifyEvent;
    function GetShadow: TGUIShadow;
    function GetOversampling: TFontOversampling;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetButtonColor(const Value: TColor);
    procedure SetCaption(const Value: string);
    procedure SetBorderRadius(Value: Single);
    procedure SetShadow(const Value: TGUIShadow);
    procedure SetTransparent(const Value: Boolean);
    procedure SetOversampling(const Value: TFontOversampling);
  protected
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBackBuffer : Boolean;
    FUpdateBuffer     : Boolean;

    procedure Paint; virtual;
    procedure BufferChanged; virtual;
    procedure BackBufferChanged; virtual;
    procedure RenderButton(PixelMap: TGuiCustomPixelMap);
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure UpdateBackBuffer; virtual;
    procedure UpdateBuffer; virtual;

    procedure Resize; override;
    procedure Loaded; override;
    procedure PaintWindow(DC: HDC); override;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure AlignmentChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure CaptionChanged; virtual;
    procedure TransparentChanged; virtual;

    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnShadow;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnFace;
    property Caption: string read FCaption write SetCaption;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property Transparent: Boolean read FTransparent write SetTransparent;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiButton = class(TCustomGuiButton)
  published
    property Align;
    property Anchors;
    property Alignment;
    property BorderColor;
    property BorderWidth;
    property BorderRadius;
    property ButtonColor;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property PopupMenu;
    property Shadow;
    property ShowHint;
    property Transparent;
    property Visible;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF DELPHI10_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
  end;

implementation

uses
  Math, SysUtils, DAV_GuiBlend, DAV_Math, DAV_Common, DAV_Complex,
  DAV_Approximations;

{ TCustomGuiButton }

constructor TCustomGuiButton.Create(AOwner: TComponent);
begin
 inherited;
 ControlStyle  := ControlStyle + [csOpaque, csReplicatable];

 // create control canvas
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;

 // create GUI font
 FGuiFont := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 // create buffers
 FBuffer       := TGuiPixelMapMemory.Create;
 FBackBuffer   := TGuiPixelMapMemory.Create;

 FAlignment    := taCenter;
 FCaption      := 'empty';
 FBorderRadius := 2;
 FBorderWidth  := 1;
 FButtonColor  := clBtnShadow;
end;

destructor TCustomGuiButton.Destroy;
begin
 FreeAndNil(FCanvas);
 FreeAndNil(FGuiFont);
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited;
end;

procedure TCustomGuiButton.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiButton.AlignmentChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiButton.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiButton.BorderColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiButton.BorderWidthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiButton.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TCustomGuiButton.ButtonColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiButton.CaptionChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiButton.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 FUpdateBackBuffer := False;
 if FTransparent then FBackBuffer.CopyParentImage(Self) else
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;

 FUpdateBuffer := True;
end;

procedure TCustomGuiButton.UpdateBuffer;
var
  TextSize : TSize;
begin
 FUpdateBuffer := False;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 RenderButton(FBuffer);

 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtent(FCaption);
   case FAlignment of
    taLeftJustify  : TextSize.cx := 0;
    taRightJustify : TextSize.cx := Width - TextSize.cx;
    taCenter       : TextSize.cx := (Width - TextSize.cx) div 2;
   end;

   TextSize.cy := (Height - TextSize.cy) div 2;
   FGuiFont.TextOut(FCaption, FBuffer, TextSize.cx, TextSize.cy);
  end;
end;

procedure TCustomGuiButton.BorderRadiusChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiButton.TransparentChanged;
begin
 BackBufferChanged;
end;

function TCustomGuiButton.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiButton.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiButton.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiButton.WMPaint(var Message: TWMPaint);
begin
 ControlState := ControlState + [csCustomPaint];
 inherited;
 ControlState := ControlState - [csCustomPaint];
end;

procedure TCustomGuiButton.PaintWindow(DC: HDC);
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

procedure TCustomGuiButton.Paint;
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

procedure TCustomGuiButton.RenderButton(PixelMap: TGuiCustomPixelMap);
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
   if FBorderWidth = 0
    then BorderColor := ButtonColor
    else BorderColor := ConvertColor(FBorderColor);

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

procedure TCustomGuiButton.Resize;
begin
 inherited;
 if Assigned(FBuffer) then FBuffer.SetSize(Width, Height);
 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   BackBufferChanged;
  end;
end;

procedure TCustomGuiButton.CMFontChanged(var Message: TMessage);
begin
 FGuiFont.Font.Assign(Font);
end;

procedure TCustomGuiButton.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiButton.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiButton.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiButton.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   ButtonColorChanged;
  end;
end;

procedure TCustomGuiButton.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiButton.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TCustomGuiButton.SetBorderRadius(Value: Single);
begin
 if Value < 0 then Value := 0;
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiButton.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TCustomGuiButton.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

end.
