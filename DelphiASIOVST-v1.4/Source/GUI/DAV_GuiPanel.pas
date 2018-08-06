unit DAV_GuiPanel;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Messages, Controls, Graphics, ExtCtrls, DAV_FixedPoint,
  DAV_GuiPixelMap;

type
  TCustomGuiPanel = class(TCustomPanel)
  private
    FBorderVisible : Boolean;
    FPanelColor    : TColor;
    FNative        : Boolean;
    FRoundRadius   : Single;
    FTransparent   : Boolean;
    FBorderWidth   : Single;
    FBorderColor   : TColor;
    FBuffer        : TGuiPixelMapMemory;
    FBufferChanged : Boolean;
    FAlpha         : Byte;
    procedure SetBorderVisible(const Value: Boolean);
    procedure SetNative(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetPanelColor(const Value: TColor);
    procedure SetRoundRadius(const Value: Single);
    procedure SetTransparent (const Value: Boolean);
    procedure SetAlpha(const Value: Byte);
  protected
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMChanged(var Message: TMessage); message CM_CHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentcolorchanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;

    procedure Paint; override;
    procedure Resize; override;
    procedure PaintBitmap; virtual;
    procedure Loaded; override;

    procedure AlphaChanged; virtual;
    procedure NativeChanged; virtual;
    procedure LineColorChanged; virtual;
    procedure BorderVisibleChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure PanelColorChanged; virtual;
    procedure RoundRadiusChanged; virtual;
    procedure TransparentChanged; virtual;

    procedure RenderPanel(PixelMap: TGuiCustomPixelMap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;

    property Alpha: Byte read FAlpha write SetAlpha default $FF;
    property BorderVisible: Boolean read FBorderVisible write SetBorderVisible default True;
    property PanelColor: TColor read FPanelColor write SetPanelColor default clBtnShadow;
    property Native: Boolean read FNative write SetNative default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnHighlight;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderRadius: Single read FRoundRadius write SetRoundRadius;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiPanel = class(TCustomGuiPanel)
  published
    property Align;
    property Alignment;
    property Alpha;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderColor;
    property BorderRadius;
    property BorderVisible;
    property BorderWidth;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Native;
    property PanelColor;
    property ParentBiDiMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property UseDockManager;
    property Visible;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnEndDock;
    property OnStartDock;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnClick;
    property OnDblClick;
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
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils, Math, DAV_Math, DAV_Approximations, DAV_GuiCommon, DAV_GuiBlend,
  DAV_GuiFixedPoint;


{ TCustomGuiPanel }

constructor TCustomGuiPanel.Create (AOwner: TComponent);
begin
 inherited Create(AOwner);
 FPanelColor    := clBtnHighlight;
 FBorderColor     := clBtnShadow;
 FAlpha         := $FF;
 FBorderWidth   := 2;
 FRoundRadius   := 2;
 FBorderVisible := True;
 FNative        := False;
 FBuffer        := TGuiPixelMapMemory.Create;
 FBufferChanged := True;
 FTransparent   := False;
 ParentColor    := True;
 ControlStyle   := ControlStyle + [csAcceptsControls];

 {$IFDEF FPC}
 DoubleBuffered := True;
 ControlStyle   := ControlStyle - [csOpaque];
 {$ELSE}
 ControlStyle   := ControlStyle + [csOpaque];
 {$ENDIF}

 // set initial bounds
 SetBounds(0, 0, 128, 64);
end;

destructor TCustomGuiPanel.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

procedure TCustomGuiPanel.AssignTo(Dest: TPersistent);
begin
 if Dest is TBitmap then
  begin
   (Dest as TBitmap).Canvas.Assign(Canvas);
  end else
 if Dest is TCustomGuiPanel then
  with TCustomGuiPanel(Dest) do
   begin
    FBorderVisible := Self.FBorderVisible;
    FPanelColor    := Self.FPanelColor;
    FNative        := Self.FNative;
    FRoundRadius   := Self.FRoundRadius;
    FTransparent   := Self.FTransparent;
    FBorderWidth   := Self.FBorderWidth;
    FBorderColor     := Self.FBorderColor;
    FBuffer        := Self.FBuffer;
    FBufferChanged := Self.FBufferChanged;
   end else inherited;
end;

procedure TCustomGuiPanel.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiPanel.SetNative(const Value: Boolean);
begin
 if FNative <> Value then
  begin
   FNative := Value;
   NativeChanged;
  end;
end;

procedure TCustomGuiPanel.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   LineColorChanged;
  end;
end;

procedure TCustomGuiPanel.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiPanel.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TCustomGuiPanel.SetBorderVisible(const Value: Boolean);
begin
 if FBorderVisible <> Value then
  begin
   FBorderVisible := Value;
   BorderVisibleChanged;
  end;
end;

procedure TCustomGuiPanel.SetPanelColor(const Value: TColor);
begin
 if FPanelColor <> Value then
  begin
   FPanelColor := Value;
   PanelColorChanged;
  end;
end;

procedure TCustomGuiPanel.SetRoundRadius(const Value: Single);
begin
 if FRoundRadius <> Value then
  begin
   FRoundRadius := Value;
   RoundRadiusChanged;
  end;
end;

procedure TCustomGuiPanel.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;


procedure TCustomGuiPanel.AlphaChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.NativeChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.LineColorChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.BorderWidthChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.BorderVisibleChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.RoundRadiusChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.PanelColorChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.TransparentChanged;
begin
 Changed;
end;

procedure TCustomGuiPanel.PaintBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer do
   begin
    {$IFNDEF FPC}
    if FTransparent
     then FBuffer.CopyParentImage(Self) else
    {$ENDIF}
    Clear(Color);
    RenderPanel(FBuffer);
   end;
end;

procedure TCustomGuiPanel.Paint;
begin
 if FNative
  then inherited
  else
   begin
    if FBufferChanged then
     begin
      FBufferChanged := False;
      PaintBitmap;
     end;
    FBuffer.PaintTo(Canvas);
   end;
end;

procedure TCustomGuiPanel.RenderPanel(PixelMap: TGuiCustomPixelMap);
var
  X, Y                 : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : array [0..1] of PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  Radius               : TFixed24Dot8;
  XStart               : TFixed24Dot8;
  BorderWidthFixed     : TFixed24Dot8;
  RadiusMinusOne       : TFixed24Dot8;
  RadiusMinusBorder    : TFixed24Dot8;
  SqrRadMinusBorderOne : TFixed24Dot8;
  SqrRadMinusBorder    : TFixed24Dot8;
  SqrDist, SqrYDist    : TFixed24Dot8;
  SqrRadMinusOne       : TFixed24Dot8;
  XFixed, YFixed       : TFixed24Dot8;
  WidthMinusOne        : TFixed24Dot8;
  YBorderDistance      : TFixed24Dot8;
  Temp                 : TFixed24Dot8;
begin
 with PixelMap do
  begin
   // set local colors
   PanelColor := ConvertColor(FPanelColor);
   PanelColor.A := Alpha;
   if FBorderVisible
    then BorderColor := ConvertColor(FBorderColor)
    else BorderColor := PanelColor;

   // set other local variables
   Radius := ConvertToFixed24Dot8(Min(FRoundRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8(Integer(Width - 1));

   // precalculate radius variables
   RadiusMinusOne.Fixed := Radius.Fixed - CFixed24Dot8One.Fixed;
   if RadiusMinusOne.Fixed < 0
    then RadiusMinusOne.Fixed := 0;

   RadiusMinusBorder.Fixed := Radius.Fixed - BorderWidthFixed.Fixed;
   if RadiusMinusBorder.Fixed < 0
    then RadiusMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadiusMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadiusMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := Radius.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);


   // draw rounded borders
   for Y := 0 to FixedRound(Radius) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(Radius, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(Radius).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     Temp.Fixed := RadiusMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadiusMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadiusMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadiusMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed < SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadiusMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then CombColor := PanelColor
          else CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(Radius, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);
      end;
    end;


   for Y := FixedRound(Radius) to Height - 1 - FixedRound(Radius) do
    begin
     ScnLne[0] := Scanline[Y];
     YFixed := ConvertToFixed24Dot8(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0][0], Width);
       Continue;
      end;

     // check upper/lower half and eventually precalculate y-border distance
     Temp := ConvertToFixed24Dot8(Integer(Height - 1));
     IsUpperLowerHalf := (YFixed.Fixed < BorderWidthFixed.Fixed) or
       (YFixed.Fixed > Temp.Fixed - BorderWidthFixed.Fixed);
     if IsUpperLowerHalf then
      if Y < Height div 2
       then YBorderDistance.Fixed := BorderWidthFixed.Fixed - YFixed.Fixed
       else YBorderDistance.Fixed := YFixed.Fixed - Temp.Fixed + BorderWidthFixed.Fixed
      else YBorderDistance.Fixed := 0;

     X := 0;
     while X < Width do
      begin
       // convert
       XFixed := ConvertToFixed24Dot8(X);

       // check whether position is an upper/lower half border
       if IsUpperLowerHalf then
        begin
         if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed >= ConvertToFixed24Dot8(Width).Fixed - BorderWidthFixed.Fixed)
          then CombColor := BorderColor else
         if (XFixed.Fixed < BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end else
         if (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := XFixed.Fixed + BorderWidthFixed.Fixed - WidthMinusOne.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end
         else
          begin
           Assert(YBorderDistance.Fixed >= 0);
           Assert(YBorderDistance.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, YBorderDistance.Fixed);
           BlendPixelLine(CombColor, @ScnLne[0][X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
          (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed)
        then CombColor := BorderColor else
       if (XFixed.Fixed < BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end else
       if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := XFixed.Fixed - WidthMinusOne.Fixed + BorderWidthFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end
       else
        begin
         BlendPixelLine(PanelColor, @ScnLne[0][X], Width - 2 * X);
         EMMS;
         X := Width - X;
         Continue;
        end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TCustomGuiPanel.Resize;
begin
 inherited;

 if Assigned(FBuffer) then
  begin
   if FBuffer.Width <> Width then
    begin
     FBuffer.Width := Width;
     FBufferChanged := True;
    end;
   if FBuffer.Height <> Height then
    begin
     FBuffer.Height := Height;
     FBufferChanged := True;
    end;
  end;
end;

procedure TCustomGuiPanel.CMChanged(var Message: TMessage);
begin
 inherited;

 if not FNative
  then FBufferChanged := True;

 Invalidate;
end;

procedure TCustomGuiPanel.CMColorChanged(var Message: TMessage);
begin
 inherited;

 if (not FNative)
  then FBufferChanged := True;

 Invalidate;
end;

procedure TCustomGuiPanel.CMEnabledChanged(var Message: TMessage);
begin
 inherited;

 if not FNative
  then FBufferChanged := True;

 Invalidate;
end;

procedure TCustomGuiPanel.CMParentColorChanged(var Message: TMessage);
begin
 inherited;

 if not FNative and ParentColor
  then FBufferChanged := True;

 Invalidate;
end;

end.
