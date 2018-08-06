unit DAV_GuiLED;

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
  {$IFDEF FPC} LCLIntf, LCLType, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, DAV_GuiCommon, DAV_GuiPixelMap,
  DAV_GuiGraphicControl;

type
  TParentControl = class(TWinControl);

  TCustomGuiLED = class(TGraphicControl)
  private
    FBuffer        : TGuiCustomPixelMap;
    FOnPaint       : TNotifyEvent;

    FLEDColor      : TColor;
    FBrightness    : Single;
    FUniformity    : Single;
    FBorderFactor  : Single;
    FBorderWidth   : Single;
    FTransparent   : Boolean;
    FBufferChanged : Boolean;
    FOnChange      : TNotifyEvent;
    function GetUniformity: Single;
    function GetBorderStrength: Single;
    procedure SetLEDColor(const Value: TColor);
    procedure SetBrightness(const Value: Single);
    procedure SetUniformity(const Value: Single);
    procedure SetBorderStrength(const Value: Single);
    procedure SetTransparent(const Value: Boolean);
  protected
    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave : TNotifyEvent;
    FOnMouseEnter : TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$ELSE}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message: TLmEraseBkgnd); message LM_ERASEBKGND;
    {$ENDIF}

    procedure RenderLED(const PixelMap: TGuiCustomPixelMap);
    procedure SetBorderWidth(const Value: Single);

    procedure Paint; override;
    procedure BufferChanged; virtual;
    procedure ResizeBuffer; virtual;
    procedure UpdateBuffer; virtual;

    procedure BorderStrengthChanged; virtual;
    procedure BrightnessChanged; virtual;
    procedure LEDColorChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure TransparentChanged; virtual;
    procedure UniformityChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;
    procedure Loaded; override;

    property BorderStrength_Percent: Single read GetBorderStrength write SetBorderStrength;
    property Brightness_Percent: Single read FBrightness write SetBrightness;
    property LEDColor: TColor read FLEDColor write SetLEDColor default clBlack;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Uniformity_Percent: Single read GetUniformity write SetUniformity;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiLED = class(TCustomGuiLED)
  published
    property BorderStrength_Percent;
    property Brightness_Percent;
    property LEDColor;
    property BorderWidth;
    property Uniformity_Percent;
    property Transparent;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
    {$IFDEF Delphi6_Up}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$IFDEF DELPHI8_Up}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    {$ENDIF}
  end;

implementation

uses
  ExtCtrls, Math, DAV_Math, DAV_Common, DAV_Complex, DAV_Approximations, DAV_GuiBlend;

{ TCustomGuiLED }

constructor TCustomGuiLED.Create(AOwner: TComponent);
begin
 inherited;
 FBuffer := TGuiPixelMapMemory.Create;

 FLEDColor      := clRed;
 FBorderWidth   := 1.5;
 FBrightness    := 100;
 FUniformity    := 0.4;
 FBufferChanged := True;

 ControlStyle := ControlStyle + [csOpaque] - [csFramed];
end;

destructor TCustomGuiLED.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

procedure TCustomGuiLED.Resize;
begin
 inherited Resize;
 ResizeBuffer;
end;

procedure TCustomGuiLED.ResizeBuffer;
begin
 if (Width > 0) and (Height > 0) then
  begin
   FBuffer.Width := Width;
   FBuffer.Height := Height;
   BufferChanged;
  end;
end;

procedure TCustomGuiLED.Loaded;
begin
 inherited;
 ResizeBuffer;
end;

procedure TCustomGuiLED.Paint;
begin
 if FBufferChanged
  then UpdateBuffer;

 FBuffer.PaintTo(Canvas);
 if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TCustomGuiLED.BufferChanged;
begin
 FBufferChanged := True;
 Invalidate;
end;

procedure TCustomGuiLED.BorderStrengthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiLED.BrightnessChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiLED.LEDColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiLED.BorderWidthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiLED.TransparentChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiLED.UniformityChanged;
begin
 BufferChanged;
end;


procedure TCustomGuiLED.SetBorderStrength(const Value: Single);
begin
 if BorderStrength_Percent <> Value then
  begin
   FBorderFactor := 1 - Limit(0.01 * Value, 0, 1);
   BorderStrengthChanged;
  end;
end;

procedure TCustomGuiLED.SetBrightness(const Value: Single);
begin
 if FBrightness <> Value then
  begin
   FBrightness := Value;
   BrightnessChanged;
  end;
end;

procedure TCustomGuiLED.SetLEDColor(const Value: TColor);
begin
 if (Value <> FLEDColor) then
  begin
   FLEDColor := Value;
   LEDColorChanged;
  end;
end;

procedure TCustomGuiLED.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiLED.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiLED.SetUniformity(const Value: Single);
begin
 if Uniformity_Percent <> Value then
  begin
   FUniformity := Sqr(1 - Limit(0.01 * Value, 0, 1));
   UniformityChanged;
  end;
end;

procedure TCustomGuiLED.UpdateBuffer;
begin
 FBufferChanged := False;

 if FTransparent
  then FBuffer.CopyParentImage(Self)
  else FBuffer.Clear(Color);

 RenderLED(FBuffer);
end;

procedure TCustomGuiLED.RenderLED(const PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : PPixel32Array;
  LEDColor          : TPixel32;
  CombColor         : TPixel32;
  Radius            : Single;
  XStart            : Single;
  RadMinusBorderOne : Single;
  Scale             : Single;
  Center            : TComplex32;
  SqrYDist          : Single;
  SqrDist           : Single;
  Bright            : Single;
  SqrRadMinusOne    : Single;
  SqrRadMinusBorder : Single;
  ReciSqrRad        : Single;
  Temp              : Single;
  CombAlpha         : Integer;

const
  CBlack : TPixel32 = (ARGB : $FF000000);
begin
 with PixelMap do
  begin
   LEDColor := ConvertColor(FLEDColor);
   Bright := 0.3 + 0.007 * FBrightness;

   // draw circle
   Radius := 0.5 * Math.Min(Width, Height) - 3;
   if Radius <= 0 then Exit;

   ReciSqrRad := 1 / Sqr(Radius);
   RadMinusBorderOne := BranchlessClipPositive(Radius - FBorderWidth + 1);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - FBorderWidth));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   Center.Re := 0.5 * Width;
   Center.Im := 0.5 * Height;

   for Y := Round(Center.Im - Radius) to Round(Center.Im + Radius) do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Radius) - SqrYDist;
     if XStart < 0
      then Continue
      else XStart := Sqrt(XStart) - 0.4999999;

     ScnLne := Scanline[Y];
     for X := Round(Center.Re - XStart) to Round(Center.Re + XStart) do
      begin
       // calculate squared distance
       SqrDist := Sqr(X - Center.Re) + SqrYDist;
       Scale := Bright * (1 - FUniformity * SqrDist * ReciSqrRad);

       if FBorderWidth > 1.5 then
        begin
         if SqrDist <= SqrRadMinusBorder
          then CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF))
          else
         if SqrDist <= Sqr(RadMinusBorderOne) then
          begin
           Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
           Scale := (Temp + (1 - Temp) * FBorderFactor) * Scale;

           CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF));
          end else
         if SqrDist < SqrRadMinusOne
          then CombColor := CombinePixel(LEDColor, CBlack, Round(FBorderFactor * Scale * $FF))
          else
           begin
            Scale := FBorderFactor * Scale;
            CombColor := CBlack;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombinePixelInplace(LEDColor, CombColor, Round(Scale * $FF));
            CombColor.A := CombAlpha;
           end;
        end
       else
        begin
         if SqrDist < SqrRadMinusOne
          then CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF))
          else
           begin
            CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF));
            EMMS;
            Temp := FastSqrtBab2(SqrDist) - (Radius - 1);
            Scale := (Temp + (1 - Temp) * FBorderFactor) * 0.5 * FBorderWidth * Scale;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombColor := CombinePixel(CBlack, CombColor, Round((1 - Scale) * $FF));
            CombColor.A := CombAlpha;

 (*
            Scale := FBorderFactor * Scale;
            CombColor := CBlack;
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            CombinePixelInplace(LEDColor, CombColor, Round(Scale * $FF));
            CombColor.A := CombAlpha;
            BlendPixelInplace(CombColor, ScnLne[X]);

 ( *
            Scale := FBorderFactor * Scale;
            Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
            CombAlpha := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
            Scale := (Temp + (1 - Temp) * FBorderFactor) * Scale;
            CombColor := CombinePixel(LEDColor, CBlack, Round(Scale * $FF));
            CombinePixelInplace(LEDColor, CombColor, Round(Scale * $FF));
            CombColor.A := CombAlpha;
            BlendPixelInplace(CombColor, ScnLne[X]);
 *)
           end;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       EMMS;
      end;
    end;
  end;
end;

function TCustomGuiLED.GetBorderStrength: Single;
begin
 Result := 100 * (1 - FBorderFactor);
end;

function TCustomGuiLED.GetUniformity: Single;
begin
 Result := 100 * (1 - Sqrt(FUniformity));
end;

{$IFNDEF FPC}
{$IFNDEF COMPILER10_UP}
procedure TCustomGuiLED.CMMouseEnter(var Message: TMessage);
begin
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomGuiLED.CMMouseLeave(var Message: TMessage);
begin
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$ENDIF}

procedure TCustomGuiLED.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
 Message.Result := 0;
end;

{$ELSE}
procedure TCustomGuiLED.WMEraseBkgnd(var Message: TLmEraseBkgnd);
begin
  Message.Result := 0;
end;
{$ENDIF}

procedure TCustomGuiLED.CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 BufferChanged;
end;

procedure TCustomGuiLED.CMEnabledChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 BufferChanged;
end;

end.
