unit DAV_GuiEQSlide;

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
  DAV_GuiCustomControl, DAV_GuiPixelMap;

type
  TCustomGuiEQSlide = class;
  TGuiEQSlide = class;

  TGetColorEvent = function(Sender: TObject; const Frequency: Single): TColor of object;

  TCustomGuiEQSlideAxis = class(TPersistent)
  protected
    FOwner        : TCustomGuiEQSlide;
    FUpper        : Single;
    FLower        : Single;
    FRange        : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
    procedure CalculateRange;
    procedure RangeChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQSlide); virtual;
    property Range: Single read FRange;
  end;

  // X-Axis

  TCustomGuiEQSlideXAxis = class(TCustomGuiEQSlideAxis)
  private
    FInvUpper       : Single;
    FInvLower       : Single;
    FLog2Ratio      : Single;
    FInvLog2Ratio   : Single;
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LowerFrequencyChanged; virtual;
    procedure UpperFrequencyChanged; virtual;

    procedure CalculateLowerFrequencyReciprocal;
    procedure CalculateUpperFrequencyReciprocal;
    procedure CalculateFrequencyRangeRatios;
  public
    constructor Create(AOwner: TCustomGuiEQSlide); override;

    // conversion between logarithmic frequency and linear
    function LinearToLogarithmicFrequency(Value: Double): Double;
    function LogarithmicFrequencyToLinear(Value: Double): Double;

    // conversion between linear and logarithmic frequency
    function FastLinearToLogarithmicFrequency(Value: Single): Single;
    function FastLogarithmicFrequencyToLinear(Value: Single): Single;

    property UpperFrequency: Single read FUpper write SetUpperFrequency;
    property LowerFrequency: Single read FLower write SetLowerFrequency;
  end;

  TGuiEQSlideXAxis = class(TCustomGuiEQSlideXAxis)
  published
    property UpperFrequency;
    property LowerFrequency;
  end;


  // EQ-Slide

  TCustomGuiEQSlide = class(TGuiCustomControl)
  private
    FAutoColor    : Boolean;
    FBorderRadius : Integer;
    FBorderWidth  : Integer;
    FBorderColor  : TColor;
    FXAxis        : TGuiEQSlideXAxis;
    FOnPaint      : TNotifyEvent;
    FOnGetColor   : TGetColorEvent;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetXAxis(const Value: TGuiEQSlideXAxis);
    procedure RenderRoundedFrameRectangle(PixelMap: TGuiCustomPixelMap);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AutoColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateBuffer; override;

    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;

    property XAxis: TGuiEQSlideXAxis read FXAxis write SetXAxis;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnGetColor: TGetColorEvent read FOnGetColor write FOnGetColor;
  end;

  TGuiEQSlide = class(TCustomGuiEQSlide)
  published
    property AutoColor;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property Transparent;
    property XAxis;

    property OnGetColor;
    property OnPaint;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
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
  Math, DAV_Common, DAV_GuiBlend, DAV_Approximations;

{ TCustomGuiEQSlideAxis }

constructor TCustomGuiEQSlideAxis.Create(AOwner: TCustomGuiEQSlide);
begin
 FOwner := AOwner;
end;

procedure TCustomGuiEQSlideAxis.Changed;
begin
 FOwner.BufferChanged;
end;

procedure TCustomGuiEQSlideAxis.RangeChanged;
begin
 CalculateRange;
end;

procedure TCustomGuiEQSlideAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQSlideAxis then
  with TCustomGuiEQSlideAxis(Dest) do
   begin
    FOwner        := Self.FOwner;
    FUpper        := Self.FUpper;
    FLower        := Self.FLower;
    FRange        := Self.FRange;
   end
 else inherited;
end;

procedure TCustomGuiEQSlideAxis.CalculateRange;
begin
 FRange := FUpper - FLower;
end;


{ TCustomGuiEQSlideXAxis }

constructor TCustomGuiEQSlideXAxis.Create(AOwner: TCustomGuiEQSlide);
begin
 inherited;
 FLower := 20;
 FUpper := 20000;
 CalculateUpperFrequencyReciprocal;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
end;

procedure TCustomGuiEQSlideXAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQSlideXAxis then
  with TCustomGuiEQSlideXAxis(Dest) do
   begin
    inherited;
    FInvUpper     := Self.FInvUpper;
    FInvLower     := Self.FInvLower;
    FLog2Ratio    := Self.FLog2Ratio;
    FInvLog2Ratio := Self.FInvLog2Ratio;
   end
 else inherited;
end;

function TCustomGuiEQSlideXAxis.LogarithmicFrequencyToLinear(Value: Double): Double;
begin
 Result := Log2(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQSlideXAxis.LinearToLogarithmicFrequency(Value: Double): Double;
begin
 Result := Power(2, Value * FLog2Ratio) * FLower;
end;

function TCustomGuiEQSlideXAxis.FastLogarithmicFrequencyToLinear(Value: Single): Single;
begin
 Result := FastLog2MinError3(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQSlideXAxis.FastLinearToLogarithmicFrequency(Value: Single): Single;
begin
 Result := FastPower2MinError3(Value * FLog2Ratio) * FLower;
end;

procedure TCustomGuiEQSlideXAxis.SetLowerFrequency(const Value: Single);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerFrequencyChanged;
  end;
end;

procedure TCustomGuiEQSlideXAxis.SetUpperFrequency(const Value: Single);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperFrequencyChanged;
  end;
end;

procedure TCustomGuiEQSlideXAxis.UpperFrequencyChanged;
begin
 RangeChanged;
 CalculateUpperFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQSlideXAxis.LowerFrequencyChanged;
begin
 RangeChanged;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQSlideXAxis.CalculateUpperFrequencyReciprocal;
begin
 Assert(FUpper <> 0);

 // calculate reciprocal of upper frequency
 FInvUpper := 1 / FUpper;
end;

procedure TCustomGuiEQSlideXAxis.CalculateLowerFrequencyReciprocal;
begin
 Assert(FLower <> 0);

 // calculate reciprocal of lower frequency
 FInvLower := 1 / FLower;
end;

procedure TCustomGuiEQSlideXAxis.CalculateFrequencyRangeRatios;
begin
 Assert(FUpper <> 0);
 Assert(FInvLower <> 0);

 // calculate lograithmic frequency ratio (as new logarithm base)
 FLog2Ratio := Log2(FUpper * FInvLower);
 FInvLog2Ratio := 1 / FLog2Ratio;
end;


{ TCustomGuiEQSlide }

constructor TCustomGuiEQSlide.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop := False; // Ensure we're not a tab-stop
 Color := clBtnFace;

 FXAxis           := TGuiEQSlideXAxis.Create(Self);

 FAutoColor       := False;
 FBorderColor     := $202020;
 FBorderWidth     := 1;
end;

destructor TCustomGuiEQSlide.Destroy;
begin
 FreeAndNil(FXAxis);
 inherited Destroy;
end;

procedure TCustomGuiEQSlide.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiEQSlide then
  with TCustomGuiEQSlide(Dest) do
   begin
    FAutoColor    := Self.FAutoColor;
    FBorderRadius := Self.FBorderRadius;
    FBorderWidth  := Self.FBorderWidth;
    FBorderColor  := Self.FBorderColor;
    FOnPaint      := Self.FOnPaint;
    FOnGetColor   := Self.FOnGetColor;

    FXAxis.Assign(Self.FXAxis);
   end;
end;

procedure TCustomGuiEQSlide.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiEQSlide.SetBorderRadius(const Value: Integer);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiEQSlide.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
  end;
end;

procedure TCustomGuiEQSlide.SetBorderWidth(const Value: Integer);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiEQSlide.AutoColorChanged;
begin
 if FAutoColor then
  begin
(*
   FChartColor32 := Lighten(Color32(Color),60);
   FChartColor := WinColor(FChartColor32);
*)
   BufferChanged;
  end;
end;

procedure TCustomGuiEQSlide.SetXAxis(const Value: TGuiEQSlideXAxis);
begin
 FXAxis.Assign(Value);
end;

procedure TCustomGuiEQSlide.BorderWidthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQSlide.BorderColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQSlide.BorderRadiusChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQSlide.UpdateBuffer;
begin
 inherited;

 RenderRoundedFrameRectangle(FBuffer);
end;

procedure TCustomGuiEQSlide.RenderRoundedFrameRectangle(
  PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  BackColor         : TPixel32;
  BorderColor       : TPixel32;
  CombColor         : TPixel32;
  ColorArray        : array of TPixel32;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Scale, Temp       : Single;
begin
 with PixelMap do
  begin
   BackColor := ConvertColor(Color);
   if FBorderWidth > 0
    then BorderColor := ConvertColor(FBorderColor)
    else BorderColor := BackColor;

   // initialize variables
   Radius := FBorderRadius;
   if 0.5 * Width < Radius then Radius := 0.5 * Width;
   if 0.5 * Height < Radius then Radius := 0.5 * Height;
   BorderWidth := Math.Max(FBorderWidth, 1);
   Scale := 1 / (Width - 2 * FBorderRadius);

   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth - 1));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

   // calculate colors
   SetLength(ColorArray, Width);
   if Assigned(FOnGetColor) then
    for X := 0 to Width - 1
     do ColorArray[X] := ConvertColor(FOnGetColor(Self,
       FXAxis.LinearToLogarithmicFrequency((X - FBorderRadius) * Scale)))
   else
    for X := 0 to Width - 1
     do ColorArray[X] := BackColor;

   // draw upper & lower part (with rounded corners)
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
       CombColor := ColorArray[X];

       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (Width - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (Width - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist >= SqrRadMinusBorder then
        if SqrDist <= Sqr(RadMinusBorderOne) then
         begin
          Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
          CombColor := CombinePixel(BorderColor, CombColor, Round($FF - Temp * $FF));
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
       CombColor := ColorArray[X];

       // check whether value is a pure border
       if (Y < BorderWidth - 1) or (Y > Height - 1 - BorderWidth + 1)
        then CombColor := BorderColor else

       // check whether value is an upper half border
       if (Y < BorderWidth) then
        begin
         Temp := BorderWidth - Y;
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF))
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF))
          end
         else
          CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF));
        end else

       // check whether value is a lower half border
       if (Y > Height - 1 - BorderWidth) then
        begin
         Temp := Y - (Height - 1 - BorderWidth);
         if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
          then CombColor := BorderColor else
         if (X < BorderWidth) then
          begin
           Temp := Temp + (BorderWidth - X) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, CombColor, Round(Temp * $FF));
        end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;
  end;
end;

end.
