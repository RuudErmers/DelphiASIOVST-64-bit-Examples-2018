unit DAV_GuiSlider;

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
  DAV_GuiPixelMap, DAV_GuiVectorPixelRectangle, DAV_GuiFixedPoint,
  DAV_GuiFont, DAV_GuiShadow;

type
  TSliderDirection = (sdLeftToRight);
  TSliderGetTextEvent = procedure(Sender: TObject; var Text: string) of object;

  TCustomGuiSlider = class(TCustomControl)
  private
    FAutoColor       : Boolean;
    FBorderColor     : TColor;
    FBorderRadius    : Single;
    FBorderWidth     : Single;
    FCurveMapping    : Single;
    FCurveMappingExp : Single;
    FDefaultValue    : Single;
    FDigits          : Integer;
    FDirection       : TSliderDirection;
    FMinimum         : Single;
    FMaximum         : Single;
    FShowText        : Boolean;
    FSlideColor      : TColor;
    FValue           : Single;
    FCaption         : TCaption;
    FReadOnly        : Boolean;
    FOnGetText       : TSliderGetTextEvent;
    FOnChange        : TNotifyEvent;
    FOnPaint         : TNotifyEvent;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Single);
    procedure SetBorderWidth(const Value: Single);
    procedure SetCaption(const Value: TCaption);
    procedure SetCurveMapping(const Value: Single);
    procedure SetDefaultValue(Value: Single);
    procedure SetDigits(const Value: Integer);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetValue(Value: Single);
    procedure SetSlideColor(const Value: TColor);
    procedure SetDirection(const Value: TSliderDirection);
    procedure SetShowText(const Value: Boolean);

    // floating point storage filer
    procedure ReadDefaultValueProperty(Reader: TReader);
    procedure WriteDefaultValueProperty(Writer: TWriter);
    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
    procedure ReadValueProperty(Reader: TReader);
    procedure WriteValueProperty(Writer: TWriter);
  protected
    function MapValue(Value: Double): Double;
    function UnmapValue(Value: Double): Double;

    procedure AssignTo(Dest: TPersistent); override;
    procedure AutoColorChanged; virtual;
    procedure CaptionChanged; virtual;
    procedure CurveMappingChanged; virtual;
    procedure DirectionChanged; virtual;
    procedure DigitsChanged; virtual;
    procedure PositionChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure ControlChanged; virtual;
    procedure SlideColorChanged; virtual;
    procedure ShowTextChanged; virtual;

    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;

    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property Caption: TCaption read FCaption write SetCaption;
    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property DefaultValue: Single read FDefaultValue write SetDefaultValue;
    property Digits: Integer read FDigits write SetDigits default 4;
    property Direction: TSliderDirection read FDirection write SetDirection default sdLeftToRight;
    property Max: Single read FMaximum write SetMax;
    property Min: Single read FMinimum write SetMin;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Value: Single read FValue write SetValue;
    property SlideColor: TColor read FSlideColor write SetSlideColor default $303030;
    property ShowText: Boolean read FShowText write SetShowText default False;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnGetText: TSliderGetTextEvent read FOnGetText write FOnGetText;
  end;

  TCustomGuiSliderGDI = class(TCustomGuiSlider)
  private
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FGuiFont          : TGuiOversampledGDIFont;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;
    FTransparent      : Boolean;

    function GetFontShadow: TGUIShadow;
    function GetOversampling: TFontOversampling;
    procedure SetTransparent(const Value: Boolean);
    procedure SetFontShadow(const Value: TGUIShadow);
    procedure SetOversampling(const Value: TFontOversampling);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;

    procedure ControlChanged; override;
    procedure TransparentChanged; virtual;

    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
    procedure FontChangedHandler(Sender: TObject);

    procedure BackBufferChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure RenderRoundedFrameRectangle(PixelMap: TGuiCustomPixelMap);

    // mouse input
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property FontShadow: TGUIShadow read GetFontShadow write SetFontShadow;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TGuiSliderGDI = class(TCustomGuiSliderGDI)
  published
    property Align;
    property Anchors;
    property AutoColor;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property CurveMapping;
    property DefaultValue;
    property Digits;
    property Direction;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property FontOversampling;
    property FontShadow;
    property Max;
    property Min;
    property ReadOnly;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Value;
    property ShowHint;
    property ShowText;
    property SlideColor;
    property Transparent;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
    property OnChange;
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
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnGetText;
  end;

  TGuiSlider = class(TGuiSliderGDI);

implementation

uses
  Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Common, DAV_GuiBlend,
  DAV_Approximations;


{ TCustomGuiSlider }

constructor TCustomGuiSlider.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle  := ControlStyle + [csOpaque];
 TabStop       := False; // Ensure we're not a tab-stop
 Color         := clBtnFace;

 FAutoColor    := False;
 FSlideColor   := $606060;
 FBorderColor  := $202020;
 FBorderWidth  := 1;
 FDigits       := 4;
 FDirection    := sdLeftToRight;
 FShowText     := False;

 FMinimum          :=   0;
 FMaximum          := 100;
 FValue        :=  50;
 FDefaultValue :=  50;

 FCurveMapping     :=  0;
 FCurveMappingExp  :=  1;

 {$IFDEF FPC}
 DoubleBuffered := True;
 {$ENDIF}
end;

procedure TCustomGuiSlider.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiSlider then
  with TCustomGuiSlider(Dest) do
   begin
    FAutoColor       := Self.FAutoColor;
    FBorderColor     := Self.FBorderColor;
    FBorderRadius    := Self.FBorderRadius;
    FBorderWidth     := Self.FBorderWidth;
    FCaption         := Self.FCaption;
    FCurveMapping    := Self.FCurveMapping;
    FCurveMappingExp := Self.FCurveMappingExp;
    FDefaultValue    := Self.FDefaultValue;
    FMaximum             := Self.FMaximum;
    FMinimum             := Self.FMinimum;
    FSlideColor      := Self.FSlideColor;
    FValue           := Self.FValue;
    FOnPaint         := Self.FOnPaint;
    FOnChange        := Self.FOnChange;
   end;
end;

procedure TCustomGuiSlider.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValueProperty,
    WriteValueProperty, Value = 0);
  Filer.DefineProperty('DefaultValue', ReadDefaultValueProperty,
    WriteDefaultValueProperty, DefaultValue = 0);
  Filer.DefineProperty('Max', ReadMaxProperty,
    WriteMaxProperty, Max = 0);
end;

procedure TCustomGuiSlider.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiSlider.SetBorderRadius(const Value: Single);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiSlider.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
  end;
end;

procedure TCustomGuiSlider.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiSlider.SetCaption(const Value: TCaption);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiSlider.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   CurveMappingChanged;
  end;
end;

procedure TCustomGuiSlider.CurveMappingChanged;
begin
 FCurveMappingExp := Power(2, FCurveMapping);
 ControlChanged;
end;

procedure TCustomGuiSlider.SetDefaultValue(Value: Single);
begin
 if not (csLoading in ComponentState) then
  begin
   if Value < FMinimum then Value := FMinimum else
   if Value > FMaximum then Value := FMaximum;
  end;

 FDefaultValue := Value;
end;

procedure TCustomGuiSlider.SetDigits(const Value: Integer);
begin
 if FDigits <> Value then
  begin
   FDigits := Value;
   DigitsChanged;
  end;
end;

procedure TCustomGuiSlider.SetDirection(const Value: TSliderDirection);
begin
 if FDirection <> Value then
  begin
   FDirection := Value;
   DirectionChanged;
  end;
end;

procedure TCustomGuiSlider.DigitsChanged;
begin
 if ShowText
  then ControlChanged;
end;

procedure TCustomGuiSlider.DirectionChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.SetMax(const Value: Single);
begin
  if Value <> FMaximum then
  begin
   {$IFNDEF FPC}
   if (Value < FMinimum) and not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMinimum + 1, MaxInt]);
   {$ENDIF}

   FMaximum := Value;
   MaximumChanged;
  end;
end;

procedure TCustomGuiSlider.SetMin(const Value: Single);
begin
  if Value <> FMinimum then
  begin
   {$IFNDEF FPC}
   if (Value > FMaximum) and not (csLoading in ComponentState) then
    raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMaximum - 1]);
   {$ENDIF}

   FMinimum := Value;
  end;
end;

procedure TCustomGuiSlider.MaximumChanged;
begin
 if FValue > FMaximum then FValue := FMaximum;
 if FDefaultValue > FMaximum then FDefaultValue := FMaximum;
 ControlChanged;
end;

procedure TCustomGuiSlider.MinimumChanged;
begin
 if FValue < FMinimum then FValue := FMinimum;
 if FDefaultValue < FMinimum then FDefaultValue := FMinimum;
 ControlChanged;
end;

procedure TCustomGuiSlider.SetValue(Value: Single);
begin
  if Value < FMinimum then Value := FMinimum else
  if Value > FMaximum then Value := FMaximum;

  if FValue <> Value then
   begin
    FValue := Value;
    PositionChanged;
   end;
end;

procedure TCustomGuiSlider.PositionChanged;
begin
 if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
 ControlChanged;
end;

procedure TCustomGuiSlider.ReadDefaultValueProperty(Reader: TReader);
begin
 FDefaultValue := Reader.ReadFloat;
end;

procedure TCustomGuiSlider.ReadMaxProperty(Reader: TReader);
begin
 FMaximum := Reader.ReadFloat;
end;

procedure TCustomGuiSlider.ReadValueProperty(Reader: TReader);
begin
 FValue := Reader.ReadFloat;
end;

procedure TCustomGuiSlider.SetShowText(const Value: Boolean);
begin
 if FShowText <> Value then
  begin
   FShowText := Value;
   ShowTextChanged;
  end;
end;

procedure TCustomGuiSlider.ShowTextChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.SetSlideColor(const Value: TColor);
begin
 if FSlideColor <> Value then
  begin
   FSlideColor := Value;
   SlideColorChanged;
  end;
end;

procedure TCustomGuiSlider.AutoColorChanged;
begin
 if FAutoColor then
  begin
(*
   FChartColor32 := Lighten(Color32(Color),60);
   FChartColor := WinColor(FChartColor32);
*)
   ControlChanged;
  end;
end;

procedure TCustomGuiSlider.BorderWidthChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.CaptionChanged;
begin
 if FShowText
  then ControlChanged;
end;

procedure TCustomGuiSlider.ControlChanged;
begin
 Invalidate;
end;

procedure TCustomGuiSlider.BorderColorChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.BorderRadiusChanged;
begin
 ControlChanged;
end;

procedure TCustomGuiSlider.SlideColorChanged;
begin
 ControlChanged;
end;

function TCustomGuiSlider.MapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), FCurveMappingExp)
  else Result :=  Power(Abs(Value), FCurveMappingExp);
end;

function TCustomGuiSlider.UnmapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(Abs(Value), 1 / FCurveMappingExp)
end;

procedure TCustomGuiSlider.WriteDefaultValueProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FDefaultValue);
end;

procedure TCustomGuiSlider.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMaximum);
end;

procedure TCustomGuiSlider.WriteValueProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FValue);
end;


{ TCustomGuiSliderGDI }

constructor TCustomGuiSliderGDI.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 FBuffer           := TGuiPixelMapMemory.Create;
 FBackBuffer       := TGuiPixelMapMemory.Create;
 FTransparent      := False;
end;

destructor TCustomGuiSliderGDI.Destroy;
begin
 FreeAndNil(FGuiFont);
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited Destroy;
end;

procedure TCustomGuiSliderGDI.FontChangedHandler(Sender: TObject);
begin
 ControlChanged;
end;

function TCustomGuiSliderGDI.GetFontShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

function TCustomGuiSliderGDI.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

procedure TCustomGuiSliderGDI.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiSliderGDI then
  with TCustomGuiSliderGDI(Dest) do
   begin
    FTransparent     := Self.FTransparent;
    FBuffer.Assign(Self.FBuffer);
    FBackBuffer.Assign(Self.FBackBuffer);
   end;
end;

procedure TCustomGuiSliderGDI.ControlChanged;
begin
 FUpdateBuffer := True;
 inherited;
end;

procedure TCustomGuiSliderGDI.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 ControlChanged;
end;

{$IFNDEF FPC}
procedure TCustomGuiSliderGDI.CMFontChanged(var Message: TMessage);
{$ELSE}
procedure TCustomGuiSliderGDI.CMFontChanged(var Message: TLMessage);
{$ENDIF}
begin
 inherited;
 FGuiFont.Font.Assign(Font);
end;


procedure TCustomGuiSliderGDI.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NormalizedPosition : Single;
begin
 if (not FReadOnly) and (Button = mbLeft) then
  begin
   NormalizedPosition := X / (Width - 1);
   Value := Limit(FMinimum + MapValue(NormalizedPosition) * (FMaximum - FMinimum), FMinimum, FMaximum);
  end;

 inherited;
end;

procedure TCustomGuiSliderGDI.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NormalizedPosition : Single;
begin
 if (not FReadOnly) and (ssLeft in Shift) then
  begin
   NormalizedPosition := X / (Width - 1);
   Value := Limit(FMinimum + MapValue(NormalizedPosition) * (FMaximum - FMinimum), FMinimum, FMaximum);
  end;

 inherited;
end;


// Drawing stuff

procedure TCustomGuiSliderGDI.Paint;
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

procedure TCustomGuiSliderGDI.UpdateBackBuffer;
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

procedure TCustomGuiSliderGDI.UpdateBuffer;
var
  CurrentText : string;
  TextSize    : TSize;
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

 RenderRoundedFrameRectangle(FBuffer);

 if FShowText and Assigned(FGuiFont) then
  begin
   CurrentText := FloatToStrF(FValue, ffGeneral, FDigits, FDigits);
   if FCaption <> ''
    then CurrentText := FCaption + ': ' + CurrentText;

   if Assigned(FOnGetText)
    then FOnGetText(Self, CurrentText);

   TextSize := FGuiFont.TextExtent(CurrentText);
   FGuiFont.TextOut(CurrentText, FBuffer, (Width - TextSize.cx) div 2,
     (Height - TextSize.cy) div 2);
  end;
end;

procedure TCustomGuiSliderGDI.RenderRoundedFrameRectangle(
  PixelMap: TGuiCustomPixelMap);
var
  X, Y, XPos        : Integer;
  ScnLne            : array [0..1] of PPixel32Array;
  SliderColor       : TPixel32;
  BackColor         : TPixel32;
  BorderColor       : TPixel32;
  CombColor         : TPixel32;
  InnerColor        : TPixel32;
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
   SliderColor := ConvertColor(FSlideColor);
   BackColor := ConvertColor(Color);
   if FBorderWidth > 0
    then BorderColor := ConvertColor(FBorderColor)
    else BorderColor := SliderColor;

   XPos := Round(Width * UnmapValue((FValue - FMinimum) / (FMaximum - FMinimum)));

   // initialize variables
   Radius := FBorderRadius;
   if 0.5 * Width < Radius then Radius := 0.5 * Width;
   if 0.5 * Height < Radius then Radius := 0.5 * Height;
   BorderWidth := Math.Max(FBorderWidth, 1);

   if Radius = 0 then
    begin
     Y := Trunc(FBorderWidth);
     PixelMap.FillRect(Rect(Y, Y, XPos - 1, Height - Y), SliderColor);
     PixelMap.FillRect(Rect(XPos, Y, Width - Y, Height - Y), BackColor);
     for X := 0 to Y - 1
      do PixelMap.FrameRect(Rect(X, X, Width - 1 - X, Height - 1 - X), BorderColor);
     BorderColor.A := Round($FF * (FBorderWidth - Y));
     PixelMap.FrameRect(Rect(Y, Y, Width - 1 - Y, Height - 1 - Y), BorderColor);
     Exit;
    end;

   RadMinusBorderOne := BranchlessClipPositive(Radius - BorderWidth);
   SqrRadMinusBorder := Sqr(BranchlessClipPositive(Radius - BorderWidth - 1));
   SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));

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
       if X < XPos
        then CombColor := SliderColor
        else CombColor := BackColor;

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
       if X < XPos
        then CombColor := SliderColor
        else CombColor := BackColor;

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

procedure TCustomGuiSliderGDI.Resize;
begin
 inherited;

 if Assigned(FBuffer)
  then FBuffer.SetSize(Self.Width, Self.Height);

 if Assigned(FBackBuffer)
  then FBackBuffer.SetSize(Self.Width, Self.Height);

 BackBufferChanged;
end;

procedure TCustomGuiSliderGDI.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiSliderGDI.SetFontShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TCustomGuiSliderGDI.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TCustomGuiSliderGDI.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiSliderGDI.TransparentChanged;
begin
 ControlChanged;
end;

end.
