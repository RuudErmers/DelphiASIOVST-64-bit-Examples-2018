unit DAV_GuiSelectBox;

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
  {$IFDEF FPC} LCLIntf, LMessages, Types, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Controls, Graphics, Menus, DAV_GuiPixelMap, DAV_GuiCustomControl,
  DAV_GuiFont, DAV_GuiShadow, DAV_FixedPoint;

type
  TCustomGuiSelectBox = class(TGuiCustomControl)
  private
    FAlignment        : TAlignment;
    FAlternate        : Boolean;
    FArrowColor       : TColor;
    FArrowWidth       : Integer;
    FArrowButtonWidth : Integer;
    FBackColor        : TColor;
    FButtonColor      : TColor;
    FBorderColor      : TColor;
    FBorderWidth      : Single;
    FBorderRadius     : Single;
    FGuiFont          : TGuiOversampledGDIFont;
    FOnChange         : TNotifyEvent;
    FSelectBoxColor   : TColor;
    FLoop             : Boolean;
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAlternate(const Value: Boolean);
    procedure SetArrowColor(const Value: TColor);
    procedure SetArrowWidth(const Value: Integer);
    procedure SetButtonColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetBorderRadius(Value: Single);
    procedure SetSelectBoxColor(const Value: TColor);
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetShadow(const Value: TGUIShadow);
    procedure SetBackColor(const Value: TColor);
  protected
    procedure AlignmentChanged; virtual;
    procedure AlternateChanged; virtual;
    procedure ArrowColorChanged; virtual;
    procedure ArrowWidthChanged; virtual;
    procedure BackColorChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure ButtonColorChanged; virtual;
    procedure ButtonWidthChanged; virtual;
    procedure SelectBoxColorChanged; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure RenderSelectBox(PixelMap: TGuiCustomPixelMap);
    procedure RenderText(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure UpdateBuffer; override;

    {$IFDEF FPC}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBtnHighlight;
    property ArrowWidth: Integer read FArrowWidth write SetArrowWidth default 1;
    property Alternate: Boolean read FAlternate write SetAlternate default False;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnHighlight;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default clBtnShadow;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnShadow;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Loop: Boolean read FLoop write FLoop default True;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property SelectBoxColor: TColor read FSelectBoxColor write SetSelectBoxColor default clBtnShadow;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiIntegerBox = class(TCustomGuiSelectBox)
  private
    FMaximum : Integer;
    FMinimum : Integer;
    FValue   : Integer;
    procedure SetMaximum(const Value: Integer);
    procedure SetMinimum(const Value: Integer);
    procedure SetValue(Value: Integer);
  protected
    procedure RenderText(PixelMap: TGuiCustomPixelMap); override;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure ValueChanged; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Value: Integer read FValue write SetValue default 0;

    property Align;
    property Alignment;
    property Alternate;
    property Anchors;
    property ArrowColor;
    property ArrowWidth;
    property BackColor;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ButtonColor;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property Loop;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property SelectBoxColor;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property Transparent;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
  end;

  TGuiSelectBox = class(TCustomGuiSelectBox)
  private
    FItemIndex : Integer;
    FItems     : TStrings;
    FPopupMenu : TPopupMenu;
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure MenuItemClick(Sender: TObject);
  protected
    procedure ItemIndexChanged; virtual;

    procedure NextItem; virtual;
    procedure PreviousItem; virtual;

    procedure RenderText(PixelMap: TGuiCustomPixelMap); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowMenu;
    procedure Clear;
  published
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;

    property Align;
    property Alignment;
    property Alternate;
    property Anchors;
    property ArrowColor;
    property ArrowWidth;
    property BackColor;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ButtonColor;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property Loop;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property SelectBoxColor;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property Transparent;
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
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
  end;

implementation

uses
  Math, SysUtils, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Approximations, DAV_Common,
  DAV_Math, DAV_Complex, DAV_GuiCommon, DAV_GuiBlend, DAV_GuiFixedPoint;

{ TCustomGuiSelectBox }

constructor TCustomGuiSelectBox.Create(AOwner: TComponent);
begin
 inherited;
 FArrowColor     := clBtnHighlight;
 FArrowWidth     := 1;
 FAlternate      := False;
 FLoop           := True;
 FAlignment      := taCenter;
 FBorderRadius   := 2;
 FBorderWidth    := 2;
 FBackColor      := clBtnShadow;
 FBorderColor    := clBtnHighlight;
 FButtonColor    := clBtnShadow;
 FSelectBoxColor := clBtnHighlight;
 FSelectBoxColor := clBtnShadow;

 // create GUI font
 FGuiFont := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 ButtonWidthChanged;
end;

destructor TCustomGuiSelectBox.Destroy;
begin
 FreeAndNil(FGuiFont);

 inherited;
end;

function TCustomGuiSelectBox.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiSelectBox.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiSelectBox.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.CMFontchanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FGuiFont.Font.Assign(Font);
 BufferChanged;
end;

procedure TCustomGuiSelectBox.RenderSelectBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y              : Integer;
  ScnLn             : array [0..1] of PPixel32Array;
  BackColor         : TPixel32;
  ButtonColor       : TPixel32;
  BorderColor       : TPixel32;
  BgColor           : TPixel32;
  CombColor         : TPixel32;
  ArrowColor        : TPixel32;
  Radius            : Single;
  XStart            : Single;
  BorderWidth       : Single;
  SqrRadMinusBorder : Single;
  RadMinusBorderOne : Single;
  SqrDist, SqrYDist : Single;
  SqrRadMinusOne    : Single;
  Temp              : Single;
  XFixed            : TFixed24Dot8;
  XAdv              : TFixed24Dot8;
  Offset            : Integer;
  ArrowHeight       : Integer;
  ArrowWidth        : Integer;
begin
 with PixelMap do
  begin
   BackColor := ConvertColor(FBackColor);
   ButtonColor := ConvertColor(FButtonColor);
   BgColor     := ButtonColor;
   if FBorderWidth = 0
    then BorderColor := ButtonColor
    else BorderColor := ConvertColor(FBorderColor);
   ArrowColor := ConvertColor(FArrowColor);

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
     ScnLn[0] := Scanline[Y];
     ScnLn[1] := Scanline[Height - 1 - Y];

     for X := Round((Radius - 1) - XStart) to Round((Width - 1) - (Radius - 1) + XStart) do
      begin
       if (X < FArrowButtonWidth) or (X > Width - 1 - FArrowButtonWidth)
        then BgColor := BackColor
        else BgColor := ButtonColor;

       // calculate squared distance
       if X < (Radius - 1)
        then SqrDist := Sqr(X - (Radius - 1)) + SqrYDist else

       if X > (Width - 1) - (Radius - 1)
        then SqrDist := Sqr(X - (Width - 1) + (Radius - 1)) + SqrYDist
        else SqrDist := SqrYDist;

       if SqrDist < SqrRadMinusBorder
        then CombColor := BgColor
        else
       if SqrDist <= Sqr(RadMinusBorderOne) then
        begin
         Temp := RadMinusBorderOne - FastSqrtBab2(SqrDist);
         CombColor := CombinePixel(BorderColor, BgColor, Round($FF - Temp * $FF));
        end else
       if SqrDist < SqrRadMinusOne
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          CombColor.A := Round($FF * (Radius - FastSqrtBab2(SqrDist)));
         end;

       // lines
       if ((X >= FArrowButtonWidth) and (X < FArrowButtonWidth + FBorderWidth)) or
          ((X > Width - 1 - FArrowButtonWidth - FBorderWidth) and (X <= Width - 1 - FArrowButtonWidth))
        then CombColor := BorderColor;

       BlendPixelInplace(CombColor, ScnLn[0][X]);
       BlendPixelInplace(CombColor, ScnLn[1][X]);
       EMMS;
      end;
    end;

   // non-rounded area
   for Y := Round(Radius) to Height - 1 - Round(Radius) do
    begin
     ScnLn[0] := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       if (X < FArrowButtonWidth) or (X > Width - 1 - FArrowButtonWidth)
        then BgColor := BackColor
        else BgColor := ButtonColor;

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
           CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
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
           CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
          end else
         if (X > Width - 1 - BorderWidth) then
          begin
           Temp := Temp + (X - Width + 1 + BorderWidth) * (1 - Temp);
           CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
          end
         else CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
        end else

       if (X < BorderWidth - 1) or (X > Width - 1 - BorderWidth + 1)
        then CombColor := BorderColor else
       if (X < BorderWidth) then
        begin
         Temp := BorderWidth - X;
         CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
        end else
       if (X > Width - 1 - BorderWidth) then
        begin
         Temp := X - (Width - 1 - BorderWidth);
         CombColor := CombinePixel(BorderColor, BgColor, Round(Temp * $FF));
        end else
       if ((X >= FArrowButtonWidth) and (X < FArrowButtonWidth + FBorderWidth)) or
          ((X > Width - 1 - FArrowButtonWidth - FBorderWidth) and (X <= Width - 1 - FArrowButtonWidth))
        then CombColor := BorderColor
        else CombColor := BgColor;


       BlendPixelInplace(CombColor, ScnLn[0][X]);
       EMMS;
      end;
    end;

   ArrowHeight := Round(0.5 * (Height - FBorderWidth));
   ArrowWidth := Round(0.5 * (FArrowButtonWidth - FBorderWidth));
   Offset := Round(FArrowButtonWidth - ArrowWidth div 2);

   // set advance
   XAdv := ConvertToFixed24Dot8(2 * ArrowWidth / ArrowHeight);

   // set start
   XFixed := CFixed24Dot8Half;
   for Y := 0 to (ArrowHeight div 2) do
    begin
     ScnLn[0] := ScanLine[(Height - ArrowHeight) div 2 + Y];
     ScnLn[1] := ScanLine[(Height + ArrowHeight) div 2 - Y];
     X := FixedFloor(XFixed);
     BlendPixelLine(ArrowColor, @ScnLn[0]^[Offset - X], X);
     BlendPixelInplace(ApplyAlpha(ArrowColor, XFixed.Frac), ScnLn[0]^[Offset - X - 1]);
     BlendPixelLine(ArrowColor, @ScnLn[0]^[Width - Offset], X);
     BlendPixelInplace(ApplyAlpha(ArrowColor, XFixed.Frac), ScnLn[0]^[Width - Offset + X]);
     if ScnLn[0] <> ScnLn[1] then
      begin
       BlendPixelLine(ArrowColor, @ScnLn[1]^[Offset - X], X);
       BlendPixelInplace(ApplyAlpha(ArrowColor, XFixed.Frac), ScnLn[1]^[Offset - X - 1]);
       BlendPixelLine(ArrowColor, @ScnLn[1]^[Width - Offset], X);
       BlendPixelInplace(ApplyAlpha(ArrowColor, XFixed.Frac), ScnLn[1]^[Width - Offset + X]);
      end else Break;
     XFixed := FixedAdd(XFixed, XAdv);
    end;
   EMMS;


  end;
end;

procedure TCustomGuiSelectBox.UpdateBuffer;
begin
 inherited;

 RenderSelectBox(FBuffer);
 RenderText(FBuffer);
end;

procedure TCustomGuiSelectBox.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiSelectBox.AlignmentChanged;
begin
 ButtonWidthChanged;
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetAlternate(const Value: Boolean);
begin
 if Alternate <> Value then
  begin
   FAlternate := Value;
   AlternateChanged;
  end;
end;

procedure TCustomGuiSelectBox.AlternateChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.ButtonWidthChanged;
begin
 case FAlignment of
   taLeftJustify : FArrowButtonWidth := 12 + Round(0.5 * FBorderWidth) + FArrowWidth;
        taCenter : FArrowButtonWidth := Max(Max(FArrowWidth, Round(0.5 * FBorderRadius)) + 4, abs(Font.Height)) + Round(0.5 * FBorderWidth);
  taRightJustify : FArrowButtonWidth := 12 + Round(0.5 * FBorderWidth) + FArrowWidth;
 end;
 Inc(FArrowButtonWidth, FArrowWidth);
end;

procedure TCustomGuiSelectBox.SetArrowColor(const Value: TColor);
begin
 if FArrowColor <> Value then
  begin
   FArrowColor := Value;
   ArrowColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.ArrowColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetArrowWidth(const Value: Integer);
begin
 if FArrowWidth <> Value then
  begin
   FArrowWidth := Value;
   ArrowWidthChanged;
  end;
end;

procedure TCustomGuiSelectBox.ArrowWidthChanged;
begin
 ButtonWidthChanged;
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetBackColor(const Value: TColor);
begin
 if FBackColor <> Value then
  begin
   FBackColor := Value;
   BackColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiSelectBox.SetButtonColor(const Value: TColor);
begin
 if FButtonColor <> Value then
  begin
   FButtonColor := Value;
   ButtonColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.ButtonColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.BackColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.BorderColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TCustomGuiSelectBox.SetSelectBoxColor(const Value: TColor);
begin
 if FSelectBoxColor <> Value then
  begin
   FSelectBoxColor := Value;
   SelectBoxColorChanged;
  end;
end;

procedure TCustomGuiSelectBox.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TCustomGuiSelectBox.SelectBoxColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiSelectBox.SetBorderRadius(Value: Single);
begin
 if Value < 0 then Value := 0;

 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiSelectBox.BorderRadiusChanged;
begin
 ButtonWidthChanged;
 BufferChanged;
end;

procedure TCustomGuiSelectBox.BorderWidthChanged;
begin
 BufferChanged;
end;


{ TGuiIntegerBox }


{ TGuiSelectBox  }

constructor TGuiSelectBox.Create(AOwner: TComponent);
begin
  inherited;

 // create items
 FItems          := TStringList.Create;
 FItemIndex      := -1;
end;

destructor TGuiSelectBox.Destroy;
begin
 FreeAndNil(FItems);

 if Assigned(FPopupMenu)
  then FreeAndNil(FPopupMenu);

 inherited;
end;

procedure TGuiSelectBox.SetItemIndex(Value: Integer);
begin
 if Value < -1 then Value := -1 else
 if Value >= FItems.Count then Value := FItems.Count - 1;
 if FItemIndex <> Value then
  begin
   FItemIndex := Value;
   ItemIndexChanged;
  end;
end;

procedure TGuiSelectBox.ItemIndexChanged;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
 BufferChanged;
end;

procedure TGuiSelectBox.NextItem;
begin
 if FItemIndex < FItems.Count - 1 then ItemIndex := ItemIndex + 1 else
 if Loop and (FItemIndex >= FItems.Count - 1) then ItemIndex := 0;
end;

procedure TGuiSelectBox.PreviousItem;
begin
 if FItemIndex > 0 then ItemIndex := ItemIndex - 1 else
 if Loop and (FItemIndex = 0) then ItemIndex := FItems.Count - 1;
end;

procedure TGuiSelectBox.Clear;
begin
 FItems.Clear;
 FItemIndex := -1;
end;

procedure TGuiSelectBox.RenderText(PixelMap: TGuiCustomPixelMap);
var
  Offset   : Integer;
  TextSize : TSize;
begin
 with PixelMap do
  begin
   // draw text
   Offset := FArrowButtonWidth;
   case FAlignment of
    taLeftJustify :
      if FItemIndex >= 0 then
       begin
        TextSize := FGuiFont.TextExtent(FItems[FItemIndex]);
        FGuiFont.TextOut(FItems[FItemIndex], PixelMap, Offset, (Height - TextSize.cy) div 2);
       end;
    taCenter :
      if FItemIndex >= 0 then
       begin
        TextSize := FGuiFont.TextExtent(FItems[FItemIndex]);
        FGuiFont.TextOut(FItems[FItemIndex], PixelMap,
          (Width - TextSize.cx) div 2, (Height - TextSize.cy) div 2);
       end;
    taRightJustify :
      if FItemIndex >= 0 then
       begin
        TextSize := FGuiFont.TextExtent(FItems[FItemIndex]);
        FGuiFont.TextOut(FItems[FItemIndex], PixelMap,
          Width - Offset - TextSize.cx, (Height - TextSize.cy) div 2);
       end;
   end;
  end;
end;

procedure TGuiSelectBox.SetItems(const Value: TStrings);
begin
 if Assigned(FItems)
  then FItems.Assign(Value)
  else FItems := Value;
 FItemIndex := - 1;
 BufferChanged;
end;

procedure TGuiSelectBox.ShowMenu;
var
  i  : Integer;
  MI : TMenuItem;
begin
 if Assigned(FPopupMenu)
  then FPopupMenu.Items.Clear
  else FPopupMenu := TPopupMenu.Create(Self);
 for i := 0 to FItems.Count - 1 do
  begin
   MI := TMenuItem.Create(FPopupMenu);
   MI.Caption   := FItems[i];
   MI.RadioItem := True;
   MI.Checked   := i = ItemIndex;
   MI.OnClick   := MenuItemClick;
   MI.Tag       := i;
   FPopupMenu.Items.Add(MI);
  end;
 FPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TGuiSelectBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then
  case FAlignment of
   taLeftJustify :
    begin
     if (x > Width - FArrowButtonWidth)
      then PreviousItem else
     if (x > Width - 2 * FArrowButtonWidth)
      then NextItem;

     inherited;
    end;
   taCenter :
    begin
     if (x < FArrowButtonWidth)
      then PreviousItem else
     if (x > Width - FArrowButtonWidth)
      then NextItem;

     inherited;
    end;
   taRightJustify :
    begin
     if (x < FArrowButtonWidth)
      then PreviousItem else
     if (x < 2 * FArrowButtonWidth)
      then NextItem;

     inherited;
    end;
  end else
// if Button = mbRight then
  begin
   inherited;
   ShowMenu;
  end; // else inherited;
end;

procedure TGuiSelectBox.MenuItemClick(Sender: TObject);
begin
 Assert(Sender is TMenuItem);
 with TMenuItem(Sender) do
  begin
   ItemIndex := Tag;
  end;
end;


{ TGuiIntegerBox }

constructor TGuiIntegerBox.Create(AOwner: TComponent);
begin
  inherited;
  FMaximum := 100;
  FMinimum := 0;
  FValue   := 0;
end;

procedure TGuiIntegerBox.SetMaximum(const Value: Integer);
begin
 if FMaximum <> Value then
  begin
   {$IFNDEF FPC}
   if (Value < FMinimum) and not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMinimum + 1, MaxInt]);
   {$ENDIF}

   FMaximum := Value;
   MaximumChanged;
  end;
end;

procedure TGuiIntegerBox.SetMinimum(const Value: Integer);
begin
 if FMinimum <> Value then
  begin
   {$IFNDEF FPC}
   if (Value > FMaximum) and not (csLoading in ComponentState) then
    raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMaximum - 1]);
   {$ENDIF}

   FMinimum := Value;
   MinimumChanged;
  end;
end;

procedure TGuiIntegerBox.SetValue(Value: Integer);
begin
 if Value < FMinimum then Value := FMinimum else
 if Value > FMaximum then Value := FMaximum;

 if FValue <> Value then
  begin
   FValue := Value;
   ValueChanged;
  end;
end;

procedure TGuiIntegerBox.MaximumChanged;
begin
 if FValue > FMaximum
  then Value := Maximum;
end;

procedure TGuiIntegerBox.MinimumChanged;
begin
 if FValue < FMinimum
  then Value := Minimum;
end;

procedure TGuiIntegerBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if Button = mbLeft then
  case FAlignment of
   taLeftJustify :
    begin
     if (x > Width - FArrowButtonWidth)
      then Value := Value - 1 else
     if (x > Width - 2 * FArrowButtonWidth)
      then Value := Value + 1;

     inherited;
    end;
   taCenter :
    begin
     if (x < FArrowButtonWidth)
      then Value := Value - 1 else
     if (x > Width - FArrowButtonWidth)
      then Value := Value + 1;

     inherited;
    end;
   taRightJustify :
    begin
     if (x < FArrowButtonWidth)
      then Value := Value - 1 else
     if (x < 2 * FArrowButtonWidth)
      then Value := Value + 1;

     inherited;
    end;
  end else inherited;
end;

procedure TGuiIntegerBox.ValueChanged;
begin
 if not (csLoading in ComponentState) and Assigned(FOnChange)
  then FOnChange(Self);
 BufferChanged;
end;

procedure TGuiIntegerBox.RenderText(PixelMap: TGuiCustomPixelMap);
var
  Offset   : Integer;
  TextSize : TSize;
begin
 with PixelMap do
  begin
   // draw text
   Offset := FArrowButtonWidth;
   case FAlignment of
    taLeftJustify :
      begin
       TextSize := FGuiFont.TextExtent(IntToStr(FValue));
       FGuiFont.TextOut(IntToStr(FValue), PixelMap, Offset, (Height - TextSize.cy) div 2);
      end;
    taCenter :
      begin
       TextSize := FGuiFont.TextExtent(IntToStr(FValue));
       FGuiFont.TextOut(IntToStr(FValue), PixelMap,
         (Width - TextSize.cx) div 2, (Height - TextSize.cy) div 2);
      end;
    taRightJustify :
      begin
       TextSize := FGuiFont.TextExtent(IntToStr(FValue));
       FGuiFont.TextOut(IntToStr(FValue), PixelMap,
         Width - Offset - TextSize.cx, (Height - TextSize.cy) div 2);
      end;
   end;
  end;
end;

end.
