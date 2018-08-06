unit DAV_GuiEdit;

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
  Classes, Controls, Forms, Graphics, StdCtrls, SysUtils, DAV_GuiCommon,
  DAV_GuiPixelMap, DAV_GuiVector, DAV_GuiFixedPoint,
  DAV_GuiFont, DAV_GuiShadow;

{$DEFINE New}

type
  TGuiCustomControlsEdit = class(TCustomEdit)
  private
    FAlignment        : TAlignment;
    FBorderMargin     : Integer;
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FCanvas           : TCanvas;
    FGuiFont          : TGuiOversampledGDIFont;
    FNative           : Boolean;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;

    FBorderColor      : TColor;
    FBackgroundColor  : TColor;
    FFocusedColor     : TColor;
    FDisabled         : TColor;

    FMouseInControl   : Boolean;
    FParentColor      : Boolean;
    FRoundBorders     : Boolean;
    FShowBorders      : Boolean;
    FTransparent      : Boolean;

    FOnPaint          : TNotifyEvent;

    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderMargin(Value: Integer);
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetNative(const Value: Boolean);
    procedure SetParentColor(Value: Boolean);
    procedure SetRoundBorders(const Value: Boolean);
    procedure SetShowBorders(Value: Boolean);
    procedure SetTransparent(Value: Boolean);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;

    procedure NewAdjustHeight;
  protected
(*
    procedure WndProc(var Msg: TMessage); override;
    function BaseWndProc(Msg: Integer; WParam: Integer = 0; LParam: Integer = 0): Integer;
    function WantKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean; reintroduce; virtual;
*)

    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    {$IFNDEF New}
    procedure WMNCCalcSize(var Message: {$IFDEF FPC}TLMNCCalcSize{$ELSE}TWMNCCalcSize{$ENDIF}); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    {$ENDIF}

    procedure BufferChanged; virtual;
    procedure BackBufferChanged; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; virtual;
    procedure PaintWindow(DC: HDC); override;

    {$IFNDEF New}
    procedure CustomPaintEdit; virtual;
    {$ELSE}
    procedure RenderEdit(PixelMap: TGuiCustomPixelMap);
    {$ENDIF}

    {$IFNDEF FPC}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Canvas: TCanvas read FCanvas;
    property ColorFocused: TColor index 0 read FFocusedColor write SetColors default clWhite;
    property ColorBorder: TColor index 1 read FBorderColor write SetColors default clBlack;
    property ColorBackground: TColor index 2 read FBackgroundColor write SetColors default $00E1EAEB;
    property ColorDisabled: TColor index 3 read FDisabled write SetColors default clBtnShadow;
    property ParentColor: Boolean read FParentColor write SetParentColor default false;
    property Native: Boolean read FNative write SetNative default False;
    property RoundBorders : Boolean read FRoundBorders write SetRoundBorders default False;
    property BorderMargin : Integer read FBorderMargin write SetBorderMargin default 2;
    property Transparent : Boolean read FTransparent write SetTransparent default False;
    property ShowBorders : Boolean read FShowBorders write SetShowBorders default True;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGuiControlsEdit = class(TGuiCustomControlsEdit)
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderMargin;
    property CharCase;
    property Color;
    property ColorBorder;
    property ColorDisabled;
    property ColorBackground;
    property ColorFocused;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property Native;
    property OnEndDock;
    property OnStartDock;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property RoundBorders;
    property ShowBorders;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Transparent;
    property Visible;

    property OnChange;
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
    property OnStartDrag;
    {$IFNDEF FPC}
    property HideSelection;
    property OEMConvert;
    {$ENDIF}
  end;

implementation

{$IFDEF COMPILER_7_UP}
uses
  Types;
{$ENDIF}

function ShiftStateToKeyData(Shift: TShiftState): Longint;
const
  AltMask = $20000000;
  CtrlMask = $10000000;
  ShiftMask = $08000000;
begin
 Result := 0;
 if ssAlt   in Shift then Result := Result or AltMask;
 if ssCtrl  in Shift then Result := Result or CtrlMask;
 if ssShift in Shift then Result := Result or ShiftMask;
end;

constructor TGuiCustomControlsEdit.Create(AOwner: TComponent);
begin
 inherited;

 ControlStyle := ControlStyle - [csFramed] + [csOpaque, csReplicatable];

 // create buffers (& set size)
 FBuffer     := TGuiPixelMapMemory.Create;
 FBackBuffer := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(Width, Height);
 FBackBuffer.SetSize(Width, Height);

 // create font
 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 // create control canvas
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;

 ParentFont       := True;
 FFocusedColor    := clWhite;
 FBorderColor     := clBlack;
 FBackgroundColor := clBtnFace;
 FParentColor     := True;
 FNative          := False;
 FBorderMargin    := 2;
 FTransparent     := False;
 FShowBorders     := True;
 AutoSize         := False;
 Ctl3D            := False;
 BorderStyle      := bsNone;
 BorderWidth      := 0;
 {$IFDEF FPC}
 DoubleBuffered   := True;
 {$ENDIF}

 SetBounds(0, 0, 121, 19);
end;

destructor TGuiCustomControlsEdit.Destroy;
begin
 FreeAndNil(FCanvas);
 FreeAndNil(FGuiFont);

 // dispose buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);

 inherited;
end;

{$IFNDEF FPC}
procedure TGuiCustomControlsEdit.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);

 case FAlignment of
         taCenter: Params.Style := (Params.Style and not $3) or (ES_CENTER and $3);
    taLeftJustify: Params.Style := (Params.Style and not $3) or (ES_LEFT and $3);
   taRightJustify: Params.Style := (Params.Style and not $3) or (ES_RIGHT and $3);
 end;

 {$IFDEF New}
 Params.Style := Params.Style or ES_MULTILINE;
 {$ENDIF}
end;
{$ENDIF}

procedure TGuiCustomControlsEdit.FontChangedHandler(Sender: TObject);
begin
// BufferChanged;
end;

procedure TGuiCustomControlsEdit.SetParentColor(Value: Boolean);
begin
 if Value <> FParentColor then
  begin
   FParentColor := Value;
   if FParentColor then
    begin
     if Parent <> nil
      then FBackgroundColor := TForm(Parent).Color;
     Invalidate;
    end;
  end;
end;

procedure TGuiCustomControlsEdit.CMSysColorChange(var Message: TMessage);
begin
(*
 if FParentColor then
  if Parent <> nil
   then FBackgroundColor := TForm(Parent).Color;
*)
 inherited;
end;

procedure TGuiCustomControlsEdit.CMParentColorChanged(var Message: TWMNoParams);
begin
(*
 if FParentColor then
  if Assigned(Parent)
   then FBackgroundColor := TForm(Parent).Color;
*)
 inherited;
end;

procedure TGuiCustomControlsEdit.CMMouseEnter(var Message: TMessage);
begin
 inherited;
 if (GetActiveWindow <> 0) and not Native then
  begin
   FMouseInControl := True;
   Invalidate;
  end;
end;

procedure TGuiCustomControlsEdit.CMMouseLeave(var Message: TMessage);
begin
 inherited;

 if not FNative then
  begin
   FMouseInControl := False;
   BufferChanged;
  end;
end;

procedure TGuiCustomControlsEdit.SetColors(Index: Integer; Value: TColor);
begin
 case Index of
  0: FFocusedColor    := Value;
  1: FBorderColor     := Value;
  2: FBackgroundColor := Value;
  3: FDisabled        := Value;
 end;
 if Index = 2 then FParentColor := False;
 inherited;
end;

procedure TGuiCustomControlsEdit.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
 DC := GetDC(0);
 SaveFont := SelectObject(DC, Font.Handle);
 GetTextMetrics(DC, Metrics);
 SelectObject(DC, SaveFont);
 ReleaseDC(0, DC);
 Height := Metrics.tmHeight + 6;
end;

procedure TGuiCustomControlsEdit.Paint;
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

procedure TGuiCustomControlsEdit.PaintWindow(DC: HDC);
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

procedure TGuiCustomControlsEdit.Resize;
begin
 inherited;

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

procedure TGuiCustomControlsEdit.Loaded;
begin
 inherited;
 Resize;

 if not FNative
  then if not(csDesigning in ComponentState) then NewAdjustHeight;
end;

procedure TGuiCustomControlsEdit.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TGuiCustomControlsEdit.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TGuiCustomControlsEdit.CMFontChanged(var Message: TMessage);
begin
 inherited;

 if not((csDesigning in ComponentState) and (csLoading in ComponentState)) and not Native
  then NewAdjustHeight;
end;

procedure TGuiCustomControlsEdit.WMKillFocus(var Message: TWMKillFocus);
begin
 inherited;
 Invalidate;
end;

{$IFNDEF New}
procedure TGuiCustomControlsEdit.WMNCCalcSize(var Message: {$IFDEF FPC}TLMNCCalcSize{$ELSE}TWMNCCalcSize{$ENDIF});
begin
 inherited;

 if not FNative
  then InflateRect(Message.CalcSize_Params^.rgrc[0], -FBorderMargin, -FBorderMargin);
end;

procedure TGuiCustomControlsEdit.WMNCPaint(var Message: TWMNCPaint);
begin
 inherited;
 Invalidate;
end;
{$ENDIF}

procedure TGuiCustomControlsEdit.SetNative(const Value: Boolean);
begin
 if FNative <> Value then
  begin
   FNative := Value;
   if FNative then
    begin
     BorderStyle := bsSingle;
     BorderWidth := 1;
     RecreateWnd;
     Invalidate;
    end
   else
    begin
     BorderStyle := bsNone;
     BorderWidth := 0;
     RecreateWnd;
     BufferChanged;
    end;
  end;
end;

procedure TGuiCustomControlsEdit.SetRoundBorders(const Value: Boolean);
begin
 if FRoundBorders <> Value then
  begin
   FRoundBorders := Value;
   Invalidate;
  end;
end;

procedure TGuiCustomControlsEdit.SetAlignment(Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   RecreateWnd;
   if not FNative
    then BufferChanged;
  end;
end;

procedure TGuiCustomControlsEdit.SetBorderMargin(Value: Integer);
begin
 if Value < 1 then Value := 1 else
 if Value > (Height div 2) then Value := (Height div 2);
 if FRoundBorders and (Value < 2) then Value := 2;
 if Value <> FBorderMargin then
  begin
   FBorderMargin := Value;
   Invalidate;
  end;
end;

procedure TGuiCustomControlsEdit.SetTransparent(Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   Invalidate;
  end;
end;

procedure TGuiCustomControlsEdit.UpdateBackBuffer;
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

procedure TGuiCustomControlsEdit.UpdateBuffer;
begin
 FUpdateBuffer := False;

 inherited;

 // check whether a buffer or a back buffer is assigned
 if not Assigned(FBuffer) or not Assigned(FBackBuffer)
  then Exit;

 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy entire back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));

 RenderEdit(FBuffer);
end;

procedure TGuiCustomControlsEdit.SetShowBorders(Value: Boolean);
begin
 if FShowBorders <> Value then
  begin
   FShowBorders := Value;
   Invalidate;
  end;
end;

procedure TGuiCustomControlsEdit.WMPaint(var Message: TWMPaint);
begin
 if not FNative then
  begin
   {$IFDEF New}
   ControlState := ControlState + [csCustomPaint];
   inherited;
   ControlState := ControlState - [csCustomPaint];
   {$ELSE}
   CustomPaintEdit;
   {$ENDIF}
  end
 else inherited;
end;

procedure TGuiCustomControlsEdit.WMSetFocus(var Message: TWMSetFocus);
begin
 inherited;
 Invalidate;
end;

{$IFNDEF New}
procedure TGuiCustomControlsEdit.CustomPaintEdit;
var
  i  : Integer;
  R  : TRect;
  CC : TControlCanvas;
begin
 CC := TControlCanvas.Create;
 with CC do
  try
   Handle := GetWindowDC(Self.Handle);
   GetWindowRect(Self.Handle, R);
   OffsetRect(R, -R.Left, -R.Top);

(*
   if FTransparent
    then DrawParentImage(Self, CC);
*)

   if FRoundBorders then
    begin
     if (not (csDesigning in ComponentState) and
      (Focused or(FMouseInControl and not
      (Screen.ActiveControl is TGuiCustomControlsEdit)))) then
      begin
       { Focus }
       if not FTransparent
        then Color := FFocusedColor;
       Pen.Color := FFocusedColor;
       Brush.Color := FFocusedColor;
      end
     else
      begin
       { non Focus }
       if not FTransparent
        then Color := FBackgroundColor;
       Pen.Color := FBackgroundColor;
       Brush.Color := FBackgroundColor;
      end;
     with R do
      begin
       Right  := Right  - 1;
       Bottom := Bottom - 1;
       if not FTransparent then
        begin
         MoveTo(R.Left  + 2, R.Top    + 1);
         LineTo(R.Right - 1, R.Top    + 1);
         MoveTo(R.Left  + 2, R.Bottom - 1);
         LineTo(R.Right - 1, R.Bottom - 1);
         MoveTo(R.Left  + 1, R.Top    + 2);
         LineTo(R.Left  + 1, R.Bottom    );
         MoveTo(R.Right - 1, R.Top    + 1);
         LineTo(R.Right - 1, R.Bottom    );
         Pixels[R.Left     , R.Top       ] := FBackgroundColor;
         Pixels[R.Left  + 1, R.Top       ] := FBackgroundColor;
         Pixels[R.Left     , R.Top    + 1] := FBackgroundColor;
         Pixels[R.Right    , R.Top       ] := FBackgroundColor;
         Pixels[R.Right - 1, R.Top       ] := FBackgroundColor;
         Pixels[R.Right    , R.Top    + 1] := FBackgroundColor;

         Pixels[R.Left     , R.Bottom    ] := FBackgroundColor;
         Pixels[R.Left  + 1, R.Bottom    ] := FBackgroundColor;
         Pixels[R.Left     , R.Bottom - 1] := FBackgroundColor;
         Pixels[R.Right    , R.Bottom    ] := FBackgroundColor;
         Pixels[R.Right - 1, R.Bottom    ] := FBackgroundColor;
         Pixels[R.Right    , R.Bottom - 1] := FBackgroundColor;
        end;

       if FShowBorders then
        if Enabled
         then Pen.Color := FBorderColor
         else Pen.Color := FFocusedColor
        else Pen.Color := Pen.Color;
       Brush.Color     := Self.Color;
       PolyLine([Point(Left  + 1, Bottom - 1),
                 Point(Left     , Bottom - 2),
                 Point(Left     , Top    + 2),
                 Point(Left  + 2, Top       ),
                 Point(Right - 2, Top       ),
                 Point(Right    , Top    + 2)]);

       if FShowBorders then
        if Enabled
         then Pen.Color := FBorderColor
         else Pen.Color := FFocusedColor
        else Pen.Color := Brush.Color;
       PolyLine([Point(Right - 1, Top    + 1),
                 Point(Right    , Top    + 2),
                 Point(Right    , Bottom - 2),
                 Point(Right - 2, Bottom    ),
                 Point(Left  + 2, Bottom    ),
                 Point(Left     , Bottom - 2)]);
      end;
    end
   else
    begin
     if FShowBorders then
      begin
       if Enabled
        then Brush.Color := FBorderColor
        else Brush.Color := FFocusedColor;
       FrameRect(R);
      end
     else
      begin
       Brush.Color := Self.Color;
       FrameRect(R);
      end;
     if (not (csDesigning in ComponentState) and
      (Focused or(FMouseInControl and not
      (Screen.ActiveControl is TGuiCustomControlsEdit))))
      then Brush.Color := FFocusedColor
      else Brush.Color := FBackgroundColor;
     if not FTransparent
      then Color := Brush.Color;
     for i := 1 to FBorderMargin - 1 do
      begin
       InflateRect(r, -1, -1);
       FrameRect(R);
      end;
     Pen.Color   := Brush.Color;
    end;

   Brush.Style := bsClear;
   if FTransparent
    then TextOut(2, 2, Text);
  finally
   ReleaseDC(Self.Handle,Handle);
   Free;
  end;
end;
{$ENDIF}

procedure TGuiCustomControlsEdit.RenderEdit(PixelMap: TGuiCustomPixelMap);
var
  ColorBG : TPixel32;
begin
 with PixelMap do
  begin
   ColorBG := ConvertColor(FBackgroundColor);
   FillRect(ClientRect, ColorBG);
  end;
end;

end.
