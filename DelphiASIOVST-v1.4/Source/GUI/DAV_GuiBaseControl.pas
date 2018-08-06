unit DAV_GuiBaseControl;

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
//  The initial developer of this code is Maik Menz and Christian-W. Budde    //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, Controls, ExtCtrls, DAV_GuiCommon, DAV_Classes;

type
  TGuiOnDragMouseMove = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;

  TGuiMouseButtonState = record
    ButtonDown     : Boolean;
    EventX, EventY : Integer;
    ShiftState     : TShiftState;
  end;

  TGuiMouseState = class
    LeftBtn,
    MiddleBtn,
    RightBtn     : TGuiMouseButtonState;
    LastEventX,
    LastEventY   : Integer;
  end;

  TGuiMouseStateClass = class of TGuiMouseState;

  TGuiAntiAlias = (gaaNone, gaaLinear2x, gaaLinear3x, gaaLinear4x, gaaLinear8x, gaaLinear16x);

  TGuiStitchKind = (skHorizontal, skVertical);

  TGUIShadow = class(TPersistent)
  private
    FBlur         : Byte;
    FOffset       : TPoint;
    FTransparency : Byte;
    FVisible      : Boolean;
    FOnChange     : TNotifyEvent;
    FColor        : TColor;
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
    procedure SetBlur(const Value: Byte);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetTransparency(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetOffset(const Value: TPoint);
    procedure SetColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create; virtual;
    property Offset : TPoint read FOffset write SetOffset;
  published
    property Blur: Byte read FBlur write SetBlur default 4;
    property Color: TColor read FColor write SetColor;
    property OffsetX: Integer read GetOffsetX write SetOffsetX default 1;
    property OffsetY: Integer read GetOffsetY write SetOffsetY default 1;
    property Transparency: Byte read FTransparency write SetTransparency default $FF;
    property Visible: Boolean read FVisible write SetVisible default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBufferedGraphicControl = class(TGraphicControl)
  protected
    FBuffer       : TBitmap;
    FOnPaint      : TNotifyEvent;

    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave : TNotifyEvent;
    FOnMouseEnter : TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ELSE}
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TLmEraseBkgnd); message LM_ERASEBKGND;
    {$ENDIF}

    procedure Downsample2xBitmap(var Bitmap: TBitmap);
    procedure Downsample3xBitmap(var Bitmap: TBitmap);
    procedure Downsample4xBitmap(var Bitmap: TBitmap);
    procedure Upsample2xBitmap(var Bitmap: TBitmap);
    procedure Upsample3xBitmap(var Bitmap: TBitmap);
    procedure Upsample4xBitmap(var Bitmap: TBitmap);
    procedure Resize; override;
    procedure ResizeBuffer; dynamic;
    procedure UpdateBuffer; dynamic; abstract;
    procedure FontChanged; virtual;

    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
    {$ENDIF}
  end;

(*
  TBufferedWinControl = class(TWinControl)
  protected
    FBuffer   : TBitmap;
    FOnPaint  : TNotifyEvent;

    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}

    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}

    procedure Downsample2xBitmap(var Bitmap: TBitmap);
    procedure Downsample4xBitmap(var Bitmap: TBitmap);
    procedure Upsample2xBitmap(var Bitmap: TBitmap);
    procedure Upsample4xBitmap(var Bitmap: TBitmap);
    procedure Resize; override;
    procedure ResizeBuffer; dynamic;
    procedure UpdateBuffer; dynamic; abstract;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    {$IFNDEF COMPILER10_UP}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
  end;
*)

  TCustomGuiBaseControl = class(TBufferedGraphicControl)
  protected
    FLineColor   : TColor;
    FLineWidth   : Integer;
    FTransparent : Boolean;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetLineWidth(Value: Integer); virtual;
    procedure SetLineColor(Value: TColor); virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;

  TCustomGuiBaseMouseControl = class(TCustomGuiBaseControl)
  protected
    FRedrawTimer            : TTimer;
    FTimerMustRedraw        : Boolean;
    FReleaseMouseBtnOnLeave : Boolean;
    FOnMouseLeave           : TNotifyEvent;
    FOnMouseEnter           : TNotifyEvent;
    FOnDragMouseMove        : TGuiOnDragMouseMove;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure CreateMouseClass(MouseStateClass: TGuiMouseStateClass); dynamic;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure DragMouseMoveMiddle(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure DragMouseMoveRight(Shift: TShiftState; X, Y: Integer); dynamic;

    {$IFNDEF FPC}

    {$IFNDEF Delphi7_Up}
    procedure WMMouseWheel(var Message : TWMMouseWheel);  message WM_MOUSEWHEEL;
    procedure DoOnMouseWheel(Delta: Integer; MousePos: TPoint); virtual;
    procedure MouseWheelHandler(var Message: TMessage); dynamic;
    {$ENDIF}

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    {$ELSE}
    procedure CMMouseEnter(var Message: TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    {$ENDIF}

    procedure SetRedrawInterval(Value: Integer); virtual;
    function  GetRedrawInterval: Integer; virtual;
  public
    MouseState: TGuiMouseState;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; MouseStateClass: TGuiMouseStateClass); reintroduce; overload;
    destructor Destroy; override;

    procedure UpdateGuiTimer(Sender: TObject); virtual;

    property RedrawInterval: Integer read GetRedrawInterval write SetRedrawInterval default 0;
    property ReleaseMouseBtnOnLeave: Boolean read FReleaseMouseBtnOnLeave write FReleaseMouseBtnOnLeave default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnDragMouseMove: TGuiOnDragMouseMove read FOnDragMouseMove write FOnDragMouseMove;
  end;

  TCustomGuiBaseAntialiasedControl = class(TCustomGuiBaseMouseControl)
  private
    FAntiAlias  : TGuiAntiAlias;
    FOSValue    : Integer;
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
  protected
    procedure AntialiasChanged; virtual;
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);
    property OversamplingFactor: Integer read FOSValue;
  public
    constructor Create(AOwner: TComponent); override;
    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
  end;

  TGuiBaseControl = class(TCustomGuiBaseMouseControl)
  published
    property Enabled;
    property Align;
    property Anchors;
    property Constraints;
    property ShowHint;
    property Visible;
    property PopupMenu;
    property DragKind;
    property DragCursor;
    property DragMode;

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
    {$IFDEF Delphi6_Up}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDragMouseMove;
  end;

procedure CopyParentImage(Control: TControl; Dest: TCanvas);

implementation

uses
  SysUtils;

resourcestring
  RCStrNotSupported = 'not supported';

type
  TParentControl = class(TWinControl);

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count  : Integer;
  SaveIndex : Integer;
  DC        : THandle;
  Pnt       : TPoint;
  R, SelfR  : TRect;
  CtlR      : TRect;
begin
 {$IFNDEF FPC}
 if (Control = nil) or (Control.Parent = nil) then Exit;
 Count := Control.Parent.ControlCount;
 DC := Dest.Handle;
 {$IFDEF WIN32}
 with Control.Parent do ControlState := ControlState + [csPaintCopy];
 try
 {$ENDIF}
   with Control do
    begin
     SelfR := Bounds(Left, Top, Width, Height);
     Pnt.X := -Left; Pnt.Y := -Top;
    end;
   { Copy parent control image }
   SaveIndex := SaveDC(DC);
   try
    SetViewportOrgEx(DC, Pnt.X, Pnt.Y, nil);
    IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight);
    with TParentControl(Control.Parent) do
     begin
      Perform(WM_ERASEBKGND, DC, 0);
      PaintWindow(DC);
     end;
   finally
    RestoreDC(DC, SaveIndex);
   end;
   { Copy images of graphic controls }
   for I := 0 to Count - 1 do
    begin
     if Control.Parent.Controls[I] = Control then Break else
      if (Control.Parent.Controls[I] <> nil) and
         (Control.Parent.Controls[I] is TGraphicControl)
       then
        with TGraphicControl(Control.Parent.Controls[I]) do
         begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
           begin
            {$IFDEF WIN32}
            ControlState := ControlState + [csPaintCopy];
            {$ENDIF}
            SaveIndex := SaveDC(DC);
            try
             SaveIndex := SaveDC(DC);
             SetViewportOrgEx(DC, Left + Pnt.X, Top + Pnt.Y, nil);
             IntersectClipRect(DC, 0, 0, Width, Height);
             Perform(WM_PAINT, DC, 0);
            finally
             RestoreDC(DC, SaveIndex);
             {$IFDEF WIN32}
             ControlState := ControlState - [csPaintCopy];
             {$ENDIF}
            end;
           end;
         end;
    end;
 {$IFDEF WIN32}
 finally
   with Control.Parent do ControlState := ControlState - [csPaintCopy];
 end;
 {$ENDIF}
 {$ENDIF}
end;


{ TCustomGuiBaseControl }

constructor TBufferedGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer      := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csReplicatable, csOpaque];
//  DoubleBuffered := True;
end;

destructor TBufferedGraphicControl.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

{$IFNDEF FPC}
procedure TBufferedGraphicControl.DrawParentImage(Dest: TCanvas);
var
  SaveIndex : Integer;
  DC        : THandle;
  Position  : TPoint;
begin
  if Parent = nil then Exit;
  DC := Dest.Handle;
  SaveIndex := SaveDC(DC);
  GetViewportOrgEx(DC, Position);
  SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
  IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
  Parent.Perform(WM_ERASEBKGND, Longint(DC), 0);
  Parent.Perform(WM_PAINT, Longint(DC), 0);
  RestoreDC(DC, SaveIndex);
end;
{$ENDIF}

procedure TBufferedGraphicControl.Downsample2xBitmap(var Bitmap: TBitmap);
begin
 case Bitmap.PixelFormat of
  pf24bit : Downsample2xBitmap24(Bitmap);
  pf32bit : Downsample2xBitmap32(Bitmap);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TBufferedGraphicControl.Downsample3xBitmap(var Bitmap: TBitmap);
begin
 case Bitmap.PixelFormat of
  pf24bit : Downsample3xBitmap24(Bitmap);
  pf32bit : Downsample3xBitmap32(Bitmap);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TBufferedGraphicControl.Downsample4xBitmap(var Bitmap: TBitmap);
begin
 case Bitmap.PixelFormat of
  pf24bit : Downsample4xBitmap24(Bitmap);
  pf32bit : Downsample4xBitmap32(Bitmap);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TBufferedGraphicControl.Upsample2xBitmap(var Bitmap: TBitmap);
begin
 case Bitmap.PixelFormat of
  pf24bit : Upsample2xBitmap24(Bitmap);
  pf32bit : Upsample2xBitmap32(Bitmap);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TBufferedGraphicControl.Upsample3xBitmap(var Bitmap: TBitmap);
begin
 case Bitmap.PixelFormat of
  pf24bit : Upsample3xBitmap24(Bitmap);
  pf32bit : Upsample3xBitmap32(Bitmap);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TBufferedGraphicControl.Upsample4xBitmap(var Bitmap: TBitmap);
begin
 case Bitmap.PixelFormat of
  pf24bit : Upsample4xBitmap24(Bitmap);
  pf32bit : Upsample4xBitmap32(Bitmap);
  else raise Exception.Create(RCStrNotSupported);
 end;
end;

procedure TBufferedGraphicControl.Loaded;
begin
 inherited;
 ResizeBuffer;
end;

procedure TBufferedGraphicControl.Paint;
begin
 with Canvas do
  begin
   UpdateBuffer;
   CopyMode := cmSrcCopy;
   Draw(0, 0, FBuffer);
  end;
 if Assigned(FOnPaint) then FOnPaint(Self);
end;

{$IFNDEF FPC}

{$IFNDEF Delphi7_Up}
function TCustomGuiBaseMouseControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
 // not used yet
end;

procedure TCustomGuiBaseMouseControl.MouseWheelHandler(var Message: TMessage);
begin
end;

procedure TCustomGuiBaseMouseControl.WMMouseWheel(var Message: TWMMouseWheel);
begin
 if not Mouse.WheelPresent then
  begin
   Mouse.FWheelPresent := True;
   Mouse.SettingChanged(SPI_GETWHEELSCROLLLINES);
  end;
 if DoMouseWheel(KeysToShiftState(Message.Keys), WheelDelta, SmallPointToPoint(Pos))
  then Message.Result := 1
end;
{$ENDIF}

procedure TBufferedGraphicControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;

{$IFNDEF COMPILER10_UP}
procedure TBufferedGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TBufferedGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;
{$ENDIF}

{$ELSE}
procedure TBufferedGraphicControl.WMEraseBkgnd(var Message: TLmEraseBkgnd);
begin
  Message.Result := 0;
end;
{$ENDIF}

procedure TBufferedGraphicControl.ResizeBuffer;
begin
 if (Width > 0) and (Height > 0) then
  begin
   FBuffer.Width := Width;
   FBuffer.Height := Height;
   Invalidate;
  end;
end;

procedure TBufferedGraphicControl.Resize;
begin
  inherited Resize;
  ResizeBuffer;
end;

procedure TBufferedGraphicControl.CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 Invalidate;
end;

procedure TBufferedGraphicControl.CMEnabledChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 Invalidate;
end;

procedure TBufferedGraphicControl.CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FontChanged;
end;

procedure TBufferedGraphicControl.FontChanged;
begin
 Invalidate;
end;


{ TCustomGuiBaseControl }

constructor TCustomGuiBaseControl.Create(AOwner: TComponent);
begin
 inherited;
 FLineWidth   := 1;
 FLineColor   := clBlack;
 FTransparent := False;
end;

procedure TCustomGuiBaseControl.SetLineColor(Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiBaseControl.SetLinewidth(Value: Integer);
begin
 if (Value > 0) and (Value < 200) and (FLineWidth <> Value) then
  begin
   FLineWidth := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiBaseControl.SetTransparent(Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   Invalidate;
  end;
end;


{ TCustomGuiBaseMouseControl }

constructor TCustomGuiBaseMouseControl.Create(AOwner: TComponent);
begin
  inherited;
  FReleaseMouseBtnOnLeave := False;
  FRedrawTimer            := TTimer.Create(self);
  FRedrawTimer.Interval   := 0;
  FRedrawTimer.OnTimer    := UpdateGuiTimer;
  FTimerMustRedraw        := False;

  CreateMouseClass(TGuiMouseState);
end;

constructor TCustomGuiBaseMouseControl.Create(AOwner: TComponent;
  MouseStateClass: TGuiMouseStateClass);
begin
  Create(AOwner);

  CreateMouseClass(MouseStateClass);
end;

destructor TCustomGuiBaseMouseControl.Destroy;
begin
  FreeAndNil(FRedrawTimer);
  if assigned(MouseState) then FreeAndNil(MouseState);
  inherited;
end;

procedure TCustomGuiBaseMouseControl.CreateMouseClass(MouseStateClass: TGuiMouseStateClass);
begin
 MouseState := MouseStateClass.Create;
 with MouseState do
  begin
   LeftBtn.ButtonDown   := False;
   MiddleBtn.ButtonDown := False;
   RightBtn.ButtonDown  := False;
   LastEventX := 0;
   LastEventY := 0;
  end;
end;

procedure TCustomGuiBaseMouseControl.CMMouseEnter(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  MouseEnter;
end;

procedure TCustomGuiBaseMouseControl.CMMouseLeave(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  MouseLeave;
end;

procedure TCustomGuiBaseMouseControl.DragMouseMoveLeft(Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnDragMouseMove) then FOnDragMouseMove(Self, mbLeft, Shift, X, Y);
end;

procedure TCustomGuiBaseMouseControl.DragMouseMoveMiddle(Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnDragMouseMove) then FOnDragMouseMove(Self, mbMiddle, Shift, X, Y);
end;

procedure TCustomGuiBaseMouseControl.DragMouseMoveRight(Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnDragMouseMove) then FOnDragMouseMove(Self, mbRight, Shift, X, Y);
end;

function TCustomGuiBaseMouseControl.GetRedrawInterval: Integer;
begin
  Result := FRedrawTimer.Interval;
end;

procedure TCustomGuiBaseMouseControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    inherited;
    MouseCapture := True;
//    Click;

    case Button of
      mbLeft:   with MouseState.LeftBtn do begin
                  ButtonDown := True;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbMiddle: with MouseState.MiddleBtn do begin
                  ButtonDown := True;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbRight:  with MouseState.RightBtn do begin
                  ButtonDown := True;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
    end;

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;
  end;
end;

procedure TCustomGuiBaseMouseControl.MouseEnter;
begin
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomGuiBaseMouseControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  if FReleaseMouseBtnOnLeave then
  begin
    with MouseState.LeftBtn   do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
    with MouseState.MiddleBtn do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
    with MouseState.RightBtn  do if ButtonDown then MouseUp(mbLeft, ShiftState, EventX, EventY);
  end;
end;

procedure TCustomGuiBaseMouseControl.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  if Enabled then
  begin
    inherited;

    with MouseState.LeftBtn   do if ButtonDown then DragMouseMoveLeft(Shift, X, Y);
    with MouseState.MiddleBtn do if ButtonDown then DragMouseMoveMiddle(Shift, X, Y);
    with MouseState.RightBtn  do if ButtonDown then DragMouseMoveRight(Shift, X, Y);

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;
  end;
end;

procedure TCustomGuiBaseMouseControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then
  begin
    inherited;

    case Button of
      mbLeft:   with MouseState.LeftBtn do begin
                  ButtonDown := False;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbMiddle: with MouseState.MiddleBtn do begin
                  ButtonDown := False;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
      mbRight:  with MouseState.RightBtn do begin
                  ButtonDown := False;
                  EventX := X;
                  EventY := Y;
                  ShiftState := Shift;
                end;
    end;

    MouseState.LastEventX := X;
    MouseState.LastEventY := Y;

    MouseCapture := MouseState.LeftBtn.ButtonDown or MouseState.MiddleBtn.ButtonDown or MouseState.RightBtn.ButtonDown;
  end;
end;

procedure TCustomGuiBaseMouseControl.SetRedrawInterval(Value: Integer);
begin
  FRedrawTimer.Interval := Value;
end;

procedure TCustomGuiBaseMouseControl.UpdateGuiTimer(Sender: TObject);
begin
  if not FTimerMustRedraw then Exit;

  FRedrawTimer.Enabled := False;
  Invalidate;
  FRedrawTimer.Enabled := True;

  FTimerMustRedraw := False;
end;

{ TGUIShadow }

constructor TGUIShadow.Create;
begin
 FBlur         := 4;
 FOffset.X     := 1;
 FOffset.Y     := 1;
 FTransparency := $FF;
 FVisible      := False;
end;

procedure TGUIShadow.Changed;
begin
 if assigned(FOnChange)
  then FOnChange(Self);
end;

function TGUIShadow.GetOffsetX: Integer;
begin
 result := FOffset.X;
end;

function TGUIShadow.GetOffsetY: Integer;
begin
 result := FOffset.Y;
end;

procedure TGUIShadow.SetBlur(const Value: Byte);
begin
 if FBlur <> Value then
  begin
   FBlur := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetOffset(const Value: TPoint);
begin
 if (FOffset.X <> Value.X) or (FOffset.Y <> Value.Y) then
  begin
   FOffset := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetOffsetX(const Value: Integer);
begin
 if FOffset.X <> Value then
  begin
   FOffset.X := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetOffsetY(const Value: Integer);
begin
 if FOffset.Y <> Value then
  begin
   FOffset.Y := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetTransparency(const Value: Byte);
begin
 if FTransparency <> Value then
  begin
   FTransparency := Value;
   Changed;
  end;
end;

procedure TGUIShadow.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   Changed;
  end;
end;

{ TCustomGuiBaseAntialiasedControl }

procedure TCustomGuiBaseAntialiasedControl.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntialiasChanged;
  end;
end;

procedure TCustomGuiBaseAntialiasedControl.AntialiasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSValue :=  1;
   gaaLinear2x : FOSValue :=  2;
   gaaLinear3x : FOSValue :=  3;
   gaaLinear4x : FOSValue :=  4;
   gaaLinear8x : FOSValue :=  8;
  gaaLinear16x : FOSValue := 16;
 end;
 Invalidate;
end;

procedure TCustomGuiBaseAntialiasedControl.UpsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Upsample2xBitmap(Bitmap);
   gaaLinear3x: Upsample3xBitmap(Bitmap);
   gaaLinear4x: Upsample4xBitmap(Bitmap);
   gaaLinear8x: begin
                 Upsample4xBitmap(Bitmap);
                 Upsample2xBitmap(Bitmap);
                end;
  gaaLinear16x: begin
                 Upsample4xBitmap(Bitmap);
                 Upsample4xBitmap(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

constructor TCustomGuiBaseAntialiasedControl.Create(AOwner: TComponent);
begin
 inherited;
 FAntiAlias := gaaNone;
 FOSValue   := 1;
end;

procedure TCustomGuiBaseAntialiasedControl.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Downsample2xBitmap(Bitmap);
   gaaLinear3x: Downsample3xBitmap(Bitmap);
   gaaLinear4x: Downsample4xBitmap(Bitmap);
   gaaLinear8x: begin
                 Downsample4xBitmap(Bitmap);
                 Downsample2xBitmap(Bitmap);
                end;
  gaaLinear16x: begin
                 Downsample4xBitmap(Bitmap);
                 Downsample4xBitmap(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;


end.
