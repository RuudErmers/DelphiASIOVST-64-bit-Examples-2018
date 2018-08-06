unit DAV_GuiGroup;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LMessages, Types, {$ELSE} Windows, {$ENDIF}
  Messages, SysUtils, Forms, Classes, Graphics, Controls, StdCtrls,
  DAV_FixedPoint, DAV_GuiCommon, DAV_GuiFont, DAV_GuiPixelMap, DAV_GuiShadow;

type
  TCustomGuiGroup = class(TCustomGroupBox)
  private
    FAutoFocus    : Boolean;
    FBorderColor  : TColor;
    FBorderWidth  : Single;
    FBorderRadius : Single;
    FNative       : Boolean;
    FTransparent  : Boolean;
    FOnPaint      : TNotifyEvent;
    FAlpha        : Byte;
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
    procedure SetAlpha(const Value: Byte);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Single);
    procedure SetNative(const Value: Boolean);
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetBorderRadius(Value: Single);
    procedure SetShadow(const Value: TGUIShadow);
    procedure SetTransparent(const Value: Boolean);
    {$IFDEF FPC}
    function GetCanvas: TCanvas;
    {$ENDIF}
  protected
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FUpdateBuffer     : Boolean;
    FUpdateBackBuffer : Boolean;
    FGuiFont          : TGuiOversampledGDIFont;
    {$IFDEF FPC}
    FCanvas           : TControlCanvas;
    {$ENDIF}

    procedure CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TWmNoParams{$ENDIF}); message CM_TEXTCHANGED;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    {$IFDEF FPC}
    procedure LMPaint(var Message: TLMPaint); message LM_PAINT;
    {$ELSE}
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    {$ENDIF}

    procedure AlphaChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure Click; override;
    procedure NativeChanged; virtual;
    procedure RoundRadiusChanged; virtual;
    procedure TextChanged; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
    procedure TransparentChanged; virtual;

    procedure BufferChanged; virtual;
    procedure BackBufferChanged; virtual;
    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure UpdateBuffer; virtual;
    procedure UpdateBackBuffer; virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Resize; override;

    procedure Paint; {$IFDEF FPC} virtual; {$ELSE} override; {$ENDIF}

    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); virtual; abstract;

    {$IFDEF FPC}
    procedure PaintWindow(DC: HDC); override;

    property Canvas: TCanvas read GetCanvas;
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Alpha: Byte read FAlpha write SetAlpha default $FF;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnShadow;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property Caption;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Native: Boolean read FNative write SetNative default True;
    property Shadow: TGUIShadow read GetShadow write SetShadow;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TCustomGuiGroupColor = class(TCustomGuiGroup)
  private
    FGroupColor  : TColor;
    procedure SetGroupColor(const Value: TColor);
  protected
    procedure GroupColorChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property GroupColor: TColor read FGroupColor write SetGroupColor default clBtnFace;
  end;

  TGuiGroup = class(TCustomGuiGroupColor)
  private
    FHeaderWidth  : Integer;
    FHeaderHeight : Integer;
    procedure CalculateHeaderSize;
  protected
    procedure BorderWidthChanged; override;
    procedure FontChangedHandler(Sender: TObject); override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
    procedure RoundRadiusChanged; override;
    procedure TextChanged; override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property GroupColor;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property BorderRadius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroupSide = class(TCustomGuiGroupColor)
  private
    FHeaderWidth  : Integer;
    procedure CalculateHeaderSize;
  protected
    procedure FontChangedHandler(Sender: TObject); override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
    procedure TextChanged; override;
    procedure AdjustClientRect(var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property GroupColor;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property BorderRadius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroupTop = class(TCustomGuiGroupColor)
  private
    FHeaderHeight : Integer;
    procedure CalculateHeaderSize;
  protected
    procedure FontChangedHandler(Sender: TObject); override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
    procedure TextChanged; override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property GroupColor;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property BorderRadius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TGuiGroupSimple = class(TCustomGuiGroup)
  private
    FTextWidth  : Integer;
    FYOffset    : Integer;
    procedure CalculateTextWidthAndOffset;
  protected
    procedure BorderWidthChanged; override;
    procedure FontChangedHandler(Sender: TObject); override;
    procedure TextChanged; override;
    procedure RenderGroupBox(PixelMap: TGuiCustomPixelMap); override;
    procedure RenderCaption(PixelMap: TGuiCustomPixelMap); override;
  published
    property Align;
    property Alpha;
    property Anchors;
    property BorderColor;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property HelpContext;
    property Hint;
    property Native;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property BorderRadius;
    property Shadow;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses
  Math, DAV_Math, DAV_Approximations, DAV_GuiBlend;

{ TCustomGuiGroup }

constructor TCustomGuiGroup.Create(AOwner: TComponent);
begin
 inherited;

 ControlStyle   := ControlStyle + [csAcceptsControls, csReplicatable];
 {$IFDEF FPC}
 DoubleBuffered := True;
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;
 ControlStyle   := ControlStyle - [csOpaque];
 {$ELSE}
 ControlStyle   := ControlStyle + [csOpaque];
 {$ENDIF}

 // create buffers (& set size)
 FBuffer           := TGuiPixelMapMemory.Create;
 FBackBuffer       := TGuiPixelMapMemory.Create;
 FBuffer.SetSize(Width, Height);
 FBackBuffer.SetSize(Width, Height);

 // create font
 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;

 // initialize variables
 ParentColor       := True;
 ParentFont        := True;
 Enabled           := True;
 Visible           := True;
 FAlpha            := $FF;
 FBorderRadius      := 2;
 FUpdateBuffer     := True;
 FUpdateBackBuffer := True;
 FTransparent      := False;
 FBorderColor      := clBtnShadow;
 FBorderWidth      := 1;
 FNative           := False;

 // set initial bounds
 SetBounds(0, 0, 128, 64);
end;

procedure TCustomGuiGroup.CreateParams(var Params: TCreateParams);
begin
 inherited;

 if not FNative then
  with Params do Style := (Style and not $1F) or BS_OWNERDRAW;
end;

destructor TCustomGuiGroup.Destroy;
begin
 {$IFDEF FPC}
 FreeAndNil(FCanvas);
 {$ENDIF}

 // free buffers
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 FreeAndNil(FGuiFont);

 inherited;
end;

procedure TCustomGuiGroup.Click;
begin
 if FAutoFocus then SetFocus;
 inherited;
end;

procedure TCustomGuiGroup.Paint;
begin
 if FNative
  then inherited
  else
   begin
    if FUpdateBackBuffer
     then UpdateBackBuffer;

    if FUpdateBuffer
     then UpdateBuffer;

    if Assigned(FOnPaint)
     then FOnPaint(Self);

    if Assigned(FBuffer)
    {$IFDEF FPC}
     then FBuffer.PaintTo(Canvas, -2, -16);
    {$ELSE}
     then FBuffer.PaintTo(Canvas);
    {$ENDIF}
   end;
end;

procedure TCustomGuiGroup.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiGroup.Resize;
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

{$IFDEF FPC}
function TCustomGuiGroup.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

procedure TCustomGuiGroup.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
  inherited;
end;
{$ENDIF}

procedure TCustomGuiGroup.WMMove(var Message: TWMMove);
begin
 inherited;

 if FTransparent
  then BackBufferChanged;
end;

procedure TCustomGuiGroup.CMColorChanged(var Message: TMessage);
begin
 inherited;

 if not FNative
  then BackBufferChanged;
end;

{$IFDEF FPC}
procedure TCustomGuiGroup.LMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited;
  Exclude(FControlState, csCustomPaint);
end;

{$ELSE}

procedure TCustomGuiGroup.CMDialogChar(var Message: TCMDialogChar);
begin
 with Message do
  if IsAccel(Message.CharCode, Caption) and CanFocus then
   begin
    SetFocus;
    Result := 1;
   end;
end;

procedure TCustomGuiGroup.CMSysColorChange(var Message: TMessage);
begin
 inherited;

 BufferChanged;
end;

{$ENDIF}

procedure TCustomGuiGroup.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 BufferChanged;
end;

procedure TCustomGuiGroup.CMFontChanged(var Message: TMessage);
begin
 FGuiFont.Font.Assign(Font);
end;

procedure TCustomGuiGroup.CMTextChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TWmNoParams{$ENDIF});
begin
 TextChanged;
end;

procedure TCustomGuiGroup.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.AlphaChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.BorderColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.BorderWidthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.BackBufferChanged;
begin
 if not FNative
  then FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGroup.BufferChanged;
begin
 if not FNative
  then FUpdateBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGroup.NativeChanged;
begin
 {$IFDEF FPC}
 RecreateWnd(Self);
 {$ELSE}
 RecreateWnd;
 {$ENDIF}

 if not FNative
  then FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGroup.RoundRadiusChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.TextChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.TransparentChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroup.UpdateBackBuffer;
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
 FBackBuffer.MakeOpaque;
 FUpdateBuffer := True;
end;

procedure TCustomGuiGroup.UpdateBuffer;
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

 RenderGroupBox(FBuffer);
 RenderCaption(FBuffer);
end;

procedure TCustomGuiGroup.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TCustomGuiGroup.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

function TCustomGuiGroup.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiGroup.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiGroup.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiGroup.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
end;

procedure TCustomGuiGroup.SetBorderRadius(Value: Single);
begin
 if Value < 0 then Value := 0;
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   RoundRadiusChanged;
  end;
end;

procedure TCustomGuiGroup.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
end;

procedure TCustomGuiGroup.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiGroup.SetNative(const Value: Boolean);
begin
 if FNative <> Value then
  begin
   FNative := Value;
   NativeChanged;
  end;
end;


{ TCustomGuiGroupColor }

constructor TCustomGuiGroupColor.Create(AOwner: TComponent);
begin
 inherited;
 FGroupColor := clBtnFace;
end;

procedure TCustomGuiGroupColor.GroupColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiGroupColor.SetGroupColor(const Value: TColor);
begin
 if FGroupColor <> Value then
  begin
   FGroupColor := Value;
   GroupColorChanged;
  end;
end;


{ TCustomGuiGroupA }

{
procedure TCustomGuiGroupA.RenderGroupBoxToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplex32;
  Steps, i : Integer;
  LineOffs : array[0..1] of Integer;
  PntArray : array of TPoint;
//  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;

   Brush.Style := bsSolid;
   Brush.Color := FGroupColor;
   Pen.Width   := FOSFactor * FBorderWidth;
   Pen.Color   := FBorderColor;
   Font.Assign(Self.Font);
   Font.Size := FOSFactor * Font.Size;
   TextSize := TextExtent(FCaption);

   case FBorderRadius of
    0, 1 : begin
            FrameRect(ClipRect);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
           end;
       2 : begin
            LineOffs[0] := Round(BorderWidth div 2);
            LineOffs[1] := Round((BorderWidth + 1) div 2);
            with ClipRect do
             begin
              PntArray[ 0] := Point(Left  + 1 + LineOffs[0], Bottom - 1 - LineOffs[1]);
              PntArray[ 1] := Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]);
              PntArray[ 2] := Point(Left      + LineOffs[0], Top    + 2 + LineOffs[0]);
              PntArray[ 3] := Point(Left  + 2 + LineOffs[0], Top        + LineOffs[0]);
              PntArray[ 4] := Point(Right - 2 - LineOffs[1], Top        + LineOffs[0]);
              PntArray[ 5] := Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]);
              PntArray[ 6] := Point(Right - 1 - LineOffs[1], Top    + 1 + LineOffs[0]);
              PntArray[ 7] := Point(Right     - LineOffs[1], Top    + 2 + LineOffs[0]);
              PntArray[ 8] := Point(Right     - LineOffs[1], Bottom - 2 - LineOffs[1]);
              PntArray[ 9] := Point(Right - 2 - LineOffs[1], Bottom     - LineOffs[1]);
              PntArray[10] := Point(Left  + 2 + LineOffs[0], Bottom     - LineOffs[1]);
              PntArray[11] := Point(Left      + LineOffs[0], Bottom - 2 - LineOffs[1]);
             end;
            PolyLine(PntArray);
            FillRect(Rect(1, 1, TextSize.cx + 12, TextSize.cy + 4));
            MoveTo(1, TextSize.cy + 4);
            LineTo(TextSize.cx + 11, TextSize.cy + 4);
            // MoveTo(TextSize.cx + 12, 1);
            // LineTo(TextSize.cx + 12, TextSize.cy + 3);
           end;
    else
     begin
      rad := FOSFactor * FBorderRadius;
      Steps := Round(2 / Arcsin(1 / rad)) + 1;
      if Steps > 1 then
      begin
        SetLength(PtsArray, Steps + 4);
        Val.Im := 0; Val.Re := -1;
        Val.Re := Val.Re * rad; Val.Im := Val.Im * rad;

        GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
        PtsArray[0] := Point(Round(BorderWidth div 2), Round(BorderWidth div 2 + rad));

        // upper left corner
        for i := 1 to Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 4] := Point(BorderWidth div 2 + rad, BorderWidth div 2 + 0);

        // upper right corner
        for i := Steps div 4 to Steps div 2 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 1] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 2 + 1] := Point(ClipRect.Right - (BorderWidth + 1) div 2, BorderWidth div 2 + rad);

        // lower right corner
        for i := Steps div 2 to 3 * Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 2] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[3 * Steps div 4 + 2] := Point(ClipRect.Right - rad - (BorderWidth + 1) div 2, ClipRect.Bottom - (BorderWidth + 1) div 2);

        // lower left corner
        for i := 3 * Steps div 4 to Steps - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[Steps + 3] := Point(BorderWidth div 2, rad + BorderWidth div 2);

        PolyGon(PtsArray);

(*
        // Draw inner text
        //////////////////

        Brush.Color   := FBorderColor;
        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(2 * Pi / (Steps div 2 - 1), Off.Im, Off.Re);
        rct := Rect(BorderWidth div 2, BorderWidth div 2, max(TextSize.cx + 10, FOSFactor * FHeaderMinWidth) - (BorderWidth + 1) div 2, TextSize.cy + 5 - (BorderWidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to (Steps div 4) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4    ] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4 + 1] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4 + 2] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := (Steps div 4) to (Steps div 2) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(rct.Right - rad + Val.Re), Round(rct.Bottom - rad + Val.Im));
         end;

        PtsArray[Steps div 2 + 3] := Point(rct.Right - rad, rct.Bottom);
        PtsArray[Steps div 2 + 4] := Point(rct.Left, rct.Bottom);

        Polygon(PtsArray);
*)
      end;
     end;
   end;

   Brush.Style := bsClear;
   TextOut(6, 2, FCaption);
   Unlock;
  end;
end;
}


{ TGuiGroup }

procedure TGuiGroup.FontChangedHandler(Sender: TObject);
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroup.BorderWidthChanged;
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroup.CalculateHeaderSize;
var
  TextSize : TSize;
begin
 TextSize := FGuiFont.TextExtent(Caption);
 FHeaderWidth := Round(TextSize.cx + 2 * (FBorderWidth + FBorderRadius));
 FHeaderHeight := Round(TextSize.cy + 2 * FBorderWidth);
end;

procedure TGuiGroup.RenderCaption(PixelMap: TGuiCustomPixelMap);
begin
 if Assigned(FGuiFont) then
  begin
   FGuiFont.TextOut(Caption, PixelMap, Round(FBorderWidth + FBorderRadius), Round(FBorderWidth));
  end;
end;

procedure TGuiGroup.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y                : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : array [0..1] of PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  HeaderHeight         : TFixed24Dot8;
  HeaderWidth          : TFixed24Dot8;
  RadiusFixed          : TFixed24Dot8;
  XStart               : TFixed24Dot8;
  BorderWidthFixed     : TFixed24Dot8;
  RadMinusOne          : TFixed24Dot8;
  RadMinusBorder       : TFixed24Dot8;
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
   PanelColor := ConvertColor(FGroupColor);
   PanelColor.A := Alpha;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   RadiusFixed := ConvertToFixed24Dot8(Min(FBorderRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8(Integer(Width - 1));
   HeaderHeight.Fixed := ConvertToFixed24Dot8(FHeaderHeight).Fixed;
   HeaderWidth.Fixed := ConvertToFixed24Dot8(FHeaderWidth).Fixed;

   // precalculate BorderRadius variables
   RadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := RadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);



(*
   // draw the very top borders
   for Y := 0 to FixedFloor(BorderWidthFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne[0] := Scanline[Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8(Width - 1), Temp));
     X := FixedRound(HeaderWidth) + 1;
     while X <= XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       Temp.Fixed := ConvertToFixed24Dot8(Width - 1).Fixed - RadMinusOne.Fixed;
       if XFixed.Fixed > Temp.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
        else SqrDist := SqrYDist;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed < SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then CombColor := PanelColor
          else CombColor := CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X);
      end;
    end;
*)



   // draw bottom rounded borders
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne[0] := Scanline[Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedRoundHalfUp(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     if XRange[0] < 0 then XRange[0] := 0;

     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed < SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
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
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       // Assert(X <= Width);
       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;

   for Y := FixedRoundHalfUp(RadiusFixed) to FixedRoundHalfUp(HeaderHeight) - 1 do
    begin
     ScnLne[0] := Scanline[Y];
     YFixed := ConvertToFixed24Dot8(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0][0], Width);
       Continue;
      end;

     // check upper/lower half and eventually precalculate y-border distance
     Temp := ConvertToFixed24Dot8(Integer(Height - 1));
     IsUpperLowerHalf := (YFixed.Fixed < BorderWidthFixed.Fixed) or
       (YFixed.Fixed > Temp.Fixed - BorderWidthFixed.Fixed);
     if IsUpperLowerHalf
      then YBorderDistance.Fixed := BorderWidthFixed.Fixed - YFixed.Fixed
      else YBorderDistance.Fixed := 0;

     X := FixedRoundHalfUp(FixedSub(HeaderWidth, RadiusFixed));
     while X < Width do
      begin
       // convert
       XFixed := ConvertToFixed24Dot8(X);

       // check whether position is an upper/lower half border
       if IsUpperLowerHalf then
        begin
         if (XFixed.Fixed > ConvertToFixed24Dot8(Width).Fixed - BorderWidthFixed.Fixed)
          then CombColor := BorderColor else
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
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
           if Width - X <= 0 then
            begin
             BlendPixelLine(CombColor, @ScnLne[0][X], Width - 2 * X);
             EMMS;
             X := Width - X;
             Continue;
            end;
          end;
        end else
       if (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed)
        then CombColor := BorderColor else
       if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := XFixed.Fixed - WidthMinusOne.Fixed + BorderWidthFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end
       else
        begin
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[0][X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X)
      end;
    end;
   EMMS;

   for Y := FixedRoundHalfUp(HeaderHeight) to Height - 1 - FixedRoundHalfUp(RadiusFixed) do
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
         if (XFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed > ConvertToFixed24Dot8(Width).Fixed - BorderWidthFixed.Fixed)
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
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
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
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[0][X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X)
      end;
    end;
   EMMS;


   for Y := FixedRoundHalfUp(RadiusFixed) to FixedRoundHalfUp(HeaderHeight) - 1 - FixedRoundHalfUp(RadiusFixed) do
    begin
     BlendPixelLine(BorderColor, @Scanline[Y][0], FHeaderWidth);
     EMMS;
    end;


   // draw bottom rounded borders
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne[0] := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedRoundHalfUp(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed < SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
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
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       EMMS;
      end;
    end;




   // draw top label border
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[FixedRoundHalfUp(HeaderHeight) - Y - 1];

     BlendPixelLine(BorderColor, @ScnLne[1][0], FixedRoundHalfUp(FixedSub(HeaderWidth, RadiusFixed)) - 1);

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     for X := FixedRoundHalfUp(Temp) to FixedRoundHalfUp(RadiusFixed) do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else SqrDist := SqrYDist;

       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][FixedRoundHalfUp(HeaderWidth) - 1 - X]);
      end;

     BlendPixelLine(BorderColor, @ScnLne[0][FixedRoundHalfUp(RadiusFixed) + 1], FixedRoundHalfUp(FixedSub(HeaderWidth, RadiusFixed))  - 1);
     EMMS;
    end;

  end;
end;

procedure TGuiGroup.RoundRadiusChanged;
begin
 inherited;
 CalculateHeaderSize;
end;

procedure TGuiGroup.TextChanged;
begin
 CalculateHeaderSize;
 inherited;
end;

(*
procedure TCustomGuiGroupB.RenderGroupBoxToBitmap(Bitmap: TBitmap);
var
  Val, Off : TComplex32;
  Steps, i : Integer;
  rct      : TRect;
  tmp      : Single;
  rad      : Integer;
  TextSize : TSize;
  PtsArray : array of TPoint;
begin
 with Bitmap.Canvas do
  begin
   Lock;

   Brush.Style := bsSolid;
   Brush.Color := FGroupColor;
   Pen.Width   := FOSFactor * FBorderWidth;
   Pen.Color   := FBorderColor;
   Font.Assign(Self.Font);
   Font.Size   := FOSFactor * Font.Size;
   TextSize    := TextExtent(FCaption);
   TextSize.cx := TextSize.cx + 2 * FOSFactor * (FBorderWidth + FOffset);
   if TextSize.cx < FHeaderMinWidth
    then TextSize.cx := TextSize.cx + FOSFactor * FHeaderMinWidth;
   TextSize.cy := TextSize.cy + 3 * FOSFactor * FBorderWidth div 2;

   rct := ClipRect;
   InflateRect(rct, -FOSFactor * (BorderWidth + 1) div 2, -FOSFactor * (BorderWidth + 1) div 2);

   case FBorderRadius of
    0, 1 : begin
            Rectangle(rct.Left, rct.Top, rct.Right + 1, rct.Bottom + 1);
            Brush.Color := FGroupColor;
            FillRect(Rect(rct.Left + FOSFactor * BorderWidth div 2, rct.Top + FOSFactor * BorderWidth div 2, TextSize.cx, TextSize.cy));
            MoveTo(FOSFactor * FBorderWidth, TextSize.cy);
            LineTo(TextSize.cx, TextSize.cy);
            LineTo(TextSize.cx, rct.Top);
           end;
       2 : begin
            with rct do
             PolyLine([Point(Left  + 1 * FOSFactor, Bottom - 1 * FOSFactor),
                       Point(Left                 , Bottom - 2 * FOSFactor),
                       Point(Left                 , Top    + 2 * FOSFactor),
                       Point(Left  + 2 * FOSFactor, Top                   ),
                       Point(Right - 2 * FOSFactor, Top                   ),
                       Point(Right                , Top    + 2 * FOSFactor),
                       Point(Right - 1 * FOSFactor, Top    + 1 * FOSFactor),
                       Point(Right                , Top    + 2 * FOSFactor),
                       Point(Right                , Bottom - 2 * FOSFactor),
                       Point(Right - 2 * FOSFactor, Bottom                ),
                       Point(Left  + 2 * FOSFactor, Bottom                ),
                       Point(Left                 , Bottom - 2 * FOSFactor)]);
            Brush.Color := FGroupColor;
            FillRect(Rect(rct.Left + FOSFactor * BorderWidth div 2, rct.Top + FOSFactor * BorderWidth div 2, TextSize.cx, TextSize.cy));
            MoveTo(FOSFactor * FBorderWidth, TextSize.cy);
            LineTo(TextSize.cx, TextSize.cy);
            LineTo(TextSize.cx, rct.Top);
           end;
    else
     begin
      rct := ClipRect;
      Brush.Color := FGroupColor;
      InflateRect(rct, -FOSFactor * (BorderWidth + 1) div 2, -FOSFactor * (BorderWidth + 1) div 2);

      rad := FOSFactor * FBorderRadius;
      Steps := Round(2 / arcsin(1 / rad)) + 1;
      if Steps > 1 then
       begin
        SetLength(PtsArray, Steps + 4);
        Val.Im := 0; Val.Re := -1;
        Val.Re := Val.Re * rad; Val.Im := Val.Im * rad;

        GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
        PtsArray[0] := Point(Round(BorderWidth div 2), Round(BorderWidth div 2 + rad));

        // upper left corner
        for i := 1 to Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 4] := Point(BorderWidth div 2 + rad, BorderWidth div 2 + 0);

        // upper right corner
        for i := Steps div 4 to Steps div 2 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 1] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(BorderWidth div 2 + rad + Val.Im));
         end;
        PtsArray[Steps div 2 + 1] := Point(ClipRect.Right - (BorderWidth + 1) div 2, BorderWidth div 2 + rad);

        // lower right corner
        for i := Steps div 2 to 3 * Steps div 4 - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 2] := Point(Round(ClipRect.Right - rad - (BorderWidth + 1) div 2 + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[3 * Steps div 4 + 2] := Point(ClipRect.Right - rad - (BorderWidth + 1) div 2, ClipRect.Bottom - (BorderWidth + 1) div 2);

        // lower left corner
        for i := 3 * Steps div 4 to Steps - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(BorderWidth div 2 + rad + Val.Re), Round(ClipRect.Bottom - (BorderWidth + 1) div 2 - rad + Val.Im));
         end;
        PtsArray[Steps + 3] := Point(BorderWidth div 2, rad + BorderWidth div 2);

        PolyGon(PtsArray);


        // Draw inner text
        //////////////////

        Brush.Color := FGroupColor;
        SetLength(PtsArray, Steps div 2 + 5);
        Val.Re := -rad; Val.Im := 0;

        GetSinCos(Pi / (Steps div 2 - 1), Off.Im, Off.Re);
        rct := Rect(BorderWidth div 2, BorderWidth div 2, TextSize.cx + 10 - (BorderWidth + 1) div 2, TextSize.cy + 5 - (BorderWidth + 1) div 2);
        PtsArray[0] := Point(Round(rct.Left), Round(rct.Top + rad));

        // upper left corner
        for i := 1 to (Steps div 4) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i] := Point(Round(rct.Left + rad + Val.Re), Round(rct.Top + rad + Val.Im));
         end;
        PtsArray[Steps div 4    ] := Point(rct.Left + rad, rct.Top);
        PtsArray[Steps div 4 + 1] := Point(rct.Right, rct.Top);
        PtsArray[Steps div 4 + 2] := Point(rct.Right, rct.Bottom - rad);

        Val.Re := rad; Val.Im := 0;

        // lower right corner
        for i := (Steps div 4) to (Steps div 2) - 1 do
         begin
          tmp := Val.Re * Off.Re - Val.Im * Off.Im;
          Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
          Val.Re := tmp;
          PtsArray[i + 3] := Point(Round(rct.Right - rad + Val.Re), Round(rct.Bottom - rad + Val.Im));
         end;

        PtsArray[Steps div 2 + 3] := Point(rct.Right - rad, rct.Bottom);
        PtsArray[Steps div 2 + 4] := Point(rct.Left, rct.Bottom);

        Polygon(PtsArray);
      end;
     end;
   end;

   Brush.Style := bsClear;
   TextOut(FOSFactor * (FBorderWidth + FOffset), FOSFactor * FBorderWidth, FCaption);
   Unlock;
  end;
end;
*)

{ TGuiGroupSide }

procedure TGuiGroupSide.AdjustClientRect(var Rect: TRect);
begin
 {$IFDEF DELPHI10_UP}
 Inc(Rect.Left, Padding.Left);
 Inc(Rect.Top, Padding.Top);
 Dec(Rect.Right, Padding.Right);
 Dec(Rect.Bottom, Padding.Bottom);
 {$ELSE}
 inherited AdjustClientRect(Rect);
 {$ENDIF}

 Inc(Rect.Left, FHeaderWidth);
 InflateRect(Rect, -Round(FBorderWidth), -Round(FBorderWidth));
end;

procedure TGuiGroupSide.CalculateHeaderSize;
var
  TextSize : TSize;
begin
 TextSize := FGuiFont.TextExtent(Caption);
 FHeaderWidth := TextSize.cy;
end;

constructor TGuiGroupSide.Create(AOwner: TComponent);
begin
 inherited;
 FGuiFont.FontTurn := ftCounterClockwise;
end;

procedure TGuiGroupSide.FontChangedHandler(Sender: TObject);
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroupSide.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y                 : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  HeaderWidth          : TFixed24Dot8;
  RadiusFixed          : TFixed24Dot8;
  XStart               : TFixed24Dot8;
  BorderWidthFixed     : TFixed24Dot8;
  RadMinusOne          : TFixed24Dot8;
  RadMinusBorder       : TFixed24Dot8;
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
   PanelColor := ConvertColor(FGroupColor);
   PanelColor.A := Alpha;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   RadiusFixed := ConvertToFixed24Dot8(Min(FBorderRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8(Integer(Width - 1));
   HeaderWidth.Fixed := ConvertToFixed24Dot8(FHeaderWidth).Fixed +
     3 * BorderWidthFixed.Fixed;

   // precalculate BorderRadius variables
   RadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := RadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw top rounded borders
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedRoundHalfUp(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     X := XRange[0];
     while X <= XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if (XFixed.Fixed <= BorderWidthFixed.Fixed + HeaderWidth.Fixed - CFixed24Dot8One.Fixed) and
          (SqrDist.Fixed < SqrRadMinusBorder.Fixed)
        then CombColor := BorderColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X);
      end;
    end;

   // draw bottom rounded borders
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedRoundHalfUp(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if (XFixed.Fixed <= BorderWidthFixed.Fixed + HeaderWidth.Fixed - CFixed24Dot8One.Fixed) and
          (SqrDist.Fixed < SqrRadMinusBorder.Fixed)
        then CombColor := BorderColor
        else
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
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
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   for Y := FixedRoundHalfUp(RadiusFixed) to Height - 1 - FixedRoundHalfUp(RadiusFixed) do
    begin
     ScnLne := Scanline[Y];
     YFixed := ConvertToFixed24Dot8(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0], Width);
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
         if (XFixed.Fixed < BorderWidthFixed.Fixed + HeaderWidth.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed > ConvertToFixed24Dot8(Width).Fixed - BorderWidthFixed.Fixed)
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
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
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
           BlendPixelLine(CombColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed + HeaderWidth.Fixed - CFixed24Dot8One.Fixed) or
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
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TGuiGroupSide.RenderCaption(PixelMap: TGuiCustomPixelMap);
var
  TextSize : TSize;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtent(Caption);
   FGuiFont.TextOut(Caption, PixelMap, Round(2 * FBorderWidth), (Height - TextSize.cx) div 2);
  end;
end;

procedure TGuiGroupSide.TextChanged;
begin
 CalculateHeaderSize;
 inherited;
end;


{ TGuiGroupTop }

procedure TGuiGroupTop.CalculateHeaderSize;
var
  TextSize : TSize;
begin
 TextSize := FGuiFont.TextExtent(Caption);
 FHeaderHeight := TextSize.cy;
end;

procedure TGuiGroupTop.FontChangedHandler(Sender: TObject);
begin
 CalculateHeaderSize;
 inherited;
end;

procedure TGuiGroupTop.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y                 : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  HeaderHeight         : TFixed24Dot8;
  RadiusFixed          : TFixed24Dot8;
  XStart               : TFixed24Dot8;
  BorderWidthFixed     : TFixed24Dot8;
  RadMinusOne          : TFixed24Dot8;
  RadMinusBorder       : TFixed24Dot8;
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
   PanelColor := ConvertColor(FGroupColor);
   PanelColor.A := Alpha;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   RadiusFixed := ConvertToFixed24Dot8(Min(FBorderRadius, Min(FHeaderHeight, 0.5 * Min(Width, Height))) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8(Integer(Width - 1));
   HeaderHeight.Fixed := ConvertToFixed24Dot8(FHeaderHeight).Fixed +
     2 * BorderWidthFixed.Fixed;

   // precalculate BorderRadius variables
   RadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := RadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := RadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw top rounded borders
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedRoundHalfUp(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed
        then CombColor := BorderColor
        else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   // draw bottom rounded borders
   for Y := 0 to FixedRoundHalfUp(RadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);

     // calculate x offset
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(RadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(RadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // set scan lines
     ScnLne := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedRoundHalfUp(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       // select color to be drawn
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed <= SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
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
          Temp := FixedMul(Temp, FixedSub(RadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[X]);
      end;
    end;

   for Y := FixedRoundHalfUp(RadiusFixed) to Height - 1 - FixedRoundHalfUp(RadiusFixed) do
    begin
     ScnLne := Scanline[Y];
     YFixed := ConvertToFixed24Dot8(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0], Width);
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
         if (XFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed > ConvertToFixed24Dot8(Width).Fixed - BorderWidthFixed.Fixed) or
            (YFixed.Fixed <= HeaderHeight.Fixed)
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
         if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
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
           BlendPixelLine(CombColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
          (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed) or
          (YFixed.Fixed <= HeaderHeight.Fixed)
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
         if Width - X <= 0 then
          begin
           BlendPixelLine(PanelColor, @ScnLne[X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end else CombColor := PanelColor;
        end;

       BlendPixelInplace(CombColor, ScnLne[X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TGuiGroupTop.RenderCaption(PixelMap: TGuiCustomPixelMap);
var
  TextSize : TSize;
begin
 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtent(Caption);
   FGuiFont.TextOut(Caption, PixelMap, (Width - TextSize.cx) div 2 , Round(FBorderWidth));
  end;
end;

procedure TGuiGroupTop.TextChanged;
begin
 CalculateHeaderSize;
 inherited;
end;


{ TGuiGroupSimple }

procedure TGuiGroupSimple.BorderWidthChanged;
begin
 CalculateTextWidthAndOffset;
 inherited;
end;

procedure TGuiGroupSimple.CalculateTextWidthAndOffset;
var
  TextSize : TSize;
begin
 if Caption <> '' then
  begin
   TextSize := FGuiFont.TextExtent(Caption);
   FTextWidth := Round(TextSize.cx + 3 * (FBorderWidth));
   FYOffset := Round(0.5 * (TextSize.cy - FBorderWidth));
  end
 else FTextWidth := 0;
end;

procedure TGuiGroupSimple.TextChanged;
begin
 CalculateTextWidthAndOffset;
 inherited;
end;

procedure TGuiGroupSimple.FontChangedHandler(Sender: TObject);
begin
 CalculateTextWidthAndOffset;
 inherited;
end;

procedure TGuiGroupSimple.RenderCaption(PixelMap: TGuiCustomPixelMap);
var
  XPos : Integer;
begin
 if Assigned(FGuiFont) then
  begin
   XPos := Round(Max(FBorderRadius, FBorderWidth) + 1.5 * FBorderWidth);
   FGuiFont.TextOut(Caption, PixelMap, XPos, 0);
  end;
end;

procedure TGuiGroupSimple.RenderGroupBox(PixelMap: TGuiCustomPixelMap);
var
  X, Y                 : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : array [0..1] of PPixel32Array;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  XStart               : TFixed24Dot8;
  BorderRadiusFixed    : TFixed24Dot8;
  BorderWidthFixed     : TFixed24Dot8;
  RadMinusOne          : TFixed24Dot8;
  RadMinusBorder       : TFixed24Dot8;
  SqrRadMinusBorderOne : TFixed24Dot8;
  SqrRadMinusBorder    : TFixed24Dot8;
  SqrDist, SqrYDist    : TFixed24Dot8;
  SqrRadMinusOne       : TFixed24Dot8;
  XFixed, YFixed       : TFixed24Dot8;
  Temp                 : TFixed24Dot8;

  YRange               : array [0..1] of Integer;
  CenterX              : TFixed24Dot8;
  CenterY              : TFixed24Dot8;
begin
 with PixelMap do
  begin
   // set local colors
   BorderColor := ConvertColor(FBorderColor);
   BorderColor.A := Alpha;

   // set other local variables
   BorderRadiusFixed := ConvertToFixed24Dot8(Min(FBorderRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8(Max(FBorderWidth, 1));

   // precalculate BorderRadiusFixed variables
   RadMinusOne.Fixed := BorderRadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := BorderRadiusFixed.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := BorderRadiusFixed.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw rounded borders
   for Y := 0 to FixedRoundHalfUp(BorderRadiusFixed) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(BorderRadiusFixed, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(BorderRadiusFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;
     ScnLne[0] := Scanline[FYOffset + Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRoundHalfUp(Temp);
     XRange[1] := FixedFloor(RadMinusOne);
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else SqrDist := SqrYDist;

       // calculate color based on distance
       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then Continue
        else
       if SqrDist.Fixed < SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then Continue
          else
           begin
            CombColor := BorderColor;
            CombColor.A := (CombColor.A * Round($FF - Temp.Fixed) + $80) shr 8;
           end;
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(BorderRadiusFixed, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       // finally draw pixels
       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);

       BlendPixelInplace(CombColor, ScnLne[0][Width - 1 - X]);
       BlendPixelInplace(CombColor, ScnLne[1][Width - 1 - X]);
      end;
    end;

   // draw horizontal lines
   X := Max(FixedFloor(BorderWidthFixed), FixedFloor(BorderRadiusFixed));
   for Y := 0 to FixedFloor(BorderWidthFixed) - 1  do
    begin
     BlendPixelLine(BorderColor, @Scanline[FYOffset + Y][X + FTextWidth], Width - 2 * X - FTextWidth);
     BlendPixelLine(BorderColor, @Scanline[Height - 1 - Y][X], Width - 2 * X);
    end;

   // draw fractional horizontal line
   Y := FixedFloor(BorderWidthFixed);
   CombColor := BorderColor;
   CombColor.A := (CombColor.A * BorderWidthFixed.Frac + $80) shr 8;
   BlendPixelLine(CombColor, @Scanline[FYOffset + Y][X + FTextWidth], Width - 2 * X - FTextWidth);
   BlendPixelLine(CombColor, @Scanline[Height - 1 - Y][X], Width - 2 * X);

   // draw missing top/bottom borders
   if BorderWidthFixed.Fixed > BorderRadiusFixed.Fixed then
    begin
     X := FixedFloor(BorderRadiusFixed);
     XRange[0] := FixedFloor(BorderWidthFixed) - X;
     if XRange[0] > 0 then
      for Y := 0 to FixedRoundHalfUp(BorderRadiusFixed) - 1 do
       begin
        ScnLne[0] := Scanline[FYOffset + Y];
        ScnLne[1] := Scanline[Height - 1 - Y];

        BlendPixelLine(BorderColor, @ScnLne[0][X], XRange[0]);
        BlendPixelLine(BorderColor, @ScnLne[0][Width - XRange[0] - X], XRange[0]);
        BlendPixelLine(BorderColor, @ScnLne[1][X], XRange[0]);
        BlendPixelLine(BorderColor, @ScnLne[1][Width - XRange[0] - X], XRange[0]);
       end;
    end;

   // draw vertical lines
   Temp.Fixed := Max(BorderRadiusFixed.Fixed, BorderWidthFixed.Fixed);
   XRange[0] := FYOffset + FixedRoundHalfUp(Temp);
   XRange[1] := Height - FixedRoundHalfUp(Temp);
   for Y := XRange[0] to XRange[1] - 1 do
    begin
     ScnLne[0] := Scanline[Y];
     YFixed := ConvertToFixed24Dot8(Integer(Y - FYOffset));

     X := FixedFloor(BorderWidthFixed);
     BlendPixelLine(BorderColor, @ScnLne[0][0], X);
     BlendPixelLine(BorderColor, @ScnLne[0][Width - X], X);

     CombColor := BorderColor;
     CombColor.A := (CombColor.A * BorderWidthFixed.Frac + $80) shr 8;
     BlendPixelInplace(CombColor, ScnLne[0][X]);
     BlendPixelInplace(CombColor, ScnLne[0][Width - X - 1]);
    end;

   // draw missing vertical lines part
   if (BorderWidthFixed.Fixed > BorderRadiusFixed.Fixed) then
    for Y := FixedRoundHalfUp(BorderRadiusFixed) to FixedRoundHalfUp(BorderWidthFixed) - 1 do
     begin
      ScnLne[0] := Scanline[FYOffset + Y];
      ScnLne[1] := Scanline[Height - Y - 1];
      YFixed := ConvertToFixed24Dot8(Integer(Y - FYOffset));

      X := FixedFloor(BorderWidthFixed);
      BlendPixelLine(BorderColor, @ScnLne[0][0], X);
      BlendPixelLine(BorderColor, @ScnLne[0][Width - X], X);
      BlendPixelLine(BorderColor, @ScnLne[1][0], X);
      BlendPixelLine(BorderColor, @ScnLne[1][Width - X], X);
     end;




   // draw half circles
   CenterX.Fixed := Max(BorderRadiusFixed.Fixed, BorderWidthFixed.Fixed);
   CenterX.Frac := 0;
   BorderWidthFixed := FixedMul(FixedAdd(BorderWidthFixed, CFixed24Dot8One), CFixed24Dot8Half);
   CenterY := FixedSub(BorderWidthFixed, CFixed24Dot8One);

   // calculate affected scanlines
   YRange[0] := FixedRoundHalfUp(FixedSub(CenterY, BorderWidthFixed));
   YRange[1] := FixedRoundHalfUp(FixedAdd(CenterY, BorderWidthFixed));

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;

   SqrRadMinusOne := FixedSqr(FixedSub(BorderWidthFixed, CFixed24Dot8One));

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := FixedSqr(FixedSub(ConvertToFixed24Dot8(Y), CenterY));

     XStart.Fixed := FixedSqr(BorderWidthFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // calculate affected pixels within this scanline
     XRange[0] := FixedRoundHalfUp(CenterX);
     XRange[1] := FixedRoundHalfUp(FixedAdd(CenterX, XStart));

     ScnLne[0] := Scanline[FYOffset + Y];
     X := XRange[0];
     while X <= XRange[1] do
      begin
       // calculate squared distance
       SqrDist.Fixed := X shl 8 - CenterX.Fixed;
       SqrDist.Fixed := FixedSqr(SqrDist).Fixed + SqrYDist.Fixed;
       CombColor := BorderColor;
       if SqrDist.Fixed >= SqrRadMinusOne.Fixed then
        begin
         SqrDist.Fixed := BorderWidthFixed.Fixed - FixedSqrt(SqrDist).Fixed;
         if SqrDist.Fixed < $FF
          then CombColor.A := ((SqrDist.Fixed * CombColor.A + $7F) shr 8);
         BlendPixelInplace(CombColor, ScnLne[0][X]);
        end
       else BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X);
      end;
    end;


   CenterX := FixedAdd(CenterX, ConvertToFixed24Dot8(Integer(FTextWidth - 1)));
   CenterX.Frac := 0;

   // calculate affected scanlines
   YRange[0] := FixedRoundHalfUp(FixedSub(CenterY, BorderWidthFixed));
   YRange[1] := FixedRoundHalfUp(FixedAdd(CenterY, BorderWidthFixed));

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;

   SqrRadMinusOne := FixedSqr(FixedSub(BorderWidthFixed, CFixed24Dot8One));

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := FixedSqr(FixedSub(ConvertToFixed24Dot8(Y), CenterY));

     XStart.Fixed := FixedSqr(BorderWidthFixed).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;

     // calculate affected pixels within this scanline
     XRange[0] := FixedRoundHalfUp(FixedSub(CenterX, XStart));
     XRange[1] := FixedRoundHalfUp(CenterX);

     ScnLne[0] := Scanline[FYOffset + Y];
     X := XRange[0];
     while X <= XRange[1] do
      begin
       // calculate squared distance
       SqrDist.Fixed := X shl 8 - CenterX.Fixed;
       SqrDist.Fixed := FixedSqr(SqrDist).Fixed + SqrYDist.Fixed;
       CombColor := BorderColor;
       if SqrDist.Fixed >= SqrRadMinusOne.Fixed then
        begin
         SqrDist.Fixed := BorderWidthFixed.Fixed - FixedSqrt(SqrDist).Fixed;
         if SqrDist.Fixed < $FF
          then CombColor.A := ((SqrDist.Fixed * CombColor.A + $7F) shr 8);
         BlendPixelInplace(CombColor, ScnLne[0][X]);
        end
       else BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X);
      end;
    end;


   EMMS;
  end;
end;

end.
