unit DAV_GuiModular;

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, Forms, Messages, SysUtils, Controls,
  DAV_GuiBaseControl, DAV_ModularManager, DAV_ModularContainer,
  DAV_ModularPin;

type
  TCustomGuiModular = class(TCustomControl)
  private
    FBuffer        : TBitmap;
    FPinSize       : Integer;
    FModuleManager : TModularManager;
    procedure SetModuleManager(const Value: TModularManager);
    procedure SetPinSize(const Value: Integer);
    procedure RenderModule(ModuleItem: TCustomModularItem);
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure Paint; override;
    

    procedure RenderBuffer; virtual;
(*
    procedure RenderInputPins; virtual;
    procedure RenderOutputPins; virtual;
*)

    procedure PinSizeChanged; virtual;
    procedure ModuleManagerChanged; virtual;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PinSize: Integer read FPinSize write SetPinSize;
    property ModuleManager: TModularManager read FModuleManager write SetModuleManager;
  end;

  TGuiModular = class(TCustomGuiModular)
  published
    property Align;
    property Anchors;
    property Color;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ModuleManager;
    property OnClick;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    {$IFDEF DELPHICOMPILER8_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentFont;
    property PinSize;
    property PopupMenu;
    property Visible;
  end;

implementation

uses
  Math;

{ TCustomGuiModular }

constructor TCustomGuiModular.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 ControlStyle        := ControlStyle + [csOpaque];
 FBuffer             := TBitmap.Create;
 FBuffer.PixelFormat := pf24bit;
 FPinSize            := 8;
end;

destructor TCustomGuiModular.Destroy;
begin
 FreeAndNil(FBuffer);
 inherited;
end;

procedure TCustomGuiModular.Loaded;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
end;

procedure TCustomGuiModular.Paint;
begin
 RenderBuffer;
 Canvas.Draw(0, 0, FBuffer);
 inherited;
end;

procedure TCustomGuiModular.RenderBuffer;
var
  ModuleNo : Integer;
begin
 with FBuffer, Canvas do
  if (Width > 0) and (Height > 0) then
   begin
    Brush.Color := Self.Color;
    FillRect(ClipRect);
//    Font.Assign(Self.Font);

    // check whether a module manager is assigned
    if not assigned(FModuleManager) then Exit;

    // render modules
    for ModuleNo := 0 to FModuleManager.ModuleCount - 1
     do RenderModule(FModuleManager.ModuleItem[ModuleNo]);
   end;
end;

procedure TCustomGuiModular.RenderModule(ModuleItem: TCustomModularItem);
var
  DefaultHeight : Integer;
  InputHeight   : Integer;
  OutputHeight  : Integer;
  CurrentWidth  : Integer;
  MaximumWidth  : Integer;
  PinNo         : Integer;
  ModuleRect    : TRect;
  ModuleWidth   : Integer;
  ModuleHeight  : Integer;
  PinCenter     : Integer;
begin
 with FBuffer, Canvas do
  begin
   // estimate default height
   DefaultHeight := (TextHeight('Pin') + 2);

   // estimate height of the input pins
   InputHeight := 2 + (ModuleItem.Module.PinCountInput + 1) * DefaultHeight;

   // estimate height of the output pins
   OutputHeight := 2 + (ModuleItem.Module.PinCountOutput + 1) * DefaultHeight;

   // estimate pin width
   MaximumWidth := TextWidth(ModuleItem.Module.Name);
   with ModuleItem.Module do
    for PinNo := 0 to min(PinCountInput, PinCountOutput) - 1 do
     begin
      CurrentWidth := 2 * PinSize + TextWidth(PinInput[PinNo].DisplayName +
        ' - ' +  PinOutput[PinNo].DisplayName);
      if CurrentWidth > MaximumWidth
       then MaximumWidth := CurrentWidth;
     end;

   // estimate pin width for single pins
   with ModuleItem.Module do
    if PinCountInput > PinCountOutput then
     for PinNo := PinCountOutput to PinCountInput - 1 do
      begin
       CurrentWidth := 4 + PinSize + TextWidth(PinInput[PinNo].DisplayName);
       if CurrentWidth > MaximumWidth
        then MaximumWidth := CurrentWidth;
      end
    else
     for PinNo := PinCountOutput to PinCountInput - 1 do
      begin
       CurrentWidth := 4 + PinSize + TextWidth(PinOutput[PinNo].DisplayName);
       if CurrentWidth > MaximumWidth
        then MaximumWidth := CurrentWidth;
      end;

   ModuleWidth := MaximumWidth;
   ModuleHeight := Max(InputHeight, OutputHeight);
   if ModuleItem.Module is TModularIO then
    begin
     ModuleHeight := ModuleHeight + DefaultHeight;
     ModuleWidth := max(ModuleWidth, 4 + 2 * PinSize + TextWidth('Spare'));
    end;

   ModuleRect := Rect(ModuleItem.Left, ModuleItem.Top, ModuleItem.Left +
     ModuleWidth, ModuleItem.Top + ModuleHeight);

   // draw panel
   Brush.Color := clSilver;
   Brush.Style := bsSolid;
   Pen.Color := clBlack;
   Rectangle(ModuleRect);

   // draw name
   Brush.Color := $6A240A;
   Pen.Color := $6A240A;
   Font.Color := clWhite;

   FillRect(Rect(ModuleRect.Left, ModuleRect.Top, ModuleRect.Right,
     ModuleRect.Top + DefaultHeight));

   // render caption
   with ModuleItem.Module do
     TextOut(ModuleItem.Left + (ModuleWidth - TextWidth(Name)) div 2,
       ModuleRect.Top + 1, ModuleItem.Module.Name);

   Font.Color := Self.Font.Color;
   Pen.Color := clBlack;
   Brush.Style := bsClear;

   // render input pins
   with ModuleItem.Module do
    for PinNo := 0 to PinCountInput - 1 do
     begin
      case PinInput[PinNo].Datatype of
       mdtDouble : Canvas.Font.Color := $960032;
       mdtSingle  : Canvas.Font.Color := $8c5000;
       mdtInteger : Canvas.Font.Color := clOlive;
       else Canvas.Font.Color := clBlack;
      end;

      PinCenter := ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight + DefaultHeight div 2;
      Rectangle(Rect(ModuleRect.Left, PinCenter - PinSize div 2,
        ModuleRect.Left + Pinsize - 1, PinCenter + PinSize div 2));

      TextOut(Pinsize + ModuleRect.Left + 2,
        ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight,
        PinInput[PinNo].DisplayName);
     end;

   // render output pins
   with ModuleItem.Module do
    for PinNo := 0 to PinCountOutput - 1 do
     begin
      case PinOutput[PinNo].Datatype of
       mdtDouble : Canvas.Font.Color := $960032;
       mdtSingle  : Canvas.Font.Color := $8c5000;
       mdtInteger : Canvas.Font.Color := clOlive;
       else Canvas.Font.Color := clBlack;
      end;

      PinCenter := ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight + DefaultHeight div 2;
      Rectangle(Rect(ModuleRect.Right - Pinsize + 1, PinCenter - PinSize div 2,
        ModuleRect.Right, PinCenter + PinSize div 2));

      TextOut(ModuleRect.Right - Pinsize - TextWidth(PinOutput[PinNo].DisplayName),
        ModuleRect.Top + 1 + (PinNo + 1) * DefaultHeight,
        PinOutput[PinNo].DisplayName);
     end;

   if ModuleItem.Module is TModularIO then
    begin
     PinCenter := ModuleRect.Bottom - 2 - DefaultHeight div 2;
     Rectangle(Rect(ModuleRect.Right - Pinsize + 1, PinCenter - PinSize div 2,
       ModuleRect.Right, PinCenter + PinSize div 2));

     Rectangle(Rect(ModuleRect.Left, PinCenter - PinSize div 2,
       ModuleRect.Left + Pinsize - 1, PinCenter + PinSize div 2));

     TextOut((ModuleRect.Left + ModuleRect.Right -
       TextWidth('Spare')) div 2, ModuleRect.Bottom - 2 - DefaultHeight,
       'Spare');
    end;
  end;
end;

procedure TCustomGuiModular.Resize;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
end;

procedure TCustomGuiModular.CMFontChanged(var Message: TMessage);
begin
 FBuffer.Canvas.Font.Assign(Self.Font);
end;

procedure TCustomGuiModular.SetModuleManager(const Value: TModularManager);
begin
 if FModuleManager <> Value then
  begin
   FModuleManager := Value;
   ModuleManagerChanged;
  end;
end;

procedure TCustomGuiModular.SetPinSize(const Value: Integer);
begin
 if FPinSize <> Value then
  begin
   FPinSize := Value;
   PinSizeChanged;
  end;
end;

procedure TCustomGuiModular.PinSizeChanged;
begin
 // nothing yet in here
end;

procedure TCustomGuiModular.ModuleManagerChanged;
begin
 Invalidate;
end;

end.
