unit DAV_GuiAudioDataDisplay;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Controls, Graphics, DAV_Common, DAV_GuiCommon, DAV_GuiBaseControl,
  DAV_AudioData, DAV_GuiAudioDataDisplayCursor, DAV_GuiAudioDataDisplayAxis;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmLine, wdmSolid, wdmPoints, wdmOutline);

  TDisplayChannels = class(TOwnedCollection)
  private
    FOnChanged: TNotifyEvent;
  protected
    {$IFDEF Delphi6_Up}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ELSE}
    procedure Update(Item: TCollectionItem); override;
    {$ENDIF}
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Items; default;
  end;

  TCustomDisplayChannel = class(TCollectionItem)
  private
    FDisplayName  : string;
    FColor        : TColor;
    procedure SetColor(const Value: TColor);
    procedure ColorChanged;
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property DisplayName;
    property Color: TColor read FColor write SetColor;
  end;

  TCustomGuiAudioDataDisplay = class(TCustomControl)
  private
    FAntiAlias            : TGuiAntiAlias;
    FAudioData            : TCustomAudioData;
    FAudioDataCollection  : TCustomAudioDataCollection;
    FBuffer               : TBitmap;
    FDisplayedChannel     : Integer;
    FDisplayChannels      : TDisplayChannels;
    FHalfHeight           : Integer;
    FLineColor            : TColor;
    FLineWidth            : Integer;
    FOSFactor             : Integer;
    FSolidColor           : TColor;
    FTransparent          : Boolean;
    FWaveDrawMode         : TGuiWaveDrawMode;
    FNormalize            : Boolean;
    FScaleFactor          : Single;
    FCursor               : TGuiAudioDataDisplayCursor;
    FXAxis                : TGuiAudioDataDisplayXAxis;

    function GetChannelCount: Integer;
    function GetSampleFrames: Integer;
    procedure AxisChangedHandler(Sender: TObject);
    procedure CursorChangedHandler(Sender: TObject);
    procedure DisplayChannelsChangedHandler(Sender: TObject);
    procedure DrawChannelData(Bitmap: TBitmap; Channel: Integer);
    procedure SetAntiAlias(const Value: TGuiAntiAlias);
    procedure SetAudioData(const Value: TCustomAudioData);
    procedure SetAudioDataCollection(const Value: TCustomAudioDataCollection);
    procedure SetCursor(const Value: TGuiAudioDataDisplayCursor);
    procedure SetDisplayChannels(const Value: TDisplayChannels);
    procedure SetDisplayedChannel(Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
    procedure SetNormalize(const Value: Boolean);
    procedure SetSolidColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWaveDrawMode(Value: TGuiWaveDrawMode);
    procedure SetXAxis(const Value: TGuiAudioDataDisplayXAxis);
    procedure RenderDisplayToBitmap(Bitmap: TBitmap);
    procedure RenderCursorsToBitmap(Bitmap: TBitmap);
    procedure DownsampleBitmap(Bitmap: TBitmap);
    procedure UpsampleBitmap(Bitmap: TBitmap);
    {$IFNDEF FPC}
    procedure DrawParentImage(Dest: TCanvas); virtual;
    {$ENDIF}
    procedure CalculateScaleFactor;
  protected
    procedure Resize; override;
//    procedure RedrawBuffer(doBufferFlip: Boolean = False); override;
    procedure Loaded; override;
    procedure RenderBuffer; virtual;

    procedure AntiAliasChanged; virtual;
    procedure AudioDataChanged; virtual;
    procedure AudioDataCollectionChanged; virtual;
    procedure NormalizeChanged; virtual;
    procedure LineColorChanged; virtual;
    procedure LineWidthChanged; virtual;
    procedure TransparentChanged; virtual;
    procedure WaveDrawModeChanged; virtual;

    property SampleFrames: Integer read GetSampleFrames;
    property ChannelCount: Integer read GetChannelCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property AntiAlias: TGuiAntiAlias read FAntiAlias write SetAntiAlias default gaaNone;
    property AudioData: TCustomAudioData read FAudioData write SetAudioData;
    property AudioDataCollection: TCustomAudioDataCollection read FAudioDataCollection write SetAudioDataCollection;
    property DisplayedChannel: Integer read FDisplayedChannel write SetDisplayedChannel default -1;
    property DisplayChannels: TDisplayChannels read FDisplayChannels write SetDisplayChannels;
    property Cursor: TGuiAudioDataDisplayCursor read FCursor write SetCursor;
    property SolidColor: TColor read FSolidColor write SetSolidColor default clRed;

    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    property Normalize: Boolean read FNormalize write SetNormalize default True;
    {$IFNDEF FPC}
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {$ENDIF}
    property WaveDrawMode: TGuiWaveDrawMode read FWaveDrawMode write SetWaveDrawMode default wdmLine;

    property XAxis: TGuiAudioDataDisplayXAxis read FXAxis write SetXAxis;
  end;

  TGuiAudioDataDisplay = class(TCustomGuiAudioDataDisplay)
  published
    property Align;
    property Anchors;
    property AntiAlias;
    property AudioData;
    property AudioDataCollection;
    property Color;
    property Constraints;
    property Cursor;
    property DisplayedChannel;
    property DisplayChannels;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property PopupMenu;
    property LineColor;
    property LineWidth;
    property Normalize;
    property ShowHint;
    property Visible;
    property WaveDrawMode;
    property XAxis;

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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
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
  Math, SysUtils;

{ TDisplayChannels }

{$IFDEF Delphi6_Up}
procedure TDisplayChannels.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
{$ELSE}
procedure TDisplayChannels.Update(Item: TCollectionItem);
{$ENDIF}
begin
 inherited;
 if Assigned(OnChanged)
  then OnChanged(Self);
end;


{ TCustomDisplayChannel }

constructor TCustomDisplayChannel.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := 'Channel ' + IntToStr(Collection.Count);
end;

function TCustomDisplayChannel.GetDisplayName: string;
begin
 result := FDisplayName;
end;

procedure TCustomDisplayChannel.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDisplayChannel then
  begin
   TCustomDisplayChannel(Dest).FDisplayName := FDisplayName;
   TCustomDisplayChannel(Dest).FColor := FColor;
  end
 else inherited;
end;

procedure TCustomDisplayChannel.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TCustomDisplayChannel.ColorChanged;
begin

end;

procedure TCustomDisplayChannel.SetDisplayName(const Value: string);
begin
 FDisplayName := Value;
 inherited;
end;


{ TCustomGuiAudioDataDisplay }

constructor TCustomGuiAudioDataDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle        := ControlStyle + [csOpaque];
  FLineColor          := clBlack;
  FSolidColor         := clRed;
  FDisplayedChannel   := -1;
  FOSFactor           := 1;
  FScaleFactor        := 1;
  FWaveDrawMode       := wdmLine;
  FBuffer             := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;

  FCursor             := TGuiAudioDataDisplayCursor.Create;
  FCursor.OnChanged   := CursorChangedHandler;

  FXAxis              := TGuiAudioDataDisplayXAxis.Create;
  FXAxis.OnChanged    := AxisChangedHandler;

  FDisplayChannels    := TDisplayChannels.Create(Self, TCustomDisplayChannel);
  FDisplayChannels.OnChanged := DisplayChannelsChangedHandler;
end;

destructor TCustomGuiAudioDataDisplay.Destroy;
begin
 FreeAndNil(FXAxis);
 FreeAndNil(FCursor);
 FreeAndNil(FBuffer);
 FreeAndNil(FDisplayChannels);
 inherited;
end;

procedure TCustomGuiAudioDataDisplay.DisplayChannelsChangedHandler(Sender: TObject);
begin
 //
end;

procedure TCustomGuiAudioDataDisplay.CursorChangedHandler(Sender: TObject);
begin
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.AxisChangedHandler(Sender: TObject);
begin
 Invalidate;
end;

{$IFNDEF FPC}
procedure TCustomGuiAudioDataDisplay.DrawParentImage(Dest: TCanvas);
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

function TCustomGuiAudioDataDisplay.GetSampleFrames: Integer;
begin
 if Assigned(FAudioDataCollection)
  then result := FAudioDataCollection.SampleFrames
  else result := 0;
end;

procedure TCustomGuiAudioDataDisplay.Loaded;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
 FHalfHeight    := Height div 2;
end;

function TCustomGuiAudioDataDisplay.GetChannelCount: Integer;
begin
 if Assigned(FAudioDataCollection)
  then Result := FAudioDataCollection.Channels.Count else
 if Assigned(FAudioData)
  then Result := 1
  else Result := 0;
end;

procedure TCustomGuiAudioDataDisplay.SetAntiAlias(const Value: TGuiAntiAlias);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.AntiAliasChanged;
begin
 case FAntiAlias of
       gaaNone : FOSFactor :=  1;
   gaaLinear2x : FOSFactor :=  2;
   gaaLinear3x : FOSFactor :=  3;
   gaaLinear4x : FOSFactor :=  4;
   gaaLinear8x : FOSFactor :=  8;
  gaaLinear16x : FOSFactor := 16;
 end;
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.SetAudioData(const Value: TCustomAudioData);
begin
 if FAudioData <> Value then
  begin
   FAudioData := Value;
   AudioDataChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetAudioDataCollection(const Value: TCustomAudioDataCollection);
begin
 if FAudioDataCollection <> Value then
  begin
   FAudioDataCollection := Value;
   AudioDataCollectionChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.AudioDataChanged;
begin
 if Assigned(FAudioData) then
  begin
   FXAxis.SetBounds(0, FAudioData.SampleCount - 1);
   CalculateScaleFactor;
  end
 else FScaleFactor := 1;
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.AudioDataCollectionChanged;
begin
 if Assigned(FAudioDataCollection) then
  begin
   FXAxis.SetBounds(0, FAudioDataCollection.SampleFrames - 1);
   CalculateScaleFactor;
  end
 else FScaleFactor := 1;
 Invalidate;
end;

procedure TCustomGuiAudioDataDisplay.CalculateScaleFactor;
var
  Peak : Single;
begin
 if FNormalize then
  begin
   if Assigned(FAudioData) then
    begin
     Peak := FAudioData.Peak;
     if Peak > 0
      then FScaleFactor := 1 / Peak;
    end else
   if Assigned(FAudioDataCollection) then
    begin
     Peak := FAudioDataCollection.Peak;
     if Peak > 0
      then FScaleFactor := 1 / Peak;
    end
   else FScaleFactor := 1;
  end
 else FScaleFactor := 1;
end;

procedure TCustomGuiAudioDataDisplay.SetCursor(const Value: TGuiAudioDataDisplayCursor);
begin
 FCursor.Assign(Value);
end;

procedure TCustomGuiAudioDataDisplay.SetDisplayChannels(
  const Value: TDisplayChannels);
begin
 FDisplayChannels.Assign(Value);
end;

procedure TCustomGuiAudioDataDisplay.SetDisplayedChannel(Value: Integer);
begin
 if Value < -1 then Value := -1;
 if FDisplayedChannel <> Value then
  begin
   FDisplayedChannel := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetSolidColor(const Value: TColor);
begin
 if FSolidColor <> Value then
  begin
   FSolidColor := Value;
   Invalidate;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetLineColor(const Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   LineColorChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetLineWidth(const Value: Integer);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   LineWidthChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetNormalize(const Value: Boolean);
begin
 if FNormalize <> Value then
  begin
   FNormalize := Value;
   NormalizeChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.SetWaveDrawMode(Value: TGuiWaveDrawMode);
begin
 if FWaveDrawMode <> Value then
  begin
   FWaveDrawMode := Value;
   WaveDrawModeChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplay.LineColorChanged;
begin
 Changed;
end;

procedure TCustomGuiAudioDataDisplay.LineWidthChanged;
begin
 Changed;
end;

procedure TCustomGuiAudioDataDisplay.NormalizeChanged;
begin
 Changed;
 CalculateScaleFactor;
end;

procedure TCustomGuiAudioDataDisplay.WaveDrawModeChanged;
begin
 Changed;
end;

procedure TCustomGuiAudioDataDisplay.TransparentChanged;
begin
 Changed;
end;

procedure TCustomGuiAudioDataDisplay.SetXAxis(
  const Value: TGuiAudioDataDisplayXAxis);
begin
 FXAxis.Assign(Value);
end;

(*
procedure TCustomGuiAudioDataDisplay.RedrawBuffer(doBufferFlip: Boolean);
begin
 inherited;
 Invalidate;
end;
*)

procedure TCustomGuiAudioDataDisplay.Resize;
begin
 inherited;
 FBuffer.Width  := Width;
 FBuffer.Height := Height;
 FHalfHeight    := Height div 2;
end;

// Drawing stuff

procedure TCustomGuiAudioDataDisplay.UpsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Upsample2xBitmap32(Bitmap);
   gaaLinear3x: Upsample3xBitmap32(Bitmap);
   gaaLinear4x: Upsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Upsample4xBitmap32(Bitmap);
                 Upsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TCustomGuiAudioDataDisplay.DownsampleBitmap(Bitmap: TBitmap);
begin
 case FAntiAlias of
   gaaLinear2x: Downsample2xBitmap32(Bitmap);
   gaaLinear3x: Downsample3xBitmap32(Bitmap);
   gaaLinear4x: Downsample4xBitmap32(Bitmap);
   gaaLinear8x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample2xBitmap32(Bitmap);
                end;
  gaaLinear16x: begin
                 Downsample4xBitmap32(Bitmap);
                 Downsample4xBitmap32(Bitmap);
                end;
  else raise Exception.Create('not yet supported');
 end;
end;

procedure TCustomGuiAudioDataDisplay.Paint;
begin
 RenderBuffer;
 Canvas.Draw(0, 0, FBuffer);
 inherited;
end;

procedure TCustomGuiAudioDataDisplay.RenderBuffer;
var
  Bmp: TBitmap;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    Brush.Assign(Canvas.Brush);

    case FAntiAlias of
     gaaNone:
      begin
       // draw background
       {$IFNDEF FPC}
       if FTransparent
        then DrawParentImage(FBuffer.Canvas)
        else
       {$ENDIF}
        begin
         Brush.Color := Self.Color;
         FillRect(ClipRect);
        end;
       RenderDisplayToBitmap(FBuffer);
      end;
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := FOSFactor * FBuffer.Width;
         Height      := FOSFactor * FBuffer.Height;
         {$IFNDEF FPC}
         if FTransparent then
          begin
           CopyParentImage(Self, Bmp.Canvas);
//           DrawParentImage(Bmp.Canvas);
           UpsampleBitmap(Bmp);
          end
         else
         {$ENDIF}
          with Bmp.Canvas do
           begin
            Brush.Color := Self.Color;
            FillRect(ClipRect);
           end;
         RenderDisplayToBitmap(Bmp);
         DownsampleBitmap(Bmp);
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;
    end;
    Unlock;
   end;
end;

procedure TCustomGuiAudioDataDisplay.RenderDisplayToBitmap(Bitmap: TBitmap);
var
  ch: Integer;
begin
 if (ChannelCount > 0) and (ChannelCount > FDisplayedChannel) then
  if FDisplayedChannel >= 0
   then DrawChannelData(Bitmap, FDisplayedChannel)
   else
    for ch := 0 to ChannelCount - 1 do DrawChannelData(Bitmap, ch);
 RenderCursorsToBitmap(Bitmap);
end;

procedure TCustomGuiAudioDataDisplay.RenderCursorsToBitmap(Bitmap: TBitmap);
begin
 // yet todo
end;

procedure TCustomGuiAudioDataDisplay.DrawChannelData(Bitmap: TBitmap; Channel: Integer);
var
  PixelPerSample     : Single;
  MinVal, MaxVal     : Single;
  Sample             : Cardinal;
  HlfHght            : Integer;
  XPixelPosAsSingle  : Single;
  XPixelPosAsInt, o  : Integer;
begin
 with Bitmap.Canvas do
  begin
   if SampleFrames = 0 then exit;
   HlfHght := FOSFactor * FHalfHeight;
   PixelPerSample := FOSFactor * Self.Width / (SampleFrames - 1);
   Pen.Width := FOSFactor * FLineWidth;

   if Channel < FDisplayChannels.Count
    then Pen.Color := TCustomDisplayChannel(FDisplayChannels[Channel]).Color
    else Pen.Color := FLineColor;

   if (FAudioDataCollection.Channels.Items[Channel] is TAudioChannel32) then
    with TAudioChannel32(FAudioDataCollection.Channels.Items[Channel]) do
     begin
      if SampleCount = 0 then Exit;
      Sample := Cardinal(FXAxis.SampleLower);
      MinVal := ChannelDataPointer^[Sample];
      MaxVal := MinVal;

      MoveTo(0, Round((1 - FScaleFactor * MinVal) * HlfHght));
      XPixelPosAsInt    := 0;
      XPixelPosAsSingle := 0;
      inc(Sample);

      while Sample < SampleCount do
       begin
        // check for minimum and maximum
        if ChannelDataPointer^[Sample] > MaxVal then MaxVal := ChannelDataPointer^[Sample] else
        if ChannelDataPointer^[Sample] < MinVal then MinVal := ChannelDataPointer^[Sample];

        XPixelPosAsSingle := XPixelPosAsSingle + PixelPerSample;
        if XPixelPosAsSingle > XPixelPosAsInt then
         begin
          XPixelPosAsInt := Round(XPixelPosAsSingle + 0.5);
          if MinVal = MaxVal then
           begin
            LineTo(XPixelPosAsInt, HlfHght * Round((1 - FScaleFactor * MinVal)));
           end
          else
           begin
            o := PenPos.Y - HlfHght;
            if abs(o - MinVal * HlfHght) > abs(o - MaxVal * HlfHght)
             then
              begin
               LineTo(XPixelPosAsInt, Round((1 - FScaleFactor * MinVal) * HlfHght));
               LineTo(XPixelPosAsInt, Round((1 - FScaleFactor * MaxVal) * HlfHght));
              end
             else
              begin
               LineTo(XPixelPosAsInt, Round((1 - FScaleFactor * MaxVal) * HlfHght));
               LineTo(XPixelPosAsInt, Round((1 - FScaleFactor * MinVal) * HlfHght));
              end;
           end;
          MinVal := ChannelDataPointer^[Sample];
          MaxVal := MinVal;
         end;
        inc(Sample);
       end;
     end;
  end;
end;

end.
