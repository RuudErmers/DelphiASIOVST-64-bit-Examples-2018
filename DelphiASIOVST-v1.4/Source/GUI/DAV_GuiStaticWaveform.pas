unit DAV_GuiStaticWaveform;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DAV_GuiBaseControl, DAV_Types;

type
  TGuiNormalizationType = (ntNone, ntPerChannel, ntOverallChannels);
  TGuiWaveDrawMode = (wdmSolid, wdmPoints, wdmOutline, wdmSimple);

  TCustomGuiStaticWaveform = class(TCustomGuiBaseMouseControl)
  private
    FNormalizationType    : TGuiNormalizationType;
    FNormalizationFactors : TDAVSingleDynArray;
    FWaveHalfHeight       : Integer;
    FWaveData             : TDAVArrayOfSingleDynArray;
    FWaveVPadding         : Integer;
    FDisplayChannels      : Integer;
    FMedianVisible        : Boolean;
    FMedianColor          : TColor;
    FMedianLineWidth      : Integer;
    FWaveDrawMode         : TGuiWaveDrawMode;

    procedure SetNormalizationType(Value: TGuiNormalizationType);
    function  GetWaveLength: Integer;
    function  GetWaveChannels: Integer;
    procedure SetWaveVPadding(Value: Integer);
    procedure SetDisplayChannels(Value: Integer);

    procedure SetMedianVisible(Value: Boolean);
    procedure SetMedianColor(Value: TColor);
    procedure SetMedianLineWidth(Value: Integer);
    procedure SetWaveDrawMode(Value: TGuiWaveDrawMode);
    procedure SetWaveLength(const Value: Integer);
    procedure SetWaveChannels(const Value: Integer);
    procedure MedianColorChanged;
    procedure MedianVisibleChanged;
    procedure WaveVPaddingChanged;
    procedure NormalizationTypeChanged;
    procedure WaveDrawModeChanged;
    procedure RenderToBitmap(Bitmap: TBitmap);
  protected
    procedure DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
    procedure ResizeBuffer; override;
    procedure DrawMedian(YOffset: Integer);
    procedure DrawGraphs(Bitmap: TBitmap); virtual;
    procedure DrawSingleWave(YOffset, HalfHeight, Channel: Integer);
    function  GetMaxAmp(Channel: Integer): single;
    procedure MedianLineWidthChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateBuffer; override;

    procedure SetWaveForm(const NewWaveData: PDAVSingleFixedArray; const NewWaveDataLength: Integer; const UpdateBuffer: Boolean = False); overload;
    procedure SetWaveForm(const NewWaveData: TDAVSingleDynArray; const UpdateBuffer: Boolean = False); overload;
    procedure SetWaveForm(const NewWaveData: TDAVArrayOfSingleDynArray; const UpdateBuffer: Boolean = False); overload;
    procedure ClearWaveForm(UpdateBuffer: Boolean = False);

    property Wavedata: TDAVArrayOfSingleDynArray read FWaveData;
    property WaveLength: Integer read GetWaveLength write SetWaveLength;
    property WaveChannels: Integer read GetWaveChannels write SetWaveChannels;
    property DisplayChannels: Integer read FDisplayChannels write SetDisplayChannels default 2;
    property WaveVPadding: Integer read FWaveVPadding write SetWaveVPadding default 3;

    property MedianVisible: Boolean read FMedianVisible write SetMedianVisible default True;
    property MedianColor: TColor read FMedianColor write SetMedianColor default clRed;
    property MedianLineWidth: Integer read FMedianLineWidth write SetMedianLineWidth default 1;
    property NormalizationType: TGuiNormalizationType read FNormalizationType write SetNormalizationType default ntNone;
    property WaveDrawMode: TGuiWaveDrawMode read FWaveDrawMode write SetWaveDrawMode default wdmSolid;
  end;

  TGuiStaticWaveform = class(TCustomGuiStaticWaveform)
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DisplayChannels;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property LineColor;
    property LineWidth;
    property MedianColor;
    property MedianLineWidth;
    property MedianVisible;
    property NormalizationType;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property WaveDrawMode;
    property WaveVPadding;
    {$IFNDEF FPC}
    property Transparent;
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDragMouseMove;
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
    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses Math;

constructor TCustomGuiStaticWaveform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNormalizationType := ntNone;
  
  FDisplayChannels := 2;
  FWaveVPadding    := 3;
  FMedianVisible   := True;
  FMedianColor     := clRed;
  FMedianLineWidth := 1;
  FWaveDrawMode    := wdmSolid;

  SetLength(FNormalizationFactors, FDisplayChannels); // !IMPORTANT
  ClearWaveForm;
end;

destructor TCustomGuiStaticWaveform.Destroy;
begin
  ClearWaveForm;
  inherited;
end;

procedure TCustomGuiStaticWaveform.ClearWaveForm(UpdateBuffer: Boolean);
begin
 SetLength(FWaveData, 0, 0);

 if UpdateBuffer then Invalidate;
end;

function TCustomGuiStaticWaveform.GetWaveLength: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FWaveData) - 1
   do Result := Max(Result, Length(FWaveData[i]));
end;

function TCustomGuiStaticWaveform.GetWaveChannels: Integer;
begin
  Result := Length(FWaveData);
end;

procedure TCustomGuiStaticWaveform.SetDisplayChannels(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FDisplayChannels <> Value then
   begin
    FDisplayChannels := Value;
    ResizeBuffer;
   end;
end;


procedure TCustomGuiStaticWaveform.SetMedianVisible(Value: Boolean);
begin
 if FMedianVisible <> Value then
  begin
   FMedianVisible := Value;
   MedianVisibleChanged;
  end;
end;

procedure TCustomGuiStaticWaveform.MedianVisibleChanged;
begin
 Invalidate;
end;

procedure TCustomGuiStaticWaveform.SetMedianColor(Value: TColor);
begin
  if FMedianColor <> Value then
  begin
    FMedianColor := Value;
    MedianColorChanged;
  end;
end;

procedure TCustomGuiStaticWaveform.MedianColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiStaticWaveform.SetMedianLineWidth(Value: Integer);
begin
  if FMedianLineWidth <> Value then
  begin
    FMedianLineWidth := Value;
    MedianLineWidthChanged;
  end;
end;

procedure TCustomGuiStaticWaveform.MedianLineWidthChanged;
begin
 Invalidate;
end;

procedure TCustomGuiStaticWaveform.SetWaveVPadding(Value: Integer);
begin
 if FWaveVPadding <> Value then
  begin
   FWaveVPadding := Value;
   WaveVPaddingChanged;
  end;
end;

procedure TCustomGuiStaticWaveform.WaveVPaddingChanged;
begin
 ResizeBuffer;
end;

procedure TCustomGuiStaticWaveform.SetNormalizationType(Value: TGuiNormalizationType);
begin
 if FNormalizationType <> Value then
  begin
   FNormalizationType := Value;
   NormalizationTypeChanged;
  end;
end;

procedure TCustomGuiStaticWaveform.NormalizationTypeChanged;
begin
 Invalidate;
end;

procedure TCustomGuiStaticWaveform.SetWaveChannels(const Value: Integer);
begin
 if Value <> WaveChannels then
  begin
   SetLength(FWaveData, Value);
  end;
end;

procedure TCustomGuiStaticWaveform.SetWaveDrawMode(Value: TGuiWaveDrawMode);
begin
 if FWaveDrawMode <> Value then
  begin
   FWaveDrawMode := Value;
   WaveDrawModeChanged;
  end;
end;

procedure TCustomGuiStaticWaveform.WaveDrawModeChanged;
begin
 Invalidate;
end;

procedure TCustomGuiStaticWaveform.ResizeBuffer;
begin
 FWaveHalfHeight := Height div (2 * FDisplayChannels) - FWaveVPadding;
 SetLength(FNormalizationFactors, FDisplayChannels);

 inherited;
end;

procedure TCustomGuiStaticWaveform.SetWaveForm(const NewWaveData: TDAVSingleDynArray; const UpdateBuffer: Boolean);
begin
 ClearWaveForm;
 SetLength(FWaveData, 1);
 FWaveData[0] := NewWaveData;

 if UpdateBuffer
  then Invalidate;
end;

procedure TCustomGuiStaticWaveform.SetWaveForm(const NewWaveData: TDAVArrayOfSingleDynArray;
  const UpdateBuffer: Boolean = False);
var
  i, len: Integer;
begin
 if Length(NewWaveData) < 1
  then ClearWaveForm(UpdateBuffer)
  else
   begin
    ClearWaveForm;
    SetLength(FWaveData, Length(NewWaveData));
    for i := 0 to Length(NewWaveData) - 1 do
     begin
      len := Length(NewWaveData[i]);
      SetLength(FWaveData[i], len);
      Move(NewWaveData[i][0], FWaveData[i][0], len * SizeOf(Single));
     end;

    if UpdateBuffer
     then Invalidate;
   end;
end;

procedure TCustomGuiStaticWaveform.SetWaveForm(const NewWaveData: PDAVSingleFixedArray;
  const NewWaveDataLength: Integer; const UpdateBuffer: Boolean = False);
begin
 ClearWaveForm;
 SetLength(FWaveData, 1, NewWaveDataLength);
 Move(NewWaveData^, FWaveData[0, 0], NewWaveDataLength * SizeOf(Single));

 if UpdateBuffer
  then Invalidate;
end;

procedure TCustomGuiStaticWaveform.SetWaveLength(const Value: Integer);
var
  Channel : Integer;
begin
 if Value <> WaveLength then
  begin
   for Channel := 0 to Length(FWaveData) - 1 do
   SetLength(FWaveData[Channel], Value);
  end;
end;

function TCustomGuiStaticWaveform.GetMaxAmp(Channel: Integer):single;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FWaveData[Channel]) - 1
   do Result := max(Result, abs(FWaveData[Channel][i]));
end;

procedure TCustomGuiStaticWaveform.DrawMedian(YOffset: Integer);
begin
 with FBuffer.Canvas do
  begin
   Pen.Width := FMedianLineWidth;
   Pen.Color := FMedianColor;

   MoveTo(0, YOffset);
   LineTo(width, YOffset);
  end;
end;

procedure TCustomGuiStaticWaveform.DrawSamples(var OldMaxPos, OldMinPos: TPoint; NewMax, NewMin: TPoint);
var
  LastCenter: Integer;
begin
 with FBuffer.Canvas do
  begin
   case FWaveDrawMode of
    wdmPoints:
     begin
      Pixels[NewMax.X, NewMax.Y] := Pen.Color;
      Pixels[NewMin.X, NewMin.Y] := Pen.Color;
     end;

     wdmOutline:
      begin
       if OldMaxPos.Y = OldMinPos.Y then
        begin
         MoveTo(NewMin.X, NewMin.Y);
         LineTo(OldMinPos.X, OldMinPos.Y);
         if NewMax.Y <> NewMin.Y then LineTo(NewMax.X, NewMax.Y);
        end else
       if NewMax.Y=NewMin.Y then
        begin
         MoveTo(OldMinPos.X, OldMinPos.Y);
         LineTo(NewMax.X, NewMax.Y);
         LineTo(OldMaxPos.X, OldMaxPos.Y);
        end
       else
        begin
         MoveTo(NewMin.X, NewMin.Y);
         LineTo(OldMinPos.X, OldMinPos.Y);
         MoveTo(NewMax.X, NewMax.Y);
         LineTo(OldMaxPos.X, OldMaxPos.Y);
        end;
      end;
     wdmSimple:
      begin
       LineTo(OldMaxPos.X, NewMin.Y);
       LineTo(NewMax.X, NewMax.Y);
      end;
     else
      begin
       LastCenter := (OldMaxPos.Y + OldMinPos.Y) div 2;
       if abs(NewMax.Y - LastCenter) < abs(NewMin.Y - LastCenter) then
        begin
         LineTo(NewMax.X, NewMax.Y);
         if NewMin.Y <> NewMax.Y then LineTo(NewMin.X, NewMin.Y);
        end
       else
        begin
         LineTo(NewMin.X, NewMin.Y);
         if NewMin.Y <> NewMax.Y then LineTo(NewMax.X, NewMax.Y);
        end;
      end
   end
  end;

 OldMaxPos := NewMax;
 OldMinPos := NewMin;
end;

procedure TCustomGuiStaticWaveform.DrawSingleWave(YOffset, HalfHeight, Channel: Integer);
var
  SampleWidth, COffset  : Single;
  MinSample, MaxSample  : Single;
  OldMaxPos             : TPoint;
  OldMinPos             : TPoint;
  COffsetRounded, i     : Integer;
begin
 with FBuffer.Canvas do
  begin
   Pen.Width := fLineWidth;
   Pen.Color := fLineColor;

   SampleWidth := (width - 1) / (WaveLength - 1);

   MinSample := FWaveData[Channel][0];
   MaxSample := MinSample;

   COffset := 0;
   COffsetRounded := 1;
   i := 1;
   while i<Length(FWaveData[Channel]) do
    begin
     COffset := COffset+SampleWidth;
     if (COffset > COffsetRounded) or (i = Length(FWaveData[Channel]) - 1) then
      begin
       if COffsetRounded = 1 then
        begin
         OldMaxPos := Point(0, round(YOffset - MaxSample*FNormalizationFactors[Channel]*HalfHeight));
         OldMinPos := Point(0, round(YOffset - MinSample*FNormalizationFactors[Channel]*HalfHeight));
         MoveTo((OldMinPos.X+OldMaxPos.X) div 2, (OldMinPos.Y+OldMaxPos.Y) div 2);
        end;

       COffsetRounded := ceil(COffset);
       DrawSamples(
         OldMaxPos,
         OldMinPos,
         Point(COffsetRounded - 1, round(YOffset - MaxSample*FNormalizationFactors[Channel]*HalfHeight)),
         Point(COffsetRounded - 1, round(YOffset - MinSample*FNormalizationFactors[Channel]*HalfHeight)));

       MaxSample := FWaveData[Channel][i];
       MinSample := MaxSample;
      end
     else
      begin
       if FWaveData[Channel][i] > MaxSample
        then MaxSample := FWaveData[Channel][i] else
       if FWaveData[Channel][i] < MinSample
        then MinSample := FWaveData[Channel][i];
      end;

     Inc(i);
   end;
 end;
end;

procedure TCustomGuiStaticWaveform.DrawGraphs(Bitmap: TBitmap);
var
  YOffset, i : Integer;
begin
 with Bitmap.Canvas do
  for i := 0 to FDisplayChannels - 1 do
   begin
    YOffset := (FWaveVPadding + FWaveHalfHeight) * (i * 2 + 1);

    if FNormalizationFactors[i] > 0
     then DrawSingleWave(YOffset, FWaveHalfHeight, i);

    if FMedianVisible then DrawMedian(YOffset);
   end;
end;

procedure TCustomGuiStaticWaveform.UpdateBuffer;
begin
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    try
     FBuffer.Canvas.Brush.Color := Self.Color;

     {$IFNDEF FPC}
     if FTransparent
      then CopyParentImage(Self, FBuffer.Canvas)
      else {$ENDIF} FillRect(ClipRect);

     RenderToBitmap(FBuffer);
    finally
     UnLock;
    end;
   end;
end;

procedure TCustomGuiStaticWaveform.RenderToBitmap(Bitmap: TBitmap);
var
  i      : Integer;
  MaxAmp : Single;
  Amp    : Single;
begin
 with Bitmap.Canvas do
  begin
   Lock;
   try
    MaxAmp := 0;
    if FDisplayChannels < 1 then Exit;
    for i := 0 to FDisplayChannels - 1 do
      if i >= Length(FWaveData)
       then FNormalizationFactors[i] := 0 else
      if Length(FWaveData[i]) < 1 then FNormalizationFactors[i] := 0 else
       begin
        Amp := GetMaxAmp(i);
        MaxAmp := Max(MaxAmp, Amp);
        if Amp = 0
         then FNormalizationFactors[i] := 0
         else FNormalizationFactors[i] := 1 / Amp;
      end;
    
    if FNormalizationType = ntNone then
     begin
      for i := 0 to FDisplayChannels - 1 do
       if FNormalizationFactors[i] > 0
        then FNormalizationFactors[i] := 1;
     end
    else if (FNormalizationType = ntOverallChannels) and (MaxAmp > 0) then
     begin
      for i := 0 to FDisplayChannels - 1 do
       if FNormalizationFactors[i] > 0
        then FNormalizationFactors[i] := 1 / MaxAmp;
     end;

    DrawGraphs(Bitmap);
   finally
    UnLock;
   end;
  end;
end;

end.
