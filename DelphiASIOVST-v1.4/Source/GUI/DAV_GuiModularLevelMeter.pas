unit DAV_GuiModularLevelMeter;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Controls, Graphics, DAV_Common, DAV_GuiBaseControl,
  DAV_ModularEnvelopeFollower;

type
  TGuiLevelDirection = (ldirHorizontal, ldirHorizontalInverted, ldmVertical, ldmVerticalInverted);
  TGuiShowClipping = (scNo, scTopLeft, scBottomRight);

  TCustomGuiLevelMeter = class(TGuiBaseControl)
  private
    FBarWidthPercentage : Single;
    FClippingBoxSize    : Integer;
    FClippingFillColor  : TColor;
    FClippingFillStyle  : TBrushStyle;
    FClippingLineColor  : TColor;
    FClippingLineStyle  : TPenStyle;
    FClippingLineWidth  : Integer;
    FFillColor          : TColor;
    FFillStyle          : TBrushStyle;
    FLastMaxPeaks       : TDAVSingleDynArray;
    FLastMinPeaks       : TDAVSingleDynArray;
    FLevelDirection     : TGuiLevelDirection;
    FLineStyle          : TPenStyle;
    FMaximumTimeFactor  : Single;
    FMaxLineColor       : TColor;
    FMaxLineStyle       : TPenStyle;
    FMaxLineWidth       : Integer;
    FMaxPeakEnvFollower : TDspEnvelopeFollower;
    FPeakEnvFollower    : TDspEnvelopeFollower;
    FShowClipping       : TGuiShowClipping;
    FShowMaximum        : Boolean;

    function GetLevelAttack: Single;
    function GetLevelRelease: Single;
    function GetDisplayChannels: Integer;

    procedure SetBarWidthPercentage(const Value: Single);
    procedure SetClippingBoxSize(const Value: Integer);
    procedure SetClippingFillColor(const Value: TColor);
    procedure SetClippingFillStyle(const Value: TBrushStyle);
    procedure SetClippingLineColor(const Value: TColor);
    procedure SetClippingLineStyle(const Value: TPenStyle);
    procedure SetClippingLineWidth(const Value: Integer);
    procedure SetDisplayChannels(const Value: Integer);
    procedure SetFillColor(const Value: TColor);
    procedure SetFillStyle(const Value: TBrushStyle);
    procedure SetLevelAttack(const Value: Single);
    procedure SetLevelDirection(const Value: TGuiLevelDirection);
    procedure SetLevelRelease(const Value: Single);
    procedure SetLineStyle(const Value: TPenStyle);
    procedure SetMaximumTimeFactor(const Value: Single);
    procedure SetMaxLineColor(const Value: TColor);
    procedure SetMaxLineStyle(const Value: TPenStyle);
    procedure SetMaxLineWidth(const Value: Integer);
    procedure SetShowClipping(const Value: TGuiShowClipping);
    procedure SetShowMaximum(const Value: Boolean);
  protected
    procedure DrawGauge(GaugeRect: TRect); virtual;
    procedure DrawMaxLine(x1, y1, x2, y2: Integer); virtual;
    procedure DrawClipIndicator(ClipIndRect: TRect); virtual;

    procedure DrawSingleBarH (BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure DrawSingleBarHI(BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure DrawSingleBarV (BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure DrawSingleBarVI(BarRect: TRect; Peak, MaxPeak: Single); virtual;
    procedure SetRedrawInterval(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure RedrawBuffer(doBufferFlip: Boolean); override;
    procedure ResetPeaks;

    procedure ProcessBufferIndirect(NewWaveData: TDAVArrayOfSingleDynArray; Channels, SampleFrames: Integer);
    procedure ProcessBuffer(NewWaveData: TDAVSingleDynArray; InpLen: Integer = -1); overload;
    procedure ProcessBuffer(NewWaveData: TDAVArrayOfSingleDynArray; InpLen: Integer = -1); overload;

    property FillColor: TColor read FFillColor write SetFillColor default clGreen;
    property FillStyle: TBrushStyle read FFillStyle write SetFillStyle default bsSolid;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psSolid;

    property MaxLineColor: TColor read FMaxLineColor write SetMaxLineColor default clBlue;
    property MaxLineStyle: TPenStyle read FMaxLineStyle write SetMaxLineStyle default psSolid;
    property MaxLineWidth: Integer read FMaxLineWidth write SetMaxLineWidth default 1;

    property ClippingLineColor: TColor read FClippingLineColor write SetClippingLineColor default clBlack;
    property ClippingLineStyle: TPenStyle read FClippingLineStyle write SetClippingLineStyle default psSolid;
    property ClippingLineWidth: Integer read FClippingLineWidth write SetClippingLineWidth default 1;
    property ClippingFillColor: TColor read FClippingFillColor write SetClippingFillColor default clRed;
    property ClippingFillStyle: TBrushStyle read FClippingFillStyle write SetClippingFillStyle default bsSolid;
    property ClippingBoxSize: Integer read FClippingBoxSize write SetClippingBoxSize default 5;

    property ShowMaximum: Boolean read FShowMaximum write SetShowMaximum default True;
    property ShowClipping: TGuiShowClipping read FShowClipping write SetShowClipping default scTopLeft;

    property LevelAttack: Single read GetLevelAttack write SetLevelAttack;
    property LevelRelease: Single read GetLevelRelease write SetLevelRelease;

    property LevelDirection: TGuiLevelDirection read FLevelDirection write SetLevelDirection default ldmVertical;
    property DisplayChannels: Integer read GetDisplayChannels write SetDisplayChannels default 2;
    property BarWidthPercentage: Single read FBarWidthPercentage write SetBarWidthPercentage;
    property MaximumTimeFactor: Single read FMaximumTimeFactor write SetMaximumTimeFactor;
  end;

  TGuiLevelMeter = class(TCustomGuiLevelMeter)
  published
    property BarWidthPercentage;
    property ClippingBoxSize;
    property ClippingFillColor;
    property ClippingFillStyle;
    property ClippingLineColor;
    property ClippingLineStyle;
    property ClippingLineWidth;
    property Color;
    property DisplayChannels;
    property FillColor;
    property FillStyle;
    property LevelAttack;
    property LevelDirection;
    property LevelRelease;
    property LineColor;
    property LineStyle;
    property LineWidth;
    property MaximumTimeFactor;
    property MaxLineColor;
    property MaxLineStyle;
    property MaxLineWidth;
    property RedrawInterval;
    property ShowClipping;
    property ShowMaximum;
    property Transparent;
  end;

implementation

uses
  SysUtils, Math,
    {$IFDEF PUREPASCAL}DAV_BufferMathPascal{$ELSE}DAV_BufferMathAsm,
  DateUtils{$ENDIF};

{ TCustomGuiLevelMeter }

constructor TCustomGuiLevelMeter.Create(AOwner: TComponent);
begin    
  inherited;

  FPeakEnvFollower    := TDspEnvelopeFollower.Create(self);
  FMaxPeakEnvFollower := TDspEnvelopeFollower.Create(self);

  FFillColor         := clGreen;
  FFillStyle         := bsSolid;
  FLineStyle         := psSolid;
  FMaxLineColor      := clBlue;
  FMaxLineStyle      := psSolid;
  FMaxLineWidth      := 1;

  FClippingLineColor := clBlack;
  FClippingLineStyle := psSolid;
  FClippingLineWidth := 1;
  FClippingFillColor := clRed;
  FClippingFillStyle := bsSolid;
  FClippingBoxSize   := 5;

  FShowMaximum       := True;
  FShowClipping      := scTopLeft;

  FLevelDirection    := ldmVertical;
  FPeakEnvFollower.Channels := 2;
  FMaxPeakEnvFollower.Channels := 2;

  FBarWidthPercentage :=  0.8;

  ResetPeaks;

  FMaximumTimeFactor := 3;
  RedrawInterval := 30;
  LevelAttack    := 0;
  LevelRelease   := 0;
  FMaxPeakEnvFollower.Attack := 0;
  FMaxPeakEnvFollower.Release := 1;
end;

destructor TCustomGuiLevelMeter.Destroy;
begin
  FreeAndNil(FPeakEnvFollower);
  FreeAndNil(FMaxPeakEnvFollower);
  SetLength(FLastMaxPeaks, 0);
  SetLength(FLastMinPeaks, 0);
  inherited;
end;

procedure TCustomGuiLevelMeter.ResetPeaks;
begin
  SetLength(FLastMaxPeaks, FPeakEnvFollower.Channels);
  SetLength(FLastMinPeaks, FPeakEnvFollower.Channels);
  FillChar(FLastMaxPeaks[0], SizeOf(Single) * FPeakEnvFollower.Channels, 0);
  FillChar(FLastMinPeaks[0], SizeOf(Single) * FPeakEnvFollower.Channels, 0);
  RedrawBuffer(True);
end;


procedure TCustomGuiLevelMeter.SetLevelAttack(const Value: Single);
begin
  FPeakEnvFollower.Attack := Value;
end;

procedure TCustomGuiLevelMeter.SetLevelRelease(const Value: Single);
begin
  FPeakEnvFollower.Release := Value;
end;

function TCustomGuiLevelMeter.GetLevelAttack: Single;
begin
  Result := FPeakEnvFollower.Attack
end;

function TCustomGuiLevelMeter.GetLevelRelease: Single;
begin
  Result := FPeakEnvFollower.Release
end;

procedure TCustomGuiLevelMeter.DrawClipIndicator(ClipIndRect: TRect);
begin
  with fBuffer.Canvas do
   begin
    Pen.Color := FClippingLineColor;
    Pen.Width := FClippingLineWidth;
    Pen.Style := FClippingLineStyle;
    Brush.Color := FClippingFillColor;
    Brush.Style := FClippingFillStyle;
    Rectangle(ClipIndRect);
   end;
end;

procedure TCustomGuiLevelMeter.DrawGauge(GaugeRect: TRect);
begin
  with fBuffer.Canvas do
   begin
    Pen.Color := fLineColor;
    Pen.Width := fLineWidth;
    Pen.Style := FLineStyle;
    Brush.Color := FFillColor;
    Brush.Style := FFillStyle;

    Rectangle(GaugeRect);
   end;
end;

procedure TCustomGuiLevelMeter.DrawMaxLine(x1, y1, x2, y2: Integer);
begin
  with fBuffer.Canvas do
  begin
    Pen.Color := FMaxLineColor;
    Pen.Width := FMaxLineWidth;
    Pen.Style := FMaxLineStyle;

    MoveTo(x1, y1);
    LineTo(x2, y2);
  end;
end;


procedure TCustomGuiLevelMeter.DrawSingleBarH(BarRect: TRect; Peak, MaxPeak: Single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left+FClippingBoxSize+1, BarRect.Top, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Left+FClippingBoxSize, BarRect.Bottom); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right-FClippingBoxSize-1, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Right-FClippingBoxSize, BarRect.Top, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Right := Round((tmpRect.Right-tmpRect.Left)*tmp + tmpRect.Left);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp := min(1, MaxPeak);
    GaugeRect.Right := Round((GaugeRect.Right-GaugeRect.Left)*tmp + GaugeRect.Left);
    DrawMaxLine(GaugeRect.Right, GaugeRect.Top, GaugeRect.Right, GaugeRect.Bottom);
  end;
  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.DrawSingleBarHI(BarRect: TRect; Peak, MaxPeak: Single);
var ClipIndRect, GaugeRect, tmpRect: TRect; tmp: Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left+FClippingBoxSize+1, BarRect.Top, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Left+FClippingBoxSize, BarRect.Bottom); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right-FClippingBoxSize-1, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Right-FClippingBoxSize, BarRect.Top, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Left := Round(tmpRect.Right-(tmpRect.Right-tmpRect.Left)*tmp);
  DrawGauge(tmpRect);

  if FShowMaximum then
  begin
    tmp := min(1, MaxPeak);
    GaugeRect.Left := Round(GaugeRect.Right-(GaugeRect.Right-GaugeRect.Left)*tmp);
    DrawMaxLine(GaugeRect.Left, GaugeRect.Top, GaugeRect.Left, GaugeRect.Bottom);
  end;

  if (FShowClipping<>scNo) and (MaxPeak>1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.DrawSingleBarV(BarRect: TRect; Peak, MaxPeak: Single);
var
  ClipIndRect,
  GaugeRect,
  tmpRect      : TRect;
  tmp          : Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left, BarRect.Top+FClippingBoxSize+1, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Top+FClippingBoxSize); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Bottom-FClippingBoxSize-1);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Bottom-FClippingBoxSize, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Top := Round(tmpRect.Bottom-(tmpRect.Bottom-tmpRect.Top)*tmp);
  DrawGauge(tmpRect);

  if FShowMaximum then
   begin
    tmp := min(1, MaxPeak);
    GaugeRect.Top := Round(GaugeRect.Bottom-(GaugeRect.Bottom-GaugeRect.Top)*tmp);
    DrawMaxLine(GaugeRect.Left, GaugeRect.Top, GaugeRect.Right, GaugeRect.Top);
   end;

  if (FShowClipping <> scNo) and
     (MaxPeak > 1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.DrawSingleBarVI(BarRect: TRect; Peak, MaxPeak: Single);
var
  ClipIndRect,
  GaugeRect,
  tmpRect      : TRect;
  tmp          : Single;
begin
  case FShowClipping of
    scNo:          begin GaugeRect   := BarRect;
                         ClipIndRect := Rect(0,0,0,0); end;
    scTopLeft:     begin GaugeRect   := Rect(BarRect.Left, BarRect.Top+FClippingBoxSize+1, BarRect.Right, BarRect.Bottom);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Top+FClippingBoxSize); end;
    scBottomRight: begin GaugeRect   := Rect(BarRect.Left, BarRect.Top, BarRect.Right, BarRect.Bottom-FClippingBoxSize-1);
                         ClipIndRect := Rect(BarRect.Left, BarRect.Bottom-FClippingBoxSize, BarRect.Right, BarRect.Bottom); end;
  end;

  tmp := min(1, Peak);
  tmpRect := GaugeRect;
  tmpRect.Bottom := Round((tmpRect.Bottom-tmpRect.Top)*tmp + tmpRect.Top);
  DrawGauge(tmpRect);

  if FShowMaximum then
   begin
    tmp := min(1, MaxPeak);
    GaugeRect.Bottom := Round((GaugeRect.Bottom - GaugeRect.Top) * tmp + GaugeRect.Top);
    DrawMaxLine(GaugeRect.Left, GaugeRect.Bottom, GaugeRect.Right, GaugeRect.Bottom);
   end;

  if (FShowClipping <> scNo) and
     (MaxPeak > 1) then DrawClipIndicator(ClipIndRect);
end;

procedure TCustomGuiLevelMeter.RedrawBuffer(doBufferFlip: Boolean);
var
  CurrentPeak, CurrentMax: Single;
  i: Integer;
  DestBarRect: TRect;
  SplitSize: Single;
  BarPadding: Single;
begin
  if (Width > 0) and (Height > 0) then
  with fBuffer.Canvas do
   begin
    Lock;
    Brush.Color := Self.Color;

    {$IFNDEF FPC}if fTransparent then DrawParentImage(fBuffer.Canvas) else{$ENDIF}
      FillRect(fBuffer.Canvas.ClipRect);

    if FLevelDirection in [ldirHorizontal, ldirHorizontalInverted]
     then SplitSize   := Height / FPeakEnvFollower.Channels
     else SplitSize   := Width / FPeakEnvFollower.Channels;

    BarPadding := (1 - FBarWidthPercentage) * SplitSize * 0.5;

    for i := 0 to FPeakEnvFollower.Channels - 1 do
     begin
      CurrentPeak := (FLastMaxPeaks[i]-FLastMinPeaks[i]) * 0.5;
      FLastMaxPeaks[i] := 0;
      FLastMinPeaks[i] := 0;

      FPeakEnvFollower.ProcessS(CurrentPeak, i);
      CurrentMax := CurrentPeak;
      FMaxPeakEnvFollower.ProcessS(CurrentMax, i);

      if FLevelDirection in [ldirHorizontal, ldirHorizontalInverted]
       then DestBarRect := Rect(0, Round(splitsize * i + BarPadding), width, Round(splitsize * (i + 1) - BarPadding))
       else DestBarRect := Rect(Round(splitsize * i + BarPadding), 0, Round(splitsize * (i + 1) - BarPadding), height);

      case FLevelDirection of
        ldirHorizontal:         DrawSingleBarH (DestBarRect, CurrentPeak , CurrentMax);
        ldirHorizontalInverted: DrawSingleBarHI(DestBarRect, CurrentPeak , CurrentMax);
        ldmVertical:            DrawSingleBarV (DestBarRect, CurrentPeak , CurrentMax);
        ldmVerticalInverted:    DrawSingleBarVI(DestBarRect, CurrentPeak , CurrentMax);
      end;
     end;

    UnLock;
   end;

  if doBufferFlip then Invalidate;
end;

procedure TCustomGuiLevelMeter.SetBarWidthPercentage(const Value: Single);
begin
  if FBarWidthPercentage <> Value then
   begin
    FBarWidthPercentage := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingFillColor(const Value: TColor);
begin
  if FClippingFillColor <> Value then
   begin
    FClippingFillColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingFillStyle(const Value: TBrushStyle);
begin
  if FClippingFillStyle <> Value then
   begin
    FClippingFillStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingLineColor(const Value: TColor);
begin
  if FClippingLineColor <> Value then
   begin
    FClippingLineColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingLineStyle(const Value: TPenStyle);
begin
  if FClippingLineStyle <> Value then
   begin
    FClippingLineStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingLineWidth(const Value: Integer);
begin
  if FClippingLineWidth <> Value then
   begin
    FClippingLineWidth := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetClippingBoxSize(const Value: Integer);
begin
  if FClippingBoxSize <> Value then
   begin
    FClippingBoxSize := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetDisplayChannels(const Value: Integer);
begin
  if FPeakEnvFollower.Channels <> Value then
   begin
    FPeakEnvFollower.Channels := Value;
    FMaxPeakEnvFollower.Channels := Value;
    ResetPeaks;
   end;
end;

function TCustomGuiLevelMeter.GetDisplayChannels: Integer;
begin
  Result := FPeakEnvFollower.Channels;
end;

procedure TCustomGuiLevelMeter.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
   begin
    FFillColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetFillStyle(const Value: TBrushStyle);
begin
  if FFillStyle <> Value then
   begin
    FFillStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetLevelDirection(const Value: TGuiLevelDirection);
begin
  if FLevelDirection <> Value then
   begin
    FLevelDirection := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetLineStyle(const Value: TPenStyle);
begin
  if FLineStyle <> Value then
   begin
    FLineStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaxLineColor(const Value: TColor);
begin
  if FMaxLineColor <> Value then
   begin
    FMaxLineColor := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaxLineStyle(const Value: TPenStyle);
begin
  if FMaxLineStyle <> Value then
   begin
    FMaxLineStyle := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetMaxLineWidth(const Value: Integer);
begin
  if FMaxLineWidth <> Value then
   begin
    FMaxLineWidth := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetShowClipping(const Value: TGuiShowClipping);
begin
  if FShowClipping <> Value then
   begin
    FShowClipping := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetShowMaximum(const Value: Boolean);
begin
  if FShowMaximum <> Value then
   begin
    FShowMaximum := Value;
    RedrawBuffer(True);
   end;
end;

procedure TCustomGuiLevelMeter.SetRedrawInterval(Value: Integer);
begin
  if Value < 1
   then raise Exception.Create('RedrawInterval must greater than 0');

  FPeakEnvFollower.SampleRate := 1000 / Value;
  FMaxPeakEnvFollower.SampleRate := FMaximumTimeFactor * FPeakEnvFollower.SampleRate;

  inherited;
end;

procedure TCustomGuiLevelMeter.SetMaximumTimeFactor(const Value: Single);
begin
  if FMaximumTimeFactor <> value then
   begin
     FMaximumTimeFactor := value;
     FMaxPeakEnvFollower.SampleRate := FMaximumTimeFactor * FPeakEnvFollower.SampleRate;
   end;
end;

procedure TCustomGuiLevelMeter.ProcessBufferIndirect(NewWaveData: TDAVArrayOfSingleDynArray; Channels, SampleFrames: Integer);
var
  tmp : TDAVArrayOfSingleDynArray;
  i   : Integer;
begin
  SetLength(tmp, Channels, SampleFrames);
  for i := 0 to Channels - 1 do
    move(NewWaveData[i,0], tmp[i,0], SampleFrames * SizeOf(Single));

  ProcessBuffer(tmp, SampleFrames);
end;

procedure TCustomGuiLevelMeter.ProcessBuffer(NewWaveData: TDAVSingleDynArray; InpLen: Integer);
var
  tmp: TDAVArrayOfSingleDynArray;
begin
  SetLength(tmp, 1);
  tmp[0] := NewWaveData;
  ProcessBuffer(tmp, InpLen);
end;

procedure TCustomGuiLevelMeter.ProcessBuffer(NewWaveData: TDAVArrayOfSingleDynArray; InpLen: Integer);
var
  minPeak  : TDAVSingleDynArray;
  maxPeak  : TDAVSingleDynArray;
  i        : Integer;
begin
  SetLength(minPeak, FPeakEnvFollower.Channels);
  SetLength(maxPeak, FPeakEnvFollower.Channels);

  GetPeaks(NewWaveData, minPeak, maxPeak, FPeakEnvFollower.Channels, InpLen);
  for i := 0 to FPeakEnvFollower.Channels - 1 do
   begin
    if maxPeak[i] > FLastMaxPeaks[i] then FLastMaxPeaks[i] := maxPeak[i]
    else if minPeak[i] < FLastMinPeaks[i] then FLastMinPeaks[i] := minPeak[i];
   end;
  fTimerMustRedraw := True;
end;

end.
