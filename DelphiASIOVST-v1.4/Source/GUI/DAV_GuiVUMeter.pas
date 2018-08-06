unit DAV_GuiVUMeter;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, Graphics, Forms,
  Messages, SysUtils, Controls, DAV_GuiBaseControl;

type
  TCustomGuiVUMeter = class(TBufferedGraphicControl)
  private
    FAutoSize      : Boolean;
    FVUMeterBitmap : TBitmap;
    FGlyphCount    : Integer;
    FLastGlyph     : Integer;
    FGlyphIndex    : Integer;
    FStitchKind    : TGuiStitchKind;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetVUMeterBitmap(const Value: TBitmap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetGlyphIndex(Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
  protected
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure UpdateBuffer; override;
    procedure AutoSizeChanged; virtual;
    procedure GlyphCountChanged; virtual;
    procedure GlyphIndexChanged; virtual;
    procedure StitchKindChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property VUMeterBitmap: TBitmap read FVUMeterBitmap write SetVUMeterBitmap;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
  end;

  TGuiVUMeter = class(TCustomGuiVUMeter)
  published
    property AutoSize;
    property Color;
    property GlyphCount;
    property PopupMenu;
    property GlyphIndex;
    property StitchKind;
    property VUMeterBitmap;
  end;

implementation

//uses Consts;

{ TCustomGuiVUMeter }

constructor TCustomGuiVUMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphIndex             := 0;
  FGlyphCount             := 1;
  FLastGlyph              := -1;
  FStitchKind             := skHorizontal;
  FVUMeterBitmap          := TBitmap.Create;
  FVUMeterBitmap.OnChange := SettingsChanged;
end;

destructor TCustomGuiVUMeter.Destroy;
begin
  FreeAndNil(FVUMeterBitmap);
  inherited;
end;

procedure TCustomGuiVUMeter.DoAutoSize;
begin
 if FVUMeterBitmap.Empty or (FGlyphCount = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FVUMeterBitmap.Width;
   Height := FVUMeterBitmap.Height div FGlyphCount;
  end
 else
  begin
   Width  := FVUMeterBitmap.Width div FGlyphCount;
   Height := FVUMeterBitmap.Height;
  end;
end;

procedure TCustomGuiVUMeter.UpdateBuffer;
var
  theRect : TRect;
  GlyphNr : Integer;
begin
 if (Width <= 0) and (Height <= 0) then
  with FBuffer.Canvas do
   begin
    Brush.Color := Self.Color;
    FillRect(ClipRect);
   end
 else
  with FBuffer.Canvas do
   begin
    GlyphNr := FGlyphIndex;
    if (GlyphNr >= FGlyphCount) then GlyphNr := FGlyphCount - 1 else
    if (GlyphNr < 0) then GlyphNr := 0;
    if GlyphNr = FLastGlyph then Exit;
    theRect := ClientRect;

    if FStitchKind = skVertical then
     begin
      theRect.Top    := FVUMeterBitmap.Height * GlyphNr div FGlyphCount;
      theRect.Bottom := FVUMeterBitmap.Height * (GlyphNr + 1) div FGlyphCount;
     end
    else
     begin
      theRect.Left  := FVUMeterBitmap.Width * GlyphNr div FGlyphCount;
      theRect.Right := FVUMeterBitmap.Width * (GlyphNr + 1) div FGlyphCount;
     end;

    Lock;
    CopyRect(Clientrect, FVUMeterBitmap.Canvas, theRect);
    Unlock;
    FLastGlyph := GlyphNr;
   end;
end;

procedure TCustomGuiVUMeter.SetAutoSize(const Value: Boolean);
begin
 if FAutoSize <> Value then
  begin
   FAutoSize := Value;
   AutoSizeChanged;
  end;
end;

procedure TCustomGuiVUMeter.AutoSizeChanged;
begin
 if Autosize then DoAutoSize;
end;

procedure TCustomGuiVUMeter.SetVUMeterBitmap(const Value: TBitmap);
begin
 FVUMeterBitmap.Assign(Value);
 DoAutoSize;
end;

procedure TCustomGuiVUMeter.SetGlyphCount(const Value: Integer);
begin
 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TCustomGuiVUMeter.GlyphCountChanged;
begin
 FLastGlyph := -1;
 DoAutoSize;
end;

procedure TCustomGuiVUMeter.SetGlyphIndex(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value > FGlyphCount then Value := FGlyphCount;

 if FGlyphIndex <> Value then
  begin
   FGlyphIndex := Value;
   GlyphIndexChanged;
  end;
end;

procedure TCustomGuiVUMeter.GlyphIndexChanged;
begin
 Invalidate;
end;

procedure TCustomGuiVUMeter.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;

procedure TCustomGuiVUMeter.StitchKindChanged;
begin
 FLastGlyph := -1;
 DoAutoSize;
end;

procedure TCustomGuiVUMeter.SettingsChanged(Sender: TObject);
begin
 FVUMeterBitmap.Canvas.Brush.Color := Self.Color;
 Invalidate;
 FLastGlyph := -1;
end;

end.
