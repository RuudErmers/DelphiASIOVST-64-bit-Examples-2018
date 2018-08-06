unit GuiLayers;

interface

uses
  Classes, Graphics, SysUtils, Controls, GR32, GR32_Image, GR32_Layers;

type
  IVSTParameter = interface
  ['{EF231D56-9B6B-4BE0-92E9-77F74974B507}']
    function GetParameter: Integer;
    procedure SetParameter(const Value: Integer);

    property Parameter: Integer read GetParameter write SetParameter;
  end;

  TTextLayer = class(TBitmapLayer, IVSTParameter)
  private
    FText      : string;
    FAntialias : Integer;
    FFont      : TFont;
    FAlpha     : Byte;
    FParameter : Integer;
    function GetParameter: Integer;
    procedure SetParameter(const Value: Integer);
    procedure SetAntialias(const Value: Integer);
    procedure SetAlphaValue(const Value: Byte);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);
    procedure FontChangeHandler(Sender: TObject);
  protected
    procedure AlphaChanged; virtual;
    procedure AntialiasChanged; virtual;
    procedure CalculateBounds; virtual;
    procedure TextChanged; virtual;
    procedure ParameterChanged; virtual;
    procedure RenderTextToBitmap; virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
  published
    property Alpha: Byte read FAlpha write SetAlphaValue;
    property Antialias: Integer read FAntialias write SetAntialias;
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property Parameter: Integer read FParameter write SetParameter;
  end;

  TStitchDirection = (sdVertical, sdHorizontal);
  TDialLayer = class(TBitmapLayer, IVSTParameter)
  private
    FGlyphCount       : Integer;
    FStitchBitmap    : TBitmap32;
    FStitchDirection : TStitchDirection;
    FStitchIndex     : Integer;
    FMousePos        : TPoint;
    FParameter       : Integer;
    FOnChanged       : TNotifyEvent;
    function GetParameter: Integer;
    procedure StitchBitmapChangedHandler(Sender: TObject);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchBitmap(const Value: TBitmap32);
    procedure SetStitchDirection(const Value: TStitchDirection);
    procedure SetStitchIndex(Value: Integer);
    procedure SetParameter(const Value: Integer);
  protected
    procedure GlyphCountChanged; virtual;
    procedure StitchDirectionChanged; virtual;
    procedure StitchIndexChanged; virtual;
    procedure ParameterChanged; virtual;
    procedure RecalculateLocation; virtual;
    procedure RenderDialToBitmap; virtual;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure LoadFromGraphic(Graphic: TGraphic);
  published
    property StitchBitmap: TBitmap32 read FStitchBitmap write SetStitchBitmap;
    property StitchDirection: TStitchDirection read FStitchDirection write SetStitchDirection;
    property StitchIndex: Integer read FStitchIndex write SetStitchIndex;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property Parameter: Integer read GetParameter write SetParameter;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

uses
  GR32_Backends, GR32_Backends_Generic, GR32_Backends_VCL;

{ TTextLayer }

constructor TTextLayer.Create(ALayerCollection: TLayerCollection);
begin
 inherited;
 FAntialias := 2;
 FAlpha := 255;
 FFont := TFont.Create;
 with FFont do
  begin
   OnChange := FontChangeHandler;
   Size := 16;
  end;
 FParameter := -1; 
end;

destructor TTextLayer.Destroy;
begin
 FreeAndNil(FFont);
 inherited;
end;

procedure TTextLayer.SetAlphaValue(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TTextLayer.SetAntialias(const Value: Integer);
begin
 if FAntialias <> Value then
  begin
   FAntialias := Value;
   AntialiasChanged;
  end;
end;

procedure TTextLayer.SetFont(const Value: TFont);
begin
 FFont.Assign(Font);
end;

procedure TTextLayer.SetParameter(const Value: Integer);
begin
 if FParameter <> Value then
  begin
   FParameter := Value;
   ParameterChanged;
  end;
end;

procedure TTextLayer.SetText(const Value: string);
begin
 if FText <> Value then
  begin
   FText := Value;
   TextChanged;
  end;
end;

procedure TTextLayer.FontChangeHandler(Sender: TObject);
begin
 CalculateBounds;
 RenderTextToBitmap;
 Changed;
end;

function TTextLayer.GetParameter: Integer;
begin
 Result := FParameter;
end;

procedure TTextLayer.ParameterChanged;
begin
  // yet undefined
end;

procedure TTextLayer.AlphaChanged;
begin
 RenderTextToBitmap;
 Changed;
end;

procedure TTextLayer.AntialiasChanged;
begin
 RenderTextToBitmap;
 Changed;
end;

procedure TTextLayer.TextChanged;
begin
 CalculateBounds;
 RenderTextToBitmap;
 Changed;
end;

procedure TTextLayer.CalculateBounds;
begin
 with Bitmap do
  begin
   Bitmap.Font.Assign(FFont);
   Location := FloatRect(Location.Left, Location.Top, Location.Left +
     TextWidth(Text), Location.Top + TextHeight(Text));
   Width := TextWidth(FText);
   Height := TextHeight(FText);
  end;
end;

procedure TTextLayer.RenderTextToBitmap;
begin
 with Bitmap do
  begin
   Clear(0);
   Bitmap.Font.Assign(FFont);
   RenderText(0, 0, FText, FAntialias, SetAlpha(Color32(FFont.Color), FAlpha));
  end;
end;


{ TDialLayer }

constructor TDialLayer.Create(ALayerCollection: TLayerCollection);
begin
 inherited;
 FStitchBitmap := TBitmap32.Create;
 with FStitchBitmap do
  begin
   OnChange := StitchBitmapChangedHandler;
   Backend := TMemoryBackend.Create;
  end;

 FStitchDirection := sdHorizontal;
 FGlyphCount := 1;
 FStitchIndex := 0;
end;

destructor TDialLayer.Destroy;
begin
 FreeAndNil(FStitchBitmap);
 inherited;
end;

function TDialLayer.GetParameter: Integer;
begin
 Result := FParameter;
end;

procedure TDialLayer.LoadFromGraphic(Graphic: TGraphic);
var
  i : Integer;
begin
 FStitchBitmap.Assign(Graphic);

 if FStitchBitmap.Width > FStitchBitmap.Height then
  begin
   FStitchDirection := sdHorizontal;
   FGlyphCount := Round(FStitchBitmap.Width / FStitchBitmap.Height);

   if FStitchBitmap.Width <> FStitchBitmap.Height * FGlyphCount then
    begin
     for i := 0 to FGlyphCount div 2 do
      begin
       if (FStitchBitmap.Width div (FGlyphCount + i)) = (FStitchBitmap.Width / (FGlyphCount + i)) then
        begin
         FGlyphCount := FGlyphCount + i;
         Break;
        end;
       if (FStitchBitmap.Width div (FGlyphCount - i)) = (FStitchBitmap.Width / (FGlyphCount - i)) then
        begin
         FGlyphCount := FGlyphCount - i;
         Break;
        end;
      end;
    end;

   Bitmap.Width := FStitchBitmap.Width div FGlyphCount;
   Bitmap.Height := FStitchBitmap.Height;
  end
 else
  begin
   FStitchDirection := sdVertical;
   FGlyphCount := Round(FStitchBitmap.Height / FStitchBitmap.Width);

   if FStitchBitmap.Height <> FStitchBitmap.Width * FGlyphCount then
    begin
     for i := 0 to FGlyphCount div 2 do
      begin
       if (FStitchBitmap.Height div (FGlyphCount + i)) = (FStitchBitmap.Height / (FGlyphCount + i)) then
        begin
         FGlyphCount := FGlyphCount + i;
         Break;
        end;
       if (FStitchBitmap.Height div (FGlyphCount - i)) = (FStitchBitmap.Height / (FGlyphCount - i)) then
        begin
         FGlyphCount := FGlyphCount - i;
         Break;
        end;
      end;
    end;

   Bitmap.Width := FStitchBitmap.Width;
   Bitmap.Height := FStitchBitmap.Height div FGlyphCount;
  end;

 if FStitchIndex >= FGlyphCount
  then FStitchIndex := FGlyphCount - 1;

 RecalculateLocation;
 RenderDialToBitmap;
end;

procedure TDialLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 inherited;
 FMousePos.X := X;
 FMousePos.Y := Y;
end;

procedure TDialLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 if ssLeft in Shift then
  begin
   StitchIndex := FStitchIndex + FMousePos.Y - Y;
   if Assigned(FOnChanged)
    then FOnChanged(Self);
  end;

 FMousePos.X := X;
 FMousePos.Y := Y;
end;

procedure TDialLayer.GlyphCountChanged;
begin
 RecalculateLocation;
 RenderDialToBitmap;
end;

procedure TDialLayer.StitchBitmapChangedHandler(Sender: TObject);
begin
 RecalculateLocation;
 RenderDialToBitmap;
end;

procedure TDialLayer.StitchDirectionChanged;
begin
 RecalculateLocation;
 RenderDialToBitmap;
end;

procedure TDialLayer.StitchIndexChanged;
begin
 RenderDialToBitmap;
end;

procedure TDialLayer.RecalculateLocation;
begin
 with Location, FStitchBitmap do
  if FStitchDirection = sdHorizontal
   then Location := FloatRect(Left, Top, Left + Width / FGlyphCount, Top + Height)
   else Location := FloatRect(Left, Top, Left + Width, Top + Height / FGlyphCount);
 Bitmap.Width := Round(Location.Right - Location.Left);
 Bitmap.Height := Round(Location.Bottom - Location.Top);
end;

procedure TDialLayer.RenderDialToBitmap;
begin
 Bitmap.Clear(0);
 with FStitchBitmap do
  if (Width > 0) and (Height > 0) then
   begin
    if FStitchDirection = sdHorizontal
     then Bitmap.Draw(0, 0, Rect((FStitchIndex * Width) div FGlyphCount, 0,
       ((FStitchIndex + 1) * Width) div FGlyphCount, Height), FStitchBitmap)
     else Bitmap.Draw(0, 0, Rect(0, (FStitchIndex * Height) div FGlyphCount,
       Width, ((FStitchIndex + 1) * Height) div FGlyphCount), FStitchBitmap);
   end;
end;

procedure TDialLayer.ParameterChanged;
begin

end;

procedure TDialLayer.SetGlyphCount(const Value: Integer);
begin
 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TDialLayer.SetParameter(const Value: Integer);
begin
 if FParameter <> Value then
  begin
   FParameter := Value;
   ParameterChanged;
  end;
end;

procedure TDialLayer.SetStitchBitmap(const Value: TBitmap32);
begin
 FStitchBitmap.Assign(Value);
end;

procedure TDialLayer.SetStitchDirection(const Value: TStitchDirection);
begin
 if FStitchDirection <> Value then
  begin
   FStitchDirection := Value;
   StitchDirectionChanged;
  end;
end;

procedure TDialLayer.SetStitchIndex(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FGlyphCount then Value := FGlyphCount - 1;

 if FStitchIndex <> Value then
  begin
   FStitchIndex := Value;
   StitchIndexChanged;
  end;
end;

end.
