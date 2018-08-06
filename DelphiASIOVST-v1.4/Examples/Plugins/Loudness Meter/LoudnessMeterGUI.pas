unit LoudnessMeterGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, StdCtrls,
  Graphics, DAV_Types, DAV_VSTModule, DAV_GuiPixelMap, DAV_GuiGraphicControl,
  DAV_GuiLabel, DAV_GuiPanel, DAV_GuiGroup, Menus;

type
  TFmLoudnessMeter = class(TForm)
    GbLoudness: TGuiGroupSimple;
    GbMomentaryPeak: TGuiGroupSimple;
    GbUnit: TGuiGroupSimple;
    GbIntegrationTime: TGuiGroupSimple;
    GbTimeDisplay: TGuiGroupSimple;
    LbLoudness: TGuiLabel;
    LbUnit: TGuiLabel;
    LbIntegrationTime: TGuiLabel;
    LbMomentaryPeak: TGuiLabel;
    LbTimeDisplay: TGuiLabel;
    TrScreenUpdate: TTimer;
    PmIntegrationTime: TPopupMenu;
    MiIntegrationShort: TMenuItem;
    MiIntegrationMomentary: TMenuItem;
    MiIntegrationLongTerm: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TrScreenUpdateTimer(Sender: TObject);
    procedure LbUnitClick(Sender: TObject);
    procedure LbIntegrationTimeClick(Sender: TObject);
    procedure LbMomentaryPeakClick(Sender: TObject);
    procedure LbTimeDisplayClick(Sender: TObject);
    procedure MiIntegrationShortClick(Sender: TObject);
    procedure MiIntegrationMomentaryClick(Sender: TObject);
    procedure MiIntegrationLongTermClick(Sender: TObject);
  private
    FTotalInterval    : Integer;
    FBackgroundBitmap : TGuiCustomPixelMap;
  public
    procedure UpdateLoudness;
    procedure UpdatePeak;
    procedure UpdateScale;
    procedure UpdateState;
    procedure UpdateTime;
    procedure UpdateTotalSamples;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_GuiCommon, LoudnessMeterDSP;

{ TFmLoudnessMeter }

procedure TFmLoudnessMeter.FormCreate(Sender: TObject);
begin
 FBackgroundBitmap := TGuiPixelMapMemory.Create;
end;

procedure TFmLoudnessMeter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgroundBitmap);
end;

procedure TFmLoudnessMeter.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmLoudnessMeter.FormResize(Sender: TObject);
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
begin
 with FBackgroundBitmap do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       ScnLn[x].B := Round($70 - $34 * (s[1] - h));
       ScnLn[x].G := Round($84 - $48 * (s[1] - h));
       ScnLn[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmLoudnessMeter.FormShow(Sender: TObject);
begin
 FTotalInterval := 0;
 UpdateScale;
 UpdateTime;
 UpdateState;
 UpdatePeak;
 UpdateLoudness;
 UpdateTotalSamples;
end;

procedure TFmLoudnessMeter.LbIntegrationTimeClick(Sender: TObject);
begin
 with TLoudnessMeterModule(Owner) do
  case Round(Parameter[1]) of
   0 : Parameter[1] := 1;
   1 : Parameter[1] := 2;
   2 : Parameter[1] := 0;
  end;
end;

procedure TFmLoudnessMeter.LbMomentaryPeakClick(Sender: TObject);
begin
 TLoudnessMeterModule(Owner).ResetPeak;
end;

procedure TFmLoudnessMeter.LbTimeDisplayClick(Sender: TObject);
begin
 with TLoudnessMeterModule(Owner)
  do Parameter[2] := 1 - Parameter[2];
end;

procedure TFmLoudnessMeter.LbUnitClick(Sender: TObject);
begin
 with TLoudnessMeterModule(Owner)
  do Parameter[0] := 1 - Parameter[0];
end;

procedure TFmLoudnessMeter.MiIntegrationMomentaryClick(Sender: TObject);
begin
 TLoudnessMeterModule(Owner).Parameter[1] := 0;
end;

procedure TFmLoudnessMeter.MiIntegrationShortClick(Sender: TObject);
begin
 TLoudnessMeterModule(Owner).Parameter[1] := 1;
end;

procedure TFmLoudnessMeter.MiIntegrationLongTermClick(Sender: TObject);
begin
 TLoudnessMeterModule(Owner).Parameter[2] := 0;
end;

procedure TFmLoudnessMeter.TrScreenUpdateTimer(Sender: TObject);
begin
 UpdateTotalSamples;
end;

procedure TFmLoudnessMeter.UpdateLoudness;
var
  Value : Single;
  Str   : string;
begin
 with TLoudnessMeterModule(Owner) do
  begin
   Value := RoundTo(UnitOffset + Loudness, -1);
   Str := FloatToStrF(Value, ffGeneral, 4, 4);

   // correct FS scale
   if Round(Parameter[0]) = 1 then Value := Value + 23;

   if Value < -18.1 then
    begin
     Str := 'LOW';
     LbLoudness.Font.Color := $00DBE1E3;
    end else
   if Value < -1.1
    then LbLoudness.Font.Color := $000080FF else
   if Value < 1
    then LbLoudness.Font.Color := $0000AE00
    else
     begin
      LbLoudness.Font.Color := clRed;
      if Value > 9
       then Str := 'OVER';
     end;

   LbLoudness.Caption := Str;
  end;
end;

procedure TFmLoudnessMeter.UpdatePeak;
var
  Value : Single;
  Str   : string;
begin
 with TLoudnessMeterModule(Owner) do
  begin
   Value := RoundTo(UnitOffset + PeakHold, -1);
   Str := FloatToStrF(Value, ffGeneral, 4, 4);

   // correct FS scale
   if Round(Parameter[0]) = 1 then Value := Value + 23;

   if Value < -18.1 then
    begin
     Str := 'LOW';
     LbMomentaryPeak.Font.Color := $00DBE1E3;
    end else
   if Value < -1.1
    then LbMomentaryPeak.Font.Color := $000080FF else
   if Value < 1
    then LbMomentaryPeak.Font.Color := $0000AE00
    else
     begin
      LbMomentaryPeak.Font.Color := clRed;
      if Value > 9
       then Str := 'OVER';
     end;

   LbMomentaryPeak.Caption := Str;
  end;
end;

procedure TFmLoudnessMeter.UpdateTotalSamples;
var
  Hr, Min, Sec : Integer;
  Str          : string;
begin
 with TLoudnessMeterModule(Owner) do
  begin
   Sec := Round(TotalSamples / SampleRate);
   Min := Sec div 60;
   Hr  := Min div 60;
   Min := Min mod 60;
   Sec := Sec mod 60;
   Str := IntToStr(Sec);
   if Sec < 10 then Str := '0' + Str;
   Str := IntToStr(Min) + ':' + Str;
   if Min < 10 then Str := '0' + Str;
   Str := IntToStr(Hr) + ':' + Str;
   if Hr < 10 then Str := '0' + Str;
   LbTimeDisplay.Caption := Str;
  end;
end;

procedure TFmLoudnessMeter.UpdateScale;
begin
 with TLoudnessMeterModule(Owner)
  do LbUnit.Caption := string(ParameterDisplay[0]);

 UpdateLoudness;
 UpdatePeak;
end;

procedure TFmLoudnessMeter.UpdateState;
begin
 with TLoudnessMeterModule(Owner)
  do TrScreenUpdate.Enabled := Parameter[2] > 0.5;
end;

procedure TFmLoudnessMeter.UpdateTime;
begin
 with TLoudnessMeterModule(Owner) do
  begin
   LbIntegrationTime.Caption := string(ParameterDisplay[1]);
   case Round(Parameter[1]) of
    0 : MiIntegrationMomentary.Checked := True;
    1 : MiIntegrationShort.Checked := True;
    2 : MiIntegrationLongTerm.Checked := True;
   end;
  end;

 UpdateLoudness;
 UpdatePeak;
end;

end.
