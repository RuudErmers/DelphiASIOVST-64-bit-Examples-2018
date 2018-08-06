unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, ComCtrls, ActnList, DAV_GuiByteMap,
  DAV_GuiPixelMap, DAV_GuiGraphicControl, DAV_GuiLabel, DAV_GuiSlider,
  DAV_GuiFilters, DAV_GuiVectorPixelGraph, DAV_GuiFixedPoint;

type
  TFmESTP = class(TForm)
    ACAddTinyValue: TAction;
    ACSubtractTinyValue: TAction;
    ActionList1: TActionList;
    MiAddTinyValue: TMenuItem;
    MiScenarioExceedBorders: TMenuItem;
    MiScenarioLargeUpDown: TMenuItem;
    MiScenarioPeakLine1: TMenuItem;
    MiScenarioPeakLine2: TMenuItem;
    MiScenarioPeakLine3: TMenuItem;
    MiScenarioRandom: TMenuItem;
    MiScenarioSmallIncrease: TMenuItem;
    MiScenarioStandard: TMenuItem;
    MiSubtractTinyValue: TMenuItem;
    MiUpDown: TMenuItem;
    MiWidthA: TMenuItem;
    MiWidthB: TMenuItem;
    MiWidthC: TMenuItem;
    MiWidthD: TMenuItem;
    MiWidthE: TMenuItem;
    MiWidthF: TMenuItem;
    MiWidthG: TMenuItem;
    MiWidthH: TMenuItem;
    N1: TMenuItem;
    PaintBox: TPaintBox;
    PuLinePreset: TPopupMenu;
    PuScenario: TPopupMenu;
    SlLineWidth: TGuiSlider;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SlLineWidthChange(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure ACAddTinyValueExecute(Sender: TObject);
    procedure ACSubtractTinyValueExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MiScenarioExceedBordersClick(Sender: TObject);
    procedure MiScenarioLargeUpDownClick(Sender: TObject);
    procedure MiScenarioPeakLine1Click(Sender: TObject);
    procedure MiScenarioPeakLine2Click(Sender: TObject);
    procedure MiScenarioPeakLine3Click(Sender: TObject);
    procedure MiScenarioRandomClick(Sender: TObject);
    procedure MiScenarioSmallIncreaseClick(Sender: TObject);
    procedure MiScenarioStandardClick(Sender: TObject);
    procedure MiUpDownClick(Sender: TObject);
    procedure MiWidthAClick(Sender: TObject);
    procedure MiWidthBClick(Sender: TObject);
    procedure MiWidthCClick(Sender: TObject);
    procedure MiWidthDClick(Sender: TObject);
    procedure MiWidthEClick(Sender: TObject);
    procedure MiWidthFClick(Sender: TObject);
    procedure MiWidthGClick(Sender: TObject);
    procedure MiWidthHClick(Sender: TObject);
  private
    FPixelMap       : TGuiCustomPixelMap;
    FLineWidth      : Single;
    FPaintBoxUpdate : Boolean;
    FPointArray     : array of Double;

    FESPL           : TGuiPixelEquallySpacedPolyline;
    procedure SetLineWidth(const Value: Single);

    function GetValueHandler(Sender: TObject; PixelPosition: Integer): TFixed24Dot8;

    procedure ScenarioPeakLine1;
    procedure ScenarioPeakLine2;
    procedure ScenarioPeakLine3;
    procedure ScenarioRandom;
    procedure ScenarioStandard;
    procedure ScenarioSmallIncrease;
    procedure ScenarioExceedBorders;
    procedure ScenarioLargeUpDown;
    procedure ScenarioUpDown;
    procedure UpdateStatusInformation;
  protected
    procedure LineWidthChanged; virtual;
    procedure UpdateGui; virtual;
    procedure RenderNewPolyline;
    procedure RenderReferencePolyline;
    procedure RenderPolyline;
    procedure RenderPolyline2Pixel;
    procedure RenderPolylineDraft;
    procedure RenderExternalPolyline;
  public
    property LineWidth: Single read FLineWidth write SetLineWidth;
    property PixelMap: TGuiCustomPixelMap read FPixelMap;
  end;

var
  FmESTP: TFmESTP;

implementation

{$R *.dfm}

uses
  Math, DAV_Types, DAV_Common, DAV_GuiCommon, DAV_GuiBlend,
  DAV_MemoryUtils, Magnifier;

procedure TFmESTP.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 FESPL := TGuiPixelEquallySpacedPolyline.Create;
 FESPL.Color := clWhite;
 FESPL.Alpha := $FF;
 FESPL.LineWidth := ConvertToFixed24Dot8(2);
 FESPL.GeometricShape.OnGetValue := GetValueHandler;

 FPaintBoxUpdate := True;
 PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
 FLineWidth := 2;
end;

procedure TFmESTP.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FESPL);
 FreeAndNil(FPixelMap);
end;

procedure TFmESTP.FormShow(Sender: TObject);
begin
 UpdateStatusInformation;
end;

procedure TFmESTP.FormResize(Sender: TObject);
begin
 with FPixelMap do
  begin
   SetSize(PaintBox.Width, PaintBox.Height);
   SetLength(FPointArray, PaintBox.Width);

   ScenarioStandard;
   FPaintBoxUpdate := True;
  end;
end;

function TFmESTP.GetValueHandler(Sender: TObject;
  PixelPosition: Integer): TFixed24Dot8;
begin
 if (PixelPosition > 0) and (PixelPosition < Length(FPointArray))
  then Result := ConvertToFixed24Dot8(FPointArray[PixelPosition])
  else Result := ConvertToFixed24Dot8(0.5 * FPixelMap.Height);
end;

procedure TFmESTP.ScenarioStandard;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   FPointArray[0] := Round(0.5 * Height);
   FPointArray[1] := FPointArray[0]; // + 0.0001;
   FPointArray[2] := FPointArray[0];
   FPointArray[3] := FPointArray[0];
   FPointArray[4] := 0.1 * Height;
   for x := 5 to 19
    do FPointArray[x] := (1 + 0.1 * (x - 4)) * 0.1 * Height;

   for x := 20 to 27
    do FPointArray[x] := FPointArray[19];
   FPointArray[28] := 0.3 * Height;
   FPointArray[29] := 0.5 * Height;
   for x := 30 to 32
    do FPointArray[x] := 0.7 * Height;

   for x := 33 to 59
    do FPointArray[x] := Round(0.5 * Height);
   FPointArray[44] := 0.1 * Height;
   FPointArray[45] := 0.9 * Height;

   for x := 60 to 90
    do FPointArray[x] := 0.5 * Height + x;

   for x := 91 to 199
    do FPointArray[x] := 0.5 * Height + 0.008 * Sqr(x - 91);

   for x := 200 to Length(FPointArray) - 1
    do FPointArray[x] := Height * (0.5 * (1 + 0.5 * (Random - Random)));

  end;
end;

procedure TFmESTP.ScenarioUpDown;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1
    do FPointArray[x] := 0.1 + 0.5 * Height + (2 * ((x div 32) mod 2) - 1) *
      (x mod 32) + 0.1 * (x div 32) - 32 * ((x div 32) mod 2);
  end;
end;

procedure TFmESTP.ScenarioLargeUpDown;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1
    do FPointArray[x] := 0.1 + Height * (0.825 + 1 / 24 * ((2 * (((x + 8) div 16) mod 2) - 1) *
      ((x + 8) mod 16) - 16 * (((x + 8) div 16) mod 2))) + 0.1 * ((x + 8) div 16);
  end;
end;

procedure TFmESTP.ScenarioPeakLine1;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1
    do FPointArray[x] := Round(0.5 * Height) + 0.001;
   FPointArray[1] := Round(0.1 * Height);
   FPointArray[2] := Round(0.1 * Height);
   FPointArray[3] := Round(0.1 * Height);
   FPointArray[4] := Round(0.1 * Height);

   FPointArray[24] := Round(0.1 * Height) + 0.5;
   FPointArray[25] := Round(0.1 * Height) + 0.5;
   FPointArray[26] := Round(0.1 * Height) + 0.5;
   FPointArray[27] := Round(0.1 * Height) + 0.5;
   FPointArray[28] := Round(0.1 * Height) + 0.5;

   FPointArray[42] := 0.9 * Height;


   FPointArray[56] := 0.9 * Height;
   FPointArray[57] := 0.9 * Height;
   FPointArray[58] := 0.9 * Height;
   FPointArray[59] := 0.9 * Height;
   FPointArray[60] := 0.9 * Height;

   FPointArray[76] := 0.6 * Height;
   FPointArray[79] := 0.9 * Height;

   FPointArray[77] := FPointArray[79];
   FPointArray[78] := FPointArray[79];
   FPointArray[80] := FPointArray[79];

   FPointArray[84] := Round(FPointArray[84]);
   for x := 85 to 130
    do FPointArray[x] := FPointArray[x - 1] + 1.5;
(*
   FPointArray[5] := 0.1 * Height;
   FPointArray[6] := 0.1 * Height;
   FPointArray[7] := 0.1 * Height;
   FPointArray[8] := 0.1 * Height;
*)
  end;
end;

procedure TFmESTP.ScenarioPeakLine2;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1 do
    begin

     case x mod 64 of
      0 : FPointArray[x] := Round(0.1 * Height);
      32 : FPointArray[x] := Round(0.9 * Height);
      else FPointArray[x] := Round(0.5 * Height);
     end;

     FPointArray[x] := FPointArray[x] + x / Length(FPointArray);
    end;
  end;
end;

procedure TFmESTP.ScenarioPeakLine3;
var
  x : Integer;
begin
 with FPixelMap do
  begin
   for x := 0 to Length(FPointArray) - 1 do
    begin

     case x mod 64 of
       0 : FPointArray[x] := Round(0.1 * Height);
       1 : FPointArray[x] := Round(0.9 * Height);
      32 : FPointArray[x] := Round(0.9 * Height);
      33 : FPointArray[x] := Round(0.1 * Height);
      else FPointArray[x] := Round(0.5 * Height);
     end;

     FPointArray[x] := FPointArray[x] + x / Length(FPointArray);
    end;
  end;
end;

procedure TFmESTP.ScenarioRandom;
var
  x : Integer;
begin
 with FPixelMap do
  for x := 0 to Length(FPointArray) - 1
   do FPointArray[x] := Height * (0.5 * (1 + 0.5 * (Random - Random)));
end;

procedure TFmESTP.ScenarioSmallIncrease;
var
  x : Integer;
begin
 with FPixelMap do
  for x := 0 to Length(FPointArray) - 1
   do FPointArray[x] := Height * 0.5 - 4 * x / Length(FPointArray);
end;

procedure TFmESTP.ScenarioExceedBorders;
var
  x : Integer;
begin
 with FPixelMap do
  for x := 0 to Length(FPointArray) - 1
   do FPointArray[x] := (2 * Random - 0.5) * Height;
end;

procedure TFmESTP.LineWidthChanged;
begin
 UpdateGui;
end;

procedure TFmESTP.PaintBoxClick(Sender: TObject);
begin
 with FmMagnifier do
  begin
   Show;
   Magnify(4);
  end;
end;

procedure TFmESTP.UpdateGui;
begin
 FPaintBoxUpdate := True;
 PaintBox.Invalidate
end;

procedure TFmESTP.MiWidthAClick(Sender: TObject);
begin
 SlLineWidth.Value := 2.0;
end;

procedure TFmESTP.MiWidthBClick(Sender: TObject);
begin
 SlLineWidth.Value := 2.99999;
end;

procedure TFmESTP.MiWidthCClick(Sender: TObject);
begin
 SlLineWidth.Value := 3.0;
end;

procedure TFmESTP.MiWidthDClick(Sender: TObject);
begin
 SlLineWidth.Value := 3.5;
end;

procedure TFmESTP.MiWidthEClick(Sender: TObject);
begin
 SlLineWidth.Value := 4.5;
end;

procedure TFmESTP.MiWidthFClick(Sender: TObject);
begin
 SlLineWidth.Value := 7.0;
end;

procedure TFmESTP.MiWidthGClick(Sender: TObject);
begin
 SlLineWidth.Value := 7.076;
end;

procedure TFmESTP.MiWidthHClick(Sender: TObject);
begin
 SlLineWidth.Value := 9.0;
end;

procedure TFmESTP.MiScenarioExceedBordersClick(Sender: TObject);
begin
 MiScenarioExceedBorders.Checked := True;
 ScenarioExceedBorders;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioLargeUpDownClick(Sender: TObject);
begin
 MiScenarioLargeUpDown.Checked := True;
 ScenarioLargeUpDown;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioPeakLine1Click(Sender: TObject);
begin
 MiScenarioPeakLine1.Checked := True;
 ScenarioPeakLine1;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioPeakLine2Click(Sender: TObject);
begin
 MiScenarioPeakLine2.Checked := True;
 ScenarioPeakLine2;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioPeakLine3Click(Sender: TObject);
begin
 MiScenarioPeakLine3.Checked := True;
 ScenarioPeakLine3;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioRandomClick(Sender: TObject);
begin
 MiScenarioRandom.Checked := True;
 ScenarioRandom;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioSmallIncreaseClick(Sender: TObject);
begin
 MiScenarioSmallIncrease.Checked := True;
 ScenarioSmallIncrease;
 UpdateGui;
end;

procedure TFmESTP.MiScenarioStandardClick(Sender: TObject);
begin
 MiScenarioStandard.Checked := True;
 ScenarioStandard;
 UpdateGui;
end;

procedure TFmESTP.ACAddTinyValueExecute(Sender: TObject);
begin
 SlLineWidth.Value := SlLineWidth.Value + 0.0001;
end;

procedure TFmESTP.ACSubtractTinyValueExecute(Sender: TObject);
begin
 SlLineWidth.Value := SlLineWidth.Value - 0.0001;
end;

procedure TFmESTP.MiUpDownClick(Sender: TObject);
begin
 MiUpDown.Checked := True;
 ScenarioUpDown;
 UpdateGui;
end;

procedure TFmESTP.PaintBoxPaint(Sender: TObject);
begin
 if FPaintBoxUpdate then
  begin
//   RenderNewPolyline;
//   RenderReferencePolyline;
//   RenderPolyline;
   RenderExternalPolyline;

   with FmMagnifier do
    if Visible then
     begin
      Magnify(4);
      PaintBox.Invalidate;
     end;
  end;

 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(PaintBox.Canvas);
end;

procedure MergeBytesInplace(Foreground: Byte; var Background: Byte);
begin
 Background := Round($FF - ($FF - Foreground) * (1 - Background * COne255th));
end;


{$DEFINE DrawSolid}
{-$DEFINE DrawAntialiasedBorder}
{-$DEFINE DrawAntialiasedLines}

{-$DEFINE DrawInnerHalfLines}
{-$DEFINE DrawOuterHalfLines}

{-$DEFINE DrawHalo}



{-$DEFINE ShowCenter}
{-$DEFINE ShowHalfLine}


procedure TFmESTP.RenderReferencePolyline;
var
  Distance        : Double;
  IntLineWdth     : Double;
  RadiusMinusHalf : Double;
  SqrRadius       : Double;
  SqrDist         : Double;

  Sum, Mn, Mx     : Double;
  Value           : Double;
  XPos            : Double;
  YDest, YSrc     : Double;

  CurrentValue    : Double;

  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  YBounds         : array [0..1] of Integer;
  NewSolid        : Integer;
  x, y            : Integer;
  PtIndex, PtSgn  : Integer;
  PtOuter         : Integer;

  YValues         : array of Double;
  VertLine        : PByteArray;
  PointPtr        : PDAVDoubleFixedArray;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;
begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;
   IntLineWdth := Max(FLineWidth - 1, 0);
   RadiusMinusHalf := 0.5 * IntLineWdth;
   SqrRadius := Sqr(RadiusMinusHalf + 1);

   GetAlignedMemory(VertLine, Height);
   try
    // initialize temporaty variables
    IntegerRadiusX := 1 + Ceil(RadiusMinusHalf);
    IntegerRadiusY := 2 + Trunc(RadiusMinusHalf);
    SetLength(YValues, 1 + 2 * IntegerRadiusX);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[IntegerRadiusX];

    // fill additional points
    for PtIndex := 0 to IntegerRadiusX - 1
     do YValues[PtIndex] := 0.5 * Height;

    for PtIndex := IntegerRadiusX to Length(YValues) - 1
     do YValues[PtIndex] := FPointArray[PtIndex - IntegerRadiusX];


    for x := 0 to Width - 1 do
     begin
      // get next value
      if IntegerRadiusX + X < Length(FPointArray)
       then YValues[Length(YValues) - 1] := FPointArray[x + IntegerRadiusX]
       else YValues[Length(YValues) - 1] := 0;

      // clear vertical line array
      FillChar(VertLine^, Height, 0);

      // determine minimum and maximum
      Mn := PointPtr[0]; // - IntLineWdth;
      Mx := PointPtr[0]; // + IntLineWdth;
      for PtIndex := 1 to IntegerRadiusX - 2 do
       begin
        if PointPtr[ PtIndex] > Mx then Mx := PointPtr[ PtIndex];
        if PointPtr[ PtIndex] < Mn then Mn := PointPtr[ PtIndex];
        if PointPtr[-PtIndex] > Mx then Mx := PointPtr[-PtIndex];
        if PointPtr[-PtIndex] < Mn then Mn := PointPtr[-PtIndex];
       end;

      // determine y bounds
      YBounds[0] := Trunc(Mn - RadiusMinusHalf);
      YBounds[1] := Ceil(Mx + RadiusMinusHalf);
      for PtIndex := Max(1, IntegerRadiusX - 2) to IntegerRadiusX do
       begin
        CurrentValue := PointPtr[PtIndex];
        if CurrentValue - RadiusMinusHalf < YBounds[0] then YBounds[0] := Trunc(CurrentValue - RadiusMinusHalf);
        if CurrentValue + RadiusMinusHalf > YBounds[1] then YBounds[1] := Ceil(CurrentValue + RadiusMinusHalf);
        CurrentValue := PointPtr[-PtIndex];
        if CurrentValue - RadiusMinusHalf < YBounds[0] then YBounds[0] := Trunc(CurrentValue - RadiusMinusHalf);
        if CurrentValue + RadiusMinusHalf > YBounds[1] then YBounds[1] := Ceil(CurrentValue + RadiusMinusHalf);
       end;

      if YBounds[0] < 0 then YBounds[0] := 0;
      if YBounds[1] > Height - 1 then YBounds[1] := Height - 1;
      Assert(YBounds[0] <= YBounds[1]);

      for y := YBounds[0] to YBounds[1] do
       begin
        Sum := 0;

        if (y > Mn) and (y < Mx) then
         begin
          VertLine^[y] := $FF;
          Continue;
         end;

        for PtIndex := -IntegerRadiusX to IntegerRadiusX do
         begin
          // initialize defaults
          XPos := PtIndex;
          CurrentValue := PointPtr[PtIndex];

          if (Abs(PtIndex) >= IntegerRadiusX - 1) then
           begin
            PtSgn := Sign(PtIndex);

            YSrc := PointPtr[PtIndex - PtSgn];
            YDest := PointPtr[PtIndex];

            if YDest <> YSrc then
             begin
              if (YDest > Y) and (YSrc <= Y) then
               begin
                CurrentValue := Y;
                XPos := (Y - YDest) / (YSrc - YDest);
                XPos := PtIndex - PtSgn * XPos;
               end else
              if (YDest < Y) and (YSrc >= Y) then
               begin
                CurrentValue := Y;
                XPos := (Y - YDest) / (YSrc - YDest);
                XPos := PtIndex - PtSgn * XPos;
               end
              else
               if Abs(PtIndex) >= IntegerRadiusX
                then //Continue
                else
                 if ((PointPtr[PtIndex - PtSgn] < PointPtr[PtIndex]) and
                     (PointPtr[PtIndex + PtSgn] > PointPtr[PtIndex])) or
                    ((PointPtr[PtIndex - PtSgn] > PointPtr[PtIndex]) and
                    (PointPtr[PtIndex + PtSgn] < PointPtr[PtIndex]))
                  then Continue;
             end;
          end;

          SqrDist := Sqr(CurrentValue - y) + Sqr(XPos);

          if SqrDist < SqrRadius then
           begin
            Distance := Sqrt(SqrDist);
            if Distance > RadiusMinusHalf
             then
              begin
               Value := (Distance - RadiusMinusHalf);
               Assert(Value >= 0);
               Assert(Value <= 1);
               Sum := 1 - Value * (1 - Sum);
               if Sum > 1 then Sum := 1;
               if Sum = 1 then Break;
              end
             else
              begin
               Sum := 1;
               Break;
              end;
           end;
         end;

        VertLine^[y] := Round($FF * Limit(Sum, 0, 1));
       end;



      // copy line to pixel map
      for y := 0 to Height - 1 do
       if VertLine^[y] > 0
        then CombinePixelInplace(PxColor, PixelPointer[x, y]^, VertLine^[y]);
      EMMS;


      {$IFDEF ShowCenter}
      if (PointPtr[0] >= 0) and (PointPtr[0] < Height) then
       begin
        CurrentValue := PointPtr[0];
        DontRaiseExceptionsAndSetFPUcodeword;
        Y := Round(CurrentValue);
        BlendPixelInplace(pxRed32, PixelPointer[x, Y]^);
       end;
      EMMS;
      {$ENDIF}


      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(Double));
     end;
   finally
    FreeAlignedMemory(VertLine);
   end;

   MakeOpaque;
  end;
end;


procedure TFmESTP.RenderExternalPolyline;
begin
 // clear
 FPaintBoxUpdate := False;
 FPixelMap.FillRect(FPixelMap.ClientRect, pxBlack32);

 FESPL.Draw(FPixelMap);
end;


procedure TFmESTP.RenderNewPolyline;
var
  Distance        : Double;
  IntLineWdth     : Double;
  Radius          : Double;
  RadiusMinusHalf : Double;
  SqrRadius       : Double;
  SqrDist         : Double;

  Sum, Mn, Mx     : Double;
  Value           : Double;
  XPos            : Double;
  YDest, YSrc     : Double;

  CurrentValue    : Double;

  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  YBounds         : array [0..1] of Integer;
  NewSolid        : Integer;
  x, y            : Integer;
  PtIndex, PtSgn  : Integer;
  PtOuter         : Integer;

  YValues         : array of Double;
  VertLine        : PByteArray;
  PointPtr        : PDAVDoubleFixedArray;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;

begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;
   IntLineWdth := Max(FLineWidth - 1, 0);
   RadiusMinusHalf := 0.5 * IntLineWdth;
   Radius := RadiusMinusHalf + 1;
   SqrRadius := Sqr(Radius);

   GetAlignedMemory(VertLine, Height);
   try
    // initialize temporaty variables
    IntegerRadiusX := 1 + Ceil(RadiusMinusHalf);
    IntegerRadiusY := 2 + Trunc(RadiusMinusHalf);
    SetLength(YValues, 1 + 2 * IntegerRadiusX);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[IntegerRadiusX];

    // fill additional points
    for PtIndex := 0 to IntegerRadiusX - 1
     do YValues[PtIndex] := 0.5 * Height;

    for PtIndex := IntegerRadiusX to Length(YValues) - 1
     do YValues[PtIndex] := FPointArray[PtIndex - IntegerRadiusX];


    for x := 0 to Width - 1 do
     begin
      // get next value
      if IntegerRadiusX + X < Length(FPointArray)
       then YValues[Length(YValues) - 1] := FPointArray[x + IntegerRadiusX]
       else YValues[Length(YValues) - 1] := 0;

      // clear vertical line array
      FillChar(VertLine^, Height, 0);

      // determine minimum and maximum
      Mn := PointPtr[0]; // - IntLineWdth;
      Mx := PointPtr[0]; // + IntLineWdth;
      for PtIndex := 1 to IntegerRadiusX - 2 do
       begin
        if PointPtr[ PtIndex] > Mx then Mx := PointPtr[ PtIndex];
        if PointPtr[ PtIndex] < Mn then Mn := PointPtr[ PtIndex];
        if PointPtr[-PtIndex] > Mx then Mx := PointPtr[-PtIndex];
        if PointPtr[-PtIndex] < Mn then Mn := PointPtr[-PtIndex];
       end;

      // determine y bounds
      YBounds[0] := Trunc(Mn - RadiusMinusHalf);
      YBounds[1] := Ceil(Mx + RadiusMinusHalf);
      for PtIndex := Max(1, IntegerRadiusX - 2) to IntegerRadiusX do
       begin
        CurrentValue := PointPtr[PtIndex];
        if CurrentValue - RadiusMinusHalf < YBounds[0] then YBounds[0] := Trunc(CurrentValue - RadiusMinusHalf);
        if CurrentValue + RadiusMinusHalf > YBounds[1] then YBounds[1] := Ceil(CurrentValue + RadiusMinusHalf);
        CurrentValue := PointPtr[-PtIndex];
        if CurrentValue - RadiusMinusHalf < YBounds[0] then YBounds[0] := Trunc(CurrentValue - RadiusMinusHalf);
        if CurrentValue + RadiusMinusHalf > YBounds[1] then YBounds[1] := Ceil(CurrentValue + RadiusMinusHalf);
       end;

      if YBounds[0] < 0 then YBounds[0] := 0;
      if YBounds[1] > Height - 1 then YBounds[1] := Height - 1;
      Assert(YBounds[0] <= YBounds[1]);

      for y := YBounds[0] to YBounds[1] do
       begin
        // check for solid area
        if (y > Mn) and (y < Mx) then
         begin
          VertLine^[y] := $FF;
          Continue;
         end;

        // draw center
        Sum := 0;
        Value := Abs(PointPtr[0] - y);
        if Value < Radius then
         begin
          Value := Value - RadiusMinusHalf;
          if Value > 0 then
           begin
            Sum := 1 - Value;
            if Sum >= 1 then
             begin
              VertLine^[y] := $FF;
              Continue;
             end;
           end
          else
           begin
            VertLine^[y] := $FF;
            Continue;
           end;
         end;

        for PtIndex := 1 to IntegerRadiusX - 2 do
         begin
          // draw left
          CurrentValue := PointPtr[-PtIndex];
          SqrDist := Sqr(CurrentValue - y) + Sqr(PtIndex);
          if SqrDist < SqrRadius then
           begin
            Distance := Sqrt(SqrDist);
            if Distance > RadiusMinusHalf
             then
              begin
               Value := (Distance - RadiusMinusHalf);
               Sum := 1 - Value * (1 - Sum);
               if Sum > 1 then Sum := 1;
               if Sum = 1 then Break;
              end
             else
              begin
               Sum := 1;
               Break;
              end;
           end;

          // draw right
          CurrentValue := PointPtr[PtIndex];
          SqrDist := Sqr(CurrentValue - y) + Sqr(PtIndex);
          if SqrDist < SqrRadius then
           begin
            Distance := Sqrt(SqrDist);
            if Distance > RadiusMinusHalf
             then
              begin
               Value := (Distance - RadiusMinusHalf);
               Sum := 1 - Value * (1 - Sum);
               if Sum > 1 then Sum := 1;
               if Sum = 1 then Break;
              end
             else
              begin
               Sum := 1;
               Break;
              end;
           end;
         end;

        // check if sum already equals 1
        if Sum = 1 then
         begin
          VertLine^[y] := $FF;
          Continue;
         end;


        {$IFDEF DrawAntialiasedLines}

        {$IFDEF DrawInnerHalfLines}
        PtIndex := IntegerRadiusX - 1;
        if PtIndex > 0 then
         for LeftRightIdx := 0 to 1 do
          begin
           // initialize defaults
           PtSgn := (2 * LeftRightIdx - 1);
           XPos := PtSgn * PtIndex;
           CurrentValue := PointPtr[PtSgn * PtIndex];

           YSrc := PointPtr[PtSgn * (PtIndex - 1)];
           YDest := PointPtr[PtSgn * PtIndex];

           if YDest <> YSrc then
            begin
             if ((YDest >= Y) and (YSrc <= Y)) or
                ((YDest <= Y) and (YSrc >= Y)) then
              begin
               XPos := PtSgn * (PtIndex - (Y - YDest) / (YSrc - YDest));

               if XPos < Radius then
                if Abs(XPos) > RadiusMinusHalf then
                 begin
                  Value := Abs(XPos) - RadiusMinusHalf;
                  Sum := 1 - Value * (1 - Sum);
                  if Sum > 1 then Sum := 1;
                  if Sum = 1 then Break;
                 end
                else
                 begin
                  Sum := 1;
                  Break;
                 end;
               Continue;
              end
             else
              if ((YSrc < YDest - 1) and (PointPtr[PtSgn * (PtIndex + 1)] > YDest + 1)) or
                 ((YSrc > YDest + 1) and (PointPtr[PtSgn * (PtIndex + 1)] < YDest - 1))
               then Continue;
            end;

            SqrDist := Sqr(CurrentValue - y) + Sqr(XPos);

            if SqrDist < SqrRadius then
             begin
              Distance := Sqrt(SqrDist);
              Assert(Distance > RadiusMinusHalf);
              Value := (Distance - RadiusMinusHalf);
              Sum := 1 - Value * (1 - Sum);
              if Sum > 1 then Sum := 1;
              if Sum = 1 then Break;
             end;
          end;


        // check if sum already equals 1
        if Sum = 1 then
         begin
          VertLine^[y] := $FF;
          Continue;
         end;
        {$ENDIF}

        {$IFDEF DrawOuterHalfLines}
        YSrc := PointPtr[IntegerRadiusX - 1];
        YDest := PointPtr[IntegerRadiusX];

        if (((YDest >= Y) and (YSrc <= Y)) or
            ((YDest <= Y) and (YSrc >= Y))) and (YSrc <> YDest) then
         begin
          XPos := IntegerRadiusX - (Y - YDest) / (YSrc - YDest);

          if XPos <= Radius then
           begin
            Assert(XPos >= RadiusMinusHalf);
            Value := XPos - RadiusMinusHalf;
            Sum := 1 - Value * (1 - Sum);
            if Sum >= 1 then
             begin
              VertLine^[y] := $FF;
              Break;
             end;
           end;
         end;

        YSrc := PointPtr[1 - IntegerRadiusX];
        YDest := PointPtr[-IntegerRadiusX];

        if (((YDest >= Y) and (YSrc <= Y)) or
            ((YDest <= Y) and (YSrc >= Y))) and (YSrc <> YDest) then
         begin
          XPos := IntegerRadiusX - (Y - YDest) / (YSrc - YDest);

          if XPos <= Radius then
           begin
            Assert(XPos >= RadiusMinusHalf);
            Value := XPos - RadiusMinusHalf;
            Sum := 1 - Value * (1 - Sum);
            if Sum >= 1 then
             begin
              VertLine^[y] := $FF;
              Break;
             end;
           end;
         end;
        {$ENDIF}
        {$ENDIF}

        VertLine^[y] := Round($FF * Limit(Sum, 0, 1));
       end;


      // copy line to pixel map
      for y := 0 to Height - 1 do
       if VertLine^[y] > 0
        then CombinePixelInplace(PxColor, PixelPointer[x, y]^, VertLine^[y]);
      EMMS;


      {$IFDEF ShowCenter}
      if (PointPtr[0] >= 0) and (PointPtr[0] < Height) then
       begin
        CurrentValue := PointPtr[0];
        DontRaiseExceptionsAndSetFPUcodeword;
        Y := Round(CurrentValue);
        BlendPixelInplace(pxRed32, PixelPointer[x, Y]^);
       end;
      EMMS;
      {$ENDIF}


      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(Double));
     end;
   finally
    FreeAlignedMemory(VertLine);
   end;

   MakeOpaque;
  end;
end;

procedure TFmESTP.RenderPolyline;
var
  InternalLineWidth : Double;
  Radius            : Double;
  SolidRangeCount   : Integer;
  TotalRangeCount   : Integer;

  Distance          : Double;
  IntLineWdth       : Double;
  RadiusMinusHalf   : Double;
  CurrentValue      : Double;
  YDestPos          : Double;
  YSrcPos           : Double;
  YSplitPos         : Double;
  Delta             : Double;
  WidthScale        : Double;
  TempDouble        : Double;

  YRange            : array [0..1] of Integer;
  SolidRange        : array [0..1] of Integer;
  NewYRange         : array [0..1] of Integer;
  IntegerRadiusX    : Integer;
  IntegerRadiusY    : Integer;
  NewSolid          : Integer;
  x, y              : Integer;
  PtIndex           : Integer;

  YValues           : array of Double;
  CircleValues      : array of Double;

  VertLine          : PByteArray;
  PointPtr          : PDAVDoubleFixedArray;
  PxColor           : TPixel32;
  LeftRightIdx      : Integer;

  procedure AddToSolidRange(Lower, Upper: Integer);
  begin
   if Lower < Upper then
    begin
     if Lower < SolidRange[0] then SolidRange[0] := Lower;
     if Upper > SolidRange[1] then SolidRange[1] := Upper;
    end;
  end;

  function CalculateCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

  function CalculateMinCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value - 0.5) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

  function CalculateMaxCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value + 0.5) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   // set local variables
   PxColor := pxWhite32;
   InternalLineWidth := Max(FLineWidth, 0);
   Radius := 0.5 * InternalLineWidth;
   SolidRangeCount := 2 + Trunc(RadiusMinusHalf);
   TotalRangeCount := 1 + Ceil(RadiusMinusHalf);

   // old
   IntLineWdth := Max(FLineWidth - 1, 0);
   RadiusMinusHalf := 0.5 * IntLineWdth;

   GetAlignedMemory(VertLine, Height);
   try
    // initialize temporaty variables
    IntegerRadiusX := 1 + Ceil(RadiusMinusHalf);
    IntegerRadiusY := 2 + Trunc(RadiusMinusHalf);
    SetLength(YValues, 1 + 2 * IntegerRadiusX);
    SetLength(CircleValues, IntegerRadiusX - 1);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[IntegerRadiusX];

    // initialize circle values
    for PtIndex := 0 to Length(CircleValues) - 1 do
     begin
      CircleValues[PtIndex] := Sqrt(Sqr(RadiusMinusHalf) - Sqr(PtIndex));
     end;

    // fill additional points
    for PtIndex := 0 to IntegerRadiusX - 1
     do YValues[PtIndex] := 0.5 * Height;

    for PtIndex := IntegerRadiusX to Length(YValues) - 1
     do YValues[PtIndex] := FPointArray[PtIndex - IntegerRadiusX];


    for x := 0 to Width - 1 do
     begin
      // get next value
      if IntegerRadiusX + X < Length(FPointArray)
       then YValues[Length(YValues) - 1] := FPointArray[x + IntegerRadiusX]
       else YValues[Length(YValues) - 1] := 0;

      // clear vertical line array
      FillChar(VertLine^, Height, 0);

      ///////////////////
      // start drawing //
      ///////////////////

      // calculate solid range
      CurrentValue := PointPtr^[0];
      YRange[0] := Trunc(CurrentValue - RadiusMinusHalf);
      YRange[1] := Ceil(CurrentValue + RadiusMinusHalf);
      SolidRange[0] := YRange[0] + 1;
      SolidRange[1] := YRange[1] - 1;

      // check for the solid range
      for PtIndex := 1 to IntegerRadiusY - 2 do
       begin
        // calculate distance
        Distance := Sqrt(Sqr(RadiusMinusHalf) - Sqr(PtIndex));

        for LeftRightIdx := 0 to 1 do
         begin
          CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

          NewSolid := Trunc(CurrentValue - Distance) + 1;
          if NewSolid < SolidRange[0]
           then SolidRange[0] := NewSolid
           else
            begin
             NewSolid := Ceil(CurrentValue + Distance) - 1;
             if NewSolid > SolidRange[1]
              then SolidRange[1] := NewSolid;
            end;
         end;
       end;





      {$IFDEF DrawAntialiasedBorder}
      // draw antialiased border
      if (YRange[0] < SolidRange[0]) or (YRange[0] >= SolidRange[1]) then
       begin
        Distance := 1 + RadiusMinusHalf - PointPtr^[0] + YRange[0];
        if (YRange[0] >= 0) and (YRange[0] < Height) and (VertLine^[YRange[0]] < $FF)
         then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[0]]);

        Assert(Distance > 0);
        Assert(Distance <= 1);
       end;

      if (YRange[1] < SolidRange[0]) or (YRange[1] >= SolidRange[1]) then
       begin
        Distance := 1 + RadiusMinusHalf - YRange[1] + PointPtr^[0];
        if (YRange[1] >= 0) and (YRange[1] < Height) and (VertLine^[YRange[1]] < $FF)
         then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[1]]);

        Assert(Distance > 0);
        Assert(Distance <= 1);
       end;
      {$ENDIF}




      {$IFDEF DrawAntialiasedLines}

      // calculate width scale
      WidthScale := RadiusMinusHalf - (IntegerRadiusX - 2);


      {$IFDEF DrawInnerHalfLines}
      if IntegerRadiusY = IntegerRadiusX then
       for LeftRightIdx := 0 to 1 do
        begin
         // set start/end values (left/right)
         YDestPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 2)];
         YSrcPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];

         if Abs(YDestPos - YSrcPos) <= 1
          then Continue;

(*
         // eventually skip drawing if inside the solid range
         if YDestPos < YSrcPos then
          if (YDestPos > SolidRange[0]) and (YSrcPos < SolidRange[1])
           then Continue else else
          if (YSrcPos > SolidRange[0]) and (YDestPos < SolidRange[1])
           then Continue;
*)

         // calculate split point
         YSplitPos := YDestPos + WidthScale * (YSrcPos - YDestPos);

         if YSrcPos <> YSplitPos then
          begin
           if YDestPos < YSrcPos then
            begin
             YRange[0] := Round(YSplitPos);
             YRange[1] := Round(YSrcPos);

             AddToSolidRange(Round(YDestPos), YRange[0]);
            end
           else
            begin
             YRange[0] := Round(YSrcPos);
             YRange[1] := Round(YSplitPos);
             AddToSolidRange(YRange[1], Round(YDestPos));
            end;

           Delta := 1 / (YSplitPos - YSrcPos);

           for Y := YRange[0] to YRange[1] - 1 do
            begin
             TempDouble := WidthScale + (Y - YSrcPos) * Delta;

//             if (TempDouble > 1) or (TempDouble < 0) then Continue;

             if (y >= 0) and (y < Height) and (VertLine^[y] < $FF)
              then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[y]);
            end;
          end;
        end;
      {$ENDIF}



      {$IFDEF DrawOuterHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YDestPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];
        YSrcPos := PointPtr^[(2 * LeftRightIdx - 1) * IntegerRadiusX];

        if Abs(YDestPos - YSrcPos) <= FLineWidth then
         begin
          YRange[0] := Trunc(PointPtr^[0] - RadiusMinusHalf);
          YRange[1] := Ceil(PointPtr^[0] + RadiusMinusHalf);

          // draw antialiased border
          if (YRange[0] < SolidRange[0]) or (YRange[0] >= SolidRange[1]) then
           begin
            Distance := 1 + RadiusMinusHalf - PointPtr^[0] + YRange[0];
            if (YRange[0] >= 0) and (YRange[0] < Height) and (VertLine^[YRange[0]] < $FF)
             then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[0]]);

            Assert(Distance > 0);
            Assert(Distance <= 1);
           end;

          if (YRange[1] < SolidRange[0]) or (YRange[1] >= SolidRange[1]) then
           begin
            Distance := 1 + RadiusMinusHalf - YRange[1] + PointPtr^[0];
            if (YRange[1] >= 0) and (YRange[1] < Height) and (VertLine^[YRange[1]] < $FF)
             then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[1]]);

            Assert(Distance > 0);
            Assert(Distance <= 1);
           end;

          Continue;
         end;

        // calculate split point
        YSplitPos := YDestPos + WidthScale * (YSrcPos - YDestPos);

        if YSplitPos <> YDestPos then
         begin
          if YDestPos < YSrcPos then
           begin
            YRange[0] := Round(YDestPos);
            YRange[1] := Round(YSplitPos);
           end
          else
           begin
            YRange[0] := Round(YSplitPos);
            YRange[1] := Round(YDestPos);
           end;

          Delta := WidthScale / (YDestPos - YSplitPos);

          for Y := YRange[0] to YRange[1] - 1 do
           begin
            TempDouble := (Y - YSplitPos) * Delta;

            // do not consider range outside the line
            if (TempDouble > 1) or (TempDouble < 0)
             then Continue;

            if (y >= 0) and (y < Height) and (VertLine^[Y] < $FF)
             then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[Y]);
           end;
         end;
       end;
      {$ENDIF}

      {$ENDIF}


      // draw round borders
      {$IFDEF DrawHalo}
(*
      for PtIndex := 0 to IntegerRadiusY - 1 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];
         TempDouble := CalculateMaxCircleYOffset(RadiusMinusHalf, PtIndex);

         YRange[0] := Round(CurrentValue - TempDouble);
         YRange[1] := Round(CurrentValue + TempDouble);


         TempDouble := CalculateMinCircleYOffset(RadiusMinusHalf, PtIndex);
         NewYRange[0] := Round(CurrentValue - TempDouble) - 1;
         NewYRange[1] := Round(CurrentValue + TempDouble) + 1;

         for Y := YRange[0] to NewYRange[0] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;

           if (y >= 0) and (y < Height) then
//            if (VertLine^[Y] = 0) then
            begin
//             Assert(RadiusMinusHalf + 0.5 - Distance <= 1);
             MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
            end;
          end;

         for Y := NewYRange[1] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;

           if (y >= 0) and (y < Height) then
//            if (VertLine^[Y] = 0) then
            begin
//             Assert(RadiusMinusHalf + 0.5 - Distance <= 1);
             MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
            end;
          end;


{
         for Y := YRange[0] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;

           if (y >= 0) and (y < Height) then
//            if (VertLine^[Y] = 0) then
             MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
          end;
}
        end;
*)

      for PtIndex := 1 to IntegerRadiusY - 1 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];
         TempDouble := Sqr(RadiusMinusHalf + 0.5) - Sqr(PtIndex);
         if TempDouble > 0
          then TempDouble := Sqrt(TempDouble)
          else TempDouble := 0;

         YRange[0] := Round(CurrentValue - TempDouble);
         YRange[1] := Round(CurrentValue + TempDouble);

         for Y := YRange[0] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;
           if ((y >= SolidRange[0]) and (y <= SolidRange[1]))
            then Continue;

//           if ((y >= UsedRange[0]) and (y <= UsedRange[1])) then Continue;

           if (y >= 0) and (y < Height) and (VertLine[Y] < $FF) then
            if (RadiusMinusHalf + 0.5 - Distance < 1) // and (VertLine^[Y] = 0)
             then MergeBytesInplace(Round(Limit(RadiusMinusHalf + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
          end;

        end;
      {$ENDIF}


      {$IFDEF DrawSolid}
      // fill solid
      for y := Max(0, SolidRange[0]) to Min(Height - 1, SolidRange[1])
       do VertLine^[y] := $FF;
      {$ENDIF}


      // copy line to pixel map
      for y := 0 to Height - 1 do
       if VertLine^[y] > 0
        then CombinePixelInplace(PxColor, PixelPointer[x, y]^, VertLine^[y]);
      EMMS;

      {$IFDEF ShowCenter}
      if (PointPtr[0] >= 0) and (PointPtr[0] < Height) then
       begin
        CurrentValue := PointPtr[0];
        DontRaiseExceptionsAndSetFPUcodeword;
        Y := Round(CurrentValue);
        BlendPixelInplace(pxRed32, PixelPointer[x, Y]^);
       end;
      EMMS;
      {$ENDIF}

      {$IFDEF ShowHalfLine}
      BlendPixelInplace(pxSemiRed32, PixelPointer[x, Round(0.5 * Height)]^);
      EMMS;
      {$ENDIF}

      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(Double));
     end;
   finally
    FreeAlignedMemory(VertLine);
   end;

   MakeOpaque;
  end;
end;

procedure TFmESTP.RenderPolyline2Pixel;
var
  Distance        : Double;
  IntLineWdth     : Double;
  RadiusMinusHalf : Double;
  CurrentValue    : Double;
  YDestPos        : Double;
  YSrcPos         : Double;
  YWSSplitPos     : Double;
  Delta           : Double;
  WidthScale      : Double;
  TempDouble      : Double;

  YRange          : array [0..1] of Integer;
  SolidRange      : array [0..1] of Integer;
  NewYRange       : array [0..1] of Integer;
  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  NewSolid        : Integer;
  x, y            : Integer;
  PtIndex         : Integer;

  YValues         : array of Double;
  VertLine        : PByteArray;
  PointPtr        : PDAVDoubleFixedArray;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;

  procedure AddToSolidRange(Lower, Upper: Integer);
  begin
   if Lower < Upper then
    begin
     if Lower < SolidRange[0] then SolidRange[0] := Lower;
     if Upper + 1 > SolidRange[1] then SolidRange[1] := Upper + 1;
    end;
  end;

(*
  function CalculateCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

  function CalculateMinCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value - 0.5) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;

  function CalculateMaxCircleYOffset(Value: Single; XOffset: Integer): Single;
  begin
   Result := Sqr(Value + 0.5) - Sqr(XOffset);
   if Result > 0
    then Result := Sqrt(Result)
    else Result := 0;
  end;
*)

begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;

   GetAlignedMemory(VertLine, Height);
   try
    // initialize temporaty variables
    SetLength(YValues, 5);
    Assert(Length(YValues) mod 2 = 1);
    PointPtr := @YValues[2];

   // fill additional points
    for PtIndex := 0 to 1
     do YValues[PtIndex] := 0.5 * Height;

    for PtIndex := 2 to Length(YValues) - 1
     do YValues[PtIndex] := FPointArray[PtIndex - 2];


    for x := 0 to Width - 1 do
     begin
      // get next value
      if 2 + X < Length(FPointArray)
       then YValues[Length(YValues) - 1] := FPointArray[x + 2]
       else YValues[Length(YValues) - 1] := 0;

      // clear vertical line array
      FillChar(VertLine^, Height, 0);

      // calculate solid range
      CurrentValue := PointPtr^[0];
      SolidRange[0] := Ceil(CurrentValue - 0.5);
      SolidRange[1] := Trunc(CurrentValue + 0.5) + 1;
      Assert(SolidRange[1] > SolidRange[0]);



      {$IFDEF DrawAntialiasedLines}

      {$IFDEF DrawInnerHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YDestPos := PointPtr^[0];
        YSrcPos := PointPtr^[(2 * LeftRightIdx - 1)];

(*
        if YDestPos < YSrcPos then
         begin
          YDestPos := YDestPos + 0.5;
          YSrcPos := YSrcPos - 0.5;
         end
        else
         begin
          YDestPos := YDestPos - 0.5;
          YSrcPos := YSrcPos + 0.5;
         end;
*)


        if Abs(YDestPos - YSrcPos) < 2
         then Continue;


(*
        // eventually skip drawing if inside the solid range
        if YDestPos < YSrcPos then
         if (YDestPos + 0.5 > SolidRange[0]) and (YSrcPos < SolidRange[1])
          then Continue else else
         if (YSrcPos > SolidRange[0]) and (YDestPos < SolidRange[1])
          then Continue;
*)

        // calculate split point
        YWSSplitPos := 0.5 * (YDestPos + YSrcPos);

        if YSrcPos <> YWSSplitPos then
         begin
          if YDestPos < YSrcPos then
           begin
            YRange[0] := Round(YWSSplitPos);
            YRange[1] := Round(YSrcPos);

            AddToSolidRange(Round(YDestPos), YRange[0]);
           end
          else
           begin
            YRange[0] := Round(YSrcPos);
            YRange[1] := Round(YWSSplitPos);

            AddToSolidRange(YRange[1], Round(YDestPos));
           end;

          Delta := 1  / (YWSSplitPos - YSrcPos);

          for Y := YRange[0] to YRange[1] - 1 do
           begin
            TempDouble := 0.5 + (Y - YSrcPos) * Delta;

            if (y >= 0) and (y < Height) and (VertLine^[y] < $FF)
             then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[y]);
           end;
         end;
       end;
      {$ENDIF}



      {$IFDEF DrawOuterHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YDestPos := PointPtr^[(2 * LeftRightIdx - 1)];
        YSrcPos := PointPtr^[(2 * LeftRightIdx - 1) * 2];

        // calculate split point
        YWSSplitPos := 0.5 * (YSrcPos + YDestPos);

        if YWSSplitPos <> YDestPos then
         begin
          if YDestPos < YSrcPos then
           begin
            YRange[0] := Round(YDestPos);
            YRange[1] := Round(YWSSplitPos);
           end
          else
           begin
            YRange[0] := Round(YWSSplitPos);
            YRange[1] := Round(YDestPos);
           end;

          Delta := 0.5 / (YDestPos - YWSSplitPos);

          for Y := YRange[0] to YRange[1] - 1 do
           begin
            TempDouble := (Y - YWSSplitPos) * Delta;
            if (y >= 0) and (y < Height) and (VertLine^[Y] < $FF)
             then MergeBytesInplace(Round(Limit(TempDouble, 0, 1) * $FF), VertLine^[Y]);
           end;
         end;
       end;
      {$ENDIF}

      {$ENDIF}


      // draw round borders
      {$IFDEF DrawAntialiasedBorder}
      // draw antialiased border
      YRange[0] := Ceil(PointPtr^[0] - 0.5) - 1;
      Distance := YRange[0] - PointPtr^[0] + 1.5;
      Assert(Distance >= 0);
      Assert(Distance <= 1);

      if (SolidRange[0] - 1 >= 0) and (SolidRange[0] - 1 < Height) and (VertLine^[YRange[0]] < $FF)
       then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[0]]);

      YRange[1] := Trunc(PointPtr^[0] + 0.5) + 1;
      Distance := PointPtr^[0] - YRange[1] + 1.5;
      Assert(Distance >= 0);
      Assert(Distance <= 1);
      if (SolidRange[1] >= 0) and (SolidRange[1] < Height) and (VertLine^[YRange[1]] < $FF)
       then MergeBytesInplace(Round($FF * Distance), VertLine^[YRange[1]]);
      {$ENDIF}

      {$IFDEF DrawHalo}
      for PtIndex := 1 to 1 do
       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];
         TempDouble := Sqr(0.5 + 0.5) - Sqr(PtIndex);
         if TempDouble > 0
          then TempDouble := Sqrt(TempDouble)
          else TempDouble := 0;

         YRange[0] := Round(CurrentValue - TempDouble);
         YRange[1] := Round(CurrentValue + TempDouble);

         for Y := YRange[0] to YRange[1] do
          begin
           Distance := Sqrt(Sqr(Y - CurrentValue) + Sqr(PtIndex)) - 0.5;
           if ((y >= SolidRange[0]) and (y <= SolidRange[1]))
            then Continue;

(*
           if ((y >= UsedRange[0]) and (y <= UsedRange[1]))
            then Continue;
*)

           if (y >= 0) and (y < Height) and (VertLine[Y] < $FF) then
            if (0.5 + 0.5 - Distance < 1) // and (VertLine^[Y] = 0)
             then MergeBytesInplace(Round(Limit(0.5 + 0.5 - Distance, 0, 1) * $FF), VertLine^[Y]);
          end;

        end;
      {$ENDIF}


      {$IFDEF DrawSolid}
      // fill solid
      for y := Max(0, SolidRange[0]) to Min(Height, SolidRange[1]) - 1
       do VertLine^[y] := $FF;
      {$ENDIF}


      // copy line to pixel map
      for y := 0 to Height - 1 do
       if VertLine^[y] > 0
        then CombinePixelInplace(PxColor, PixelPointer[x, y]^, VertLine^[y]);
      EMMS;

      {$IFDEF ShowCenter}
      if (PointPtr[0] >= 0) and (PointPtr[0] < Height) then
       begin
        CurrentValue := PointPtr[0];
        DontRaiseExceptionsAndSetFPUcodeword;
        Y := Round(CurrentValue);
        BlendPixelInplace(pxSemiBlue32, PixelPointer[x, Y]^);
       end;
      EMMS;
      {$ENDIF}

      {$IFDEF ShowHalfLine}
      BlendPixelInplace(pxSemiRed32, PixelPointer[x, Round(0.5 * Height)]^);
      EMMS;
      {$ENDIF}

      // shift y-values
      Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(Double));
     end;
   finally
    FreeAlignedMemory(VertLine);
   end;

   MakeOpaque;
  end;
end;


procedure TFmESTP.RenderPolylineDraft;
var
  SolidRange      : array [0..1] of Integer;
  IntegerRadiusX  : Integer;
  IntegerRadiusY  : Integer;
  NewSolid        : Integer;
  x, y            : Integer;
  PtIndex         : Integer;

  YValues         : array of TFixed24Dot8;
  Distance        : TFixed24Dot8;
  IntLineWdth     : TFixed24Dot8;
  RadiusMinusHalf : TFixed24Dot8;
  CurrentValue    : TFixed24Dot8;
  YStartPos       : TFixed24Dot8;
  YEndPos         : TFixed24Dot8;
  WidthScale      : TFixed24Dot8;
  PointPtr        : PFixed24Dot8Array;
  PxColor         : TPixel32;
  LeftRightIdx    : Integer;


  procedure AddToSolidRange(Lower, Upper: Integer);
  begin
   if Lower < Upper then
    begin
     if Lower < SolidRange[0] then SolidRange[0] := Lower;
     if Upper > SolidRange[1] then SolidRange[1] := Upper;
    end;
  end;

begin
 FPaintBoxUpdate := False;
 with FPixelMap do
  begin
   FillRect(ClientRect, pxBlack32);

   PxColor := pxWhite32;
   IntLineWdth := ConvertToFixed24Dot8(Max(FLineWidth - 1, 0));
   RadiusMinusHalf := FixedMul(IntLineWdth, CFixed24Dot8Half);

   // initialize temporaty variables
   IntegerRadiusX := 1 + FixedCeil(RadiusMinusHalf);
   IntegerRadiusY := 2 + FixedFloor(RadiusMinusHalf);
   SetLength(YValues, 1 + 2 * IntegerRadiusX);
   Assert(Length(YValues) mod 2 = 1);
   PointPtr := @YValues[IntegerRadiusX];

   // fill additional points
   for PtIndex := 0 to IntegerRadiusX - 1
    do YValues[PtIndex] := ConvertToFixed24Dot8(0.5 * Height);

   for PtIndex := IntegerRadiusX to Length(YValues) - 1
    do YValues[PtIndex] := ConvertToFixed24Dot8(FPointArray[PtIndex - IntegerRadiusX]);


   for x := 0 to Width - 1 do
    begin
     // get next value
     if IntegerRadiusX + x < Length(FPointArray)
      then YValues[Length(YValues) - 1] := ConvertToFixed24Dot8(FPointArray[x + IntegerRadiusX])
      else YValues[Length(YValues) - 1].Fixed := 0;

     // calculate solid range
     CurrentValue := PointPtr^[0];
     SolidRange[0] := FixedRound(FixedSub(CurrentValue, RadiusMinusHalf));
     SolidRange[1] := FixedRound(FixedAdd(CurrentValue, RadiusMinusHalf));

     // check for the solid range
     for PtIndex := 1 to IntegerRadiusY - 2 do
      begin
       // calculate distance
       Distance := FixedSqrt(FixedSub(FixedSqr(RadiusMinusHalf),
         FixedSqr(ConvertToFixed24Dot8(PtIndex))));

       for LeftRightIdx := 0 to 1 do
        begin
         CurrentValue := PointPtr^[(2 * LeftRightIdx - 1) * PtIndex];

         NewSolid := FixedRound(FixedSub(CurrentValue, Distance));
         if NewSolid < SolidRange[0]
          then SolidRange[0] := NewSolid
          else
           begin
            NewSolid := FixedRound(FixedAdd(CurrentValue, Distance));
            if NewSolid > SolidRange[1]
             then SolidRange[1] := NewSolid;
           end;
        end;
      end;
     {$IFDEF DrawAntialiasedLines}

     // calculate width scale (0 < x <= 1)
     WidthScale := FixedSub(RadiusMinusHalf, ConvertToFixed24Dot8(IntegerRadiusX - 2));

     {$IFDEF DrawInnerHalfLines}
     if IntegerRadiusY = IntegerRadiusX then
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 2)];
        YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];

        // calculate split point
        Distance := FixedSub(YStartPos, FixedMul(WidthScale, FixedSub(YStartPos, YEndPos)));
        CurrentValue := FixedAdd(YEndPos,
          ((FixedMul(FixedSub(CFixed24Dot8Half, WidthScale), FixedSub(Distance, YEndPos)))));

        Y := FixedRound(YStartPos);
        if YStartPos.Fixed <= YEndPos.Fixed then
         if FixedRound(CurrentValue) <= FixedRound(YEndPos) - 1
          then AddToSolidRange(Y, FixedRound(CurrentValue))
          else AddToSolidRange(Y, FixedRound(YEndPos) - 1)
        else
         if FixedRound(CurrentValue) + 1 > FixedRound(YEndPos)
          then AddToSolidRange(FixedRound(CurrentValue) + 1, Y)
          else AddToSolidRange(FixedRound(YEndPos), Y);

       end;
     {$ENDIF}

     {$IFDEF DrawOuterHalfLines}
      for LeftRightIdx := 0 to 1 do
       begin
        // set start/end values (left/right)
        YStartPos := PointPtr^[(2 * LeftRightIdx - 1) * (IntegerRadiusX - 1)];
        YEndPos := PointPtr^[(2 * LeftRightIdx - 1) * IntegerRadiusX];

        // calculate split point
        CurrentValue := FixedSub(YStartPos, YEndPos);
        Distance := FixedSub(YStartPos, FixedMul(WidthScale, CurrentValue));
        CurrentValue := FixedAdd(Distance, FixedMul(CFixed24Dot8Half, CurrentValue));

        Y := FixedRound(YStartPos);
        if YStartPos.Fixed <= YEndPos.Fixed then
         if FixedRound(CurrentValue) <= FixedRound(Distance) - 1
          then AddToSolidRange(Y, FixedRound(CurrentValue))
          else AddToSolidRange(Y, FixedRound(Distance) - 1)
        else
         if FixedRound(CurrentValue) + 1 > FixedRound(Distance)
          then AddToSolidRange(FixedRound(CurrentValue) + 1, Y)
          else AddToSolidRange(FixedRound(Distance), Y);
       end;
     {$ENDIF}

     {$ENDIF}

     // copy line to pixel map
     for y := Max(0, SolidRange[0]) to Min(Height - 1, SolidRange[1])
      do BlendPixelInplace(PxColor, PixelPointer[x, y]^);

     // shift y-values
     Move(YValues[1], YValues[0], (Length(YValues) - 1) * SizeOf(TFixed24Dot8));
    end;

   EMMS;
   MakeOpaque;
  end;
end;


procedure TFmESTP.SetLineWidth(const Value: Single);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   LineWidthChanged;
  end;
end;

procedure TFmESTP.SlLineWidthChange(Sender: TObject);
begin
 LineWidth := SlLineWidth.Value;
 FESPL.LineWidth := ConvertToFixed24Dot8(SlLineWidth.Value);
 UpdateStatusInformation;
end;

procedure TFmESTP.UpdateStatusInformation;
var
  TempValue    : Double;
  CenterValue  : Integer;
begin
 TempValue := 0.5 * Max(SlLineWidth.Value - 1, 0);
 CenterValue := 1 + Ceil(TempValue);
 StatusBar.Panels[0].Text := 'X Pixel: ' + IntToStr(1 + 2 * CenterValue);
 StatusBar.Panels[1].Text := 'Fractional: ' + FloatToString(2 + 0.5 * (FLineWidth - 1) - CenterValue, 4);
end;

end.
