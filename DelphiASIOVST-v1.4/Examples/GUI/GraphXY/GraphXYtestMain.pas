unit GraphXYtestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiCustomControl, DAV_GuiGraphXY, DAV_GuiPixelMap;

type
  TFmGraphXY = class(TForm)
    GraphXYA: TGuiGraphXY;
    GraphXYB: TGuiGraphXY;
    GraphXYC: TGuiGraphXY;
    GraphXYD: TGuiGraphXY;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    function SimpleFunctionEvaluate(Sender: TObject; X: Double): Double;
  end;

var
  FmGraphXY: TFmGraphXY;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmGraphXY.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
 TGuiGraphXYFunctionSeries(GraphXYA[0].Series).OnEvaluate := SimpleFunctionEvaluate;
 TGuiGraphXYFunctionSeries(GraphXYB[0].Series).OnEvaluate := SimpleFunctionEvaluate;
 TGuiGraphXYFunctionSeries(GraphXYC[0].Series).OnEvaluate := SimpleFunctionEvaluate;
 TGuiGraphXYFunctionSeries(GraphXYD[0].Series).OnEvaluate := SimpleFunctionEvaluate;
end;

procedure TFmGraphXY.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmGraphXY.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmGraphXY.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array[0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLn[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLn[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmGraphXY.FormShow(Sender: TObject);
begin
 GraphXYA.UpdateGraph;
 GraphXYB.UpdateGraph;
 GraphXYC.UpdateGraph;
 GraphXYD.UpdateGraph;
end;

function TFmGraphXY.SimpleFunctionEvaluate(Sender: TObject; X: Double): Double;
begin
 Result := X * sqr(X) * 0.1;
end;

end.
