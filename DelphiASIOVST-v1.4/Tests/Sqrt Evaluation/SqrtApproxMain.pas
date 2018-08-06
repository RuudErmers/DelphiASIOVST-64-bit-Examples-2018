unit SqrtApproxMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiBaseControl, DAV_GuiGraphXY;

type
  TFmSqrtApproximation = class(TForm)
    GuiGraphXY: TGuiGraphXY;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function EvaluateSqrt(Sender: TObject; X: Double): Double;
    function EvaluateFastSqrt(Sender: TObject; X: Double): Double;
  public
    { Public-Deklarationen }
  end;

var
  FmSqrtApproximation: TFmSqrtApproximation;

implementation

uses
  Math, DAV_Approximations;

{$R *.dfm}

{ TForm1 }

procedure TFmSqrtApproximation.FormCreate(Sender: TObject);
begin
 TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series).OnEvaluate := EvaluateSqrt;
 TGuiGraphXYFunctionSeries(GuiGraphXY[1].Series).OnEvaluate := EvaluateFastSqrt;
 GuiGraphXY.XAxis.Minimum := 0;
end;

procedure TFmSqrtApproximation.FormShow(Sender: TObject);
begin
 GuiGraphXY.Invalidate;
end;

function TFmSqrtApproximation.EvaluateSqrt(Sender: TObject; X: Double): Double;
begin
 result := Sqrt(X);
end;

function FastExp(X: Double): Double;
var
  Error : Single;
begin
// result := FastSqrtMinError2(x);
 result := FastSqrt(x);
// result := sqrt(x) - result;
end;

function TFmSqrtApproximation.EvaluateFastSqrt(Sender: TObject; X: Double): Double;
begin
 result := FastExp(X);
end;

end.
