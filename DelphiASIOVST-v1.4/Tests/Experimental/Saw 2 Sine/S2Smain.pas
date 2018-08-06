unit S2Smain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_DspWaveshaper, DAV_GuiBaseControl, DAV_GuiGraphXY,
  DAV_DifferentialEvolution;

type
  TDriver = class(TThread)
  private
    { Private-Deklarationen }
  protected
    procedure Execute; override;
  end;

  TFmMain = class(TForm)
    GuiGraphXY: TGuiGraphXY;
    BtOptimize: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtOptimizeClick(Sender: TObject);
  private
    FWaveShaper : TChebyshevWaveshaper;
    FDiffEvol   : TDifferentialEvolution;
    function CalcCosts(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
    function SawPlot(Sender: TObject; X: Double): Double;
    function SinePlot(Sender: TObject; X: Double): Double;
    function ApproximatedSinePlot(Sender: TObject; X: Double): Double;
  public
  end;

var
  FmMain: TFmMain;

implementation

{$R *.dfm}

{ TDriver }

procedure TDriver.Execute;
begin
  { Thread-Code hier einfügen }
end;

{ TFmMain }

procedure TFmMain.FormCreate(Sender: TObject);
var
  OptIndex : Integer;
begin
 FWaveShaper := TChebyshevWaveshaper.Create;
 with FWaveShaper do
  begin
   Order := 7;
   Gain[0] := 1;
   Gain[2] := -0.25;
   Gain[4] := -0.1;
  end;

 FDiffEvol := TDifferentialEvolution.Create(Self);
 with FDiffEvol do
  begin
   VariableCount := FWaveShaper.Order div 2;
   for OptIndex := 0 to VariableCount - 1 do
    begin
     MinArr[0] := -1;
     MaxArr[0] := +1;
    end;
   OnCalcCosts := CalcCosts;
   Initialize;
  end;

 TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series).OnEvaluate := SawPlot;
 TGuiGraphXYFunctionSeries(GuiGraphXY[1].Series).OnEvaluate := SinePlot;
 TGuiGraphXYFunctionSeries(GuiGraphXY[2].Series).OnEvaluate := ApproximatedSinePlot;
 GuiGraphXY.Invalidate;
end;

procedure TFmMain.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FDiffEvol);
 FreeAndNil(FWaveShaper);
end;

procedure TFmMain.BtOptimizeClick(Sender: TObject);
var
  Index    : Integer;
begin
 with FDiffEvol do
  begin
   repeat
    Evolve;
    for Index := 0 to Length(GetBestPopulation) - 1
     do FWaveShaper.Gain[2 * Index] := GetBestPopulation[Index];
    GuiGraphXY.Invalidate;
    Application.ProcessMessages;
   until False;
  end;
end;

function TFmMain.CalcCosts(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
var
  Position : Single;
  Advance  : Single;
  Current  : Double;
  Index    : Integer;
begin
 Result := 0;
 Advance := 0.01;
 Position := Advance;
 for Index := 0 to Length(Population) - 1
  do FWaveShaper.Gain[2 * Index] := Population[Index];

 while Position < 1 do
  begin
   Current := Abs(FWaveShaper.ProcessSample64(Position) / Sin(0.5 * Pi * Position) - 1);
//   Current := Abs(FWaveShaper.ProcessSample64(Position) - Sin(0.5 * Pi * Position));
   if Current > Result
    then Result := Current;

   Position := Position + Advance;
  end;
end;

function TFmMain.SawPlot(Sender: TObject; X: Double): Double;
begin
 Result := X;
end;

function TFmMain.ApproximatedSinePlot(Sender: TObject; X: Double): Double;
begin
(*
 if X <> 0
  then Result := FWaveShaper.ProcessSample64(X) / X - 1
  else Result := 0;
*)

 Result := FWaveShaper.ProcessSample64(X)
end;

function TFmMain.SinePlot(Sender: TObject; X: Double): Double;
begin
 Result := Sin(0.5 * Pi * X);
end;

end.
