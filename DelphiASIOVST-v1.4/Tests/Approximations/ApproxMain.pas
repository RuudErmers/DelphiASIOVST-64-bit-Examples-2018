unit ApproxMain;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, DAV_GuiBaseControl, DAV_GuiGraphXY,
  DAV_GuiCustomControl;

type
  TOneParameterFunction = function (Value: Single): Single;

  TFmApproximationBenchmark = class(TForm)
    GuiGraphXY: TGuiGraphXY;
    MainMenu: TMainMenu;
    Memo: TMemo;
    MiAccuracy: TMenuItem;
    MiAccuracyLog2: TMenuItem;
    MiAccuracyLog2ContError2: TMenuItem;
    MiAccuracyLog2ContError3: TMenuItem;
    MiAccuracyLog2ContError4: TMenuItem;
    MiAccuracyLog2ContError5: TMenuItem;
    MiAccuracyLog2MinError2: TMenuItem;
    MiAccuracyLog2MinError3: TMenuItem;
    MiAccuracyLog2MinError4: TMenuItem;
    MiAccuracyLog2MinError5: TMenuItem;
    MiAccuracyPower2ContError2: TMenuItem;
    MiAccuracyPower2ContError3: TMenuItem;
    MiAccuracyPower2ContError4: TMenuItem;
    MiAccuracyPower2ContError5: TMenuItem;
    MiAccuracyPower2MinError2: TMenuItem;
    MiAccuracyPower2MinError3: TMenuItem;
    MiAccuracyPower2MinError4: TMenuItem;
    MiAccuracyPower2MinError5: TMenuItem;
    MIBenchmark: TMenuItem;
    MICos: TMenuItem;
    MiCosAll: TMenuItem;
    MICosInBounds4Term: TMenuItem;
    MICosInBounds5Term: TMenuItem;
    MICosInBounds6Term: TMenuItem;
    MIExit: TMenuItem;
    MIExp: TMenuItem;
    MiExpAll: TMenuItem;
    MIExpContError2Term: TMenuItem;
    MIExpContError3Term: TMenuItem;
    MIExpContError4Term: TMenuItem;
    MIExpContError5Term: TMenuItem;
    MIExpMinError2Term: TMenuItem;
    MIExpMinError3Term: TMenuItem;
    MIExpMinError4Term: TMenuItem;
    MIExpMinError5Term: TMenuItem;
    MIFastCos3Term: TMenuItem;
    MIFastCos4Term: TMenuItem;
    MIFastCos5Term: TMenuItem;
    MIFastCos6Term: TMenuItem;
    MIFile: TMenuItem;
    MIInBounds3Term: TMenuItem;
    MILn: TMenuItem;
    MiLnAll: TMenuItem;
    MILnContError2Term: TMenuItem;
    MILnContError3Term: TMenuItem;
    MILnContError4Term: TMenuItem;
    MILnContError5Term: TMenuItem;
    MILnMinError2Term: TMenuItem;
    MILnMinError3Term: TMenuItem;
    MILnMinError4Term: TMenuItem;
    MILnMinError5Term: TMenuItem;
    MILog2: TMenuItem;
    MiLog2All: TMenuItem;
    MILog2ContError2Term: TMenuItem;
    MILog2ContError3Term: TMenuItem;
    MILog2ContError4Term: TMenuItem;
    MILog2ContError5Term: TMenuItem;
    MILog2MinError2Term: TMenuItem;
    MILog2MinError3Term: TMenuItem;
    MILog2MinError4Term: TMenuItem;
    MILog2MinError5Term: TMenuItem;
    MIPower2: TMenuItem;
    MiPower2All: TMenuItem;
    MIPower2ContError2Term: TMenuItem;
    MIPower2ContError3Term: TMenuItem;
    MIPower2ContError4Term: TMenuItem;
    MIPower2ContError5Term: TMenuItem;
    MIPower2MinError2Term: TMenuItem;
    MIPower2MinError3Term: TMenuItem;
    MIPower2MinError4Term: TMenuItem;
    MIPower2MinError5Term: TMenuItem;
    MISaveLog: TMenuItem;
    MISin: TMenuItem;
    MiSinAll: TMenuItem;
    MISinComplete3Term: TMenuItem;
    MISinComplete4Term: TMenuItem;
    MISinComplete5Term: TMenuItem;
    MISinComplete6Term: TMenuItem;
    MISinInbounds3Term: TMenuItem;
    MISinInbounds4Term: TMenuItem;
    MISinInbounds5Term: TMenuItem;
    MISinInbounds6Term: TMenuItem;
    MITan: TMenuItem;
    MiTanAll: TMenuItem;
    MITanComplete2Term: TMenuItem;
    MITanComplete3Term: TMenuItem;
    MITanComplete4Term: TMenuItem;
    MITanComplete6Term: TMenuItem;
    MITanh: TMenuItem;
    MiTanhAll: TMenuItem;
    MITanhComplete3Term: TMenuItem;
    MITanhComplete4Term: TMenuItem;
    MITanhComplete5Term: TMenuItem;
    MITanhComplete6Term: TMenuItem;
    MITanhComplete7Term: TMenuItem;
    MITanhExp2TermContError: TMenuItem;
    MITanhExp2TermMinError: TMenuItem;
    MITanhExp3TermContError: TMenuItem;
    MITanhExp3TermMinError: TMenuItem;
    MITanhExp4TermContError: TMenuItem;
    MITanhExp4TermMinError: TMenuItem;
    MITanhExp5TermContError: TMenuItem;
    MITanhExp5TermMinError: TMenuItem;
    MITanhRationalPolynom3TermFPU: TMenuItem;
    MITanhRationalPolynom4TermFPU: TMenuItem;
    MITanhRationalPolynom5TermFPU: TMenuItem;
    MITanhRationalPolynom6TermFPU: TMenuItem;
    MITanhRationalPolynom7TermFPU: TMenuItem;
    MITanInbounds2Term: TMenuItem;
    MITanInbounds3Term: TMenuItem;
    MITanInbounds4Term: TMenuItem;
    MITanInbounds6Term: TMenuItem;
    N1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N2: TMenuItem;
    N20: TMenuItem;
    N2x1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    Splitter: TSplitter;
    procedure MemoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function Evaluation(Sender: TObject; X: Double): Double;
    procedure MiAccuracyLog2Click(Sender: TObject);
    procedure MiAccuracyPower2MinErrorClick(Sender: TObject);
    procedure MiCosAllClick(Sender: TObject);
    procedure MICosClick(Sender: TObject);
    procedure MICosInBounds3TermClick(Sender: TObject);
    procedure MICosInBounds4TermClick(Sender: TObject);
    procedure MICosInBounds5TermClick(Sender: TObject);
    procedure MICosInBounds6TermClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MiExpAllClick(Sender: TObject);
    procedure MIExpClick(Sender: TObject);
    procedure MIExpContError2TermClick(Sender: TObject);
    procedure MIExpContError3TermClick(Sender: TObject);
    procedure MIExpContError4TermClick(Sender: TObject);
    procedure MIExpContError5TermClick(Sender: TObject);
    procedure MIExpMinError2TermClick(Sender: TObject);
    procedure MIExpMinError3TermClick(Sender: TObject);
    procedure MIExpMinError4TermClick(Sender: TObject);
    procedure MIExpMinError5TermClick(Sender: TObject);
    procedure MIFastCos3TermClick(Sender: TObject);
    procedure MIFastCos4TermClick(Sender: TObject);
    procedure MIFastCos5TermClick(Sender: TObject);
    procedure MIFastCos6TermClick(Sender: TObject);
    procedure MiLnAllClick(Sender: TObject);
    procedure MILnClick(Sender: TObject);
    procedure MILnContError2TermClick(Sender: TObject);
    procedure MILnContError3TermClick(Sender: TObject);
    procedure MILnContError4TermClick(Sender: TObject);
    procedure MILnContError5TermClick(Sender: TObject);
    procedure MILnMinError2TermClick(Sender: TObject);
    procedure MILnMinError3TermClick(Sender: TObject);
    procedure MILnMinError4TermClick(Sender: TObject);
    procedure MILnMinError5TermClick(Sender: TObject);
    procedure MiLog2AllClick(Sender: TObject);
    procedure MILog2Click(Sender: TObject);
    procedure MILog2ContError2TermClick(Sender: TObject);
    procedure MILog2ContError3TermClick(Sender: TObject);
    procedure MILog2ContError4TermClick(Sender: TObject);
    procedure MILog2ContError5TermClick(Sender: TObject);
    procedure MILog2MinError2TermClick(Sender: TObject);
    procedure MILog2MinError3TermClick(Sender: TObject);
    procedure MILog2MinError4TermClick(Sender: TObject);
    procedure MILog2MinError5TermClick(Sender: TObject);
    procedure MiPower2AllClick(Sender: TObject);
    procedure MIPower2Click(Sender: TObject);
    procedure MIPower2ContError2TermClick(Sender: TObject);
    procedure MIPower2ContError3TermClick(Sender: TObject);
    procedure MIPower2ContError4TermClick(Sender: TObject);
    procedure MIPower2ContError5TermClick(Sender: TObject);
    procedure MIPower2MinError2TermClick(Sender: TObject);
    procedure MIPower2MinError3TermClick(Sender: TObject);
    procedure MIPower2MinError4TermClick(Sender: TObject);
    procedure MIPower2MinError5TermClick(Sender: TObject);
    procedure MISaveLogClick(Sender: TObject);
    procedure MiSinAllClick(Sender: TObject);
    procedure MISinClick(Sender: TObject);
    procedure MISinComplete3TermClick(Sender: TObject);
    procedure MISinComplete4TermClick(Sender: TObject);
    procedure MISinComplete5TermClick(Sender: TObject);
    procedure MISinComplete6TermClick(Sender: TObject);
    procedure MISinInbounds3TermClick(Sender: TObject);
    procedure MISinInbounds4TermClick(Sender: TObject);
    procedure MISinInbounds5TermClick(Sender: TObject);
    procedure MISinInbounds6TermClick(Sender: TObject);
    procedure MiTanAllClick(Sender: TObject);
    procedure MITanClick(Sender: TObject);
    procedure MITanComplete2TermClick(Sender: TObject);
    procedure MITanComplete3TermClick(Sender: TObject);
    procedure MITanComplete4TermClick(Sender: TObject);
    procedure MITanComplete6TermClick(Sender: TObject);
    procedure MiTanhAllClick(Sender: TObject);
    procedure MITanhClick(Sender: TObject);
    procedure MITanhComplete3TermClick(Sender: TObject);
    procedure MITanhComplete4TermClick(Sender: TObject);
    procedure MITanhComplete5TermClick(Sender: TObject);
    procedure MITanhComplete6TermClick(Sender: TObject);
    procedure MITanhComplete7TermClick(Sender: TObject);
    procedure MITanhExp2TermContErrorClick(Sender: TObject);
    procedure MITanhExp2TermMinErrorClick(Sender: TObject);
    procedure MITanhExp3TermContErrorClick(Sender: TObject);
    procedure MITanhExp3TermMinErrorClick(Sender: TObject);
    procedure MITanhExp4TermContErrorClick(Sender: TObject);
    procedure MITanhExp4TermMinErrorClick(Sender: TObject);
    procedure MITanhExp5TermContErrorClick(Sender: TObject);
    procedure MITanhExp5TermMinErrorClick(Sender: TObject);
    procedure MITanhRationalPolynom3TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom4TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom5TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom6TermFPUClick(Sender: TObject);
    procedure MITanhRationalPolynom7TermFPUClick(Sender: TObject);
    procedure MITanInbounds2TermClick(Sender: TObject);
    procedure MITanInbounds3TermClick(Sender: TObject);
    procedure MITanInbounds4TermClick(Sender: TObject);
    procedure MITanInbounds6TermClick(Sender: TObject);
  protected
    function EvaluateCosine(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine3(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine4(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine5(Sender: TObject; X: Double): Double;
    function EvaluateFastCosine6(Sender: TObject; X: Double): Double;
    function EvaluateSine(Sender: TObject; X: Double): Double;
    function EvaluateFastSine3(Sender: TObject; X: Double): Double;
    function EvaluateFastSine4(Sender: TObject; X: Double): Double;
    function EvaluateFastSine5(Sender: TObject; X: Double): Double;
    function EvaluateFastSine6(Sender: TObject; X: Double): Double;
    function EvaluateTan(Sender: TObject; X: Double): Double;
    function EvaluateFastTan2(Sender: TObject; X: Double): Double;
    function EvaluateFastTan3(Sender: TObject; X: Double): Double;
    function EvaluateFastTan4(Sender: TObject; X: Double): Double;
    function EvaluateFastTan6(Sender: TObject; X: Double): Double;
    function EvaluateTanh(Sender: TObject; X: Double): Double;
    function EvaluateFastTanhOpt3(Sender: TObject; X: Double): Double;
    function EvaluateFastTanhOpt4(Sender: TObject; X: Double): Double;
    function EvaluateFastTanhOpt5(Sender: TObject; X: Double): Double;
    function EvaluateFastTanhOpt6(Sender: TObject; X: Double): Double;
    function EvaluateFastTanhOpt7(Sender: TObject; X: Double): Double;
    function EvaluateLn(Sender: TObject; X: Double): Double;
    function EvaluateLog2(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2MinError2(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2MinError3(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2MinError4(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2MinError5(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2ContinousError2(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2ContinousError3(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2ContinousError4(Sender: TObject; X: Double): Double;
    function EvaluateFastLog2ContinousError5(Sender: TObject; X: Double): Double;
    function EvaluatePower2(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2MinError2(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2MinError3(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2MinError4(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2MinError5(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2ContinousError2(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2ContinousError3(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2ContinousError4(Sender: TObject; X: Double): Double;
    function EvaluateFastPower2ContinousError5(Sender: TObject; X: Double): Double;
    function EvaluateExp(Sender: TObject; X: Double): Double;
  end;

var
  FmApproximationBenchmark: TFmApproximationBenchmark;

implementation

uses
  Math, DAV_Common, DAV_Approximations;

{$R *.dfm}

procedure TFmApproximationBenchmark.FormShow(Sender: TObject);
begin
// MemoClick(Sender);
 TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series).OnEvaluate := Evaluation;
end;

procedure TFmApproximationBenchmark.MIExitClick(Sender: TObject);
begin
 Close;
end;


function EvaluateFunction(OneParameterFunction: TOneParameterFunction): Single;
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // evaluate performance for cos(x)
 Temp := 1 / TestLength;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   OneParameterFunction(i * Temp);
   OneParameterFunction(i * Temp);
   OneParameterFunction(i * Temp);
   OneParameterFunction(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Result := (B - A) / C * 1000;
end;


// Evaluation

function TFmApproximationBenchmark.Evaluation(Sender: TObject; X: Double): Double;
var
  Value : Single;
begin
 // Result := Amp_to_dB(abs(x)) - FastAmpTodBMinError5(abs(x));
 // Result := Amp_to_dB(abs(x)) - FastAmptodBContinousError5(abs(x));
 Value := x;
 Result := dB_to_Amp(x) - FastdBtoAmpMinError3(Value);
end;

function TFmApproximationBenchmark.EvaluateCosine(Sender: TObject; X: Double): Double;
begin
 Result := Cos(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine3(Sender: TObject; X: Double): Double;
begin
 Result := FastCos3Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine4(Sender: TObject; X: Double): Double;
begin
 Result := FastCos4Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine5(Sender: TObject; X: Double): Double;
begin
 Result := FastCos5Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastCosine6(Sender: TObject; X: Double): Double;
begin
 Result := FastCos6Term(x);
end;

function TFmApproximationBenchmark.EvaluateSine(Sender: TObject; X: Double): Double;
begin
 Result := Sin(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine3(Sender: TObject; X: Double): Double;
begin
 Result := FastSin3Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine4(Sender: TObject; X: Double): Double;
begin
 Result := FastSin4Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine5(Sender: TObject; X: Double): Double;
begin
 Result := FastSin5Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastSine6(Sender: TObject; X: Double): Double;
begin
 Result := FastSin6Term(x);
end;

function TFmApproximationBenchmark.EvaluateTan(Sender: TObject; X: Double): Double;
begin
 Result := Tan(x);
end;

function TFmApproximationBenchmark.EvaluateFastTan2(Sender: TObject; X: Double): Double;
begin
 Result := FastTan2Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTan3(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTan3Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTan4(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTan4Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTan6(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTan6Term(x);
end;

function TFmApproximationBenchmark.EvaluateTanh(Sender: TObject;
  X: Double): Double;
begin
 Result := Tanh(x);
end;

function TFmApproximationBenchmark.EvaluateFastTanhOpt3(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTanhOpt3Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTanhOpt4(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTanhOpt4Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTanhOpt5(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTanhOpt5Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTanhOpt6(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTanhOpt6Term(x);
end;

function TFmApproximationBenchmark.EvaluateFastTanhOpt7(Sender: TObject;
  X: Double): Double;
begin
 Result := FastTanhOpt7Term(x);
end;

function TFmApproximationBenchmark.EvaluatePower2(Sender: TObject;
  X: Double): Double;
begin
 Result := Power(2, x);
end;

function TFmApproximationBenchmark.EvaluateLn(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := Ln(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateLog2(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := Log2(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2MinError2(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2MinError2(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2MinError3(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2MinError3(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2MinError4(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2MinError4(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2MinError5(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2MinError5(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastPower2ContinousError2(
  Sender: TObject; X: Double): Double;
begin
 Result := FastPower2ContinousError2(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2ContinousError3(
  Sender: TObject; X: Double): Double;
begin
 Result := FastPower2ContinousError3(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2ContinousError4(
  Sender: TObject; X: Double): Double;
begin
 Result := FastPower2ContinousError4(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2ContinousError5(
  Sender: TObject; X: Double): Double;
begin
 Result := FastPower2ContinousError5(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2MinError2(Sender: TObject;
  X: Double): Double;
begin
 Result := FastPower2MinError2(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2MinError3(Sender: TObject;
  X: Double): Double;
begin
 Result := FastPower2MinError3(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2MinError4(Sender: TObject;
  X: Double): Double;
begin
 Result := FastPower2MinError4(x);
end;

function TFmApproximationBenchmark.EvaluateFastPower2MinError5(Sender: TObject;
  X: Double): Double;
begin
 Result := FastPower2MinError5(x);
end;

function TFmApproximationBenchmark.EvaluateFastLog2ContinousError2(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2ContinousError2(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2ContinousError3(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2ContinousError3(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2ContinousError4(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2ContinousError4(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateFastLog2ContinousError5(Sender: TObject;
  X: Double): Double;
begin
 if x > 0
  then Result := FastLog2ContinousError5(x)
  else Result := -Infinity;
end;

function TFmApproximationBenchmark.EvaluateExp(Sender: TObject;
  X: Double): Double;
begin
 Result := Exp(x);
end;


// Reference

procedure TFmApproximationBenchmark.MICosClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Cos() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');
 Application.ProcessMessages;

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateCosine;

 GuiGraphXY.YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
 GuiGraphXY.YAxis.SetBounds(-1, 1);
 GuiGraphXY.UpdateGraph;

 // evaluate performance for cos(x)
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Cos(i * Temp);
   Cos(i * Temp);
   Cos(i * Temp);
   Cos(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Cos(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Cos() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');
 Application.ProcessMessages;

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateSine;

 GuiGraphXY.YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
 GuiGraphXY.YAxis.SetBounds(-1, 1);
 GuiGraphXY.UpdateGraph;

 // evaluate performance for cos(x)
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Sin(i * Temp);
   Sin(i * Temp);
   Sin(i * Temp);
   Sin(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Sin(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateTan;

 GuiGraphXY.YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
 GuiGraphXY.YAxis.SetBounds(-1, 1);
 GuiGraphXY.UpdateGraph;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Tan(i * Temp);
   Tan(i * Temp);
   Tan(i * Temp);
   Tan(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Tan(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateTanh;

 GuiGraphXY.YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
 GuiGraphXY.YAxis.SetBounds(-1, 1);
 GuiGraphXY.UpdateGraph;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Tanh(i * Temp);
   Tanh(i * Temp);
   Tanh(i * Temp);
   Tanh(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Tanh(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateLn;

 with GuiGraphXY do
  begin
   XAxis.SetBounds(0.01, 5);
   YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
   YAxis.SetBounds(-10, 3);
   UpdateGraph;
  end;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Ln(i * Temp);
   Ln(i * Temp);
   Ln(i * Temp);
   Ln(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Ln(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2Click(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluatePower2;

 with GuiGraphXY do
  begin
   YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
   YAxis.SetBounds(0, 4);
   XAxis.SetBounds(-5, 3);
   UpdateGraph;
  end;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Power(2, i * Temp);
   Power(2, i * Temp);
   Power(2, i * Temp);
   Power(2, i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Power(2, x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateExp;

 with GuiGraphXY do
  begin
   YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
   YAxis.SetBounds(0, 4);
   XAxis.SetBounds(-5, 3);
   UpdateGraph;
  end;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Exp(i * Temp);
   Exp(i * Temp);
   Exp(i * Temp);
   Exp(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Exp(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2Click(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Clear;
 Memo.Lines.Add('Benchmark started');

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series)
   do OnEvaluate := EvaluateLog2;

 with GuiGraphXY do
  begin
   XAxis.SetBounds(0.01, 5);
   YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
   YAxis.SetBounds(-10, 3);
   UpdateGraph;
  end;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Log2(i * Temp);
   Log2(i * Temp);
   Log2(i * Temp);
   Log2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Log2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

// Approximations

procedure TFmApproximationBenchmark.MIExpContError2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastExpContinousError2(x)
 Temp := EvaluateFunction(FastExpContinousError2);
 Memo.Lines.Add('Reference: FastExpContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpContError3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastExpContinousError3(x)
 Temp := EvaluateFunction(FastExpContinousError3);
 Memo.Lines.Add('Reference: FastExpContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpContError4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastExpContinousError4(x)
 Temp := EvaluateFunction(FastExpContinousError4);
 Memo.Lines.Add('Reference: FastExpContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpContError5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastExpContinousError5(x)
 Temp := EvaluateFunction(FastExpContinousError5);
 Memo.Lines.Add('Reference: FastExpContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiExpAllClick(Sender: TObject);
var
  Temp : Single;
begin
 MIExpClick(Sender);

 Temp := EvaluateFunction(FastExpMinError2);
 Memo.Lines.Add('Reference: FastExpMinError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpMinError3);
 Memo.Lines.Add('Reference: FastExpMinError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpMinError4);
 Memo.Lines.Add('Reference: FastExpMinError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpMinError5);
 Memo.Lines.Add('Reference: FastExpMinError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpContinousError2);
 Memo.Lines.Add('Reference: FastExpContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpContinousError3);
 Memo.Lines.Add('Reference: FastExpContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpContinousError4);
 Memo.Lines.Add('Reference: FastExpContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastExpContinousError5);
 Memo.Lines.Add('Reference: FastExpContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIExpClick(Sender);

 // evaluate performance for FastExpMinError2(x)
 Temp := EvaluateFunction(FastExpMinError2);
 Memo.Lines.Add('Reference: FastExpMinError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError3TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MIExpClick(Sender);

 // evaluate performance for FastExpMinError3(x)
 Temp := EvaluateFunction(FastExpMinError3);
 Memo.Lines.Add('Reference: FastExpMinError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError4TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MIExpClick(Sender);

 // evaluate performance for FastExpMinError4(x)
 Temp := EvaluateFunction(FastExpMinError4);
 Memo.Lines.Add('Reference: FastExpMinError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIExpMinError5TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MIExpClick(Sender);

 // evaluate performance for FastExpMinError5(x)
 Temp := EvaluateFunction(FastExpMinError5);
 Memo.Lines.Add('Reference: FastExpMinError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISaveLogClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   Filter := 'Text (*.txt)|*.txt';
   DefaultExt := 'txt';
   if Execute then
    begin
     Memo.Lines.SaveToFile(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmApproximationBenchmark.MILnContError2TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnContinousError2(x)
 Temp := EvaluateFunction(FastLnContinousError2);
 Memo.Lines.Add('Reference: FastLnContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnContError3TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnContinousError3(x)
 Temp := EvaluateFunction(FastLnContinousError3);
 Memo.Lines.Add('Reference: FastLnContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnContError4TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnContinousError4(x)
 Temp := EvaluateFunction(FastLnContinousError4);
 Memo.Lines.Add('Reference: FastLnContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnContError5TermClick(Sender: TObject);
var
  Temp    : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnContinousError5(x)
 Temp := EvaluateFunction(FastLnContinousError5);
 Memo.Lines.Add('Reference: FastLnContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiLnAllClick(Sender: TObject);
var
  Temp : Single;
begin
 MILnClick(Sender);

 Temp := EvaluateFunction(FastLnMinError2);
 Memo.Lines.Add('Reference: FastLnMinError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnMinError3);
 Memo.Lines.Add('Reference: FastLnMinError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnMinError4);
 Memo.Lines.Add('Reference: FastLnMinError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnMinError5);
 Memo.Lines.Add('Reference: FastLnMinError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnContinousError2);
 Memo.Lines.Add('Reference: FastLnContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnContinousError3);
 Memo.Lines.Add('Reference: FastLnContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnContinousError4);
 Memo.Lines.Add('Reference: FastLnContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLnContinousError5);
 Memo.Lines.Add('Reference: FastLnContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnMinError2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnMinError2(x)
 Temp := EvaluateFunction(FastLnMinError2);
 Memo.Lines.Add('Reference: FastLnMinError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnMinError3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnMinError3(x)
 Temp := EvaluateFunction(FastLnMinError3);
 Memo.Lines.Add('Reference: FastLnMinError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnMinError4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnMinError4(x)
 Temp := EvaluateFunction(FastLnMinError4);
 Memo.Lines.Add('Reference: FastLnMinError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILnMinError5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILnClick(Sender);

 // evaluate performance for FastLnMinError5(x)
 Temp := EvaluateFunction(FastLnMinError5);
 Memo.Lines.Add('Reference: FastLnMinError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiLog2AllClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 Temp := EvaluateFunction(FastLog2MinError2);
 Memo.Lines.Add('Reference: FastLog2MinError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2MinError3);
 Memo.Lines.Add('Reference: FastLog2MinError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2MinError4);
 Memo.Lines.Add('Reference: FastLog2MinError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2MinError5);
 Memo.Lines.Add('Reference: FastLog2MinError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2ContinousError2);
 Memo.Lines.Add('Reference: FastLog2ContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2ContinousError3);
 Memo.Lines.Add('Reference: FastLog2ContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2ContinousError4);
 Memo.Lines.Add('Reference: FastLog2ContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastLog2ContinousError5);
 Memo.Lines.Add('Reference: FastLog2ContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiTanAllClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 Temp := EvaluateFunction(FastTan2Term);
 Memo.Lines.Add('Reference: FastTan2Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTan3Term);
 Memo.Lines.Add('Reference: FastTan3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTan4Term);
 Memo.Lines.Add('Reference: FastTan4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTan6Term);
 Memo.Lines.Add('Reference: FastTan6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanInBounds2Term);
 Memo.Lines.Add('Reference: FastTanInBounds2Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanInBounds3Term);
 Memo.Lines.Add('Reference: FastTanInBounds3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanInBounds4Term);
 Memo.Lines.Add('Reference: FastTanInBounds4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanInBounds6Term);
 Memo.Lines.Add('Reference: FastTanInBounds6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanComplete2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastTan2Term(x)
 Temp := EvaluateFunction(FastTan2Term);
 Memo.Lines.Add('Reference: FastTan2Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanComplete3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastTan3Term(x)
 Temp := EvaluateFunction(FastTan3Term);
 Memo.Lines.Add('Reference: FastTan3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanComplete4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastTan4Term(x)
 Temp := EvaluateFunction(FastTan4Term);
 Memo.Lines.Add('Reference: FastTan4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanComplete6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastTan6Term(x)
 Temp := EvaluateFunction(FastTan6Term);
 Memo.Lines.Add('Reference: FastTan6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiTanhAllClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 Temp := EvaluateFunction(FastTanhOpt3Term);
 Memo.Lines.Add('Reference: FastTanhOpt3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt4Term);
 Memo.Lines.Add('Reference: FastTanhOpt4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt5Term);
 Memo.Lines.Add('Reference: FastTanhOpt5Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt6Term);
 Memo.Lines.Add('Reference: FastTanhOpt6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt7Term);
 Memo.Lines.Add('Reference: FastTanhOpt7Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 {$IFNDEF CPU64}
 Temp := EvaluateFunction(FastTanhOpt3TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt3TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt4TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt4TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt5TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt5TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt6Term);
 Memo.Lines.Add('Reference: FastTanhOpt6TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhOpt7Term);
 Memo.Lines.Add('Reference: FastTanhOpt7TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');
 {$ENDIF}

 Temp := EvaluateFunction(FastTanhMinError2);
 Memo.Lines.Add('Reference: FastTanhMinError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhContinousError2);
 Memo.Lines.Add('Reference: FastTanhContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhMinError3);
 Memo.Lines.Add('Reference: FastTanhMinError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhContinousError3);
 Memo.Lines.Add('Reference: FastTanhContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhMinError4);
 Memo.Lines.Add('Reference: FastTanhMinError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhContinousError4);
 Memo.Lines.Add('Reference: FastTanhContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhMinError5);
 Memo.Lines.Add('Reference: FastTanhMinError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastTanhContinousError5);
 Memo.Lines.Add('Reference: FastTanhContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhOpt3Term(x)
 Temp := EvaluateFunction(FastTanhOpt3Term);
 Memo.Lines.Add('Reference: FastTanhOpt3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhOpt4Term(x)
 Temp := EvaluateFunction(FastTanhOpt4Term);
 Memo.Lines.Add('Reference: FastTanhOpt4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhOpt5Term(x)
 Temp := EvaluateFunction(FastTanhOpt5Term);
 Memo.Lines.Add('Reference: FastTanhOpt5Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhOpt6Term(x)
 Temp := EvaluateFunction(FastTanhOpt6Term);
 Memo.Lines.Add('Reference: FastTanhOpt6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhComplete7TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhOpt7Term(x)
 Temp := EvaluateFunction(FastTanhOpt7Term);
 Memo.Lines.Add('Reference: FastTanhOpt7Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp2TermContErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhContinousError2(x)
 Temp := EvaluateFunction(FastTanhContinousError2);
 Memo.Lines.Add('Reference: FastTanhContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp2TermMinErrorClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhMinError2(x)
 Temp := EvaluateFunction(FastTanhMinError2);
 Memo.Lines.Add('Reference: FastTanhMinError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp3TermContErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhContinousError3(x)
 Temp := EvaluateFunction(FastTanhContinousError3);
 Memo.Lines.Add('Reference: FastTanhContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp3TermMinErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastTanhMinError3(x)
 Temp := EvaluateFunction(FastTanhMinError3);
 Memo.Lines.Add('Reference: FastTanhMinError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp4TermContErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhContinousError4);
 Memo.Lines.Add('Reference: FastTanhContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp4TermMinErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhMinError4);
 Memo.Lines.Add('Reference: FastTanhMinError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp5TermContErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhContinousError5);
 Memo.Lines.Add('Reference: FastTanhContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhExp5TermMinErrorClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhMinError5);
 Memo.Lines.Add('Reference: FastTanhMinError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom3TermFPUClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhOpt3TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt3TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom4TermFPUClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhOpt4TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt4TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom5TermFPUClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhOpt5TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt5TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom6TermFPUClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhOpt6TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt6TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanhRationalPolynom7TermFPUClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MITanhClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanhOpt7TermFPU);
 Memo.Lines.Add('Reference: FastTanhOpt7TermFPU(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanInBounds2Term);
 Memo.Lines.Add('Reference: FastTanInBounds2Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanInBounds3Term);
 Memo.Lines.Add('Reference: FastTanInBounds3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanInBounds4Term);
 Memo.Lines.Add('Reference: FastTanInBounds4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MITanInbounds6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MITanClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastTanInBounds6Term);
 Memo.Lines.Add('Reference: FastTanInBounds6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIFastCos3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos3Term(x)
 Temp := EvaluateFunction(FastCos3Term);
 Memo.Lines.Add('Reference: FastCos3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiCosAllClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 Temp := EvaluateFunction(FastCos3Term);
 Memo.Lines.Add('Reference: FastCos3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCos4Term);
 Memo.Lines.Add('Reference: FastCos4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCos5Term);
 Memo.Lines.Add('Reference: FastCos5Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCos6Term);
 Memo.Lines.Add('Reference: FastCos6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCosInBounds3Term);
 Memo.Lines.Add('Reference: FastCosInBounds3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCosInBounds4Term);
 Memo.Lines.Add('Reference: FastCosInBounds4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCosInBounds5Term);
 Memo.Lines.Add('Reference: FastCosInBounds5Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastCosInBounds6Term);
 Memo.Lines.Add('Reference: FastCosInBounds6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIFastCos4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos4Term(x)
 Temp := EvaluateFunction(FastCos4Term);
 Memo.Lines.Add('Reference: FastCos4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIFastCos5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos5Term(x)
 Temp := EvaluateFunction(FastCos5Term);
 Memo.Lines.Add('Reference: FastCos5Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIFastCos6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCos6Term(x)
 Temp := EvaluateFunction(FastCos6Term);
 Memo.Lines.Add('Reference: FastCos6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2MinError2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2MinError2(x)
 Temp := EvaluateFunction(FastLog2MinError2);
 Memo.Lines.Add('Reference: FastLog2MinError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2MinError3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2MinError3(x)
 Temp := EvaluateFunction(FastLog2MinError3);
 Memo.Lines.Add('Reference: FastLog2MinError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2MinError4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2MinError4(x)
 Temp := EvaluateFunction(FastLog2MinError4);
 Memo.Lines.Add('Reference: FastLog2MinError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2MinError5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2MinError5(x)
 Temp := EvaluateFunction(FastLog2MinError5);
 Memo.Lines.Add('Reference: FastLog2MinError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiAccuracyLog2Click(Sender: TObject);
begin
 Memo.Lines.Clear;

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
   case TComponent(Sender).Tag of
    0 : OnEvaluate := EvaluateFastLog2MinError2;
    1 : OnEvaluate := EvaluateFastLog2MinError3;
    2 : OnEvaluate := EvaluateFastLog2MinError4;
    3 : OnEvaluate := EvaluateFastLog2MinError5;
    4 : OnEvaluate := EvaluateFastLog2ContinousError2;
    5 : OnEvaluate := EvaluateFastLog2ContinousError3;
    6 : OnEvaluate := EvaluateFastLog2ContinousError4;
    7 : OnEvaluate := EvaluateFastLog2ContinousError5;
   end;

 with GuiGraphXY do
  begin
   XAxis.SetBounds(0.01, 5);
   YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
   YAxis.SetBounds(-10, 3);
   UpdateGraph;
  end;
end;

procedure TFmApproximationBenchmark.MiAccuracyPower2MinErrorClick(
  Sender: TObject);
begin
 Memo.Lines.Clear;

 if GuiGraphXY[0].Series is TGuiGraphXYFunctionSeries then
  with TGuiGraphXYFunctionSeries(GuiGraphXY[0].Series) do
   case TComponent(Sender).Tag of
    0 : OnEvaluate := EvaluateFastPower2MinError2;
    1 : OnEvaluate := EvaluateFastPower2MinError3;
    2 : OnEvaluate := EvaluateFastPower2MinError4;
    3 : OnEvaluate := EvaluateFastPower2MinError5;
    4 : OnEvaluate := EvaluateFastPower2ContinousError2;
    5 : OnEvaluate := EvaluateFastPower2ContinousError3;
    6 : OnEvaluate := EvaluateFastPower2ContinousError4;
    7 : OnEvaluate := EvaluateFastPower2ContinousError5;
   end;

 with GuiGraphXY do
  begin
   YAxis.Flags := GuiGraphXY.YAxis.Flags + [cafAutoExtendBounds];
   YAxis.SetBounds(0, 4);
   XAxis.SetBounds(-5, 3);
   UpdateGraph;
  end;
end;

procedure TFmApproximationBenchmark.MIPower2ContError2TermClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2ContinousError2(x)
 Temp := EvaluateFunction(FastPower2ContinousError2);
 Memo.Lines.Add('Reference: FastPower2ContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2ContError3TermClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2ContinousError3(x)
 Temp := EvaluateFunction(FastPower2ContinousError3);
 Memo.Lines.Add('Reference: FastPower2ContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2ContError4TermClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2ContinousError4(x)
 Temp := EvaluateFunction(FastPower2ContinousError4);
 Memo.Lines.Add('Reference: FastPower2ContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2ContError5TermClick(
  Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2ContinousError5(x)
 Temp := EvaluateFunction(FastPower2ContinousError5);
 Memo.Lines.Add('Reference: FastPower2ContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiPower2AllClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 Temp := EvaluateFunction(FastPower2MinError2);
 Memo.Lines.Add('Reference: FastPower2MinError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2MinError3);
 Memo.Lines.Add('Reference: FastPower2MinError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2MinError4);
 Memo.Lines.Add('Reference: FastPower2MinError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2MinError5);
 Memo.Lines.Add('Reference: FastPower2MinError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2ContinousError2);
 Memo.Lines.Add('Reference: FastPower2ContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2ContinousError3);
 Memo.Lines.Add('Reference: FastPower2ContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2ContinousError4);
 Memo.Lines.Add('Reference: FastPower2ContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastPower2ContinousError5);
 Memo.Lines.Add('Reference: FastPower2ContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2MinError2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2MinError2(x)
 Temp := EvaluateFunction(FastPower2MinError2);
 Memo.Lines.Add('Reference: FastPower2MinError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2MinError3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2MinError3(x)
 Temp := EvaluateFunction(FastPower2MinError3);
 Memo.Lines.Add('Reference: FastPower2MinError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2MinError4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2MinError4(x)
 Temp := EvaluateFunction(FastPower2MinError4);
 Memo.Lines.Add('Reference: FastPower2MinError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MIPower2MinError5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MIPower2Click(Sender);

 // evaluate performance for FastPower2MinError5(x)
 Temp := EvaluateFunction(FastPower2MinError5);
 Memo.Lines.Add('Reference: FastPower2MinError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError2TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2ContinousError2(x)
 Temp := EvaluateFunction(FastLog2ContinousError2);
 Memo.Lines.Add('Reference: FastLog2ContinousError2(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2ContinousError3(x)
 Temp := EvaluateFunction(FastLog2ContinousError3);
 Memo.Lines.Add('Reference: FastLog2ContinousError3(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2ContinousError4(x)
 Temp := EvaluateFunction(FastLog2ContinousError4);
 Memo.Lines.Add('Reference: FastLog2ContinousError4(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MILog2ContError5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MILog2Click(Sender);

 // evaluate performance for FastLog2ContinousError5(x)
 Temp := EvaluateFunction(FastLog2ContinousError5);
 Memo.Lines.Add('Reference: FastLog2ContinousError5(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCosInBounds3Term(x)
 Temp := EvaluateFunction(FastCosInBounds3Term);
 Memo.Lines.Add('Reference: FastCosInBounds3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCosInBounds4Term(x)
 Temp := EvaluateFunction(FastCosInBounds4Term);
 Memo.Lines.Add('Reference: FastCosInBounds4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCosInBounds5Term(x)
 Temp := EvaluateFunction(FastCosInBounds5Term);
 Memo.Lines.Add('Reference: FastCosInBounds5Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MICosInBounds6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MICosClick(Sender);

 // evaluate performance for FastCosInBounds6Term(x)
 Temp := EvaluateFunction(FastCosInBounds6Term);
 Memo.Lines.Add('Reference: FastCosInBounds6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MiSinAllClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 Temp := EvaluateFunction(FastSin3Term);
 Memo.Lines.Add('Reference: FastSin3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSin4Term);
 Memo.Lines.Add('Reference: FastSin4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSin5Term);
 Memo.Lines.Add('Reference: FastSin5Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSin6Term);
 Memo.Lines.Add('Reference: FastSin6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSinInBounds3Term);
 Memo.Lines.Add('Reference: FastSinInBounds3Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSinInBounds4Term);
 Memo.Lines.Add('Reference: FastSinInBounds4Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSinInBounds5Term);
 Memo.Lines.Add('Reference: FastSinInBounds5Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Temp := EvaluateFunction(FastSinInBounds6Term);
 Memo.Lines.Add('Reference: FastSinInBounds6Term(x): ' + IntToStr(Round(Temp)) + 'ms');

 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin3Term(x)
 Temp := EvaluateFunction(FastSin3Term);
 Memo.Lines.Add('Reference: FastSin3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin4Term(x)
 Temp := EvaluateFunction(FastSin4Term);
 Memo.Lines.Add('Reference: FastSin4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin5Term(x)
 Temp := EvaluateFunction(FastSin5Term);
 Memo.Lines.Add('Reference: FastSin5Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinComplete6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSin6Term(x)
 Temp := EvaluateFunction(FastSin6Term);
 Memo.Lines.Add('Reference: FastSin6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds3TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSinInBounds3Term(x)
 Temp := EvaluateFunction(FastSinInBounds3Term);
 Memo.Lines.Add('Reference: FastSinInBounds3Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds4TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSinInBounds4Term(x)
 Temp := EvaluateFunction(FastSinInBounds4Term);
 Memo.Lines.Add('Reference: FastSinInBounds4Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds5TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSinInBounds5Term(x)
 Temp := EvaluateFunction(FastSinInBounds5Term);
 Memo.Lines.Add('Reference: FastSinInBounds5Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MISinInbounds6TermClick(Sender: TObject);
var
  Temp : Single;
begin
 MISinClick(Sender);

 // evaluate performance for FastSinInBounds6Term(x)
 Temp := EvaluateFunction(FastSinInBounds6Term);
 Memo.Lines.Add('Reference: FastSinInBounds6Term(x): ' + IntToStr(Round(Temp)) + 'ms');
 Application.ProcessMessages;
end;

procedure TFmApproximationBenchmark.MemoClick(Sender: TObject);
var
  i       : Integer;
  A, B, C : Int64;
  Temp    : Single;
const
  TestLength = $100000;
begin
 // Fast Tan() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Tan(i * Temp);
   Tan(i * Temp);
   Tan(i * Temp);
   Tan(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Tan(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan3Term(i * Temp);
   FastTan3Term(i * Temp);
   FastTan3Term(i * Temp);
   FastTan3Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan3Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan4Term(i * Temp);
   FastTan4Term(i * Temp);
   FastTan4Term(i * Temp);
   FastTan4Term(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan4Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

  QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastTan6Term(i * Temp);
   FastTan6Term(i * Temp);
   FastTan6Term(i * Temp);
   FastTan6Term(i * Temp);
 end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastTan6Term(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast Log2() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Log2(i * Temp);
   Log2(i * Temp);
   Log2(i * Temp);
   Log2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Log2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError2(i * Temp);
   FastLog2MinError2(i * Temp);
   FastLog2MinError2(i * Temp);
   FastLog2MinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError3(i * Temp);
   FastLog2MinError3(i * Temp);
   FastLog2MinError3(i * Temp);
   FastLog2MinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError4(i * Temp);
   FastLog2MinError4(i * Temp);
   FastLog2MinError4(i * Temp);
   FastLog2MinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastLog2MinError5(i * Temp);
   FastLog2MinError5(i * Temp);
   FastLog2MinError5(i * Temp);
   FastLog2MinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastLog2MinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast Power2() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Power(2, i * Temp);
   Power(2, i * Temp);
   Power(2, i * Temp);
   Power(2, i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Power(2, x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError2(i * Temp);
   FastPower2MinError2(i * Temp);
   FastPower2MinError2(i * Temp);
   FastPower2MinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError3(i * Temp);
   FastPower2MinError3(i * Temp);
   FastPower2MinError3(i * Temp);
   FastPower2MinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError4(i * Temp);
   FastPower2MinError4(i * Temp);
   FastPower2MinError4(i * Temp);
   FastPower2MinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastPower2MinError5(i * Temp);
   FastPower2MinError5(i * Temp);
   FastPower2MinError5(i * Temp);
   FastPower2MinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastPower2MinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast AmpTodB() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   Amp_To_dB(i * Temp);
   Amp_To_dB(i * Temp);
   Amp_To_dB(i * Temp);
   Amp_To_dB(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: Amp_To_dB(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError2(i * Temp);
   FastAmpTodBMinError2(i * Temp);
   FastAmpTodBMinError2(i * Temp);
   FastAmpTodBMinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError3(i * Temp);
   FastAmpTodBMinError3(i * Temp);
   FastAmpTodBMinError3(i * Temp);
   FastAmpTodBMinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError4(i * Temp);
   FastAmpTodBMinError4(i * Temp);
   FastAmpTodBMinError4(i * Temp);
   FastAmpTodBMinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastAmpTodBMinError5(i * Temp);
   FastAmpTodBMinError5(i * Temp);
   FastAmpTodBMinError5(i * Temp);
   FastAmpTodBMinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastAmpTodBMinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 // Fast dBtoAmp() Test
 Temp := 1 / TestLength;
 Memo.Lines.Add('----------------');
 Application.ProcessMessages;
 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   dB_to_Amp(i * Temp);
   dB_to_Amp(i * Temp);
   dB_to_Amp(i * Temp);
   dB_to_Amp(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: dB_to_Amp(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError2(i * Temp);
   FastdBtoAmpMinError2(i * Temp);
   FastdBtoAmpMinError2(i * Temp);
   FastdBtoAmpMinError2(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError2(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError3(i * Temp);
   FastdBtoAmpMinError3(i * Temp);
   FastdBtoAmpMinError3(i * Temp);
   FastdBtoAmpMinError3(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError3(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError4(i * Temp);
   FastdBtoAmpMinError4(i * Temp);
   FastdBtoAmpMinError4(i * Temp);
   FastdBtoAmpMinError4(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError4(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;

 QueryPerformanceFrequency(C);
 QueryPerformanceCounter(A);
 for i := 1 to TestLength do
  begin
   FastdBtoAmpMinError5(i * Temp);
   FastdBtoAmpMinError5(i * Temp);
   FastdBtoAmpMinError5(i * Temp);
   FastdBtoAmpMinError5(i * Temp);
  end;
 QueryPerformanceCounter(B);
 Memo.Lines.Add('Reference: FastdBtoAmpMinError5(x): ' + IntToStr(round((B - A) / C * 1000)) + 'ms');
 Application.ProcessMessages;
end;

end.
