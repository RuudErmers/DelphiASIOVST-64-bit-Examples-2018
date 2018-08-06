unit ThirdOctaveAnalyserGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, ExtCtrls, TeEngine, Series, Controls, TeeProcs, Chart, StdCtrls, Spin, 
  DAV_Types, DAV_VSTModule, DAV_DspThirdOctaveAnalyserFilter,
  DAV_DspThirdOctaveAnalyser;

type
  TFmThirdOctaveAnalyser = class(TForm)
    LbSpeed: TLabel;
    LbFullScale: TLabel;
    LbFullScaleUnit: TLabel;
    RbFast: TRadioButton;
    RbMedium: TRadioButton;
    RbSlow: TRadioButton;
    SEFullscaleGain: TSpinEdit;
    AnalyserChart: TChart;
    BarSeries: TBarSeries;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure RbFastClick(Sender: TObject);
    procedure RbMediumClick(Sender: TObject);
    procedure RbSlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFSGain : Single;  
  public
    procedure UpdateFullscaleGain;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ThirdOctaveAnalyserDM;

procedure TFmThirdOctaveAnalyser.FormCreate(Sender: TObject);
var
  Band : Integer;
begin
 // add bands
 for Band := 0 to CNumFrequencies - 1 do
  begin
   {$IFNDEF FPC}
   if CThirdOctaveFrequencies[Band] < 1000
    then BarSeries.Add(0, FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz')
    else BarSeries.Add(0, FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz');
   {$ELSE}
   if Frequency < 1000
    then AnalyserChart.AddBar(FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz', 0, $000000FF)
    else AnalyserChart.AddBar(FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz', 0, $000000FF);
   {$ENDIF}
  end;
end;

procedure TFmThirdOctaveAnalyser.FormShow(Sender: TObject);
begin
 Timer.Enabled := True;
end;

procedure TFmThirdOctaveAnalyser.RbFastClick(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[0] := 0.99;
  end;
end;

procedure TFmThirdOctaveAnalyser.RbMediumClick(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[0] := 0.999;
  end;
end;

procedure TFmThirdOctaveAnalyser.RbSlowClick(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[0] := 0.9999;
  end;
end;

procedure TFmThirdOctaveAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   Parameter[1] := SEFullscaleGain.Value;
  end;
end;

procedure TFmThirdOctaveAnalyser.UpdateFullscaleGain;
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   if SEFullscaleGain.Value <> Round(Parameter[1])
    then SEFullscaleGain.Value := Round(Parameter[1]);
   FFSGain := Parameter[1];
  end;
end;

procedure TFmThirdOctaveAnalyser.TimerTimer(Sender: TObject);
var
  Band : Integer;
begin
 with TThirdOctaveAnalyserModule(Owner) do
  begin
   {$IFNDEF FPC}
   for Band := 0 to CNumFrequencies - 1
    do BarSeries.YValue[Band] := BandRMS[Band] + FFSGain;
   AnalyserChart.Invalidate;
   {$ELSE}
   for j := 0 to CNumFrequencies - 1
    do TBar(AnalyserChart.Bars.Items[Band]).Value := Round(FFilterArray[Band].RMS + FFSGain);
   AnalyserChart.Invalidate;
   {$ENDIF}
  end;
end;

end.
