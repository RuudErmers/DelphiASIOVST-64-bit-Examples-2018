unit AnalyserForm;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Spin, Math, TeeProcs, 
  TeEngine, Chart, Series, DAV_Types, DAV_Complex, DAV_ASIOHost;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
      630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
      10000, 12500, 16000, 20000);

type
  TFmAnalyser = class(TForm)
    AnalyserChart: TChart;
    ASIOHost: TASIOHost;
    BarSeries: TBarSeries;
    BtAnalyse: TButton;
    BtControlPanel: TButton;
    CbChannel: TComboBox;
    CbDriver: TComboBox;
    LbChannels: TLabel;
    LbFullScaleUnit: TLabel;
    LbDriverName: TLabel;
    LbFullscale: TLabel;
    LbSpeed: TLabel;
    RbFast: TRadioButton;
    RbMedium: TRadioButton;
    RbSlow: TRadioButton;
    SEFullscaleGain: TSpinEdit;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AnalyserChartDblClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtAnalyseClick(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
    procedure RbFastClick(Sender: TObject);
    procedure RbMediumClick(Sender: TObject);
    procedure RbSlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FMagnitudes     : Array [0..cNumFrequencies - 1] of Double;
    FThirdOctaveExp : Array [0..cNumFrequencies - 1] of TComplex32;
    FSpeedConst     : Array [0..1] of Single;
    FChannelNr      : Integer;
    FSampleRateReci : Double;
    FFSGain         : Single;
    FIniFile        : TFileName;
    FBuffer         : PDAVSingleFixedArray;  // the Buffer
    FBufferSize     : Integer;               // Buffer size
    FBufferPosition : Integer;               // position within the Buffer
    FBufferOverlap  : Integer;               // overlap in samples
    procedure DoGoertzelMagic;
    procedure CalculateWeight;
    procedure CalculateComplexAngulars;
  end;

var
  FmAnalyser: TFmAnalyser;

implementation

{$R *.DFM}

uses
  Inifiles, Registry, DAV_Common, DAV_Math, DAV_ASIOConvert, DAV_DspDft;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var
  Band : Integer;
begin
 FChannelNr := 0;
 FSpeedConst[0] := 0.99;
 CalculateWeight;

 FBufferPosition := 0;
 FBufferOverlap  := 15 * 1024;
 FBufferSize     := 16 * 1024;
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));

 FFSGain := SEFullscaleGain.Value;
 FSampleRateReci := 1 / ASIOHost.SampleRate;
 CbDriver.Items := ASIOHost.DriverList;

 if CbDriver.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // set absolute ini file 
 FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   CbDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if CbDriver.ItemIndex >= 0 then CbDriverChange(CbDriver);
   CbChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   SEFullscaleGain.Value := ReadInteger('Audio', 'Fullscale Gain', 0);
  finally
   Free;
  end;

 CalculateComplexAngulars;
 for Band := 0 to CNumFrequencies - 1 do
  begin
   {$IFNDEF FPC}
   if CThirdOctaveFrequencies[Band] < 1000
    then BarSeries.Add(0,FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz')
    else BarSeries.Add(0,FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz');
   {$ELSE}
   if Frequency < 1000
    then AnalyserChart.AddBar(FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz', 0, $000000FF)
    else AnalyserChart.AddBar(FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz', 0, $000000FF);
   {$ENDIF}
  end;

 ASIOHostSampleRateChanged(Sender);
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
begin
 ASIOHost.Active := False;
 with TIniFile.Create(FIniFile) do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', CbDriver.ItemIndex);
   WriteInteger('Audio', 'Channels', CbChannel.ItemIndex);
   WriteInteger('Audio', 'Fullscale Gain', SEFullscaleGain.Value);
  finally
   Free;
  end;
 Dispose(FBuffer);
end;

procedure TFmAnalyser.CalculateComplexAngulars;
var
  Band : Integer;
begin
 for Band := 0 to CNumFrequencies - 1
  do GetSinCos(CThirdOctaveFrequencies[Band] * FSampleRateReci, FThirdOctaveExp[Band].Im, FThirdOctaveExp[Band].Re);
end;

procedure TFmAnalyser.RbFastClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.9;
 CalculateWeight;
end;

procedure TFmAnalyser.RbMediumClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.99;
 CalculateWeight;
end;

procedure TFmAnalyser.RbSlowClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.999;
 CalculateWeight;
end;

procedure TFmAnalyser.CalculateWeight;
begin
 FSpeedConst[1] := 0.5 * (1 - FSpeedConst[0]);
end;

procedure TFmAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 FFSGain := SEFullscaleGain.Value;
// AnalyserChart.LeftAxis.Maximum := FFSGain + 20;
end;

procedure TFmAnalyser.CbDriverChange(Sender: TObject);
var
  i : Integer;
begin
 BtControlPanel.Enabled := False;
 BtAnalyse.Enabled := False;
 CbDriver.ItemIndex := CbDriver.Items.IndexOf(CbDriver.Text);
 if CbDriver.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := CbDriver.ItemIndex;
   CbChannel.Clear;
   for i := 0 to ASIOHost.InputChannelCount - 1
    do CbChannel.Items.Add(ASIOHost.InputChannelInfos[i].name);
   with TIniFile.Create(FIniFile) do
    try
     WriteInteger('Audio', 'Asio Driver', CbDriver.ItemIndex);
    finally
     Free;
    end;
   BtControlPanel.Enabled := True;
   BtAnalyse.Enabled := True;
   CbChannel.ItemIndex := 0;
  end;
end;

procedure TFmAnalyser.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAnalyser.BtAnalyseClick(Sender: TObject);
begin
 if BtAnalyse.Caption = 'Analyse' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtAnalyse.Caption := 'Stop';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtAnalyse.Caption := 'Analyse';
  end;
 Timer.Enabled := ASIOHost.Active;
end;

procedure TFmAnalyser.ASIOHostSampleRateChanged(Sender: TObject);
begin
 FSampleRateReci := 1 / ASIOHost.SampleRate;
 CalculateComplexAngulars;
end;

procedure TFmAnalyser.BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;
 repeat
  if FBufferPosition + (ASIOHost.BufferSize - CurrentPosition) < FBufferSize then
   begin
    Move(InBuffer[0, CurrentPosition],
         FBuffer[FBufferPosition],
         (ASIOHost.BufferSize - CurrentPosition) * Sizeof(Single));
    FBufferPosition  := FBufferPosition + (ASIOHost.BufferSize - CurrentPosition);
    CurrentPosition := ASIOHost.BufferSize;
   end
  else
   begin
    Move(InBuffer[0, CurrentPosition],
         FBuffer[FBufferPosition],
         (FBufferSize - FBufferPosition) * Sizeof(Single));

    DoGoertzelMagic; // do Processing here!

    Move(FBuffer[FBufferSize - FBufferOverlap],
         FBuffer[0], FBufferOverlap * Sizeof(Single));

    CurrentPosition := CurrentPosition + (FBufferSize - FBufferPosition);
    FBufferPosition := FBufferOverlap;
   end;
 until CurrentPosition >= ASIOHost.BufferSize;
end;

procedure TFmAnalyser.DoGoertzelMagic;
var
  i  : Integer;
  bs : Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   bs := round(sqr(sqr(1 - CThirdOctaveFrequencies[i] * FSampleRateReci)) * FBufferSize);
   with Goertzel(PDAVSingleFixedArray(@FBuffer^[(FBufferSize - bs) div 2]), bs,
                 FThirdOctaveExp[i])
    do FMagnitudes[i] := FSpeedConst[0] * FMagnitudes[i]+
                         FSpeedConst[1] * Amp_to_dB(sqr(Re) + sqr(Im));
  end;
end;

procedure TFmAnalyser.AnalyserChartDblClick(Sender: TObject);
begin
 with AnalyserChart do
  if Align <> alClient
   then Align := alClient
   else
    begin
     Align := alBottom;
     Top := 88;
     Height := Self.ClientHeight - 88;
    end;
end;

procedure TFmAnalyser.TimerTimer(Sender: TObject);
var
  Band : Integer;
begin
 {$IFNDEF FPC}
 for Band := 0 to cNumFrequencies - 1
  do BarSeries.YValue[Band] := FMagnitudes[Band] + FFSGain;
 AnalyserChart.Invalidate;
 {$ELSE}
 for j := 0 to cNumFrequencies - 1
  do TBar(AnalyserChart.Bars.Items[Band]).Value := round(FMagnitudes[Band] + FFSGain);
 AnalyserChart.Invalidate;
 {$ENDIF}
end;

initialization
  Set8087CW(Default8087CW or $3F);

end.
