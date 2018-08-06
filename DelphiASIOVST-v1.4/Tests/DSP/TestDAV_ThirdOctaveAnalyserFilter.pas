unit TestDAV_ThirdOctaveAnalyserFilter;
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
  Classes, TestFramework, DAV_Types, DAV_Classes, DAV_CustomDataContainer,
  DAV_DspFilterChebyshevType1, DAV_DspThirdOctaveAnalyser,
  DAV_DspThirdOctaveAnalyserFilter, DAV_DspSimpleOscillator;

type
  TTestTThirdOctaveAnalyser = class(TTestCase)
  protected
    FThirdOctaveAnalyser: TCustomThirdOctaveAnalyser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample32;
    procedure TestProcessSample64;
  end;

  TTestTThirdOctaveAnalyserFilter = class(TTestTThirdOctaveAnalyser)
  private
    function GetAnalyser: TThirdOctaveAnalyserFilter;
  protected
    property Analyser: TThirdOctaveAnalyserFilter read GetAnalyser;
  public
    procedure SetUp; override;
  end;

  TTestTThirdOctaveAnalyserFilterDownSampling = class(TTestTThirdOctaveAnalyser)
  private
    function GetAnalyser: TThirdOctaveAnalyserFilterDownSampling;
  protected
    property Analyser: TThirdOctaveAnalyserFilterDownSampling read GetAnalyser;
  public
    procedure SetUp; override;
  end;

implementation

uses
  SysUtils;

{ TTestTThirdOctaveAnalyser }

procedure TTestTThirdOctaveAnalyser.SetUp;
begin
  inherited;

end;

procedure TTestTThirdOctaveAnalyser.TearDown;
begin
 inherited;
 FreeAndNil(FThirdOctaveAnalyser);
end;


procedure TTestTThirdOctaveAnalyser.TestProcessSample32;
var
  BandIndex   : Integer;
  SampleIndex : Integer;
begin
  with TCustomSimpleOscillator32.Create do
   try
    for BandIndex := 0 to Length(CThirdOctaveFrequencies) - 1 do
     begin
      Reset;
      Frequency := CThirdOctaveFrequencies[BandIndex];
      SampleRate := FThirdOctaveAnalyser.SampleRate;

      for SampleIndex := 0 to Round(2 * SampleRate) - 1 do
       begin
        FThirdOctaveAnalyser.ProcessSample32(Sine);
        CalculateNextSample;
       end;

      with TCustomThirdOctaveAnalyserFilter(FThirdOctaveAnalyser) do
       begin
        if BandIndex > 0
         then CheckTrue(BandData[BandIndex - 1] < BandData[BandIndex]);
        CheckEquals(0, BandData[BandIndex]);
        if BandIndex + 1 < Length(CThirdOctaveFrequencies)
         then CheckTrue(BandData[BandIndex - 1] < BandData[BandIndex]);
       end;
     end;
   finally
    Free;
   end;
end;

procedure TTestTThirdOctaveAnalyser.TestProcessSample64;
var
  BandIndex   : Integer;
  SampleIndex : Integer;
begin
  with TCustomSimpleOscillator64.Create do
   try
    for BandIndex := 0 to Length(CThirdOctaveFrequencies) - 1 do
     begin
      Reset;
      Frequency := CThirdOctaveFrequencies[BandIndex];
      SampleRate := FThirdOctaveAnalyser.SampleRate;

      for SampleIndex := 0 to Round(5 * SampleRate) - 1 do
       begin
        FThirdOctaveAnalyser.ProcessSample64(Sine);
        CalculateNextSample;
       end;

      with TCustomThirdOctaveAnalyserFilter(FThirdOctaveAnalyser) do
       begin
        if BandIndex > 0
         then CheckTrue(BandData[BandIndex - 1] < BandData[BandIndex]);
        CheckEquals(0, BandData[BandIndex]);
        if BandIndex + 1 < Length(CThirdOctaveFrequencies)
         then CheckTrue(BandData[BandIndex - 1] < BandData[BandIndex]);
       end;
     end;
   finally
    Free;
   end;
end;


{ TTestTThirdOctaveAnalyserFilter }

procedure TTestTThirdOctaveAnalyserFilter.SetUp;
begin
 inherited;
 FThirdOctaveAnalyser := TThirdOctaveAnalyserFilter.Create;
 with FThirdOctaveAnalyser do
  begin
   SampleRate := 44100;
  end;
end;

function TTestTThirdOctaveAnalyserFilter.GetAnalyser: TThirdOctaveAnalyserFilter;
begin
 Result := TThirdOctaveAnalyserFilter(FThirdOctaveAnalyser);
end;


{ TTestTThirdOctaveAnalyserFilterDownSampling }

procedure TTestTThirdOctaveAnalyserFilterDownSampling.SetUp;
begin
 inherited;
 FThirdOctaveAnalyser := TThirdOctaveAnalyserFilterDownSampling.Create;
 with FThirdOctaveAnalyser do
  begin
   SampleRate := 44100;
  end;
end;

function TTestTThirdOctaveAnalyserFilterDownSampling.GetAnalyser: TThirdOctaveAnalyserFilterDownSampling;
begin
 Result := TThirdOctaveAnalyserFilterDownSampling(FThirdOctaveAnalyser);
end;


initialization
  RegisterTest(TTestTThirdOctaveAnalyserFilter.Suite);
  RegisterTest(TTestTThirdOctaveAnalyserFilterDownSampling.Suite);

end.


