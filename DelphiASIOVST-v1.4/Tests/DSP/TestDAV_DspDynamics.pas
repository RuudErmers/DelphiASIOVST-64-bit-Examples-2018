unit TestDAV_DspDynamics;

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
  TestFramework, DAV_DspDynamics, DAV_Common, DAV_DspCommon,
  DAV_DspFilterButterworth;

type
  // Test methods for class TSimpleDirectGate
  TestTSimpleDirectGate = class(TTestCase)
  strict private
    FSimpleDirectGate: TSimpleDirectGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
    procedure TestProcessSample;
  end;

  // Test methods for class TSoftDirectGate
  TestTSoftDirectGate = class(TTestCase)
  strict private
    FSoftDirectGate: TSoftDirectGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestInputSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TBrickwallLimiter
  TestTBrickwallLimiter = class(TTestCase)
  strict private
    FBrickwallLimiter: TBrickwallLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
    procedure TestCharacteristicCurve;
  end;

  // Test methods for class TSoftBrickwallLimiter
  TestTSoftBrickwallLimiter = class(TTestCase)
  strict private
    FSoftBrickwallLimiter: TSoftBrickwallLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleSoftBrickwallLimiter
  TestTSimpleSoftBrickwallLimiter = class(TTestCase)
  strict private
    FSimpleSoftBrickwallLimiter: TSimpleSoftBrickwallLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TLimiter
  TestTLimiter = class(TTestCase)
  strict private
    FLimiter: TLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
    procedure TestCharacteristicCurve;
  end;

  // Test methods for class TSoftKneeLimiter
  TestTSoftKneeLimiter = class(TTestCase)
  strict private
    FSoftKneeLimiter: TSoftKneeLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleSoftKneeLimiter
  TestTSimpleSoftKneeLimiter = class(TTestCase)
  strict private
    FSimpleSoftKneeLimiter: TSimpleSoftKneeLimiter;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestProcessSample;
  end;

  // Test methods for class TClassicGate
  TestTClassicGate = class(TTestCase)
  strict private
    FClassicGate: TClassicGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
    procedure TestProcessSample;
  end;

  // Test methods for class TClassicSoftRangeGate
  TestTClassicSoftRangeGate = class(TTestCase)
  strict private
    FClassicSoftRangeGate: TClassicSoftRangeGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
  end;

  // Test methods for class TClassicSoftKneeGate
  TestTClassicSoftKneeGate = class(TTestCase)
  strict private
    FClassicSoftKneeGate: TClassicSoftKneeGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
    procedure TestInputSample;
  end;

  // Test methods for class TAdvancedGate
  TestTAdvancedGate = class(TTestCase)
  strict private
    FAdvancedGate: TAdvancedGate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInputSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleCompressor
  TestTSimpleCompressor = class(TTestCase)
  strict private
    FSimpleCompressor: TSimpleCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSoftKneeCompressor
  TestTSoftKneeCompressor = class(TTestCase)
  strict private
    FSoftKneeCompressor: TSoftKneeCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleFeedbackCompressor
  TestTSimpleFeedbackCompressor = class(TTestCase)
  strict private
    FSimpleFeedbackCompressor: TSimpleFeedbackCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
    procedure TestCharacteristicCurve;
  end;

  // Test methods for class TSoftKneeFeedbackCompressor
  TestTSoftKneeFeedbackCompressor = class(TTestCase)
  strict private
    FSoftKneeFeedbackCompressor: TSoftKneeFeedbackCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

  // Test methods for class TSimpleRMSCompressor
  TestTSimpleRMSCompressor = class(TTestCase)
  strict private
    FSimpleRMSCompressor: TSimpleRMSCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProcessSample;
    procedure TestInputSample;
  end;

  // Test methods for class TCompressor
  TestTCompressor = class(TTestCase)
  strict private
    FCompressor: TCompressor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInputSample;
    procedure TestProcessSample;
    procedure TestTranslatePeakToGain;
  end;

implementation

uses
  SysUtils;

procedure TestTSimpleDirectGate.SetUp;
begin
 FSimpleDirectGate := TSimpleDirectGate.Create;
end;

procedure TestTSimpleDirectGate.TearDown;
begin
 FreeAndNil(FSimpleDirectGate);
end;

procedure TestTSimpleDirectGate.TestTranslatePeakToGain;
begin
 with FSimpleDirectGate do
  begin
   Threshold_dB := -10;
   CheckTrue(Abs(TranslatePeakToGain(0.5) - 1) < 1E-15);
   CheckTrue(Abs(TranslatePeakToGain(0.25)) < 1E-15);
  end;
end;

procedure TestTSimpleDirectGate.TestInputSample;
begin
 with FSimpleDirectGate do
  begin
   Threshold_dB := -6;
   InputSample(0);
   CheckEquals(GainSample(0), 0);
   InputSample(1);
   CheckEquals(GainSample(1), 1);
   InputSample(0.1);
   CheckEquals(GainSample(1), 0);
  end;
end;

procedure TestTSimpleDirectGate.TestProcessSample;
begin
 with FSimpleDirectGate do
  begin
   Threshold_dB := -6;
   CheckEquals(ProcessSample64(0), 0);
   CheckEquals(ProcessSample64(1), 1);
   CheckEquals(ProcessSample64(0.1), 0);
  end;
end;


{ TestTSoftDirectGate }

procedure TestTSoftDirectGate.SetUp;
begin
 FSoftDirectGate := TSoftDirectGate.Create;
end;

procedure TestTSoftDirectGate.TearDown;
begin
 FreeAndNil(FSoftDirectGate);
end;

procedure TestTSoftDirectGate.TestProcessSample;
begin
 with FSoftDirectGate do
  begin
   Threshold_dB := -6;
   CheckEquals(ProcessSample64(0), 0, 'Error: Processing silence <> 0');
   CheckTrue(Abs(ProcessSample64(1) - 1) < 1E-1);
   CheckTrue(Abs(ProcessSample64(0.01)) < 1E-3);
  end;
end;

procedure TestTSoftDirectGate.TestInputSample;
begin
 with FSoftDirectGate do
  begin
   Threshold_dB := -6;
   InputSample(0);
   CheckEquals(GainSample(0), 0);
   InputSample(1);
   CheckTrue(Abs(GainSample(1) - 1) < 1E-1);
   InputSample(0.1);
   CheckTrue(Abs(ProcessSample64(0.01)) < 1E-3);
  end;
end;

procedure TestTSoftDirectGate.TestTranslatePeakToGain;
begin
 with FSoftDirectGate do
  begin
   Threshold_dB := -10;
   CheckTrue(Abs(TranslatePeakToGain(0.5) - 1) < 1E-1);
   CheckTrue(Abs(TranslatePeakToGain(0.01)) < 1E-3);
  end;
end;


{ TestTBrickwallLimiter }

procedure TestTBrickwallLimiter.SetUp;
begin
 FBrickwallLimiter := TBrickwallLimiter.Create;
end;

procedure TestTBrickwallLimiter.TearDown;
begin
 FreeAndNil(FBrickwallLimiter);
end;

procedure TestTBrickwallLimiter.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FBrickwallLimiter do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;

procedure TestTBrickwallLimiter.TestTranslatePeakToGain;
begin
 with FBrickwallLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(Abs(TranslatePeakToGain(0.5) - 1) < 1E-15);
  end;
end;

procedure TestTBrickwallLimiter.TestCharacteristicCurve;
begin
 with FBrickwallLimiter do
  begin
   Threshold_dB := -10;
   CheckTrue(Abs(CharacteristicCurve_dB(0) + 10) < 1E-5);
   CheckTrue(Abs(CharacteristicCurve_dB(-10) + 10) < 1E-5);
   CheckTrue(Abs(CharacteristicCurve_dB(-20) + 20) < 1E-5);
  end;
end;


{ TestTSoftBrickwallLimiter }

procedure TestTSoftBrickwallLimiter.SetUp;
begin
 FSoftBrickwallLimiter := TSoftBrickwallLimiter.Create;
end;

procedure TestTSoftBrickwallLimiter.TearDown;
begin
 FreeAndNil(FSoftBrickwallLimiter);
end;

procedure TestTSoftBrickwallLimiter.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FSoftBrickwallLimiter do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;

procedure TestTSoftBrickwallLimiter.TestTranslatePeakToGain;
begin
 with FSoftBrickwallLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(Abs(TranslatePeakToGain(0.01) - 1) < 1E-9);
  end;
end;


{ TestTSimpleSoftBrickwallLimiter }

procedure TestTSimpleSoftBrickwallLimiter.SetUp;
begin
 FSimpleSoftBrickwallLimiter := TSimpleSoftBrickwallLimiter.Create;
end;

procedure TestTSimpleSoftBrickwallLimiter.TearDown;
begin
 FreeAndNil(FSimpleSoftBrickwallLimiter);
end;

procedure TestTSimpleSoftBrickwallLimiter.TestTranslatePeakToGain;
begin
 with FSimpleSoftBrickwallLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(Abs(TranslatePeakToGain(0.01) - 1) < 1E-9);
  end;
end;


{ TestTLimiter }

procedure TestTLimiter.SetUp;
begin
  FLimiter := TLimiter.Create;
end;

procedure TestTLimiter.TearDown;
begin
 FreeAndNil(FLimiter);
end;

procedure TestTLimiter.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FLimiter do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;

procedure TestTLimiter.TestTranslatePeakToGain;
begin
 with FLimiter do
  begin
   Threshold_dB := 0;
   CheckTrue(Abs(TranslatePeakToGain(0.01) - 1) < 1E-9);
  end;
end;

procedure TestTLimiter.TestCharacteristicCurve;
var
  ReturnValue: Double;
begin
 with FLimiter do
  begin
   Threshold_dB := -10;
   ReturnValue := CharacteristicCurve(0);
   CheckTrue(Abs(ReturnValue) < 1E-9, 'CharacteristicCurve(0) <> 0!' );
  end;
end;


{ TestTSoftKneeLimiter }

procedure TestTSoftKneeLimiter.SetUp;
begin
  FSoftKneeLimiter := TSoftKneeLimiter.Create;
end;

procedure TestTSoftKneeLimiter.TearDown;
begin
 FreeAndNil(FSoftKneeLimiter);
end;

procedure TestTSoftKneeLimiter.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FSoftKneeLimiter do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;

procedure TestTSoftKneeLimiter.TestTranslatePeakToGain;
begin
 with FSoftKneeLimiter do
  begin
   Threshold_dB := 0;
   Fail('Test not specified yet!');
   CheckTrue(Abs(TranslatePeakToGain(0.01) - 1) < 1E-9);
  end;
end;


{ TestTSimpleSoftKneeLimiter }

procedure TestTSimpleSoftKneeLimiter.SetUp;
begin
  FSimpleSoftKneeLimiter := TSimpleSoftKneeLimiter.Create;
end;

procedure TestTSimpleSoftKneeLimiter.TearDown;
begin
 FreeAndNil(FSimpleSoftKneeLimiter);
end;

procedure TestTSimpleSoftKneeLimiter.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleSoftKneeLimiter.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTSimpleSoftKneeLimiter.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FSimpleSoftKneeLimiter do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;


{ TestTClassicGate }

procedure TestTClassicGate.SetUp;
begin
  FClassicGate := TClassicGate.Create;
end;

procedure TestTClassicGate.TearDown;
begin
 FreeAndNil(FClassicGate);
end;

procedure TestTClassicGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTClassicGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FClassicGate.InputSample(Input);
  // TODO: Validate method results
end;

procedure TestTClassicGate.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FClassicGate do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;


{ TestTClassicSoftRangeGate }

procedure TestTClassicSoftRangeGate.SetUp;
begin
  FClassicSoftRangeGate := TClassicSoftRangeGate.Create;
end;

procedure TestTClassicSoftRangeGate.TearDown;
begin
 FreeAndNil(FClassicSoftRangeGate);
end;

procedure TestTClassicSoftRangeGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicSoftRangeGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTClassicSoftRangeGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FClassicSoftRangeGate.InputSample(Input);
  // TODO: Validate method results
end;


{ TestTClassicSoftKneeGate }

procedure TestTClassicSoftKneeGate.SetUp;
begin
  FClassicSoftKneeGate := TClassicSoftKneeGate.Create;
end;

procedure TestTClassicSoftKneeGate.TearDown;
begin
 FreeAndNil(FClassicSoftKneeGate);
end;

procedure TestTClassicSoftKneeGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FClassicSoftKneeGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTClassicSoftKneeGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FClassicSoftKneeGate.InputSample(Input);
  // TODO: Validate method results
end;


{ TestTAdvancedGate }

procedure TestTAdvancedGate.SetUp;
begin
  FAdvancedGate := TAdvancedGate.Create;
end;

procedure TestTAdvancedGate.TearDown;
begin
 FreeAndNil(FAdvancedGate);
end;

procedure TestTAdvancedGate.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FAdvancedGate.InputSample(Input);
  // TODO: Validate method results
end;

procedure TestTAdvancedGate.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FAdvancedGate.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleCompressor }

procedure TestTSimpleCompressor.SetUp;
begin
  FSimpleCompressor := TSimpleCompressor.Create;
end;

procedure TestTSimpleCompressor.TearDown;
begin
 FreeAndNil(FSimpleCompressor);
end;

procedure TestTSimpleCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSoftKneeCompressor }

procedure TestTSoftKneeCompressor.SetUp;
begin
  FSoftKneeCompressor := TSoftKneeCompressor.Create;
end;

procedure TestTSoftKneeCompressor.TearDown;
begin
 FreeAndNil(FSoftKneeCompressor);
end;

procedure TestTSoftKneeCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleFeedbackCompressor }

procedure TestTSimpleFeedbackCompressor.SetUp;
begin
  FSimpleFeedbackCompressor := TSimpleFeedbackCompressor.Create;
end;

procedure TestTSimpleFeedbackCompressor.TearDown;
begin
 FreeAndNil(FSimpleFeedbackCompressor);
end;

procedure TestTSimpleFeedbackCompressor.TestProcessSample;
var
  ThresholdFactor: Single;
begin
 with FSimpleFeedbackCompressor do
  begin
   Threshold_dB := -10;
   ThresholdFactor := dB_to_Amp(Threshold_dB);
   CheckEquals(ProcessSample64(0), 0, 'ProcessSample64(0) <> 0');
   CheckTrue(Abs(ProcessSample64(ThresholdFactor) - ThresholdFactor) < 1E-5);
   CheckTrue(Abs(ProcessSample64(1) - ThresholdFactor) < 1E-5);
  end;
end;

procedure TestTSimpleFeedbackCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleFeedbackCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;

procedure TestTSimpleFeedbackCompressor.TestCharacteristicCurve;
var
  ReturnValue: Double;
  InputLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleFeedbackCompressor.CharacteristicCurve(InputLevel);
  // TODO: Validate method results
end;


{ TestTSoftKneeFeedbackCompressor }

procedure TestTSoftKneeFeedbackCompressor.SetUp;
begin
  FSoftKneeFeedbackCompressor := TSoftKneeFeedbackCompressor.Create;
end;

procedure TestTSoftKneeFeedbackCompressor.TearDown;
begin
 FreeAndNil(FSoftKneeFeedbackCompressor);
end;

procedure TestTSoftKneeFeedbackCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeFeedbackCompressor.ProcessSample64(Input);
  // TODO: Validate method results
end;

procedure TestTSoftKneeFeedbackCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSoftKneeFeedbackCompressor.TranslatePeakToGain(PeakLevel);
  // TODO: Validate method results
end;


{ TestTSimpleRMSCompressor }

procedure TestTSimpleRMSCompressor.SetUp;
begin
  FSimpleRMSCompressor := TSimpleRMSCompressor.Create;
end;

procedure TestTSimpleRMSCompressor.TearDown;
begin
 FreeAndNil(FSimpleRMSCompressor);
end;

procedure TestTSimpleRMSCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FSimpleRMSCompressor.ProcessSample64(Input);
  // TODO: Validate method results
end;

procedure TestTSimpleRMSCompressor.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FSimpleRMSCompressor.InputSample(Input);
  // TODO: Validate method results
end;


{ TestTCompressor }

procedure TestTCompressor.SetUp;
begin
  FCompressor := TCompressor.Create;
end;

procedure TestTCompressor.TearDown;
begin
 FreeAndNil(FCompressor);
end;

procedure TestTCompressor.TestInputSample;
var
  Input: Double;
begin
  // TODO: Setup method call parameters
  FCompressor.InputSample(Input);
  // TODO: Validate method results
end;

procedure TestTCompressor.TestProcessSample;
var
  ReturnValue: Double;
  Input: Double;
begin
  // TODO: Setup method call parameters
  ReturnValue := FCompressor.ProcessSample64(Input);
  // TODO: Validate method results
end;

procedure TestTCompressor.TestTranslatePeakToGain;
var
  ReturnValue: Double;
  PeakLevel: Double;
begin
 with FCompressor do
  begin
   Threshold_dB := -20;
   MakeUpGain_dB := 0;
   Ratio := 1;

   PeakLevel := dB_to_Amp(2 * Threshold_dB); 
  end;
 ReturnValue := FCompressor.TranslatePeakToGain(PeakLevel);
 CheckEquals(PeakLevel, ReturnValue);
end;

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Dynamics Tests');
 TS.AddSuite(TestTSimpleDirectGate.Suite);
 TS.AddSuite(TestTSoftDirectGate.Suite);
 TS.AddSuite(TestTBrickwallLimiter.Suite);
 TS.AddSuite(TestTSoftBrickwallLimiter.Suite);
 TS.AddSuite(TestTSimpleSoftBrickwallLimiter.Suite);
 TS.AddSuite(TestTLimiter.Suite);
 TS.AddSuite(TestTSoftKneeLimiter.Suite);
 TS.AddSuite(TestTSimpleSoftKneeLimiter.Suite);
 TS.AddSuite(TestTClassicGate.Suite);
 TS.AddSuite(TestTClassicSoftRangeGate.Suite);
 TS.AddSuite(TestTClassicSoftKneeGate.Suite);
 TS.AddSuite(TestTAdvancedGate.Suite);
 TS.AddSuite(TestTSimpleCompressor.Suite);
 TS.AddSuite(TestTSoftKneeCompressor.Suite);
 TS.AddSuite(TestTSimpleFeedbackCompressor.Suite);
 TS.AddSuite(TestTSoftKneeFeedbackCompressor.Suite);
 TS.AddSuite(TestTSimpleRMSCompressor.Suite);
 TS.AddSuite(TestTCompressor.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
