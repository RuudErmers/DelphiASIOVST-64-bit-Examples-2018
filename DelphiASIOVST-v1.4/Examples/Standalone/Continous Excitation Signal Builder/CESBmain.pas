unit CESBmain;

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

{$I ..\DAV_Compiler.inc}
{-$DEFINE Use_IPPS}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, XPMan, Spin, Menus, 
  DAV_Classes, DAV_AudioData, DAV_AudioFile, DAV_AudioFileWav, 
  DAV_AudioFileAIFF, DAV_GuiAudioDataDisplay, DAV_DspFilterChebyshev, 
  DAV_DspFilterChebyshevType1;

type
  TFmContinousExcitationSignalBuilder = class(TForm)
    GbSignalType: TGroupBox;
    RbPureTone: TRadioButton;
    RbThirdOctaveNoise: TRadioButton;
    RbOctaveNoise: TRadioButton;
    RbWhiteNoise: TRadioButton;
    GbFrequency: TGroupBox;
    TbFrequency: TTrackBar;
    EdFrequency: TEdit;
    LbFrequencyUnit: TLabel;
    GbOutput: TGroupBox;
    EdFileName: TEdit;
    LbFilename: TLabel;
    RbPinkNoise: TRadioButton;
    BtExit: TButton;
    BtBuild: TButton;
    LbChannels: TLabel;
    SeChannels: TSpinEdit;
    LbLength: TLabel;
    SeLength: TSpinEdit;
    LbLengthUnit: TLabel;
    PuFrequency: TPopupMenu;
    Mi20Hz: TMenuItem;
    Mi25Hz: TMenuItem;
    Mi31Hz5: TMenuItem;
    Mi40Hz: TMenuItem;
    Mi50Hz: TMenuItem;
    Mi63Hz: TMenuItem;
    Mi80Hz: TMenuItem;
    Mi100Hz: TMenuItem;
    Mi125Hz: TMenuItem;
    Mi160Hz: TMenuItem;
    Mi200Hz: TMenuItem;
    Mi250Hz: TMenuItem;
    Mi315Hz: TMenuItem;
    Mi400Hz: TMenuItem;
    Mi500Hz: TMenuItem;
    Mi630Hz: TMenuItem;
    Mi800Hz: TMenuItem;
    Mi1kHz: TMenuItem;
    Mi1k25Hz: TMenuItem;
    Mi1k6Hz: TMenuItem;
    Mi2kHz: TMenuItem;
    Mi2k5Hz: TMenuItem;
    Mi31k5Hz: TMenuItem;
    Mi4kHz: TMenuItem;
    Mi5kHz: TMenuItem;
    Mi6k3Hz: TMenuItem;
    Mi8kHz: TMenuItem;
    Mi10kHz: TMenuItem;
    Mi12k5Hz: TMenuItem;
    Mi16kHz: TMenuItem;
    Mi20kHz: TMenuItem;
    AudioDataCollection: TAudioDataCollection32;
    procedure FormCreate(Sender: TObject);
    procedure BtBuildClick(Sender: TObject);
    procedure BtExitClick(Sender: TObject);
    procedure EdFrequencyChange(Sender: TObject);
    procedure EdFrequencyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EdFrequencyMouseLeave(Sender: TObject);
    procedure Mi100HzClick(Sender: TObject);
    procedure Mi10kHzClick(Sender: TObject);
    procedure Mi125HzClick(Sender: TObject);
    procedure Mi12k5HzClick(Sender: TObject);
    procedure Mi160HzClick(Sender: TObject);
    procedure Mi16HzClick(Sender: TObject);
    procedure Mi16kHzClick(Sender: TObject);
    procedure Mi1k25HzClick(Sender: TObject);
    procedure Mi1k6HzClick(Sender: TObject);
    procedure Mi1kHzClick(Sender: TObject);
    procedure Mi200HzClick(Sender: TObject);
    procedure Mi20HzClick(Sender: TObject);
    procedure Mi20kHzClick(Sender: TObject);
    procedure Mi250HzClick(Sender: TObject);
    procedure Mi25HzClick(Sender: TObject);
    procedure Mi2k5HzClick(Sender: TObject);
    procedure Mi2kHzClick(Sender: TObject);
    procedure Mi315HzClick(Sender: TObject);
    procedure Mi31Hz5Click(Sender: TObject);
    procedure Mi31k5HzClick(Sender: TObject);
    procedure Mi400HzClick(Sender: TObject);
    procedure Mi40HzClick(Sender: TObject);
    procedure Mi4kHzClick(Sender: TObject);
    procedure Mi500HzClick(Sender: TObject);
    procedure Mi50HzClick(Sender: TObject);
    procedure Mi5kHzClick(Sender: TObject);
    procedure Mi630HzClick(Sender: TObject);
    procedure Mi63HzClick(Sender: TObject);
    procedure Mi6k3HzClick(Sender: TObject);
    procedure Mi800HzClick(Sender: TObject);
    procedure Mi80HzClick(Sender: TObject);
    procedure Mi8kHzClick(Sender: TObject);
    procedure RbOctaveNoiseClick(Sender: TObject);
    procedure RbPinkNoiseClick(Sender: TObject);
    procedure RbPureToneClick(Sender: TObject);
    procedure RbThirdOctaveNoiseClick(Sender: TObject);
    procedure RbWhiteNoiseClick(Sender: TObject);
    procedure SeChannelsChange(Sender: TObject);
    procedure TbFrequencyChange(Sender: TObject);
  private
    FSignalTypeName : string;
    FAdditionalInfo : string;
    FSampleRate     : Single;
    FFrequency: Single;

    procedure SetSignalTypeName(const Value: string);
    procedure SetAdditionalInfo(const Value: string);
    procedure PresetFrequency(Frequency: Single);
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
  protected
    procedure UpdateFilename; virtual;
    procedure AdditionalInfoChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure SignalTypeNameChanged; virtual;

    procedure BuildPureToneSignal;
    procedure BuildThirdOctaveNoiseSimple;
    procedure BuildThirdOctaveNoiseCrossfade;
    procedure BuildThirdOctaveNoiseFFT;
    procedure BuildThirdOctaveNoiseFFTResampled;
    procedure BuildOctaveNoiseSimple;
    procedure BuildOctaveNoiseFFT;
    procedure BuildWhiteNoise;
    procedure BuildPinkNoise;
  public
    property SignalTypeName: string read FSignalTypeName write SetSignalTypeName;
    property AdditionalInfo: string read FAdditionalInfo write SetAdditionalInfo;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
  end;

var
  FmContinousExcitationSignalBuilder: TFmContinousExcitationSignalBuilder;

implementation

uses
  Math, DAV_Common, DAV_Math, DAV_Complex, DAV_Types, DAV_DspPinkNoiseGenerator,
  DAV_DspSimpleOscillator, DAV_DspFilter, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

{$R *.dfm}

procedure TFmContinousExcitationSignalBuilder.FormCreate(Sender: TObject);
begin
 FSampleRate := 44100;
 RbPureToneClick(Sender);
 PresetFrequency(1000);
 UpdateFilename;
end;

procedure TFmContinousExcitationSignalBuilder.RbPureToneClick(Sender: TObject);
begin
 GbFrequency.Enabled := True;
 SignalTypeName := 'Pure Tone';
end;

procedure TFmContinousExcitationSignalBuilder.RbThirdOctaveNoiseClick(Sender: TObject);
begin
 GbFrequency.Enabled := True;
 SignalTypeName := 'Third-Octave Noise';
end;

procedure TFmContinousExcitationSignalBuilder.EdFrequencyChange(
  Sender: TObject);
begin
 FAdditionalInfo := ' ' + EdFrequency.Text;
end;

procedure TFmContinousExcitationSignalBuilder.EdFrequencyKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
 if Key = 13 then
  try
   PresetFrequency(StrToFloat(EdFrequency.Text));
   FFrequency := StrToFloat(EdFrequency.Text);
  except
   on E : Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.EdFrequencyMouseLeave(
  Sender: TObject);
begin
 UpdateFilename;
end;

procedure TFmContinousExcitationSignalBuilder.Mi100HzClick(Sender: TObject);
begin
 PresetFrequency(100);
end;

procedure TFmContinousExcitationSignalBuilder.Mi10kHzClick(Sender: TObject);
begin
 PresetFrequency(10000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi125HzClick(Sender: TObject);
begin
 PresetFrequency(125);
end;

procedure TFmContinousExcitationSignalBuilder.Mi12k5HzClick(Sender: TObject);
begin
 PresetFrequency(12500);
end;

procedure TFmContinousExcitationSignalBuilder.Mi160HzClick(Sender: TObject);
begin
 PresetFrequency(160);
end;

procedure TFmContinousExcitationSignalBuilder.Mi16HzClick(Sender: TObject);
begin
 PresetFrequency(16);
end;

procedure TFmContinousExcitationSignalBuilder.Mi16kHzClick(Sender: TObject);
begin
 PresetFrequency(16000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi1k25HzClick(Sender: TObject);
begin
 PresetFrequency(1250);
end;

procedure TFmContinousExcitationSignalBuilder.Mi1k6HzClick(Sender: TObject);
begin
 PresetFrequency(1600);
end;

procedure TFmContinousExcitationSignalBuilder.Mi1kHzClick(Sender: TObject);
begin
 PresetFrequency(1000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi200HzClick(Sender: TObject);
begin
 PresetFrequency(200);
end;

procedure TFmContinousExcitationSignalBuilder.Mi20HzClick(Sender: TObject);
begin
 PresetFrequency(20);
end;

procedure TFmContinousExcitationSignalBuilder.Mi20kHzClick(Sender: TObject);
begin
 PresetFrequency(20000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi250HzClick(Sender: TObject);
begin
 PresetFrequency(250);
end;

procedure TFmContinousExcitationSignalBuilder.Mi25HzClick(Sender: TObject);
begin
 PresetFrequency(25);
end;

procedure TFmContinousExcitationSignalBuilder.Mi2k5HzClick(Sender: TObject);
begin
 PresetFrequency(2500);
end;

procedure TFmContinousExcitationSignalBuilder.Mi2kHzClick(Sender: TObject);
begin
 PresetFrequency(2000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi315HzClick(Sender: TObject);
begin
 PresetFrequency(315);
end;

procedure TFmContinousExcitationSignalBuilder.Mi31Hz5Click(Sender: TObject);
begin
 PresetFrequency(31.5);
end;

procedure TFmContinousExcitationSignalBuilder.Mi31k5HzClick(Sender: TObject);
begin
 PresetFrequency(3150);
end;

procedure TFmContinousExcitationSignalBuilder.Mi400HzClick(Sender: TObject);
begin
 PresetFrequency(400);
end;

procedure TFmContinousExcitationSignalBuilder.Mi40HzClick(Sender: TObject);
begin
 PresetFrequency(40);
end;

procedure TFmContinousExcitationSignalBuilder.Mi4kHzClick(Sender: TObject);
begin
 PresetFrequency(4000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi500HzClick(Sender: TObject);
begin
 PresetFrequency(500);
end;

procedure TFmContinousExcitationSignalBuilder.Mi50HzClick(Sender: TObject);
begin
 PresetFrequency(50);
end;

procedure TFmContinousExcitationSignalBuilder.Mi5kHzClick(Sender: TObject);
begin
 PresetFrequency(5000);
end;

procedure TFmContinousExcitationSignalBuilder.Mi630HzClick(Sender: TObject);
begin
 PresetFrequency(630);
end;

procedure TFmContinousExcitationSignalBuilder.Mi63HzClick(Sender: TObject);
begin
 PresetFrequency(63);
end;

procedure TFmContinousExcitationSignalBuilder.Mi6k3HzClick(Sender: TObject);
begin
 PresetFrequency(6300);
end;

procedure TFmContinousExcitationSignalBuilder.Mi800HzClick(Sender: TObject);
begin
 PresetFrequency(800);
end;

procedure TFmContinousExcitationSignalBuilder.Mi80HzClick(Sender: TObject);
begin
 PresetFrequency(80);
end;

procedure TFmContinousExcitationSignalBuilder.Mi8kHzClick(Sender: TObject);
begin
 PresetFrequency(8000);
end;

procedure TFmContinousExcitationSignalBuilder.PresetFrequency(Frequency: Single);
begin
 TbFrequency.Position := Round(TbFrequency.Max * FreqLogToLinear(Frequency));
 FFrequency := Frequency;
 if Frequency < 1000
  then AdditionalInfo := ' ' + FloatToStrF(Frequency, ffGeneral, 3, 3) + ' Hz'
  else AdditionalInfo := ' ' + FloatToStrF(1E-3 * Frequency, ffGeneral, 3, 3) + ' kHz';
end;

procedure TFmContinousExcitationSignalBuilder.RbOctaveNoiseClick(Sender: TObject);
begin
 GbFrequency.Enabled := True;
 SignalTypeName := 'Octave-Band Noise';
end;

procedure TFmContinousExcitationSignalBuilder.RbPinkNoiseClick(Sender: TObject);
begin
 GbFrequency.Enabled := False;
 FAdditionalInfo := '';
 SignalTypeName := 'Pink Noise';
end;

procedure TFmContinousExcitationSignalBuilder.RbWhiteNoiseClick(Sender: TObject);
begin
 GbFrequency.Enabled := False;
 FAdditionalInfo := '';
 SignalTypeName := 'White Noise';
end;

procedure TFmContinousExcitationSignalBuilder.SeChannelsChange(Sender: TObject);
begin
 AudioDataCollection.ChannelCount := SeChannels.Value;
end;

procedure TFmContinousExcitationSignalBuilder.SetAdditionalInfo(
  const Value: string);
begin
 if FAdditionalInfo <> Value then
  begin
   FAdditionalInfo := Value;
   AdditionalInfoChanged;
  end;
end;

procedure TFmContinousExcitationSignalBuilder.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TFmContinousExcitationSignalBuilder.SetSampleRate(const Value: Single);
begin
 if Value <> SampleRate
  then SampleRate := Value;
end;

procedure TFmContinousExcitationSignalBuilder.SetSignalTypeName(const Value: string);
begin
 if FSignalTypeName <> Value then
  begin
   FSignalTypeName := Value;
   SignalTypeNameChanged;
  end;
end;

procedure TFmContinousExcitationSignalBuilder.FrequencyChanged;
begin
 EdFrequency.Text := FloatToStrF(FFrequency, ffGeneral, 5, 5);
end;

procedure TFmContinousExcitationSignalBuilder.AdditionalInfoChanged;
begin
 UpdateFilename;
end;

procedure TFmContinousExcitationSignalBuilder.SignalTypeNameChanged;
begin
 EdFileName.Text := FSignalTypeName + FAdditionalInfo + '.wav';
end;

procedure TFmContinousExcitationSignalBuilder.UpdateFilename;
begin
 EdFileName.Text := FSignalTypeName + FAdditionalInfo + '.wav';
end;

procedure TFmContinousExcitationSignalBuilder.TbFrequencyChange(Sender: TObject);
begin
 Frequency := FreqLinearToLog(TbFrequency.Position / TbFrequency.Max)
end;

procedure TFmContinousExcitationSignalBuilder.BtBuildClick(Sender: TObject);
begin
 // disabling controls
 BtBuild.Enabled := False;
 GbSignalType.Enabled := False;
 GbFrequency.Enabled := False;
 GbOutput.Enabled := False;
 BtBuild.Caption := 'Generating...';
 Application.ProcessMessages;

 if EdFileName.Text <> '' then
  if RbPureTone.Checked then BuildPureToneSignal else
  if RbThirdOctaveNoise.Checked then BuildThirdOctaveNoiseFFT else
  if RbOctaveNoise.Checked then BuildOctaveNoiseFFT else
  if RbWhiteNoise.Checked then BuildWhiteNoise else
  if RbPinkNoise.Checked then BuildPinkNoise;

 // re-enabling controls
 BtBuild.Caption := '&Build';
 GbOutput.Enabled := True;
 GbSignalType.Enabled := True;
 GbFrequency.Enabled := True;
 BtBuild.Enabled := True;
end;

procedure TFmContinousExcitationSignalBuilder.BuildPureToneSignal;
var
  Channel : Integer;
  Sample  : Integer;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;
   SampleFrames := Round(SampleRate * 1E-3 * SeLength.Value);
   SampleRate := Self.SampleRate;
   with TCustomSimpleOscillator32.Create do
    try
     // quantize frequency
     Amplitude := dB_to_Amp(-0.1);
     SampleRate := Self.SampleRate;
     Frequency := (SampleRate / SampleFrames *
       Round(SampleFrames / SampleRate * Self.Frequency));

     // process channels
     for Channel := 0 to ChannelCount - 1 do
      begin
       for Sample := 0 to SampleFrames - 1 do
        begin
         ChannelDataPointer[Channel]^[Sample] := Sine;
         CalculateNextSample;
        end;
       Reset;
      end;
    finally
     Free;
    end;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildThirdOctaveNoiseSimple;
var
  Channel  : Integer;
  Sample   : Integer;
  Temp     : Double;
  TempPeak : Double;
  IRSize   : Integer;
  LowPass  : TChebyshev1LowpassFilter;
  Highpass : TChebyshev1HighpassFilter;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;

   SampleFrames := Round(SampleRate * 1E-3 * SeLength.Value);
   SampleRate := Self.SampleRate;

   LowPass  := TChebyshev1LowpassFilter.Create(12);
   with LowPass do
    begin
     SampleRate := Self.SampleRate;
     Frequency := Self.Frequency * Power(2, 0.167);
     Ripple := 0.1;
    end;
   Highpass := TChebyshev1HighpassFilter.Create(12);
   with Highpass do
    begin
     SampleRate := Self.SampleRate;
     Frequency := Self.Frequency * Power(2, -0.167);
     Ripple := 0.1;
    end;

   // calculate IR size
   TempPeak := 0;
   IRSize := 0;
   Highpass.ProcessSample64(Lowpass.ProcessSample64(1.0));
   repeat
    Temp := Highpass.ProcessSample64(Lowpass.ProcessSample64(0.0));
    if Abs(Temp) > TempPeak
     then TempPeak := Abs(Temp)
     else TempPeak := 0.9999 * TempPeak;
    Inc(IRSize);
   until (TempPeak < 1E-7) and (IRSize > Lowpass.Order * Highpass.Order);

   with TFastPinkNoiseGenerator.Create do
    try
     // quantize frequency
     Amplitude := dB_to_Amp(-0.1);
     SampleRate := Self.SampleRate;

     // first pass (get system into the mood)
     for Sample := 0 to 2 * IRSize - 1
      do Highpass.ProcessSample64(LowPass.ProcessSample64(ProcessSample64));

     TempPeak := 0;

     // generate samples
     for Channel := 0 to ChannelCount - 1 do
      begin
       for Sample := 0 to SampleFrames - 1
        do ChannelDataPointer[Channel]^[Sample] := Lowpass.ProcessSample64(
          Highpass.ProcessSample64(ProcessSample64));
       Temp := TAudioChannel32(Channels[Channel]).Peak;
       if Temp > TempPeak then TempPeak := Temp;
      end;

     // normalize samples
     Multiply(1 / TempPeak);
    finally
     Free;
    end;

   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildThirdOctaveNoiseCrossfade;
var
  Channel  : Integer;
  Sample   : Integer;
  Temp     : Double;
  TempPeak : Double;
  IRSize   : Integer;
  LowPass  : TChebyshev1LowpassFilter;
  Highpass : TChebyshev1HighpassFilter;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;

   SampleFrames := 2 * Round(SampleRate * 1E-3 * SeLength.Value);
   SampleRate := Self.SampleRate;

   LowPass  := TChebyshev1LowpassFilter.Create(12);
   with LowPass do
    begin
     SampleRate := Self.SampleRate;
     Frequency := Self.Frequency * Power(2, -0.167);
     Ripple := 0.1;
    end;
   Highpass := TChebyshev1HighpassFilter.Create(12);
   with Highpass do
    begin
     SampleRate := Self.SampleRate;
     Frequency := Self.Frequency * Power(2, -0.167);
     Ripple := 0.1;
    end;

   // calculate IR size
   TempPeak := 0;
   IRSize := 0;
   Highpass.ProcessSample64(Lowpass.ProcessSample64(1.0));
   repeat
    Temp := Highpass.ProcessSample64(Lowpass.ProcessSample64(0.0));
    if Abs(Temp) > TempPeak
     then TempPeak := Abs(Temp)
     else TempPeak := 0.9999 * TempPeak;
    Inc(IRSize);
   until (TempPeak < 1E-7) and (IRSize > Lowpass.Order * Highpass.Order);

   with TFastPinkNoiseGenerator.Create do
    try
     // quantize frequency
     Amplitude := dB_to_Amp(-0.1);
     SampleRate := Self.SampleRate;

     // first pass (get system into the mood)
     for Sample := 0 to 2 * IRSize - 1
      do Highpass.ProcessSample64(LowPass.ProcessSample64(ProcessSample64));

     // generate samples
     for Channel := 0 to ChannelCount - 1 do
      begin
       for Sample := 0 to SampleFrames - 1
        do ChannelDataPointer[Channel]^[Sample] := Lowpass.ProcessSample64(
          Highpass.ProcessSample64(ProcessSample64));

      TempPeak := 0;
      Temp := 4 / SampleFrames;
      for Sample := 0 to SampleFrames div 4 - 1 do
       begin
        ChannelDataPointer[Channel]^[Sample] :=
          (Sample + 1) * Temp * ChannelDataPointer[Channel]^[Sample] +
          (1 - (Sample + 1) * Temp) *
          ChannelDataPointer[Channel]^[(3 * SampleFrames div 4 ) - Sample - 1];

        ChannelDataPointer[Channel]^[SampleFrames div 4 + Sample] :=
          (1 - (Sample + 1) * Temp) *
          ChannelDataPointer[Channel]^[SampleFrames div 4 + Sample] +
          ((Sample + 1) * Temp) *
          ChannelDataPointer[Channel]^[SampleFrames - Sample - 1];

(*
        // clear beyond
        ChannelDataPointer[Channel]^[SampleFrames div 2 + Sample] := 0;
        ChannelDataPointer[Channel]^[3 * SampleFrames div 4 + Sample] := 0;
*)
       end;
      Temp := TAudioChannel32(Channels[Channel]).Peak;
      if Temp > TempPeak then TempPeak := Temp;
     end;

     // normalize samples
     Multiply(1 / TempPeak);
    finally
     Free;
    end;

   SampleFrames := SampleFrames div 2;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildThirdOctaveNoiseFFT;
var
  Channel   : Integer;
  Bin       : Integer;
  BinBorder : array [0..1] of Integer;
  Temp      : Double;
  Peak      : Double;
  IRSize    : Integer;
  FreqDom   : PDAVComplexSingleFixedArray;

  FFT       : TFftReal2Complex;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;

   SampleFrames := RoundToPowerOf2(Round(SampleRate * 1E-3 * SeLength.Value));
   SampleRate := Self.SampleRate;

   {$IFDEF Use_IPPS}
   Fft := TFftReal2ComplexIPPSFloat32.Create(TruncLog2(SampleFrames));
   {$ELSE} {$IFDEF Use_CUDA}
   Fft := TFftReal2ComplexCUDA32.Create(TruncLog2(SampleFrames));
   {$ELSE}
   Fft := TFftReal2ComplexNativeFloat32.Create(TruncLog2(SampleFrames));
   Fft.DataOrder := doPackedComplex;
   {$ENDIF}{$ENDIF}

   try
    FFT.DataOrder := doPackedComplex;
    FFT.AutoScaleType := astDivideFwdByN;

    GetMem(FreqDom, FFT.BinCount * SizeOf(TComplex32));

    Temp := FFT.FFTSize * Self.Frequency / SampleRate;
    BinBorder[0] := Round(Temp * Power(2, -1/6));
    BinBorder[1] := Round(Temp * Power(2, +1/6));

    for Channel := 0 to ChannelCount - 1 do
     begin
      FreqDom[0].Re := 0;
      FreqDom[0].Im := 0;

      for Bin := 1 to FFT.BinCount - 2 do
       if (Bin >= BinBorder[0]) and (Bin <= BinBorder[1])
        then FreqDom[Bin] := ComplexPolar32(1 / Bin, Pi * FastRandom)
        else FreqDom[Bin] := ComplexPolar32(0, 0);

      FreqDom[FFT.BinCount - 1].Re := 0;
      FreqDom[FFT.BinCount - 1].Im := 0;

      FFT.PerformIFFT(FreqDom, ChannelDataPointer[Channel]);
      TAudioChannel32(Channels[Channel]).RemoveDC;
     end;
   finally
    FreeAndNil(FFT);
   end;

   Normalize;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildThirdOctaveNoiseFFTResampled;
var
  Channel     : Integer;
  Bin         : Integer;
  BinBorder   : array [0..1] of Integer;
  Temp        : Double;
  Peak        : Double;
  IRSize      : Integer;
  FreqDom     : PDAVComplexSingleFixedArray;
  TimeDom     : PDAVSingleFixedArray;
  ChannelData : PDAVSingleFixedArray;

  FFT         : TFftReal2Complex;

  i, j, m, n  : Integer;
  Fak, Spd    : Double;
  TA, TB, TC  : Double;
  SRe, SIm    : Double;
  ARe, AIm    : Double;
  BRe, BIm    : Double;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;

   SampleFrames := Round(SampleRate * 1E-3 * SeLength.Value);
   SampleRate := Self.SampleRate;

   {$IFDEF Use_IPPS}
   Fft := TFftReal2ComplexIPPSFloat32.Create(TruncLog2(SampleFrames));
   {$ELSE} {$IFDEF Use_CUDA}
   Fft := TFftReal2ComplexCUDA32.Create(TruncLog2(SampleFrames));
   {$ELSE}
   Fft := TFftReal2ComplexNativeFloat32.Create(TruncLog2(SampleFrames));
   Fft.DataOrder := doPackedComplex;
   {$ENDIF}{$ENDIF}

   try
    FFT.DataOrder := doPackedComplex;
    FFT.AutoScaleType := astDivideFwdByN;

    GetMem(FreqDom, FFT.BinCount * SizeOf(TComplex32));
    GetMem(TimeDom, FFT.FFTSize * SizeOf(Single));

    Temp := FFT.FFTSize * Self.Frequency / SampleRate;
    BinBorder[0] := Round(Temp * Power(2, -1/6));
    BinBorder[1] := Round(Temp * Power(2, +1/6));

    for Channel := 0 to ChannelCount - 1 do
     begin
      FreqDom[0].Re := 0;
      FreqDom[0].Im := 0;

      for Bin := 1 to FFT.BinCount - 2 do
       if (Bin >= BinBorder[0]) and (Bin <= BinBorder[1])
        then FreqDom[Bin] := ComplexPolar32(1 / Bin, Pi * FastRandom)
        else FreqDom[Bin] := ComplexPolar32(0, 0);

      FreqDom[FFT.BinCount - 1].Re := 0;
      FreqDom[FFT.BinCount - 1].Im := 0;

      FFT.PerformIFFT(FreqDom, TimeDom);

      // perform resampling (still containing errors!)
      FillChar(ChannelDataPointer[Channel]^, SampleFrames * SizeOf(Single), 0);
      Fak := SampleFrames / FFT.FFTSize;
      Spd := PI / Fak;
      ChannelData := ChannelDataPointer[Channel];

      for j := 0 to FFT.FFTSize - 1 do
       begin
        ARe := 0;
        AIm := -1;
        TA := j * Fak * Spd;
        TB := (j * Fak - SampleFrames) * Spd;
        TC := (j * Fak + SampleFrames) * Spd;
        GetSinCos(PI - TA, ARe, AIm);
        GetSinCos(PI - TB, BRe, BIm);
        GetSinCos(Spd, SIm, SRe);
        for i := 0 to SampleFrames - 1 do
         begin
          if TimeDom[j] = 1 then Break;
          if Abs(i * Spd - TA) < 1E-5
           then ChannelData^[i] := ChannelData^[i] - 1
           else ChannelData^[i] := ChannelData^[i] - ARe / (i * Spd - TA);
          if Abs(i * Spd - TB) < 1E-5
           then ChannelData^[i] := ChannelData^[i] - 1
           else ChannelData^[i] := ChannelData^[i] - BRe / (i * Spd - TB);
          if Abs(i * Spd - TC) < 1E-5
           then ChannelData^[i] := ChannelData^[i] - 1
           else ChannelData^[i] := ChannelData^[i] - BRe / (i * Spd - TC);

          Temp := SRe * ARe - SIm * AIm;
          AIm := SIm * ARe + SRe * AIm;
          ARe := Temp;
          Temp := SRe * BRe - SIm * BIm;
          BIm := SIm * BRe + SRe * BIm;
          BRe := Temp;
         end;
       end;

      TAudioChannel32(Channels[Channel]).RemoveDC;
     end;
   finally
    FreeAndNil(FFT);
   end;

   Normalize;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildOctaveNoiseFFT;
var
  Channel   : Integer;
  Bin       : Integer;
  BinBorder : array [0..1] of Integer;
  Temp      : Double;
  Peak      : Double;
  IRSize    : Integer;
  FreqDom   : PDAVComplexSingleFixedArray;

  FFT       : TFftReal2Complex;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;

   SampleFrames := RoundToPowerOf2(Round(SampleRate * 1E-3 * SeLength.Value));
   SampleRate := Self.SampleRate;

   {$IFDEF Use_IPPS}
   Fft := TFftReal2ComplexIPPSFloat32.Create(TruncLog2(SampleFrames));
   {$ELSE} {$IFDEF Use_CUDA}
   Fft := TFftReal2ComplexCUDA32.Create(TruncLog2(SampleFrames));
   {$ELSE}
   Fft := TFftReal2ComplexNativeFloat32.Create(TruncLog2(SampleFrames));
   Fft.DataOrder := doPackedComplex;
   {$ENDIF}{$ENDIF}

   try
    FFT.DataOrder := doPackedComplex;
    FFT.AutoScaleType := astDivideFwdByN;

    GetMem(FreqDom, FFT.BinCount * SizeOf(TComplex32));

    Temp := FFT.FFTSize * Self.Frequency / SampleRate;
    BinBorder[0] := Round(Temp * Power(2, -0.5));
    BinBorder[1] := Round(Temp * Power(2, +0.5));

    for Channel := 0 to ChannelCount - 1 do
     begin
      FreqDom[0].Re := 0;
      FreqDom[0].Im := 0;

      for Bin := 1 to FFT.BinCount - 2 do
       if (Bin >= BinBorder[0]) and (Bin <= BinBorder[1])
        then FreqDom[Bin] := ComplexPolar32(1 / Bin, Pi * FastRandom)
        else FreqDom[Bin] := ComplexPolar32(0, 0);

      FreqDom[FFT.BinCount - 1].Re := 0;
      FreqDom[FFT.BinCount - 1].Im := 0;

      FFT.PerformIFFT(FreqDom, ChannelDataPointer[Channel]);
      TAudioChannel32(Channels[Channel]).RemoveDC;
     end;
   finally
    FreeAndNil(FFT);
   end;

   Normalize;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildOctaveNoiseSimple;
var
  Channel  : Integer;
  Sample   : Integer;
  Scale    : Double;
  IRSize   : array [0..1] of Integer;
  LowPass  : TChebyshev1LowpassFilter;
  Highpass : TChebyshev1HighpassFilter;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;
   SampleFrames := Round(SampleRate * 1E-3 * SeLength.Value);
   SampleRate := Self.SampleRate;
   LowPass  := TChebyshev1LowpassFilter.Create(12);
   with LowPass do
    begin
     SampleRate := Self.SampleRate;
     Frequency := Self.Frequency * 1.99;
     Ripple := 0.1;
    end;
   Highpass := TChebyshev1HighpassFilter.Create(12);
   with Highpass do
    begin
     SampleRate := Self.SampleRate;
     Frequency := Self.Frequency / 1.99;
     Ripple := 0.1;
    end;

   with TFastPinkNoiseGenerator.Create do
    try
     // quantize frequency
     Amplitude := dB_to_Amp(-0.1);
     SampleRate := Self.SampleRate;

     // process channels
     for Channel := 0 to ChannelCount - 1 do
      begin
       Lowpass.ResetStates;
       Highpass.ResetStates;

       Scale := dB_to_Amp(-100);

       // calculate IR size for the lowpass
       IRSize[0] := 0;
       Lowpass.ProcessSample64(1.0);
       while Lowpass.ProcessSample64(0.0) > Scale
        do Inc(IRSize[0]);

       // calculate IR size for the highpass
       IRSize[1] := 0;
       Highpass.ProcessSample64(1.0);
       while Highpass.ProcessSample64(0.0) > Scale
        do Inc(IRSize[1]);

       // first pass (get system into the mood)
       for Sample := 0 to Max(IRSize[0], IRSize[1]) - 1
        do Highpass.ProcessSample64(LowPass.ProcessSample64(ProcessSample64));

       for Sample := 0 to SampleFrames - 1
        do ChannelDataPointer[Channel]^[Sample] := Lowpass.ProcessSample64(
          Highpass.ProcessSample64(ProcessSample64));
      end;
    finally
     Free;
    end;
   Normalize;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildWhiteNoise;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;
   SampleFrames := Round(SampleRate * 1E-3 * SeLength.Value);
   GenerateWhiteNoise(dB_to_Amp(-0.1));
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BuildPinkNoise;
var
  Channel : Integer;
begin
 with AudioDataCollection do
  begin
   ChannelCount := SeChannels.Value;
   SampleFrames := Round(SampleRate * 1E-3 * SeLength.Value);
   with TFastPinkNoiseGenerator.Create do
    try
     for Channel := 0 to ChannelCount - 1
      do ProcessBlock32(ChannelDataPointer[Channel], SampleFrames);
    finally
     Free;
    end;
   SaveToFile(EdFileName.Text);
  end;
end;

procedure TFmContinousExcitationSignalBuilder.BtExitClick(Sender: TObject);
begin
 Close;
end;

end.
