unit AudioSheet_Page;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, ComCtrls, Menus, DAV_AudioFileWAV, DAV_AudioFileAIFF,
  DAV_AudioFileAU, DAV_AudioData, DAV_GuiAudioDataDisplay, DAV_Classes;

type
  TChartOverlay = (coNone, coReverb, coDistortion);
  TFmPage = class(TForm)
    PCInfo: TPageControl;
    TSBasicInformation: TTabSheet;
    LbSamplerate: TLabel;
    LbSamplerateValue: TLabel;
    LbSampleLength: TLabel;
    LbSampleLengthValue: TLabel;
    LbChannels: TLabel;
    LbChannelsValue: TLabel;
    LbBitsPerSampleValue: TLabel;
    LbBitsPerSample: TLabel;
    TSSignalProperties: TTabSheet;
    SignalMemo: TMemo;
    ADD: TGuiAudioDataDisplay;
    ADC: TAudioDataCollection32;
  private
    FFileName: TFileName;
    FRT60, FD50, FC50 : Double;
    FRT60Left         : Double;
    FRT60Right        : Double;
    FChartOverlay     : TChartOverlay;
    procedure SetFileName(const Value: TFileName);
    procedure PostProcessing;
  published
    property RT60 : Double read FRT60;
    property D50 : Double read FD50;
    property C50 : Double read FC50;
  public
    property FileName : TFileName read FFileName write SetFileName;
  end;

implementation

{$R *.dfm}

uses
  DAV_Types, Math;

procedure TFmPage.PostProcessing;
var
  i, j, c     : Integer;
  cd          : Cardinal;
  RMS         : array [0..1] of Double;
  ZC, LC, DC  : Double;
  FilteredDbl : TDAVDoubleFixedArray;
begin
(*
 with MFTimeDomain do
 if (Count > 0) and (MaximumTime > 0) then
  begin
   SignalMemo.Clear;
   if MaximumTime <= 4194304 then
    begin
     MFFFTRealDouble.FFTLength := Round(IntPower(2, Round(Log2(MaximumTime) - 0.5)));
     MFFreqDomain.FFTSize := MFFFTRealDouble.FFTLength;
     MFFreqDomain.Count := Count;
     for i := 0 to Count - 1 do
      begin
       // Check for Time Signal
       RMS[0] := sqr(MFTimeDomain[i].Data[0]);
       for j := 1 to (MaximumTime div 2) - 1
        do RMS[0] := RMS[0] + sqr(MFTimeDomain[i].Data[j]);

       DC := MFTimeDomain[i].Data[0];
       for j := 1 to MaximumTime - 1
        do DC := DC + MFTimeDomain[i].Data[j];
       DC := DC / MaximumTime;
       SignalMemo.Lines.Add('Channel ' + IntToStr(i + 1) + ' DC: '
                            + FloatToStrF(DC, ffGeneral, 3, 2) + ' (= '
                            + FloatToStrF(Amp_to_dB(abs(DC)), ffGeneral, 3, 2) + 'dB)');

       RMS[1] := sqr(MFTimeDomain[i].Data[(MaximumTime div 2)]);
       for j := (MaximumTime div 2) + 1 to MaximumTime - 1
        do RMS[1] := RMS[1] + sqr(MFTimeDomain[i].Data[j]);
       MFTimeDomain.IsImpulseResponse := RMS[0] > 0.5 * RMS[1];

       MFFFTRealDouble.Do_FFT_only(@MFFreqDomain[i].Data[0], @MFTimeDomain[i].Data[0]);
       if abs(RMS[0] - RMS[1]) < 0.1
        then MFFFTRealDouble.RescaleA_Only(@MFFreqDomain[i].Data[0]);
       if (abs(RMS[0] - RMS[1]) < 0.05) and (Count = 1) then
        begin
         cd := 1;
         c := 0;
         LC := 0;
         ZC := 0;
         while cd < MaximumTime do
          begin
           if (MFTimeDomain[i].Data[cd-1] < 0) and (MFTimeDomain[i].Data[cd] > 0) then
            begin
             ZC := ZC * c / (c + 1) + (cd - LC) * 1 / (c + 1);
             LC := cd;
             inc(c);
             inc(cd, 2);
            end
           else inc(cd);
          end;
         MFAC.FrequencyHighlight.Frequency := 44100 / ZC;
         MFAC.FrequencyHighlight.Select := fsFrequency;
        end
       else
        begin
         SetLength(FilteredDbl, MFTimeDomain.MaximumTime);

         for j := 0 to MFTimeDomain.MaximumTime-1 do
          begin
           FilteredDbl[j] := 0;
           for c := 0 to MFTimeDomain.Channels.Count - 1
            do FilteredDbl[j] := FilteredDbl[j] + MFTimeDomain[c].Data[j];
          end;

         with TMFSchroederData.Create(FilteredDbl, MFFreqDomain.SampleRate) do
          try
           FRT60        := RT60;
           FD50         := D50;
           FC50         := C50;
           FRT60Left.X  := StartSample;
           FRT60Left.Y  := StartValue;
           FRT60Right.X := EndSample;
           FRT60Right.Y := EndValue;
          finally
           Free;
          end;
        end;
       MFFFTRealDouble.ReIm2Abs_Only(@MFFreqDomain[i].Data[0], @MFFreqDomain[i].Magnitude[0]);
      end;
    end
   else
    begin
     CBAntiAlias.Checked := False;
     MFAC.DrawingStyle := dsFast;
    end;
  end;
*)
end;

procedure TFmPage.SetFileName(const Value: TFileName);
begin
 ADC.LoadFromFile(Value);
end;

end.
