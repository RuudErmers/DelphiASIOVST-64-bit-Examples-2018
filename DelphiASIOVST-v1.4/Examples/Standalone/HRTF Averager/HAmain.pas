unit HAmain;

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
{$DEFINE Use_IPPS}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, Spin, ComCtrls,
  DAV_GuiAudioDataDisplay, DAV_DspHrtf, DAV_AudioData, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS, DAV_Classes{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  THrtfInterpolation = (hiTimeDomain, hiFrequencyDomain);
  TFmHrtfAverager = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MISelectDirectory: TMenuItem;
    MISaveAs: TMenuItem;
    N1: TMenuItem;
    MIExit: TMenuItem;
    AudioDataDisplayLeft: TGuiAudioDataDisplay;
    AudioDataDisplayRight: TGuiAudioDataDisplay;
    LbPolarUnit: TLabel;
    SEPolar: TSpinEdit;
    LbPolar: TLabel;
    LbAzimuthUnit: TLabel;
    SEAzimuth: TSpinEdit;
    LbAzimuth: TLabel;
    SEHrtfIndex: TSpinEdit;
    LbHrtfIndex: TLabel;
    ADHRIR: TAudioDataCollection32;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    procedure MIExitClick(Sender: TObject);
    procedure MISelectDirectoryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure SEHrtfIndexChange(Sender: TObject);
    procedure SEAzimuthChange(Sender: TObject);
    procedure SEPolarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FHRTFFile          : THrtfs;
    FHrtfInterpolation : THrtfInterpolation;
    {$IFDEF Use_IPPS}
    FFft               : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft               : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft               : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure HRTFFileChanged;

    property HrtfInterpolation: THrtfInterpolation read FHrtfInterpolation write FHrtfInterpolation default hiFrequencyDomain;
  end;

var
  FmHrtfAverager: TFmHrtfAverager;

implementation

{$R *.dfm}

uses
  DAV_Common, DAV_Complex, FileCtrl;

const
  CDegToRad         : Single = 2 * Pi / 360;
  SCRound8087CW     : Word = $133F; // round FPU codeword, with exceptions disabled
  SCChop8087CW      : Word = $1F3F; // Trunc (chop) FPU codeword, with exceptions disabled
  SCRoundDown8087CW : Word = $173F; // exceptions disabled
  SCRoundUp8087CW   : Word = $1B3F; // exceptions disabled

procedure TFmHrtfAverager.FormCreate(Sender: TObject);
begin
 FHRTFFile := THrtfs.Create;
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(6);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FHrtfInterpolation := hiFrequencyDomain;

 DontRaiseExceptionsAndSetFPUcodeword;
end;

procedure TFmHrtfAverager.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FHRTFFile);
 FreeAndNil(FFft);
end;

procedure TFmHrtfAverager.FormResize(Sender: TObject);
begin
 AudioDataDisplayLeft.Height := (ClientHeight - SEHrtfIndex.Height - 48) div 2;
 AudioDataDisplayRight.Height := AudioDataDisplayLeft.Height;
 AudioDataDisplayRight.Top := AudioDataDisplayLeft.Top + AudioDataDisplayLeft.Height + 6;
end;

procedure TFmHrtfAverager.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmHrtfAverager.MISaveAsClick(Sender: TObject);
begin
 with SaveDialog do
  if Execute
   then FHRTFFile.SaveToFile(FileName);
end;

procedure TFmHrtfAverager.MISelectDirectoryClick(Sender: TObject);
var
  Dir     : string;
  SR      : TSearchRec;
  HRTF    : THrtfs;
  Hrir    : TCustomHrir;
  ADC     : TAudioDataCollection32;
  HRTFCnt : Integer;
  Sample  : Integer;
  Channel : Integer;
  i       : Integer;
  CurArg  : Double;
  Phase   : array [0..1] of Double;
  Scale   : array [0..1] of Double;
  FreqDom : array [0..1] of PDAVComplexSingleFixedArray;
begin
 Dir := 'C:\Users\Christian Budde\Projects\VSTPack\Resources\HRTFs';
 HRTFCnt := 0;
 SelectDirectory('Select a directory', '', Dir);

 if DirectoryExists(Dir) then
  begin
   FHRTFFile.ClearHrirs;
   HRTF := THrtfs.Create;
   try
    ADC := TAudioDataCollection32.Create(Self);
    try
     // set stereo
     ADC.ChannelCount := 2;

     // nil frequency domain memory pointers
     FreqDom[0] := nil;
     FreqDom[1] := nil;

     if FindFirst(Dir + '\*.HRTF', 0, SR) = 0 then
      repeat

       // load HRTF file
       HRTF.ClearHrirs;
       HRTF.LoadFromFile(Dir + '\' + SR.Name);

       // inform user
       StatusBar.SimpleText := SR.Name;
       Application.ProcessMessages;

       // if HRTF file contains no data continue to next HRTF
       if HRTF.HrirCount = 0 then Continue;

       inc(HRTFCnt);

       if FHRTFFile.HrirCount = 0 then
        begin
         for i := 0 to HRTF.HrirCount - 1 do
//          if assigned(HRTF.Hrir[i]) then
           begin
            Hrir := TCustomHrir.Create;
            Hrir.Assign(HRTF.Hrir[i]);
            FHRTFFile.AddChunk(Hrir);
           end;
        end
       else
        begin
         // calculate scale factors
         Scale[0] := 1 / HRTFCnt;
         Scale[1] := 1 - Scale[0];

         for i := 0 to HRTF.HrirCount - 1 do
          begin
           ADC.SampleFrames := HRTF.Hrir[i].SampleFrames;
           HRTF.GetHrirByIndex(i, ADC.SampleFrames, ADC[0].ChannelDataPointer,
             ADC[1].ChannelDataPointer);

           ADHRIR.SampleFrames := ADC.SampleFrames;
           FHRTFFile.InterpolateHrir(HRTF.Hrir[i].Azimuth, HRTF.Hrir[i].Polar,
             ADC.SampleFrames, ADHRIR[0].ChannelDataPointer,
             ADHRIR[1].ChannelDataPointer);

           // allocate memory for frequency domain
           ReallocMem(FreqDom[0], (ADHRIR.SampleFrames div 2 + 1) * SizeOf(Single));
           ReallocMem(FreqDom[1], (ADHRIR.SampleFrames div 2 + 1) * SizeOf(Single));
           FFft.FFTSize := ADHRIR.SampleFrames;

           // actual interpolation
           case FHrtfInterpolation of
            hiTimeDomain :
             for Channel := 0 to ADHRIR.ChannelCount - 1 do
              for Sample := 0 to ADHRIR.SampleFrames - 1 do
               begin
                ADHRIR[Channel].ChannelDataPointer^[Sample] :=
                  Scale[0] * ADC[Channel].ChannelDataPointer^[Sample] +
                  Scale[1] * ADHRIR[Channel].ChannelDataPointer^[Sample];
               end;
            hiFrequencyDomain :
             for Channel := 0 to ADHRIR.ChannelCount - 1 do
              begin
               FFft.AutoScaleType := astDivideFwdByN;
               // transferm to frequency domain
               FFft.PerformFFT(FreqDom[0], ADC[Channel].ChannelDataPointer);
               FFft.PerformFFT(FreqDom[1], ADHRIR[Channel].ChannelDataPointer);

               // average DC
               FreqDom[1]^[0].Re := Scale[0] * FreqDom[0]^[0].Re +
                                    Scale[1] * FreqDom[1]^[0].Re;

               Phase[0] := 0;
               Phase[1] := 0;

               for Sample := 1 to (ADHRIR.SampleFrames div 2) - 1 do
                begin
                 CurArg := ComplexArgument32(FreqDom[0]^[Sample]);
                 while CurArg + Pi < Phase[0] do CurArg := CurArg + 2 * Pi;
                 while CurArg - Pi > Phase[0] do CurArg := CurArg - 2 * Pi;
                 Phase[0] := CurArg;

                 CurArg := ComplexArgument32(FreqDom[0]^[Sample]);
                 while CurArg + Pi < Phase[1] do CurArg := CurArg + 2 * Pi;
                 while CurArg - Pi > Phase[1] do CurArg := CurArg - 2 * Pi;
                 Phase[1] := CurArg;

                 FreqDom[1]^[Sample] := ComplexPolar32(
                   Scale[0] * ComplexMagnitude32(FreqDom[0]^[Sample]) +
                   Scale[1] * ComplexMagnitude32(FreqDom[1]^[Sample]),
                   Scale[0] * CurArg + Scale[1] * CurArg);
                end;

               // average Nyquist
               FreqDom[1]^[(ADHRIR.SampleFrames div 2)].Re :=
                 Scale[0] * FreqDom[0]^[(ADHRIR.SampleFrames div 2)].Re +
                 Scale[1] * FreqDom[1]^[(ADHRIR.SampleFrames div 2)].Re;

               // average Nyquist (alt. coding)
               FreqDom[1]^[0].Im := Scale[0] * FreqDom[0]^[0].Im +
                                    Scale[1] * FreqDom[1]^[0].Im;

               // transferm back to time domain
               FFft.PerformIFFT(FreqDom[1], ADHRIR[Channel].ChannelDataPointer);
              end;
           end;

           FHRTFFile.Hrir[i].AssignLeft32(ADHRIR[0].ChannelDataPointer, ADC.SampleFrames);
           FHRTFFile.Hrir[i].AssignRight32(ADHRIR[1].ChannelDataPointer, ADC.SampleFrames);
          end;
        end;
      until FindNext(SR) <> 0;
     FindClose(sr);

     // dispose memory
     Dispose(FreqDom[0]);
     Dispose(FreqDom[1]);
    finally
     FreeAndNil(ADC);
    end;
   finally
    FreeAndNil(HRTF);
   end;
  end;
 StatusBar.SimpleText := '';
 HRTFFileChanged;
end;

procedure TFmHrtfAverager.SEAzimuthChange(Sender: TObject);
begin
 FHRTFFile.InterpolateHrir(SEAzimuth.Value * CDegToRad,
   SEPolar.Value * CDegToRad, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
end;

procedure TFmHrtfAverager.SEHrtfIndexChange(Sender: TObject);
begin
 assert(SEHrtfIndex.Value < FHRTFFile.HrirCount);
 FHRTFFile.GetHrirByIndex(SEHrtfIndex.Value, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
 with FHRTFFile.Hrir[SEHrtfIndex.Value] do
  begin
   SEAzimuth.Value := round(360 / (2 * Pi) * Azimuth);
   SEPolar.Value := round(360 / (2 * Pi) * Polar);
  end;
end;

procedure TFmHrtfAverager.SEPolarChange(Sender: TObject);
begin
 FHRTFFile.InterpolateHrir(SEAzimuth.Value * CDegToRad,
   SEPolar.Value * CDegToRad, ADHRIR.SampleFrames,
   ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
 AudioDataDisplayLeft.Invalidate;
 AudioDataDisplayRight.Invalidate;
end;

procedure TFmHrtfAverager.HRTFFileChanged;
begin
 LbHrtfIndex.Enabled   := FHRTFFile.HrirCount > 0;
 SEHrtfIndex.Enabled   := LbHrtfIndex.Enabled;
 LbAzimuth.Enabled     := LbHrtfIndex.Enabled;
 SEAzimuth.Enabled     := LbHrtfIndex.Enabled;
 LbPolar.Enabled       := LbHrtfIndex.Enabled;
 SEPolar.Enabled       := LbHrtfIndex.Enabled;
 LbPolarUnit.Enabled   := LbHrtfIndex.Enabled;
 LbAzimuthUnit.Enabled := LbHrtfIndex.Enabled;

 if SEHrtfIndex.Enabled
  then SEHrtfIndex.MaxValue := FHRTFFile.HrirCount - 1;

 AudioDataDisplayLeft.Update;
 AudioDataDisplayRight.Update;
end;

end.
