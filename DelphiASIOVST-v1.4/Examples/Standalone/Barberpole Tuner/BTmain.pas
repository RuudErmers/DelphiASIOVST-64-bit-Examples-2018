unit BTmain;

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

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, ExtCtrls, Dialogs, DAV_Types, DAV_ASIOHost,  
  DAV_DspLightweightDynamics, DAV_DspBarberpoleTuner, DAV_DspFilterButterworth, 
  DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiGraphicControl;

type
  TGuitarString = (gtLowE, gtA, gtD, gtG, gtH, gtE);
  TFmBarberpoleTuner = class(TForm)
    ASIOHost: TASIOHost;
    Barberpole: TPaintBox;
    LbA: TGuiLabel;
    LbD: TGuiLabel;
    LbDisplay: TGuiLabel;
    LbE: TGuiLabel;
    LbG: TGuiLabel;
    LbGuitarTuning: TGuiLabel;
    LbH: TGuiLabel;
    LbLowE: TGuiLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject;
      const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BarberpolePaint(Sender: TObject);
    procedure LbNoteClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBackground       : TGuiCustomPixelMap;
    FLowpass          : TButterworthLowPassFilter;
    FLimiter          : array [0..1] of TLightweightSoftKneeLimiter;
    FBarberpoleFilter : TBarberpoleFilter;
    FDownSamplePos    : Integer;
    FDownSampleCount  : Integer;
    FLinearBuffer     : PDAVSingleFixedArray;
    FGuitarString     : TGuitarString;
    procedure SetGuitarString(const Value: TGuitarString);
  protected  
    procedure GuitarStringChanged; virtual;
  public
    property BufferPointer: PDAVSingleFixedArray read FLinearBuffer;
    property GuitarString: TGuitarString read FGuitarString write SetGuitarString;
  end;

var
  FmBarberpoleTuner: TFmBarberpoleTuner;

implementation

uses
  DAV_Common, DAV_GuiCommon, DAV_DspDynamics;

resourcestring
  RCStrASIO4ALLV2NotFound = 'ASIO4ALL v2 not found!';

{$R *.dfm}

procedure TFmBarberpoleTuner.FormCreate(Sender: TObject);
var
  DrvIndx : Integer;
begin
 // create and setup initial lowpass filter
 FLowpass := TButterworthLowPassFilter.Create(3);
 with FLowpass do
  begin
   SampleRate := ASIOHost.SampleRate;
   Frequency := 8000;
  end;

 // create and setup barberpole tuner filter
 FBarberpoleFilter := TBarberpoleFilter.Create;
 FBarberpoleFilter.SampleRate := ASIOHost.SampleRate;

 FLimiter[0] := TLightweightSoftKneeLimiter.Create;
 with FLimiter[0] do
  begin
   SampleRate := ASIOHost.SampleRate;
   Attack := 0.1;
   Release := 200;
   Threshold_dB := -40;
   AutoMakeUp := True;
   Knee_dB := 2;
  end;

 FLimiter[1] := TLightweightSoftKneeLimiter.Create;
 with FLimiter[1] do
  begin
   SampleRate := ASIOHost.SampleRate;
   Attack := 0.1;
   Release := 200;
   Threshold_dB := -20;
   MakeUpGain_dB := 40;
   Knee_dB := 2;
  end;

 FDownSamplePos    := 0;
 FDownSampleCount  := 1 shl 8;
 GetMem(FLinearBuffer, 256 * SizeOf(Single));

 // Create Background Image
 FBackground := TGuiPixelMapMemory.Create;
 Barberpole.ControlStyle := Barberpole.ControlStyle + [csOpaque];
 FormResize(Sender);

 try
  DrvIndx := ASIOHost.DriverList.IndexOf('ASIO4ALL v2');
  if DrvIndx < 0
   then raise Exception.Create(RCStrASIO4ALLV2NotFound);
  ASIOHost.DriverIndex := DrvIndx;
  ASIOHost.Active := True;
 except
  on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
 end;
end;

procedure TFmBarberpoleTuner.FormDestroy(Sender: TObject);
begin
 ASIOHost.Active := False;

 FreeAndNil(FBarberpoleFilter);
 FreeAndNil(FBackground);
 FreeAndNil(FLowpass);
 FreeAndNil(FLimiter[0]);
 FreeAndNil(FLimiter[1]);
end;

procedure TFmBarberpoleTuner.FormShow(Sender: TObject);
begin
 // workaround, please remove these lines if fixed!!!
 LbDisplay.Width := LbDisplay.Width + 1;
 LbGuitarTuning.Width := LbGuitarTuning.Width + 1;
 LbGuitarTuning.Width := LbGuitarTuning.Width + 1;
 LbLowE.Height := LbLowE.Height - 1;
 LbA.Height := LbA.Height - 1;
 LbD.Height := LbD.Height - 1;
 LbG.Height := LbG.Height - 1;
 LbH.Height := LbH.Height - 1;
 LbE.Height := LbE.Height - 1;
end;

procedure TFmBarberpoleTuner.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmBarberpoleTuner.FormResize(Sender: TObject);
var
  x, y    : Integer;
  Filter  : array [0..1] of Single;
  h, hr   : Single;
  ScnLn   : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLn[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLn[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmBarberpoleTuner.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if Assigned(FBarberpoleFilter) then FBarberpoleFilter.SampleRate := ASIOHost.SampleRate;
 if Assigned(FLowpass) then FLowpass.SampleRate := ASIOHost.SampleRate;
 if Assigned(FLimiter[0]) then FLimiter[0].SampleRate := ASIOHost.SampleRate;
 if Assigned(FLimiter[1]) then FLimiter[1].SampleRate := ASIOHost.SampleRate;
end;

procedure TFmBarberpoleTuner.BarberpolePaint(Sender: TObject);
var
  Column : Integer;
begin
 with Barberpole.Canvas do
  begin
   Pen.Color := clBlack;
   Pen.Style := psSolid;
   Brush.Color := clBlack;
   Brush.Style := bsSolid;
   FrameRect(Barberpole.ClientRect);
  end;

 if Assigned(BufferPointer) then
  for Column := 0 to Barberpole.Width - 3 do
   begin
    Barberpole.Canvas.Pen.Color :=
      Round($70 - $34 * BufferPointer^[Column]) shl 16 +
      Round($84 - $48 * BufferPointer^[Column]) shl  8 +
      Round($8D - $50 * BufferPointer^[Column]);
    Barberpole.Canvas.MoveTo(Column + 1, 1);
    Barberpole.Canvas.LineTo(Column + 1, Barberpole.Height - 1);
   end;
end;

procedure TFmBarberpoleTuner.LbNoteClick(Sender: TObject);
begin
 if Sender <> LbLowE then LbLowE.Font.Color := $4F4F4F else begin LbLowE.Font.Color := clBlack; GuitarString := gtLowE; end;
 if Sender <> LbA then LbA.Font.Color := $4F4F4F else begin LbA.Font.Color := clBlack; GuitarString := gtA; end;
 if Sender <> LbD then LbD.Font.Color := $4F4F4F else begin LbD.Font.Color := clBlack; GuitarString := gtD; end;
 if Sender <> LbG then LbG.Font.Color := $4F4F4F else begin LbG.Font.Color := clBlack; GuitarString := gtG; end;
 if Sender <> LbH then LbH.Font.Color := $4F4F4F else begin LbH.Font.Color := clBlack; GuitarString := gtH; end;
 if Sender <> LbE then LbE.Font.Color := $4F4F4F else begin LbE.Font.Color := clBlack; GuitarString := gtE; end;
end;

procedure TFmBarberpoleTuner.SetGuitarString(const Value: TGuitarString);
begin
 if FGuitarString <> Value then
  begin
   FGuitarString := Value;
   GuitarStringChanged;
  end;
end;

procedure TFmBarberpoleTuner.GuitarStringChanged;
var
  CenterFrequency : Single;
begin
 case FGuitarString of
  gtLowE : CenterFrequency := 329.62755691286992973584176104656;
  gtA    : CenterFrequency := 440;
  gtD    : CenterFrequency := 587.32953583481512052556602772116;
  gtG    : CenterFrequency := 783.99087196349858817139906091965;
  gtH    : CenterFrequency := 987.76660251224822366150908371768;
  gtE    : CenterFrequency := 1318.5102276514797189433670441862;
  else raise Exception.Create('Current Frequency doesn''t exist');
 end;

 if Assigned(FBarberpoleFilter)
  then FBarberpoleFilter.Frequency := CenterFrequency;
 if Assigned(FLowpass)
  then FLowpass.Frequency := 4 * CenterFrequency; 
end;

procedure TFmBarberpoleTuner.TimerTimer(Sender: TObject);
begin
 Barberpole.Invalidate;
end;

procedure TFmBarberpoleTuner.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample, c1 : Integer;
  Signal     : Single;
begin
 c1 := 1;
 for Sample := 0 to ASIOHost.BufferSize - 1 do
  begin
   Signal := FLimiter[0].ProcessSample64(FLowpass.ProcessSample64(InBuffer[0, Sample]));
   Signal := FBarberpoleFilter.ProcessSample32(Signal + 2 * sqr(Signal) - 1);
   Signal := FLimiter[1].ProcessSample64(Signal);
   if FDownSamplePos = 0 then
    begin
     Move(FLinearBuffer^[0], FLinearBuffer^[c1], 255 * SizeOf(Single));
     FLinearBuffer^[0] := Limit(Signal, -1, 1);
    end;

   // advance downsample position
   Inc(FDownSamplePos);
   if FDownSamplePos >= FDownSampleCount
    then FDownSamplePos := 0;
  end;
end;

initialization
  DontRaiseExceptionsAndSetFPUcodeword;

end.
