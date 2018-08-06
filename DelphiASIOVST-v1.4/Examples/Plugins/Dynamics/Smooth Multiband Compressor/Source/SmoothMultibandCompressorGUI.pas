unit SmoothMultibandCompressorGUI;

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
  Forms, Types, Controls, ExtCtrls, GR32, GR32_Image, Graphics, GraphicEx, 
  DAV_Types, DAV_VSTModule;

const
  cSweepRange = 10;
  cFadeSpeed = 11;
  cFadeSpeedH = cFadeSpeed div 2;
  cAboutScrollLength = 700;

type
  TVerticallyStitchedBitmap32 = class(TBitmap32)
  private
    FGlyphCount: Integer;
    function GetRealHeight: Integer;
    procedure SetGlyphCount(const Value: Integer);
  public
    constructor Create; override;
  published
    property RealWidth : Integer read FWidth;
    property RealHeight : Integer read GetRealHeight;
    property GlyphCount : Integer read FGlyphCount write SetGlyphCount;
  end;

  TEditValue = (edNone, edLow, edHigh, edAttack, edRelease, edThreshold,
    edRatio, edKnee, edMakeUpGain, edVolume);

  TGraph = (gLow, gLowMid, gHighMid, gHigh);
  TGraphs = set of TGraph;
  TFmSmoothMultibandCompressor = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    FBackground   : array [0..1] of TBitmap32;

    FStage        : Integer;
    FTimer        : TTimer;
    FEditValue    : TEditValue;
    FAnimateFrame : Integer;
    procedure RestitchPNG2Bitmap32(const PNG: TPNGGraphic;
      const BMP: TVerticallyStitchedBitmap32);

    procedure DrawAll;
    procedure DrawLowFrequency;
    procedure DrawHighFrequency;
    procedure DrawOutputGain;
    procedure DrawAttack(const Stage: Integer);
    procedure DrawRelease(const Stage: Integer);
    procedure DrawThreshold(const Stage: Integer);
    procedure DrawRatio(const Stage: Integer);
    procedure DrawKnee(const Stage: Integer);
    procedure DrawMakeUpGain(const Stage: Integer);
    procedure DrawState(const Stage: Integer);
    procedure DrawSoftClip;
    procedure DrawGraph(const Stage: Integer);
    procedure DrawBypass(const Stage: Integer);
  public
    procedure UpdateLowFrequency;
    procedure UpdateOutputGain;
    procedure UpdateHighFrequency;
    procedure UpdateLimit;
    procedure UpdateLowAttack;
    procedure UpdateLowAutoMakeUpGain;
    procedure UpdateLowKnee;
    procedure UpdateLowMakeUp;
    procedure UpdateLowRatio;
    procedure UpdateLowRelease;
    procedure UpdateLowThreshold;
    procedure UpdateLowState;
    procedure UpdateMidAttack;
    procedure UpdateMidAutoMakeUpGain;
    procedure UpdateMidKnee;
    procedure UpdateMidMakeUp;
    procedure UpdateMidRatio;
    procedure UpdateMidRelease;
    procedure UpdateMidThreshold;
    procedure UpdateMidState;
    procedure UpdateHighAttack;
    procedure UpdateHighAutoMakeUpGain;
    procedure UpdateHighKnee;
    procedure UpdateHighMakeUp;
    procedure UpdateHighRatio;
    procedure UpdateHighRelease;
    procedure UpdateHighThreshold;
    procedure UpdateHighState;
  end;

var
  GBG           : TVerticallyStitchedBitmap32;
  GKnob         : TVerticallyStitchedBitmap32;
  GSoftClip     : TVerticallyStitchedBitmap32;
  GAutoGain     : TVerticallyStitchedBitmap32;
  GRectStage    : array [0..2, 0..8] of TRect;
  GRectTop      : array [0..3] of TRect;

implementation

uses
  Math, GR32_Backends, DAV_Common, DAV_VSTModuleWithPrograms,
  SmoothMultibandCompressorDM;

procedure TFmSmoothMultibandCompressor.FormCreate(Sender: TObject);
var
  tmp   : TPNGGraphic;
  RS    : TResourceStream;
  Stage : Integer;
  Param : Integer;
begin
 FTimer := TTimer.Create(Self);
 with FTimer do
  begin
   Interval := 28;
   OnTimer := TimerTimer;
  end;

 ControlStyle    := ControlStyle + [csOpaque];
 FBackground[1]  := TBitmap32.Create;
 FBackground[0]  := TBitmap32.Create;
 FBackground[0].Font.Name := 'Arial';
 FAnimateFrame := 0;

 if not Assigned(GBG) then
  begin
   tmp := TPNGGraphic.Create;
   try
    RS  := TResourceStream.Create(hInstance, 'LPMCBackground', 'PNG');
    try
     tmp.LoadFromStream(RS);
     GBG := TVerticallyStitchedBitmap32.Create;
     GBG.Assign(tmp);

    finally
     RS.Free;
    end;
   finally
    tmp.Free;
   end;

   GRectStage[0, 6] := Rect(620, 109, 698, 170);
   GRectStage[1, 6] := Rect(620, 216, 698, 277);
   GRectStage[2, 6] := Rect(620, 318, 698, 379);
  end;

 if not Assigned(GKnob) then
  begin
   tmp := TPNGGraphic.Create;
   try
    RS  := TResourceStream.Create(hInstance, 'LPMCKnob', 'PNG');
    try
     tmp.LoadFromStream(RS);
     GKnob := TVerticallyStitchedBitmap32.Create;
     GKnob.DrawMode := dmBlend;
     GKnob.GlyphCount := 64;
     GKnob.Assign(tmp);
//     RestitchPNG2Bitmap32(tmp, GKnob);
     GKnob.Font.Assign(tmp.Canvas.Font);
    finally
     RS.Free;
    end;
   finally
    tmp.Free;
   end;

   // Calculate Rects
   GRectTop[0].TopLeft := Point(416, 25);
   GRectTop[1].TopLeft := Point(520, 25);
   GRectTop[2].TopLeft := Point(628, 25);

   GRectTop[0].Right  := GRectTop[0].Left + GKnob.RealWidth;
   GRectTop[0].Bottom := GRectTop[0].Top + GKnob.RealHeight;
   GRectTop[1].Right  := GRectTop[1].Left + GKnob.RealWidth;
   GRectTop[1].Bottom := GRectTop[1].Top + GKnob.RealHeight;
   GRectTop[2].Right  := GRectTop[2].Left + GKnob.RealWidth;
   GRectTop[2].Bottom := GRectTop[2].Top + GKnob.RealHeight;

   GRectStage[0, 0].TopLeft := Point(144, 124);
   GRectStage[0, 1].TopLeft := Point(220, 124);
   GRectStage[0, 2].TopLeft := Point(313, 124);
   GRectStage[0, 3].TopLeft := Point(402, 124);
   GRectStage[0, 4].TopLeft := Point(480, 124);
   GRectStage[0, 5].TopLeft := Point(558, 124);

   GRectStage[1, 0].TopLeft := Point(144, 230);
   GRectStage[1, 1].TopLeft := Point(220, 230);
   GRectStage[1, 2].TopLeft := Point(313, 230);
   GRectStage[1, 3].TopLeft := Point(402, 230);
   GRectStage[1, 4].TopLeft := Point(480, 230);
   GRectStage[1, 5].TopLeft := Point(558, 230);

   GRectStage[2, 0].TopLeft := Point(142, 332);
   GRectStage[2, 1].TopLeft := Point(220, 332);
   GRectStage[2, 2].TopLeft := Point(313, 332);
   GRectStage[2, 3].TopLeft := Point(402, 332);
   GRectStage[2, 4].TopLeft := Point(480, 332);
   GRectStage[2, 5].TopLeft := Point(558, 332);

   for Stage := 0 to Length(GRectStage) - 1 do
    for Param := 0 to Length(GRectStage[Stage]) - 3 do
     begin
      GRectStage[Stage, Param].Right  := GRectStage[Stage, Param].Left + GKnob.RealWidth;
      GRectStage[Stage, Param].Bottom := GRectStage[Stage, Param].Top + GKnob.RealHeight;
     end;

  end;

  if not Assigned(GSoftClip) then
   begin
    tmp := TPNGGraphic.Create;
    try
     RS  := TResourceStream.Create(hInstance, 'LPMCSoftClip', 'PNG');
     try
      tmp.LoadFromStream(RS);
      GSoftClip := TVerticallyStitchedBitmap32.Create;
      GSoftClip.DrawMode := dmBlend;
      GSoftClip.GlyphCount := 2;
      RestitchPNG2Bitmap32(tmp, GSoftClip);
      GSoftClip.Font.Assign(tmp.Canvas.Font);
     finally
      RS.Free;
     end;
    finally
     tmp.Free;
    end;

    // Calculate Rects
    GRectTop[3].TopLeft := Point(713, 21);
    GRectTop[3].Right   := GRectTop[3].Left + GSoftClip.RealWidth;
    GRectTop[3].Bottom  := GRectTop[3].Top + GSoftClip.RealHeight;
   end;

  if not Assigned(GAutoGain) then
   begin
    tmp := TPNGGraphic.Create;
    try
     RS  := TResourceStream.Create(hInstance, 'LPMCAutoGain', 'PNG');
     try
      tmp.LoadFromStream(RS);
      GAutoGain := TVerticallyStitchedBitmap32.Create;
      GAutoGain.DrawMode := dmBlend;
      GAutoGain.GlyphCount := 2;
      RestitchPNG2Bitmap32(tmp, GAutoGain);
      GAutoGain.Font.Assign(tmp.Canvas.Font);
     finally
      RS.Free;
     end;
    finally
     tmp.Free;
    end;

    // Calculate Rects
    GRectStage[0, 7].TopLeft := Point(717, 125);
    GRectStage[1, 7].TopLeft := Point(717, 230);
    GRectStage[2, 7].TopLeft := Point(717, 334);

    GRectStage[0, 7].Right   := GRectStage[0, 7].Left + GAutoGain.RealWidth;
    GRectStage[0, 7].Bottom  := GRectStage[0, 7].Top + GAutoGain.RealHeight;
    GRectStage[1, 7].Right   := GRectStage[1, 7].Left + GAutoGain.RealWidth;
    GRectStage[1, 7].Bottom  := GRectStage[1, 7].Top + GAutoGain.RealHeight;
    GRectStage[2, 7].Right   := GRectStage[2, 7].Left + GAutoGain.RealWidth;
    GRectStage[2, 7].Bottom  := GRectStage[2, 7].Top + GAutoGain.RealHeight;

    // Calculate Rects
    GRectStage[0, 8] := Rect(30,  86, 772, 188);
    GRectStage[1, 8] := Rect(30, 193, 772, 293);
    GRectStage[2, 8] := Rect(30, 297, 772, 397);
   end;

 FEditValue := edNone;
end;

procedure TFmSmoothMultibandCompressor.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FTimer);
 FreeAndNil(FBackground[0]);
 FreeAndNil(FBackground[1]);
end;

procedure TFmSmoothMultibandCompressor.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pml   : Integer;
  Pt    : TPoint;
  Stage : Integer;
begin
 with TSmoothMultibandCompressorDataModule(Owner) do
  begin
   if Button = mbLeft
    then MidiLearnParameter := -1;
   pml := MidiLearnParameter;
   Pt := Point(X, Y);
   SetFocus;

   if (x < 10) and (y < 10) then
    begin
     FBackground[0].DrawMode := dmOpaque;
     FBackground[0].DrawTo(FBackground[1]);
     Screen.Cursor := crNone;
    end;
   if PtInRect(GRectTop[0], Pt) then
    if ssCtrl in Shift
     then Parameter[0] := 500
     else FEditValue := edLow
    else
   if PtInRect(GRectTop[1], Pt) then
    if ssCtrl in Shift
     then Parameter[1] := 2500
     else FEditValue := edHigh
    else
   if PtInRect(GRectTop[2], Pt) then
    if ssCtrl in Shift
     then Parameter[24] := 0
     else FEditValue := edVolume
    else
   if PtInRect(GRectTop[3], Pt)  then
    begin
     FBackground[0].DrawMode := dmOpaque;
     FBackground[0].DrawTo(FBackground[1]);
     Parameter[2] := 1 - Round(Parameter[2]);
     FAnimateFrame := -$20;
    end else
   for Stage := 0 to Length(GRectStage) - 1 do
    begin
     FStage := Stage;
     if PtInRect(GRectStage[FStage, 0], Pt)  then
      begin
       if ssCtrl in Shift
        then Parameter[5 + Stage * 7] := -10
        else FEditValue := edThreshold;
       break;
      end else
     if PtInRect(GRectStage[FStage, 1], Pt)  then
      begin
       if ssCtrl in Shift
        then Parameter[6 + Stage * 7] := 4
        else FEditValue := edRatio;
       break;
      end else
     if PtInRect(GRectStage[FStage, 2], Pt)  then
      begin
       if ssCtrl in Shift
        then Parameter[3 + Stage * 7] := 5
        else FEditValue := edAttack;
       break;
      end else
     if PtInRect(GRectStage[FStage, 3], Pt)  then
      begin
       if ssCtrl in Shift
        then Parameter[4 + Stage * 7] := 50
        else FEditValue := edRelease;
       break;
      end else
     if PtInRect(GRectStage[FStage, 4], Pt)  then
      begin
       if ssCtrl in Shift
        then Parameter[7 + Stage * 7] := 1
        else FEditValue := edKnee;
       break;
      end else
     if PtInRect(GRectStage[FStage, 5], Pt)  then
      begin
       if ssCtrl in Shift
        then Parameter[8 + Stage * 7] := 3
        else FEditValue := edMakeUpGain;
       break;
      end else
     if PtInRect(GRectStage[FStage, 7], Pt)  then
      begin
       FBackground[0].DrawMode := dmOpaque;
       FBackground[0].DrawTo(FBackground[1]);
       if bsSmooth in BandStates[Stage]
        then Parameter[9 + Stage * 7] := 0
        else Parameter[9 + Stage * 7] := 4;
       FAnimateFrame := -$20;
       Exit;
      end else
     if PtInRect(GRectStage[FStage, 8], Pt) and (ssCtrl in Shift) then
      begin
       FBackground[0].DrawMode := dmOpaque;
       FBackground[0].DrawTo(FBackground[1]);
       if bsBypass in BandStates[Stage]
        then Parameter[9 + Stage * 7] := 0
        else Parameter[9 + Stage * 7] := 1;
       FAnimateFrame := -$20;
       Exit;
      end;
    end;

   if FEditValue in [edLow, edHigh, edVolume, edAttack, edRelease, edThreshold,
     edRatio, edKnee, edMakeUpGain] then
    begin
     FBackground[0].DrawMode := dmOpaque;
     FBackground[0].DrawTo(FBackground[1]);
     Screen.Cursor := crNone;
    end;
 end;
(*
 FBackground[0].RenderText(x, y, IntToStr(X) +', ' + IntToStr(Y), 0, clWhite32);
 FBackground[1].RenderText(x, y, IntToStr(X) +', ' + IntToStr(Y), 0, clWhite32);
 Invalidate;
*)
end;

procedure TFmSmoothMultibandCompressor.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  s     : Single;
  Pt    : TPoint;
  Param : Integer;
  Stage : Integer;
begin
 with TSmoothMultibandCompressorDataModule(Owner) do
  case FEditValue of
   edLow: with GRectTop[0] do
             begin
              Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
              s := Parameter[0];
              if ssAlt in Shift
               then s := s * Power(2, 5E-3 * (Pt.Y - Y))
               else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (X - Pt.X));
              with ParameterProperties[0]
               do Parameter[0] := Limit(s, Min, Max);
             end;
   edHigh: with GRectTop[1] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               s := Parameter[1];
               if ssAlt in Shift
                then s := s * Power(2, 5E-3 * (Pt.Y - Y))
                else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (X - Pt.X));
               with ParameterProperties[1]
                do Parameter[1] := Limit(s, Min, Max);
              end;
   edVolume: with GRectTop[2] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               s := Parameter[24];
               if ssAlt in Shift
                then s := s + 0.002 * (Pt.Y - Y)
                else s := s + 0.016 * (Pt.Y - Y) - 0.002 * (Pt.X - X);
               with ParameterProperties[24]
                do Parameter[24] := Limit(s, Min, Max);
              end;
   edThreshold: with GRectStage[FStage, 0] do
                 begin
                  Param := 5 + FStage * 7;
                  Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
                  s := Parameter[Param];
                  if ssAlt in Shift
                   then s := s + 0.01 * (Pt.Y - Y)
                   else s := s + 0.05 * (Pt.Y - Y) - 0.01 * (Pt.X - X);
                  if not (ssShift in Shift) then
                   with ParameterProperties[Param]
                    do Parameter[Param] := Limit(s, Min, Max) else
                   for Stage := 0 to 2 do
                    begin
                     Param := 5 + Stage * 7;
                     with ParameterProperties[Param]
                      do Parameter[Param] := Limit(s, Min, Max);
                    end;
                 end;
   edRatio: with GRectStage[FStage, 1] do
             begin
              Param := 6 + FStage * 7;
              Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
              s := Parameter[Param];
              if ssAlt in Shift
               then s := s * Power(2, 2E-3 * (Pt.Y - Y))
               else s := s * Power(2, 0.008 * (Pt.Y - Y)) * Power(2, 2E-3 * (Pt.X - X));
              if not (ssShift in Shift) then
               with ParameterProperties[Param]
                do Parameter[Param] := Limit(s, Min, Max) else
               for Stage := 0 to 2 do
                begin
                 Param := 6 + Stage * 7;
                 with ParameterProperties[Param]
                  do Parameter[Param] := Limit(s, Min, Max);
                end;
             end;
   edAttack: with GRectStage[FStage, 2] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               Param := 3 + FStage * 7;
               s := Parameter[Param];
               if ssAlt in Shift
                then s := s * Power(2, 5E-3 * (Pt.Y - Y))
                else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (Pt.X - X));
               if not (ssShift in Shift) then
                with ParameterProperties[Param]
                 do Parameter[Param] := Limit(s, Min, Max) else
                for Stage := 0 to 2 do
                 begin
                  Param := 3 + Stage * 7;
                  with ParameterProperties[Param]
                   do Parameter[Param] := Limit(s, Min, Max);
                 end;
              end;
   edRelease: with GRectStage[FStage, 3] do
                begin
                 Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
                 Param := 4 + FStage * 7;
                 s := Parameter[Param];
                 if ssAlt in Shift
                  then s := s * Power(2, 5E-3 * (Pt.Y - Y))
                  else s := s * Power(2, 0.01 * (Pt.Y - Y)) * Power(2, 5E-3 * (Pt.X - X));
                 if not (ssShift in Shift) then
                  with ParameterProperties[Param]
                   do Parameter[Param] := Limit(s, Min, Max) else
                  for Stage := 0 to 2 do
                   begin
                    Param := 4 + Stage * 7;
                    with ParameterProperties[Param]
                     do Parameter[Param] := Limit(s, Min, Max);
                   end;
                end;
   edKnee: with GRectStage[FStage, 4] do
              begin
               Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
               Param := 7 + FStage * 7;
               s := Parameter[Param];
               if ssAlt in Shift
                then s := s + 0.002 * (Pt.Y - Y)
                else s := s + 0.016 * (Pt.Y - Y) - 0.002 * (Pt.X - X);
               if not (ssShift in Shift) then
                with ParameterProperties[Param]
                 do Parameter[Param] := Limit(s, Min, Max) else
                for Stage := 0 to 2 do
                 begin
                  Param := 7 + Stage * 7;
                  with ParameterProperties[Param]
                   do Parameter[Param] := Limit(s, Min, Max);
                 end;
              end;
   edMakeUpGain: with GRectStage[FStage, 5] do
                begin
                 Pt := Point((3 * Left + Right) div 4, (3 * Top + Bottom) div 4);
                 Param := 8 + FStage * 7;
                 s := Parameter[Param];
                 if ssAlt in Shift
                  then s := s + 0.01 * (Pt.Y - Y)
                  else s := s + 0.05 * (Pt.Y - Y) - 0.01 * (Pt.X - X);
                 if not (ssShift in Shift) then
                  with ParameterProperties[Param]
                   do Parameter[Param] := Limit(s, Min, Max) else
                  for Stage := 0 to 2 do
                   begin
                    Param := 8 + Stage * 7;
                    with ParameterProperties[Param]
                     do Parameter[Param] := Limit(s, Min, Max);
                   end;
                end;
  end;

 if FEditValue in [edLow, edHigh, edVolume, edThreshold, edRatio, edAttack,
   edRelease, edKnee, edMakeUpGain, edVolume] then
  begin
   Pt := ClientToScreen(Pt);
   OnMouseMove := nil;
   SetCursorPos(Pt.X, Pt.Y);
   FAnimateFrame := -$10;
   Invalidate;
   Sleep(3);
   Application.ProcessMessages;
   OnMouseMove := FormMouseMove;
  end;

end;

procedure TFmSmoothMultibandCompressor.FormMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 Screen.Cursor := crDefault;
 FEditValue := edNone;
end;

procedure TFmSmoothMultibandCompressor.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Stage : Integer;  
begin
 MousePos := ScreenToClient(MousePos);
 if PtInRect(GRectTop[0], MousePos) then FEditValue := edLow else
 if PtInRect(GRectTop[1], MousePos) then FEditValue := edHigh else
 if PtInRect(GRectTop[2], MousePos) then FEditValue := edVolume else
 for Stage := 0 to Length(GRectStage) - 1 do
  begin
   if PtInRect(GRectStage[Stage, 0], MousePos)  then
    begin
     FEditValue := edThreshold;
     FStage := Stage;
     break;
    end else
   if PtInRect(GRectStage[Stage, 1], MousePos)  then
    begin
     FEditValue := edRatio;
     FStage := Stage;
     break;
    end else
   if PtInRect(GRectStage[Stage, 4], MousePos)  then
    begin
     FEditValue := edKnee;
     FStage := Stage;
     break;
    end else
   if PtInRect(GRectStage[Stage, 5], MousePos)  then
    begin
     FEditValue := edMakeUpGain;
     FStage := Stage;
     break;
    end else
   if PtInRect(GRectStage[Stage, 2], MousePos)  then
    begin
     FEditValue := edAttack;
     FStage := Stage;
     break;
    end else
   if PtInRect(GRectStage[Stage, 3], MousePos)  then
    begin
     FEditValue := edRelease;
     FStage := Stage;
     break;
    end;
  end;

 if WheelDelta > 0
  then FormMouseMove(Sender, Shift + [ssAlt, ssLeft], MousePos.X, MousePos.Y - 10)
  else FormMouseMove(Sender, Shift + [ssAlt, ssLeft], MousePos.X, MousePos.Y + 10);
 FEditValue := edNone;
end;

procedure TFmSmoothMultibandCompressor.FormPaint(Sender: TObject);
begin
 if FAnimateFrame >= 0
  then FBackground[0].DrawTo(Canvas.Handle, 0, 0)
  else
   begin
    FBackground[0].MasterAlpha := $80 + 2 * FAnimateFrame;
    FBackground[0].DrawMode := dmBlend;
    FBackground[0].DrawTo(FBackground[1]);
    FBackground[1].DrawTo(Canvas.Handle, 0, 0);
    inc(FAnimateFrame);
   end;
end;

procedure TFmSmoothMultibandCompressor.FormResize(Sender: TObject);
begin
 with FBackground[0] do
  begin
   BeginUpdate;
   SetSize(ClientWidth, ClientHeight);
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.FormShow(Sender: TObject);
begin
 DrawAll;
end;

procedure TFmSmoothMultibandCompressor.TimerTimer(Sender: TObject);
begin
 Invalidate;
end;

procedure TFmSmoothMultibandCompressor.RestitchPNG2Bitmap32(const PNG : TPNGGraphic; const BMP : TVerticallyStitchedBitmap32);
var i : Integer;
begin
 BMP.Width  := PNG.Width div BMP.GlyphCount;
 BMP.Height := PNG.Height * BMP.GlyphCount;
 PNG.Canvas.Lock;
 try
  for i := 0 to BMP.GlyphCount - 1
   do BitBlt(BMP.Handle, 0, i * PNG.Height, PNG.Width, PNG.Height, PNG.Canvas.Handle, i * BMP.Width, 0, SRCCOPY);
 finally
  PNG.Canvas.UnLock;
 end;
end;

procedure TFmSmoothMultibandCompressor.DrawAll;
var
  Stage: Integer;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   Draw(0, 0, GBG);
   DrawLowFrequency;
   DrawHighFrequency;
   DrawOutputGain;
   DrawSoftClip;
   for Stage := 0 to 2 do
    begin
     DrawThreshold(Stage);
     DrawRatio(Stage);
     DrawAttack(Stage);
     DrawRelease(Stage);
     DrawKnee(Stage);
     DrawMakeUpGain(Stage);
     DrawState(Stage);
     DrawGraph(Stage);
     DrawBypass(Stage);
    end;
  finally
   EndUpdate;
  end;
 FBackground[1].Assign(FBackground[0]);
end;

procedure TFmSmoothMultibandCompressor.DrawLowFrequency;
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[0]
    do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[0]));
   if i < 0 then i := 0
    else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
   r := GRectTop[0];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 17;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[0] + ' ' + ParameterLabel[0];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FFC1CAD1);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawHighFrequency;
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[1]
    do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[1]));
   if i < 0 then i := 0
    else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
   r := GRectTop[1];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 17;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[1] + ' ' + ParameterLabel[1];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FFC1CAD1);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawOutputGain;
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[24]
    do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[24]));
   i := Limit(i, 0, GKnob.GlyphCount - 1);
   r := GRectTop[2];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
   r.Bottom := r.Top;
   r.Top    := r.Bottom - 17;
   r.Left   := (3 * r.Left + r.Right) div 4;
   r.Right  := r.Left + 32;
   r.Left   := r.Left - 32;
   Draw(r, r, GBG);
   t := ParameterDisplay[24] + ' ' + ParameterLabel[24];
   RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FFC1CAD1);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawSoftClip;
var
  i : Integer;
  r : TRect;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  try
   BeginUpdate;
   with ParameterProperties[2]
    do i := Round((GSoftClip.GlyphCount - 1) * Parameter2VSTParameter(Parameter[2]));
   if i < 0 then i := 0
    else if i >= GSoftClip.GlyphCount then i := GSoftClip.GlyphCount - 1;
   r := GRectTop[3];
   Draw(r, r, GBG);
   Draw(r, Rect(0, i * GSoftClip.RealHeight, GSoftClip.RealWidth, (i + 1) * GSoftClip.RealHeight), GSoftClip);
  finally
   EndUpdate;
  end;
end;

procedure TFmSmoothMultibandCompressor.DrawThreshold(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    with ParameterProperties[5 + Stage * 7]
     do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[5 + Stage * 7]));
    if i < 0 then i := 0
     else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
    r := GRectStage[Stage, 0];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
    r.Bottom := r.Top;
    r.Top    := r.Bottom - 19;
    r.Left   := (3 * r.Left + r.Right) div 4;
    r.Right  := r.Left + 32;
    r.Left   := r.Left - 32;
    Draw(r, r, GBG);
    t := ParameterDisplay[5 + Stage * 7] + ' ' + ParameterLabel[5 + Stage * 7];
    RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4A4645);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawRatio(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    with ParameterProperties[6 + Stage * 7]
     do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[6 + Stage * 7]));
    if i < 0 then i := 0
     else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
    r := GRectStage[Stage, 1];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
    r.Bottom := r.Top;
    r.Top    := r.Bottom - 19;
    r.Left   := (3 * r.Left + r.Right) div 4;
    r.Right  := r.Left + 32;
    r.Left   := r.Left - 32;
    Draw(r, r, GBG);
    t := ParameterDisplay[6 + Stage * 7] + ' : 1';
    RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4A4645);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawAttack(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    with ParameterProperties[3 + Stage * 7]
     do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[3 + Stage * 7]));
    if i < 0 then i := 0
     else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
    r := GRectStage[Stage, 2];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
    r.Bottom := r.Top;
    r.Top    := r.Bottom - 19;
    r.Left   := (3 * r.Left + r.Right) div 4;
    r.Right  := r.Left + 32;
    r.Left   := r.Left - 32;
    Draw(r, r, GBG);
    t := ParameterDisplay[3 + Stage * 7] + ' ' + ParameterLabel[3 + Stage * 7];
    RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4A4645);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawRelease(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    with ParameterProperties[4 + Stage * 7]
     do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[4 + Stage * 7]));
    if i < 0 then i := 0
     else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
    r := GRectStage[Stage, 3];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
    r.Bottom := r.Top;
    r.Top    := r.Bottom - 19;
    r.Left   := (3 * r.Left + r.Right) div 4;
    r.Right  := r.Left + 32;
    r.Left   := r.Left - 32;
    Draw(r, r, GBG);
    t := ParameterDisplay[4 + Stage * 7] + ' ' + ParameterLabel[4 + Stage * 7];
    RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4A4645);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawKnee(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    with ParameterProperties[7 + Stage * 7]
     do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[7 + Stage * 7]));
    if i < 0 then i := 0
     else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
    r := GRectStage[Stage, 4];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
    r.Bottom := r.Top;
    r.Top    := r.Bottom - 19;
    r.Left   := (3 * r.Left + r.Right) div 4;
    r.Right  := r.Left + 32;
    r.Left   := r.Left - 32;
    Draw(r, r, GBG);
    t := ParameterDisplay[7 + Stage * 7] + ' ' + ParameterLabel[7 + Stage * 7];
    RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4A4645);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawMakeUpGain(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  t : string;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    with ParameterProperties[8 + Stage * 7]
     do i := Round((GKnob.GlyphCount - 1) * Parameter2VSTParameter(Parameter[8 + Stage * 7]));
    if i < 0 then i := 0
     else if i >= GKnob.GlyphCount then i := GKnob.GlyphCount - 1;
    r := GRectStage[Stage, 5];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GKnob.RealHeight, GKnob.RealWidth, (i + 1) * GKnob.RealHeight), GKnob);
    r.Bottom := r.Top;
    r.Top    := r.Bottom - 19;
    r.Left   := (3 * r.Left + r.Right) div 4;
    r.Right  := r.Left + 32;
    r.Left   := r.Left - 32;
    Draw(r, r, GBG);
    t := ParameterDisplay[8 + Stage * 7] + ' ' + ParameterLabel[8 + Stage * 7];
    RenderText((r.Left + r.Right - TextWidth(t)) div 2, R.Top, t, 0, $FF4A4645);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawGraph(const Stage: Integer);
var
  i : Integer;
  r : TRect;
  n : Single;
  s : Single;
const
  COneSixty : Single = 1 / 60;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   with LightweightCompressor[Stage] do
    begin
     BeginUpdate;
     try
      r := GRectStage[Stage, 6];
      Draw(r, r, GBG);

      s := 60 / (r.Bottom - r.Top);
      PenColor := $DF6289A8;

      i := r.Left;

      repeat
       n := CharacteristicCurve_dB(-54 + (i - r.Left) * s);
       n := r.Bottom - (r.Bottom - r.Top) * ((n + 55) * COneSixty);
       inc(i)
      until n < r.Bottom;
      MoveToF(i, Limit(n - 1, r.Top, r.Bottom - 1));
      while i < r.Right do
       begin
        n := CharacteristicCurve_dB(-54 + (i - r.Left) * s);
        if n > 6 then PenColor := SetAlpha(PenColor, Round($DF - (n - 6) * 2));
        LineToFS(i, Limit(r.Bottom - (r.Bottom - r.Top) * ((n + 55) * COneSixty), r.Top, r.Bottom - 1));
        inc(i);
       end;
     finally
      EndUpdate;
     end;
    end;
end;

procedure TFmSmoothMultibandCompressor.DrawState(const Stage: Integer);
var
  i : Integer;
  r : TRect;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if not (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;
    i := Integer(bsSmooth in BandStates[Stage]);
    if i < 0 then i := 0
     else if i >= GAutoGain.GlyphCount then i := GAutoGain.GlyphCount - 1;
    r := GRectStage[Stage, 7];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GAutoGain.RealHeight, GAutoGain.RealWidth, (i + 1) * GAutoGain.RealHeight), GAutoGain);
   finally
    EndUpdate;
   end;
end;

procedure TFmSmoothMultibandCompressor.DrawBypass(const Stage: Integer);
var
  i : Integer;
  r : TRect;
begin
 with FBackground[0], TSmoothMultibandCompressorDataModule(Owner) do
  if (bsBypass in BandStates[Stage]) then
   try
    BeginUpdate;

    r := GRectStage[Stage, 8];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GAutoGain.RealHeight, GAutoGain.RealWidth, (i + 1) * GAutoGain.RealHeight), GAutoGain);

    FillRectTS(GRectStage[Stage, 8], clTrBlack32);
   finally
    EndUpdate;
   end
  else
   try
    BeginUpdate;

    r := GRectStage[Stage, 8];
    Draw(r, r, GBG);
    Draw(r, Rect(0, i * GAutoGain.RealHeight, GAutoGain.RealWidth, (i + 1) * GAutoGain.RealHeight), GAutoGain);

    DrawThreshold(Stage);
    DrawRatio(Stage);
    DrawAttack(Stage);
    DrawRelease(Stage);
    DrawKnee(Stage);
    DrawMakeUpGain(Stage);
    DrawState(Stage);
    DrawGraph(Stage);
   finally
    EndUpdate;
   end
end;

////////////
// Update //
////////////

procedure TFmSmoothMultibandCompressor.UpdateLowFrequency;
begin
 DrawLowFrequency;
end;

procedure TFmSmoothMultibandCompressor.UpdateHighFrequency;
begin
 DrawHighFrequency;
end;

procedure TFmSmoothMultibandCompressor.UpdateLowAttack;
begin
 DrawAttack(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowRelease;
begin
 DrawRelease(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowState;
begin
 DrawState(0);
 DrawBypass(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowThreshold;
begin
 DrawThreshold(0);
 DrawGraph(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowRatio;
begin
 DrawRatio(0);
 DrawGraph(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowKnee;
begin
 DrawKnee(0);
 DrawGraph(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowMakeUp;
begin
 DrawMakeUpGain(0);
 DrawGraph(0);
end;

procedure TFmSmoothMultibandCompressor.UpdateLowAutoMakeUpGain;
begin
 // todo
end;

procedure TFmSmoothMultibandCompressor.UpdateMidAttack;
begin
 DrawAttack(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidRelease;
begin
 DrawRelease(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidState;
begin
 DrawState(1);
 DrawBypass(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidThreshold;
begin
 DrawThreshold(1);
 DrawGraph(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateOutputGain;
begin
 DrawOutputGain;
end;

procedure TFmSmoothMultibandCompressor.UpdateMidRatio;
begin
 DrawRatio(1);
 DrawGraph(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidKnee;
begin
 DrawKnee(1);
 DrawGraph(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidMakeUp;
begin
 DrawMakeUpGain(1);
 DrawGraph(1);
end;

procedure TFmSmoothMultibandCompressor.UpdateMidAutoMakeUpGain;
begin
 // todo
end;

procedure TFmSmoothMultibandCompressor.UpdateHighAttack;
begin
 DrawAttack(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighRelease;
begin
 DrawRelease(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighState;
begin
 DrawState(2);
 DrawBypass(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighThreshold;
begin
 DrawThreshold(2);
 DrawGraph(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighRatio;
begin
 DrawRatio(2);
 DrawGraph(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighKnee;
begin
 DrawKnee(2);
 DrawGraph(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighMakeUp;
begin
 DrawMakeUpGain(2);
 DrawGraph(2);
end;

procedure TFmSmoothMultibandCompressor.UpdateHighAutoMakeUpGain;
begin
 // todo
end;

procedure TFmSmoothMultibandCompressor.UpdateLimit;
begin
 DrawSoftClip;
end;

{ TVerticallyStitchedBitmap32 }

constructor TVerticallyStitchedBitmap32.Create;
begin
 inherited;
 FGlyphCount := 1;
end;

function TVerticallyStitchedBitmap32.GetRealHeight: Integer;
begin
 result := FHeight div FGlyphCount;
end;

procedure TVerticallyStitchedBitmap32.SetGlyphCount(const Value: Integer);
begin
 FGlyphCount := Value;
end;

end.
