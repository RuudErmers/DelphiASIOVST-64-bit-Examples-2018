unit DecimatorGUI;

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
  Forms, ExtCtrls, Controls, Gauges, StdCtrls, Graphics, DAV_Types, 
  DAV_VSTModule;

type
  TMouseContext = (mcNone, mcSHRate, mcBits, mcCut, mcRes, mcMix, mcVol);
  TVSTGUI = class(TForm)
    FltType: TImage;
    FltType2: TImage;
    ImageAbout: TImage;
    ImageBackground: TImage;
    ImageDecimator: TImage;
    ImageTobyBear: TImage;
    LbAbout: TLabel;
    LbBits: TLabel;
    LbContact: TLabel;
    LbCut: TLabel;
    LbMix: TLabel;
    LbRate: TLabel;
    LbRes: TLabel;
    LbVol: TLabel;
    LbVSTTechnology: TLabel;
    PanelAbout: TPanel;
    ShBits: TShape;
    ShBitsBg: TShape;
    ShCut: TShape;
    ShCutBg: TShape;
    ShMix: TShape;
    ShMixBg: TShape;
    ShRes: TShape;
    ShResBg: TShape;
    ShSHRate: TShape;
    ShSHRateBg: TShape;
    ShVol: TShape;
    shVolBg: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FltType2Click(Sender: TObject);
    procedure ImageTobyBearClick(Sender: TObject);
    procedure ImageDecimatorClick(Sender: TObject);
    procedure LbAboutClick(Sender: TObject);
    procedure ShSHRateBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShCutBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShResBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShMixBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shVolBgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShSHRateBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShSHRateMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShSHRateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShCutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShResMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShMixMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShVolMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShCutBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShResBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShMixBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure shVolBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ShBitsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShCutMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShResMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShMixMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShVolMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FMouseContext  : TMouseContext;
  public
    procedure UpdateSampleRate;
    procedure UpdateBits;
    procedure UpdateFrequency;
    procedure UpdateResonance;
    procedure UpdateFilterType;
    procedure UpdateMix;
    procedure UpdateOutput;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DecimatorModule;

procedure TVSTGUI.FormCreate(Sender: TObject);
begin
 PanelAbout.Top := ImageDecimator.Height - 1;
end;

procedure TVSTGUI.FormShow(Sender: TObject);
begin
 UpdateSampleRate;
 UpdateBits;
 UpdateFrequency;
 UpdateResonance;
 UpdateFilterType;
 UpdateMix;
 UpdateOutput;
end;

procedure TVSTGUI.FltType2Click(Sender: TObject);
begin
 with TVSTDecimator(Owner) do
  if FltType.Visible then
   begin
    FltType.Visible := False;
    Parameter[4] := 0;
   end
  else
   begin
    FltType.Visible := True;
    Parameter[4] := 1;
   end;
end;

procedure TVSTGUI.ImageTobyBearClick(Sender: TObject);
begin
 PanelAbout.Visible := not PanelAbout.Visible;
end;

procedure TVSTGUI.LbAboutClick(Sender: TObject);
begin
 PanelAbout.Visible := False;
end;

procedure TVSTGUI.ImageDecimatorClick(Sender: TObject);
begin
 PanelAbout.Visible := not PanelAbout.Visible;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////// Mouse Downs /////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TVSTGUI.ShSHRateBgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcSHRate;
 ShSHRateBgMouseMove(Sender, Shift, X, Y);
end;

procedure TVSTGUI.ShSHRateMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcSHRate;
 ShSHRateBgMouseMove(Sender, Shift, X, ShSHRate.Top - ShSHRateBg.Top + Y);
end;

procedure TVSTGUI.ShBitsBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcBits;
 ShBitsBgMouseMove(Sender, Shift, X, Y);
end;

procedure TVSTGUI.ShCutBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcCut;
 ShCutBgMouseMove(Sender, Shift, X, Y);
end;

procedure TVSTGUI.ShResBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcRes;
 ShResBgMouseMove(Sender, Shift, X, Y);
end;

procedure TVSTGUI.ShMixBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcMix;
 ShMixBgMouseMove(Sender, Shift, X, Y);
end;

procedure TVSTGUI.shVolBgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcVol;
 ShVolBgMouseMove(Sender, Shift, X, Y);
end;

procedure TVSTGUI.ShBitsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcBits;
 ShBitsBgMouseMove(Sender, Shift, X, ShBits.Top - ShBitsBg.Top + Y);
end;

procedure TVSTGUI.ShCutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcCut;
 ShCutBgMouseMove(Sender, Shift, X, ShCut.Top - ShCutBg.Top + Y);
end;

procedure TVSTGUI.ShResMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcRes;
 ShResBgMouseMove(Sender, Shift, X, ShRes.Top - ShResBg.Top + Y);
end;

procedure TVSTGUI.ShMixMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcMix;
 ShMixBgMouseMove(Sender, Shift, X, ShMix.Top - ShMixBg.Top + Y);
end;

procedure TVSTGUI.ShVolMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FMouseContext := mcVol;
 ShVolBgMouseMove(Sender, Shift, X, ShVol.Top - ShVolBg.Top + Y);
end;

procedure TVSTGUI.shMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 case FMouseContext of
  mcSHRate : LbRate.Caption := 'Rate';
  mcBits   : LbBits.Caption := 'Bits';
  mcCut    : LbCut.Caption  := 'Cut';
  mcRes    : LbRes.Caption  := 'Res';
  mcMix    : LbMix.Caption  := 'Mix';
  mcVol    : LbVol.Caption  := 'Vol';
 end;

 FMouseContext := mcNone;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////// Mouse Moves /////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TVSTGUI.ShSHRateBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (FMouseContext = mcSHRate) then
  with TVSTDecimator(Owner) do
   begin
    if ShSHRateBg.Height - Y > ShSHRateBg.Height
     then Parameter[0] := 44100
     else
      if ShSHRateBg.Height - Y < 0
       then Parameter[0] := 44.1
       else Parameter[0] := FreqLinearToLog(1 - Y / ShSHRateBg.Height) * 2.205;
    LbRate.Caption := FloatToStrF(Parameter[0], ffGeneral, 5, 2);
   end;
end;

procedure TVSTGUI.ShBitsBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (FMouseContext = mcBits) then
  with TVSTDecimator(Owner) do
   begin
    if ShBitsBg.Height - Y > ShBitsBg.Height
     then Parameter[1] := 24
     else
      if ShBitsBg.Height - Y < 0
       then Parameter[1] := 1
       else Parameter[1] := Round(23 * (1 - (Y / ShBitsBg.Height)) + 1);
    LbBits.Caption := IntToStr(Round(Parameter[1]));
   end;
end;

procedure TVSTGUI.ShCutBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (FMouseContext = mcCut) and (Y >= -10) then
  with TVSTDecimator(Owner) do
   begin
    if ShCutBg.Height - Y > ShCutBg.Height
     then Parameter[2] := 20000
     else
      if ShCutBg.Height - Y < 0
       then Parameter[2] := 20
       else Parameter[2] := FreqLinearToLog(1 - Y / ShCutBg.Height);
    LbCut.Caption := FloatToStrF(Parameter[2], ffGeneral, 5, 2);
   end;
end;

procedure TVSTGUI.ShResBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (FMouseContext=mcRes) then
  with TVSTDecimator(Owner) do
   begin
    if ShResBg.Height - Y > ShResBg.Height
     then Parameter[3] := 8
     else
      if ShResBg.Height - Y <= 0
       then Parameter[3] := 0.1
       else Parameter[3] := 8 * (1 - Y / ShResBg.Height);
    LbRes.Caption := FloatToStrF(Parameter[3], ffGeneral, 2, 2);
   end;
end;

procedure TVSTGUI.ShMixBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (FMouseContext = mcMix) then
  with TVSTDecimator(Owner) do
   begin
    if ShMixBg.Height - Y > ShMixBg.Height
     then Parameter[5] := 100
     else
      if ShMixBg.Height - Y <= 0
       then Parameter[5] := 0
       else Parameter[5] := 100 * (1 - Y / ShMixBg.Height);
    LbMix.Caption := IntToStr(Round(Parameter[5]));
   end;
end;

procedure TVSTGUI.shVolBgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if (ssLeft in Shift) and (FMouseContext = mcVol) then
  with TVSTDecimator(Owner) do
   begin
    if ShVolBg.Height - Y > ShVolBg.Height
     then Parameter[6] := -24
     else
      if ShVolBg.Height - Y <= 0
       then Parameter[6] := 6
       else Parameter[6] := 6 - 30 * Y  / ShVolBg.Height;
    LbVol.Caption := IntToStr(Round(Parameter[6]));
   end;
end;

procedure TVSTGUI.ShSHRateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShSHRateBgMouseMove(Sender, Shift, X, ShSHRate.Top - ShSHRateBg.Top + Y);
end;

procedure TVSTGUI.ShBitsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShBitsBgMouseMove(Sender, Shift, X, ShBits.Top - ShBitsBg.Top + Y);
end;

procedure TVSTGUI.ShCutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShCutBgMouseMove(Sender, Shift, X, ShCut.Top-ShCutBg.Top+Y);
end;

procedure TVSTGUI.ShResMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShResBgMouseMove(Sender, Shift, X, ShRes.Top-ShResBg.Top+Y);
end;

procedure TVSTGUI.ShMixMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShMixBgMouseMove(Sender, Shift, X, ShMix.Top-ShMixBg.Top+Y);
end;

procedure TVSTGUI.ShVolMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 ShVolBgMouseMove(Sender, Shift, X, ShVol.Top-ShVolBg.Top+Y);
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////// GUI Update Functions ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure TVSTGUI.UpdateBits;
const
  COne24th = 1 / 24;
var
  BitTopPos : Integer;
begin
 with TVSTDecimator(Owner) do
  begin
   BitTopPos := Round((1 - BitDepth * COne24th) * ShBitsBg.Height + ShBitsBg.Top);
   if ShBits.Top <> BitTopPos then
    try
     ShBits.Top := BitTopPos;
     ShBits.Height := ShBitsBg.Height - ShBits.Top + ShBitsBg.Top;
    except
    end;
  end;
end;

procedure TVSTGUI.UpdateFilterType;
begin
 with TVSTDecimator(Owner) do
  begin
   FltType.Visible := FilterType = dftHighpass;
  end;
end;

procedure TVSTGUI.UpdateFrequency;
var
  FreqTopPos : Integer;
begin
 with TVSTDecimator(Owner) do
  begin
   FreqTopPos := Round((1 - 2 * CutoffNormalizedFrequency) * ShCutBg.Height + ShCutBg.Top);
   if ShCut.Top <> FreqTopPos then
    begin
     ShCut.Top := FreqTopPos;
     ShCut.Height := ShCutBg.Height - ShCut.Top + ShCutBg.Top;
    end;
  end;
end;

procedure TVSTGUI.UpdateMix;
var
  MixTopPos : Integer;
begin
 with TVSTDecimator(Owner) do
  begin
   MixTopPos := Round((1 - WetMix) * ShMixBg.Height + ShMixBg.Top);
   if ShMix.Top <> MixTopPos then
    begin
     ShMix.Top := MixTopPos;
     ShMix.Height := ShMixBg.Height - ShMix.Top + ShMixBg.Top;
    end;
  end;
end;

procedure TVSTGUI.UpdateOutput;
var
  VolTopPos: Integer;
begin
 with TVSTDecimator(Owner) do
  begin
   VolTopPos := Round(ShVolBg.Top - (OutputVolume - 6) / 30 * ShVolBg.Height);
   if ShVol.Top <> VolTopPos then
    begin
     ShVol.Top := VolTopPos;
     ShVol.Height := ShVolBg.Height - (ShVol.Top - ShVolBg.Top);
    end;
  end;
end;

procedure TVSTGUI.UpdateResonance;
var
  ResTopPos : Integer;
begin
 with TVSTDecimator(Owner) do
  begin
   ResTopPos := Round((1 - 1.25 * Resonance) * ShResBg.Height + ShResBg.Top);
   if ShRes.Top <> ResTopPos then
    begin
     ShRes.Top := ResTopPos;
     ShRes.Height := ShResBg.Height - ShRes.Top + ShResBg.Top;
    end;
  end;
end;

procedure TVSTGUI.UpdateSampleRate;
var
  SRTopPos : Integer;
begin
 with TVSTDecimator(Owner) do
  begin
   SRTopPos := Round((1 - SampleHoldRate) * ShSHRateBg.Height + ShSHRateBg.Top);
   if ShSHRate.Top <> SRTopPos then
    begin
     ShSHRate.Top := SRTopPos;
     ShSHRate.Height := ShSHRateBg.Height - ShSHRate.Top + ShSHRateBg.Top;
    end;
  end;
end;

end.
