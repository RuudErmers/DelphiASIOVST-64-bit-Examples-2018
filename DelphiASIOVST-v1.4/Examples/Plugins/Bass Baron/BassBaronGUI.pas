unit BassBaronGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiPng,
  DAV_GuiGraphicControl, pngimage, ExtCtrls, DAV_GuiImageControl,
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiCustomControl,
  DAV_GuiStitchedDial, DAV_GuiGroup, DAV_GuiSelectBox, DAV_GuiPanel;

type
  TFmBassBaron = class(TForm)
    AnimationTimer: TTimer;
    DialBassMix: TGuiStitchedDial;
    DialFrequency: TGuiStitchedDial;
    DialHF: TGuiStitchedDial;
    DialInput: TGuiStitchedDial;
    DialLowCut: TGuiStitchedDial;
    DialOutput: TGuiStitchedDial;
    DialResponse: TGuiStitchedDial;
    GpAlgorithm: TGuiGroupTop;
    GpFrequencySplitting: TGuiGroupTop;
    GSPL: TGuiStitchedPNGList;
    ImLogo: TImage;
    ImOrnament: TImage;
    LbAlgorithmType: TGuiLabel;
    LbBassMix: TGuiLabel;
    LbBassMixValue: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbHfLevel: TGuiLabel;
    LbHfLevelValue: TGuiLabel;
    LbInputLevel: TGuiLabel;
    LbInputLevelValue: TGuiLabel;
    LbLowcut: TGuiLabel;
    LbLowcutValue: TGuiLabel;
    LbOutputLevel: TGuiLabel;
    LbOutputLevelValue: TGuiLabel;
    LbResponse: TGuiLabel;
    LbResponseValue: TGuiLabel;
    LbSubTitle: TGuiLabel;
    LbTitle: TGuiLabel;
    PnFrequencies: TGuiPanel;
    SbAlgorithmType: TGuiSelectBox;
    SbSplitFilterSetup: TGuiSelectBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);
    procedure DialBassMixChange(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialHFChange(Sender: TObject);
    procedure DialInputChange(Sender: TObject);
    procedure DialLowCutChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialResponseChange(Sender: TObject);
    procedure LbTitleMouseEnter(Sender: TObject);
    procedure LbTitleMouseLeave(Sender: TObject);
    procedure SbAlgorithmTypeChange(Sender: TObject);
    procedure SbSplitFilterSetupChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
    FBlurUp     : Boolean;
    FBlurStop   : Boolean;
  public
    procedure UpdateAlgorithm;
    procedure UpdateBassMix;
    procedure UpdateFilterType;
    procedure UpdateFrequency;
    procedure UpdateHFLevel;
    procedure UpdateInputLevel;
    procedure UpdateLowCut;
    procedure UpdateOutputLevel;
    procedure UpdateResponse;
  end;

implementation

uses
  BassBaronVST;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmBassBaron.AnimationTimerTimer(Sender: TObject);
begin
  if FBlurUp then
   begin
    LbTitle.Shadow.Blur := LbTitle.Shadow.Blur + 0.1;
    if LbTitle.Shadow.Blur > 10
     then FBlurUp := False;
   end
  else
   begin
    LbTitle.Shadow.Blur := LbTitle.Shadow.Blur - 0.1;
    if LbTitle.Shadow.Blur <= 3 then
     if FBlurStop then
      begin
       LbTitle.Shadow.Blur := 3;
       AnimationTimer.Enabled := False;
       FBlurStop := False;
      end
     else FBlurUp := True;
   end;
end;

procedure TFmBassBaron.DialInputChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[0] := DialInput.Value;
  end;
end;

procedure TFmBassBaron.DialFrequencyChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[1] := DialFrequency.Value;
  end;
end;

procedure TFmBassBaron.SbSplitFilterSetupChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[2] := SbSplitFilterSetup.ItemIndex;
  end;
end;

procedure TFmBassBaron.DialLowCutChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[3] := DialLowCut.Value;
  end;
end;

procedure TFmBassBaron.SbAlgorithmTypeChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[4] := SbAlgorithmType.ItemIndex;
  end;
end;

procedure TFmBassBaron.DialResponseChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[5] := DialResponse.Value;
  end;
end;

procedure TFmBassBaron.DialHFChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[6] := DialHF.Value;
  end;
end;

procedure TFmBassBaron.DialBassMixChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[7] := DialBassMix.Value;
  end;
end;

procedure TFmBassBaron.DialOutputChange(Sender: TObject);
begin
 with TBassBaronModule(Owner) do
  begin
   Parameter[8] := DialOutput.Value;
  end;
end;

procedure TFmBassBaron.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmBassBaron.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmBassBaron.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmBassBaron.FormResize(Sender: TObject);
var
(*
  RS     : TResourceStream;
  Png32  : TPortableNetworkGraphicPixel32;
  PM     : TGuiCustomPixelMap;
*)
  x, y   : Integer;
  s      : array [0..2] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.3 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * Random;
       s[0] := s[1];
       s[2] := s[1] - h;

       ScnLn[x].B := Round($A - $8 * s[2]);
       ScnLn[x].G := Round($C - $A * s[2]);
       ScnLn[x].R := Round($E - $C * s[2]);
      end;
    end;
  end;

(*
 Png32 := TPortableNetworkGraphicPixel32.Create;
 try
  PM := TGuiPixelMapMemory.Create;
  try
   RS := TResourceStream.Create(hInstance, 'Left', 'PNG');
   try
    Png32.LoadFromStream(RS);
    PM.Assign(Png32);
    FBackground.Draw(PM, 8, 8);
   finally
    RS.Free;
   end;

   RS := TResourceStream.Create(hInstance, 'Right', 'PNG');
   try
    Png32.LoadFromStream(RS);
    PM.Assign(Png32);
    FBackground.Draw(PM, Width - Png32.Width - 8, 8);
   finally
    RS.Free;
   end;
  finally
   FreeAndNil(PM);
  end;
 finally
  FreeAndNil(Png32);
 end;
*)
end;

procedure TFmBassBaron.FormShow(Sender: TObject);
begin
 UpdateAlgorithm;
 UpdateBassMix;
 UpdateFilterType;
 UpdateFrequency;
 UpdateHFLevel;
 UpdateInputLevel;
 UpdateLowCut;
 UpdateOutputLevel;
 UpdateResponse;
end;

procedure TFmBassBaron.LbTitleMouseEnter(Sender: TObject);
begin
  AnimationTimer.Enabled := True;
end;

procedure TFmBassBaron.LbTitleMouseLeave(Sender: TObject);
begin
  FBlurStop := True;
end;

procedure TFmBassBaron.UpdateInputLevel;
var
  Level : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Level := Parameter[0];
   if DialInput.Value <> Level
    then DialInput.Value := Level;
   LbInputLevelValue.Caption := string(ParameterDisplay[0] + ' ' + ParameterLabel[0]);
  end;
end;

procedure TFmBassBaron.UpdateFrequency;
var
  Frequency : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Frequency := Parameter[1];
   if DialFrequency.Value <> Frequency
    then DialFrequency.Value := Frequency;
   LbFrequencyValue.Caption := string(ParameterDisplay[1] + ' ' + ParameterLabel[1]);
  end;
end;

procedure TFmBassBaron.UpdateFilterType;
var
  FilterType : Integer;
begin
 with TBassBaronModule(Owner) do
  begin
   FilterType := Round(Parameter[2]);
   if SbSplitFilterSetup.ItemIndex <> FilterType
    then SbSplitFilterSetup.ItemIndex := FilterType;
  end;
end;

procedure TFmBassBaron.UpdateLowCut;
var
  Frequency : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Frequency := Parameter[3];
   if DialLowCut.Value <> Frequency
    then DialLowCut.Value := Frequency;
   LbLowcutValue.Caption := string(ParameterDisplay[3] + ' ' + ParameterLabel[3]);
  end;
end;

procedure TFmBassBaron.UpdateAlgorithm;
var
  AlgorithmType : Integer;
begin
 with TBassBaronModule(Owner) do
  begin
   AlgorithmType := Round(Parameter[4]);
   if SbAlgorithmType.ItemIndex <> AlgorithmType
    then SbAlgorithmType.ItemIndex := AlgorithmType;
  end;
end;

procedure TFmBassBaron.UpdateResponse;
var
  Response : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Response := Parameter[5];
   if DialResponse.Value <> Response
    then DialResponse.Value := Response;
   LbResponseValue.Caption := string(ParameterDisplay[5] + ' ' + ParameterLabel[5]);
  end;
end;

procedure TFmBassBaron.UpdateHFLevel;
var
  Level : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Level := Parameter[6];
   if DialHF.Value <> Level
    then DialHF.Value := Level;
   LbHFLevelValue.Caption := string(ParameterDisplay[6] + ' ' + ParameterLabel[6]);
  end;
end;

procedure TFmBassBaron.UpdateBassMix;
var
  Mix : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Mix := Parameter[7];
   if DialBassMix.Value <> Mix
    then DialBassMix.Value := Mix;
   LbBassMixValue.Caption := string(ParameterDisplay[7] + ' ' + ParameterLabel[7]);
  end;
end;

procedure TFmBassBaron.UpdateOutputLevel;
var
  Level : Single;
begin
 with TBassBaronModule(Owner) do
  begin
   Level := Parameter[8];
   if DialOutput.Value <> Level
    then DialOutput.Value := Level;
   LbOutputLevelValue.Caption := string(ParameterDisplay[8] + ' ' + ParameterLabel[8]);
  end;
end;

end.
