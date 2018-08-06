unit SubBoostGUI;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel, DAV_GuiSelectBox,
  DAV_GuiPanel, DAV_GuiGraphicControl, DAV_GuiPixelMap, DAV_GuiCustomControl;

type
  TFmSubBoost = class(TForm)
    DialDryMix: TGuiDial;
    DialFilterOrder: TGuiDial;
    DialInputFilter: TGuiDial;
    DialLevel: TGuiDial;
    DialRelease: TGuiDial;
    DialThreshold: TGuiDial;
    DialTune: TGuiDial;
    LbFreq: TGuiLabel;
    GuiPanel1: TGuiPanel;
    GuiPanel2: TGuiPanel;
    LbDryMix: TGuiLabel;
    LbFilterOrder: TGuiLabel;
    LbLevel: TGuiLabel;
    LbRelease: TGuiLabel;
    LbThreshold: TGuiLabel;
    LbTitle: TGuiLabel;
    LbTitleShadow: TGuiLabel;
    LbTune: TGuiLabel;
    LbType: TGuiLabel;
    SBType: TGuiSelectBox;
    procedure DialDryMixChange(Sender: TObject);
    procedure DialLevelChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialThresholdChange(Sender: TObject);
    procedure DialTuneChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure DialFilterOrderChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateType;
    procedure UpdateLevel;
    procedure UpdateTune;
    procedure UpdateDryMix;
    procedure UpdateThreshold;
    procedure UpdateRelease;
  end;

var
  FmSubBoost: TFmSubBoost;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PngImage, DAV_GUICommon, SubBoostDM;

procedure TFmSubBoost.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngImage;
begin
 // Create Background Image
 FBackground := TGuiPixelMapMemory.Create;

 PngBmp := TPngImage.Create;
 try
  RS := TResourceStream.Create(hInstance, 'SubBoostKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialLevel.DialBitmap.Assign(PngBmp);
   DialTune.DialBitmap.Assign(PngBmp);
   DialDryMix.DialBitmap.Assign(PngBmp);
   DialThreshold.DialBitmap.Assign(PngBmp);
   DialRelease.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSubBoost.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSubBoost.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  b      : ShortInt;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.9 * Filter[0] + 0.1 * Random;
       b := Round($3F * Filter[1]);
       Filter[0] := Filter[1];
       ScnLn[x].B := b;
       ScnLn[x].G := b;
       ScnLn[x].R := b;
       ScnLn[x].A := 0;
      end;
    end;
  end;
end;

procedure TFmSubBoost.FormShow(Sender: TObject);
begin
 UpdateType;
 UpdateLevel;
 UpdateTune;
 UpdateDryMix;
 UpdateThreshold;
 UpdateRelease;
 LbTitleShadow.Transparent := True;
 LbTitle.Transparent := True;
 LbType.Transparent := True;
end;

procedure TFmSubBoost.SBTypeChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[0] <> SBType.ItemIndex
    then Parameter[0] := SBType.ItemIndex;
  end;
end;

procedure TFmSubBoost.DialFilterOrderChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[6] <> DialFilterOrder.Position
    then Parameter[6] := DialFilterOrder.Position;
   LbFilterOrder.Caption := IntToStr(Round(DialFilterOrder.Position));
  end;
end;

procedure TFmSubBoost.DialLevelChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[1] <> DialLevel.Position
    then Parameter[1] := DialLevel.Position;
  end;
end;

procedure TFmSubBoost.DialTuneChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[2] <> DialTune.Position
    then Parameter[2] := DialTune.Position;
   LbTune.Caption := FloatToStrF(Parameter[2], ffGeneral, 3, 3);
  end;
end;

procedure TFmSubBoost.DialDryMixChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[3] <> DialDryMix.Position
    then Parameter[3] := DialDryMix.Position;
  end;
end;

procedure TFmSubBoost.DialThresholdChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[4] <> DialThreshold.Position
    then Parameter[4] := DialThreshold.Position;
  end;
end;

procedure TFmSubBoost.DialReleaseChange(Sender: TObject);
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[5] <> DialRelease.Position
    then Parameter[5] := DialRelease.Position;
  end;
end;

procedure TFmSubBoost.UpdateType;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if Parameter[0] <> Round(SBType.ItemIndex)
    then SBType.ItemIndex := Round(Parameter[0]);
  end;
end;

procedure TFmSubBoost.UpdateLevel;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialLevel.Position <> Parameter[1] 
    then DialLevel.Position := Parameter[1];
  end;
end;

procedure TFmSubBoost.UpdateTune;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialTune.Position <> Parameter[2]
    then DialTune.Position := Parameter[2];
  end;
end;

procedure TFmSubBoost.UpdateDryMix;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialDryMix.Position <> Parameter[3] 
    then DialDryMix.Position := Parameter[3];
  end;
end;

procedure TFmSubBoost.UpdateRelease;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialRelease.Position <> Parameter[4]
    then DialRelease.Position := Parameter[4];
  end;
end;

procedure TFmSubBoost.UpdateThreshold;
begin
 with TSubBoostDataModule(Owner) do
  begin
   if DialThreshold.Position <> Parameter[5] 
    then DialThreshold.Position := Parameter[5];
  end;
end;

end.
