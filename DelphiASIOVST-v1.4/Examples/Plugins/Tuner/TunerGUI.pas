unit TunerGUI;

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
  Forms, Graphics, Controls, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel,
  DAV_GuiGraphicControl;

type
  TFmTuner = class(TForm)
    PBDisplay: TPaintBox;
    LbLowE: TGuiLabel;
    LbA: TGuiLabel;
    LbD: TGuiLabel;
    LbG: TGuiLabel;
    LbH: TGuiLabel;
    LbE: TGuiLabel;
    LbGuitarTuning: TGuiLabel;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbNoteClick(Sender: TObject);
    procedure PBDisplayPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBackgroundBitmap  : TBitmap;
    FNeedlePosition    : Single;
    FOldNeedlePosition : Integer;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_GuiCommon, TunerDM;

procedure TFmTuner.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  b      : ShortInt;
  Line   : PRGB24Array;
//  RS     : TResourceStream;
//  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgroundBitmap := TBitmap.Create;
 with FBackgroundBitmap do
  begin
   PixelFormat := pf24bit;
   Width := ClientWidth;
   Height := ClientHeight;
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       b := Round($9F + $1A * s[1]);
       s[0] := s[1];
       Line[x].B := b;
       Line[x].G := b;
       Line[x].R := b;
      end;
    end;
  end;

(*
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'TwoBandKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   with DIL.DialImages.Add do
    begin
     DialBitmap.Canvas.Brush.Color := $696969;
     DialBitmap.Assign(PngBmp);
     GlyphCount := 65;
    end;
   DialFreq.DialImageIndex := 0;
   DialOrder.DialImageIndex := 0;
   DialHighDist.DialImageIndex := 0;
   DialLowDist.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
*)
// PBDisplay.ControlStyle := PBDisplay.ControlStyle + [csOpaque];
end;

procedure TFmTuner.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgroundBitmap);
end;

procedure TFmTuner.FormShow(Sender: TObject);
begin
 LbLowE.Transparent := True;
 LbA.Transparent := True;
 LbD.Transparent := True;
 LbG.Transparent := True;
 LbH.Transparent := True;
 LbE.Transparent := True;
 LbGuitarTuning.Transparent := True;
end;

procedure TFmTuner.LbNoteClick(Sender: TObject);
begin
 if Sender <> LbLowE then LbLowE.Font.Color := $4F4F4F else begin LbLowE.Font.Color := clBlack; TTunerDataModule(Owner).Parameter[0] := 1; end;
 if Sender <> LbA then LbA.Font.Color := $4F4F4F else begin LbA.Font.Color := clBlack; TTunerDataModule(Owner).Parameter[0] := 2; end;
 if Sender <> LbD then LbD.Font.Color := $4F4F4F else begin LbD.Font.Color := clBlack; TTunerDataModule(Owner).Parameter[0] := 3; end;
 if Sender <> LbG then LbG.Font.Color := $4F4F4F else begin LbG.Font.Color := clBlack; TTunerDataModule(Owner).Parameter[0] := 4; end;
 if Sender <> LbH then LbH.Font.Color := $4F4F4F else begin LbH.Font.Color := clBlack; TTunerDataModule(Owner).Parameter[0] := 5; end;
 if Sender <> LbE then LbE.Font.Color := $4F4F4F else begin LbE.Font.Color := clBlack; TTunerDataModule(Owner).Parameter[0] := 6; end;
end;

procedure TFmTuner.PBDisplayPaint(Sender: TObject);
var
  NeedlePosition : Integer;
begin
 FNeedlePosition := 0.9 * FNeedlePosition + 0.1 * ((2 * random) - 1);

 NeedlePosition := Round( (PBDisplay.Width div 2) * FNeedlePosition);
 FOldNeedlePosition := NeedlePosition;

 with PBDisplay.Canvas do
  begin
   Lock;

   // main line
   Pen.Color := clBlack;
(*
   Pen.Mode := pmNot;
   MoveTo((PBDisplay.Width div 2) + FOldNeedlePosition, 0);
   LineTo((PBDisplay.Width div 2) + FOldNeedlePosition, PBDisplay.Height);
*)

   MoveTo((PBDisplay.Width div 2) + NeedlePosition, 0);
   LineTo((PBDisplay.Width div 2) + NeedlePosition, PBDisplay.Height);

   // side lines
   Pen.Color := $4F4F4F;
   MoveTo((PBDisplay.Width div 2) + NeedlePosition - 1, 0);
   LineTo((PBDisplay.Width div 2) + NeedlePosition - 1, PBDisplay.Height);
   MoveTo((PBDisplay.Width div 2) + NeedlePosition + 1, 0);
   LineTo((PBDisplay.Width div 2) + NeedlePosition + 1, PBDisplay.Height);

   Unlock;
  end;
end;

procedure TFmTuner.TimerTimer(Sender: TObject);
begin
 PBDisplay.Invalidate;
end;

end.
