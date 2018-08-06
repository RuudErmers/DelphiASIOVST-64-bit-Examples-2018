unit BarberpoleTunerGUI;

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
  Forms, Controls, StdCtrls, Graphics, ExtCtrls, DAV_Types, DAV_VSTModule,
  DAV_GuiLabel, DAV_GuiPixelMap;

type
  TFmBarberpoleTuner = class(TForm)
    LbDisplay: TGuiLabel;
    Barberpole: TPaintBox;
    Timer: TTimer;
    LbGuitarTuning: TGuiLabel;
    LbLowE: TGuiLabel;
    LbA: TGuiLabel;
    LbD: TGuiLabel;
    LbG: TGuiLabel;
    LbH: TGuiLabel;
    LbE: TGuiLabel;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure BarberpolePaint(Sender: TObject);
    procedure LbNoteClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

implementation

uses
  DAV_GuiCommon, BarberpoleTunerDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmBarberpoleTuner.FormCreate(Sender: TObject);
begin
 // create background pixelmap
 FBackground := TGuiPixelMapMemory.Create;

 Barberpole.ControlStyle := Barberpole.ControlStyle + [csOpaque];
end;

procedure TFmBarberpoleTuner.FormPaint(Sender: TObject);
begin
 FBackground.PaintTo(Canvas);
end;

procedure TFmBarberpoleTuner.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 // resize background pixelmap and render
 if Assigned(FBackground) then
  with FBackground do
   begin
    SetSize(ClientWidth, ClientHeight);
    s[0] := 0;
    s[1] := 0;
    hr   := 1 / Height;
    for y := 0 to Height - 1 do
     begin
      ScnLn := Scanline[y];
      h := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
      for x := 0 to Width - 1 do
       begin
        s[1] := 0.97 * s[0] + 0.03 * random;
        s[0] := s[1];

        ScnLn[x].B := Round($70 - $34 * (s[1] - h));
        ScnLn[x].G := Round($84 - $48 * (s[1] - h));
        ScnLn[x].R := Round($8D - $50 * (s[1] - h));
       end;
     end;
   end;
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

procedure TFmBarberpoleTuner.LbNoteClick(Sender: TObject);
begin
 if Sender <> LbLowE then LbLowE.Font.Color := $4F4F4F else begin LbLowE.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 1; end;
 if Sender <> LbA then LbA.Font.Color := $4F4F4F else begin LbA.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 2; end;
 if Sender <> LbD then LbD.Font.Color := $4F4F4F else begin LbD.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 3; end;
 if Sender <> LbG then LbG.Font.Color := $4F4F4F else begin LbG.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 4; end;
 if Sender <> LbH then LbH.Font.Color := $4F4F4F else begin LbH.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 5; end;
 if Sender <> LbE then LbE.Font.Color := $4F4F4F else begin LbE.Font.Color := clBlack; TBarberpoleTunerDataModule(Owner).Parameter[0] := 6; end;
end;

procedure TFmBarberpoleTuner.TimerTimer(Sender: TObject);
begin
 Barberpole.Invalidate;
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

 with TBarberpoleTunerDataModule(Owner) do
  for Column := 0 to Barberpole.Width - 3 do
   begin
    Barberpole.Canvas.Pen.Color := Round($70 - $34 * BufferPointer^[Column]) shl 16 +
                                   Round($84 - $48 * BufferPointer^[Column]) shl  8 +
                                   Round($8D - $50 * BufferPointer^[Column]);
//    Barberpole.Canvas.Pen.Color := TColor(Round($64 + BufferPointer^[Column] * $60));
    Barberpole.Canvas.MoveTo(Column + 1, 1);
    Barberpole.Canvas.LineTo(Column + 1, Barberpole.Height - 1);
   end;
end;

end.
