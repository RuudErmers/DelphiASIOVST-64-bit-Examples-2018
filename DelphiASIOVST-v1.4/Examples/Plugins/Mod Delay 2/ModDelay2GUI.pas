unit ModDelay2GUI;

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
  Forms, Graphics, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel,
  DAV_GuiSlider, DAV_GuiGraphicControl, DAV_GuiPixelMap;

type
  TFmModDelay2 = class(TForm)
    LbCurrentValue: TGuiLabel;
    LbDelay: TGuiLabel;
    LbDepth: TGuiLabel;
    LbFeedback: TGuiLabel;
    LbGain: TGuiLabel;
    LbLeft: TGuiLabel;
    LbLpf: TGuiLabel;
    LbMix: TGuiLabel;
    LbRate: TGuiLabel;
    LbRight: TGuiLabel;
    SBGainLeft: TGuiSlider;
    SBMixLeft: TGuiSlider;
    SBLPFLeft: TGuiSlider;
    SBDelayLeft: TGuiSlider;
    SbDepthLeft: TGuiSlider;
    SBRateLeft: TGuiSlider;
    SBFeedbackLeft: TGuiSlider;
    SBGainRight: TGuiSlider;
    SBMixRight: TGuiSlider;
    SBLpfRight: TGuiSlider;
    SBdelayRight: TGuiSlider;
    SBDepthRight: TGuiSlider;
    SBRateRight: TGuiSlider;
    SBFeedbackRight: TGuiSlider;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

implementation

uses
  DAV_GuiCommon, ModDelay2DM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmModDelay2.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmModDelay2.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmModDelay2.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmModDelay2.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
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

end.
