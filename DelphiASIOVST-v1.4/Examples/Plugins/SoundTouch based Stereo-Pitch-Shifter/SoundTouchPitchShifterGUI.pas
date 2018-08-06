unit SoundTouchPitchShifterGUI;

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
  Forms, Controls, Graphics, DAV_Types, DAV_VSTModule, DAV_GuiCommon, 
  DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiPng, DAV_GuiStitchedControls, 
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList, DAV_GuiPixelMap,
  DAV_GuiImageControl, DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmSoundTouchPitchShifter = class(TForm)
    LbSemitones: TGuiLabel;
    LbSemitoneValue: TGuiLabel;
    GSPL: TGuiStitchedPNGList;
    DialSemitones: TGuiStitchedDial;
    procedure DialSemitonesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateSemitones;
  end;

implementation

uses
  SoundTouchPitchShifterDM, DAV_VSTModuleWithPrograms;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSoundTouchPitchShifter.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmSoundTouchPitchShifter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSoundTouchPitchShifter.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSoundTouchPitchShifter.FormResize(Sender: TObject);
var
  X, Y   : Integer;
  Filter : array [0..1] of Single;
  Value  : Byte;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   for Y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[Y];
     for X := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * (2 * Random - 1);
       Filter[0] := Filter[1];
       Value := Round($0E * Filter[1]);
       ScnLn[X].B := $0F + Value;
       ScnLn[X].G := $12 + Value;
       ScnLn[X].R := $13 + Value;
      end;
    end;
  end;
end;

procedure TFmSoundTouchPitchShifter.DialSemitonesChange(Sender: TObject);
begin
 with TSoundTouchPitchShifterModule(Owner) do
  begin
   if Parameter[0] <> DialSemitones.Value
    then Parameter[0] := DialSemitones.Value
  end;
end;

procedure TFmSoundTouchPitchShifter.UpdateSemitones;
var
  SemiTones : Integer;
begin
 with TSoundTouchPitchShifterModule(Owner) do
  begin
   if DialSemitones.Value <> Parameter[0]
    then DialSemitones.Value := Parameter[0];
   SemiTones := Round(Parameter[0]);
   LbSemitoneValue.Caption := IntToStr(SemiTones) + ' : ' +
     IntToStr(Round(100 * (Parameter[0] - SemiTones)));
  end;
end;

end.
