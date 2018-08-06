unit FrequencyDomainPitchShifterGUI;

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
  DAV_GuiLabel, DAV_GuiStitchedControls, DAV_GuiStitchedPngList, 
  DAV_GuiStitchedDial, DAV_GuiPixelMap, DAV_GuiImageControl,
  DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmFrequencyDomainPitchShifter = class(TForm)
    DialSemitones: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbSemitones: TGuiLabel;
    LbSemitoneValue: TGuiLabel;
    procedure DialSemitonesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateSemitones;
  end;

implementation

uses
  FrequencyDomainPitchShifterDM, DAV_VSTModuleWithPrograms;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmFrequencyDomainPitchShifter.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmFrequencyDomainPitchShifter.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmFrequencyDomainPitchShifter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * (2 * random - 1);
       s[0] := s[1];
       ScnLn[x].B := Round($0F + $0E * s[1]);;
       ScnLn[x].G := Round($12 + $0E * s[1]);;
       ScnLn[x].R := Round($13 + $0E * s[1]);;
      end;
    end;
  end;
end;

procedure TFmFrequencyDomainPitchShifter.DialSemitonesChange(Sender: TObject);
begin
 with TFrequencyDomainPitchShifterModule(Owner) do
  begin
   if Parameter[0] <> DialSemitones.Value
    then Parameter[0] := DialSemitones.Value
  end;
end;

procedure TFmFrequencyDomainPitchShifter.UpdateSemitones;
var
  SemiTones : Integer;
begin
 with TFrequencyDomainPitchShifterModule(Owner) do
  begin
   if DialSemitones.Value <> Parameter[0]
    then DialSemitones.Value := Parameter[0];
   SemiTones := Round(Parameter[0]);
   LbSemitoneValue.Caption := IntToStr(SemiTones) + ' : ' +
     IntToStr(Round(100 * (Parameter[0] - SemiTones)));
  end;
end;

end.
