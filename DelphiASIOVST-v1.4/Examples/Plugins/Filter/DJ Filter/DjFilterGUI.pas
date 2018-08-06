unit DjFilterGUI;

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
  Forms, Controls, Graphics, StdCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiPixelMap, DAV_GuiLabel, DAV_GuiGroup, DAV_GuiStitchedControls,
  DAV_GuiStitchedDial, DAV_GuiStitchedPngList, DAV_GuiImageControl,
  DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmDjFilter = class(TForm)
    GSPL: TGuiStitchedPNGList;
    GpDjFilter: TGuiGroupSimple;
    LbHigh: TGuiLabel;
    LbMid: TGuiLabel;
    LbLow: TGuiLabel;
    DialHigh: TGuiStitchedDial;
    DialMid: TGuiStitchedDial;
    DialLow: TGuiStitchedDial;
    GuiLabel1: TGuiLabel;
    GuiLabel2: TGuiLabel;
    GuiLabel3: TGuiLabel;
    GuiLabel4: TGuiLabel;
    GuiLabel5: TGuiLabel;
    GuiLabel6: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DialHighChange(Sender: TObject);
    procedure DialLowChange(Sender: TObject);
    procedure DialMidChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    procedure UpdateLow;
    procedure UpdateMid;
    procedure UpdateHigh;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_GuiCommon, DjFilterDSP;

procedure TFmDjFilter.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmDjFilter.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmDjFilter.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmDjFilter.FormResize(Sender: TObject);
var
  X, Y   : Integer;
  Filter : array [0..1] of Single;
  Value  : ShortInt;
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
       Filter[1] := 0.9 * Filter[0] + 0.1 * Random;
       Value := Round($13 + $20 * Filter[1]);
       Filter[0] := Filter[1];
       ScnLn[X].B := Value;
       ScnLn[X].G := Value;
       ScnLn[X].R := Value;
      end;
    end;
  end;
end;

procedure TFmDjFilter.FormShow(Sender: TObject);
begin
 UpdateLow;
 UpdateMid;
 UpdateHigh;
end;

procedure TFmDjFilter.UpdateLow;
begin
 with TDjFilterDataModule(Owner) do
  begin
   if DialLow.Value <> Parameter[0]
    then DialLow.Value := Parameter[0];
  end;
end;

procedure TFmDjFilter.UpdateMid;
begin
 with TDjFilterDataModule(Owner) do
  begin
   if DialMid.Value <> Parameter[1]
    then DialMid.Value := Parameter[1];
  end;
end;

procedure TFmDjFilter.UpdateHigh;
begin
 with TDjFilterDataModule(Owner) do
  begin
   if DialHigh.Value <> Parameter[2]
    then DialHigh.Value := Parameter[2];
  end;
end;

procedure TFmDjFilter.DialLowChange(Sender: TObject);
begin
 with TDjFilterDataModule(Owner) do
  begin
   if Parameter[0] <> DialLow.Value
    then Parameter[0] := DialLow.Value;
  end;
end;

procedure TFmDjFilter.DialMidChange(Sender: TObject);
begin
 with TDjFilterDataModule(Owner) do
  begin
   if Parameter[1] <> DialMid.Value
    then Parameter[1] := DialMid.Value;
  end;
end;

procedure TFmDjFilter.DialHighChange(Sender: TObject);
begin
 with TDjFilterDataModule(Owner) do
  begin
   if Parameter[2] <> DialHigh.Value
    then Parameter[2] := DialHigh.Value;
  end;
end;

end.
