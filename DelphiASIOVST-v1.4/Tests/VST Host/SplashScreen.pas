unit SplashScreen;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, DAV_GuiPixelMap,
  DAV_GuiGraphicControl, DAV_GuiLabel;

type
  TFmSplashScreen = class(TForm)
    IVST: TImage;
    IDUnit: TImage;
    LbTitle: TGuiLabel;
    LbVstAbout: TLabel;
    LbDUnitAbout: TLabel;
    LbScanning: TLabel;
    LbScannedPlugin: TLabel;
    Border: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPixelMap : TGuiCustomPixelMap;
  end;

implementation

uses
  DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSplashScreen.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
end;

procedure TFmSplashScreen.FormShow(Sender: TObject);
begin
 LbTitle.Width := LbTitle.Width + 1;
 FormResize(Sender);
end;

procedure TFmSplashScreen.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
end;

procedure TFmSplashScreen.FormPaint(Sender: TObject);
begin
 if Assigned(FPixelMap)
  then FPixelMap.PaintTo(Canvas);
end;

procedure TFmSplashScreen.FormResize(Sender: TObject);
var
  Filter : array [0..1] of Single;
  hr, h  : Single;
  x, y   : Integer;
  ScnLn  : PPixel32Array;
begin
 with FPixelMap do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h := 0.3 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round(255 - $1F * (Filter[1] - h));
       ScnLn[x].G := ScnLn[x].B;
       ScnLn[x].R := ScnLn[x].B;
      end;
    end;
  end;
end;

end.
