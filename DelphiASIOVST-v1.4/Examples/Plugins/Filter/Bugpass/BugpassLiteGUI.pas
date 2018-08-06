unit BugpassLiteGUI;

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
  Forms, Controls, Graphics, ExtCtrls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, 
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiGraphicControl;

type
  TMouseEdit = (meNone, meLow, meHigh);
  TFmBugpassLite = class(TForm)
    LbTitle: TGuiLabel;
    LbFreqLowValue: TGuiLabel;
    FrequencyBar: TPaintBox;
    LbSubTitle: TGuiLabel;
    LbFreqHighValue: TGuiLabel;
    LbVST: TGuiLabel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FrequencyBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrequencyBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrequencyBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
    FMouseEdit  : TMouseEdit;
  public
    procedure UpdateFrequencyBar;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  BugpassLiteDM, DAV_Common, DAV_VSTModuleWithPrograms;

procedure TFmBugpassLite.FormCreate(Sender: TObject);
begin
 // Create Background Image
 FBackground := TGuiPixelMapMemory.Create;
 FrequencyBar.ControlStyle := FrequencyBar.ControlStyle + [csOpaque];
end;

procedure TFmBugpassLite.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmBugpassLite.FormShow(Sender: TObject);
begin
 UpdateFrequencyBar;
end;

procedure TFmBugpassLite.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmBugpassLite.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  amt    : Shortint;
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
       amt := Round($E * (2 * Filter[1] - 1));
       ScnLn[x].B := $E0 + amt;
       ScnLn[x].G := $D0 + amt;
       ScnLn[x].R := $B6 + amt;
       Filter[0] := Filter[1];
      end;
    end;
  end;
end;

procedure TFmBugpassLite.FrequencyBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 with TBugpassLiteDataModule(Owner), FrequencyBar do
  if abs(X - FreqLogToLinear(Parameter[0]) * Width) < 5
   then FMouseEdit := meLow else
  if abs(X - FreqLogToLinear(Parameter[1]) * Width) < 5
   then FMouseEdit := meHigh
   else FMouseEdit := meNone;
end;

procedure TFmBugpassLite.FrequencyBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if ssLeft in Shift then
  with TBugpassLiteDataModule(Owner), FrequencyBar do
   case FMouseEdit of
    meLow  : Parameter[0] := Limit(FreqLinearToLog(Limit(X / Width, 0, Width)), 20, 20000);
    meHigh : Parameter[1] := Limit(FreqLinearToLog(Limit(X / Width, 0, Width)), 20, 20000);
   end;
end;

procedure TFmBugpassLite.FrequencyBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FMouseEdit := meNone;
end;

procedure TFmBugpassLite.PaintBoxPaint(Sender: TObject);
begin
 with TBugpassLiteDataModule(Owner), FrequencyBar, Canvas do
  begin
   Brush.Color := $00C1AE9B;
   FillRect(Rect(0, 0, Round(FreqLogToLinear(Parameter[0]) * Width), Height));
   FillRect(Rect(Round(FreqLogToLinear(Parameter[1]) * Width), 0, Width, Height));

   Brush.Color := $00372D22;
   FillRect(Rect(Round(FreqLogToLinear(Parameter[0]) * Width), 0,
                 Round(FreqLogToLinear(Parameter[1]) * Width), Height));
  end;
end;

procedure TFmBugpassLite.DialFrequencyChange(Sender: TObject);
begin
 with TBugpassLiteDataModule(Owner) do
  begin

  end;
end;

procedure TFmBugpassLite.UpdateFrequencyBar;
var
  Freq : array [0..1] of Single;
begin
 with TBugpassLiteDataModule(Owner) do
  begin
   Freq[0] := Parameter[0];
   Freq[1] := Parameter[1];

   FrequencyBar.Invalidate;

   if Freq[0] < 1000
    then LbFreqLowValue.Caption := FloatToStrF(Freq[0], ffGeneral, 3, 3) + ' Hz'
    else LbFreqLowValue.Caption := FloatToStrF(1E-3 * Freq[0], ffGeneral, 3, 3) + ' kHz';
   if Freq[1] < 1000
    then LbFreqHighValue.Caption := FloatToStrF(Freq[1], ffGeneral, 3, 3) + ' Hz'
    else LbFreqHighValue.Caption := FloatToStrF(1E-3 * Freq[1], ffGeneral, 3, 3) + ' kHz';
  end;
end;

end.
