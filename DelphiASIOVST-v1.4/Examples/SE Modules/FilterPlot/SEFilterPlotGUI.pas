unit SEFilterPlotGUI;

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
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  Windows, Classes, Graphics, ExtCtrls, DAV_SEModule, DAV_SEGUI, DAV_DspFilter,
  SEFilterPlotModule;

type
  TRGB24 = packed record
    B, G, R: Byte;
  end;
  TRGB32 = packed record
    R, G, B, A: Byte;
  end;
  TRGB24Array = packed array [0..MaxInt div SizeOf(TRGB24) - 1] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  TSEFilterPlotGui = class(TSEGUIBase)
  private
    FColorLine  : TColor;
    FColorCurve : TColor;
    FBitmap     : TBitmap;
    FLock       : Boolean;
    FFilter     : TCustomFilter;
    FTimer      : TTimer;
    function InvalidateControl: Integer;
    procedure BitmapChanged(Sender: TObject);
    procedure GUIChanged(Sender: TObject);
    procedure GUIDraw(Sender: TObject);
  protected
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer); override;
    procedure GuiPinValueChange(CurrentPin: TSeGuiPin); override;
    procedure GuiWindowOpen(WI: PSEWndInfo); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, DAV_Types, DAV_Approximations;

constructor TSEFilterPlotGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
 FColorLine                 := clGray;
 FColorCurve                := clSilver;
 FLock                      := False;
 FBitmap                    := TBitmap.Create;
 FTimer                     := TTimer.Create(nil);
 FTimer.OnTimer             := GUIDraw;
 FTimer.Interval            := 50;
 FBitmap.PixelFormat        := pf24bit;
 FBitmap.Canvas.Font.Height := 24;

 CallHost(seGuiHostSetWindowSize, 64, 64);
 CallHost(seGuiHostSetWindowType, 0); // 0 = Draw on SE's window (default), 1 = HWND based

 // CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE or HWF_NO_CUSTOM_GFX_ON_STRUCTURE));
 CallHost(seGuiHostSetWindowFlags, Integer(hwfResizable));
end;

destructor TSEFilterPlotGui.Destroy;
begin
 FreeAndNil(FTimer);
 FreeAndNil(FBitmap);
 inherited;
end;

procedure TSEFilterPlotGui.GuiPaint(hDC: HDC; wi :PSEWndInfo);
begin
 if not FLock then
  begin
   if FBitmap.Width  <> wi.Width  then FBitmap.Width  := wi.Width;
   if FBitmap.Height <> wi.Height then FBitmap.Height := wi.Height;
  end;
 with TCanvas.Create do
  try
   Handle := hDC;
   if not FLock then
    begin
     BitBlt(FBitmap.Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height, Handle, 0, 0, SRCCOPY);
     BitmapChanged(FBitmap);
    end;
   Draw(0, 0, FBitmap);
  finally
   Free;
  end;
end;

procedure TSEFilterPlotGui.GuiPinValueChange(CurrentPin: TSeGuiPin);
begin
 case CurrentPin.PinIndex of
  1 : begin
       FFilter := TCustomFilter(Pin[1].ValueAsInteger);
       if not (FFilter is TCustomFilter)
        then FFilter := nil
        else FFilter.OnChange := GUIChanged;
       GUIDraw(nil);
      end;
  3 : begin
       FColorLine  := TColor(Pin[3].ValueAsInteger);
       GUIDraw(nil);
      end;
  4 : begin
       FColorCurve := TColor(Pin[4].ValueAsInteger);
       GUIDraw(nil);
      end;
 end;
 inherited;
end;

procedure TSEFilterPlotGui.GUIChanged(Sender: TObject);
begin
 FTimer.Enabled := True;
end;

procedure TSEFilterPlotGui.GUIDraw(Sender: TObject);
begin
 BitmapChanged(Self);
 CallHost(seGuiHostRequestRepaint);
 FTimer.Enabled := False;
end;

procedure TSEFilterPlotGui.GuiWindowOpen(WI: PSEWndInfo);
begin
 inherited;
 FColorLine  := TColor(Pin[3].ValueAsInteger);
 FColorCurve := TColor(Pin[4].ValueAsInteger);
// FFilter     := TCustomFilter(Pin[0].ValueAsInteger);
end;

procedure TSEFilterPlotGui.BitmapChanged(Sender: TObject);
var
  R         : TRect;
  HalfHght  : Integer;
  c, Wdth   : Integer;
  WdthRez   : Single;
  Magn, Frq : Single;
  Band      : Integer;
const
  CdBFactor : Single = 0.2006829232;
begin
 with FBitmap.Canvas do
  begin
   R := ClipRect;
   Pen.Color := FColorLine;
   RoundRect(R.Left, R.Top, R.Right, R.Bottom, 2, 2);
   InflateRect(R, -1, -1);
   Brush.Color := FColorLine;
(*
   FillRect(R);
*)
   Brush.Style := bsClear;

   Wdth := R.Right - R.Left + 1;
   WdthRez := 1 / Wdth;
   HalfHght := (R.Bottom - R.Top) div 2;

   // draw 100 Hz
   Band := Round(FreqLogToLinear(100) * Wdth);
   MoveTo(Band, R.Top);
   LineTo(Band, R.Bottom);

   // draw 1 kHz
   Band := Round(FreqLogToLinear(1E3) * Wdth);
   MoveTo(Band, R.Top);
   LineTo(Band, R.Bottom);

   // draw 10 kHz
   Band := Round(FreqLogToLinear(1E4) * Wdth);
   MoveTo(Band, R.Top);
   LineTo(Band, R.Bottom);

   // draw middle line
   MoveTo(1, HalfHght);
   LineTo(Wdth, HalfHght);

   Pen.Color := FColorCurve;
   if Assigned(FFilter) then
    begin
     Magn := FFilter.MagnitudeSquared(20);
     MoveTo(1, Round(HalfHght * (1 - FastLog2MinError5(Magn) * CdBFactor)));
     for c := 2 to Wdth do
      begin
       Frq := FreqLinearToLog(c * WdthRez);
       Magn := FFilter.MagnitudeSquared(Frq);
       LineTo(c, Round(HalfHght * (1 - FastLog2MinError5(Magn) * CdBFactor )));
      end;
    end;
  end;
end;

procedure TSEFilterPlotGui.GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer);
begin
 InvalidateControl;
end;

function TSEFilterPlotGui.InvalidateControl: Integer;
begin
 Result := CallHost(seGuiHostRequestRepaint);
end;

end.
