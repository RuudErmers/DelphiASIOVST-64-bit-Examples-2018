unit SESonogramGUI;

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
  Windows, Classes, Controls, Graphics, ExtCtrls, DAV_SEModule, DAV_SEGUI,
  DAV_Sonogram, SESonogramModule;

const
  pinEnumOut = 2;

type
  TSESonogramGui = class(TSEGUIBase)
  private
    FSonogram : TBitmapSonogram32;
    FTimer    : TTimer;
    procedure GUIDraw(Sender: TObject);
  protected
    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer); override;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DAV_Types, SysUtils, DAV_GuiBaseControl;

constructor TSESonogramGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;

 FTimer := TTimer.Create(nil);
 FTimer.OnTimer  := GUIDraw;
 FTimer.Interval := 40;
 FTimer.Enabled  := True;

 CallHost(seGuiHostSetWindowSize, 64, 64);
 CallHost(seGuiHostSetWindowType, 0); // 0 = Draw on SE's window (default), 1 = HWND based

 CallHost(seGuiHostSetWindowFlags, Integer(hwfResizable) +
   Integer(hwfNoCustomGfxOnStructure));
end;

destructor TSESonogramGui.Destroy;
begin
 FTimer.Enabled := False;
 FreeAndNil(FTimer);
 inherited;
end;

procedure TSESonogramGui.GuiPaint(hDC: HDC; wi: PSEWndInfo);
begin
 if Assigned(FSonogram) and Assigned(CriticalSection) then
  with FSonogram do
   begin
    if not Assigned(Bitmap) then Exit;

    CriticalSection.Enter;
    try
     if Assigned(wi) then
      begin
       if Bitmap.Width  <> wi.Width  then Bitmap.Width  := wi.Width;
       if Bitmap.Height <> wi.Height then Bitmap.Height := wi.Height;
      end;

     BitBlt(hDC, 0, 0 + (Bitmap.Height - CurrentSlice), Bitmap.Width,
       CurrentSlice, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
     BitBlt(hDC, 0, 0, Bitmap.Width, (Bitmap.Height - CurrentSlice),
       Bitmap.Canvas.Handle, 0, CurrentSlice, SRCCOPY);

(*
     with TCanvas.Create do
      try
       Handle := hDC;

       Draw(0, 0, Bitmap);
      finally
       Free;
      end;
*)
    finally
     CriticalSection.Leave;
    end;
   end;
end;

procedure TSESonogramGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
begin
 inherited;
 case CurrentPin.PinIndex of
  1 : begin
       FSonogram := TBitmapSonogram32(Pin[1].ValueAsInteger);
       if not (FSonogram is TBitmapSonogram32)
        then FSonogram := nil
        else GUIDraw(nil);
      end;
 end;
end;

procedure TSESonogramGui.GUIDraw(Sender: TObject);
begin
 CallHost(seGuiHostRequestRepaint);
end;

procedure TSESonogramGui.GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer);
begin
 CallHost(seGuiHostRequestRepaint);
end;

end.
