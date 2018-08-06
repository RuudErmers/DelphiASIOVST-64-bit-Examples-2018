unit DAV_GuiStitchedButton;

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
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, Messages,
  {$ENDIF} Classes, Graphics, Forms, SysUtils, Controls, Contnrs,
  DAV_GuiCommon, DAV_GuiStitchedControls;

type
  TCustomGuiStitchedButton = class(TGuiCustomStitchedControl)
  private
    FMouseInRect : Boolean;
  protected
    procedure UpdateBuffer; override;
    {$IFDEF FPC}
    procedure CMMouseEnter(var Message: TLMessage); message LM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message LM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    {$ELSE}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    {$ENDIF}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure Click; override;
  public
    property GlyphIndex read FGlyphIndex;
  end;

  TGuiStitchedButton = class(TCustomGuiStitchedButton)
  published
    property AutoSize;
    property ImageList;
    property ImageIndex;
    property Transparent;

    property OnChange;
    property OnClick;
  end;

implementation

uses
  DAV_Common, DAV_GuiBlend;

{ TCustomGuiStitchedButton }

procedure TCustomGuiStitchedButton.Click;
begin
 if Enabled then inherited;
end;

procedure TCustomGuiStitchedButton.CMEnabledChanged(var Message:
  {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 if Assigned(FImageItem) then
  with StitchedImageItem do
   if (GlyphCount > 2) and (not Enabled)
    then GlyphIndex := 2 else
   if (GlyphCount > 3) and FMouseInRect
    then GlyphIndex := 3
    else GlyphIndex := 0;
end;

procedure TCustomGuiStitchedButton.CMMouseEnter(var Message:
  {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FMouseInRect := True;
 if Enabled and Assigned(FImageItem) then
  with StitchedImageItem do
   if (GlyphCount > 3) and (GlyphIndex = 0)
    then GlyphIndex := 3;
end;

procedure TCustomGuiStitchedButton.CMMouseLeave(var Message:
    {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FMouseInRect := False;
 if Enabled and Assigned(FImageItem) then
  with StitchedImageItem do
   if (GlyphIndex = 3)
    then GlyphIndex := 0;
end;

procedure TCustomGuiStitchedButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Enabled and Assigned(FImageItem) then
  with StitchedImageItem do
   if (GlyphCount > 1)
    then GlyphIndex := 1;
end;

procedure TCustomGuiStitchedButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Enabled and Assigned(FImageItem) then
  with StitchedImageItem do
   if (GlyphCount > 3) then
    if FMouseInRect
     then GlyphIndex := 3 else
    else GlyphIndex := 0;
end;

procedure TCustomGuiStitchedButton.UpdateBuffer;
begin
 inherited;

(*
 if not (Assigned(FStitchedList) and (FStitchedImageIndex >= 0)) then
  if Assigned(FBuffer) and (FGlyphIndex = 0)
   then FBuffer.FillRect(ClientRect, pxLime32)
   else FBuffer.FillRect(ClientRect, pxRed32);
*)
end;

end.
