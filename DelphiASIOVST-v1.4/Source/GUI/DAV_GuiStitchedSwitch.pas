unit DAV_GuiStitchedSwitch;

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
  TCustomGuiStitchedSwitch = class(TGuiCustomStitchedControl)
  private
    FReadOnly : Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateBuffer; override;
  public
    constructor Create(AOwner: TComponent); override;

    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;

  TGuiStitchedSwitch = class(TCustomGuiStitchedSwitch)
  published
    property Anchors;
    property AutoSize;
    property Color;
    property DefaultGlyphIndex;
    property GlyphIndex;
    property PopupMenu;
    property ReadOnly;
    property ImageList;
    property ImageIndex;
    property Transparent;
    property OnChange;
  end;

implementation

uses
  DAV_Common, DAV_GuiBlend;

{ TCustomGuiStitchedSwitch }

constructor TCustomGuiStitchedSwitch.Create(AOwner: TComponent);
begin
 inherited;
 FReadOnly := False;
end;

procedure TCustomGuiStitchedSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 if not FReadOnly then
  if Assigned(FImageItem) then
   with StitchedImageItem do
    begin
     if (Button = mbLeft) then
      if FGlyphIndex < GlyphCount - 1
       then GlyphIndex := FGlyphIndex + 1
       else GlyphIndex := 0 else
     if (Button = mbRight) then
      if FGlyphIndex > 0
       then GlyphIndex := FGlyphIndex - 1
       else GlyphIndex := GlyphCount - 1;
    end
  else GlyphIndex := -1;

 inherited;
end;

procedure TCustomGuiStitchedSwitch.UpdateBuffer;
begin
 inherited;

 if not (Assigned(FImageList) and (ImageIndex >= 0)) then
  if Assigned(FBuffer) then
   if (FGlyphIndex = 0)
    then FBuffer.FillRect(ClientRect, pxLime32)
    else FBuffer.FillRect(ClientRect, pxRed32);
end;

end.
