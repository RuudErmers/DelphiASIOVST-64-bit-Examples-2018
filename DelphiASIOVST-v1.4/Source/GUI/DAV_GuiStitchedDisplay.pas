unit DAV_GuiStitchedDisplay;

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
  TCustomGuiStitchedDisplay = class(TGuiCustomStitchedControl)
  protected
    procedure UpdateBuffer; override;
  end;

  TGuiStitchedDisplay = class(TCustomGuiStitchedDisplay)
  published
    property Anchors;
    property AutoSize;
    property DefaultGlyphIndex;
    property GlyphIndex;
    property PopupMenu;
    property ImageList;
    property ImageIndex;
    property Transparent;

    property OnDblClick;
    property OnChange;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnEnter;
    property OnExit;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;

implementation

uses
  DAV_Common, DAV_GuiBlend;

{ TCustomGuiStitchedDisplay }

procedure TCustomGuiStitchedDisplay.UpdateBuffer;
begin
 inherited;
 if not (Assigned(FImageList) and (ImageIndex >= 0)) then
  if Assigned(FBuffer)
   then FBuffer.FillRect(ClientRect, ConvertColor(Color));
end;

end.
