unit DAV_GuiStitchedRadioSwitch;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Messages, Classes, Graphics, Forms, SysUtils, Controls, Contnrs,
  DAV_GuiCommon, DAV_GuiStitchedControls;

type
  TCustomGuiRadioStitchedSwitch = class(TGuiCustomStitchedControl)
  private
    FGroupIndex : Integer;
    FReadOnly   : Boolean;
    procedure SetGroupIndex(const Value: Integer);

  protected
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateBuffer; override;
    procedure GroupIndexChanged; virtual;
  public
    constructor Create(AOwner: TComponent);

    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

  TGuiStitchedRadioSwitch = class(TCustomGuiRadioStitchedSwitch)
  published
    property Anchors;
    property AutoSize;
    property Color;
    property DefaultGlyphIndex;
    property GlyphIndex;
    property GroupIndex;
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

{ TCustomGuiRadioStitchedSwitch }

constructor TCustomGuiRadioStitchedSwitch.Create(AOwner: TComponent);
begin
 inherited;
 FReadOnly := False;
end;

procedure TCustomGuiRadioStitchedSwitch.GroupIndexChanged;
begin
 inherited;
 // yet todo !
end;

procedure TCustomGuiRadioStitchedSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
   if Parent <> nil then
    with Parent do
     for I := 0 to ControlCount - 1 do
      begin
       Sibling := Controls[I];
       if (Sibling <> Self) and (Sibling is TCustomGuiRadioStitchedSwitch) then
        with TCustomGuiRadioStitchedSwitch(Sibling) do
         if (GroupIndex = Self.GroupIndex) and (GlyphCount >= 1)
          then GlyphIndex := 0;
      end;
  end;

begin
 if Enabled and (not FReadOnly) and (Button = mbLeft) then
  if Assigned(FImageItem) then
   with StitchedImageItem do
    begin
     if GlyphCount >= 2
      then GlyphIndex := 1;
    end;

 inherited;
end;

procedure TCustomGuiRadioStitchedSwitch.CMEnabledChanged(var Message: TMessage);
begin
 if not Enabled then
  if (GlyphIndex in [0..1]) then
   begin
    if (GlyphIndex = 1) and (GlyphCount >= 4)
     then GlyphIndex := 3 else
    if (GlyphCount >= 3)
     then GlyphIndex := 2;
   end else else
 if (GlyphIndex in [2..3])
  then GlyphIndex := GlyphIndex - 2;
end;

procedure TCustomGuiRadioStitchedSwitch.SetGroupIndex(const Value: Integer);
begin
 if FGroupIndex <> Value then
  begin
   FGroupIndex := Value;
   GroupIndexChanged;
  end;
end;

procedure TCustomGuiRadioStitchedSwitch.UpdateBuffer;
begin
 inherited;

 if not (Assigned(FImageList) and (ImageIndex >= 0)) then
  if Assigned(FBuffer) then
   if (FGlyphIndex = 0)
    then FBuffer.FillRect(ClientRect, pxLime32)
    else FBuffer.FillRect(ClientRect, pxRed32);
end;

end.
