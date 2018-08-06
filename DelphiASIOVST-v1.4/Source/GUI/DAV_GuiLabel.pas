unit DAV_GuiLabel;

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
  {$IFDEF FPC} LCLIntf, LMessages, Types, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, StdCtrls, DAV_GuiCommon,
  DAV_GuiGraphicControl, DAV_GuiPixelMap, DAV_GuiFont, DAV_GuiShadow;

type
  TCustomGuiLabel = class(TCustomGuiGraphicControl)
  private
    FGuiFont   : TGuiOversampledGDIFont;
    FAlignment : TAlignment;
    FCaption   : string;
    procedure SetOversampling(const Value: TFontOversampling);
    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetShadow(const Value: TGUIShadow);
    function GetOversampling: TFontOversampling;
    function GetShadow: TGUIShadow;
  protected
    {$IFNDEF FPC}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$ELSE}
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
    {$ENDIF}

    procedure FontChangedHandler(Sender: TObject); virtual;
    procedure UpdateBuffer; override;

    procedure AlignmentChanged; virtual;
    procedure CaptionChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property Shadow: TGUIShadow read GetShadow write SetShadow;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiLabel = class(TCustomGuiLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FontOversampling;
    property ParentFont;
    property PopupMenu;
    property Shadow;
    property ShowHint;
    property Visible;
    property Transparent;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnPaint;
  end;

implementation

{ TCustomGuiLabel }

constructor TCustomGuiLabel.Create(AOwner: TComponent);
begin
 inherited;
 FGuiFont          := TGuiOversampledGDIFont.Create;
 FGuiFont.OnChange := FontChangedHandler;
 FAlignment        := taLeftJustify;
end;

destructor TCustomGuiLabel.Destroy;
begin
 FreeAndNil(FGuiFont);
 inherited;
end;

procedure TCustomGuiLabel.UpdateBuffer;
var
  TextSize : TSize;
begin
 inherited;

 if Assigned(FGuiFont) then
  begin
   TextSize := FGuiFont.TextExtent(FCaption);
   case FAlignment of
    taLeftJustify  : TextSize.cx := 0;
    taRightJustify : TextSize.cx := Width - TextSize.cx;
    taCenter       : TextSize.cx := (Width - TextSize.cx) div 2;
   end;

   TextSize.cy := 0;
   FGuiFont.TextOut(FCaption, FBuffer, TextSize.cx, TextSize.cy);
  end;
end;

procedure TCustomGuiLabel.AlignmentChanged;
begin
 Invalidate;
end;

procedure TCustomGuiLabel.CaptionChanged;
begin
 BufferChanged;
end;

function TCustomGuiLabel.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

function TCustomGuiLabel.GetShadow: TGUIShadow;
begin
 Result := FGuiFont.Shadow;
end;

procedure TCustomGuiLabel.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

procedure TCustomGuiLabel.CMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FGuiFont.Font.Assign(Font);
 BufferChanged;
end;

procedure TCustomGuiLabel.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiLabel.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiLabel.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
 BufferChanged;
end;

procedure TCustomGuiLabel.SetShadow(const Value: TGUIShadow);
begin
 FGuiFont.Shadow.Assign(Value);
 BufferChanged;
end;

end.
