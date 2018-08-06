unit DAV_GuiInscription;

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
  Classes, Graphics, Forms, SysUtils, Controls,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiGraphicControl, DAV_GuiShadow,
  DAV_GuiFont, DAV_GuiFontList;

type
  TCustomGuiInscription = class(TCustomGuiGraphicControl)
  private
    FAlignment        : TAlignment;
    FCaption          : string;
    FFontList         : TGuiCustomFontList;
    function GetFontIndex: Integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetFontIndex(Value: Integer);
    procedure SetFontList(const Value: TGuiCustomFontList);
    procedure SetFontItem(Value: TGuiCustomFontCollectionItem);
  protected
    FFontItem      : TGuiCustomFontCollectionItem;
    FFontItemIndex : Integer;
    procedure AlignmentChanged; virtual;
    procedure CaptionChanged; virtual;
    procedure FontIndexChanged; virtual;
    procedure FontListChanged; virtual;

    procedure UpdateBuffer; override;
    procedure FontChangedHandler(Sender: TObject); virtual;

    procedure Loaded; override;

    {$IFDEF FPC}
    procedure GMFontChanged(var Message: TLMessage); message GM_FontChanged;
    procedure GMFontListChanged(var Message: TLMessage); message GM_FontListChanged;
    {$ELSE}
    procedure GMFontChanged(var Message: TMessage); message GM_FontChanged;
    procedure GMFontListChanged(var Message: TMessage); message GM_FontListChanged;
    {$ENDIF}

    property FontItem: TGuiCustomFontCollectionItem read FFontItem write SetFontItem;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property FontIndex: Integer read GetFontIndex write SetFontIndex;
    property FontList: TGuiCustomFontList read FFontList write SetFontList;
  end;

  TGuiInscription = class(TCustomGuiInscription)
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
    property FontList;
    property FontIndex;
    property PopupMenu;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
(*
    property OnPaint;
    property OnMouseEnter;
    property OnMouseLeave;
*)
  end;

implementation

{ TCustomGuiInscription }

constructor TCustomGuiInscription.Create(AOwner: TComponent);
begin
 inherited;
end;

destructor TCustomGuiInscription.Destroy;
begin
 inherited;
end;

procedure TCustomGuiInscription.Loaded;
begin
 inherited;

 if Assigned(FFontList) then
  begin
   if FFontItemIndex >= FFontList.Count then
    begin
     FontIndex := -1;
     FFontItem := nil;
     Exit;
    end;

   if FFontItemIndex >= 0
    then FontItem := FFontList[FFontItemIndex];
   FontIndexChanged;
  end;
end;

procedure TCustomGuiInscription.UpdateBuffer;
var
  TextSize : TSize;
begin
 inherited;

 if Assigned(FFontItem) and Assigned(FFontItem.Font) then
  begin
   TextSize := FFontItem.Font.TextExtent(FCaption);
   case FAlignment of
    taLeftJustify  : TextSize.cx := 0;
    taRightJustify : TextSize.cx := Width - TextSize.cx;
    taCenter       : TextSize.cx := (Width - TextSize.cx) div 2;
   end;

   TextSize.cy := 0;
   FFontItem.Font.TextOut(FCaption, FBuffer, TextSize.cx, TextSize.cy);
  end;
end;

procedure TCustomGuiInscription.FontChangedHandler(Sender: TObject);
begin
 BufferChanged;
end;

function TCustomGuiInscription.GetFontIndex: Integer;
begin
 if Assigned(FFontItem)
  then Result := FFontItem.Index
  else Result := -1;
end;

procedure TCustomGuiInscription.GMFontChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 case Message.WParam of
  0 : if Message.LParam = 0
       then FontIndex := -1
 end;
 BufferChanged;
end;

procedure TCustomGuiInscription.GMFontListChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 case Message.WParam of
  0 : if Message.LParam = 0
       then FontList := nil;
  1 : BufferChanged;
 end;
end;

procedure TCustomGuiInscription.AlignmentChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.CaptionChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.FontIndexChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiInscription.FontListChanged;
begin
 FontItem := nil;
 BufferChanged;
end;

procedure TCustomGuiInscription.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   AlignmentChanged;
  end;
end;

procedure TCustomGuiInscription.SetCaption(const Value: string);
begin
 if FCaption <> Value then
  begin
   FCaption := Value;
   CaptionChanged;
  end;
end;

procedure TCustomGuiInscription.SetFontIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FFontItemIndex := Value;
   Exit;
  end;

 // check if Font image list is available
 if Assigned(FFontList) then
  begin
   // limit range to existing Font images (or -1 for nothing)
   if Value < 0 then Value := -1 else
   if Value >= FFontList.Count then Value := FFontList.Count - 1;

   if FontIndex <> Value then
    begin
     FFontItemIndex := Value;

     if Value > -1
      then FontItem := FFontList[Value]
      else FontItem := nil;

     FontIndexChanged;
    end;
  end;
end;

procedure TCustomGuiInscription.SetFontItem(
  Value: TGuiCustomFontCollectionItem);
begin
 if FFontItem <> Value then
  begin
   if not Assigned(Value) then
    begin
     Value := FFontItem;
     FFontItem := nil;
     Value.UnlinkControl(Self);
    end
   else
    begin
     if Assigned(FFontItem)
      then FFontItem.UnLinkControl(Self);
     FFontItem := Value;
     FFontItem.LinkControl(Self);
    end;
  end;
end;

procedure TCustomGuiInscription.SetFontList(const Value: TGuiCustomFontList);
begin
 if FFontList <> Value then
  begin
   // check whether a list is linked at all
   if not Assigned(Value) then
    begin
     Assert(Assigned(FFontList));
     FontItem := nil;
     FFontList.UnLinkControl(Self);
     FFontList := nil;
    end
   else
    begin
     Assert(Assigned(Value));
     FFontList := Value;
     FFontList.LinkControl(Self);
    end;
   FontListChanged;
  end;
end;

end.
