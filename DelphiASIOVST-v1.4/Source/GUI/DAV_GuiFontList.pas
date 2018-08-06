unit DAV_GuiFontList;

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
  Graphics, Classes, SysUtils, Controls, Contnrs, Messages, DAV_Common,
  DAV_GuiCommon, DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiFont;

const
  GM_FontChanged = WM_USER + $F0;
  GM_FontListChanged = WM_USER + $F1;

type
  // forward declarations
  TGuiCustomFontCollectionItem = class;

  TGuiFontMessage = (fmSet = 0, fmUpdate = 1);

  TGuiFontCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiCustomFontCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiCustomFontCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiCustomFontCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Add: TGuiCustomFontCollectionItem;
    function Insert(Index: Integer): TGuiCustomFontCollectionItem;
    procedure Delete(Index: Integer);

    property Count;
  end;

  TGuiCustomFontCollectionItem = class(TCollectionItem)
  private
    FOnChange           : TNotifyEvent;
    FDisplayName        : string;
    FFont               : TGuiCustomFont;
    FOnFontClassChanged : TNotifyEvent;
    FLinkedControls     : TObjectList;
    function GetFontClassName: string;
    procedure SetFont(const Value: TGuiCustomFont);
    procedure SetFontClassName(const Value: string);
    procedure SettingsChanged(Sender: TObject);
  protected
    procedure Changed;
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure LinkControl(Control: TControl);
    procedure UnLinkControl(Control: TControl);
    procedure UnLinkControls;

    property DisplayName: string read GetDisplayName write SetDisplayName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property FontClassName: string read GetFontClassName write SetFontClassName;
    property Font: TGuiCustomFont read FFont write SetFont;
    property OnFontClassChanged: TNotifyEvent read FOnFontClassChanged write FOnFontClassChanged;
  end;

  TGuiFontCollectionItem = class(TGuiCustomFontCollectionItem)
  published
    property DisplayName;
    property FontClassName;
    property Font;
    property OnFontClassChanged;
    property OnChange;
  end;

  TGuiCustomFontList = class(TComponent)
  private
    function GetItems(Index: Integer): TGuiCustomFontCollectionItem;
  protected
    FFontCollection : TGuiFontCollection;
    FLinkedControls : TObjectList;
    function GetCount: Integer; virtual; abstract;
    property Items[Index: Integer]: TGuiCustomFontCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LinkControl(Control: TControl);
    procedure UnLinkControl(Control: TControl);
    procedure UnLinkControls;

    property Count: Integer read GetCount;
  end;

  TGuiFontList = class(TGuiCustomFontList)
  private
    function GetItems(Index: Integer): TGuiFontCollectionItem;
  protected
    function GetCount: Integer; override;
    property Items[Index: Integer]: TGuiFontCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fonts: TGuiFontCollection read FFontCollection write FFontCollection;
  end;

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TGuiFontCollection }

constructor TGuiFontCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 inherited Create(AOwner, ItemClass);
end;

function TGuiFontCollection.Add: TGuiCustomFontCollectionItem;
begin
 Result := TGuiCustomFontCollectionItem(inherited Add);
end;

procedure TGuiFontCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiFontCollection.GetItem(
  Index: Integer): TGuiCustomFontCollectionItem;
begin
 Result := TGuiCustomFontCollectionItem(inherited GetItem(Index));
end;

function TGuiFontCollection.Insert(
  Index: Integer): TGuiCustomFontCollectionItem;
begin
 Result:= TGuiCustomFontCollectionItem(inherited Insert(Index));
end;

procedure TGuiFontCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;

 if Item is TGuiCustomFontCollectionItem then
  with TGuiCustomFontCollectionItem(Item) do
   begin
    if Action in [cnDeleting, cnExtracting]
     then UnLinkControls;
   end;
end;

procedure TGuiFontCollection.SetItem(Index: Integer;
  const Value: TGuiCustomFontCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiCustomFontCollectionItem }

constructor TGuiCustomFontCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FLinkedControls := TObjectList.Create(False);
 FFont := TGuiSimpleGDIFont.Create;
end;

destructor TGuiCustomFontCollectionItem.Destroy;
begin
 FreeAndNil(FLinkedControls);
 if Assigned(FFont)
  then FreeAndNil(FFont);
 inherited;
end;

procedure TGuiCustomFontCollectionItem.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomFontCollectionItem then
  with TGuiCustomFontCollectionItem(Dest) do
   begin
    FFont.Assign(Self.FFont);
    FDisplayName := Self.FDisplayName;
    FOnFontClassChanged := Self.FOnFontClassChanged;
   end;
end;

procedure TGuiCustomFontCollectionItem.Changed;
begin
 if Assigned(FOnFontClassChanged)
  then FOnFontClassChanged(Self);
end;

function TGuiCustomFontCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiCustomFontCollectionItem.GetFontClassName: string;
begin
 if Assigned(FFont)
  then Result := FFont.ClassName
  else Result := '';
end;

procedure TGuiCustomFontCollectionItem.LinkControl(Control: TControl);
begin
 if FLinkedControls.IndexOf(Font) < 0 then
  begin
   FLinkedControls.Add(Font);
   Control.Perform(GM_FontChanged, 0, Integer(Self));
  end;
end;

procedure TGuiCustomFontCollectionItem.UnLinkControl(Control: TControl);
begin
 if Assigned(Control) then
  begin
   if FLinkedControls.Remove(Control) <> 0
    then Control.Perform(GM_FontChanged, 0, 0);
  end;
end;

procedure TGuiCustomFontCollectionItem.UnLinkControls;
var
  Index : Integer;
begin
 if Assigned(FLinkedControls) then
  while FLinkedControls.Count > 0
   do UnLinkControl(TControl(FLinkedControls[0]));

(*
Index := 0;
 if Assigned(FLinkedControls) then
  while FLinkedControls.Count > Index do
   with TControl(FLinkedControls[Index]) do
    if Perform(GM_FontChanged, 0, 0) <> 1
     then Inc(Index);
*)
end;

procedure TGuiCustomFontCollectionItem.SettingsChanged(Sender: TObject);
var
  Index : Integer;
begin
 for Index := 0 to FLinkedControls.Count - 1 do
  with TControl(FLinkedControls[Index])
   do Perform(GM_FontChanged, 1, Integer(Self));
end;

procedure TGuiCustomFontCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiCustomFontCollectionItem.SetFont(const Value: TGuiCustomFont);
begin
 FFont.Assign(Value);
end;

procedure TGuiCustomFontCollectionItem.SetFontClassName(const Value: string);
var
  FontClass: TGuiCustomFontClass;
begin
 if (Value <> '') and (FFont.ClassName <> Value) and Assigned(FontClassList) then
  begin
   FontClass := TGuiCustomFontClass(FontClassList.Find(Value));
   if Assigned(FontClass) then
    begin
     FFont.Free;
     FFont := FontClass.Create;
     Changed;
    end;
  end;
end;


{ TGuiFontList }

constructor TGuiFontList.Create(AOwner: TComponent);
begin
 inherited;
 FFontCollection := TGuiFontCollection.Create(Self, TGuiFontCollectionItem);
end;

destructor TGuiFontList.Destroy;
begin
 UnLinkControls;
 FreeAndNil(FFontCollection);
 inherited;
end;

function TGuiFontList.GetCount: Integer;
begin
 Result := FFontCollection.Count;
end;

function TGuiFontList.GetItems(Index: Integer): TGuiFontCollectionItem;
begin
 if (Index >= 0) and (Index < FFontCollection.Count)
  then Result := TGuiFontCollectionItem(FFontCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;


{ TGuiCustomFontList }

constructor TGuiCustomFontList.Create(AOwner: TComponent);
begin
 inherited;
 FLinkedControls := TObjectList.Create(False);
end;

destructor TGuiCustomFontList.Destroy;
begin
 FreeAndNil(FLinkedControls);
 inherited;
end;

function TGuiCustomFontList.GetItems(
  Index: Integer): TGuiCustomFontCollectionItem;
begin
 Assert(Assigned(FFontCollection));
 if (Index >= 0) and (Index < FFontCollection.Count)
  then Result := TGuiCustomFontCollectionItem(FFontCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiCustomFontList.LinkControl(Control: TControl);
begin
 Assert(Assigned(FLinkedControls));
 if FLinkedControls.IndexOf(Control) < 0 then
  begin
   FLinkedControls.Add(Control);
   Control.Perform(GM_FontListChanged, 0, Integer(Self));
  end;
end;

procedure TGuiCustomFontList.UnLinkControl(Control: TControl);
begin
 Assert(Assigned(FLinkedControls));
 if Assigned(Control)
  then FLinkedControls.Remove(Control);
end;

procedure TGuiCustomFontList.UnLinkControls;
begin
 Assert(Assigned(FLinkedControls));
 while FLinkedControls.Count > 0
  do TControl(FLinkedControls[0]).Perform(GM_FontListChanged, 0, 0);
end;

end.
