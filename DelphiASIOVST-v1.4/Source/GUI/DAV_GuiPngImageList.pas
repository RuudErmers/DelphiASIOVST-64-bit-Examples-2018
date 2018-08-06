unit DAV_GuiPngImageList;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, SysUtils, ImgList, Contnrs, DAV_GuiCommon,
  DAV_GuiPixelMap, DAV_GuiPng;

type
  TGuiPngImageCollectionItem = class;
  TGuiPngImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiPngImageCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiPngImageCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiPngImageCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiPngImageCollectionItem;
    function Insert(Index: Integer): TGuiPngImageCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGuiPngImageCollectionItem = class(TCollectionItem)
  private
    FPngBitmap    : TBitmap;
    FOnChange     : TNotifyEvent;
    FLinkedImages : TObjectList;
    FDisplayName  : string;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetImageBitmap(const Value: TBitmap);
    procedure SettingsChanged(Sender: TObject);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property ImageBitmap: TBitmap read FPngBitmap write SetImageBitmap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiPngImageList = class(TComponent)
  private
    FPngImageCollection : TGuiPngImageCollection;
    function GetCount: Integer;
    function GetItems(Index: Integer): TGuiPngImageCollectionItem;
  protected
    property Items[Index: Integer]: TGuiPngImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PngImages: TGuiPngImageCollection read FPngImageCollection write FPngImageCollection;
    property Count: Integer read GetCount;
  end;

implementation

{ TGuiPngImageCollection }

constructor TGuiPngImageCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiPngImageCollectionItem);
end;

function TGuiPngImageCollection.Add: TGuiPngImageCollectionItem;
begin
 Result := TGuiPngImageCollectionItem(inherited Add);
end;

procedure TGuiPngImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiPngImageCollection.GetItem(
  Index: Integer): TGuiPngImageCollectionItem;
begin
 Result := TGuiPngImageCollectionItem(inherited GetItem(Index));
end;

function TGuiPngImageCollection.Insert(
  Index: Integer): TGuiPngImageCollectionItem;
begin
 Result:= TGuiPngImageCollectionItem(inherited Insert(Index));
end;

procedure TGuiPngImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 // add things that depend on the order here!
end;

procedure TGuiPngImageCollection.SetItem(Index: Integer;
  const Value: TGuiPngImageCollectionItem);
begin
 inherited SetItem(Index, Value);
end;

{ TGuiPngImageCollectionItem }

constructor TGuiPngImageCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FPngBitmap          := TBitmap.Create;
 FPngBitmap.OnChange := SettingsChanged;
end;

destructor TGuiPngImageCollectionItem.Destroy;
begin
 FreeAndNil(FPngBitmap);
 inherited;
end;

function TGuiPngImageCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiPngImageCollectionItem.GetHeight: Integer;
begin
 Result := FPngBitmap.Height;
end;

function TGuiPngImageCollectionItem.GetWidth: Integer;
begin
 Result := FPngBitmap.Width;
end;

procedure TGuiPngImageCollectionItem.SettingsChanged(Sender: TObject);
begin
(*
var
  i : Integer;
 for i := 0 to FLinkedPngs.Count - 1 do
  with TCustomGuiPng(FLinkedPngs[i]) do
   begin
    GlyphCount := Self.GlyphCount;
    StitchKind := Self.StitchKind;
    Invalidate;
   end;
*)
end;

procedure TGuiPngImageCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then Exit;
 FPngBitmap.Width := Value;
end;

procedure TGuiPngImageCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiPngImageCollectionItem.SetHeight(const Value: Integer);
begin
 if Value < 0 then Exit;
 FPngBitmap.Height := Value;
end;

procedure TGuiPngImageCollectionItem.SetImageBitmap(const Value: TBitmap);
begin
 FPngBitmap.Assign(Value);
end;


{ TGuiPngImageList }

constructor TGuiPngImageList.Create(AOwner: TComponent);
begin
  inherited;
  FPngImageCollection := TGuiPngImageCollection.Create(Self);
end;

destructor TGuiPngImageList.Destroy;
begin
  FreeAndNil(FPngImageCollection);
  inherited;
end;

function TGuiPngImageList.GetCount: Integer;
begin
  Result := FPngImageCollection.Count;
end;

function TGuiPngImageList.GetItems(Index: Integer): TGuiPngImageCollectionItem;
begin
 if (Index >= 0) and (Index < FPngImageCollection.Count)
  then Result := FPngImageCollection[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

end.
