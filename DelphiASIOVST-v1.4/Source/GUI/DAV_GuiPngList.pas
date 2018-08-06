unit DAV_GuiPngList;

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
  Classes, SysUtils, DAV_GuiImageControl, DAV_GuiImageList, DAV_GuiPng;

type
  TGuiPNGCollectionItem = class(TGuiCustomImageCollectionItem)
  private
    FPng : TPortableNetworkGraphicPixel32;
    procedure SetPng(const Value: TPortableNetworkGraphicPixel32);
  protected
    procedure BuildPixelMap;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property PortableNetworkGraphic: TPortableNetworkGraphicPixel32 read FPng write SetPng;
    property DisplayName;
    property OnChange;
    property Height;
    property Width;
  end;

  TGuiPNGList = class(TGuiCustomImageList)
  private
    FImageCollection : TGuiImageCollection;
    function GetPngItems(Index: Integer): TGuiPNGCollectionItem;
  protected
    function GetItems(Index: Integer): TGuiCustomImageCollectionItem; override;
    function GetCount: Integer; override;
    procedure Loaded; override;
    property Items[Index: Integer]: TGuiPNGCollectionItem read GetPngItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PNGs: TGuiImageCollection read FImageCollection write FImageCollection;
  end;


implementation

uses
  DAV_Common, DAV_GuiBlend;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';


{ TGuiPNGCollectionItem }

constructor TGuiPNGCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FPng := TPortableNetworkGraphicPixel32.Create;
end;

destructor TGuiPNGCollectionItem.Destroy;
begin
 FreeAndNil(FPng);
 inherited;
end;

procedure TGuiPNGCollectionItem.BuildPixelMap;
begin
 if Assigned(FPng) and Assigned(FPixelMap) then
  begin
   FPixelMap.SetSize(FPng.Width, FPng.Height);
   FPng.DrawToPixelMap(FPixelMap);
  end;
end;

procedure TGuiPNGCollectionItem.SetPng(
  const Value: TPortableNetworkGraphicPixel32);
begin
 FPng.Assign(Value);

 BuildPixelMap;

 if Width <> FPng.Width
  then Width := FPng.Width;

 if Height <> FPng.Height
  then Height := FPng.Height;
end;


{ TGuiPNGList }

constructor TGuiPNGList.Create(AOwner: TComponent);
begin
 inherited;
 FImageCollection := TGuiImageCollection.Create(Self, TGuiPngCollectionItem);
end;

destructor TGuiPNGList.Destroy;
begin
 UnLinkImageControls;
 FreeAndNil(FImageCollection);
 inherited;
end;

function TGuiPNGList.GetCount: Integer;
begin
 Result := FImageCollection.Count;
end;

function TGuiPNGList.GetItems(Index: Integer): TGuiCustomImageCollectionItem;
begin
 if (Index >= 0) and (Index < FImageCollection.Count)
  then Result := TGuiCustomImageCollectionItem(FImageCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TGuiPNGList.GetPngItems(Index: Integer): TGuiPNGCollectionItem;
begin
 if (Index >= 0) and (Index < FImageCollection.Count)
  then Result := TGuiPNGCollectionItem(FImageCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiPNGList.Loaded;
var
  Index : Integer;
begin
 inherited;

 if Assigned(FImageCollection) then
  for Index := 0 to FImageCollection.Count - 1 do
   if FImageCollection[Index] is TGuiPNGCollectionItem
    then TGuiPNGCollectionItem(FImageCollection[Index]).BuildPixelMap;
end;

end.
