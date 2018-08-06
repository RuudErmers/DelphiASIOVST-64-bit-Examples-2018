unit DAV_GuiStitchedImageList;

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
  Classes, SysUtils, DAV_GuiStitchedControls, DAV_GuiImageControl, DAV_GuiPng;

type
  TGuiStitchedImageCollectionItem = class(TGuiCustomStitchedCollectionItem)
  published
    property DisplayName;
    property PixelMap;
    property GlyphCount;
    property StitchKind;
    property OnChange;
    property Height;
    property Width;
  end;

  TGuiStitchedImageList = class(TGuiCustomStitchedList)
  private
    function GetStitchedItems(Index: Integer): TGuiStitchedImageCollectionItem;
  protected
    function GetCount: Integer; override;
    property Items[Index: Integer]: TGuiStitchedImageCollectionItem read GetStitchedItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StitchedImages: TGuiStitchedImageCollection read FStitchedCollection write FStitchedCollection;
  end;

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TGuiStitchedImageList }

constructor TGuiStitchedImageList.Create(AOwner: TComponent);
begin
  inherited;
  FStitchedCollection := TGuiStitchedImageCollection.Create(Self, TGuiStitchedImageCollectionItem);
end;

destructor TGuiStitchedImageList.Destroy;
begin
 UnLinkImageControls;
 FreeAndNil(FStitchedCollection);
 inherited;
end;

function TGuiStitchedImageList.GetCount: Integer;
begin
 Result := FStitchedCollection.Count;
end;

function TGuiStitchedImageList.GetStitchedItems(Index: Integer): TGuiStitchedImageCollectionItem;
begin
 if (Index >= 0) and (Index < FStitchedCollection.Count)
  then Result := TGuiStitchedImageCollectionItem(FStitchedCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.
