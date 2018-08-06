unit DAV_GuiImageList;

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
  DAV_GuiCommon, DAV_GuiImageControl;

type
  TGuiImageList = class(TGuiCustomImageList)
  protected
    FImageCollection : TGuiImageCollection;
    function GetItems(Index: Integer): TGuiCustomImageCollectionItem; override;
    function GetCount: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Images: TGuiImageCollection read FImageCollection write FImageCollection;
  end;

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';


{ TGuiImageList }

constructor TGuiImageList.Create(AOwner: TComponent);
begin
 inherited;
 FImageCollection := TGuiImageCollection.Create(Self, TGuiImageCollectionItem);
end;

destructor TGuiImageList.Destroy;
begin
 FreeAndNil(FImageCollection);
 inherited;
end;

function TGuiImageList.GetCount: Integer;
begin
 Result := FImageCollection.Count;
end;

function TGuiImageList.GetItems(
  Index: Integer): TGuiCustomImageCollectionItem;
begin
 Assert(Assigned(FImageCollection));
 if (Index >= 0) and (Index < FImageCollection.Count)
  then Result := TGuiCustomImageCollectionItem(FImageCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.

