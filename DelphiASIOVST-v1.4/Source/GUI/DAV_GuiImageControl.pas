unit DAV_GuiImageControl;

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
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, Contnrs, DAV_GuiPixelMap,
  DAV_GuiCustomControl;

type
  // forward declarations
  TGuiCustomImageCollectionItem = class;
  TGuiCustomImageControl = class;

  TGuiImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiCustomImageCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiCustomImageCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiCustomImageCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Add: TGuiCustomImageCollectionItem;
    function Insert(Index: Integer): TGuiCustomImageCollectionItem;
    procedure Delete(Index: Integer);

    property Count;
  end;

  TGuiCustomImageCollectionItem = class(TCollectionItem)
  private
    FOnChange    : TNotifyEvent;
    FDisplayName : string;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetPixelMap(const Value: TGuiCustomPixelMap);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SettingsChanged(Sender: TObject);
  protected
    FPixelMap       : TGuiCustomPixelMap;
    FLinkedControls : TObjectList;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;

    procedure LinkImageControl(Image: TGuiCustomImageControl);
    procedure UnLinkImageControl(Image: TGuiCustomImageControl);
    procedure UnLinkImageControls;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property DisplayName: string read GetDisplayName write SetDisplayName;
    property PixelMap: TGuiCustomPixelMap read FPixelMap write SetPixelMap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiImageCollectionItem = class(TGuiCustomImageCollectionItem)
  published
    property DisplayName;
    property PixelMap;
    property OnChange;
    property Height;
    property Width;
  end;

  TGuiCustomImageList = class(TComponent)
  protected
    FLinkedControls  : TObjectList;
    function GetCount: Integer; virtual; abstract;
    function GetItems(Index: Integer): TGuiCustomImageCollectionItem; virtual; abstract;

    procedure LinkImageControl(ImageControl: TGuiCustomImageControl);
    procedure UnLinkImageControl(ImageControl: TGuiCustomImageControl);
    procedure UnLinkImageControls;

    property Items[Index: Integer]: TGuiCustomImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Count: Integer read GetCount;
  end;

  TGuiCustomImageControl = class(TGuiCustomControl)
  private
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    procedure SetImageItem(Value: TGuiCustomImageCollectionItem);
  protected
    FOnChange   : TNotifyEvent;
    FImageList  : TGuiCustomImageList;
    FImageItem  : TGuiCustomImageCollectionItem;
    FImageIndex : Integer;
    FLockCount  : Integer;

    procedure SetImageList(const Value: TGuiCustomImageList);

    procedure Changed; reintroduce; virtual;
    procedure ImageIndexChanged; virtual;
    procedure ImageListChanged; virtual;
    procedure ImageItemChanged; virtual;
    procedure UpdateBuffer; override;

    procedure Loaded; override;

    property ImageItem: TGuiCustomImageCollectionItem read FImageItem write SetImageItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property ImageList: TGuiCustomImageList read FImageList write SetImageList;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiCustomFader = class(TGuiCustomImageList)

  end;


implementation

uses
  DAV_GuiBlend;


{ TGuiStitchedImageCollection }

constructor TGuiImageCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 inherited Create(AOwner, ItemClass);
end;

function TGuiImageCollection.Add: TGuiCustomImageCollectionItem;
begin
 Result := TGuiCustomImageCollectionItem(inherited Add);
end;

procedure TGuiImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiImageCollection.GetItem(
  Index: Integer): TGuiCustomImageCollectionItem;
begin
 Result := TGuiCustomImageCollectionItem(inherited GetItem(Index));
end;

function TGuiImageCollection.Insert(
  Index: Integer): TGuiCustomImageCollectionItem;
begin
 Result:= TGuiCustomImageCollectionItem(inherited Insert(Index));
end;

procedure TGuiImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;

 if Item is TGuiCustomImageCollectionItem then
  with TGuiCustomImageCollectionItem(Item) do
   begin
    if Action in [cnDeleting, cnExtracting]
     then UnLinkImageControls;
   end;
end;

procedure TGuiImageCollection.SetItem(Index: Integer;
  const Value: TGuiCustomImageCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiCustomImageCollectionItem }

constructor TGuiCustomImageCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FPixelMap          := TGuiPixelMapMemory.Create;
 FPixelMap.OnResize := SettingsChanged;
 FLinkedControls    := TObjectList.Create(False);
end;

destructor TGuiCustomImageCollectionItem.Destroy;
begin
 FreeAndNil(FLinkedControls);
 FreeAndNil(FPixelMap);
 inherited;
end;

function TGuiCustomImageCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiCustomImageCollectionItem.GetHeight: Integer;
begin
 Result := FPixelMap.Height;
end;

function TGuiCustomImageCollectionItem.GetWidth: Integer;
begin
 Result := FPixelMap.Width;
end;

procedure TGuiCustomImageCollectionItem.LinkImageControl(Image: TGuiCustomImageControl);
begin
 Assert(Assigned(FLinkedControls));
 if FLinkedControls.IndexOf(Image) < 0
  then FLinkedControls.Add(Image);
end;

procedure TGuiCustomImageCollectionItem.UnLinkImageControl(Image: TGuiCustomImageControl);
begin
 Assert(Assigned(FLinkedControls));
 if Assigned(Image) then
  begin
   FLinkedControls.Remove(Image);
   Image.ImageIndex := -1;
  end;
end;

procedure TGuiCustomImageCollectionItem.UnLinkImageControls;
begin
 if Assigned(FLinkedControls) then
  while FLinkedControls.Count > 0
   do UnLinkImageControl(TGuiCustomImageControl(FLinkedControls[0]));
end;

procedure TGuiCustomImageCollectionItem.SettingsChanged(Sender: TObject);
var
  Index : Integer;
begin
 Assert(Assigned(FLinkedControls));
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomImageControl(FLinkedControls[Index]) do
   if FLockCount = 0 then BufferChanged;
end;

procedure TGuiCustomImageCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiCustomImageCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then Exit;
 FPixelMap.Width := Value;
end;

procedure TGuiCustomImageCollectionItem.SetHeight(const Value: Integer);
begin
 if Value < 0 then Exit;
 FPixelMap.Height := Value;
end;

procedure TGuiCustomImageCollectionItem.SetPixelMap(
  const Value: TGuiCustomPixelMap);
begin
 FPixelMap.Assign(Value);
end;


{ TGuiCustomImageList }

constructor TGuiCustomImageList.Create(AOwner: TComponent);
begin
 inherited;
 FLinkedControls := TObjectList.Create(False);
end;

destructor TGuiCustomImageList.Destroy;
begin
 FreeAndNil(FLinkedControls);
 inherited;
end;

procedure TGuiCustomImageList.LinkImageControl(
  ImageControl: TGuiCustomImageControl);
begin
 Assert(Assigned(FLinkedControls));
 if FLinkedControls.IndexOf(ImageControl) < 0
  then FLinkedControls.Add(ImageControl);
end;

procedure TGuiCustomImageList.UnLinkImageControl(
  ImageControl: TGuiCustomImageControl);
begin
 Assert(Assigned(FLinkedControls));
 if Assigned(ImageControl)
  then FLinkedControls.Remove(ImageControl);
end;

procedure TGuiCustomImageList.UnLinkImageControls;
begin
 Assert(Assigned(FLinkedControls));
 while FLinkedControls.Count > 0 do
  with TGuiCustomImageControl(FLinkedControls[0])
   do SetImageList(nil);
end;


{ TGuiCustomImageControl }

constructor TGuiCustomImageControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 {$IFDEF FPC}
 DoubleBuffered := True;
 {$ENDIF}

 ControlStyle   := ControlStyle + [csOpaque];
end;

destructor TGuiCustomImageControl.Destroy;
begin
 // unlink any Image item
 ImageItem := nil;

 inherited;
end;

procedure TGuiCustomImageControl.Loaded;
begin
 inherited;

 if Assigned(FImageList) then
  begin
   if FImageIndex >= FImageList.Count then
    begin
     ImageIndex := -1;
     FImageItem := nil;
     Exit;
    end;

   if FImageIndex >= 0
    then ImageItem := FImageList[FImageIndex];
   ImageIndexChanged;
  end;
end;

procedure TGuiCustomImageControl.SetImageIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FImageIndex := Value;
   Exit;
  end;

 // check if Image image list is available
 if Assigned(FImageList) then
  begin
   // limit range to existing Image images (or -1 for nothing)
   if Value < 0 then Value := -1 else
   if Value >= FImageList.Count then Value := FImageList.Count - 1;

   if ImageIndex <> Value then
    begin
     FImageIndex := Value;

     if Value > -1
      then ImageItem := FImageList[Value]
      else ImageItem := nil;

     ImageIndexChanged;
    end;
  end;
end;

procedure TGuiCustomImageControl.SetImageItem(
  Value: TGuiCustomImageCollectionItem);
begin
 if FImageItem <> Value then
  begin
   // check whether a item is linked at all
   if not Assigned(Value) then
    begin
     Assert(Assigned(FImageItem));
     Value := FImageItem;
     FImageItem := nil;
     Value.UnLinkImageControl(Self);
    end
   else
    begin
     Assert(Assigned(Value));
     if Assigned(FImageItem)
      then FImageItem.UnLinkImageControl(Self);
     FImageItem := Value;
     FImageItem.LinkImageControl(Self);
    end;
   ImageItemChanged;
  end;
end;

procedure TGuiCustomImageControl.SetImageList(const Value: TGuiCustomImageList);
begin
 if FImageList <> Value then
  begin
   // check whether a list is linked at all
   if not Assigned(Value) then
    begin
     Assert(Assigned(FImageList));
     ImageItem := nil;
     FImageList.UnLinkImageControl(Self);
     FImageList := nil;
    end
   else
    begin
     Assert(Assigned(Value));
     FImageList := Value;
     FImageList.LinkImageControl(Self);
    end;
   ImageListChanged;
  end;
end;

procedure TGuiCustomImageControl.BeginUpdate;
begin
 Inc(FLockCount);
end;

procedure TGuiCustomImageControl.EndUpdate;
begin
 if FLockCount <= 0
  then raise Exception.Create('Control is not locked');
 Dec(FLockCount);

 if FLockCount = 0
  then BufferChanged;
end;

procedure TGuiCustomImageControl.Changed;
begin
 inherited Changed;

 if Assigned(FOnChange) and ([csLoading, csDestroying] * ComponentState = [])
  then FOnChange(Self);
end;

function TGuiCustomImageControl.GetImageIndex: Integer;
begin
 if Assigned(FImageItem)
  then Result := FImageItem.Index
  else Result := -1;
end;

procedure TGuiCustomImageControl.ImageIndexChanged;
begin
 if FLockCount = 0
  then BufferChanged;
end;

procedure TGuiCustomImageControl.ImageItemChanged;
begin
 //
end;

procedure TGuiCustomImageControl.ImageListChanged;
begin
 ImageItem := nil;
 if FLockCount = 0
  then BufferChanged;
end;

procedure TGuiCustomImageControl.UpdateBuffer;
begin
 inherited;

 // yet todo
end;

end.
