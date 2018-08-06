unit DAV_GuiStitchedControls;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF} Classes, Graphics,
  SysUtils, Controls, DAV_GuiCommon, DAV_GuiImageControl;

type
  TGuiStitchKind = (skHorizontal, skVertical);

  // forward declarations
  TGuiCustomStitchedCollectionItem = class;
  TGuiCustomStitchedControl = class;

  TGuiStitchedImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiCustomStitchedCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiCustomStitchedCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiCustomStitchedCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);

    function Add: TGuiCustomStitchedCollectionItem;
    function Insert(Index: Integer): TGuiCustomStitchedCollectionItem;
    procedure Delete(Index: Integer);

    property Count;
  end;

  TGuiCustomStitchedCollectionItem = class(TGuiCustomImageCollectionItem)
  private
    FGlyphCount : Integer;
    FStitchKind : TGuiStitchKind;
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
  protected
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;
  public
    constructor Create(Collection: TCollection); override;

    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind default skHorizontal;
  end;

  TGuiCustomStitchedList = class(TGuiCustomImageList)
  private
    function GetStitchedItems(Index: Integer): TGuiCustomStitchedCollectionItem;
  protected
    FStitchedCollection : TGuiStitchedImageCollection;
    function GetItems(Index: Integer): TGuiCustomImageCollectionItem; override;
    property Items[Index: Integer]: TGuiCustomStitchedCollectionItem read GetStitchedItems; default;
  end;

  TGuiCustomStitchedControl = class(TGuiCustomImageControl)
  private
    function GetGlyphCount: Integer;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetGlyphIndex(Value: Integer);
    procedure SetDefaultGlyphIndex(Value: Integer);
    procedure SetStitchedImageList(const Value: TGuiCustomStitchedList);
    function GetStitchedImageItem: TGuiCustomStitchedCollectionItem;
    function GetStitchedImageList: TGuiCustomStitchedList;
  protected
    FAutoSize           : Boolean;
    FGlyphIndex         : Integer;
    FDefaultGlyphIndex  : Integer;

    procedure Changed; reintroduce; virtual;
    procedure DefaultGlyphIndexChanged; virtual;
    procedure DoAutomaticResize; virtual;
    procedure GlyphIndexChanged; virtual;
    procedure ImageItemChanged; override;
    procedure UpdateBuffer; override;

    procedure Loaded; override;

    property DefaultGlyphIndex: Integer read FDefaultGlyphIndex write SetDefaultGlyphIndex;
    property GlyphIndex: Integer read FGlyphIndex write SetGlyphIndex;
    property StitchedImageItem: TGuiCustomStitchedCollectionItem read GetStitchedImageItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ImageList: TGuiCustomStitchedList read GetStitchedImageList write SetStitchedImageList;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property GlyphCount: Integer read GetGlyphCount;
  end;

implementation

uses
  DAV_Common, DAV_GuiBlend;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrGlyphCountMustBePositive = 'The glyph count must be positive';

{ TGuiStitchedImageCollection }

constructor TGuiStitchedImageCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
 inherited Create(AOwner, ItemClass);
end;

function TGuiStitchedImageCollection.Add: TGuiCustomStitchedCollectionItem;
begin
 Result := TGuiCustomStitchedCollectionItem(inherited Add);
end;

procedure TGuiStitchedImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiStitchedImageCollection.GetItem(
  Index: Integer): TGuiCustomStitchedCollectionItem;
begin
 Result := TGuiCustomStitchedCollectionItem(inherited GetItem(Index));
end;

function TGuiStitchedImageCollection.Insert(
  Index: Integer): TGuiCustomStitchedCollectionItem;
begin
 Result:= TGuiCustomStitchedCollectionItem(inherited Insert(Index));
end;

procedure TGuiStitchedImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;

 if Item is TGuiCustomStitchedCollectionItem then
  with TGuiCustomStitchedCollectionItem(Item) do
   begin
    if Action in [cnDeleting, cnExtracting]
     then UnLinkImageControls;
   end;
end;

procedure TGuiStitchedImageCollection.SetItem(Index: Integer;
  const Value: TGuiCustomStitchedCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiCustomStitchedCollectionItem }

constructor TGuiCustomStitchedCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FGlyphCount := 1;
 FStitchKind := skHorizontal;
end;

procedure TGuiCustomStitchedCollectionItem.GlyphCountChanged;
var
  Index : Integer;
begin
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedControls[Index]) do
   begin
    if FDefaultGlyphIndex >= FGlyphCount then DefaultGlyphIndex := FGlyphCount - 1;
    if FGlyphIndex >= FGlyphCount then GlyphIndex := FGlyphCount - 1;

    if FAutoSize then DoAutomaticResize;
   end;
end;

procedure TGuiCustomStitchedCollectionItem.StitchKindChanged;
var
  Index : Integer;
begin
 for Index := 0 to FLinkedControls.Count - 1 do
  with TGuiCustomStitchedControl(FLinkedControls[Index]) do
   if FAutoSize then DoAutomaticResize;
end;

procedure TGuiCustomStitchedCollectionItem.SetGlyphCount(const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrGlyphCountMustBePositive);

 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TGuiCustomStitchedCollectionItem.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;


{ TGuiCustomStitchedList }

function TGuiCustomStitchedList.GetItems(
  Index: Integer): TGuiCustomImageCollectionItem;
begin
 Assert(Assigned(FStitchedCollection));
 if (Index >= 0) and (Index < FStitchedCollection.Count)
  then Result := TGuiCustomImageCollectionItem(FStitchedCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TGuiCustomStitchedList.GetStitchedItems(
  Index: Integer): TGuiCustomStitchedCollectionItem;
begin
 Assert(Assigned(FStitchedCollection));
 if (Index >= 0) and (Index < FStitchedCollection.Count)
  then Result := TGuiCustomStitchedCollectionItem(FStitchedCollection[Index])
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;


{ TGuiCustomStitchedControl }

constructor TGuiCustomStitchedControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FGlyphIndex        := 0;
 FDefaultGlyphIndex := 0;
 {$IFDEF FPC}
 DoubleBuffered     := True;
 {$ENDIF}

 ControlStyle       := ControlStyle + [csOpaque];
end;

destructor TGuiCustomStitchedControl.Destroy;
begin
 // unlink any stitched item
 ImageItem := nil;

 inherited;
end;

procedure TGuiCustomStitchedControl.DoAutomaticResize;
begin
 if Assigned(FImageItem) then
  with StitchedImageItem do
   begin
    if (GlyphCount = 0) then Exit;

    if StitchKind = skVertical then
     begin
      Self.Width  := FPixelMap.Width;
      Self.Height := FPixelMap.Height div GlyphCount;
     end
    else
     begin
      Self.Width  := FPixelMap.Width div GlyphCount;
      Self.Height := FPixelMap.Height;
     end;
   end;
end;

procedure TGuiCustomStitchedControl.Loaded;
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

 // check and set glyph index
 if FGlyphIndex > -1 then
  begin
   if Assigned(FImageItem) and (FGlyphIndex >= StitchedImageItem.FGlyphCount)
    then FGlyphIndex := -1;
  end;

 // check and set default glyph index
 if FDefaultGlyphIndex > -1 then
  begin
   if Assigned(FImageItem) and (FDefaultGlyphIndex >= StitchedImageItem.FGlyphCount)
    then FDefaultGlyphIndex := -1;
  end;
end;

procedure TGuiCustomStitchedControl.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutomaticResize;
  end;
end;

procedure TGuiCustomStitchedControl.SetDefaultGlyphIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FDefaultGlyphIndex := Value;
   Exit;
  end;

 if Assigned(FImageItem)
  then Value := Limit(Value, 0, StitchedImageItem.GlyphCount - 1)
  else Value := -1;

 if Value <> FDefaultGlyphIndex then
  begin
   FDefaultGlyphIndex := Value;
   DefaultGlyphIndexChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetGlyphIndex(Value: Integer);
begin
 if csLoading in ComponentState then
  begin
   FGlyphIndex := Value;
   Exit;
  end;

 if Assigned(FImageItem)
  then Value := Limit(Value, 0, StitchedImageItem.GlyphCount - 1)
  else Value := -1;

 if Value <> FGlyphIndex then
  begin
   FGlyphIndex := Value;
   GlyphIndexChanged;
  end;
end;

procedure TGuiCustomStitchedControl.SetStitchedImageList(
  const Value: TGuiCustomStitchedList);
begin
 inherited SetImageList(Value);
end;

procedure TGuiCustomStitchedControl.Changed;
begin
 inherited Changed;

 if Assigned(FOnChange) and ([csLoading, csDestroying] * ComponentState = [])
  then FOnChange(Self);
end;

procedure TGuiCustomStitchedControl.DefaultGlyphIndexChanged;
begin
 //
end;

function TGuiCustomStitchedControl.GetGlyphCount: Integer;
begin
 if Assigned(FImageItem)
  then Result := StitchedImageItem.GlyphCount
  else Result := 0;
end;

function TGuiCustomStitchedControl.GetStitchedImageItem: TGuiCustomStitchedCollectionItem;
begin
 Assert(ImageItem is TGuiCustomStitchedCollectionItem);
 Result := TGuiCustomStitchedCollectionItem(ImageItem)
end;

function TGuiCustomStitchedControl.GetStitchedImageList: TGuiCustomStitchedList;
begin
 if FImageList is TGuiCustomStitchedList
  then Result := TGuiCustomStitchedList(FImageList)
  else Result := nil;
end;

procedure TGuiCustomStitchedControl.GlyphIndexChanged;
begin
 Changed;
 if FLockCount = 0
  then BufferChanged;
end;

procedure TGuiCustomStitchedControl.ImageItemChanged;
begin
 if Assigned(FImageItem) and FAutoSize then
  with StitchedImageItem do
   case StitchKind of
    skHorizontal :
     begin
      Self.Width  := Width div GlyphCount;
      Self.Height := Height;
     end;
    skVertical :
     begin
      Self.Width  := Width;
      Self.Height := Height div GlyphCount;
     end;
   end;
 GlyphIndex := -1;
end;

procedure TGuiCustomStitchedControl.UpdateBuffer;
var
  DataPointer : PPixel32Array;
  LineIndex   : Integer;
begin
 inherited;

 if Assigned(FImageItem) and (FGlyphIndex >= 0) then
  with StitchedImageItem do
   begin
    // check whether the stitched item contains at least one glyph
    if GlyphCount = 0 then Exit;

    case StitchKind of
     skHorizontal :
      if FGlyphIndex * Self.Width < PixelMap.Width then
       begin
        DataPointer := @PixelMap.DataPointer[FGlyphIndex * Self.Width];
        for LineIndex := 0 to Self.Height - 1 do
         begin
          BlendLine(PPixel32(DataPointer), PPixel32(FBuffer.ScanLine[LineIndex]), Self.Width);
          DataPointer := @DataPointer[PixelMap.Width];
         end;
        EMMS;
       end;
     skVertical   :
      if FGlyphIndex * Self.Height < PixelMap.Height then
       begin
        DataPointer := PixelMap.ScanLine[FGlyphIndex * Self.Height];
        BlendLine(PPixel32(DataPointer), PPixel32(FBuffer.DataPointer),
          Self.Height * Self.Width);
        EMMS;
       end;
    end;
   end;
end;

end.
