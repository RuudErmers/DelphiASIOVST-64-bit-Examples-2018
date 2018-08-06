unit DAV_GuiMidiKeyZones;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Graphics;

type
  TGuiKeyZoneItem = class(TCollectionItem)
  protected
    FDefaultBrushColor   : TColor;
    FDefaultBrushStyle   : TBrushStyle;
    FDefaultBorderColor  : TColor;
    FDefaultBorderWidth  : Integer;
    FDefaultBorderStyle  : TPenStyle;

    FHoverBrushColor     : TColor;
    FHoverBrushStyle     : TBrushStyle;
    FHoverBorderColor    : TColor;
    FHoverBorderWidth    : Integer;
    FHoverBorderStyle    : TPenStyle;

    FSelectedBrushColor  : TColor;
    FSelectedBrushStyle  : TBrushStyle;
    FSelectedBorderColor : TColor;
    FSelectedBorderWidth : Integer;
    FSelectedBorderStyle : TPenStyle;

    FDisplayName         : String;
    FVisible             : Boolean;
    FSelected            : Boolean;
    FIsMouseOver         : Boolean;

    FLowestZoneKey       : Byte;
    FHighestZoneKey      : Byte;

    FTag                 : Integer;

    procedure SetLowestZoneKey(const Value: Byte);
    procedure SetHighestZoneKey(const Value: Byte);
    procedure SetVisible(const Value: Boolean);
    procedure SetDefaultBrushColor(const Value: TColor);
    procedure SetDefaultBrushStyle(const Value: TBrushStyle);
    procedure SetDefaultBorderColor(const Value: TColor);
    procedure SetDefaultBorderWidth(const Value: Integer);
    procedure SetDefaultBorderStyle(const Value: TPenStyle);

    procedure SetHoverBrushColor(const Value: TColor);
    procedure SetHoverBrushStyle(const Value: TBrushStyle);
    procedure SetHoverBorderColor(const Value: TColor);
    procedure SetHoverBorderWidth(const Value: Integer);
    procedure SetHoverBorderStyle(const Value: TPenStyle);

    procedure SetSelectedBrushColor(const Value: TColor);
    procedure SetSelectedBrushStyle(const Value: TBrushStyle);
    procedure SetSelectedBorderColor(const Value: TColor);
    procedure SetSelectedBorderWidth(const Value: Integer);
    procedure SetSelectedBorderStyle(const Value: TPenStyle);

    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure SetIndex(Value: Integer); override;
    procedure MoveZoneZ(NewIndex: Integer);
    procedure MoveZone(MoveAmount: Integer);
    procedure BringToFront;
    procedure SendToBack;
    procedure Select(const doUpdate: Boolean = True);
    procedure SetMouseOver(MouseOverState: Boolean = True; doUpdate: Boolean = True);
    procedure UnSelect(doUpdate: Boolean = True);
    procedure SetBorders(Key1, Key2: Byte; doUpdate: Boolean = True);
    function  KeyInZone(const KeyNr: Byte): Boolean;
  published
    property DisplayName;
    property LowestZoneKey: Byte read FLowestZoneKey write SetLowestZoneKey default 48;
    property HighestZoneKey: Byte read FHighestZoneKey write SetHighestZoneKey default 59;

    property Visible: Boolean read FVisible write SetVisible default True;

    property DefaultBrushColor: TColor read FDefaultBrushColor write SetDefaultBrushColor default clSkyBlue;
    property DefaultBrushStyle: TBrushStyle read FDefaultBrushStyle write SetDefaultBrushStyle default bsSolid;
    property DefaultBorderColor: TColor read FDefaultBorderColor write SetDefaultBorderColor default clBlack;
    property DefaultBorderWidth: Integer read FDefaultBorderWidth write SetDefaultBorderWidth default 1;
    property DefaultBorderStyle: TPenStyle read FDefaultBorderStyle write SetDefaultBorderStyle default psSolid;

    property HoverBrushColor: TColor read FHoverBrushColor write SetHoverBrushColor default $FFDAB6;
    property HoverBrushStyle: TBrushStyle read FHoverBrushStyle write SetHoverBrushStyle default bsSolid;
    property HoverBorderColor: TColor read FHoverBorderColor write SetHoverBorderColor default clBlack;
    property HoverBorderWidth: Integer read FHoverBorderWidth write SetHoverBorderWidth default 1;
    property HoverBorderStyle: TPenStyle read FHoverBorderStyle write SetHoverBorderStyle default psSolid;

    property SelectedBrushColor: TColor read FSelectedBrushColor write SetSelectedBrushColor default $000099;
    property SelectedBrushStyle: TBrushStyle read FSelectedBrushStyle write SetSelectedBrushStyle default bsSolid;
    property SelectedBorderColor: TColor read FSelectedBorderColor write SetSelectedBorderColor default clBlack;
    property SelectedBorderWidth: Integer read FSelectedBorderWidth write SetSelectedBorderWidth default 1;
    property SelectedBorderStyle: TPenStyle read FSelectedBorderStyle write SetSelectedBorderStyle default psSolid;

    property Tag: Integer read FTag write FTag;
    property Selected: Boolean read FSelected;
    property IsMouseOver: Boolean read FIsMouseOver;
  end;



  TGuiKeyZoneCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer):TGuiKeyZoneItem;
    procedure SetItem(Index: Integer; Value: TGuiKeyZoneItem);
  protected
    FAllowUpdate: Boolean;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure ClipZones;
    function Add: TGuiKeyZoneItem;
    function Selected: TGuiKeyZoneItem;
    function Insert(Index: Integer): TCollectionItem;
    procedure UpdateOwner;
    procedure UnselectAll(doUpdate: Boolean = True);
    function ZoneByKey(const KeyNr: Byte): TGuiKeyZoneItem;
    procedure DeleteSelected;
    property Items[Index: Integer]: TGuiKeyZoneItem read GetItem write SetItem; default;
  end;

implementation

uses
  DAV_GuiMidiKeys, Math;

  
constructor TGuiKeyZoneItem.Create(Collection: TCollection);
begin
 inherited Create(Collection);
 FDisplayName    := ClassName;
 FLowestZoneKey  := 48;
 FHighestZoneKey := 59;
 FVisible        := True;

 FDefaultBrushColor  := $F0CAA6;   FHoverBrushColor  := $FFDAB6;             FSelectedBrushColor  := $000099;
 FDefaultBrushStyle  := bsSolid;   FHoverBrushStyle  := FDefaultBrushStyle;  FSelectedBrushStyle  := FDefaultBrushStyle;
 FDefaultBorderWidth := 1;         FHoverBorderWidth := FDefaultBorderWidth; FSelectedBorderWidth := FDefaultBorderWidth;
 FDefaultBorderColor := clBlack;   FHoverBorderColor := FDefaultBorderColor; FSelectedBorderColor := FDefaultBorderColor;
 FDefaultBorderStyle := psSolid;   FHoverBorderStyle := FDefaultBorderStyle; FSelectedBorderStyle := FDefaultBorderStyle;
end;

procedure TGuiKeyZoneItem.SetIndex(Value: Integer);
begin
 inherited SetIndex(Value);
end;

function TGuiKeyZoneItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TGuiKeyZoneItem.SetDisplayName(const Value: string);
begin
 FDisplayName := Value;
end;





procedure TGuiKeyZoneItem.SetDefaultBrushColor(const Value: TColor);
begin
 if FDefaultBrushColor <> Value then
  begin
   FDefaultBrushColor := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBrushStyle(const Value: TBrushStyle);
begin
 if FDefaultBrushStyle <> Value then
  begin
   FDefaultBrushStyle := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBorderColor(const Value: TColor);
begin
 if FDefaultBorderColor <> Value then
  begin
   FDefaultBorderColor := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBorderWidth(const Value: Integer);
begin
 if FDefaultBorderWidth <> Value then
  begin
   FDefaultBorderWidth := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetDefaultBorderStyle(const Value: TPenStyle);
begin
 if FDefaultBorderStyle <> Value then
  begin
   FDefaultBorderStyle := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;




procedure TGuiKeyZoneItem.SetHoverBrushColor(const Value: TColor);
begin
 if FHoverBrushColor <> Value then
  begin
   FHoverBrushColor := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBrushStyle(const Value: TBrushStyle);
begin
 if FHoverBrushStyle <> Value then
  begin
   FHoverBrushStyle := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBorderColor(const Value: TColor);
begin
 if FHoverBorderColor <> Value then
  begin
   FHoverBorderColor := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBorderWidth(const Value: Integer);
begin
 if FHoverBorderWidth<>Value then
  begin
   FHoverBorderWidth := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHoverBorderStyle(const Value: TPenStyle);
begin
 if FHoverBorderStyle <> Value then
  begin
   FHoverBorderStyle := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;




procedure TGuiKeyZoneItem.SetSelectedBrushColor(const Value: TColor);
begin
 if FSelectedBrushColor <> Value then
  begin
   FSelectedBrushColor := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBrushStyle(const Value: TBrushStyle);
begin
 if FSelectedBrushStyle <> Value then
  begin
   FSelectedBrushStyle := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBorderColor(const Value: TColor);
begin
 if FSelectedBorderColor <> Value then
  begin
   FSelectedBorderColor := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBorderWidth(const Value: Integer);
begin
 if FSelectedBorderWidth <> Value then
  begin
   FSelectedBorderWidth := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetSelectedBorderStyle(const Value: TPenStyle);
begin
 if FSelectedBorderStyle <> Value then
  begin
   FSelectedBorderStyle := Value;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;




procedure TGuiKeyZoneItem.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetLowestZoneKey(const Value: Byte);
begin
 if FLowestZoneKey <> Value then
  begin
    FLowestZoneKey := Value;
    if FHighestZoneKey < FLowestZoneKey then FHighestZoneKey := FLowestZoneKey;
    if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetHighestZoneKey(const Value: Byte);
begin
 if FHighestZoneKey <> Value then
  begin
   FHighestZoneKey := Value;
   if FHighestZoneKey < FLowestZoneKey then FLowestZoneKey := FHighestZoneKey;
   if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetMouseOver(MouseOverState, doUpdate: Boolean);
var
  i: Integer;
begin
 if MouseOverState <> FIsMouseOver then
  begin
   if MouseOverState then
    with (Collection as TGuiKeyZoneCollection) do
     for i := Count - 1 downto 0 do Items[i].SetMouseOver(False, False);
   FIsMouseOver := MouseOverState;
   if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;


procedure TGuiKeyZoneItem.Select(const doUpdate: Boolean = True);
begin
 if not FSelected then
  begin
   (Collection as TGuiKeyZoneCollection).UnSelectAll(False);
   FSelected := True;
   if doUpdate
    then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.UnSelect(doUpdate: Boolean = True);
begin
 if FSelected then
  begin
   FSelected := False;
   if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
  end;
end;

procedure TGuiKeyZoneItem.SetBorders(Key1, Key2: Byte; doUpdate: Boolean);
var
  tmp: Byte;
begin
 if Key2 < Key1 then
  begin
   // flip
   tmp := Key1;
   Key1 := Key2;
   Key2 := tmp;
  end;
 FLowestZoneKey := Key1;
 FHighestZoneKey := Key2;
 if doUpdate then (Collection as TGuiKeyZoneCollection).UpdateOwner;
end;


procedure TGuiKeyZoneItem.MoveZoneZ(NewIndex: Integer);
begin
  SetIndex(NewIndex);
  if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
end;

procedure TGuiKeyZoneItem.BringToFront;
begin
  MoveZoneZ(Collection.Count-1);
end;

procedure TGuiKeyZoneItem.SendToBack;
begin
  MoveZoneZ(0);
end;

procedure TGuiKeyZoneItem.MoveZone(MoveAmount: Integer);
begin
 if MoveAmount = 0 then Exit;

 MoveAmount := Max(-FLowestZoneKey, MoveAmount);
 MoveAmount := Min(CKeyboardHighestKey - FHighestZoneKey, MoveAmount);

 FLowestZoneKey  := FLowestZoneKey + MoveAmount;
 FHighestZoneKey := FHighestZoneKey + MoveAmount;

 if Visible then (Collection as TGuiKeyZoneCollection).UpdateOwner;
end;

function TGuiKeyZoneItem.KeyInZone(const KeyNr: Byte): Boolean;
begin
  Result := (KeyNr >= FLowestZoneKey) and (KeyNr <= FHighestZoneKey);
end;






constructor TGuiKeyZoneCollection.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner, TGuiKeyZoneItem);
 FAllowUpdate := True;
end;

procedure TGuiKeyZoneCollection.UpdateOwner;
begin
 if FAllowUpdate then (Owner as TGuiMidiKeys).Invalidate;
end;

procedure TGuiKeyZoneCollection.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
 UpdateOwner;
end;

procedure TGuiKeyZoneCollection.Update(Item: TCollectionItem);
begin
 UpdateOwner;
end;

function TGuiKeyZoneCollection.GetItem(Index: Integer): TGuiKeyZoneItem;
begin
 Result := TGuiKeyZoneItem(inherited GetItem(Index));
end;

procedure TGuiKeyZoneCollection.SetItem(Index: Integer; Value: TGuiKeyZoneItem);
begin
 inherited SetItem(Index, Value);
end;

function TGuiKeyZoneCollection.Add: TGuiKeyZoneItem;
begin
 Result := TGuiKeyZoneItem(inherited Add);
end;

function TGuiKeyZoneCollection.Insert(Index: Integer): TCollectionItem;
begin
 Result := TGuiKeyZoneItem(inherited Insert(Index));
end;

function TGuiKeyZoneCollection.ZoneByKey(const KeyNr: Byte): TGuiKeyZoneItem;
var
  i: Integer;
begin
 Result := nil;
 if Count = 0 then Exit;

 for i := Count - 1 downto 0 do
  if items[i].KeyInZone(KeyNr) then
   begin
    Result := items[i];
    Exit;
   end;
end;

procedure TGuiKeyZoneCollection.ClipZones;

  procedure ClipZoneX(clipIndex, OverlayIndex: Integer);
  var
    mink, maxk: Byte;
  begin
   if not items[OverlayIndex].Visible then Exit;

   mink := items[OverlayIndex].LowestZoneKey;
   maxk := items[OverlayIndex].HighestZoneKey;
   with items[clipIndex] do
    begin
     // check for no overlay
     if (HighestZoneKey < mink) and (LowestZoneKey  < mink) then Exit;
     if (LowestZoneKey  > maxk) and (HighestZoneKey > maxk) then Exit;

     // check for complete overlay (underlaying layer is invisible)
     if  (LowestZoneKey>=mink) and (HighestZoneKey<=maxk) then
      begin
       Visible := False;
       LowestZoneKey  := 0;
       HighestZoneKey := 0;
       Exit;
      end;

     // check for partial overlay
     if (HighestZoneKey >= mink) and (HighestZoneKey <= maxk) then
      begin
       if LowestZoneKey > maxk then HighestZoneKey := maxk + 1 else HighestZoneKey := mink - 1;
      end else
     if (LowestZoneKey >= mink) and (LowestZoneKey <= maxk) then
      begin
       if HighestZoneKey > maxk
        then LowestZoneKey := maxk + 1
        else LowestZoneKey := mink - 1;
      end
     else
      begin
       // complete overlay (underlaying Layer is visible)
       HighestZoneKey := mink - 1;
      end;
    end;
  end;

var
  i, j : Integer;
begin
 if Count < 2 then Exit;

 FAllowUpdate := False;

 for j := count - 1 downto 1 do
  for i := j - 1 downto 0
   do ClipZoneX(i, j);

 FAllowUpdate := True;
 UpdateOwner;
end;

procedure TGuiKeyZoneCollection.UnSelectAll(doUpdate: Boolean = True);
var
  i : Integer;
begin
 if Count < 1 then Exit;

 for i := Count - 1 downto 0
  do Items[i].UnSelect(False);

 if doUpdate then UpdateOwner;
end;

function TGuiKeyZoneCollection.Selected: TGuiKeyZoneItem;
var
  i : Integer;
begin
 Result := nil;
 if Count < 1 then Exit;

 for i := Count-1 downto 0 do
  if Items[i].Selected
   then Result := Items[i];
end;

procedure TGuiKeyZoneCollection.DeleteSelected;
begin
 Delete(Selected.Index);
end;

end.
