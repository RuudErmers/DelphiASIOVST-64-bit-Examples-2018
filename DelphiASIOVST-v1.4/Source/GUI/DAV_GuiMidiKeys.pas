unit DAV_GuiMidiKeys;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages, {$ELSE} Windows, {$ENDIF}
  Classes, Graphics, Forms, Controls, ExtCtrls, Messages, DAV_GuiBaseControl,
  DAV_GuiMidiKeyZones;

const
  CKeyboardMaxOctaves = 11;
  CKeyboardHighestKey = CKeyboardMaxOctaves * 12 + 1;  // Octaves + Highest C
  
type
  TGuiZoneMousePosType = set of (mptOutside, mptInZone, mptOnLowestKey, mptOnHighestKey, mptOnLowestBorder, mptOnHighestBorder);
  TGuiKbMouseAction = (kmaMove, kmaDown, kmaUp, kmaStartDrag);
  
  TGuiKbMouseDragInfo = record
    Button     : TMouseButton;
    isDragging : Boolean;       
    StartKey   : Integer; // -1 = Out of visible range
    LastKey    : Integer; // -1 = Out of visible range
  end;

  TGuiZbMouseDragInfo = record
    Button              : TMouseButton;
    isDragging          : Boolean;
    Zone                : TGuiKeyZoneItem;
    InZonePos           : TGuiZoneMousePosType;
    StartKey            : Integer; // -1 = Out of visible range
    LastKey             : Integer; // -1 = Out of visible range
    StartLowestZoneKey  : Integer;
    StartHighestZoneKey : Integer;
  end;

  TGuiKeyFlag = (kfBlackKey, kfVisible, kfPressed, kfMouseOver, kfByMouse,
    kfMousePinned);
  TGuiKeyFlags = set of TGuiKeyFlag;
  TGuiKeyDownMode = (kdmUp, kdmFlat, kdmDown);
  TGuiColorRect = record
    Top, Left, Right, Bottom: TColor;
  end;

  TGuiSingleKey = record
    KeyNr        : Byte;
    Flags        : TGuiKeyFlags;
    Area         : TRect;
    BaseColor    : TColor;
    PressedColor : TColor;
    OverColor    : TColor;
    Velocity     : Single;
  end;

  TGuiKeyArray = array[0..CKeyboardHighestKey] of TGuiSingleKey;

  TGuiOnMouseUpOnMidiKey      = procedure (Sender: TObject; KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of object;
  TGuiOnMouseDownOnMidiKey    = procedure (Sender: TObject; KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of object;

  TGuiOnMouseUpOnKeyZoneBar   = procedure (Sender: TObject; KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of object;
  TGuiOnMouseDownOnKeyZoneBar = procedure (Sender: TObject; KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer) of object;

  TGuiOnZoneBarMouseEnter     = procedure (Sender: TObject; KeyNr: Byte; Shift: TShiftState; X, Y: Integer) of object;
  TGuiOnZoneBarMouseLeave     = TNotifyEvent;

  TGuiOnZoneMouseEnter        = procedure (Sender: TObject; Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer) of object;
  TGuiOnZoneMouseLeave        = procedure (Sender: TObject; Zone: TGuiKeyZoneItem) of object;
  TGuiOnZoneMouseOverChanged  = procedure (Sender: TObject; KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer) of object;
  TGuiOnZoneSelectionChanged  = procedure (Sender: TObject; Zone: TGuiKeyZoneItem) of object;

  TGuiOnKeyMouseEnter         = procedure (Sender: TObject; KeyNr: Byte; Shift: TShiftState; X, Y: Integer) of object;
  TGuiOnKeyMouseLeave         = procedure (Sender: TObject; KeyNr: Byte) of object;

  TGuiOnStartZoneBarDragging  = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of object;
  TGuiOnMoveZoneBarDragging   = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of object;
  TGuiOnEndZoneBarDragging    = procedure (Sender: TObject; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of object;

  TGuiOnStartKeyDragging      = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiKbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of object;
  TGuiOnMoveKeyDragging       = procedure (Sender: TObject; KeyNr: Integer; var DragInfo: TGuiKbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of object;
  TGuiOnEndKeyDragging        = procedure (Sender: TObject; var DragInfo: TGuiKbMouseDragInfo; Shift: TShiftState; X,Y: Integer) of object;

  TGuiOnNoteOn                = procedure (Sender: TObject; KeyNr: Byte; Velocity: Single) of object;
  TGuiOnNoteOff               = procedure (Sender: TObject; KeyNr: Byte) of object;

  TGuiOnGetKeyColorEvent      = procedure (Sender: TObject; KeyNr: Byte; var KeyColor: TColor; IsBlackKey: Boolean) of object;

  TCustomGuiMidiKeys = class(TCustomGuiBaseAntialiasedControl)
  private
    procedure SetBorderColor(Value: TColor);
    procedure SetKeySeparatorColor(Value: TColor);
    procedure SetZoneSeparatorColor(Value: TColor);
    procedure SetZoneBarColor(Value: TColor);
    procedure SetZoneBarHoverColor(Value: TColor);
    procedure SetBlackKeyColor(Value: TColor);
    procedure SetBlackKeyHoverColor(Value: TColor);
    procedure SetBlackKeyPressedColor(Value: TColor);
    procedure SetWhiteKeyColor(Value: TColor);
    procedure SetWhiteKeyHoverColor(Value: TColor);
    procedure SetWhiteKeyPressedColor(Value: TColor);

    procedure SetKeyDownMode(Value: TGuiKeyDownMode);
    procedure SetHeight3d(Value: Single);
    procedure SetBlackKeyHeight(Value: Single);
    procedure SetBaseOctave(Value: Byte);
    procedure SetNumOctaves(Value: Byte);
    procedure SetIncludeLastOctave(Value: Boolean);
    procedure SetKeyZones(Value: TGuiKeyZoneCollection);
    procedure SetShowKeyZones(Value: Boolean);
    procedure SetKeyZoneHeight(Value: Integer);

    function ZoneKeyArea(KeyNr: Byte): TRect;
  protected
    FRedrawEnabled           : Boolean;
    FKeys                    : TGuiKeyArray;
    FBaseOctave              : Byte;
    FNumOctaves              : Byte;
    FIncludeLastOctave       : Boolean;
    FShowKeyZones            : Boolean;
    FAllowKeyDragging        : Boolean;

    FKeyZoneHeight           : Integer;
    FKeyZones                : TGuiKeyZoneCollection;
    FBlackKeyHeight          : Single;
    FHeight3D                : Single;
    FBorderColor             : TColor;
    FKeySeparatorColor       : TColor;
    FZoneSeparatorColor      : TColor;
    FZoneBarColor            : TColor;
    FZoneBarHoverColor       : TColor;
    FBlackKeyColor           : TColor;
    FBlackKeyHoverColor      : TColor;
    FBlackKeyPressedColor    : TColor;
    FWhiteKeyColor           : TColor;
    FWhiteKeyHoverColor      : TColor;
    FWhiteKeyPressedColor    : TColor;
    FKeyDownMode             : TGuiKeyDownMode;

    FCursorKeys              : TCursor;
    FCursorZoneBar           : TCursor;
    FCursorZone              : TCursor;
    FCursorZoneBorder        : TCursor;

    FZoneMouseOverType       : TGuiZoneMousePosType;

    FOnGetKeyColor           : TGuiOnGetKeyColorEvent;

    FOnMouseUpOnMidiKey      : TGuiOnMouseUpOnMidiKey;
    FOnMouseUpOnKeyZoneBar   : TGuiOnMouseUpOnKeyZoneBar;
    FOnMouseDownOnMidiKey    : TGuiOnMouseDownOnMidiKey;
    FOnMouseDownOnKeyZoneBar : TGuiOnMouseDownOnKeyZoneBar;

    FOnZoneBarMouseEnter     : TGuiOnZoneBarMouseEnter;
    FOnZoneBarMouseLeave     : TGuiOnZoneBarMouseLeave;
    FOnZoneMouseEnter        : TGuiOnZoneMouseEnter;
    FOnZoneMouseLeave        : TGuiOnZoneMouseLeave;
    FOnZoneMouseOverChanged  : TGuiOnZoneMouseOverChanged;
    FOnZoneSelectionChanged  : TGuiOnZoneSelectionChanged;

    FOnKeyMouseEnter         : TGuiOnKeyMouseEnter;
    FOnKeyMouseLeave         : TGuiOnKeyMouseLeave;

    FOnStartZoneBarDragging  : TGuiOnStartZoneBarDragging;
    FOnMoveZoneBarDragging   : TGuiOnMoveZoneBarDragging;
    FOnEndZoneBarDragging    : TGuiOnEndZoneBarDragging;

    FOnStartKeyDragging      : TGuiOnStartKeyDragging;
    FOnMoveKeyDragging       : TGuiOnMoveKeyDragging;
    FOnEndKeyDragging        : TGuiOnEndKeyDragging;

    FOnNoteOn                : TGuiOnNoteOn;
    FOnNoteOff               : TGuiOnNoteOff;

    FMidiKeyDragging         : TGuiKbMouseDragInfo;
    FMidiZoneBarDragging     : TGuiZbMouseDragInfo;
    FMidiZoneBarMouseOver    : Boolean;

    procedure AntialiasChanged; override;
    function CalculateLights(Color : TColor; DoubledHeight: boolean = False; KeyPressed: boolean = False): TGuiColorRect; virtual;
    procedure CalculateVisibleKeys; virtual;
    procedure InitKeys; virtual;

    procedure DrawKeyZones(Bitmap: TBitmap); virtual;
    procedure DrawSingleKey(Bitmap: TBitmap; CurKey: TGuiSingleKey); virtual;
    procedure DrawKeys(Bitmap: TBitmap); virtual;
    procedure RenderToBitmap(Bitmap: TBitmap); virtual;

    procedure MouseLeaveAllKeys(ExceptKey: Integer = -1); virtual;
    procedure MouseLeaveAllZones(ExceptZone: TGuiKeyZoneItem = nil); virtual;
    procedure UnSelectAllZones(ExceptZone: TGuiKeyZoneItem = nil); virtual;

    procedure ResizeBuffer; override;

    procedure BaseOctaveChanged; virtual;
    procedure BlackKeyColorChanged; virtual;
    procedure BlackKeyHeightChanged; virtual;
    procedure BlackKeyHoverColorChanged; virtual;
    procedure BlackKeyPressedColorChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure Height3DChanged; virtual;
    procedure IncludeLastOctaveChanged; virtual;
    procedure KeyDownModeChanged; virtual;
    procedure KeySeparatorColorChanged; virtual;
    procedure KeyZoneHeightChanged; virtual;
    procedure NumOctavesChanged; virtual;
    procedure ShowKeyZonesChanged; virtual;
    procedure WhiteKeyColorChanged; virtual;
    procedure WhiteKeyHoverColorChanged; virtual;
    procedure WhiteKeyPressedColorChanged; virtual;
    procedure ZoneBarColorChanged; virtual;
    procedure ZoneBarHoverColorChanged; virtual;
    procedure ZoneSeparatorColorChanged; virtual;

    procedure FireNoteOn(KeyNr: Byte; Flags: TGuiKeyFlags); dynamic;
    procedure FireNoteOff(KeyNr: Byte); dynamic;

    procedure MouseLeave; override;
    procedure KeyMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure KeyMouseLeave(KeyNr: Byte); dynamic;

    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure StartZoneBarDragging(KeyNr: Integer; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MoveZoneBarDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure EndZoneBarDragging(Shift: TShiftState; X,Y: Integer); dynamic;

    procedure StartKeyDragging(KeyNr: Integer; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MoveKeyDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure EndKeyDragging(Shift: TShiftState; X,Y: Integer); dynamic;

    procedure ZoneMouseActivity(Action: TGuiKbMouseAction; KeyNr: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneMouseEnter(Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneMouseLeave(Zone: TGuiKeyZoneItem); dynamic;
    procedure ZoneMouseOverChanged(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneBarMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure ZoneBarMouseLeave; dynamic;
    procedure ZoneSelectionChanged(Zone: TGuiKeyZoneItem = nil); dynamic;

    procedure MouseUpOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MouseDownOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;

    procedure MouseUpOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
    procedure MouseDownOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetKeyColor(FromKey, ToKey: Byte; BaseColor: TColor = clNone; Over: TColor = clNone; Pressed: TColor = clNone);
    procedure SetKeyVelocity(FromKey, ToKey: Byte; Amount: Single);
    procedure SetKeyPressed(KeyNr: Byte; KeyDown: Boolean = True);
    procedure ReleaseKey(KeyNr: Byte; ReleaseMouseTriggered: boolean = False);
    procedure ReleaseAllKeys(ReleaseMouseTriggered: boolean = False);
    procedure RemoveKeyColor(FromKey, ToKey: Byte);
    procedure AllNotesOff(MouseTriggeredOnly: Boolean = True; ExceptKey: Integer = -1);
    procedure UpdateBuffer; override;
    function MousePosToKey(X, Y: Integer; CheckYPos: Boolean = True): Integer; dynamic;
    function GetZoneMouseOverType(X: Integer; KeyNr: Integer = -1; Zone: TGuiKeyZoneItem = nil): TGuiZoneMousePosType;
    function ScreenCoordOnKey(X, Y: Integer; KeyNr: Integer; CheckYPos: Boolean = True): Boolean; dynamic;
    property Keys: TGuiKeyArray read FKeys;

    property KeyDownMode: TGuiKeyDownMode read FKeyDownMode write SetKeyDownMode;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property KeySeparatorColor: TColor read FKeySeparatorColor write SetKeySeparatorColor default clBlack;
    property ZoneSeparatorColor: TColor read FZoneSeparatorColor write SetZoneSeparatorColor default clBlack;
    property ZoneBarColor: TColor read FZoneBarColor write SetZoneBarColor default clWhite;
    property ZoneBarHoverColor: TColor read FZoneBarHoverColor write SetZoneBarHoverColor default $00CCEEFF;
    property BlackKeyColor: TColor read FBlackKeyColor write SetBlackKeyColor default $00333333;
    property BlackKeyHoverColor: TColor read FBlackKeyHoverColor write SetBlackKeyHoverColor default $00666666;
    property BlackKeyPressedColor: TColor read FBlackKeyPressedColor write SetBlackKeyPressedColor default clSilver;
    property WhiteKeyColor: TColor read FWhiteKeyColor write SetWhiteKeyColor default $00F8F8F8;
    property WhiteKeyHoverColor: TColor read FWhiteKeyHoverColor write SetWhiteKeyHoverColor default $00DDDDDD;
    property WhiteKeyPressedColor: TColor read FWhiteKeyPressedColor write SetWhiteKeyPressedColor default clSilver;

    property CursorKeys: TCursor read FCursorKeys write FCursorKeys default crHandPoint;
    property CursorZoneBar: TCursor read FCursorZoneBar write FCursorZoneBar default crDefault;
    property CursorZone: TCursor read FCursorZone write FCursorZone default crSize;
    property CursorZoneBorder: TCursor read FCursorZoneBorder write FCursorZoneBorder default crSizeWE;

    property Height3d: Single read FHeight3D write SetHeight3d;
    property BlackKeyHeight: Single read FBlackKeyHeight write SetBlackKeyHeight;
    property BaseOctave: Byte read FBaseOctave write SetBaseOctave default 3;
    property NumOctaves: Byte read FNumOctaves write SetNumOctaves default 3;
    property IncludeLastOctave: Boolean read FIncludeLastOctave write SetIncludeLastOctave default False;
    property KeyZones: TGuiKeyZoneCollection read FKeyZones write SetKeyZones;

    property ShowKeyZones: Boolean read FShowKeyZones write SetShowKeyZones default True;
    property KeyZoneHeight: Integer read FKeyZoneHeight write SetKeyZoneHeight default 10;
    property AllowKeyDragging: Boolean read FAllowKeyDragging write FAllowKeyDragging default True;

    property OnGetKeyColor: TGuiOnGetKeyColorEvent read FOnGetKeyColor write FOnGetKeyColor;

    property OnMouseUpOnMidiKey: TGuiOnMouseUpOnMidiKey read FOnMouseUpOnMidiKey write FOnMouseUpOnMidiKey;
    property OnMouseUpOnKeyZoneBar: TGuiOnMouseUpOnKeyZoneBar read FOnMouseUpOnKeyZoneBar write FOnMouseUpOnKeyZoneBar;
    property OnMouseDownOnMidiKey: TGuiOnMouseDownOnMidiKey read FOnMouseDownOnMidiKey write FOnMouseDownOnMidiKey;
    property OnMouseDownKeyZoneBar: TGuiOnMouseDownOnKeyZoneBar read FOnMouseDownOnKeyZoneBar write FOnMouseDownOnKeyZoneBar;

    property OnZoneBarMouseEnter: TGuiOnZoneBarMouseEnter read FOnZoneBarMouseEnter write FOnZoneBarMouseEnter;
    property OnZoneBarMouseLeave: TGuiOnZoneBarMouseLeave read FOnZoneBarMouseLeave write FOnZoneBarMouseLeave;
    property OnZoneMouseEnter: TGuiOnZoneMouseEnter read FOnZoneMouseEnter write FOnZoneMouseEnter;
    property OnZoneMouseLeave: TGuiOnZoneMouseLeave read FOnZoneMouseLeave write FOnZoneMouseLeave;
    property OnZoneMouseOverChanged: TGuiOnZoneMouseOverChanged read FOnZoneMouseOverChanged write FOnZoneMouseOverChanged;
    property OnZoneSelectionChanged: TGuiOnZoneSelectionChanged read FOnZoneSelectionChanged write FOnZoneSelectionChanged;

    property OnKeyMouseEnter: TGuiOnKeyMouseEnter read FOnKeyMouseEnter write FOnKeyMouseEnter;
    property OnKeyMouseLeave: TGuiOnKeyMouseLeave read FOnKeyMouseLeave write FOnKeyMouseLeave;

    property OnStartZoneBarDragging: TGuiOnStartZoneBarDragging read FOnStartZoneBarDragging write FOnStartZoneBarDragging;
    property OnMoveZoneBarDragging: TGuiOnMoveZoneBarDragging read FOnMoveZoneBarDragging write FOnMoveZoneBarDragging;
    property OnEndZoneBarDragging: TGuiOnEndZoneBarDragging read FOnEndZoneBarDragging write FOnEndZoneBarDragging;

    property OnStartKeyDragging: TGuiOnStartKeyDragging read FOnStartKeyDragging write FOnStartKeyDragging;
    property OnMoveKeyDragging: TGuiOnMoveKeyDragging read FOnMoveKeyDragging write FOnMoveKeyDragging;
    property OnEndKeyDragging: TGuiOnEndKeyDragging read FOnEndKeyDragging write FOnEndKeyDragging;

    property OnNoteOn: TGuiOnNoteOn read FOnNoteOn write FOnNoteOn;
    property OnNoteOff: TGuiOnNoteOff read FOnNoteOff write FOnNoteOff;
  end;

  TGuiMidiKeys = class(TCustomGuiMidiKeys)
  published
    property Align;
    property AllowKeyDragging;
    property Anchors;
    property AntiAlias;
    property BaseOctave;
    property BlackKeyColor;
    property BlackKeyHeight;
    property BlackKeyHoverColor;
    property BlackKeyPressedColor;
    property BorderColor;
    property Constraints;
    property CursorKeys;
    property CursorZone;
    property CursorZoneBar;
    property CursorZoneBorder;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Height3d;
    property IncludeLastOctave;
    property KeyDownMode;
    property KeySeparatorColor;
    property KeyZoneHeight;
    property KeyZones;
    property NumOctaves;
    property PopupMenu;
    property ReleaseMouseBtnOnLeave;
    property ShowHint;
    property ShowKeyZones;
    property Visible;
    property WhiteKeyColor;
    property WhiteKeyHoverColor;
    property WhiteKeyPressedColor;
    property ZoneBarColor;
    property ZoneBarHoverColor;
    property ZoneSeparatorColor;

    property OnGetKeyColor;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragMouseMove;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEndKeyDragging;
    property OnEndZoneBarDragging;
    property OnKeyMouseEnter;
    property OnKeyMouseLeave;
    property OnMouseDown;
    property OnMouseDownKeyZoneBar;
    property OnMouseDownOnMidiKey;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseUpOnKeyZoneBar;
    property OnMouseUpOnMidiKey;
    property OnMoveKeyDragging;
    property OnMoveZoneBarDragging;
    property OnNoteOff;
    property OnNoteOn;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnStartKeyDragging;
    property OnStartZoneBarDragging;
    property OnZoneBarMouseEnter;
    property OnZoneBarMouseLeave;
    property OnZoneMouseEnter;
    property OnZoneMouseLeave;
    property OnZoneMouseOverChanged;
    property OnZoneSelectionChanged;
    {$IFDEF Delphi6_Up}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
  end;

implementation

uses
  SysUtils, Math;

constructor TCustomGuiMidiKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyZones := TGuiKeyZoneCollection.Create(Self);

  FShowKeyZones         := True;
  FKeyZoneHeight        := 10;

  FAllowKeyDragging     := True;

  FBorderColor          := clBlack;
  ZoneBarColor          := clWhite;
  ZoneBarHoverColor     := $00CCEEFF;
  FKeySeparatorColor    := clBlack;
  FBlackKeyColor        := $00333333;
  FBlackKeyHoverColor   := $00666666;
  FBlackKeyPressedColor := clSilver;
  FWhiteKeyColor        := $00F8F8F8;
  FWhiteKeyHoverColor   := $00DDDDDD;
  FWhiteKeyPressedColor := clSilver;
  FKeyDownMode          := kdmFlat;
  FZoneMouseOverType    := [];
  FRedrawEnabled        := True;

  FCursorKeys           := crHandPoint;
  FCursorZoneBar        := crDefault;
  FCursorZone           := crSize;
  FCursorZoneBorder     := crSizeWE;

  FMidiKeyDragging.isDragging     := False;
  FMidiKeyDragging.LastKey        := -1;
  FMidiKeyDragging.StartKey       := -1;
  FMidiZoneBarDragging.isDragging := False;
  FMidiZoneBarDragging.LastKey    := -1;
  FMidiZoneBarDragging.StartKey   := -1;
  FMidiZoneBarDragging.Zone       := nil;
  FMidiZoneBarDragging.InZonePos  := [mptOutside];
  
  FMidiZoneBarMouseOver := False;

  FBaseOctave        := 3;
  FNumOctaves        := 3;
  FIncludeLastOctave := False;
  FHeight3D          := 0.2;
  FBlackKeyHeight    := 0.63;

  InitKeys;
  RedrawInterval := 50;
end;

destructor TCustomGuiMidiKeys.Destroy;
begin
 FreeAndNil(FKeyZones);
 inherited;
end;

procedure TCustomGuiMidiKeys.InitKeys;
var
  i: Byte;
begin
 for i := 0 to CKeyboardHighestKey do with FKeys[i] do
  begin
   KeyNr          := i;
   Flags          := [];
   Area           := Rect(-1, -1, -1, -1);
   PressedColor   := clNone;
   BaseColor      := clNone;
   OverColor      := clNone;
   Velocity       := 1;

   if (i mod 12) in [1, 3, 6, 8, 10]
    then Include(Flags, kfBlackKey);
  end;
end;

procedure TCustomGuiMidiKeys.ResizeBuffer;
begin
  CalculateVisibleKeys;
  inherited;
end;

procedure TCustomGuiMidiKeys.CalculateVisibleKeys;
var
  i, cnt, KeyCount        : Integer;
  KeyWidth                : Single;
  LastRightBorder         : Integer;
  FirstKey, LastKey       : Integer;
  TopMargin, BottomMargin : Integer;
begin
 FirstKey := FBaseOctave * 12;
 LastKey  := (FBaseOctave + FNumOctaves) * 12 + Integer(FIncludeLastOctave) - 1;

 for i := 0 to CKeyboardHighestKey do with FKeys[i] do
  begin
   if i > LastKey
    then Area := Rect(OversamplingFactor * Width - 1, 0, OversamplingFactor * Width - 1, 0)
    else Area := Rect(0, 0, 0, 0);
   Exclude(Flags, kfVisible);
  end;

  if FShowKeyZones
   then TopMargin := OversamplingFactor * (FKeyZoneHeight + 2)
   else TopMargin := 1;
  BottomMargin := OversamplingFactor * Height - 1;
  

  // set white keys  
  // -------------------------------------------
  if FIncludeLastOctave
   then KeyCount := 1
   else KeyCount := 0;
  KeyWidth := OversamplingFactor * (Width - 1) / (KeyCount + FNumOctaves * 7);

  cnt := 0;
  LastRightBorder := -1;
  for i := FirstKey to LastKey do
   begin
    if (i mod 12) in [0, 2, 4, 5, 7, 9, 11] then
     begin
      FKeys[i].Area := Rect(LastRightBorder + 2, TopMargin, Round((cnt + 1) * KeyWidth) - 1, BottomMargin);
      LastRightBorder := FKeys[i].Area.Right;
      Include(FKeys[i].Flags, kfVisible);
      Inc(cnt);
     end;
   end;

  // set black keys
  // -------------------------------------------
  if not FIncludeLastOctave then KeyWidth := 0;
  KeyWidth := (OversamplingFactor * Width - 2 - KeyWidth) / (FNumOctaves * 12);

  BottomMargin := Round((OversamplingFactor * Height - TopMargin) * FBlackKeyHeight + TopMargin);

  Cnt := 0;
  for i := FirstKey to LastKey do
   begin
    if (i mod 12) in [1,3,6,8,10] then
     begin
      FKeys[i].Area := Rect(
        Round(cnt       * KeyWidth + 2), TopMargin,
        Round((cnt + 1) * KeyWidth + 2), BottomMargin);
      Include(FKeys[i].Flags, kfVisible);
     end;
    Inc(cnt);
  end;
end;

function TCustomGuiMidiKeys.CalculateLights(Color : TColor; DoubledHeight, KeyPressed: boolean): TGuiColorRect;
var
  r1, g1, b1 : Integer;
  r2, g2, b2 : Integer;
  TmpHeight  : Single;
begin
 if DoubledHeight then
   TmpHeight := FHeight3D * 2
 else
   TmpHeight := FHeight3D;
 if KeyPressed then TmpHeight := -TmpHeight;

 r1 := (Color shr 16) and $FF;
 g1 := (Color shr 8)  and $FF;
 b1 :=  Color         and $FF;

 r2 := Round(r1 + TmpHeight * $FF);
 g2 := Round(g1 + TmpHeight * $FF);
 b2 := Round(b1 + TmpHeight * $FF);

 Result.Left := Max(Min(r2, $FF), 0) shl 16
   + Max(Min(g2, $FF), 0) shl 8
   + Max(Min(b2, $FF), 0);

 r2 := Round(r1 - TmpHeight * $FF);
 g2 := Round(g1 - TmpHeight * $FF);
 b2 := Round(b1 - TmpHeight * $FF);

 Result.Right := Max(Min(r2, $FF), 0) shl 16
   + Max(Min(g2, $FF), 0) shl 8
   + Max(Min(b2, $FF), 0);

 Result.Bottom := Result.Right;
 Result.Top    := Result.Left;
end;

procedure TCustomGuiMidiKeys.SetKeyDownMode(Value: TGuiKeyDownMode);
begin
 if FKeyDownMode <> Value then
  begin
   FKeyDownMode := Value;
   KeyDownModeChanged;
  end;
end;

procedure TCustomGuiMidiKeys.KeyDownModeChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetBorderColor(Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.BorderColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetKeySeparatorColor(Value: TColor);
begin
 if FKeySeparatorColor <> Value then
  begin
   FKeySeparatorColor := Value;
   KeySeparatorColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.KeySeparatorColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetZoneSeparatorColor(Value: TColor);
begin
  if FZoneSeparatorColor <> Value then
  begin
    FZoneSeparatorColor := Value;
    ZoneSeparatorColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.ZoneSeparatorColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetZoneBarColor(Value: TColor);
begin
  if FZoneBarColor <> Value then
  begin
    FZoneBarColor := Value;
    ZoneBarColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.ZoneBarColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetZoneBarHoverColor(Value: TColor);
begin
 if FZoneBarHoverColor <> Value then
  begin
   FZoneBarHoverColor := Value;
   ZoneBarHoverColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.ZoneBarHoverColorChanged;
begin
 if FMidiZoneBarMouseOver or (FMidiZoneBarDragging.isDragging)
  then Invalidate;
end;

procedure TCustomGuiMidiKeys.SetBlackKeyColor(Value: TColor);
begin
 if FBlackKeyColor <> Value then
  begin
   FBlackKeyColor := Value;
   BlackKeyColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.BlackKeyColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetBlackKeyHoverColor(Value: TColor);
begin
 if FBlackKeyHoverColor <> Value then
  begin
   FBlackKeyHoverColor := Value;
   BlackKeyHoverColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.BlackKeyHoverColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetBlackKeyPressedColor(Value: TColor);
begin
 if FBlackKeyPressedColor <> Value then
  begin
   FBlackKeyPressedColor := Value;
   BlackKeyPressedColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.BlackKeyPressedColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetWhiteKeyColor(Value: TColor);
begin
 if FWhiteKeyColor <> Value then
  begin
   FWhiteKeyColor := Value;
   WhiteKeyColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.WhiteKeyColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetWhiteKeyHoverColor(Value: TColor);
begin
 if FWhiteKeyHoverColor <> Value then
  begin
   FWhiteKeyHoverColor := Value;
   WhiteKeyHoverColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.WhiteKeyHoverColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetWhiteKeyPressedColor(Value: TColor);
begin
  if FWhiteKeyPressedColor <> Value then
  begin
    FWhiteKeyPressedColor := Value;
    WhiteKeyPressedColorChanged;
  end;
end;

procedure TCustomGuiMidiKeys.WhiteKeyPressedColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetShowKeyZones(Value: Boolean);
begin
 if FShowKeyZones <> Value then
  begin
   FShowKeyZones := Value;
   ShowKeyZonesChanged;
  end;
end;

procedure TCustomGuiMidiKeys.ShowKeyZonesChanged;
begin
 if ShowKeyZones and (FKeyZoneHeight < 1) then KeyZoneHeight := 10 else
  begin
   CalculateVisibleKeys;
   Invalidate;
  end;
end;

procedure TCustomGuiMidiKeys.SetKeyZoneHeight(Value: Integer);
begin
  if FKeyZoneHeight <> Value then
   begin
    FKeyZoneHeight := Value;
    KeyZoneHeightChanged;
   end;
end;

procedure TCustomGuiMidiKeys.KeyZoneHeightChanged;
begin
 if not (csLoading in  ComponentState) and not ShowKeyZones and (FKeyZoneHeight > 0)
  then FShowKeyZones := True;
 CalculateVisibleKeys;
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetHeight3D(Value: Single);
begin
 if FHeight3D <> Value then
  begin
   FHeight3D := Value;
   Height3DChanged;
  end;
end;

procedure TCustomGuiMidiKeys.Height3DChanged;
begin
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetBlackKeyHeight(Value: Single);
begin
 if FBlackKeyHeight <> Value then
  begin
   FBlackKeyHeight := Value;
   BlackKeyHeightChanged;
  end;
end;

procedure TCustomGuiMidiKeys.BlackKeyHeightChanged;
begin
 CalculateVisibleKeys;
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetBaseOctave(Value: Byte);
begin
 if (Value + FNumOctaves < CKeyboardMaxOctaves) and (Value <> FBaseOctave) then
  begin
   FBaseOctave := Value;
   BaseOctaveChanged;
  end;
end;

procedure TCustomGuiMidiKeys.AntialiasChanged;
begin
 inherited;
 CalculateVisibleKeys;
end;

procedure TCustomGuiMidiKeys.BaseOctaveChanged;
begin
 CalculateVisibleKeys;
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetNumOctaves(Value: Byte);
begin
 if (Value + FBaseOctave < CKeyboardMaxOctaves) and (Value <> FNumOctaves) then
  begin
   FNumOctaves := Value;
   NumOctavesChanged;
  end;
end;

procedure TCustomGuiMidiKeys.NumOctavesChanged;
begin
 CalculateVisibleKeys;
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetIncludeLastOctave(Value: Boolean);
begin
 if FIncludeLastOctave <> Value then
  begin
   FIncludeLastOctave := Value;
   IncludeLastOctaveChanged;
  end;
end;

procedure TCustomGuiMidiKeys.IncludeLastOctaveChanged;
begin
 CalculateVisibleKeys;
 Invalidate;
end;

procedure TCustomGuiMidiKeys.SetKeyVelocity(FromKey, ToKey: Byte; Amount: Single);
var
  i, tmp: Byte;
begin
 if ToKey < FromKey then
  begin
   tmp := ToKey;
   ToKey := FromKey;
   FromKey := tmp;
  end; // Flip!!!

 for i := FromKey to Min(ToKey, CKeyboardHighestKey)
  do FKeys[i].Velocity := Amount;
end;

procedure TCustomGuiMidiKeys.SetKeyColor(FromKey, ToKey: Byte;BaseColor, Over, Pressed: TColor);
var
  i, tmp: Byte;
begin
 if ToKey < FromKey then
  begin
   // Flip!!!
   tmp := ToKey;
   ToKey := FromKey;
   FromKey := tmp;
  end;

 for i := FromKey to Min(ToKey, CKeyboardHighestKey) do
  begin
   FKeys[i].BaseColor    := BaseColor;
   FKeys[i].PressedColor := Pressed;
   FKeys[i].OverColor    := Over;
  end;

 Invalidate;
end;

procedure TCustomGuiMidiKeys.RemoveKeyColor(FromKey, ToKey: Byte);
begin
 SetKeyColor(FromKey, ToKey);
end;

procedure TCustomGuiMidiKeys.SetKeyPressed(KeyNr: Byte; KeyDown: Boolean);
begin
 if not KeyDown then ReleaseKey(KeyNr)
 else if KeyNr < CKeyboardHighestKey then Include(FKeys[KeyNr].Flags, kfPressed);

 if FRedrawTimer.Interval < 1
  then Invalidate
  else FTimerMustRedraw := True;
end;

procedure TCustomGuiMidiKeys.ReleaseKey(KeyNr: Byte; ReleaseMouseTriggered: boolean);
begin
 if KeyNr < CKeyboardHighestKey then
  if ReleaseMouseTriggered or not (kfByMouse in FKeys[KeyNr].Flags) then
   begin
    Exclude(FKeys[KeyNr].Flags, kfPressed);
    Exclude(FKeys[KeyNr].Flags, kfByMouse);
    Exclude(FKeys[KeyNr].Flags, kfMousePinned);
   end;

 if FRedrawTimer.Interval < 1
  then Invalidate
  else FTimerMustRedraw := True;
end;

procedure TCustomGuiMidiKeys.ReleaseAllKeys(ReleaseMouseTriggered: boolean);
var
  i: Byte;
begin
 for i := 0 to CKeyboardHighestKey do
  if ReleaseMouseTriggered or not (kfByMouse in FKeys[i].Flags) then
   begin
    Exclude(FKeys[i].Flags, kfPressed);
    Exclude(FKeys[i].Flags, kfByMouse);
    Exclude(FKeys[i].Flags, kfMousePinned);
   end;

 if FRedrawTimer.Interval < 1
  then Invalidate
  else FTimerMustRedraw := True;
end;

procedure TCustomGuiMidiKeys.SetKeyZones(Value: TGuiKeyZoneCollection);
begin
 FKeyZones.Assign(Value);
end;

function TCustomGuiMidiKeys.ZoneKeyArea(KeyNr: Byte): TRect;
begin
 Result := FKeys[KeyNr].Area;
 if kfBlackKey in FKeys[KeyNr].Flags then exit;
 if KeyNr > 0 then
   Result.Left := Max(Result.Left, FKeys[KeyNr - 1].Area.Right + 1);
 if KeyNr < CKeyboardHighestKey then
   Result.Right := Min(Result.Right, FKeys[KeyNr + 1].Area.Left - 1);
end;

procedure TCustomGuiMidiKeys.DrawKeyZones(Bitmap: TBitmap);
var
  i         : Integer;
  LowBound  : Integer;
  HighBound : Integer;
begin
 with Bitmap.Canvas do
  begin
   Brush.Style := bsSolid;
   if FMidiZoneBarMouseOver or (FMidiZoneBarDragging.isDragging)
    then Brush.Color := FZoneBarHoverColor
    else Brush.Color := FZoneBarColor;

   FillRect(Rect(1, 1, OversamplingFactor * Width - 1,
     OversamplingFactor * (FKeyZoneHeight + 1)));

   if FKeyZones.Count < 1 then Exit;
   for i := 0 to FKeyZones.Count - 1 do
    if FKeyZones.Items[i].Visible then
     with FKeyZones.Items[i] do
      begin
       LowBound := ZoneKeyArea(LowestZoneKey).Left;
       HighBound := ZoneKeyArea(HighestZoneKey).Right;

       if HighBound <> LowBound then
        begin
         if Selected then
          begin
           Brush.Color := SelectedBrushColor;
           Brush.Style := SelectedBrushStyle;
          end else
         if IsMouseOver then
          begin
           Brush.Color := HoverBrushColor;
           Brush.Style := HoverBrushStyle;
          end
         else
          begin
           Brush.Color := DefaultBrushColor;
           Brush.Style := DefaultBrushStyle;
          end;

         Pen.Style := psClear;
         Rectangle(OversamplingFactor * LowBound, 1,
           OversamplingFactor * (HighBound + 2),
           OversamplingFactor * (FKeyZoneHeight + 3));

         if Selected then
          begin
           Pen.Color := SelectedBorderColor;
           Pen.Width := SelectedBorderWidth;
           Pen.Style := SelectedBorderStyle;
          end else
         if IsMouseOver then
          begin
           Pen.Color := HoverBorderColor;
           Pen.Width := HoverBorderWidth;
           Pen.Style := HoverBorderStyle;
          end
         else
          begin
           Pen.Color := DefaultBorderColor;
           Pen.Width := DefaultBorderWidth;
           Pen.Style := DefaultBorderStyle;
          end;

         Dec(LowBound);
         Inc(HighBound);
         MoveTo(OversamplingFactor * LowBound, 1);  LineTo(OversamplingFactor * LowBound,
           OversamplingFactor * (FKeyZoneHeight + 1));
         MoveTo(OversamplingFactor * HighBound, 1); LineTo(OversamplingFactor * HighBound,
           OversamplingFactor * (FKeyZoneHeight + 1));
       end;
     end;
  end;
end;

procedure TCustomGuiMidiKeys.DrawSingleKey(Bitmap: TBitmap; CurKey: TGuiSingleKey);
var
  KeyColor       : TColor;
  LightAndShadow : TGuiColorRect;
begin
  if kfPressed in CurKey.Flags then KeyColor := CurKey.PressedColor else
  if kfMouseOver in CurKey.Flags then KeyColor := CurKey.OverColor
   else KeyColor := CurKey.BaseColor;

  if kfBlackKey in CurKey.Flags then
   begin
    if KeyColor = clNone then
     begin
      if kfPressed in CurKey.Flags then KeyColor := FBlackKeyPressedColor else
      if kfMouseOver in CurKey.Flags then KeyColor := FBlackKeyHoverColor
       else KeyColor := FBlackKeyColor;
     end;
   end
  else
   begin
    if KeyColor = clNone then
     begin
      if kfPressed in CurKey.Flags then KeyColor := FWhiteKeyPressedColor else
      if kfMouseOver in CurKey.Flags then KeyColor := FWhiteKeyHoverColor
       else KeyColor := FWhiteKeyColor;
     end;
   end;

  if (FKeyDownMode = kdmFlat) and (kfPressed in CurKey.Flags) then
   begin
    LightAndShadow.Top := KeyColor;
    LightAndShadow.Left := KeyColor;
    LightAndShadow.Bottom := KeyColor;
    LightAndShadow.Right := KeyColor;
   end
  else LightAndShadow := CalculateLights(KeyColor, kfBlackKey in CurKey.Flags,
    (kfPressed in CurKey.Flags) and (FKeyDownMode = kdmDown));

  if Assigned(FOnGetKeyColor) then
    FOnGetKeyColor(Self, CurKey.KeyNr, KeyColor, kfBlackKey in CurKey.Flags);

  with Bitmap.Canvas do
   begin
    Brush.Color := KeyColor;
    FillRect(CurKey.Area);
    MoveTo(CurKey.Area.Left,CurKey.Area.Top);
    Pen.Color := LightAndShadow.Top;    LineTo(CurKey.Area.Right, CurKey.Area.Top);
    Pen.Color := LightAndShadow.Right;  LineTo(CurKey.Area.Right, CurKey.Area.Bottom);
    Pen.Color := LightAndShadow.Bottom; LineTo(CurKey.Area.Left, CurKey.Area.Bottom);
    Pen.Color := LightAndShadow.Left;   LineTo(CurKey.Area.Left, CurKey.Area.Top);
   end;
end;

procedure TCustomGuiMidiKeys.DrawKeys(Bitmap: TBitmap);
var
  i: Byte;
const
  CDrawFlags = [kfVisible, kfBlackKey];   
begin
 with Bitmap.Canvas do
  begin
    Brush.Style := bsSolid;
    Pen.Width   := 1;
    Pen.Style   := psSolid;

    for i := 0 to CKeyboardHighestKey do
     begin
      if (kfVisible in FKeys[i].Flags) and not (kfBlackKey in FKeys[i].Flags)
       then DrawSingleKey(Bitmap, FKeys[i]);
      if (i > 0) and (FKeys[i - 1].Flags * CDrawFlags = CDrawFlags)
       then DrawSingleKey(Bitmap, FKeys[i - 1]);
    end;
  end;
end;

procedure TCustomGuiMidiKeys.RenderToBitmap(Bitmap: TBitmap);
begin
 with Bitmap.Canvas do
  begin
   Lock;
   try
    Brush.Color := FKeySeparatorColor;
    if FShowKeyZones then
     begin
      DrawKeyZones(Bitmap);

      Pen.Width := 1;
      Pen.Style := psSolid;
      Pen.Color := FZoneSeparatorColor;

      MoveTo(1,
        OversamplingFactor * (FKeyZoneHeight + 1));
      LineTo(OversamplingFactor * Width - 1 ,
        OversamplingFactor * (FKeyZoneHeight + 1));
     end;

    DrawKeys(Bitmap);
   finally
    UnLock;
   end;
  end;
end;

procedure TCustomGuiMidiKeys.UpdateBuffer;
var
  Bmp : TBitmap;
begin
 if not FRedrawEnabled then Exit;
 if (Width > 0) and (Height > 0) then
  with FBuffer.Canvas do
   begin
    Lock;
    try
     if AntiAlias = gaaNone then
      begin
       Assert(FBuffer.Width = Width);
       Assert(FBuffer.Height = Height);

       Brush.Color := FKeySeparatorColor;
       FillRect(ClientRect);

       RenderToBitmap(FBuffer);
      end
     else
      begin
       Bmp := TBitmap.Create;
       with Bmp do
        try
         PixelFormat := pf32bit;
         Width       := OversamplingFactor * FBuffer.Width;
         Height      := OversamplingFactor * FBuffer.Height;

         Brush.Color := FKeySeparatorColor;
         FillRect(ClientRect);

         RenderToBitmap(Bmp);
         DownsampleBitmap(Bmp);
         FBuffer.Canvas.Draw(0, 0, Bmp);
        finally
         Free;
        end;
      end;

     // draw border 
     Pen.Width := 1;
     Pen.Style := psSolid;
     Pen.Color := FBorderColor;

     MoveTo(0        , 0);
     LineTo(Width - 1, 0);
     LineTo(Width - 1, Height - 1);
     LineTo(0        , Height - 1);
     LineTo(0        , 0);
    finally
     UnLock;
    end;
   end;
end;

function TCustomGuiMidiKeys.ScreenCoordOnKey(X, Y: Integer; KeyNr: Integer; CheckYPos: Boolean): Boolean;
begin
 Result := False;
 with FKeys[KeyNr] do
  if kfVisible in Flags then
   begin
    if (X * OversamplingFactor <= Area.Right) and (X * OversamplingFactor >= Area.Left) then
     begin
      // check for overlaying black key
      if not (kfBlackKey in Flags) and (KeyNr < CKeyboardHighestKey) then
       begin
        if ScreenCoordOnKey(X, Y, KeyNr + 1, CheckYPos) then
         begin
          Result := False;
          Exit;
         end;
       end;

      // so X is on the key, and there is nothing overlaying, now check Y if required
      if CheckYPos
       then Result := (Y * OversamplingFactor <= Area.Bottom) and (Y * OversamplingFactor >= Area.Top)
       else Result := True;
     end;
   end;
end;

// returns -1 if it's not a visible key
function TCustomGuiMidiKeys.MousePosToKey(X, Y: Integer; CheckYPos: Boolean): Integer;
var
  i: Byte;
begin
 Result := -1;
 if (X < 1) or (X > Width - 2) then exit;

 for i := 0 to CKeyboardHighestKey do
  begin
   if ScreenCoordOnKey(X, Y, i, CheckYPos) then
    begin
     Result := i;
     Exit;
    end;
  end;
end;

procedure TCustomGuiMidiKeys.FireNoteOn(KeyNr: Byte; Flags: TGuiKeyFlags);
begin
 if kfPressed in FKeys[KeyNr].Flags then
  begin
   FKeys[KeyNr].Flags := FKeys[KeyNr].Flags + Flags;
   Exit;
  end;

 FKeys[KeyNr].Flags := FKeys[KeyNr].Flags + Flags;

 if Assigned(FOnNoteOn)
  then FOnNoteOn(Self, KeyNr, FKeys[KeyNr].Velocity);
end;

procedure TCustomGuiMidiKeys.FireNoteOff(KeyNr: Byte);
begin
 if not (kfPressed in FKeys[KeyNr].Flags) then Exit;

 Exclude(FKeys[KeyNr].Flags, kfPressed);
 Exclude(FKeys[KeyNr].Flags, kfByMouse);
 Exclude(FKeys[KeyNr].Flags, kfMousePinned);
  
 if Assigned(FOnNoteOff) then FOnNoteOff(Self, KeyNr);
end;

procedure TCustomGuiMidiKeys.AllNotesOff(MouseTriggeredOnly: Boolean; ExceptKey: Integer);
var
  i: Integer;
begin
 for i := 0 to CKeyboardHighestKey do
  if (i <> ExceptKey) and (kfPressed in FKeys[i].Flags) then
   if (MouseTriggeredOnly and (kfByMouse in FKeys[i].Flags) and not (kfMousePinned in FKeys[i].Flags)) or not MouseTriggeredOnly
    then FireNoteOff(i);
end;

procedure TCustomGuiMidiKeys.MouseLeaveAllKeys(ExceptKey: Integer);
var
  i: Integer;
begin
 for i := 0 to CKeyboardHighestKey do
  if (i <> ExceptKey) and (kfMouseOver in FKeys[i].Flags) then
   begin
    KeyMouseLeave(i);
    Exclude(FKeys[i].Flags, kfMouseOver);
   end;
end;

procedure TCustomGuiMidiKeys.MouseLeave;
begin
 inherited;
 if FMidiZoneBarMouseOver
  then ZoneBarMouseLeave
  else MouseLeaveAllKeys;
end;

procedure TCustomGuiMidiKeys.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  mkey: Integer;
begin
 if Enabled and (X > 0) and (X < Width-1) and (y > 0) and (y < Height - 1) then
  begin
   FRedrawEnabled := False;

   if FShowKeyZones and (Y<=FKeyZoneHeight) then
    begin
     mkey := MousePosToKey(x,y,False);
     if mkey>=0 then
      begin
       ZoneMouseActivity(kmaDown, mkey, Button, Shift, X, Y);
       if not FMidiZoneBarDragging.isDragging
        then ZoneMouseActivity(kmaStartDrag, mkey, Button, Shift, X, Y);
      end;
    end
   else
    begin
     mkey := MousePosToKey(x,y);
     if mkey >= 0 then
      begin
       MouseDownOnMidiKey(mkey, Button, Shift, X, Y);
       if not FMidiKeyDragging.isDragging and FAllowKeyDragging
        then StartKeyDragging(mkey, Button, Shift, X, Y);
      end;
    end;

  FRedrawEnabled := True;
  Invalidate;
 end;

 inherited;
end;

procedure TCustomGuiMidiKeys.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  mkey: Integer;
begin
 if Enabled then
  begin
   FRedrawEnabled := False;
   if FMidiZoneBarDragging.isDragging and (Button = FMidiZoneBarDragging.Button)
    then EndZoneBarDragging(Shift, X,Y);

   if FMidiKeyDragging.isDragging and (Button = FMidiKeyDragging.Button)
    then EndKeyDragging(Shift, X,Y);

   if (X > 0) and (X < Width - 1) and (y > 0) and (y < Height - 1) then
    begin
     if FShowKeyZones and (Y <= FKeyZoneHeight) then
      begin
       mkey := MousePosToKey(x,y,False);
       if mkey >= 0
        then ZoneMouseActivity(kmaUp, mkey, Button, Shift, X, Y);
      end
     else
      begin
       mkey := MousePosToKey(x,y);
       if mkey >= 0
        then MouseUpOnMidiKey(mkey, Button, Shift, X, Y);
      end;
    end;

   FRedrawEnabled := True;
   Invalidate;
  end;

 inherited;
end;

procedure TCustomGuiMidiKeys.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  mkey: Integer;
begin
 if Enabled then
  begin
   FRedrawEnabled := False;

   if (FMidiZoneBarDragging.isDragging) then
    begin
     mkey := MousePosToKey(x, y, False);
     MoveZoneBarDragging(mkey, Shift, X, Y);
    end else
   if FMidiKeyDragging.isDragging then
    begin
     if ((not FShowKeyZones and (y > 0)) or (FShowKeyZones and (y > FKeyZoneHeight))) and (y < Height - 1)
      then mkey := MousePosToKey(x, y)
      else mkey := MousePosToKey(x, y, False);

      MoveKeyDragging(mkey, Shift, X, Y);
    end
   else
    begin
     if FShowKeyZones and (Y <= FKeyZoneHeight) then
      begin
       mkey := MousePosToKey(x,y,False);

       if not FMidiZoneBarMouseOver then
        begin
         if mkey > -1 then ZoneBarMouseEnter(mkey, Shift, X, Y);
         MouseLeaveAllKeys;
        end;

       ZoneMouseActivity(kmaMove, mkey, mbMiddle, Shift, X, Y);
      end
     else
      begin
       if FMidiZoneBarMouseOver then ZoneBarMouseLeave;

       mkey := MousePosToKey(x, y);
       MouseLeaveAllKeys(mkey);

       if (mkey>-1) and not (kfMouseOver in FKeys[mkey].Flags) then
        begin
         Include(FKeys[mkey].Flags, kfMouseOver);
         KeyMouseEnter(mkey, Shift, X, Y);
        end;
      end;
    end;

   FRedrawEnabled := True;
   FTimerMustRedraw := True;
  end;

 inherited;
end;




{ midi key mouse tracking
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.KeyMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer);
begin
 Cursor := CursorKeys;
 if Assigned(FOnKeyMouseEnter) then FOnKeyMouseEnter(Self, KeyNr, Shift, X, Y);
end;

procedure TCustomGuiMidiKeys.KeyMouseLeave(KeyNr: Byte);
begin
 if Assigned(FOnKeyMouseLeave) then FOnKeyMouseLeave(Self, KeyNr);
end;


{ midi key mouse up/down
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.MouseUpOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 if Assigned(FOnMouseUpOnMidiKey) then FOnMouseUpOnMidiKey(Self,KeyNr, Button, Shift, X,Y);
end;

procedure TCustomGuiMidiKeys.MouseDownOnMidiKey(KeyNr: Byte; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 if (Button = mbRight) then
  begin
   if (kfPressed in FKeys[KeyNr].Flags) and (kfMousePinned in FKeys[KeyNr].Flags)
    then FireNoteOff(KeyNr)
    else FireNoteOn(KeyNr, [kfPressed, kfByMouse, kfMousePinned]);
  end else FireNoteOn(KeyNr, [kfPressed, kfByMouse]);

 if Assigned(FOnMouseDownOnMidiKey) then FOnMouseDownOnMidiKey(Self, KeyNr, Button, Shift, X,Y);
end;


{ midi key mouse dragging
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.StartKeyDragging(KeyNr: Integer; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 MouseLeaveAllKeys;

 FMidiKeyDragging.Button := DragButton;
 FMidiKeyDragging.isDragging := True;
 FMidiKeyDragging.LastKey := KeyNr;
 FMidiKeyDragging.StartKey := KeyNr;

 if Assigned(FOnStartKeyDragging)
  then FOnStartKeyDragging(Self, KeyNr, FMidiKeyDragging, Shift, X, Y);
end;

procedure TCustomGuiMidiKeys.MoveKeyDragging(KeyNr: Integer; Shift: TShiftState; X,Y: Integer);
begin
 if (FMidiKeyDragging.Button <> mbRight) then
  begin
   AllNotesOff(True, KeyNr);
   if KeyNr >= 0 then FireNoteOn(KeyNr, [kfPressed, kfByMouse]);
  end;

  if Assigned(FOnMoveKeyDragging)
   then FOnMoveKeyDragging(Self, KeyNr, FMidiKeyDragging, Shift, X, Y);
  FMidiKeyDragging.LastKey := KeyNr;
end;

procedure TCustomGuiMidiKeys.EndKeyDragging(Shift: TShiftState; X,Y: Integer);
begin
  AllNotesOff;
  if Assigned(FOnEndKeyDragging)
   then FOnEndKeyDragging(Self, FMidiKeyDragging, Shift, X, Y);
  FMidiKeyDragging.isDragging := False;
end;



{ zone bar mouse tracking
--------------------------------------------------------------------------------}
function TCustomGuiMidiKeys.GetZoneMouseOverType(X: Integer; KeyNr: Integer; Zone: TGuiKeyZoneItem): TGuiZoneMousePosType;
var
  ZKeyArea : TRect;
  temp     : Integer;
begin
 Result := [mptOutside];
 if KeyNr < 0 then KeyNr := MousePosToKey(X, 0, False) else
 if not ScreenCoordOnKey(X, 0, KeyNr, False) then Exit;

 if KeyNr < 0 then exit;
 if Zone = nil then Zone := KeyZones.ZoneByKey(KeyNr);
 if Zone = nil then exit;

 Result := [mptInZone];

 ZKeyArea := ZoneKeyArea(KeyNr);
 temp := ZKeyArea.Right - ZKeyArea.Left;
 if KeyNr = Zone.LowestZoneKey then
  begin
   Include(Result, mptOnLowestKey);
   if X < ZKeyArea.Left + temp div 4
    then Include(Result, mptOnLowestBorder);
  end;
 if KeyNr = Zone.HighestZoneKey then
  begin
   Include(Result,mptOnHighestKey);
   if X > ZKeyArea.Right - temp div 4
    then Include(Result, mptOnHighestBorder);
  end;
end;

procedure TCustomGuiMidiKeys.ZoneMouseActivity(Action: TGuiKbMouseAction; KeyNr: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Zone             : TGuiKeyZoneItem;
  tmpMouseOverType : TGuiZoneMousePosType;
begin
 if KeyNr < 0 then Exit;
 Zone := KeyZones.ZoneByKey(KeyNr);
 if Zone = nil then
  begin
   MouseLeaveAllZones;

   case Action of
    kmaDown:
     begin
      ZoneSelectionChanged;
      MouseDownOnKeyZoneBar(KeyNr, nil, [mptOutside], Button, Shift, X, Y);
     end;

    kmaUp : MouseUpOnKeyZoneBar(KeyNr, nil, [mptOutside], Button, Shift, X, Y);

    kmaStartDrag: StartZoneBarDragging(KeyNr, nil, [mptOutside], Button, Shift, X, Y);
   end;
   Exit;
  end;

  tmpMouseOverType := GetZoneMouseOverType(X, KeyNr, Zone);
  case Action of
   kmaMove:
    begin
     // new hover
     if not Zone.IsMouseOver then
      begin
       MouseLeaveAllZones(Zone);
       ZoneMouseEnter(Zone, Shift, X, Y);
       ZoneMouseOverChanged(KeyNr, Zone, tmpMouseOverType, Shift, X, Y);
      end else
     if FZoneMouseOverType <> tmpMouseOverType
      then ZoneMouseOverChanged(KeyNr, Zone, tmpMouseOverType, Shift, X, Y);
    end;

   kmaDown:
    begin
     // new select
     if not Zone.Selected then ZoneSelectionChanged(Zone);
     MouseDownOnKeyZoneBar(KeyNr, Zone, tmpMouseOverType, Button, Shift, X, Y);
    end;

   kmaUp:  MouseUpOnKeyZoneBar(KeyNr, Zone, tmpMouseOverType, Button, Shift, X, Y);

   kmaStartDrag: StartZoneBarDragging(KeyNr, Zone, tmpMouseOverType, Button, Shift, X, Y);
  end;
end; 



{ midi zone mouse selection
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.UnSelectAllZones(ExceptZone: TGuiKeyZoneItem);
var
  i : Integer;
begin
 for i := KeyZones.Count-1 downto 0 do
  if (ExceptZone <> KeyZones.Items[i]) and KeyZones.Items[i].Selected
   then KeyZones.Items[i].Unselect(False);
end;

procedure TCustomGuiMidiKeys.ZoneSelectionChanged(Zone: TGuiKeyZoneItem);
begin
 if Zone <> nil then Zone.Select(False);
 UnSelectAllZones(Zone);
 if Assigned(FOnZoneSelectionChanged)
  then FOnZoneSelectionChanged(Self, Zone);
end;

{ zone bar mouse enter/leave
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.MouseLeaveAllZones(ExceptZone: TGuiKeyZoneItem);
var
  i : Integer;
begin
 for i := KeyZones.Count-1 downto 0 do
  if (ExceptZone <> KeyZones.Items[i]) and KeyZones.Items[i].IsMouseOver
   then ZoneMouseLeave(KeyZones.Items[i]);
end;

procedure TCustomGuiMidiKeys.ZoneMouseEnter(Zone: TGuiKeyZoneItem; Shift: TShiftState; X, Y: Integer);
begin
 Cursor := CursorZone;
 Zone.SetMouseOver(True, False);
 if Assigned(FOnZoneMouseEnter)
  then FOnZoneMouseEnter(Self, Zone, Shift, X, Y);
end;

procedure TCustomGuiMidiKeys.ZoneMouseLeave(Zone: TGuiKeyZoneItem);
begin
 FZoneMouseOverType := [];
 Cursor := CursorZoneBar;
 Zone.SetMouseOver(False, False);
 if Assigned(FOnZoneMouseLeave)
  then FOnZoneMouseLeave(Self, Zone);
end;

procedure TCustomGuiMidiKeys.ZoneMouseOverChanged(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Shift: TShiftState; X, Y: Integer);
begin
 FZoneMouseOverType := MouseOverType;
 if (mptOnLowestBorder in MouseOverType) or (mptOnHighestBorder in MouseOverType)
  then Cursor := CursorZoneBorder
  else
 if mptInZone in MouseOverType
  then Cursor := CursorZone
  else Cursor := CursorZoneBar;
 if Assigned(FOnZoneMouseOverChanged)
  then FOnZoneMouseOverChanged(Self, KeyNr, Zone, MouseOverType, Shift, X, Y);
end;


procedure TCustomGuiMidiKeys.ZoneBarMouseEnter(KeyNr: Byte; Shift: TShiftState; X, Y: Integer);
begin
 Cursor := CursorZoneBar;
 FMidiZoneBarMouseOver := True;
 if Assigned(FOnZoneBarMouseEnter)
  then FOnZoneBarMouseEnter(Self, KeyNr, Shift, X, Y);
end;

procedure TCustomGuiMidiKeys.ZoneBarMouseLeave;
begin
 Cursor := CursorKeys;
 FMidiZoneBarMouseOver := False;
 MouseLeaveAllZones;
 if Assigned(FOnZoneBarMouseLeave) then FOnZoneBarMouseLeave(Self);
end;

{ zone bar mouse up/down
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.MouseUpOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 if Assigned(FOnMouseUpOnKeyZoneBar)
  then FOnMouseUpOnKeyZoneBar(Self, KeyNr, Zone, MouseOverType, Button, Shift, X,Y);
end;

procedure TCustomGuiMidiKeys.MouseDownOnKeyZoneBar(KeyNr: Byte; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 if Assigned(FOnMouseDownOnKeyZoneBar)
  then FOnMouseDownOnKeyZoneBar(Self, KeyNr, Zone, MouseOverType, Button, Shift, X,Y);
end;

{ zone bar dragging
--------------------------------------------------------------------------------}
procedure TCustomGuiMidiKeys.StartZoneBarDragging(KeyNr: Integer; Zone: TGuiKeyZoneItem; MouseOverType: TGuiZoneMousePosType; DragButton: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
 with FMidiZoneBarDragging do
  begin
   Button     := DragButton;
   isDragging := True;
   Zone       := Zone;
   InZonePos  := MouseOverType;
   LastKey    := KeyNr;
   StartKey   := KeyNr;
   if Zone <> nil then
    begin
     StartLowestZoneKey := Zone.LowestZoneKey;
     StartHighestZoneKey := Zone.HighestZoneKey;
    end;
  end;

 if Assigned(FOnStartZoneBarDragging)
  then FOnStartZoneBarDragging(Self, KeyNr, FMidiZoneBarDragging, Shift, X, Y);
end;

procedure TCustomGuiMidiKeys.MoveZoneBarDragging(KeyNr: Integer; Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FOnMoveZoneBarDragging)
  then FOnMoveZoneBarDragging(Self, KeyNr, FMidiZoneBarDragging, Shift, X, Y);
 FMidiZoneBarDragging.LastKey := KeyNr;
end;

procedure TCustomGuiMidiKeys.EndZoneBarDragging(Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FOnEndZoneBarDragging)
  then FOnEndZoneBarDragging(Self, FMidiZoneBarDragging, Shift, X, Y);
 FMidiZoneBarDragging.isDragging := False;
end;

end.
