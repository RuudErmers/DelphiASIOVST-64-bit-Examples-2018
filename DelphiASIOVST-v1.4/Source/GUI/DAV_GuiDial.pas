unit DAV_GuiDial;

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
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, Contnrs, DAV_GuiCommon,
  DAV_GuiBaseControl;

type
  TGuiDialRMBFunc = (rmbfReset, rmbfCircular);
  TGuiDialImageList = class;
  TGuiDialImageCollectionItem = class;

  {$IFDEF DELPHI10_UP} {$region 'Pointer Angles'} {$ENDIF}

  TGuiDialSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected
    procedure Changed;
  public
    constructor Create; virtual;
  end;

  TGuiDialPointerAngles = class(TGuiDialSettings)
  private
    FResolution : Extended;
    FStart      : Integer;
    FRange      : Integer;
    procedure SetRange(const Value: Integer);
    procedure SetResolution(const Value: Extended);
    procedure SetStart(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
  published
    property Start: Integer read FStart write SetStart default 0;
    property Range: Integer read FRange write SetRange default 360;
    property Resolution: Extended read FResolution write SetResolution;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Custom Dials'} {$ENDIF}

  TCustomGuiStitchedControl = class(TCustomGuiBaseAntialiasedControl)
  private
    function GetDialImageIndex: Integer;
    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean); reintroduce;
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetImageList(const Value: TImageList);
    procedure SetDialImageIndex(Value: Integer);
    procedure SetDialImageList(const Value: TGuiDialImageList);
    procedure SetDialAlpha(const Value: TBitmap);
  protected
    FAutoSize      : Boolean;
    FDialBitmap    : TBitmap;
    FDialAlpha     : TBitmap;
    FImageList     : TImageList;
    FGlyphCount    : Integer;
    FOnChange      : TNotifyEvent;
    FStitchKind    : TGuiStitchKind;
    FDialImageList : TGuiDialImageList;
    FDialImageItem : TGuiDialImageCollectionItem;

    function GetGlyphNr: Integer; virtual; abstract;
    procedure SettingsChanged(Sender: TObject); virtual;
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;
    procedure UpdateBuffer; override;
    procedure RenderBitmap(const Bitmap: TBitmap); virtual; abstract;

    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property DialBitmap: TBitmap read FDialBitmap write SetDialBitmap;
    property DialAlpha: TBitmap read FDialAlpha write SetDialAlpha;
    property DialImageIndex: Integer read GetDialImageIndex write SetDialImageIndex;
    property DialImageList: TGuiDialImageList read FDialImageList write SetDialImageList;
    property ImageList: TImageList read FImageList write SetImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
  end;

  TQuantizeValueEvent = procedure(Sender: TObject; var NewValue: Double) of object;

  TCustomGuiDial = class(TCustomGuiStitchedControl)
  private
    FAutoColor        : Boolean;
    FCircleColor      : TColor;
    FCurveMapping     : Single;
    FCurveMappingExp  : Single;
    FDefaultPosition  : Single;
    FInertia          : Single;
    FInertiaExp       : Single;
    FInertiaScale     : Single;
    FMin, FMax        : Single;
    FPointerAngles    : TGuiDialPointerAngles;
    FPosition         : Single;
    FRightMouseButton : TGuiDialRMBFunc;
    FScrollRange      : Single;
    FWheelStep        : Single;
    FOnQuantizeValue  : TQuantizeValueEvent;
    function CircularMouseToPosition(X, Y: Integer): Single;
    function PositionToAngle: Single;
    function GetNormalizedPosition: Single;
    function GetMappedPosition: Single;
    function MapValue(Value: Double): Double;
    function UnmapValue(Value: Double): Double;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetCircleColor(const Value: TColor);
    procedure SetDefaultPosition(Value: Single);
    procedure SetInertia(Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetPointerAngles(const Value: TGuiDialPointerAngles);
    procedure SetPosition(Value: Single);
    procedure SetCurveMapping(const Value: Single);
    procedure SetNormalizedPosition(const Value: Single);

    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
  protected
    function GetGlyphNr: Integer; override;

    procedure CalcColorCircle;
    procedure CircleColorChanged; virtual;
    procedure CurveMappingChanged; virtual;
    procedure InertiaChanged; virtual;
    procedure PositionChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure RenderBitmap(const Bitmap: TBitmap); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); override;
    procedure DragMouseMoveRight(Shift: TShiftState; X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    property NormalizedPosition: Single read GetNormalizedPosition write SetNormalizedPosition;
    property MappedPosition: Single read GetMappedPosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property CircleColor : TColor read FCircleColor write SetCircleColor default clBlack;
    property CurveMapping: Single read FCurveMapping write SetCurveMapping;
    property DefaultPosition: Single read FDefaultPosition write SetDefaultPosition;
    property Inertia: Single read FInertia write SetInertia;
    property Max: Single read FMax write SetMax;
    property Min: Single read FMin write SetMin;
    property PointerAngles: TGuiDialPointerAngles read FPointerAngles write SetPointerAngles;
    property Position: Single read FPosition write SetPosition;
    property RightMouseButton: TGuiDialRMBFunc read FRightMouseButton write FRightMouseButton default rmbfCircular;
    property ScrollRange_Pixel: Single read FScrollRange write FScrollRange;
    property WheelStep: Single read FWheelStep write FWheelStep;
    property OnQuantizeValue: TQuantizeValueEvent read FOnQuantizeValue write FOnQuantizeValue;
  end;

  TCustomGuiSwitch = class(TCustomGuiStitchedControl)
  private
    FGlyphNr        : Integer;
    FDefaultGlyphNr : Integer;
    FStringList     : TStringList;
    FReadOnly       : Boolean;
    procedure SetGlyphNr(Value: Integer);
    procedure SetDefaultGlyphNr(Value: Integer);
    procedure SetStringList(const Value: TStringList);
    procedure GlyphNrChanged;
  protected
    function GetGlyphNr: Integer; override;
    procedure RenderBitmap(const Bitmap: TBitmap); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure GlyphCountChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DefaultGlyphNr: Integer read FDefaultGlyphNr write SetDefaultGlyphNr;
    property GlyphNr: Integer read FGlyphNr write SetGlyphNr;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property StringList: TStringList read FStringList write SetStringList; 
  end;

  TCustomGuiDialMetal = class(TCustomGuiDial)
  protected
    procedure RenderBitmap(const Bitmap: TBitmap); override;
  end;

  TCustomGuiDialEx = class(TCustomGuiDial)
  private
    FIndLineLength : Single;
    procedure SetIndLineLength(const Value: Single);
  protected
    procedure IndLineLengthChanged;
    procedure RenderBitmap(const Bitmap: TBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    property IndicatorLineLength_Percent: Single read fIndLineLength write SetIndLineLength;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Dials'} {$ENDIF}

  TGuiDial = class(TCustomGuiDial)
  published
    property PopupMenu;
    property Anchors;
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
//    property DialAlpha;
    property DialImageList;
    property DialImageIndex;
    property ImageList;
    property Inertia;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property GlyphCount;
    property OnChange;
    property OnDblClick;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnMouseWheel;
    property OnMouseMove;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnMouseUp;
    property OnMouseDown;
    property OnQuantizeValue;
    property ParentColor;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
    property WheelStep;
  end;

  TGuiDialMetal = class(TCustomGuiDialMetal)
  published
    property Anchors;
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property DialImageList;
    property DialImageIndex;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property GlyphCount;
    property OnChange;
    property ParentColor;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
    property WheelStep;
  end;

  TGuiDialEx = class(TCustomGuiDialEx)
  published
    property Anchors;
    property AntiAlias;
    property AutoColor;
    property AutoSize;
    property CircleColor;
    property Color;
    property CurveMapping;
    property DefaultPosition;
    property DialBitmap;
    property DialImageList;
    property DialImageIndex;
    property IndicatorLineLength_Percent;
    property LineColor;
    property LineWidth;
    property Max;
    property Min;
    property GlyphCount;
    property OnChange;
    property ParentColor;
    property PointerAngles;
    property Position;
    property RightMouseButton;
    property ScrollRange_Pixel;
    property StitchKind;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
    property WheelStep;
  end;

  TGuiSwitch = class(TCustomGuiSwitch)
  published
    property Anchors;
    property AntiAlias;
    property AutoSize;
    property Color;
    property DefaultGlyphNr;
    property DialBitmap;
    property DialImageList;
    property DialImageIndex;
    property GlyphNr;
    property ImageList;
    property LineColor;
    property LineWidth;
    property GlyphCount;
    property OnChange;
    property ParentColor;
    property ReadOnly;
    property StitchKind;
    property StringList;
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}
    property Visible;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'Dial Image List'} {$ENDIF}

  TGuiDialImageCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiDialImageCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiDialImageCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiDialImageCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiDialImageCollectionItem;
    function Insert(Index: Integer): TGuiDialImageCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGuiDialImageCollectionItem = class(TCollectionItem)
  private
    FDialBitmap  : TBitmap;
    FGlyphCount  : Integer;
    FOnChange    : TNotifyEvent;
    FStitchKind  : TGuiStitchKind;
    FLinkedDials : TObjectList;
    FDisplayName : string;
    procedure SetDialBitmap(const Value: TBitmap);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SettingsChanged(Sender: TObject);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure GlyphCountChanged; virtual;
    procedure StitchKindChanged; virtual;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LinkStitchedControl(Dial: TCustomGuiStitchedControl);
    procedure UnLinkStitchedControl(Dial: TCustomGuiStitchedControl);
  published
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property DialBitmap: TBitmap read FDialBitmap write SetDialBitmap;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGuiDialImageList = class(TComponent)
  private
    FDialImageCollection : TGuiDialImageCollection;
    function GetCount: Integer;
    function GetItems(Index: Integer): TGuiDialImageCollectionItem;
  protected
    property Items[Index: Integer]: TGuiDialImageCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DialImages: TGuiDialImageCollection read FDialImageCollection write FDialImageCollection;
    property Count: Integer read GetCount;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

implementation

uses
  Dialogs, ExtCtrls, Math, {$IFNDEF FPC}Consts, {$ENDIF} DAV_Math,
  DAV_Complex, ImgList;

resourcestring
  RCStrGlyphCountMustBePositive = 'GlyphCount must be > 0!';
  RCStrPointerAngleRange = 'Range must be 1..360';
  RCStrPointerRangeResolution = 'Resolution must be above 0 and less than %d';
  RCStrPointerAngleStart = 'Start must be 0..359';

function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
const
  DegPi : Double = (180 / PI);
begin
  Result := Radians * DegPi;
end;

function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
const
  MulFak = 180 / Pi;
begin
  Result := ArcTan2(X2 - X1, Y1 - Y2) * MulFak;
end;

function SafeAngle(Angle: Single): Single;
begin
  while Angle < 0 do Angle := Angle + 360;
  while Angle >= 360 do Angle := Angle - 360;
  Result := Angle;
end;

{ This function solves for x in the equation "x is y% of z". }
function SolveForX(Y, Z: Longint): Longint;
begin
  Result := Round(Z * (Y * 0.01));//tt
end;

{ This function solves for y in the equation "x is y% of z". }
function SolveForY(X, Z: Longint): Longint;
begin
  if Z = 0 then Result := 0 else Result := Round((X * 100.0) / Z); //t
end;


{$IFDEF DELPHI10_UP} {$region 'TGuiDialPointerAngles implementation'} {$ENDIF}

{ TGuiDialSettings }

procedure TGuiDialSettings.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

constructor TGuiDialSettings.Create;
begin
  inherited;
end;


{ TGuiDialPointerAngles }

procedure TGuiDialPointerAngles.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiDialPointerAngles then
  with TGuiDialPointerAngles(Dest) do
   begin
    FRange := Self.Range;
    FStart := Self.Start;
    FResolution := Self.Resolution;
    Changed;
   end else inherited;
end;

constructor TGuiDialPointerAngles.Create;
begin
  inherited;
  FStart := 0;
  FRange := 360;
  FResolution := 0;
end;

procedure TGuiDialPointerAngles.SetRange(const Value: Integer);
begin
  if (Value < 1) or (Value > 360)
   then raise Exception.Create(RCStrPointerAngleRange);

  FRange := Value;
  if FRange > Resolution then Resolution := FRange;
  Changed;
end;

procedure TGuiDialPointerAngles.SetResolution(const Value: Extended);
begin
  if (Value < 0) or (Value > Range)
   then raise Exception.CreateFmt(RCStrPointerRangeResolution, [Range + 1]);

  FResolution := Value;
  Changed;
end;

procedure TGuiDialPointerAngles.SetStart(const Value: Integer);
begin
  if (Value < 0) or (Value > 359)
   then raise Exception.Create(RCStrPointerAngleStart);

  FStart := Value;
  Changed;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'Custom Dial implementations'} {$ENDIF}

{ TCustomGuiStitchedControl }

constructor TCustomGuiStitchedControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FLineColor              := clRed;
 FLineWidth              := 2;
 FGlyphCount             := 1;
 FStitchKind             := skHorizontal;
 FDialBitmap             := TBitmap.Create;
 FDialBitmap.PixelFormat := pf24bit;
 FDialBitmap.OnChange    := SettingsChanged;
 FDialAlpha              := TBitmap.Create;
 FDialAlpha.PixelFormat  := pf8bit;
 FDialAlpha.OnChange     := SettingsChanged;
end;

destructor TCustomGuiStitchedControl.Destroy;
begin
 FreeAndNil(FDialBitmap);
 FreeAndNil(FDialAlpha);
 inherited Destroy;
end;

procedure TCustomGuiStitchedControl.SettingsChanged(Sender: TObject);
begin
 FDialBitmap.Canvas.Brush.Color := Self.Color;
 Invalidate;
end;

procedure TCustomGuiStitchedControl.UpdateBuffer;
var
  theRect    : TRect;
  GlyphIndex : Integer;
  Bmp        : TBitmap;
  OwnerDraw  : Boolean;
begin
 if [csLoading..csDestroying] * ComponentState <> [] then exit;

 if (Width > 0) and (Height > 0) then with FBuffer.Canvas do
  begin
   Lock;
   Brush.Color := Self.Color;
   OwnerDraw := FDialBitmap.Empty and not assigned(FImageList);
   if OwnerDraw and assigned(FDialImageList) and assigned(FDialImageItem)
    then OwnerDraw := FDialImageItem.FDialBitmap.Empty;

   if OwnerDraw then
    if AntiAlias = gaaNone then
     begin
      // draw background
      {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
      FillRect(ClipRect);

      RenderBitmap(FBuffer);
     end
    else
     begin
      Bmp := TBitmap.Create;
      with Bmp do
       try
        PixelFormat := pf32bit;
        Width       := OversamplingFactor * FBuffer.Width;
        Height      := OversamplingFactor * FBuffer.Height;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := Self.Color;
        {$IFNDEF FPC}
        if FTransparent then
         begin
          CopyParentImage(Self, Bmp.Canvas);
          UpsampleBitmap(Bmp);
         end else
        {$ENDIF}
        Canvas.FillRect(Canvas.ClipRect);
        RenderBitmap(Bmp);
        DownsampleBitmap(Bmp);
        FBuffer.Canvas.Draw(0, 0, Bmp);
       finally
        FreeAndNil(Bmp);
       end;
    end
   else
    begin
     // draw background
     Brush.Color := Self.Color;
     {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
     FillRect(ClipRect);

     GlyphIndex := GetGlyphNr;
     if (GlyphIndex >= FGlyphCount) then GlyphIndex := FGlyphCount - 1 else
     if (GlyphIndex < 0) then GlyphIndex := 0;

     if Assigned(FDialImageItem)
      then Bmp := FDialImageItem.FDialBitmap
      else Bmp := DialBitmap;

     if not Bmp.Empty then
      begin

       theRect := ClientRect;
       if FStitchKind = skVertical then
        begin
         theRect.Top    := Bmp.Height * GlyphIndex div FGlyphCount;
         theRect.Bottom := Bmp.Height * (GlyphIndex + 1) div FGlyphCount;
        end
       else
        begin
         theRect.Left  := Bmp.Width * GlyphIndex div FGlyphCount;
         theRect.Right := Bmp.Width * (GlyphIndex + 1) div FGlyphCount;
        end;

       with ClientRect do
        begin
         BitBlt(Handle, Left, Top, Right - Left, Bottom - Top,
           Bmp.Canvas.Handle, theRect.Left, theRect.Top, CopyMode);
        end;
      end else

     if Assigned(ImageList)
      then ImageList.Draw(FBuffer.Canvas, 0, 0, GlyphIndex);
    end;
   Unlock;
  end;
end;

function TCustomGuiStitchedControl.GetDialImageIndex: Integer;
begin
 if assigned(FDialImageItem)
  then Result := FDialImageItem.Index
  else Result := -1;
end;

procedure TCustomGuiStitchedControl.DoAutoSize;
begin
 if assigned(FImageList) then
  begin
   Width := FImageList.Width;
   Height := FImageList.Height;
   Exit;
  end;
 if FDialBitmap.Empty or (FGlyphCount = 0) then Exit;

 if FStitchKind = skVertical then
  begin
   Width  := FDialBitmap.Width;
   Height := FDialBitmap.Height div FGlyphCount;
  end
 else
  begin
   Width  := FDialBitmap.Width div FGlyphCount;
   Height := FDialBitmap.Height;
  end;
end;

procedure TCustomGuiStitchedControl.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if Autosize then DoAutoSize;
  end;
end;

procedure TCustomGuiStitchedControl.SetGlyphCount(const Value: Integer);
begin
 if assigned(FImageList) then exit;
 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TCustomGuiStitchedControl.GlyphCountChanged;
begin
 DoAutoSize;
end;

procedure TCustomGuiStitchedControl.SetDialAlpha(const Value: TBitmap);
begin
 FDialAlpha.Assign(Value);
end;

procedure TCustomGuiStitchedControl.SetDialBitmap(const Value: TBitmap);
begin
  FDialBitmap.Assign(Value);
  DoAutoSize;
end;

procedure TCustomGuiStitchedControl.SetDialImageIndex(Value: Integer);
begin
 // check if dial image list is available
 if not assigned(FDialImageList) then exit;

 // limit range to existing dial images
 if Value < 0 then Value := 0 else
 if Value >= FDialImageList.Count then Value := FDialImageList.Count - 1;

 if DialImageIndex <> Value then
  begin
   if Value >= 0
    then FDialImageList[Value].LinkStitchedControl(Self)
    else FDialImageItem.UnLinkStitchedControl(Self);
   FDialImageItem := FDialImageList[Value];
   Invalidate;
  end;
end;

procedure TCustomGuiStitchedControl.SetDialImageList(const Value: TGuiDialImageList);
begin
 if FDialImageList <> Value then
  begin
   FDialImageList := Value;
   if not assigned(FDialImageList)
    then FDialImageItem := nil;
   Invalidate;
  end;
end;

procedure TCustomGuiStitchedControl.SetImageList(const Value: TImageList);
begin
 if FImageList <> Value then
  begin
   FImageList := Value;
   if assigned(FImageList) then
    begin
     Width := FImageList.Width;
     Height := FImageList.Height;
     FGlyphCount := FImageList.Count;
    end;
   Invalidate;
  end;
end;

procedure TCustomGuiStitchedControl.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;

  end;
end;

procedure TCustomGuiStitchedControl.StitchKindChanged;
begin
 DoAutoSize;
end;


{ TCustomGuiDial }

constructor TCustomGuiDial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPointerAngles          := TGuiDialPointerAngles.Create;
  FPointerAngles.OnChange := SettingsChanged;
  FCircleColor            := clBlack;
  FLineColor              := clRed;
  FLineWidth              := 2;
  FRightMouseButton       := rmbfCircular;
  FCurveMapping           := 0;
  FCurveMappingExp        := 1;
  FPosition               := 0;
  FDefaultPosition        := 0;
  FScrollRange            := 400;
  FInertia                := 0;
  FInertiaExp             := 1;
  FInertiaScale           := 1;
  FMin                    := 0;
  FMax                    := 100;
  FWheelStep              := 1;
end;

destructor TCustomGuiDial.Destroy;
begin
  FreeAndNil(FPointerAngles);
  inherited Destroy;
end;

procedure TCustomGuiDial.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Max', ReadMaxProperty,
    WriteMaxProperty, Max = 0);
end;

function TCustomGuiDial.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Difference : Single;
begin
 Difference := FWheelStep * WheelDelta / (120 * FScrollRange);

// apply inertia function
 if Difference < 0
  then Difference := -Power(Abs(Difference), FInertiaExp) * FInertiaScale
  else Difference :=  Power(Abs(Difference), FInertiaExp) * FInertiaScale;
 NormalizedPosition := UnMapValue(MapValue(NormalizedPosition) + Difference);
 Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TCustomGuiDial.PositionToAngle: Single;
const
  Pi180 : Double = PI / 180;
begin
 Result := SafeAngle(PointerAngles.Start + (PointerAngles.Range * MapValue(NormalizedPosition))) * Pi180;
end;

procedure TCustomGuiDial.RenderBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Val, Off : TComplex64;
  Rad, tmp : Single;
  PtsArray : Array of TPoint;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - FLineWidth div 2;
   if Rad <= 0 then exit;
   Steps := Round(2 / ArcSin(1 / Rad)) + 1;
   if Steps > 1 then
    begin
     SetLength(PtsArray, Steps);
     GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
     Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
     GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
     PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));

     for i := 1 to Steps - 1 do
      begin
       tmp := Val.Re * Off.Re - Val.Im * Off.Im;
       Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
       Val.Re := tmp;
       PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
      end;

     Pen.Width := OversamplingFactor * FLineWidth;
     Pen.Color := FLineColor;
     Brush.Color := FCircleColor;
     Polygon(PtsArray);
    end;

   // draw position line
   MoveTo(PtsArray[0].X, PtsArray[0].Y);
   LineTo(Round(0.5 * Width), Round(0.5 * Height));
  end;
end;

function TCustomGuiDial.GetNormalizedPosition: Single;
begin
 if Max = Min
  then Result := Min
  else Result := (FPosition - Min) / (Max - Min);
end;

function TCustomGuiDial.GetGlyphNr: Integer;
begin
 Result := Trunc(MapValue(NormalizedPosition) * FGlyphCount);
end;

function TCustomGuiDial.GetMappedPosition: Single;
begin
 Result := MapValue(NormalizedPosition) * (Max - Min) + Min;
end;

function TCustomGuiDial.MapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), FCurveMappingExp)
  else Result :=  Power(Abs(Value), FCurveMappingExp);
end;

function TCustomGuiDial.UnmapValue(Value: Double): Double;
begin
 if Value < 0
  then Result := -Power(Abs(Value), 1 / FCurveMappingExp)
  else Result :=  Power(Abs(Value), 1 / FCurveMappingExp)
end;

procedure TCustomGuiDial.ReadMaxProperty(Reader: TReader);
begin
 FMax := Reader.ReadFloat;
end;

procedure TCustomGuiDial.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMax);
end;

procedure TCustomGuiDial.SetMax(const Value: Single);
begin
  if Value <> FMax then
  begin
   {$IFNDEF FPC}
   if (Value < FMin) and not (csLoading in ComponentState) then
     raise EInvalidOperation.CreateFmt(SOutOfRange, [FMin + 1, MaxInt]);
   {$ENDIF}

   FMax := Value;
   MaximumChanged;
  end;
end;

procedure TCustomGuiDial.SetMin(const Value: Single);
begin
  if Value <> FMin then
  begin
   {$IFNDEF FPC}
   if (Value > FMax) and not (csLoading in ComponentState) then
    raise EInvalidOperation.CreateFmt(SOutOfRange, [-MaxInt, FMax - 1]);
   {$ENDIF}

   FMin := Value;
  end;
end;

procedure TCustomGuiDial.MaximumChanged;
begin
 if FPosition > FMax then FPosition := FMax;
 if FDefaultPosition > FMax then FDefaultPosition := FMax;
 Invalidate;
end;

procedure TCustomGuiDial.MinimumChanged;
begin
 if FPosition < FMin then FPosition := FMin;
 if FDefaultPosition < FMin then FDefaultPosition := FMin;
 Invalidate;
end;

procedure TCustomGuiDial.SetNormalizedPosition(const Value: Single);
var
  NewValue : Double;
begin
 NewValue := Min + Value * (Max - Min);

 if Assigned(FOnQuantizeValue)
  then FOnQuantizeValue(Self, NewValue);

 Position := NewValue;
end;

procedure TCustomGuiDial.SetPosition(Value: Single);
begin
  if Value < FMin then Value := FMin else
  if Value > FMax then Value := FMax;

  if FPosition <> Value then
   begin
    FPosition := Value;
    PositionChanged;
   end;
end;

procedure TCustomGuiDial.PositionChanged;
begin
 if not (csLoading in ComponentState) and Assigned(FOnChange) then FOnChange(Self);
 Invalidate;
end;

procedure TCustomGuiDial.SetDefaultPosition(Value: Single);
begin
 if not (csLoading in ComponentState) then
  begin
   if Value < FMin then Value := FMin else
   if Value > FMax then Value := FMax;
  end;

 FDefaultPosition := Value;
end;

procedure TCustomGuiDial.SetInertia(Value: Single);
begin
 if Value < 0 then Value := 0;
 if FInertia <> Value then
  begin
   FInertia := Value;
   InertiaChanged;
  end;
end;

procedure TCustomGuiDial.InertiaChanged;
begin
 FInertiaExp := Power(2, -FInertia);
 FInertiaScale := 0.01 * Power(0.01, -FInertiaExp);
 Invalidate;
end;

function TCustomGuiDial.CircularMouseToPosition(X, Y: Integer): Single;
var
  Range: Single;
  Angle: Single;
begin
  Range := Max - (Min - 1);
  Angle := SafeAngle(RelativeAngle(Width div 2, Height div 2, X, Y) - PointerAngles.Start);
  Result := Angle * Range / PointerAngles.Range;
  while Result > Max do Result := Result - Range;
  while Result < Min do Result := Result + Range;

  if Result > Max then Result := FPosition;
  if Result < Min then Result := FPosition;
end;

procedure TCustomGuiDial.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if Enabled then
  begin
   if ssCtrl in Shift then Position := FDefaultPosition;
   if (Button = mbRight) and
      (FRightMouseButton = rmbfReset)
    then Position := FDefaultPosition;
  end;

 inherited;
end;

procedure TCustomGuiDial.DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer);
var
  Difference : Double;
  NewValue   : Double;

begin
 Difference := (MouseState.LastEventY - Y) / FScrollRange;

 // apply inertia function
 if Difference < 0
  then Difference := -Power(Abs(Difference), FInertiaExp) * FInertiaScale
  else Difference :=  Power(Abs(Difference), FInertiaExp) * FInertiaScale;

 if ssShift in Shift
  then NewValue := MapValue(NormalizedPosition) + Difference * 0.1
  else NewValue := MapValue(NormalizedPosition) + Difference;

 NormalizedPosition := UnMapValue(NewValue);

 inherited;
end;

procedure TCustomGuiDial.DragMouseMoveRight(Shift: TShiftState; X, Y: Integer);
begin
 if FRightMouseButton = rmbfCircular
  then Position := CircularMouseToPosition(x, y);
 inherited;
end;

procedure TCustomGuiDial.SetPointerAngles(const Value: TGuiDialPointerAngles);
begin
 FPointerAngles.Assign(Value);
end;

procedure TCustomGuiDial.CalcColorCircle;
begin
 if (Color and $000000FF) < $80
  then if (((Color and $0000FF00) shr 8) <$80) or (((Color and $00FF0000) shr 16)<$80) then FCircleColor:=$FFFFFF
  else if (((Color and $0000FF00) shr 8) <$80) and (((Color and $00FF0000) shr 16)<$80) then FCircleColor:=$FFFFFF;

 Invalidate;
end;

procedure TCustomGuiDial.SetAutoColor(const Value: Boolean);
begin
 CalcColorCircle;
end;

procedure TCustomGuiDial.SetCircleColor(const Value: TColor);
begin
 if not FAutoColor and (Value <> FCircleColor) then
  begin
   FCircleColor := Value;
   CircleColorChanged;
  end;
end;

procedure TCustomGuiDial.CircleColorChanged;
begin
 Invalidate;
end;

procedure TCustomGuiDial.SetCurveMapping(const Value: Single);
begin
 if FCurveMapping <> Value then
  begin
   FCurveMapping := Value;
   CurveMappingChanged;
  end;
end;

procedure TCustomGuiDial.CurveMappingChanged;
begin
 FCurveMappingExp := Power(2, FCurveMapping);
 Invalidate;
end;


{ TCustomGuiSwitch }

constructor TCustomGuiSwitch.Create(AOwner: TComponent);
begin
 inherited;
 FStringList := TStringList.Create;
 FGlyphNr := 0;
 FDefaultGlyphNr := 0;
end;

destructor TCustomGuiSwitch.Destroy;
begin
 FreeAndNil(FStringList);
 inherited;
end;

function TCustomGuiSwitch.GetGlyphNr: Integer;
begin
 Result := FGlyphNr;
end;

procedure TCustomGuiSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
 if not FReadOnly then
  begin
   if (Button = mbLeft) then
    if FGlyphNr < FGlyphCount - 1
     then GlyphNr := FGlyphNr + 1
     else GlyphNr := 0 else
   if (Button = mbRight) then
    if FGlyphNr > 0
     then GlyphNr := FGlyphNr - 1
     else GlyphNr := FGlyphCount - 1;
  end;
 inherited;
end;

procedure TCustomGuiSwitch.GlyphCountChanged;
begin
 inherited;
 if FDefaultGlyphNr >= FGlyphCount then DefaultGlyphNr := FGlyphCount - 1;
 if FGlyphNr >= FGlyphCount then GlyphNr := FGlyphCount - 1;
end;

procedure TCustomGuiSwitch.RenderBitmap(const Bitmap: TBitmap);
var
  txt : string; 
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;
   Font.Assign(Self.Font);
   Font.Size := Font.Size * OversamplingFactor;
   if FGlyphNr < FStringList.Count
    then txt := FStringList[FGlyphNr]
    else txt := IntToStr(FGlyphNr);
   TextOut((Width - TextWidth(txt)) div 2, 0, txt);
  end;
end;

procedure TCustomGuiSwitch.SetDefaultGlyphNr(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FGlyphCount then Value := FGlyphCount - 1;
 if Value <> FDefaultGlyphNr then
  begin
   FDefaultGlyphNr := Value;
  end;
end;

procedure TCustomGuiSwitch.SetGlyphNr(Value: Integer);
begin
 if Value < 0 then Value := 0 else
 if Value >= FGlyphCount then Value := FGlyphCount - 1;
 if Value <> FGlyphNr then
  begin
   FGlyphNr := Value;
   GlyphNrChanged;
  end;
end;

procedure TCustomGuiSwitch.GlyphNrChanged;
begin
 if assigned(FOnChange) and ([csLoading, csDestroying] * ComponentState = [])
  then FOnChange(Self);
 Invalidate;
end; 

procedure TCustomGuiSwitch.SetStringList(const Value: TStringList);
begin
 FStringList.Assign(Value);
 if FDialBitmap.Empty
  then GlyphCount := Max(1, FStringList.Count);
 Invalidate;
end;


{ TCustomGuiDialMetal }

procedure TCustomGuiDialMetal.RenderBitmap(const Bitmap: TBitmap);
var
  Steps, i : Integer;
  Val      : Single;
  Rad      : Single;
  Cmplx    : TComplex32;
  Pnt      : TPoint;
  XStart   : Single;
  LineFrac : Single;
  BW       : Single;
  Center   : TComplex32;
  Line     : PRGB32Array;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   // draw background
   {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
   FillRect(ClipRect);

   // draw circle
   Rad := 0.45 * Math.Min(Width, Height) - FLineWidth div 2;
   BW := 1 - OversamplingFactor / Rad; // border width = 1 pixel
   if Rad < 0 then exit;

   Center.Re := 0.5 * Width;
   Center.Im := 0.5 * Height;
   Pen.Color := FLineColor;
   Brush.Color := FCircleColor;

   {$IFNDEF FPC}
   for i := 0 to Round(2 * Rad) do
    begin
     XStart := Sqrt(Abs(Sqr(rad) - Sqr(Rad - i)));
     Line := ScanLine[Round(Center.Im - (Rad - i))];
     for Steps := Round(Center.Re - XStart) to Round(Center.Re + XStart) do
      begin
       val := 2.9999 * Abs(ArcTan2(Steps - Center.Re, (Rad - i)) / Pi);
       if Round(1.5 + val) mod 3 = 0
        then val := val * 50 - 99.5
        else val := -Round(99.5 + val * 50) mod 100;
       if Sqr(Steps - Center.Re) + Sqr(Rad - i) > Sqr(BW * Rad) then val := -$90;

       Line[Steps].B := Round(Line[Steps].B + val);
       Line[Steps].G := Round(Line[Steps].G + val);
       Line[Steps].R := Round(Line[Steps].R + val);
      end;

     GetSinCos(PositionToAngle - (PI * 0.5), Cmplx.Im, Cmplx.Re);
     Pnt := Point(Round(Center.Re + Cmplx.Re * BW * Rad), Round(Center.Im + Cmplx.Im * BW * Rad));

     //LineFrac := 0.01 * fIndLineLength;
     LineFrac := 0.5;
     Pen.Width := 3 * OversamplingFactor;
     MoveTo(Pnt.X, Pnt.Y);
     LineTo(Round((1 - LineFrac) * Pnt.X + LineFrac * Center.Re),
            Round((1 - LineFrac) * Pnt.Y + LineFrac * Center.Im));
    end;
   {$ENDIF}
  end;
end;

{ TCustomGuiDialEx }

constructor TCustomGuiDialEx.Create(AOwner: TComponent);
begin
 inherited;
 FIndLineLength := 100;
end;

procedure TCustomGuiDialEx.RenderBitmap(const Bitmap: TBitmap);
var
  Steps, i  : Integer;
  Val, Off  : TComplex64;
  Rad, tmp  : Single;
  PtsArray  : Array of TPoint;
  LineFrac  : Single;
begin
 with Bitmap, Canvas do
  begin
   Brush.Color := Self.Color;

   {$IFNDEF FPC}if FTransparent then CopyParentImage(Self, FBuffer.Canvas) else{$ENDIF}
   FillRect(ClipRect);

   Rad := 0.45 * Math.Min(Width, Height) - FLineWidth div 2;
   if Rad < 0 then exit;
   Steps := Round(2 / ArcSin(1 / Rad)) + 1;
   if Steps > 1 then
    begin
     SetLength(PtsArray, Steps);
     GetSinCos(PositionToAngle - (PI * 0.5), Val.Im, Val.Re);
     Val.Re := Val.Re * Rad; Val.Im := Val.Im * Rad;
     GetSinCos(2 * Pi / (Steps - 1), Off.Im, Off.Re);
     PtsArray[0] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));

     for i:=1 to Steps - 1 do
      begin
       tmp := Val.Re * Off.Re - Val.Im * Off.Im;
       Val.Im := Val.Im * Off.Re + Val.Re * Off.Im;
       Val.Re := tmp;
       PtsArray[i] := Point(Round(0.5 * Width + Val.Re), Round(0.5 * Height + Val.Im));
      end;

     Pen.Width := FLineWidth;
     Pen.Color := FLineColor;
     Brush.Color := FCircleColor;
     Polygon(PtsArray);
    end;

   LineFrac := 0.01 * fIndLineLength;
   MoveTo(PtsArray[0].X, PtsArray[0].Y);
   LineTo(Round((1 - LineFrac) * PtsArray[0].X + LineFrac * 0.5 * Width),
          Round((1 - LineFrac) * PtsArray[0].Y + LineFrac * 0.5 * Height));
  end;
end;

procedure TCustomGuiDialEx.SetIndLineLength(const Value: Single);
begin
 if FIndLineLength <> Value then
  begin
   FIndLineLength := Value;
   IndLineLengthChanged;
  end;
end;

procedure TCustomGuiDialEx.IndLineLengthChanged;
begin
 Invalidate
end;


{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'Dial Image List Implementation'} {$ENDIF}

{ TGuiDialImageCollection }

constructor TGuiDialImageCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiDialImageCollectionItem);
end;

function TGuiDialImageCollection.Add: TGuiDialImageCollectionItem;
begin
 Result := TGuiDialImageCollectionItem(inherited Add);
end;

procedure TGuiDialImageCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiDialImageCollection.GetItem(
  Index: Integer): TGuiDialImageCollectionItem;
begin
 Result := TGuiDialImageCollectionItem(inherited GetItem(Index));
end;

function TGuiDialImageCollection.Insert(
  Index: Integer): TGuiDialImageCollectionItem;
begin
 Result:= TGuiDialImageCollectionItem(inherited Insert(Index));
end;

procedure TGuiDialImageCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 // add things that depend on the order here!
end;

procedure TGuiDialImageCollection.SetItem(Index: Integer;
  const Value: TGuiDialImageCollectionItem);
begin
 inherited SetItem(Index, Value);
end;

{ TGuiDialImageCollectionItem }

constructor TGuiDialImageCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FGlyphCount           := 1;
 FDialBitmap          := TBitmap.Create;
 FDialBitmap.OnChange := SettingsChanged;
 FLinkedDials         := TObjectList.Create(False);
 if Collection.Owner is TGuiDialImageList then
  with FDialBitmap.Canvas, TGuiDialImageList(Collection.Owner) do
   if Owner is TForm
    then Brush.Color := TForm(Owner).Color;
end;

destructor TGuiDialImageCollectionItem.Destroy;
begin
 FreeAndNil(FDialBitmap);
 FreeAndNil(FLinkedDials);
 inherited;
end;

function TGuiDialImageCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiDialImageCollectionItem.GetHeight: Integer;
begin
 Result := FDialBitmap.Height;
end;

function TGuiDialImageCollectionItem.GetWidth: Integer;
begin
 Result := FDialBitmap.Width;
end;

procedure TGuiDialImageCollectionItem.LinkStitchedControl(Dial: TCustomGuiStitchedControl);
begin
 if FLinkedDials.IndexOf(Dial) < 0 then
  begin
   FLinkedDials.Add(Dial);
   Dial.GlyphCount := GlyphCount;
   Dial.StitchKind := StitchKind;
   case StitchKind of
    skHorizontal :
     begin
      Dial.Width  := Width div GlyphCount;
      Dial.Height := Height;
     end;
    skVertical :
     begin
      Dial.Width  := Width;
      Dial.Height := Height div GlyphCount;
     end;
   end;
  end;
end;

procedure TGuiDialImageCollectionItem.UnLinkStitchedControl(Dial: TCustomGuiStitchedControl);
begin
 FLinkedDials.Remove(Dial);
end;

procedure TGuiDialImageCollectionItem.SettingsChanged(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to FLinkedDials.Count - 1 do
  with TCustomGuiDial(FLinkedDials[i]) do
   begin
    GlyphCount := Self.GlyphCount;
    StitchKind := Self.StitchKind;
    Invalidate;
   end;
end;

procedure TGuiDialImageCollectionItem.SetWidth(const Value: Integer);
begin
 if Value < 0 then exit;
 FDialBitmap.Width := Value;
end;

procedure TGuiDialImageCollectionItem.SetDialBitmap(const Value: TBitmap);
begin
 FDialBitmap.Assign(Value);
end;

procedure TGuiDialImageCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiDialImageCollectionItem.SetHeight(const Value: Integer);
begin
 if Value < 0 then Exit;
 FDialBitmap.Height := Value;
end;

procedure TGuiDialImageCollectionItem.GlyphCountChanged;
var
  i : Integer;
begin
 for i := 0 to FLinkedDials.Count - 1
  do TCustomGuiDial(FLinkedDials[i]).GlyphCount := GlyphCount;
end;

procedure TGuiDialImageCollectionItem.StitchKindChanged;
var
  i : Integer;
begin
 for i := 0 to FLinkedDials.Count - 1
  do TCustomGuiDial(FLinkedDials[i]).StitchKind := StitchKind;
end;

procedure TGuiDialImageCollectionItem.SetGlyphCount(const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrGlyphCountMustBePositive);

 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   GlyphCountChanged;
  end;
end;

procedure TGuiDialImageCollectionItem.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   StitchKindChanged;
  end;
end;

{ TGuiDialImageList }

constructor TGuiDialImageList.Create(AOwner: TComponent);
begin
  inherited;
  FDialImageCollection := TGuiDialImageCollection.Create(Self);
end;

destructor TGuiDialImageList.Destroy;
begin
  FreeAndNil(FDialImageCollection);
  inherited;
end;

function TGuiDialImageList.GetCount: Integer;
begin
  Result := FDialImageCollection.Count;
end;

function TGuiDialImageList.GetItems(Index: Integer): TGuiDialImageCollectionItem;
begin
 if (Index >= 0) and (Index < FDialImageCollection.Count)
  then Result := FDialImageCollection[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

{$IFDEF DELPHI10_UP} {$endregion } {$ENDIF}

end.
