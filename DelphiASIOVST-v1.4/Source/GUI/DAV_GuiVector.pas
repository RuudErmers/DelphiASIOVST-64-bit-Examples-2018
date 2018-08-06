unit DAV_GuiVector;

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
  {$IFDEF FPC} LCLIntf, LCLType, {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_FixedPoint, DAV_GuiFixedPoint;

type
  TGuiCustomGeometricShape = class(TPersistent)
  private
    FOnChange : TNotifyEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
  public
    constructor Create; virtual; abstract;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiCustomCenteredGeometricShape = class(TGuiCustomGeometricShape)
  private
    FCenter : TFixed24Dot8Point;
    procedure SetCenterX(const Value: TFixed24Dot8);
    procedure SetCenterY(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CenterXChanged; virtual;
    procedure CenterYChanged; virtual;
  public
    constructor Create; override;

    property CenterX: TFixed24Dot8 read FCenter.X write SetCenterX;
    property CenterY: TFixed24Dot8 read FCenter.Y write SetCenterY;
  end;

  TGuiCustomCircle = class(TGuiCustomCenteredGeometricShape)
  private
    FRadius  : TFixed24Dot8;
    procedure SetRadius(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure RadiusChanged; virtual;
  public
    constructor Create; override;

    property Radius: TFixed24Dot8 read FRadius write SetRadius;
  end;
  TGuiCircle = class(TGuiCustomCircle);

  TGuiCustomCircleSector = class(TGuiCustomCircle)
  private
    FAngleStart : TFixed24Dot8;
    FAngleEnd   : TFixed24Dot8;
    procedure SetAngleEnd(const Value: TFixed24Dot8);
    procedure SetAngleStart(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AngleStartChanged; virtual;
    procedure AngleEndChanged; virtual;
  public
    constructor Create; override;

    property AngleStart: TFixed24Dot8 read FAngleStart write SetAngleStart;
    property AngleEnd: TFixed24Dot8 read FAngleEnd write SetAngleEnd;
  end;
  TGuiCircleSector = class(TGuiCustomCircleSector);

  TGuiCustomEllipse = class(TGuiCustomCenteredGeometricShape)
  private
    FRadiusX  : TFixed24Dot8;
    FRadiusY  : TFixed24Dot8;
    procedure SetRadiusX(const Value: TFixed24Dot8);
    procedure SetRadiusY(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure RadiusXChanged; virtual;
    procedure RadiusYChanged; virtual;
  public
    constructor Create; override;

    property RadiusX: TFixed24Dot8 read FRadiusX write SetRadiusX;
    property RadiusY: TFixed24Dot8 read FRadiusY write SetRadiusY;
  end;
  TGuiEllipse = class(TGuiCustomEllipse);

  TGuiCustomRectangle = class(TGuiCustomGeometricShape)
  private
    FRight  : TFixed24Dot8;
    FBottom : TFixed24Dot8;
    FTop    : TFixed24Dot8;
    FLeft   : TFixed24Dot8;
    procedure SetBottom(const Value: TFixed24Dot8);
    procedure SetLeft(const Value: TFixed24Dot8);
    procedure SetRight(const Value: TFixed24Dot8);
    procedure SetTop(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BottomChanged; virtual;
    procedure LeftChanged; virtual;
    procedure RightChanged; virtual;
    procedure TopChanged; virtual;
  public
    constructor Create; override;

    property Left: TFixed24Dot8 read FLeft write SetLeft;
    property Right: TFixed24Dot8 read FRight write SetRight;
    property Top: TFixed24Dot8 read FTop write SetTop;
    property Bottom: TFixed24Dot8 read FBottom write SetBottom;
  end;
  TGuiRectangle = TGuiCustomRectangle;

  TGuiCustomRoundedRectangle = class(TGuiCustomRectangle)
  private
    FBorderRadius: TFixed24Dot8;
    procedure SetBorderRadius(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BorderRadiusChanged; virtual;
  public
    constructor Create; override;

    property BorderRadius: TFixed24Dot8 read FBorderRadius write SetBorderRadius;
  end;
  TGuiRoundedRectangle = class(TGuiCustomRoundedRectangle);

  TGuiCustomLine = class(TGuiCustomGeometricShape)
  private
    FXA: TFixed24Dot8;
    FYA: TFixed24Dot8;
    FXB: TFixed24Dot8;
    FYB: TFixed24Dot8;
    procedure SetXA(const Value: TFixed24Dot8);
    procedure SetXB(const Value: TFixed24Dot8);
    procedure SetYA(const Value: TFixed24Dot8);
    procedure SetYB(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure XAChanged; virtual;
    procedure YAChanged; virtual;
    procedure XBChanged; virtual;
    procedure YBChanged; virtual;
  public
    constructor Create; override;

    property XA: TFixed24Dot8 read FXA write SetXA;
    property YA: TFixed24Dot8 read FYA write SetYA;
    property XB: TFixed24Dot8 read FXB write SetXB;
    property YB: TFixed24Dot8 read FYB write SetYB;
  end;
  TGuiLine = class(TGuiCustomLine);

  TFixed24Dot8Point = record
    X, Y : TFixed24Dot8;
  end;

  TFixed16Dot16Point = record
    X, Y : TFixed16Dot16;
  end;

  TGetValueEvent = function (Sender: TObject; PixelPosition: Integer): TFixed24Dot8 of object;

  TGuiEquallySpacedPolyline = class(TGuiCustomGeometricShape)
  private
    FOnGetValue   : TGetValueEvent;
    FMarginBottom : Integer;
    FMarginTop    : Integer;
    FMarginLeft   : Integer;
    FMarginRight  : Integer;
    procedure SetMarginBottom(const Value: Integer);
    procedure SetMarginLeft(const Value: Integer);
    procedure SetMarginRight(const Value: Integer);
    procedure SetMarginTop(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure MarginChanged; virtual;
  public
    constructor Create; override;
    procedure SetAllMargins(Value: Integer);

    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
    property MarginBottom: Integer read FMarginBottom write SetMarginBottom;
    property OnGetValue: TGetValueEvent read FOnGetValue write FOnGetValue;
  end;

  TGuiCustomPolygon = class(TGuiCustomGeometricShape)
  private
    FData : array of TFixed24Dot8Point;
    function GetX(Index: Integer): TFixed24Dot8;
    function GetY(Index: Integer): TFixed24Dot8;
    procedure SetX(Index: Integer; const Value: TFixed24Dot8);
    procedure SetY(Index: Integer; const Value: TFixed24Dot8);
    function GetCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddPoint(X, Y: TFixed24Dot8); overload;
    procedure AddPoint(Point: TFixed24Dot8Point); overload;
(*
    procedure RemovePoint(X, Y: TFixed24Dot8); overload;
    procedure RemovePoint(Point: TFixed24Dot8Point); overload;
*)
    procedure RemovePoint(Index: Integer); overload;

    property X[Index: Integer]: TFixed24Dot8 read GetX write SetX;
    property Y[Index: Integer]: TFixed24Dot8 read GetY write SetY;
    property Count: Integer read GetCount;
  end;
  TGuiPolygon = class(TGuiCustomPolygon);

implementation

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds %d';

{ TGuiCustomGeometricShape }

procedure TGuiCustomGeometricShape.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiCustomGeometricShape then
  with TGuiCustomGeometricShape(Dest) do
   begin
    FOnChange := Self.FOnChange;
   end
 else inherited;
end;

procedure TGuiCustomGeometricShape.Changed;
begin
 if Assigned(FOnChange)
  then FOnChange(Self);
end;


{ TGuiCustomCenteredGeometricShape }

constructor TGuiCustomCenteredGeometricShape.Create;
begin
 inherited;
 FCenter.X.Fixed := 0;
 FCenter.Y.Fixed := 0;
end;

procedure TGuiCustomCenteredGeometricShape.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomCenteredGeometricShape then
  with TGuiCustomCenteredGeometricShape(Dest) do
   begin
    FCenter.X := Self.FCenter.X;
    FCenter.Y := Self.FCenter.Y;
   end;
end;

procedure TGuiCustomCenteredGeometricShape.CenterXChanged;
begin
 Changed;
end;

procedure TGuiCustomCenteredGeometricShape.CenterYChanged;
begin
 Changed;
end;


procedure TGuiCustomCenteredGeometricShape.SetCenterX(const Value: TFixed24Dot8);
begin
 if FCenter.X.Fixed <> Value.Fixed then
  begin
   FCenter.X := Value;
   CenterXChanged;
  end;
end;

procedure TGuiCustomCenteredGeometricShape.SetCenterY(const Value: TFixed24Dot8);
begin
 if (FCenter.Y.Fixed <> Value.Fixed) then
  begin
   FCenter.Y := Value;
   CenterYChanged;
  end;
end;


{ TGuiCustomCircle }

constructor TGuiCustomCircle.Create;
begin
 inherited;
 FRadius := CFixed24Dot8One;
end;

procedure TGuiCustomCircle.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomCircle then
  with TGuiCustomCircle(Dest) do
   begin
    FRadius := Self.FRadius;
   end;
end;

procedure TGuiCustomCircle.RadiusChanged;
begin
 Changed;
end;

procedure TGuiCustomCircle.SetRadius(const Value: TFixed24Dot8);
begin
 if (FRadius.Fixed <> Value.Fixed) then
  begin
   FRadius := Value;
   RadiusChanged;
  end;
end;


{ TGuiCustomCircleSector }

constructor TGuiCustomCircleSector.Create;
begin
 inherited;

 FAngleStart.Fixed := 0;
 FAngleEnd := CFixed24Dot8PI;
end;

procedure TGuiCustomCircleSector.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TGuiCustomCircleSector then
  with TGuiCustomCircleSector(Dest) do
   begin
    FAngleStart := Self.FAngleStart;
    FAngleEnd := Self.FAngleEnd;
   end;
end;

procedure TGuiCustomCircleSector.AngleEndChanged;
begin
 Changed;
end;

procedure TGuiCustomCircleSector.AngleStartChanged;
begin
 Changed;
end;

procedure TGuiCustomCircleSector.SetAngleEnd(const Value: TFixed24Dot8);
begin
 if FAngleEnd.Fixed <> Value.Fixed then
  begin
   FAngleEnd := Value;
   AngleEndChanged;
  end;
end;

procedure TGuiCustomCircleSector.SetAngleStart(const Value: TFixed24Dot8);
begin
 if FAngleStart.Fixed <> Value.Fixed then
  begin
   FAngleStart := Value;
   AngleStartChanged;
  end;
end;


{ TGuiCustomEllipse }

constructor TGuiCustomEllipse.Create;
begin
 inherited;
 FRadiusX := CFixed24Dot8One;
 FRadiusY := CFixed24Dot8One;
end;

procedure TGuiCustomEllipse.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomEllipse then
  with TGuiCustomEllipse(Dest) do
   begin
    FRadiusX := Self.FRadiusX;
    FRadiusY := Self.FRadiusY;
   end;
end;

procedure TGuiCustomEllipse.RadiusXChanged;
begin
 Changed;
end;

procedure TGuiCustomEllipse.RadiusYChanged;
begin
 Changed;
end;

procedure TGuiCustomEllipse.SetRadiusX(const Value: TFixed24Dot8);
begin
 if (FRadiusX.Fixed <> Value.Fixed) then
  begin
   FRadiusX := Value;
   RadiusXChanged;
  end;
end;

procedure TGuiCustomEllipse.SetRadiusY(const Value: TFixed24Dot8);
begin
 if (FRadiusY.Fixed <> Value.Fixed) then
  begin
   FRadiusY := Value;
   RadiusYChanged;
  end;
end;


{ TGuiCustomRectangle }

constructor TGuiCustomRectangle.Create;
begin
 inherited;
 FLeft.Fixed := 0;
 FTop.Fixed := 0;
 FRight := CFixed24Dot8One;
 FBottom := CFixed24Dot8One;
end;

procedure TGuiCustomRectangle.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomRectangle then
  with TGuiCustomRectangle(Dest) do
   begin
    FRight  := Self.FRight;
    FBottom := Self.FBottom;
    FTop    := Self.FTop;
    FLeft   := Self.FLeft;
   end;
end;

procedure TGuiCustomRectangle.BottomChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.LeftChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.RightChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.TopChanged;
begin
 Changed;
end;

procedure TGuiCustomRectangle.SetBottom(const Value: TFixed24Dot8);
begin
 if FBottom.Fixed <> Value.Fixed then
  begin
   FBottom := Value;
   BottomChanged;
  end;
end;

procedure TGuiCustomRectangle.SetLeft(const Value: TFixed24Dot8);
begin
 if FLeft.Fixed <> Value.Fixed then
  begin
   FLeft := Value;
   LeftChanged;
  end;
end;

procedure TGuiCustomRectangle.SetRight(const Value: TFixed24Dot8);
begin
 if FRight.Fixed <> Value.Fixed then
  begin
   FRight := Value;
   RightChanged;
  end;
end;

procedure TGuiCustomRectangle.SetTop(const Value: TFixed24Dot8);
begin
 if FTop.Fixed <> Value.Fixed then
  begin
   FTop := Value;
   TopChanged;
  end;
end;


{ TGuiCustomRoundedRectangle }

procedure TGuiCustomRoundedRectangle.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomRoundedRectangle then
  with TGuiCustomRoundedRectangle(Dest) do
   begin
    FBorderRadius := Self.FBorderRadius;
   end;
end;

procedure TGuiCustomRoundedRectangle.BorderRadiusChanged;
begin
 Changed;
end;

constructor TGuiCustomRoundedRectangle.Create;
begin
 inherited;
 FBorderRadius := CFixed24Dot8One;
end;

procedure TGuiCustomRoundedRectangle.SetBorderRadius(
  const Value: TFixed24Dot8);
begin
 if FBorderRadius.Fixed <> Value.Fixed then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;


{ TGuiCustomLine }

constructor TGuiCustomLine.Create;
begin
 inherited;
 FXA.Fixed := 0;
 FYA.Fixed := 0;
 FXB.Fixed := 0;
 FYB.Fixed := 0;
end;

procedure TGuiCustomLine.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomLine then
  with TGuiCustomLine(Dest) do
   begin
    FXA := Self.FXA;
    FYA := Self.FYA;
    FXB := Self.FXB;
    FYB := Self.FYB;
   end;
end;

procedure TGuiCustomLine.XAChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.XBChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.YAChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.YBChanged;
begin
 Changed;
end;

procedure TGuiCustomLine.SetXA(const Value: TFixed24Dot8);
begin
 if FXA.Fixed <> Value.Fixed then
  begin
   FXA := Value;
   XAChanged;
  end;
end;

procedure TGuiCustomLine.SetXB(const Value: TFixed24Dot8);
begin
 if FXB.Fixed <> Value.Fixed then
  begin
   FXB := Value;
   XBChanged;
  end;
end;

procedure TGuiCustomLine.SetYA(const Value: TFixed24Dot8);
begin
 if FYA.Fixed <> Value.Fixed then
  begin
   FYA := Value;
   YAChanged;
  end;
end;

procedure TGuiCustomLine.SetYB(const Value: TFixed24Dot8);
begin
 if FYB.Fixed <> Value.Fixed then
  begin
   FYB := Value;
   YBChanged;
  end;
end;


{ TGuiEquallySpacedPolyline }

constructor TGuiEquallySpacedPolyline.Create;
begin
  inherited;
end;

procedure TGuiEquallySpacedPolyline.MarginChanged;
begin
 Changed;
end;

procedure TGuiEquallySpacedPolyline.SetAllMargins(Value: Integer);
begin
 if (FMarginTop <> Value) or (FMarginLeft <> Value) or
   (FMarginRight <> Value) or (FMarginBottom <> Value) then
  begin
   FMarginTop := Value;
   FMarginLeft := Value;
   FMarginRight := Value;
   FMarginBottom := Value;
   MarginChanged;
  end;
end;

procedure TGuiEquallySpacedPolyline.SetMarginBottom(const Value: Integer);
begin
 if FMarginBottom <> Value then
  begin
   FMarginBottom := Value;
   MarginChanged;
  end;
end;

procedure TGuiEquallySpacedPolyline.SetMarginLeft(const Value: Integer);
begin
 if FMarginLeft <> Value then
  begin
   FMarginLeft := Value;
   MarginChanged;
  end;
end;

procedure TGuiEquallySpacedPolyline.SetMarginRight(const Value: Integer);
begin
 if FMarginRight <> Value then
  begin
   FMarginRight := Value;
   MarginChanged;
  end;
end;

procedure TGuiEquallySpacedPolyline.SetMarginTop(const Value: Integer);
begin
 if FMarginTop <> Value then
  begin
   FMarginTop := Value;
   MarginChanged;
  end;
end;

procedure TGuiEquallySpacedPolyline.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiEquallySpacedPolyline then
  with TGuiEquallySpacedPolyline(Dest) do
   begin
    FOnGetValue := Self.FOnGetValue;
   end;
end;


{ TGuiCustomPolygon }

constructor TGuiCustomPolygon.Create;
begin
 inherited;
end;

destructor TGuiCustomPolygon.Destroy;
begin
 inherited;
end;

procedure TGuiCustomPolygon.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TGuiCustomPolygon then
  with TGuiCustomPolygon(Dest) do
   begin
    SetLength(FData, Length(Self.FData));
    if Length(Self.FData) > 0
     then Move(Self.FData[0], FData[0], Length(Self.FData) * SizeOf(TFixed24Dot8Point));
   end;
end;

procedure TGuiCustomPolygon.AddPoint(X, Y: TFixed24Dot8);
begin
 SetLength(FData, Length(FData) + 1);
 FData[Length(FData) - 1].X := X;
 FData[Length(FData) - 1].Y := Y;
end;

procedure TGuiCustomPolygon.AddPoint(Point: TFixed24Dot8Point);
begin
 SetLength(FData, Length(FData) + 1);
 FData[Length(FData) - 1].X := Point.X;
 FData[Length(FData) - 1].Y := Point.Y;
end;

function TGuiCustomPolygon.GetCount: Integer;
begin
 Result := Length(FData);
end;

function TGuiCustomPolygon.GetX(Index: Integer): TFixed24Dot8;
begin
 if Index < Length(FData)
  then Result := FData[Index].X
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TGuiCustomPolygon.GetY(Index: Integer): TFixed24Dot8;
begin
 if Index < Length(FData)
  then Result := FData[Index].Y
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiCustomPolygon.RemovePoint(Index: Integer);
begin
 if Index >= Length(FData)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
   begin
    if Index + 1 < Length(FData)
     then Move(FData[Index + 1], FData[Index], (Length(FData) - Index - 1) *
       SizeOf(TFixed24Dot8Point));
    SetLength(FData, Length(FData) - 1);
   end;
end;

procedure TGuiCustomPolygon.SetX(Index: Integer;
  const Value: TFixed24Dot8);
begin
 if Index < Length(FData)
  then FData[Index].X := Value
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TGuiCustomPolygon.SetY(Index: Integer;
  const Value: TFixed24Dot8);
begin
 if Index < Length(FData)
  then FData[Index].Y := Value
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

end.
