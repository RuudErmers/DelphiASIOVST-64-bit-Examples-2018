unit DAV_GuiVectorPixel;

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
  {$IFDEF FPC} LCLIntf, LCLType, LResources, {$ELSE} Windows, Messages, {$ENDIF}
  Graphics, Classes, SysUtils, DAV_Common, DAV_MemoryUtils, DAV_GuiCommon,
  DAV_GuiBlend, DAV_GuiPixelMap, DAV_GuiVector, DAV_FixedPoint;

type
  TGuiPixelPrimitiveDraw = procedure(PixelMap: TGuiCustomPixelMap) of object;
  TGuiFillEvent = procedure(Sender: TObject; const X, Y: Integer;
    var Pixel: TPixel32) of object;

  TCustomGuiPixelPrimitive = class(TPersistent)
  protected
    FGeometricShape : TGuiCustomGeometricShape;
    FOnChanged      : TNotifyEvent;
    FDrawDraft      : TGuiPixelPrimitiveDraw;
    FDraw           : TGuiPixelPrimitiveDraw;
    procedure Changed; virtual;
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Draw: TGuiPixelPrimitiveDraw read FDraw;
    property DrawDraft: TGuiPixelPrimitiveDraw read FDrawDraft;
    property GeometricShape: TGuiCustomGeometricShape read FGeometricShape;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;
  TCustomGuiPixelPrimitiveClass = class of TCustomGuiPixelPrimitive;

  TCustomGuiPixelSimplePrimitive = class(TCustomGuiPixelPrimitive)
  private
    FColor: TColor;
    FAlpha: Byte;
    procedure SetAlpha(const Value: Byte);
    procedure SetColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AlphaChanged; virtual;
    procedure ColorChanged; virtual;
  public
    property Color: TColor read FColor write SetColor;
    property Alpha: Byte read FAlpha write SetAlpha;
  end;

  TCustomGuiPixelFramePrimitive = class(TCustomGuiPixelSimplePrimitive)
  private
    FLineWidth : TFixed24Dot8;
    procedure SetLineWidth(const Value: TFixed24Dot8);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LineWidthChanged; virtual;
  public
    property LineWidth: TFixed24Dot8 read FLineWidth write SetLineWidth;
  end;

  TCustomGuiPixelFillPrimitive = class(TCustomGuiPixelSimplePrimitive)
  private
    FOnGetFillColor : TGuiFillEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnGetFillColor: TGuiFillEvent read FOnGetFillColor write FOnGetFillColor;
  end;

  TGuiPixelFilledEllipse = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiEllipse;
  protected
    procedure DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiEllipse read GetGeometricShape;
  end;

  TGuiPixelFrameEllipse = class(TCustomGuiPixelFillPrimitive)
  private
    function GetGeometricShape: TGuiEllipse;
  protected
    procedure DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
    procedure DrawFixedPoint(PixelMap: TGuiCustomPixelMap); override;
    procedure DrawDraftShape(PixelMap: TGuiCustomPixelMap); override;
  public
    constructor Create; override;

    property GeometricShape: TGuiEllipse read GetGeometricShape;
  end;

implementation

uses
  DAV_Complex, DAV_Approximations;


{ TCustomGuiPixelPrimitive }

procedure TCustomGuiPixelPrimitive.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiPixelPrimitive then
  with TCustomGuiPixelPrimitive(Dest) do
   begin
    GeometricShape.Assign(Self.GeometricShape);
    FOnChanged := Self.FOnChanged;
   end
 else inherited;
end;

procedure TCustomGuiPixelPrimitive.Changed;
begin
 if Assigned(FOnChanged)
  then FOnChanged(Self);
end;


constructor TCustomGuiPixelPrimitive.Create;
begin
 inherited;
 FDrawDraft := DrawDraftShape;
 FDraw := DrawFixedPoint;
end;

destructor TCustomGuiPixelPrimitive.Destroy;
begin
 FreeAndNil(FGeometricShape);
 inherited;
end;

{ TCustomGuiPixelSimplePrimitive }

procedure TCustomGuiPixelSimplePrimitive.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TCustomGuiPixelSimplePrimitive.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;

procedure TCustomGuiPixelSimplePrimitive.AlphaChanged;
begin
 Changed;
end;

procedure TCustomGuiPixelSimplePrimitive.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TCustomGuiPixelSimplePrimitive then
  with TCustomGuiPixelSimplePrimitive(Dest) do
   begin
    FColor := Self.Color;
    FAlpha := Self.Alpha;
   end;
end;

procedure TCustomGuiPixelSimplePrimitive.ColorChanged;
begin
 Changed;
end;



{ TCustomGuiPixelFramePrimitive }

procedure TCustomGuiPixelFramePrimitive.LineWidthChanged;
begin
 Changed;
end;

procedure TCustomGuiPixelFramePrimitive.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TCustomGuiPixelFramePrimitive then
  with TCustomGuiPixelFramePrimitive(Dest) do
   begin
    FLineWidth := Self.FLineWidth;
   end;
end;

procedure TCustomGuiPixelFramePrimitive.SetLineWidth(
  const Value: TFixed24Dot8);
begin
 if FLineWidth.Fixed <> Value.Fixed then
  begin
   FLineWidth.Fixed := Value.Fixed;
   LineWidthChanged;
  end;
end;


{ TCustomGuiPixelFillPrimitive }

procedure TCustomGuiPixelFillPrimitive.AssignTo(Dest: TPersistent);
begin
 inherited;

 if Dest is TCustomGuiPixelFillPrimitive then
  with TCustomGuiPixelFillPrimitive(Dest) do
   begin
    FOnGetFillColor := Self.OnGetFillColor;
   end;
end;


{ TGuiPixelFilledEllipse }

constructor TGuiPixelFilledEllipse.Create;
begin
 inherited;
 FGeometricShape := TGuiEllipse.Create;
 FDraw := DrawFloatingPoint;
end;

function TGuiPixelFilledEllipse.GetGeometricShape: TGuiEllipse;
begin
 Result := TGuiEllipse(FGeometricShape);
end;

procedure TGuiPixelFilledEllipse.DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
var
  X, Y           : Integer;
  ScnLne         : PPixel32Array;
  PixelColor32   : TPixel32;
  CombColor      : TPixel32;
  Rad            : TComplex32;
  Center         : TComplex32;
  XStart         : Single;
  YRange         : array [0..1] of Integer;
  XRange         : array [0..1] of Integer;
  SqrYDist       : Single;
  SqrDist        : Single;
  SqrBorderDist  : Single;
  SqrRadMinusOne : TComplex32;
  SqrRadRatio    : array [0..1] of Single;
begin
 with PixelMap do
  begin
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   // transfer the GeometricShape data to local variables
   with GeometricShape do
    begin
     Rad.Re := ConvertFromFixed24Dot8(RadiusX) + 1;
     Rad.Im := ConvertFromFixed24Dot8(RadiusY) + 1;
     if (Rad.Re <= 0) or (Rad.Im <= 0) then Exit;
     SqrRadRatio[0] := Sqr(Rad.Re / Rad.Im);

     Center.Re := ConvertFromFixed24Dot8(CenterX);
     Center.Im := ConvertFromFixed24Dot8(CenterY);
    end;

   // calculate affected scanlines
   YRange[0] := Round(Center.Im - Rad.Im);
   YRange[1] := Round(Center.Im + Rad.Im);

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if Center.Re - Rad.Re >= Width then Exit;
   if Center.Re + Rad.Re < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   SqrRadMinusOne.Re := Sqr(BranchlessClipPositive(Rad.Re - 1));
   SqrRadMinusOne.Im := Sqr(BranchlessClipPositive(Rad.Im - 1));
   SqrRadRatio[1] := (SqrRadMinusOne.Re / SqrRadMinusOne.Im);

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Rad.Re) - SqrYDist * SqrRadRatio[0];
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;

     // calculate affected pixels within this scanline
     XRange[0] := Round(Center.Re - XStart);
     XRange[1] := Round(Center.Re + XStart);

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1] do
      begin
       // calculate squared distance
       SqrDist := Sqr(X - Center.Re) + SqrYDist;

       CombColor := PixelColor32;

       XStart := SqrRadMinusOne.Re - SqrYDist * SqrRadRatio[1];
       SqrBorderDist := XStart + SqrYDist;

       if SqrDist >= SqrBorderDist then
        begin
         if SqrBorderDist > 0 then
          begin
           XStart := 1 - (FastSqrtBab2(SqrDist) - FastSqrtBab2(SqrBorderDist));
           if XStart < 0 then XStart := 0;
           CombColor.A := Round(XStart * CombColor.A);
          end;
        end;
       BlendPixelInplace(CombColor, ScnLne[X]);
       EMMS;
      end;

    end;
  end;
end;

procedure TGuiPixelFilledEllipse.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
var
  X, Y         : Integer;
  ScnLne       : PPixel32Array;
  PixelColor32 : TPixel32;
  Rad          : TComplex32;
  Center       : TComplex32;
  SqrRadRatio  : Single;
  XStart       : Single;
  YRange       : array [0..1] of Integer;
  XRange       : array [0..1] of Integer;
  SqrYDist     : Single;
begin
 with PixelMap do
  begin
   PixelColor32 := ConvertColor(Color);
   PixelColor32.A := Alpha;

   // transfer the GeometricShape data to local variables
   with GeometricShape do
    begin
     Rad.Re := ConvertFromFixed24Dot8(RadiusX) + 1;
     Rad.Im := ConvertFromFixed24Dot8(RadiusY) + 1;
     if (Rad.Re <= 0) or (Rad.Im <= 0) then Exit;
     SqrRadRatio := Sqr(Rad.Re / Rad.Im);

     Center.Re := ConvertFromFixed24Dot8(CenterX);
     Center.Im := ConvertFromFixed24Dot8(CenterY);
    end;

   // calculate affected scanlines
   YRange[0] := Round(Center.Im - Rad.Im);
   YRange[1] := Round(Center.Im + Rad.Im);

   // check whether the bitmap needs to be drawn at all
   if YRange[0] >= Height then Exit;
   if YRange[1] < 0 then Exit;
   if Center.Re - Rad.Re >= Width then Exit;
   if Center.Re + Rad.Re < 0 then Exit;

   // eventually limit range
   if YRange[0] < 0 then YRange[0] := 0;
   if YRange[1] >= Height then YRange[1] := Height - 1;

   for Y := YRange[0] to YRange[1] do
    begin
     // calculate squared vertical distance
     SqrYDist := Sqr(Y - Center.Im);

     XStart := Sqr(Rad.Re) - SqrYDist * SqrRadRatio;
     if XStart <= 0
      then Continue
      else XStart := Sqrt(XStart) - 0.5;

     // calculate affected pixels within this scanline
     XRange[0] := Round(Center.Re - XStart);
     XRange[1] := Round(Center.Re + XStart);

     // eventually limit range
     if XRange[0] < 0 then XRange[0] := 0;
     if XRange[1] >= Width then XRange[1] := Width - 1;

     ScnLne := Scanline[Y];
     for X := XRange[0] to XRange[1]
      do BlendPixelInplace(PixelColor32, ScnLne[X]);
     EMMS;
    end;
  end;
end;

procedure TGuiPixelFilledEllipse.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin

end;


{ TGuiPixelFrameEllipse }

constructor TGuiPixelFrameEllipse.Create;
begin
 inherited;
 FGeometricShape := TGuiEllipse.Create;
 FDraw := DrawFloatingPoint;
end;

function TGuiPixelFrameEllipse.GetGeometricShape: TGuiEllipse;
begin
 Result := TGuiEllipse(FGeometricShape);
end;

procedure TGuiPixelFrameEllipse.DrawDraftShape(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiPixelFrameEllipse.DrawFixedPoint(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiPixelFrameEllipse.DrawFloatingPoint(PixelMap: TGuiCustomPixelMap);
begin

end;

end.
