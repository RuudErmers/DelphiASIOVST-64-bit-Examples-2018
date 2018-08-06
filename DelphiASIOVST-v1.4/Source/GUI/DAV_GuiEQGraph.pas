unit DAV_GuiEQGraph;

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
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, Types, SysUtils, Controls, DAV_Types, DAV_FixedPoint,
  DAV_GuiCommon, DAV_GuiBaseControl, DAV_GuiCustomControl, DAV_GuiPixelMap,
  DAV_GuiFont, DAV_GuiShadow, DAV_GuiVector, DAV_GuiVectorPixelGraph;

type
  TGuiEQGraph = class;
  TXAxisLabelPosition = (xlpNone, xlpTop, xlpBottom);
  TYAxisLabelPosition = (ylpNone, ylpLeft, ylpRight);
  TXAxisLabelFrequency = (xlfDecade, xlfThirdDecade, xlfAuto);
  TUnitPosition = (upValue, upSide);

  TMouseAction = (maShift, maZoom);
  TMouseActions = set of TMouseAction;

  TGetFilterGainEvent = function(Sender: TObject; const Frequency: Single): Single of object;

  TCustomGuiEQGraph = class;

  TCustomGuiEQGraphAxis = class(TPersistent)
  private
    FMouseActions    : TMouseActions;
    procedure SetUnitPosition(const Value: TUnitPosition);
  protected
    FOwner           : TCustomGuiEQGraph;
    FUpper           : Single;
    FLower           : Single;
    FRange           : Single;
    FRangeReciprocal : Single;
    FUnitPosition    : TUnitPosition;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; virtual;
    procedure CalculateRange;
    procedure RangeChanged; virtual;
    procedure UnitPositionChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); virtual;

    procedure ShiftByPixel(Pixel: Integer); virtual; abstract;
    procedure ZoomAroundPixel(Pixel: Integer; Scale: Single); virtual; abstract;
    procedure ZoomInByPixel(StartPixel, EndPixel: Integer); virtual; abstract;
    procedure ZoomLowerByPixel(StartPixel, EndPixel: Integer); virtual; abstract;

    property Range: Single read FRange;
    property RangeReciprocal: Single read FRangeReciprocal;
    property MouseActions: TMouseActions read FMouseActions write FMouseActions;
    property UnitPosition: TUnitPosition read FUnitPosition write SetUnitPosition default upValue;
  end;

  // X-Axis

  TCustomGuiEQGraphXAxis = class(TCustomGuiEQGraphAxis)
  private
    FLabelPosition  : TXAxisLabelPosition;
    FLabelFrequency : TXAxisLabelFrequency;
    FInvUpper       : Single;
    FInvLower       : Single;
    FLog2Ratio      : Single;
    FInvLog2Ratio   : Single;
    procedure SetLabelPosition(const Value: TXAxisLabelPosition);
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);
    procedure SetLabelFrequency(const Value: TXAxisLabelFrequency);

    // property filer
    procedure ReadLowerProperty(Reader: TReader);
    procedure ReadUpperProperty(Reader: TReader);
    procedure WriteLowerProperty(Writer: TWriter);
    procedure WriteUpperProperty(Writer: TWriter);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure LabelPositionChanged; virtual;
    procedure LabelFrequencyChanged; virtual;
    procedure LowerFrequencyChanged; virtual;
    procedure UpperFrequencyChanged; virtual;

    procedure CalculateLowerFrequencyReciprocal;
    procedure CalculateUpperFrequencyReciprocal;
    procedure CalculateFrequencyRangeRatios;

    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); override;

    // conversion between logarithmic frequency and linear
    function LinearToLogarithmicFrequency(Value: Double): Double;
    function LogarithmicFrequencyToLinear(Value: Double): Double;

    // conversion between linear and logarithmic frequency
    function FastLinearToLogarithmicFrequency(Value: Single): Single;
    function FastLogarithmicFrequencyToLinear(Value: Single): Single;

    function FrequencyAtPixel(Pixel: Integer): Single;

    procedure ShiftByPixel(Pixel: Integer); override;
    procedure ZoomAroundPixel(Pixel: Integer; Scale: Single); override;
    procedure ZoomInByPixel(StartPixel, EndPixel: Integer); override;
    procedure ZoomLowerByPixel(StartPixel, EndPixel: Integer); override;

    property UpperFrequency: Single read FUpper write SetUpperFrequency;
    property LowerFrequency: Single read FLower write SetLowerFrequency;
    property LabelPosition: TXAxisLabelPosition read FLabelPosition write SetLabelPosition default xlpNone;
    property LabelFrequency: TXAxisLabelFrequency read FLabelFrequency write SetLabelFrequency default xlfDecade;
  end;

  TGuiEQGraphXAxis = class(TCustomGuiEQGraphXAxis)
  published
    property LabelPosition;
    property UnitPosition;
    property UpperFrequency;
    property LowerFrequency;
    property LabelFrequency;
  end;

  // Y-Axis

  TCustomGuiEQGraphYAxis = class(TCustomGuiEQGraphAxis)
  private
    FLabelPosition    : TYAxisLabelPosition;
    FMaximumGridLines : Integer;
    FAutoGranularity  : Boolean;
    procedure SetAutoGranularity(const Value: Boolean);
    procedure SetGranularity(const Value: Single);
    procedure SetLabelPosition(const Value: TYAxisLabelPosition);
    procedure SetLowerLevel(const Value: Single);
    procedure SetMaximumGridLines(const Value: Integer);
    procedure SetUpperLevel(const Value: Single);
    function GetLowerGridLine: Single;
    function GetUpperGridLine: Single;
  protected
    FGranularity : Single;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AutoGranularityChanged; virtual;
    procedure CalculateGranularity;
    procedure GranularityChanged; virtual;
    procedure LabelPositionChanged; virtual;
    procedure LowerLevelChanged; virtual;
    procedure MaximumGridLinesChanged; virtual;
    procedure RangeChanged; override;
    procedure UpperLevelChanged; virtual;
  public
    constructor Create(AOwner: TCustomGuiEQGraph); override;

    procedure SetLevels(Lower, Upper: Single);
    procedure ShiftByPixel(Pixel: Integer); override;
    procedure ZoomAroundPixel(Pixel: Integer; Scale: Single); override;
    procedure ZoomInByPixel(StartPixel, EndPixel: Integer); override;
    procedure ZoomLowerByPixel(StartPixel, EndPixel: Integer); override;
    function LevelAtPixel(Pixel: Integer): Single;

    property UpperLevel: Single read FUpper write SetUpperLevel;
    property LowerLevel: Single read FLower write SetLowerLevel;

    property UpperGridline: Single read GetUpperGridLine;
    property LowerGridline: Single read GetLowerGridLine;
    property Granularity: Single read FGranularity write SetGranularity;

    property LabelPosition: TYAxisLabelPosition read FLabelPosition write SetLabelPosition default ylpNone;
    property MaximumGridLines: Integer read FMaximumGridLines write SetMaximumGridLines default 10;
    property AutoGranularity: Boolean read FAutoGranularity write SetAutoGranularity default True;
  end;

  TGuiEQGraphYAxis = class(TCustomGuiEQGraphYAxis)
  published
    property AutoGranularity;
    property LabelPosition;
    property LowerLevel;
    property UpperLevel;
    property Granularity;
    property MaximumGridLines;
    property UnitPosition;
    property MouseActions;
  end;


  // EQ Series

  TGuiEQGraphSeriesCollectionItem = class(TCollectionItem)
  private
    FDisplayName     : string;
    FOnGetFilterGain : TGetFilterGainEvent;
    FColor           : TColor;
    FLineWidth       : Single;
    FAlpha           : Byte;
    FESP             : TGuiPixelEquallySpacedPolyline;
    FDataCache       : PDAVDoubleFixedArray;
    FDataCacheSize   : Integer;
    FVisible         : Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetLineWidth(const Value: Single);
    procedure SetAlpha(const Value: Byte);
    procedure SetOnGetFilterGain(const Value: TGetFilterGainEvent);
    procedure SetVisible(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AlphaChanged; virtual;
    procedure ColorChanged; virtual;
    procedure LineWidthChanged; virtual;
    procedure VisibleChanged; virtual;
    procedure Changed; virtual;
    procedure OnGetFilterGainChanged; virtual;

    procedure CalculateDataCache;
    procedure DataCacheChanged;

    function GetZeroHandler(Sender: TObject; PixelPosition: Integer): TFixed24Dot8;
    function GetValueHandler(Sender: TObject; PixelPosition: Integer): TFixed24Dot8;

    procedure PaintToGraphAntialias(const Graph: TCustomGuiEQGraph; const PixelMap: TGuiCustomPixelMap); virtual;
    procedure PaintToGraphDraft(const Graph: TCustomGuiEQGraph; const PixelMap: TGuiCustomPixelMap); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure DataChanged; virtual;
  published
    property DisplayName;
    property Alpha: Byte read FAlpha write SetAlpha default $FF;
    property Color: TColor read FColor write SetColor default clRed;
    property LineWidth: Single read FLineWidth write SetLineWidth;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnGetFilterGain: TGetFilterGainEvent read FOnGetFilterGain write SetOnGetFilterGain;
  end;

  TGuiEQGraphSeriesCollection = class(TOwnedCollection)
  protected
    procedure Changed; virtual;
    function GetItem(Index: Integer): TGuiEQGraphSeriesCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiEQGraphSeriesCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiEQGraphSeriesCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiEQGraphSeriesCollectionItem;
    function Insert(Index: Integer): TGuiEQGraphSeriesCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;


  // EQ-Graph

  TCustomGuiEQGraph = class(TGuiCustomControl)
  private
    FAutoColor          : Boolean;
    FShowGrid           : Boolean;
    FChartColor         : TColor;
    FBorderRadius       : Single;
    FBorderWidth        : Single;
    FBorderColor        : TColor;
    FGraphColorDark     : TColor;
    FGraphColorLight    : TColor;

    FOnPaint            : TNotifyEvent;

    FFilterSeries       : TGuiEQGraphSeriesCollection;

    FGuiFont            : TGuiOversampledGDIFont;
    FYAxis              : TGuiEQGraphYAxis;
    FXAxis              : TGuiEQGraphXAxis;
    FAlpha              : Byte;
    FAntiAlias          : Boolean;
    FLastMouseCursorPos : TPoint;
    function GetFontShadow: TGuiShadow;
    function GetOversampling: TFontOversampling;
    function GetGraphWidth: Integer;
    function GetGraphHeight: Integer;
    procedure SetAutoColor(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Single);
    procedure SetBorderWidth(const Value: Single);
    procedure SetChartColor(const Value: TColor);
    procedure SetGraphColorDark(const Value: TColor);
    procedure SetGraphColorLight(const Value: TColor);
    procedure SetFilterSeries(const Value: TGuiEQGraphSeriesCollection);
    procedure SetFontShadow(const Value: TGuiShadow);
    procedure SetXAxis(const Value: TGuiEQGraphXAxis);
    procedure SetYAxis(const Value: TGuiEQGraphYAxis);
    procedure SetShowGrid(const Value: Boolean);
    procedure SetAlpha(const Value: Byte);
    procedure SetAntiAlias(const Value: Boolean);
    procedure SetOversampling(const Value: TFontOversampling);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AlphaChanged; virtual;
    procedure AntiAliasChanged; virtual;
    procedure AutoColorChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure BorderRadiusChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure ChartColorChanged; virtual;
    procedure ShowGridChanged; virtual;
    procedure GraphColorDarkChanged; virtual;
    procedure GraphColorLightChanged; virtual;

    procedure CMFontChanged(var Message: {$IFDEF LCL}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_FONTCHANGED;

    procedure RenderBorder(PixelMap: TGuiCustomPixelMap); virtual;
    procedure RenderGrid(PixelMap: TGuiCustomPixelMap); virtual;
    procedure RenderSeries(PixelMap: TGuiCustomPixelMap); virtual;
    procedure UpdateBuffer; override;
    procedure UpdateBackBuffer; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateGraph;

    property Alpha: Byte read FAlpha write SetAlpha default $FF;
    property AntiAlias: Boolean read FAntiAlias write SetAntiAlias default False;
    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
    property FontOversampling: TFontOversampling read GetOversampling write SetOversampling default foNone;
    property FontShadow: TGuiShadow read GetFontShadow write SetFontShadow;
    property GraphColorDark: TColor read FGraphColorDark write SetGraphColorDark default $303030;
    property GraphColorLight: TColor read FGraphColorLight write SetGraphColorLight default $606060;
    property GraphWidth: Integer read GetGraphWidth;
    property GraphHeight: Integer read GetGraphHeight;
    property ColorChart: TColor read FChartColor write SetChartColor;
    property BorderRadius: Single read FBorderRadius write SetBorderRadius;
    property BorderWidth: Single read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $202020;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid default True;
    property FilterSeries: TGuiEQGraphSeriesCollection read FFilterSeries write SetFilterSeries;
    property YAxis: TGuiEQGraphYAxis read FYAxis write SetYAxis;
    property XAxis: TGuiEQGraphXAxis read FXAxis write SetXAxis;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TGuiEQGraph = class(TCustomGuiEQGraph)
  published
    property Alpha;
    property AutoColor;
    property AntiAlias;
    property BorderColor;
    property BorderRadius;
    property BorderWidth;
    property ColorChart;
    property FilterSeries;
    property FontOversampling;
    property FontShadow;
    property GraphColorDark;
    property GraphColorLight;
    property ShowGrid;
    property XAxis;
    property YAxis;

    property OnPaint;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    {$IFNDEF FPC}
    property BiDiMode;
    property OnCanResize;
    {$ENDIF}
  end;

implementation

uses
  Math, DAV_Common, DAV_GuiBlend, DAV_Approximations;

{ TCustomGuiEQGraphAxis }

constructor TCustomGuiEQGraphAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 FOwner := AOwner;
 FUnitPosition := upValue;
end;

procedure TCustomGuiEQGraphAxis.Changed;
begin
// FOwner.BufferChanged;
 FOwner.BackBufferChanged;
end;

procedure TCustomGuiEQGraphAxis.RangeChanged;
begin
 CalculateRange;
end;

procedure TCustomGuiEQGraphAxis.SetUnitPosition(const Value: TUnitPosition);
begin
 if FUnitPosition <> Value then
  begin
   FUnitPosition := Value;
   UnitPositionChanged;
  end;
end;

procedure TCustomGuiEQGraphAxis.UnitPositionChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphAxis then
  with TCustomGuiEQGraphAxis(Dest) do
   begin
    FOwner        := Self.FOwner;
    FUpper        := Self.FUpper;
    FLower        := Self.FLower;
    FRange        := Self.FRange;
    FUnitPosition := Self.UnitPosition;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphAxis.CalculateRange;
begin
 FRange := FUpper - FLower;
 if FRange <> 0
  then FRangeReciprocal := 1 / FRange
  else FRangeReciprocal := 1;
end;


{ TCustomGuiEQGraphXAxis }

constructor TCustomGuiEQGraphXAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 inherited;
 FLabelPosition := xlpNone;
 FLabelFrequency := xlfDecade;
 FLower := 20;
 FUpper := 20000;
 CalculateUpperFrequencyReciprocal;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
end;

procedure TCustomGuiEQGraphXAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphXAxis then
  with TCustomGuiEQGraphXAxis(Dest) do
   begin
    inherited;
    FLabelPosition   := Self.FLabelPosition;
    FInvUpper     := Self.FInvUpper;
    FInvLower     := Self.FInvLower;
    FLog2Ratio    := Self.FLog2Ratio;
    FInvLog2Ratio := Self.FInvLog2Ratio;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphXAxis.DefineProperties(Filer: TFiler);
begin
 inherited;
  inherited DefineProperties(Filer);
  Filer.DefineProperty('LowerFrequency', ReadLowerProperty,
    WriteLowerProperty, LowerFrequency = 0);
  Filer.DefineProperty('UpperFrequency', ReadUpperProperty,
    WriteUpperProperty, UpperFrequency = 0);
end;

procedure TCustomGuiEQGraphXAxis.ReadLowerProperty(Reader: TReader);
begin
 FLower := Reader.ReadFloat;
end;

procedure TCustomGuiEQGraphXAxis.WriteLowerProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FLower);
end;

procedure TCustomGuiEQGraphXAxis.ReadUpperProperty(Reader: TReader);
begin
 FUpper := Reader.ReadFloat;
end;

procedure TCustomGuiEQGraphXAxis.WriteUpperProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FUpper);
end;

procedure TCustomGuiEQGraphXAxis.ShiftByPixel(Pixel: Integer);
var
  ShiftFrequency : Single;
begin
  ShiftFrequency := Power(2, -Pixel / FOwner.GraphHeight * FLog2Ratio);

  FLower := ShiftFrequency * FLower;
  FUpper := ShiftFrequency * FUpper;

  RangeChanged;
  CalculateUpperFrequencyReciprocal;
  CalculateLowerFrequencyReciprocal;
  CalculateFrequencyRangeRatios;
  Changed;
end;

procedure TCustomGuiEQGraphXAxis.ZoomAroundPixel(Pixel: Integer; Scale: Single);
var
  Frequency : Single;
begin
 if Scale = 1 then Exit;
 Frequency := FrequencyAtPixel(Pixel);
(*
 FLower := Level - Scale * (Level - FLower);
 FUpper := Level + Scale * (FUpper - Level);
*)
 RangeChanged;
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.ZoomInByPixel(StartPixel, EndPixel: Integer);
begin
 // yet todo
end;

procedure TCustomGuiEQGraphXAxis.ZoomLowerByPixel(StartPixel,
  EndPixel: Integer);
begin

end;

function TCustomGuiEQGraphXAxis.LogarithmicFrequencyToLinear(Value: Double): Double;
begin
 Result := Log2(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQGraphXAxis.LinearToLogarithmicFrequency(Value: Double): Double;
begin
 Result := Power(2, Value * FLog2Ratio) * FLower;
end;

function TCustomGuiEQGraphXAxis.FastLogarithmicFrequencyToLinear(Value: Single): Single;
begin
 Result := FastLog2MinError3(Value * FInvLower) * FInvLog2Ratio;
end;

function TCustomGuiEQGraphXAxis.FastLinearToLogarithmicFrequency(Value: Single): Single;
begin
 Result := FastPower2MinError3(Value * FLog2Ratio) * FLower;
end;

function TCustomGuiEQGraphXAxis.FrequencyAtPixel(Pixel: Integer): Single;
begin
 Result := LinearToLogarithmicFrequency((Pixel - Trunc(FOwner.BorderWidth)) /
   FOwner.GraphWidth);
end;

procedure TCustomGuiEQGraphXAxis.SetLabelFrequency(
  const Value: TXAxisLabelFrequency);
begin
 if FLabelFrequency <> Value then
  begin
   FLabelFrequency := Value;
   LabelFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetLabelPosition(const Value: TXAxisLabelPosition);
begin
 if FLabelPosition <> Value then
  begin
   FLabelPosition := Value;
   LabelPositionChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetLowerFrequency(const Value: Single);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.SetUpperFrequency(const Value: Single);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperFrequencyChanged;
  end;
end;

procedure TCustomGuiEQGraphXAxis.UpperFrequencyChanged;
begin
 RangeChanged;
 CalculateUpperFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.LowerFrequencyChanged;
begin
 RangeChanged;
 CalculateLowerFrequencyReciprocal;
 CalculateFrequencyRangeRatios;
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.CalculateUpperFrequencyReciprocal;
begin
 Assert(FUpper <> 0);

 // calculate reciprocal of upper frequency
 FInvUpper := 1 / FUpper;
end;

procedure TCustomGuiEQGraphXAxis.CalculateLowerFrequencyReciprocal;
begin
 Assert(FLower <> 0);

 // calculate reciprocal of lower frequency
 FInvLower := 1 / FLower;
end;

procedure TCustomGuiEQGraphXAxis.CalculateFrequencyRangeRatios;
begin
 Assert(FUpper <> 0);
 Assert(FInvLower <> 0);

 // calculate lograithmic frequency ratio (as new logarithm base)
 FLog2Ratio := Log2(FUpper * FInvLower);
 FInvLog2Ratio := 1 / FLog2Ratio;
end;

procedure TCustomGuiEQGraphXAxis.LabelPositionChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphXAxis.LabelFrequencyChanged;
begin
 Changed;
end;


{ TCustomGuiEQGraphYAxis }

constructor TCustomGuiEQGraphYAxis.Create(AOwner: TCustomGuiEQGraph);
begin
 inherited Create(AOwner);
 FUpper :=  15;
 FLower := -15;
 FLabelPosition := ylpNone;
 FMaximumGridLines := 10;
 FAutoGranularity := True;

 CalculateRange;
 CalculateGranularity;
end;

procedure TCustomGuiEQGraphYAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiEQGraphYAxis then
  with TCustomGuiEQGraphYAxis(Dest) do
   begin
    FLabelPosition := Self.FLabelPosition;
    FGranularity := Self.FGranularity;
    FMaximumGridLines := Self.FMaximumGridLines;
   end
 else inherited;
end;

procedure TCustomGuiEQGraphYAxis.SetLowerLevel(const Value: Single);
begin
 if FLower <> Value then
  begin
   FLower := Value;
   LowerLevelChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetMaximumGridLines(const Value: Integer);
begin
 if Value < 1
  then raise Exception.Create('Value must be larger than 0!');
 
 if FMaximumGridLines <> Value then
  begin
   FMaximumGridLines := Value;
   MaximumGridLinesChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetUpperLevel(const Value: Single);
begin
 if FUpper <> Value then
  begin
   FUpper := Value;
   UpperLevelChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.ShiftByPixel(Pixel: Integer);
var
  ShiftLevel : Single;
begin
  ShiftLevel := Pixel * FRange / FOwner.GraphHeight;
  FLower := FLower + ShiftLevel;
  FUpper := FUpper + ShiftLevel;
  Changed;
end;

procedure TCustomGuiEQGraphYAxis.SetAutoGranularity(const Value: Boolean);
begin
 if FAutoGranularity <> Value then
  begin
   FAutoGranularity := Value;
   AutoGranularityChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetGranularity(const Value: Single);
begin
 if FAutoGranularity
  then Exit;

 if FGranularity <> Value then
  begin
   FGranularity := Value;
   GranularityChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetLabelPosition(const Value: TYAxisLabelPosition);
begin
 if FLabelPosition <> Value then
  begin
   FLabelPosition := Value;
   LabelPositionChanged;
  end;
end;

procedure TCustomGuiEQGraphYAxis.SetLevels(Lower, Upper: Single);
begin
 // ensure that upper is above lower
 if FLower > FUpper
  then raise Exception.Create('Lower above Upper');

 if (FUpper <> Upper) or (FLower <> Lower) then
  begin
   FUpper := Upper;
   FLower := Lower;
   RangeChanged;
   Changed;
  end;
end;

procedure TCustomGuiEQGraphYAxis.AutoGranularityChanged;
begin
 if FAutoGranularity
  then CalculateGranularity;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.GranularityChanged;
begin
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.LabelPositionChanged;
begin
 Changed;
end;

function TCustomGuiEQGraphYAxis.LevelAtPixel(Pixel: Integer): Single;
var
  Ratio : Single;
begin
 Ratio := (Pixel - Trunc(FOwner.BorderWidth)) / FOwner.GraphHeight;
 Result := UpperLevel - Ratio * Range;
end;

procedure TCustomGuiEQGraphYAxis.UpperLevelChanged;
begin
 RangeChanged;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.ZoomAroundPixel(Pixel: Integer; Scale: Single);
var
  Level : Single;
begin
 if Scale = 1 then Exit;
 Level := LevelAtPixel(Pixel);
 FLower := Level - Scale * (Level - FLower);
 FUpper := Level + Scale * (FUpper - Level);
 RangeChanged;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.ZoomLowerByPixel(StartPixel, EndPixel: Integer);
var
  Level : Single;
  Ratio : Single;
begin
 Level := LevelAtPixel(StartPixel);
 Ratio := FOwner.GraphHeight / (EndPixel - Trunc(FOwner.BorderWidth));
 LowerLevel := FUpper + Ratio * (Level - FUpper);
end;

procedure TCustomGuiEQGraphYAxis.ZoomInByPixel(StartPixel, EndPixel: Integer);
var
  Level : Single;
  Ratio : Single;
begin
 if EndPixel > StartPixel then
  begin
   Level := LevelAtPixel(StartPixel);
   Ratio := FOwner.GraphHeight / (EndPixel - Trunc(FOwner.BorderWidth));
   LowerLevel := FUpper + Ratio * (Level - FUpper);
  end else
 if EndPixel < StartPixel then
  begin
   Level := LevelAtPixel(StartPixel);
   Ratio := FOwner.GraphHeight / ((EndPixel - Trunc(FOwner.BorderWidth)) - FOwner.GraphHeight);
   UpperLevel := FLower + Ratio * (FLower - Level);
  end;
end;

procedure TCustomGuiEQGraphYAxis.LowerLevelChanged;
begin
 RangeChanged;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.MaximumGridLinesChanged;
begin
 CalculateGranularity;
 Changed;
end;

procedure TCustomGuiEQGraphYAxis.RangeChanged;
begin
 inherited;
 CalculateGranularity;
end;

function TCustomGuiEQGraphYAxis.GetLowerGridLine: Single;
begin
 Result := FGranularity * Trunc(FLower / FGranularity);
end;

function TCustomGuiEQGraphYAxis.GetUpperGridLine: Single;
begin
 Result := FGranularity * Trunc(FUpper / FGranularity);
end;

procedure TCustomGuiEQGraphYAxis.CalculateGranularity;
var
  RoughGranularity : Single;
  GranularityBase  : Integer;
  GranularityScale : Single;
begin
 RoughGranularity := Range / FMaximumGridLines;
 GranularityBase  := Trunc(Log10(Abs(RoughGranularity)));
 GranularityScale := IntPower(10, GranularityBase);

 FGranularity := GranularityScale * (Trunc(RoughGranularity / GranularityScale) + 1);

 Assert(FGranularity >= RoughGranularity);
 Assert(FGranularity < Range);
end;


{ TGuiEQGraphSeriesCollectionItem }

constructor TGuiEQGraphSeriesCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FDisplayName := ClassName;
 FESP := TGuiPixelEquallySpacedPolyline.Create;

 with FESP do
  begin
   GeometricShape.OnGetValue := GetZeroHandler;
   Color := clRed;
   Alpha := $FF;
   LineWidth := CFixed24Dot8Two;
   Assert(Collection.Owner is TCustomGuiEQGraph);
   GeometricShape.SetAllMargins(Trunc(
     TCustomGuiEQGraph(Collection.Owner).BorderWidth));
  end;

 FColor := clRed;
 FAlpha := $FF;
 FLineWidth := 2;
 FVisible := True;
end;

destructor TGuiEQGraphSeriesCollectionItem.Destroy;
begin
 FreeAndNil(FESP);
 inherited;
end;

procedure TGuiEQGraphSeriesCollectionItem.DataCacheChanged;
var
  NewDataSize : Integer;
begin
 Assert(Collection.Owner is TCustomGuiEQGraph);
 NewDataSize := TCustomGuiEQGraph(Collection.Owner).GraphWidth;

 if FDataCacheSize <> NewDataSize then
  begin
   FDataCacheSize := NewDataSize;
   ReallocMem(FDataCache, FDataCacheSize * SizeOf(Double));
   CalculateDataCache;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.DataChanged;
begin
 CalculateDataCache;
end;

procedure TGuiEQGraphSeriesCollectionItem.CalculateDataCache;
var
  DataIndex : Integer;
  Freq      : Single;
begin
 with TCustomGuiEQGraph(Collection.Owner) do
  if Assigned(FOnGetFilterGain) then
   for DataIndex := 0 to FDataCacheSize - 1 do
    begin
     Freq := FXAxis.LinearToLogarithmicFrequency(DataIndex / GraphWidth);
     FDataCache[DataIndex] := FOnGetFilterGain(Self, Freq);
    end;

 // strategy for implicit oversampling: scan with higher resolution (eg. 0.25px)
 // and use the highest if the average is above the last value or use the lowest
 // in case the average is below the last value.
end;

procedure TGuiEQGraphSeriesCollectionItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiEQGraphSeriesCollectionItem then
  with TGuiEQGraphSeriesCollectionItem(Dest) do
   begin
    FColor           := Self.FColor;
    FLineWidth       := Self.FLineWidth;
    FDisplayName     := Self.FDisplayName;
    FVisible         := Self.FVisible;
    FOnGetFilterGain := Self.FOnGetFilterGain;
   end
 else inherited;
end;

procedure TGuiEQGraphSeriesCollectionItem.ColorChanged;
begin
 FESP.Color := Color;
 Changed;
end;

procedure TGuiEQGraphSeriesCollectionItem.LineWidthChanged;
begin
 FESP.LineWidth := ConvertToFixed24Dot8(FLineWidth);
 Changed;
end;

procedure TGuiEQGraphSeriesCollectionItem.OnGetFilterGainChanged;
begin
 if Assigned(FOnGetFilterGain)
  then FESP.GeometricShape.OnGetValue := GetValueHandler
  else FESP.GeometricShape.OnGetValue := GetZeroHandler;
end;

procedure TGuiEQGraphSeriesCollectionItem.PaintToGraphAntialias(
  const Graph: TCustomGuiEQGraph; const PixelMap: TGuiCustomPixelMap);
begin
 if Visible
  then FESP.Draw(PixelMap);
end;

procedure TGuiEQGraphSeriesCollectionItem.PaintToGraphDraft(
  const Graph: TCustomGuiEQGraph; const PixelMap: TGuiCustomPixelMap);
begin
 if Visible
  then FESP.DrawDraft(PixelMap);
end;

procedure TGuiEQGraphSeriesCollectionItem.AlphaChanged;
begin
 FESP.Alpha := Alpha;
 Changed;
end;

procedure TGuiEQGraphSeriesCollectionItem.VisibleChanged;
begin
 Changed;
end;

procedure TGuiEQGraphSeriesCollectionItem.Changed;
begin
 if Collection is TGuiEQGraphSeriesCollection
  then TGuiEQGraphSeriesCollection(Collection).Changed;
end;

function TGuiEQGraphSeriesCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiEQGraphSeriesCollectionItem.GetZeroHandler(Sender: TObject;
  PixelPosition: Integer): TFixed24Dot8;
begin
 Assert(Collection.Owner is TCustomGuiEQGraph);
 with TCustomGuiEQGraph(Collection.Owner) do
  begin
   Result := ConvertToFixed24Dot8((YAxis.UpperLevel * YAxis.RangeReciprocal) * GraphHeight);
  end;
end;

function TGuiEQGraphSeriesCollectionItem.GetValueHandler(Sender: TObject;
  PixelPosition: Integer): TFixed24Dot8;
(*
var
  Level : Single;
  Freq  : Single;
*)
begin
 Assert(Collection.Owner is TCustomGuiEQGraph);
 with TCustomGuiEQGraph(Collection.Owner) do
  if (PixelPosition >= 0) and (PixelPosition < FDataCacheSize)
   then Result := ConvertToFixed24Dot8(((YAxis.UpperLevel - FDataCache[PixelPosition]) * YAxis.RangeReciprocal) * GraphHeight)
   else Result.Fixed := 0;

(*
 Assert(Collection.Owner is TCustomGuiEQGraph);
 with TCustomGuiEQGraph(Collection.Owner) do
  begin
   Freq := FXAxis.LinearToLogarithmicFrequency(PixelPosition / Width);
   Level := FOnGetFilterGain(Self, Freq);
   Result := ConvertToFixed24Dot8(((YAxis.UpperLevel - Level) / YAxis.Range) * GraphHeight);
  end;
*)
end;

procedure TGuiEQGraphSeriesCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetLineWidth(const Value: Single);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   LineWidthChanged;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetOnGetFilterGain(
  const Value: TGetFilterGainEvent);
begin
 if @FOnGetFilterGain <> @Value then
  begin
   FOnGetFilterGain := Value;
   OnGetFilterGainChanged;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   VisibleChanged;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TGuiEQGraphSeriesCollectionItem.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   ColorChanged;
  end;
end;


{ TGuiEQGraphSeriesCollection }

constructor TGuiEQGraphSeriesCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiEQGraphSeriesCollectionItem);
end;

function TGuiEQGraphSeriesCollection.Add: TGuiEQGraphSeriesCollectionItem;
begin
 Result := TGuiEQGraphSeriesCollectionItem(inherited Add);
end;

procedure TGuiEQGraphSeriesCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiEQGraphSeriesCollection.GetItem(Index: Integer): TGuiEQGraphSeriesCollectionItem;
begin
 Result := TGuiEQGraphSeriesCollectionItem(inherited GetItem(Index));
end;

function TGuiEQGraphSeriesCollection.Insert(
  Index: Integer): TGuiEQGraphSeriesCollectionItem;
begin
 Result:= TGuiEQGraphSeriesCollectionItem(inherited Insert(Index));
end;

procedure TGuiEQGraphSeriesCollection.Changed;
begin
 if Owner is TCustomGuiEQGraph
  then TCustomGuiEQGraph(Owner).BufferChanged;
end;

procedure TGuiEQGraphSeriesCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 Changed;
end;

procedure TGuiEQGraphSeriesCollection.SetItem(Index: Integer;
  const Value: TGuiEQGraphSeriesCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TCustomGuiEQGraph }

constructor TCustomGuiEQGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csDoubleClicks, csReplicatable, csOpaque];
 TabStop := False; // Ensure we're not a tab-stop
 Color := clBtnFace;

 // create sub objects
 FGuiFont         := TGuiOversampledGDIFont.Create;
 FXAxis           := TGuiEQGraphXAxis.Create(Self);
 FYAxis           := TGuiEQGraphYAxis.Create(Self);
 FFilterSeries    := TGuiEQGraphSeriesCollection.Create(Self);

 FAlpha           := $FF;
 FAutoColor       := False;
 FChartColor      := Color;
 FGraphColorLight := $606060;
 FGraphColorDark  := $303030;
 FBorderColor     := $202020;
 FBorderWidth     := 1;
 FShowGrid        := True;
end;

destructor TCustomGuiEQGraph.Destroy;
begin
 FreeAndNil(FXAxis);
 FreeAndNil(FYAxis);
 FreeAndNil(FFilterSeries);
 inherited Destroy;
end;

procedure TCustomGuiEQGraph.AlphaChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.AntiAliasChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiEQGraph then
  with TCustomGuiEQGraph(Dest) do
   begin
    FAlpha           := Self.FAlpha;
    FAutoColor       := Self.FAutoColor;
    FAntiAlias       := Self.FAntiAlias;
    FChartColor      := Self.FChartColor;
    FBorderRadius    := Self.FBorderRadius;
    FBorderWidth     := Self.FBorderWidth;
    FBorderColor     := Self.FBorderColor;
    FGraphColorDark  := Self.FGraphColorDark;
    FGraphColorLight := Self.FGraphColorLight;
    FOnPaint         := Self.FOnPaint;

    FFilterSeries.Assign(Self.FFilterSeries);
    FYAxis.Assign(Self.FYAxis);
    FXAxis.Assign(Self.FXAxis);
   end;
end;

procedure TCustomGuiEQGraph.SetFilterSeries(const Value: TGuiEQGraphSeriesCollection);
begin
 FFilterSeries.Assign(Value);
end;

procedure TCustomGuiEQGraph.SetFontShadow(const Value: TGuiShadow);
begin
 FGuiFont.Shadow.Assign(Value);
 BackBufferChanged;
end;

procedure TCustomGuiEQGraph.SetChartColor(const Value: TColor);
begin
 if not FAutoColor and (FChartColor <> Value) then
  begin
   FChartColor := Value;
   ChartColorChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetAlpha(const Value: Byte);
begin
 if FAlpha <> Value then
  begin
   FAlpha := Value;
   AlphaChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetAntiAlias(const Value: Boolean);
begin
 if FAntiAlias <> Value then
  begin
   FAntiAlias := Value;
   AntiAliasChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetAutoColor(const Value: Boolean);
begin
 if FAutoColor <> Value then
  begin
   FAutoColor := Value;
   AutoColorChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetBorderColor(const Value: TColor);
begin
 if FBorderColor <> Value then
  begin
   FBorderColor := Value;
   BorderColorChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetBorderRadius(const Value: Single);
begin
 if FBorderRadius <> Value then
  begin
   FBorderRadius := Value;
   BorderRadiusChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetBorderWidth(const Value: Single);
begin
 if FBorderWidth <> Value then
  begin
   FBorderWidth := Value;
   BorderWidthChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetGraphColorDark(const Value: TColor);
begin
 if FGraphColorDark <> Value then
  begin
   FGraphColorDark := Value;
   GraphColorDarkChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetGraphColorLight(const Value: TColor);
begin
 if FGraphColorLight <> Value then
  begin
   FGraphColorLight := Value;
   GraphColorLightChanged;
  end;
end;

procedure TCustomGuiEQGraph.SetOversampling(const Value: TFontOversampling);
begin
 FGuiFont.FontOversampling := Value;
 BackBufferChanged;
end;

procedure TCustomGuiEQGraph.SetShowGrid(const Value: Boolean);
begin
 if FShowGrid <> Value then
  begin
   FShowGrid := Value;
   ShowGridChanged;
  end;
end;

procedure TCustomGuiEQGraph.AutoColorChanged;
begin
 if FAutoColor then
  begin
(*
   FChartColor32 := Lighten(Color32(Color),60);
   FChartColor := WinColor(FChartColor32);
*)
   BufferChanged;
  end;
end;

procedure TCustomGuiEQGraph.BorderWidthChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.SetXAxis(const Value: TGuiEQGraphXAxis);
begin
 FXAxis.Assign(Value);
end;

procedure TCustomGuiEQGraph.SetYAxis(const Value: TGuiEQGraphYAxis);
begin
 FYAxis.Assign(Value);
end;

procedure TCustomGuiEQGraph.ShowGridChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.UpdateBackBuffer;
begin
 inherited;
 RenderBorder(FBackBuffer);
 RenderGrid(FBackBuffer);
end;

procedure TCustomGuiEQGraph.UpdateBuffer;
begin
 inherited;
 RenderSeries(FBuffer);
end;

procedure TCustomGuiEQGraph.UpdateGraph;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.RenderBorder(PixelMap: TGuiCustomPixelMap);
var
  X, Y                 : Integer;
  XRange               : array [0..1] of Integer;
  ScnLne               : array [0..1] of PPixel32Array;
  PanelColor           : TPixel32;
  BorderColor          : TPixel32;
  CombColor            : TPixel32;
  IsUpperLowerHalf     : Boolean;
  Radius               : TFixed24Dot8;
  XStart               : TFixed24Dot8;
  BorderWidthFixed     : TFixed24Dot8;
  RadMinusOne          : TFixed24Dot8;
  RadMinusBorder       : TFixed24Dot8;
  SqrRadMinusBorderOne : TFixed24Dot8;
  SqrRadMinusBorder    : TFixed24Dot8;
  SqrDist, SqrYDist    : TFixed24Dot8;
  SqrRadMinusOne       : TFixed24Dot8;
  XFixed, YFixed       : TFixed24Dot8;
  WidthMinusOne        : TFixed24Dot8;
  YBorderDistance      : TFixed24Dot8;
  Temp                 : TFixed24Dot8;
begin
 with PixelMap do
  begin
   // set local colors
   PanelColor := ConvertColor(Color);
   PanelColor.A := Alpha;
   if BorderRadius = 0 then Exit;
   BorderColor := ConvertColor(FBorderColor);

   // set other local variables
   Radius := ConvertToFixed24Dot8(Min(FBorderRadius, 0.5 * Min(Width, Height)) + 1);
   BorderWidthFixed := ConvertToFixed24Dot8(Max(FBorderWidth, 1));
   WidthMinusOne := ConvertToFixed24Dot8(Integer(Width - 1));

   // precalculate radius variables
   RadMinusOne.Fixed := Radius.Fixed - CFixed24Dot8One.Fixed;
   if RadMinusOne.Fixed < 0
    then RadMinusOne.Fixed := 0;

   RadMinusBorder.Fixed := Radius.Fixed - BorderWidthFixed.Fixed;
   if RadMinusBorder.Fixed < 0
    then RadMinusBorder.Fixed := 0;
   SqrRadMinusBorder := FixedSqr(RadMinusBorder);

   SqrRadMinusBorderOne.Fixed := RadMinusBorder.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusBorderOne.Fixed < 0
    then SqrRadMinusBorderOne.Fixed := 0
    else SqrRadMinusBorderOne := FixedSqr(SqrRadMinusBorderOne);

   SqrRadMinusOne.Fixed := Radius.Fixed - CFixed24Dot8One.Fixed;
   if SqrRadMinusOne.Fixed < 0
    then SqrRadMinusOne.Fixed := 0
    else SqrRadMinusOne := FixedSqr(SqrRadMinusOne);

   // draw rounded borders
   for Y := 0 to FixedRound(Radius) - 1  do
    begin
     YFixed := ConvertToFixed24Dot8(Y);
     SqrYDist := FixedSqr(FixedSub(YFixed, FixedSub(Radius, CFixed24Dot8One)));
     XStart.Fixed := FixedSqr(Radius).Fixed - SqrYDist.Fixed;
     if XStart.Fixed <= 0
      then Continue
      else XStart.Fixed := FixedSqrt(XStart).Fixed - CFixed24Dot8Half.Fixed;
     ScnLne[0] := Scanline[Y];
     ScnLne[1] := Scanline[Height - 1 - Y];

     Temp.Fixed := RadMinusOne.Fixed - XStart.Fixed;
     XRange[0] := FixedRound(Temp);
     XRange[1] := FixedRound(FixedSub(ConvertToFixed24Dot8(Integer(Width - 1)), Temp));
     for X := XRange[0] to XRange[1] do
      begin
       XFixed := ConvertToFixed24Dot8(X);

       // calculate squared distance
       if XFixed.Fixed < RadMinusOne.Fixed
        then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, RadMinusOne)).Fixed + SqrYDist.Fixed
        else
         begin
          Temp.Fixed := ConvertToFixed24Dot8(Integer(Width - 1)).Fixed - RadMinusOne.Fixed;
          if XFixed.Fixed > Temp.Fixed
           then SqrDist.Fixed := FixedSqr(FixedSub(XFixed, Temp)).Fixed + SqrYDist.Fixed
           else SqrDist := SqrYDist;
         end;

       if SqrDist.Fixed < SqrRadMinusBorderOne.Fixed
        then CombColor := PanelColor
        else
       if SqrDist.Fixed < SqrRadMinusBorder.Fixed then
        begin
         Temp.Fixed := RadMinusBorder.Fixed - FixedSqrt(SqrDist).Fixed;
         Assert(Temp.Fixed >= 0);
         if Temp.Fixed > $FF
          then CombColor := PanelColor
          else CombColor := CombinePixel(BorderColor, PanelColor, Round($FF - Temp.Fixed));
        end else
       if SqrDist.Fixed < SqrRadMinusOne.Fixed
        then CombColor := BorderColor
        else
         begin
          CombColor := BorderColor;
          Temp.Fixed := CombColor.A;
          Temp := FixedMul(Temp, FixedSub(Radius, FixedSqrt(SqrDist)));
          Assert(Temp.Fixed >= 0);
          Assert(Temp.Fixed <= $FF);
          CombColor.A := Temp.Fixed;
         end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       BlendPixelInplace(CombColor, ScnLne[1][X]);
      end;
    end;

   for Y := FixedRound(Radius) to Height - 1 - FixedRound(Radius) do
    begin
     ScnLne[0] := Scanline[Y];
     YFixed := ConvertToFixed24Dot8(Y);

     // check whether position is a non-rounded border
     if (YFixed.Fixed < BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
        (YFixed.Fixed > ConvertToFixed24Dot8(Height).Fixed - BorderWidthFixed.Fixed) then
      begin
       BlendPixelLine(BorderColor, @ScnLne[0][0], Width);
       Continue;
      end;

     // check upper/lower half and eventually precalculate y-border distance
     Temp := ConvertToFixed24Dot8(Integer(Height - 1));
     IsUpperLowerHalf := (YFixed.Fixed < BorderWidthFixed.Fixed) or
       (YFixed.Fixed > Temp.Fixed - BorderWidthFixed.Fixed);
     if IsUpperLowerHalf then
      if Y < Height div 2
       then YBorderDistance.Fixed := BorderWidthFixed.Fixed - YFixed.Fixed
       else YBorderDistance.Fixed := YFixed.Fixed - Temp.Fixed + BorderWidthFixed.Fixed
      else YBorderDistance.Fixed := 0;

     X := 0;
     while X < Width do
      begin
       // convert
       XFixed := ConvertToFixed24Dot8(X);

       // check whether position is an upper/lower half border
       if IsUpperLowerHalf then
        begin
         if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
            (XFixed.Fixed >= ConvertToFixed24Dot8(Width).Fixed - BorderWidthFixed.Fixed)
          then CombColor := BorderColor else
         if (XFixed.Fixed < BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end else
         if (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
          begin
           Temp.Fixed := XFixed.Fixed + BorderWidthFixed.Fixed - WidthMinusOne.Fixed;
           Temp := FixedMul(Temp, FixedSub(CFixed24Dot8One, YBorderDistance));
           Temp.Fixed := YBorderDistance.Fixed + Temp.Fixed;
           Assert(Temp.Fixed >= 0);
           Assert(Temp.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
          end
         else
          begin
           Assert(YBorderDistance.Fixed >= 0);
           Assert(YBorderDistance.Fixed <= $FF);
           CombColor := CombinePixel(BorderColor, PanelColor, YBorderDistance.Fixed);
           BlendPixelLine(CombColor, @ScnLne[0][X], Width - 2 * X);
           EMMS;
           X := Width - X;
           Continue;
          end;
        end else
       if (XFixed.Fixed <= BorderWidthFixed.Fixed - CFixed24Dot8One.Fixed) or
          (XFixed.Fixed >= WidthMinusOne.Fixed - BorderWidthFixed.Fixed + CFixed24Dot8One.Fixed)
        then CombColor := BorderColor else
       if (XFixed.Fixed < BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := BorderWidthFixed.Fixed - XFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end else
       if (XFixed.Fixed > WidthMinusOne.Fixed - BorderWidthFixed.Fixed) then
        begin
         Temp.Fixed := XFixed.Fixed - WidthMinusOne.Fixed + BorderWidthFixed.Fixed;
         Assert(Temp.Fixed >= 0);
         Assert(Temp.Fixed <= $FF);
         CombColor := CombinePixel(BorderColor, PanelColor, Temp.Fixed);
        end
       else
        begin
         BlendPixelLine(PanelColor, @ScnLne[0][X], Width - 2 * X);
         EMMS;
         X := Width - X;
         Continue;
        end;

       BlendPixelInplace(CombColor, ScnLne[0][X]);
       Inc(X)
      end;
    end;
   EMMS;
  end;
end;

procedure TCustomGuiEQGraph.RenderGrid(PixelMap: TGuiCustomPixelMap);
var
  i, j, w, h  : Integer;
  TextSize    : TSize;
  Rct         : TRect;
  Txt         : string;
  Temp        : Single;
  Wdth        : Integer;
  Base        : Integer;
  DrawLabel   : Boolean;
  LineColor   : TPixel32;
begin
 with PixelMap do
  begin
   Rct := Rect(0, 0, Width, Height);
   InflateRect(Rct, -Trunc(FBorderWidth), -Trunc(FBorderWidth));

   // add top margin to half height as offset
   Wdth := Round(Rct.Right - Rct.Left);

   // draw y-axis grid lines
   with FYAxis do
    begin
     Temp := GetLowerGridLine;
     LineColor := ConvertColor(FGraphColorLight);

     while Temp < UpperLevel do
      begin
       i := Round(((Temp - LowerLevel) / Range) *  (Rct.Bottom - Rct.Top));
       if Temp = 0 then
        begin
         Temp := Temp + Granularity;
         Continue;
        end;
       PixelMap.HorizontalLine(Rct.Left,  Rct.Right, Round(Rct.Bottom - i),
         LineColor);
       Temp := Temp + Granularity;
      end;
    end;

   // draw x-axis grid lines
   i := Round(IntPower(10, Trunc(Log10(Abs(FXAxis.LowerFrequency)))));
   j := Round(FXAxis.LowerFrequency / i);
   if j = FXAxis.LowerFrequency / i then
    begin
     Inc(j);
     if j >= 10 then
      begin
       i := i * 10;
       j := 1;
       LineColor := ConvertColor(FGraphColorDark);
      end
     else LineColor := ConvertColor(FGraphColorLight);
    end;

   while j * i < FXAxis.UpperFrequency do
    begin
     w := Round(Rct.Left + FXAxis.LogarithmicFrequencyToLinear(j * i) * Wdth);
     PixelMap.VerticalLine(w, Rct.Top, Rct.Bottom, LineColor);
     Inc(j);
     if j >= 10 then
      begin
       i := i * 10;
       j := 1;
       LineColor := ConvertColor(FGraphColorDark);
      end
     else LineColor := ConvertColor(FGraphColorLight);
    end;

   // draw y-axis center line
   with FYAxis do
    if (LowerLevel < 0) and (UpperLevel > 0) then
     begin
      LineColor := ConvertColor(FGraphColorDark);
      i := Round(((0 - LowerLevel) / Range) *  (Rct.Bottom - Rct.Top));
      PixelMap.HorizontalLine(Rct.Left,  Rct.Right, Rct.Bottom - i, LineColor);
    end;

//   Brush.Color := FChartColor;
//   Brush.Style := bsClear;

   //////////////////////
   // draw axis labels //
   //////////////////////

   // draw x-axis label
   with FXAxis do
    begin
     Base := Round(IntPower(10, Trunc(Log10(Abs(FXAxis.LowerFrequency)))));
     j := Round(FXAxis.LowerFrequency / Base);
     if j = FXAxis.LowerFrequency / Base then
      begin
       Inc(j);
       if j >= 10 then
        begin
         Base := Base * 10;
         j := 1;
        end;
      end;

     DrawLabel := True;
     case FXAxis.LabelPosition of
      xlpBottom :
       begin
        while j * Base < FXAxis.UpperFrequency do
         begin
          case FLabelFrequency of
           xlfDecade : DrawLabel := j = 1;
           xlfThirdDecade : DrawLabel := (j in [1, 2, 5]);
          end;
          if DrawLabel then
           begin
            Txt := FloatToStrF(j * Base, ffGeneral, 5, 5);

            // eventually add unit
            if FXAxis.UnitPosition = upValue
             then Txt := Txt + ' Hz';

            TextSize := FGuiFont.TextExtent(Txt);
            h := Round(Rct.Bottom - TextSize.cy - 1);
            FGuiFont.TextOut(Txt, PixelMap, Round(Rct.Left + FXAxis.LogarithmicFrequencyToLinear(j * Base) * Wdth) - (TextSize.cx div 2), h);
           end;
          Inc(j);
          if j >= 10 then
           begin
            Base := Base * 10;
            j := 1;
           end;
         end;
       end;
      xlpTop:
       begin
        while j * Base < FXAxis.UpperFrequency do
         begin
          case FLabelFrequency of
           xlfDecade : DrawLabel := j = 1;
           xlfThirdDecade : DrawLabel := (j in [1, 2, 5]);
          end;
          if DrawLabel then
           begin
            Txt := FloatToStrF(j * Base, ffGeneral, 5, 5);

            // eventually add unit
            if FXAxis.UnitPosition = upValue
             then Txt := Txt + ' Hz';

            TextSize := FGuiFont.TextExtent(Txt);
            h := Round(Rct.Top + 1);
            FGuiFont.TextOut(Txt, PixelMap, Round(Rct.Left + FXAxis.LogarithmicFrequencyToLinear(j * Base) * Wdth) - (TextSize.cx div 2), h);
           end;
          Inc(j);
          if j >= 10 then
           begin
            Base := Base * 10;
            j := 1;
           end;
         end;
       end;
     end;
    end;

   case FYAxis.LabelPosition of
    ylpLeft:
     with FYAxis do
      begin
       Temp := GetLowerGridLine;

       while Temp < UpperLevel do
        begin
         i := Round(((Temp - LowerLevel) / Range) * (Rct.Bottom - Rct.Top));

         Txt := FloatToStrF(Temp, ffGeneral, 5, 5);

         // modify text
         if (Temp > 0) then Txt := '+' + Txt else
         if (Temp = 0) then Txt := ' ' + Txt;
         if UnitPosition = upValue
          then Txt := Txt + ' dB';

         TextSize := FGuiFont.TextExtent(Txt);
         Base := Round(Rct.Left);

         // apply font shadow offset
         with FGuiFont.Shadow do
         if Visible
          then Base := Base + Max(0, Round(Blur - OffsetX));

         FGuiFont.TextOut(Txt, PixelMap, Base, Round(Rct.Bottom - i - 0.5 * TextSize.cy));
         Temp := Temp + Granularity;
        end;
      end;
    ylpRight:
     with FYAxis do
      begin
       Temp := GetLowerGridLine;

       while Temp < UpperLevel do
        begin
         i := Round(((Temp - LowerLevel) / Range) *  (Rct.Bottom - Rct.Top));

         Txt := FloatToStrF(Temp, ffGeneral, 5, 5);

         // modify text
         if (Temp >= 0) then Txt := '+' + Txt;
         if UnitPosition = upValue
          then Txt := Txt + ' dB';

         FGuiFont.TextOut(Txt, PixelMap, Round(Rct.Right - 1 - 0.5 * TextSize.cx), Round(Rct.Bottom - i - 0.5 * TextSize.cy));
         Temp := Temp + Granularity;
        end;
      end;
   end;
  end;
end;

procedure TCustomGuiEQGraph.RenderSeries(PixelMap: TGuiCustomPixelMap);
var
  SeriesIndex : Integer;
begin
 if FAntiAlias then
  for SeriesIndex := 0 to FFilterSeries.Count - 1
   do FFilterSeries[SeriesIndex].PaintToGraphAntialias(Self, PixelMap)
 else
  for SeriesIndex := 0 to FFilterSeries.Count - 1
   do FFilterSeries[SeriesIndex].PaintToGraphDraft(Self, PixelMap)
end;

procedure TCustomGuiEQGraph.Resize;
var
  SeriesIndex : Integer;
begin
  inherited;

  // calculate data cache
  for SeriesIndex := 0 to FFilterSeries.Count - 1
   do FFilterSeries[SeriesIndex].DataCacheChanged;
end;

procedure TCustomGuiEQGraph.BorderColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.BorderRadiusChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.ChartColorChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.CMFontChanged(var Message: {$IFDEF LCL}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 FGuiFont.Font.Assign(Font);
 BackBufferChanged;
end;

function TCustomGuiEQGraph.GetFontShadow: TGuiShadow;
begin
 Result := FGuiFont.Shadow;
end;

function TCustomGuiEQGraph.GetGraphHeight: Integer;
begin
 Result := Height - 2 * Trunc(FBorderWidth);
end;

function TCustomGuiEQGraph.GetGraphWidth: Integer;
begin
 Result := Width - 2 * Trunc(FBorderWidth);
end;

function TCustomGuiEQGraph.GetOversampling: TFontOversampling;
begin
 Result := FGuiFont.FontOversampling;
end;

procedure TCustomGuiEQGraph.GraphColorDarkChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.GraphColorLightChanged;
begin
 BufferChanged;
end;

procedure TCustomGuiEQGraph.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 if ssRight in Shift then
  begin
   if (Abs(Y - FLastMouseCursorPos.Y) > 0) then
    begin
     if (maZoom in YAxis.FMouseActions) and (ssCtrl in Shift)
      then YAxis.ZoomLowerByPixel(FLastMouseCursorPos.Y, Y) else
     if (maShift in YAxis.FMouseActions)
      then YAxis.ShiftByPixel(Y - FLastMouseCursorPos.Y);
    end;

   if (Abs(X - FLastMouseCursorPos.X) > 0) then
    begin
     if (maZoom in YAxis.FMouseActions) and (ssCtrl in Shift)
      then XAxis.ZoomAroundPixel(X, Power(1.01, (X - FLastMouseCursorPos.X)))
      else
     if (maShift in YAxis.FMouseActions)
      then XAxis.ShiftByPixel(X - FLastMouseCursorPos.X);
    end;
  end;

 inherited;

 FLastMouseCursorPos := Point(X, Y);
end;

function TCustomGuiEQGraph.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
 if (maZoom in YAxis.FMouseActions) and (MousePos.X < 32)
  then YAxis.ZoomAroundPixel(MousePos.Y, Power(1.01, WheelDelta));

 Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

end.
