unit DAV_GuiDialRenderer;

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
  Classes, Graphics, Forms, SysUtils, Controls, Contnrs, DAV_Classes,
  DAV_GuiCommon, DAV_GuiBaseControl, DAV_GuiDial;

type
  {$IFDEF DELPHI10_UP} {$region 'Dial Renderer'} {$ENDIF}

  ////////////////////
  //  Dial Renderer //
  ////////////////////

  // Custom Primitives

  TCustomGuiDialPrimitiveClass = class of TCustomGuiDialPrimitive;
  TCustomGuiDialPrimitive = class(TPersistent)
  private
    FZoom     : Single;
    FTag      : Integer;
    FVisible  : Boolean;
    FOnChange : TNotifyEvent;
    procedure SetZoom(const Value: Single);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    property Zoom: Single read FZoom write SetZoom;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: Longint read FTag write FTag default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomGuiDialPrimitiveBasic = class(TCustomGuiDialPrimitive)
  private
    FColor    : TColor;
    FDiffuse  : Single;
    FSpecular : Single;
    procedure SetColor(const Value: TColor);
    procedure SetDiffuse(const Value: Single);
    procedure SetSpecular(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    property Color: TColor read FColor write SetColor;
    property Diffuse: Single read FDiffuse write SetDiffuse;
    property Specular: Single read FSpecular write SetSpecular;
  end;

  TCustomGuiDialPrimitiveLine = class(TCustomGuiDialPrimitiveBasic)
  private
    FLengh: Single;
    FWidth: Single;
    procedure SetLength(const Value: Single);
    procedure SetWidth(const Value: Single);
  public
    constructor Create; override;
  published
    property Width: Single read FWidth write SetWidth;
    property Length: Single read FLengh write SetLength;
  end;

  TCustomGuiDialPrimitiveAspectLine = class(TCustomGuiDialPrimitiveLine)
  private
    FAspect: Single;
    procedure SetAspect(const Value: Single);
  public
    constructor Create; override;
  published
    property Aspect: Single read FAspect write SetAspect;
  end;

  TCustomGuiDialPrimitiveFrame = class(TCustomGuiDialPrimitiveBasic)
  private
    FAspect     : Single;
    FFramwWidth : Single;
    procedure SetAspect(const Value: Single);
    procedure SetFramwWidth(const Value: Single);
  public
    constructor Create; override;
    property Aspect: Single read FAspect write SetAspect;
    property FramwWidth: Single read FFramwWidth write SetFramwWidth;
  end;

  TCustomGuiDialPrimitiveFill = class(TCustomGuiDialPrimitiveBasic)
  private
    FAspect        : Single;
    FTexture       : TBitmap;
    FTextureDepth  : Single;
    FTextureZoom   : Single;
    procedure SetAspect(const Value: Single);
    procedure SetTexture(const Value: TBitmap);
    procedure SetTextureDepth(const Value: Single);
    procedure SetTextureZoom(const Value: Single);
  public
    constructor Create; override;

    property Aspect: Single read FAspect write SetAspect;
    property Texture: TBitmap read FTexture write SetTexture;
    property TextureDepth: Single read FTextureDepth write SetTextureDepth;
    property TextureZoom: Single read FTextureZoom write SetTextureZoom;
  end;

  TCustomGuiDialPrimitiveEmbossFill = class(TCustomGuiDialPrimitiveFill)
  private
    FEmboss        : Single;
    FEmbossDiffuse : Single;
    procedure SetEmboss(const Value: Single);
    procedure SetEmbossDiffuse(const Value: Single);
  public
    constructor Create; override;

    property Emboss: Single read FEmboss write SetEmboss;
    property EmbossDiffuse: Single read FEmbossDiffuse write SetEmbossDiffuse;
  end;



  // Primitives

  TGuiDialPrimitiveNone = class(TCustomGuiDialPrimitive)
  published
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveImageTransparency = (itAlphaLayer, itOpaque, itFirstPixel);
  TGuiDialPrimitiveImage = class(TCustomGuiDialPrimitive)
  private
    FBitmap           : TBitmap;
    FTransparency     : TGuiDialPrimitiveImageTransparency;
    FIntelligentAlpha : Byte;
    FAutoFitToRect    : Boolean;
    FGlyphCount       : Integer;
    FStitchKind       : TGuiStitchKind;
    procedure SetAutoFitToRect(const Value: Boolean);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetIntelligentAlpha(const Value: Byte);
    procedure SetGlyphCount(const Value: Integer);
    procedure SetStitchKind(const Value: TGuiStitchKind);
    procedure SetTransparency(const Value: TGuiDialPrimitiveImageTransparency);
  public
    constructor Create; override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Transparency: TGuiDialPrimitiveImageTransparency read FTransparency write SetTransparency default itAlphaLayer;
    property IntelligentAlpha: Byte read FIntelligentAlpha write SetIntelligentAlpha default 1;
    property AutoFitToRect: Boolean read FAutoFitToRect write SetAutoFitToRect default False;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 1;
    property StitchKind: TGuiStitchKind read FStitchKind write SetStitchKind default skHorizontal;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFrameCircle = class(TCustomGuiDialPrimitiveFrame)
  published
    property Aspect;
    property Diffuse;
    property FramwWidth;
    property Specular;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillCircle = class(TCustomGuiDialPrimitiveEmbossFill)
  published
    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveMetalCircle = class(TCustomGuiDialPrimitiveEmbossFill)
  private
    FAmbient : Single;
    procedure SetAmbient(const Value: Single);
  public
    constructor Create; override;
  published
    property Ambient: Single read FAmbient write SetAmbient;
    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillWave = class(TCustomGuiDialPrimitiveFill)
  private
    FAngleStep : Single;
    FDepth     : Single;
    procedure SetDepth(const Value: Single);
    procedure SetAngleStep(const Value: Single);
  public
    constructor Create; override;
  published
    property AngleStep: Single read FAngleStep write SetAngleStep;
    property Depth: Single read FDepth write SetDepth;

    property Aspect;
    property Diffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillSphere = class(TCustomGuiDialPrimitiveEmbossFill)
  private
    FAmbient        : Single;
    FSpecularWidth  : Single;
    FLightDirection : Single;
    procedure SetAmbient(const Value: Single);
    procedure SetSpecularWidth(const Value: Single);
    procedure SetLightDirection(const Value: Single);
  public
    constructor Create; override;
  published
    property Ambient: Single read FAmbient write SetAmbient;
    property SpecularWidth: Single read FSpecularWidth write SetSpecularWidth;
    property LightDirection: Single read FLightDirection write SetLightDirection;

    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFrameRect = class(TCustomGuiDialPrimitiveFrame)
  private
    FRound : Single;
    procedure SetRound(const Value: Single);
  public
    constructor Create; override;
  published
    property Round: Single read FRound write SetRound;

    property Aspect;
    property Diffuse;
    property FramwWidth;
    property Specular;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveFillRect = class(TCustomGuiDialPrimitiveEmbossFill)
  private
    FRound : Single;
    procedure SetRound(const Value: Single);
  public
    constructor Create; override;
  published
    property Round: Single read FRound write SetRound;

    property Aspect;
    property Diffuse;
    property Emboss;
    property EmbossDiffuse;
    property Specular;
    property Texture;
    property TextureDepth;
    property TextureZoom;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveTriangle = class(TCustomGuiDialPrimitiveLine)
  private
    FTexture      : TBitmap;
    FTextureDepth : Single;
    FTextureZoom  : Single;
    procedure SetTexture(const Value: TBitmap);
    procedure SetTextureDepth(const Value: Single);
    procedure SetTextureZoom(const Value: Single);
  public
    constructor Create; override;
  published
    property Texture: TBitmap read FTexture write SetTexture;
    property TextureDepth: Single read FTextureDepth write SetTextureDepth;
    property TextureZoom: Single read FTextureZoom write SetTextureZoom;
    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveLine = class(TCustomGuiDialPrimitiveLine)
  published
    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveRadiateLine = class(TCustomGuiDialPrimitiveAspectLine)
  private
    FAngleStep : Single;
    procedure SetAngleStep(const Value: Single);
  public
    constructor Create; override;
  published
    property AngleStep: Single read FAngleStep write SetAngleStep;

    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveStrippedLines = class(TCustomGuiDialPrimitiveAspectLine)
  private
    FStep : Single;
    procedure SetStep(const Value: Single);
  public
    constructor Create; override;
  published
    property Step: Single read FStep write SetStep;

    property Diffuse;
    property Length;
    property Specular;
    property Tag;
    property Visible;
    property Width;
    property Zoom;
  end;

  TGuiDialPrimitiveText = class(TCustomGuiDialPrimitive)
  private
    FText      : string;
    FFont      : TFont;
    FAlignment : TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Text: string read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Tag;
    property Visible;
    property Zoom;
  end;

  TGuiDialPrimitiveShape = class(TCustomGuiDialPrimitiveFill)
  private
    FWidth : Single;
    FShape : string;
    FFill  : Boolean;
    procedure SetFill(const Value: Boolean);
    procedure SetShape(const Value: string);
    procedure SetWidth(const Value: Single);
  public
    constructor Create; override;

    property Shape: string read FShape write SetShape;
    property Width: Single read FWidth write SetWidth;
    property Fill: Boolean read FFill write SetFill;
  end;

  TGuiDialLayerCollectionItem = class;

  TGuiDialLayerCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TGuiDialLayerCollectionItem; virtual;
    procedure SetItem(Index: Integer; const Value: TGuiDialLayerCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property Items[Index: Integer]: TGuiDialLayerCollectionItem read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGuiDialLayerCollectionItem;
    function Insert(Index: Integer): TGuiDialLayerCollectionItem;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TGuiDialLayerCollectionItem = class(TCollectionItem)
  private
    FOnChange              : TNotifyEvent;
    FDisplayName           : string;
    FPrimitive             : TCustomGuiDialPrimitive;
    FPrimitiveClassChanged : TNotifyEvent;
    function GetPrimitiveClassName: string;
    procedure SetPrimitive(const Value: TCustomGuiDialPrimitive);
    procedure SetPrimitiveClassName(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DisplayName;
    property PrimitiveClassName: string read GetPrimitiveClassName write SetPrimitiveClassName;
    property Primitive: TCustomGuiDialPrimitive read FPrimitive write SetPrimitive;
    property PrimitiveClassChanged: TNotifyEvent read FPrimitiveClassChanged write FPrimitiveClassChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGuiDialImageRenderer = class(TComponent)
  private
    FLayers    : TGuiDialLayerCollection;
    FOnChanged : TNotifyEvent;
    function GetCount: Integer;
    function GetItems(Index: Integer): TGuiDialLayerCollectionItem;
  protected
    property Items[Index: Integer]: TGuiDialLayerCollectionItem read GetItems; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderDial(Bitmap: TBitmap; StichCount: Integer);
  published
    property Layers: TGuiDialLayerCollection read FLayers write FLayers;
    property Count: Integer read GetCount;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TGuiDialRendered = class(TCustomGuiDial)
  private
    FRenderer : TGuiDialImageRenderer;
    procedure SetRenderer(const Value: TGuiDialImageRenderer);
  protected
    procedure RendererChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Renderer: TGuiDialImageRenderer read FRenderer write SetRenderer;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

var
  PrimitiveClassList: TClassList;

implementation

{$IFDEF DELPHI10_UP} {$region 'Dial Renderer'} {$ENDIF}

{ TGuiDialLayerCollection }

constructor TGuiDialLayerCollection.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TGuiDialLayerCollectionItem);
end;

function TGuiDialLayerCollection.Add: TGuiDialLayerCollectionItem;
begin
 Result := TGuiDialLayerCollectionItem(inherited Add);
end;

procedure TGuiDialLayerCollection.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

function TGuiDialLayerCollection.GetItem(Index: Integer): TGuiDialLayerCollectionItem;
begin
 Result := TGuiDialLayerCollectionItem(inherited GetItem(Index));
end;

function TGuiDialLayerCollection.Insert(Index: Integer): TGuiDialLayerCollectionItem;
begin
 Result:= TGuiDialLayerCollectionItem(inherited Insert(Index));
end;

procedure TGuiDialLayerCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
 inherited;
 // add things that depend on the order here!
end;

procedure TGuiDialLayerCollection.SetItem(Index: Integer;
  const Value: TGuiDialLayerCollectionItem);
begin
 inherited SetItem(Index, Value);
end;


{ TGuiDialLayerCollectionItem }

constructor TGuiDialLayerCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FPrimitive := TGuiDialPrimitiveNone.Create;
 FDisplayName := 'Layer ' + IntToStr(Index + 1);
end;

destructor TGuiDialLayerCollectionItem.Destroy;
begin
 if assigned(FPrimitive) then FreeAndNil(FPrimitive);
 inherited;
end;

function TGuiDialLayerCollectionItem.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

function TGuiDialLayerCollectionItem.GetPrimitiveClassName: string;
begin
 if assigned(FPrimitive)
  then Result := FPrimitive.ClassName
  else Result := '';
end;

procedure TGuiDialLayerCollectionItem.SetDisplayName(const Value: string);
begin
 if FDisplayName <> Value then
  begin
   FDisplayName := Value;
   inherited;
  end;
end;

procedure TGuiDialLayerCollectionItem.SetPrimitive(const Value: TCustomGuiDialPrimitive);
begin
 FPrimitive.Assign(Value);
end;

procedure TGuiDialLayerCollectionItem.SetPrimitiveClassName(const Value: string);
var
  PrimitiveClass: TCustomGuiDialPrimitiveClass;
begin
 if (Value <> '') and (FPrimitive.ClassName <> Value) and Assigned(PrimitiveClassList) then
  begin
   PrimitiveClass := TCustomGuiDialPrimitiveClass(PrimitiveClassList.Find(Value));
   if Assigned(PrimitiveClass) then
    begin
     FPrimitive.Free;
     FPrimitive := PrimitiveClass.Create;
//     Changed;
    end;
  end;
end;


{ TCustomGuiDialPrimitive }

constructor TCustomGuiDialPrimitive.Create;
begin
 inherited;
 FZoom := 100;
end;

procedure TCustomGuiDialPrimitive.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiDialPrimitive then
  with TCustomGuiDialPrimitive(Dest) do
   begin
    FZoom    := Self.FZoom;
    FVisible := Self.FVisible;
    FTag     := Self.FTag;
   end
 else inherited;
end;

procedure TCustomGuiDialPrimitive.Changed;
begin

end;

procedure TCustomGuiDialPrimitive.SetVisible(const Value: Boolean);
begin
 if FVisible <> Value then
  begin
   FVisible := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitive.SetZoom(const Value: Single);
begin
 if FZoom <> Value then
  begin
   FZoom := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveImage }

constructor TGuiDialPrimitiveImage.Create;
begin
 inherited;
 FTransparency     := itAlphaLayer;
 FIntelligentAlpha := 1;
 FAutoFitToRect    := False;
 FGlyphCount        := 1;
 FStitchKind       := skHorizontal;
end;

procedure TGuiDialPrimitiveImage.SetAutoFitToRect(const Value: Boolean);
begin
 if FAutoFitToRect <> Value then
  begin
   FAutoFitToRect := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetBitmap(const Value: TBitmap);
begin
 if FBitmap <> Value then
  begin
   FBitmap := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetIntelligentAlpha(const Value: Byte);
begin
 if FIntelligentAlpha <> Value then
  begin
   FIntelligentAlpha := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetGlyphCount(const Value: Integer);
begin
 if FGlyphCount <> Value then
  begin
   FGlyphCount := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetStitchKind(const Value: TGuiStitchKind);
begin
 if FStitchKind <> Value then
  begin
   FStitchKind := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveImage.SetTransparency(
  const Value: TGuiDialPrimitiveImageTransparency);
begin
 if FTransparency <> Value then
  begin
   FTransparency := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveText }

constructor TGuiDialPrimitiveText.Create;
begin
 inherited;
 FText := '(1:99)';
 FFont := TFont.Create;
 FAlignment := taCenter;
end;

destructor TGuiDialPrimitiveText.Destroy;
begin
 FreeAndNil(FFont);
 inherited;
end;

procedure TGuiDialPrimitiveText.SetAlignment(const Value: TAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveText.SetFont(const Value: TFont);
begin
 if FFont <> Value then
  begin
   FFont.Assign(Value);
   Changed;
  end;
end;

procedure TGuiDialPrimitiveText.SetText(const Value: string);
begin
 if FText <> Value then
  begin
   FText := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveBasic }

constructor TCustomGuiDialPrimitiveBasic.Create;
begin
 inherited;
 FColor := clRed;
end;

procedure TCustomGuiDialPrimitiveBasic.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomGuiDialPrimitiveBasic then
  with TCustomGuiDialPrimitiveBasic(Dest) do
   begin
    FColor := Self.Color;
   end;
end;

procedure TCustomGuiDialPrimitiveBasic.SetColor(const Value: TColor);
begin
 if FColor <> Value then
  begin
   FColor := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveBasic.SetDiffuse(const Value: Single);
begin
 if FDiffuse <> Value then
  begin
   FDiffuse := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveBasic.SetSpecular(const Value: Single);
begin
 if FSpecular <> Value then
  begin
   FSpecular := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveLine }

constructor TCustomGuiDialPrimitiveLine.Create;
begin
 inherited;
 FLengh := 50;
 FWidth := 10;
end;

procedure TCustomGuiDialPrimitiveLine.SetLength(const Value: Single);
begin
 if FLengh <> Value then
  begin
   FLengh := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveLine.SetWidth(const Value: Single);
begin
 if FWidth <> Value then
  begin
   FWidth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveTriangle }

constructor TGuiDialPrimitiveTriangle.Create;
begin
 inherited;
 FTextureDepth := 0;
 FTextureZoom  := 100;
end;

procedure TGuiDialPrimitiveTriangle.SetTexture(const Value: TBitmap);
begin
 if FTexture <> Value then
  begin
   FTexture := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveTriangle.SetTextureDepth(const Value: Single);
begin
 if FTextureDepth <> Value then
  begin
   FTextureDepth := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveTriangle.SetTextureZoom(const Value: Single);
begin
 if FTextureZoom <> Value then
  begin
   FTextureZoom := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveAspectLine }

constructor TCustomGuiDialPrimitiveAspectLine.Create;
begin
 inherited;
 FAspect := 0;
end;

procedure TCustomGuiDialPrimitiveAspectLine.SetAspect(const Value: Single);
begin
 if FAspect <> Value then
  begin
   FAspect := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveRadiateLine }

constructor TGuiDialPrimitiveRadiateLine.Create;
begin
 inherited;
 FAngleStep := 45;
end;

procedure TGuiDialPrimitiveRadiateLine.SetAngleStep(const Value: Single);
begin
 if FAngleStep <> Value then
  begin
   FAngleStep := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveStrippedLines }

constructor TGuiDialPrimitiveStrippedLines.Create;
begin
 inherited;
 FStep := 20;
end;

procedure TGuiDialPrimitiveStrippedLines.SetStep(const Value: Single);
begin
 if FStep <> Value then
  begin
   FStep := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveFrame }

constructor TCustomGuiDialPrimitiveFrame.Create;
begin
 inherited;
 FAspect     := 0;
 FDiffuse    := 0;
 FFramwWidth := 0;
 FSpecular   := 0;
end;

procedure TCustomGuiDialPrimitiveFrame.SetAspect(const Value: Single);
begin
 if FAspect <> Value then
  begin
   FAspect := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFrame.SetFramwWidth(const Value: Single);
begin
 if FFramwWidth <> Value then
  begin
   FFramwWidth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFrameRect }

constructor TGuiDialPrimitiveFrameRect.Create;
begin
 inherited;
 FRound := 0;
end;

procedure TGuiDialPrimitiveFrameRect.SetRound(const Value: Single);
begin
 if FRound <> Value then
  begin
   FRound := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveFill }

constructor TCustomGuiDialPrimitiveFill.Create;
begin
 inherited;
 FAspect        := 0;
 FDiffuse       := 0;
 FSpecular      := 0;
 FTextureDepth  := 0;
 FTextureZoom   := 100;
end;

procedure TCustomGuiDialPrimitiveFill.SetAspect(const Value: Single);
begin
 if FAspect <> Value then
  begin
   FAspect := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFill.SetTexture(const Value: TBitmap);
begin
 if FTexture <> Value then
  begin
   FTexture := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFill.SetTextureDepth(const Value: Single);
begin
 if FTextureDepth <> Value then
  begin
   FTextureDepth := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveFill.SetTextureZoom(const Value: Single);
begin
 if FTextureZoom <> Value then
  begin
   FTextureZoom := Value;
   Changed;
  end;
end;


{ TCustomGuiDialPrimitiveEmbossFill }

constructor TCustomGuiDialPrimitiveEmbossFill.Create;
begin
 inherited;
 FEmboss        := 0;
 FEmbossDiffuse := 0;
end;

procedure TCustomGuiDialPrimitiveEmbossFill.SetEmboss(const Value: Single);
begin
 if FEmboss <> Value then
  begin
   FEmboss := Value;
   Changed;
  end;
end;

procedure TCustomGuiDialPrimitiveEmbossFill.SetEmbossDiffuse(
  const Value: Single);
begin
 if FEmbossDiffuse <> Value then
  begin
   FEmbossDiffuse := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFillSphere }

constructor TGuiDialPrimitiveFillSphere.Create;
begin
 inherited;
 FAmbient := 50;
 FSpecularWidth := 50;
 FLightDirection := -50;
end;

procedure TGuiDialPrimitiveFillSphere.SetAmbient(const Value: Single);
begin
 if FAmbient <> Value then
  begin
   FAmbient := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveFillSphere.SetLightDirection(const Value: Single);
begin
 if FLightDirection <> Value then
  begin
   FLightDirection := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveFillSphere.SetSpecularWidth(const Value: Single);
begin
 if FSpecularWidth <> Value then
  begin
   FSpecularWidth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveMetalCircle }

constructor TGuiDialPrimitiveMetalCircle.Create;
begin
 inherited;
 FAmbient := 50;
end;

procedure TGuiDialPrimitiveMetalCircle.SetAmbient(const Value: Single);
begin
 if FAmbient <> Value then
  begin
   FAmbient := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFillRect }

constructor TGuiDialPrimitiveFillRect.Create;
begin
 inherited;
 FRound := 0;
end;

procedure TGuiDialPrimitiveFillRect.SetRound(const Value: Single);
begin
 if FRound <> Value then
  begin
   FRound := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveFillWave }

constructor TGuiDialPrimitiveFillWave.Create;
begin
 inherited;
 FAngleStep := 45;
 FDepth     := 10;
end;

procedure TGuiDialPrimitiveFillWave.SetAngleStep(const Value: Single);
begin
 if FAngleStep <> Value then
  begin
   FAngleStep := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveFillWave.SetDepth(const Value: Single);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   Changed;
  end;
end;


{ TGuiDialPrimitiveShape }

constructor TGuiDialPrimitiveShape.Create;
begin
 inherited;
 FFill := True;
end;

procedure TGuiDialPrimitiveShape.SetFill(const Value: Boolean);
begin
 if FFill <> Value then
  begin
   FFill := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveShape.SetShape(const Value: string);
begin
 if FShape <> Value then
  begin
   FShape := Value;
   Changed;
  end;
end;

procedure TGuiDialPrimitiveShape.SetWidth(const Value: Single);
begin
 if FWidth <> Value then
  begin
   FWidth := Value;
   Changed;
  end;
end;


{ TGuiDialImageRenderer }

constructor TGuiDialImageRenderer.Create(AOwner: TComponent);
begin
 inherited;
 FLayers := TGuiDialLayerCollection.Create(Self);
end;

destructor TGuiDialImageRenderer.Destroy;
begin
 FreeAndNil(FLayers);
 inherited;
end;

function TGuiDialImageRenderer.GetCount: Integer;
begin
 Result := FLayers.Count;
end;

function TGuiDialImageRenderer.GetItems(
  Index: Integer): TGuiDialLayerCollectionItem;
begin
 if (Index >= 0) and (Index < FLayers.Count)
  then Result := FLayers[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TGuiDialImageRenderer.RenderDial(Bitmap: TBitmap; StichCount: Integer);
var
  LayerIndex : Integer;
begin
 for LayerIndex := 0 to FLayers.Count - 1 do
  begin
//   FLayers[LayerIndex].FPrimitive
  end;
end;

procedure RegisterPrimitiveClass(PrimitiveClass: TCustomGuiDialPrimitiveClass);
begin
  if not Assigned(PrimitiveClassList) then PrimitiveClassList := TClassList.Create;
  PrimitiveClassList.Add(PrimitiveClass);
end;

{ TGuiDialRendered }

constructor TGuiDialRendered.Create(AOwner: TComponent);
begin
 inherited;

end;

procedure TGuiDialRendered.RendererChanged;
begin
 if Assigned(FRenderer)
  then FRenderer.RenderDial(FDialBitmap, GlyphCount)
  else
   with FDialBitmap.Canvas
    do FillRect(ClipRect);
end;

procedure TGuiDialRendered.SetRenderer(const Value: TGuiDialImageRenderer);
begin
 if FRenderer <> Value then
  begin
   FRenderer := Value;
   RendererChanged;
  end;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

initialization
  // register primitive classes
  RegisterPrimitiveClass(TGuiDialPrimitiveNone);
  RegisterPrimitiveClass(TGuiDialPrimitiveImage);
  RegisterPrimitiveClass(TGuiDialPrimitiveFrameCircle);
  RegisterPrimitiveClass(TGuiDialPrimitiveFrameRect);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillCircle);
  RegisterPrimitiveClass(TGuiDialPrimitiveMetalCircle);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillSphere);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillWave);
  RegisterPrimitiveClass(TGuiDialPrimitiveFillRect);
  RegisterPrimitiveClass(TGuiDialPrimitiveLine);
  RegisterPrimitiveClass(TGuiDialPrimitiveTriangle);
  RegisterPrimitiveClass(TGuiDialPrimitiveRadiateLine);
  RegisterPrimitiveClass(TGuiDialPrimitiveStrippedLines);
  RegisterPrimitiveClass(TGuiDialPrimitiveText);

finalization
  PrimitiveClassList.Free;

end.
