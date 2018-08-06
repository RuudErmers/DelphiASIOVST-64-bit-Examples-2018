unit URMCBaseControlPanel;

interface

uses
  System.SysUtils, System.Classes, Windows, Messages, Vcl.Controls, Vcl.Forms,Vcl.Graphics,Types,URMCEmptyPanel,URMCControls, UKnobEditor,URMCConstants, Generics.Collections ;

type
  TonClicked  = procedure (Sender:TObject;index,value:integer) of object;
  TOnChanged = procedure (Sender:TObject;index,value:integer) of object;

  TRMCBaseControlPanelCustom = class (TWinControlRMC)
  private
     defaultDimensionW,defaultDimensionH:integer;
     HeaderGeneratesMouseEvent:boolean;
     FonClicked: TonClicked;
     FonChanged: TonChanged;
     FRMCElements    : TList<TRMCElement>;
     FCaption:string;
     FScalable:boolean;
     FonKnobEdit:TonKnobEdit;
     FcachedElement:TRMCElement;
     procedure OnClickCallback(Sender: TObject);
     procedure AddElement(el: TRMCElement);
     procedure OnChangedCallback(Sender: TObject; index, value: integer);
protected
     FBasePanel:TWinControl;
     DebugName:string;
     FKnobEditor:TKnobEditor;
     function GetKnobEditor:TKnobEditor;override;
     function GetElement(index: integer): TRMCElement;
     procedure GetDefaultDimensions(var w, h: integer);
     function CreatePitchKnob(index:integer;position:TRect): TRMCElement ;
     function CreateKnob(index:integer;position:TRect): TRMCElement ;
     function Create7SegElement(index: integer;      position: TRect): TRMCElement;
     function CreateRolandSlider(index: integer; position: TRect): TRMCElement;
// REMOVED 27-10-2017     function CreateRolandInfo(index: integer; position: TRect): TRMCElement;
     function CreateRolandKnob(index: integer;   position: TRect): TRMCElement;
     function CreateTwinkle(index: integer; position: TRect): TRMCElement;
     function CreateLed(index:integer;position:TRect):  TRMCElement;
     function CreateText(index:integer;position:TRect): TRMCElement;
     function CreateHeader(index:integer;position:TRect):TRMCElement;
     function CreateButton(index:integer;position:Trect):TRMCElement;
     function CreateButtonEx(index:integer;position:Trect;parent: TWinControl):TRMCElement;
     procedure SetCaption(value:string);
     procedure SetonKnobEdit(value:TonKnobEdit);
     property onKnobEdit:TonKnobEdit read FonKnobEdit write SetonKnobEdit;
     property Caption: string read FCaption write SetCaption;
  public
     procedure Changed(Sender:TRMCElement);
     procedure setTextLabel(index:integer;s:string);
     procedure SetDefaultDimensions(w,h:integer);
     procedure setHeaderLabel(index:integer;s:string);
     procedure setLedColors(index: integer; cloff,clon: TColor);
     procedure setValue(index,value:integer);virtual;
     procedure setMinMax(index,amin,amax:integer);
     procedure setShape(index:integer;value:TRMCKnobShape);
     procedure setVisible(index:integer;visible:boolean);
     procedure setDefaultOnChangeHandler;

     procedure setLed(index:integer;value:boolean);
     function getLed(index:integer):boolean;
     function getValue(index:integer):integer;

     procedure setTextColor(index: integer; value: TColor);
     procedure setSliderColor(index: integer; value: TColor);

     procedure setButton(index:integer;value:boolean);
     procedure setButtonText(index:integer;value:string);

     constructor Create(AOwner: TComponent); override;
     Destructor Destroy; Override;
     procedure SetBounds(left,top,width,height:integer);override;
     procedure KnobEditEnd;

     procedure Load(aBasePanel:TWinControl); virtual;
     function Control2Bitmap: TBitmap;
     property OnClicked: TonClicked read FonClicked write FonClicked;
     property OnChanged: TonChanged read FonChanged write FonChanged;
     property BasePanel:TWinControl read FBasePanel;
     property Scalable: boolean read FScalable write FScalable default true;

    end;
  TRMCBaseControlPanel = class (TRMCBaseControlPanelCustom)
  public
  published
     property OnChanged;
     property onKnobEdit;
     property Align;
     property Caption;
     property Visible;
     property OnClicked;
  end;

implementation

{ TRMCBaseControlPanel }

procedure TRMCBaseControlPanelCustom.setShape(index:integer;value:TRMCKnobShape);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el<>NIL then
    el.Shape:=value;
end;

procedure TRMCBaseControlPanelCustom.setValue(index, value: integer);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el<>NIL then
   el.Value:=value;
end;

procedure TRMCBaseControlPanelCustom.setVisible(index: integer;  visible: boolean);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el<>NIL then
   el.Visible:=visible;
end;

procedure TRMCBaseControlPanelCustom.setDefaultOnChangeHandler;
VAR el:TRMCElement;
     i:integer;
begin
  for i:=0 to 127 do
  begin
    el:=GetElement(i);
    if (el<>NIL) and (not assigned(el.OnChanged)) then
    el.OnChanged:=OnChangedCallback;
  end;
end;

procedure TRMCBaseControlPanelCustom.setHeaderLabel(index: integer; s: string);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el<>NIL then
   el.Caption:=s;
end;

procedure TRMCBaseControlPanelCustom.setButton(index: integer; value: boolean);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    el.value:=ord(value);
end;

procedure TRMCBaseControlPanelCustom.setButtonText(index: integer; value: string);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    el.Caption:=value;
end;

procedure TRMCBaseControlPanelCustom.SetDefaultDimensions(w, h: integer);
begin
  defaultDimensionW:=w;
  defaultDimensionH:=h;
  Width:=w;
  Height:=h;
end;

procedure TRMCBaseControlPanelCustom.setTextColor(index: integer; value: TColor);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    el.Font.Color:=value;
end;

procedure TRMCBaseControlPanelCustom.setLed(index: integer; value: boolean);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    el.Value:=ord(value);
end;

procedure TRMCBaseControlPanelCustom.setLedColors(index: integer; cloff,  clon: TColor);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then with el do
  begin
    ButtonColorOff:=cloff;
    ButtonColorOn:=clon;
  end;
end;

procedure TRMCBaseControlPanelCustom.setSliderColor(index: integer; value: TColor);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    el.SliderColor:=value;
end;

procedure TRMCBaseControlPanelCustom.setTextLabel(index: integer; s: string);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    el.SetCaption(s);
end;

procedure TRMCBaseControlPanelCustom.setMinMax(index, amin, amax: integer);
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then with el do
  begin
    minvalue:=amin;
    maxvalue:=amax;
  end;
end;

procedure TRMCBaseControlPanelCustom.SetonKnobEdit(value: TonKnobEdit);
begin
  FOnKnobEdit:=value;
end;

function TRMCBaseControlPanelCustom.CreateButton(index: integer;  position: Trect):TRMCElement;
begin
  result:=CreateButtonEx(index,position,BasePanel);
end;

function TRMCBaseControlPanelCustom.CreateButtonEx(index: integer;  position: Trect; parent: TWinControl): TRMCElement;
begin
  result:=TRMCElement.Create(parent);
  result.Parent:=parent;
  result.Position:=position;
  result.visible:=true;
  result.Index:=index;
  result.OnClick:=OnClickCallback;
  result.OnChanged:=OnChangedCallback;
  result.ButtonColorOff:=clBlack;
  result.ButtonColorOn:=clRed;
  result.Shape:=trLedButton;
  AddElement(result);
end;

function TRMCBaseControlPanelCustom.CreateHeader(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=trText;
end;

function TRMCBaseControlPanelCustom.CreateKnob(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=trKnob;
end;

function TRMCBaseControlPanelCustom.CreatePitchKnob(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=trPitchKnob;
end;


function TRMCBaseControlPanelCustom.Create7SegElement(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.TextWithSeg7:=true;
  result.shape := tkNone;
end;

function TRMCBaseControlPanelCustom.CreateRolandSlider(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=tkSlider;
end;

(*
function TRMCBaseControlPanelCustom.CreateRolandInfo(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=tkText;
  result.TextWithSeg7:=true;
end; *)

function TRMCBaseControlPanelCustom.CreateRolandKnob(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=tkValue;
end;

function TRMCBaseControlPanelCustom.CreateLed(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=trLed;
end;

function TRMCBaseControlPanelCustom.CreateTwinkle(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=trTwinkle;
end;


function TRMCBaseControlPanelCustom.CreateText(index: integer; position: TRect):TRMCElement;
begin
  result:=CreateButton(index,position);
  result.Shape:=trText;
end;

function TRMCBaseControlPanelCustom.GetElement(index: integer): TRMCElement;
  function _GetKnobElement(win:TWinControl):TRMCElement;
  VAR i:integer;
  begin
    result:=NIL;
    for i:=0 to win.ControlCount-1 do
    if win.Controls[i] is TRMCElement then
    begin
      if (win.Controls[i] as TRMCElement).Index = index then
        begin result:=win.Controls[i] as TRMCElement; exit; end;
    end
    else if win.Controls[i] is TWinControl then
      begin result:=_GetKnobElement(TWinControl(win.Controls[i])); if result<>NIL then exit; end;
  end;
VAR i:integer;
begin
  if FcachedElement<>NIL then if FcachedElement.index = index then begin result:=FcachedElement; exit; end;
  for i:=0 to FRMCElements.Count-1 do
    if (FRMCElements[i].index = index)
      then  begin FcachedElement:=FRMCElements[i]; result:=FcachedElement; exit;end;
  FcachedElement:=_GetKnobElement(BasePanel);
  result:=FcachedElement;
end;

function TRMCBaseControlPanelCustom.GetKnobEditor: TKnobEditor;
begin
  if assigned(FOnKnobEdit) then
    result:=FKnobEditor
  else if (parent is TRMCBaseControlPanelCustom) then
     result:=TRMCBaseControlPanelCustom(parent).GetKnobEditor
  else
    result:=NIL;
end;

function TRMCBaseControlPanelCustom.getLed(index: integer): boolean;
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    result:=el.Value<>0;
end;

function TRMCBaseControlPanelCustom.getValue(index:integer): integer;
VAR el:TRMCElement;
begin
  el:=GetElement(index);
  if el <> NIL then
    result:=el.Value
  else
    result:=0;
end;

procedure TRMCBaseControlPanelCustom.GetDefaultDimensions(var w, h: integer);
begin
  w:=defaultDimensionW;
  h:=defaultDimensionH;
end;

procedure TRMCBaseControlPanelCustom.OnClickCallback(Sender:TObject);
begin
  if assigned(FOnClicked) then with TRMCElement(Sender) do
    OnClicked(Sender,Index,Value);
end;


procedure TRMCBaseControlPanelCustom.OnChangedCallback(Sender:TObject;index,value:integer);
begin
  if assigned(FOnChanged) then
    FOnChanged(Sender,index,value);
end;

procedure TRMCBaseControlPanelCustom.KnobEditEnd;
begin
  FKnobEditor.EditKeyEnd;
end;

procedure TRMCBaseControlPanelCustom.Load(aBasePanel:TWinControl);
VAR i:integer;
    el:TRMCElement;
begin
  FBasePanel:=aBasePanel;
  for i:=0 to BasePanel.ControlCount-1 do
   if BasePanel.Controls[i] is TRMCElement then
   begin
     el:=TRMCElement(BasePanel.Controls[i]);
     if el.position.width>0 then continue;
     el.position:=Rect(el.left,el.top,el.width+el.left,el.top+el.height);
     AddElement(el);
   end;
end;

procedure TRMCBaseControlPanelCustom.SetBounds(left, top, width, height: integer);
VAR i,w,h:integer;
begin
  inherited;
  if not Scalable then begin w:=width;h:=height; end
  else
  begin
    GetDefaultDimensions(w,h);
    if (w<0.3) or (h<0.3) then exit;
  end;
  for i:=0 to FRMCElements.Count-1 do
    FRMCElements[i].SetScale(width/w, height / h);
end;

procedure TRMCBaseControlPanelCustom.AddElement(el: TRMCElement);
VAR i:integer;
begin
  if el.Index>0 then
  begin
    for i:=0 to FRMCElements.Count-1 do
     if (FRMCElements[i].index = el.index) then
        OutputDebugString(PChar('Add Element: Duplicate Entry..'+DebugName+' '+inttostr(el.Index)));
  end;
  FRMCElements.Add(el);
end;

procedure TRMCBaseControlPanelCustom.Changed(Sender: TRMCElement);
begin
  OnChangedCallback(Sender,Sender.Index,Sender.Value);
end;

function TRMCBaseControlPanelCustom.Control2Bitmap: TBitmap;
begin
  Result := TBitmap.Create;
  with Result do begin
    Height := BasePanel.Height;
    Width  := BasePanel.Width;
    Canvas.Handle := CreateDC(nil, nil, nil, nil);
    Canvas.Lock;
    BasePanel.PaintTo(Canvas.Handle, 0, 0);
    Canvas.Unlock;
    DeleteDC(Canvas.Handle);
  end;
end;

constructor TRMCBaseControlPanelCustom.Create(AOwner: TComponent);
begin
  FRMCElements:=TList<TRMCElement>.Create;
  FScalable:=true;
  inherited Create(Aowner);
  DoubleBuffered:=true;
  FKnobEditor:=TKnobEditor.Create(self);
  Width:=100;
  Height:=100;
end;

destructor TRMCBaseControlPanelCustom.Destroy;
begin
  FRMCElements.Free;
  FKnobEditor.Free;
  inherited;
end;


procedure TRMCBaseControlPanelCustom.SetCaption(value: string);
begin
  FCaption:=value;
  if (BasePanel is  TRMCEmptyPanel)
    then TRMCEmptyPanel(BasePanel).Caption:=value;
end;

initialization
end.





