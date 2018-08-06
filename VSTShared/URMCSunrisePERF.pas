unit URMCSunrisePERF;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, URMCControls,URMCConstants,URMCBaseControlPanel,
  Vcl.StdCtrls,URMCSunriseControlPanel;

type
  TRMCSunrisePERFFrame = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TRMCSunrisePERF = class (TRMCSunriseControlPanel)
  private
     frame: TRMCSunrisePERFFrame;
     FCurEffect:integer;
     FEffectsEnabled:boolean;
     FDelayKnob:TRMCElement;
     FDelayLabel,FTimeLabel:TLabel;
     FEffectValue:array[0..15] of integer;
     FCaptionPanel:TPanel;
     procedure Load(aBasePanel:TWinControl); override;
    procedure UIChanged(Sender: TObject; Index, Value: Integer);
    procedure setEffect(index: integer;fromButton:boolean);
    procedure setEffectValue(index,value: integer;fromButton:boolean);
    procedure updateEffectValues;
    function knobToIndex(knob: integer): integer;
    procedure setEffectsEnabled(enabled, fromButton: boolean);
    procedure sendEffectsStatus;
    procedure EffectsClick(Sender: TObject);
  public
    constructor Create(aowner:TComponent);override;
    procedure ChangePCC(pcc, Value: Integer);override;
  end;

var
  RMCSunrisePERFFrame: TRMCSunrisePERFFrame;

implementation

{$R *.dfm}

uses UVirtCC;

const ID_LED = 10;
const ID_KNOB = 20;
const ID_BUTTON = 30;

const EFFECT_DELAY = 0;

const VirtCC_SHOWEFFECTS = 1026;

procedure TRMCSunrisePERF.ChangePCC(pcc, Value: Integer);
begin
  case pcc of
    VirtCC_GLIDE_RANGE:  setValue(ID_KNOB,value);
    VirtCC_WHEEL_MOD:    setValue(ID_KNOB+1,value);
    VirtCC_NOISE_COLOR:  setValue(ID_KNOB+2,value);
    VirtCC_Glide_OnOff:  setValue(ID_BUTTON,value);
    VirtCC_POLYMODE:     setValue(ID_BUTTON+1,value);
    VirtCC_EFFECTSETTINGS: begin
                             setEffect(value and 3,false);
                             setEffectsEnabled(value and 4 = 4,false);
                           end;
  end;
  if (pcc>=VirtCC_Effects0) and (pcc<=VirtCC_Effects0+15) then
    setEffectValue(pcc-VirtCC_Effects0,value,false);
end;

constructor TRMCSunrisePERF.Create(aowner: TComponent);
begin
  DebugName:='TRMCSunriseLFO';
  inherited Create(aowner);
  Parent:=TWinControl(aowner); // changed

  Scalable:=false;
  frame:=TRMCSunrisePERFFrame.Create(self);
  frame.Width:=594;
  frame.Height:=133;
  width:=frame.width;
  height:=frame.height;
  frame.parent:=self;
  frame.Visible:=true;
  frame.align:=alClient;
  Load(frame);
  setDefaultOnChangeHandler;
  OnChanged:=UIChanged;
  setEffect(0,false);
end;


procedure TRMCSunrisePERF.Load(aBasePanel: TWinControl);
VAR i:integer;
const DEF_LEFT = 110;
const DEF_WIDTH = 62;
const effText:array[0..3] of string = ('DELAY','REVERB','CHORUS','PHASER');
VAR x,y:integer;
begin
  inherited;
  FCaptionPanel:=CreateControlPanel(Rect(15,7,74,22),'CONTROL');

  x:=DEF_LEFT;
  y:=40;
  CreateControl(x,y,tsKnob,ID_KNOB);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'GLIDE');
  x:=DEF_LEFT+DEF_WIDTH;
  CreateControl(x,y,tsKnob,ID_KNOB+1);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'WHEEL');

  x:=DEF_LEFT+2*DEF_WIDTH;
  CreateControl(x,y,tsKnob,ID_KNOB+2);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'COLOR');

  x:=DEF_LEFT+4*DEF_WIDTH;
  FDelayKnob:=CreateControl(x,y,tkNone,ID_KNOB+3);
  FDelayLabel:=CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'TIME');

  x:=DEF_LEFT+5*DEF_WIDTH;
  CreateControl(x,y,tsKnob,ID_KNOB+4);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'DEPTH');
  x:=DEF_LEFT+6*DEF_WIDTH;
  CreateControl(x,y,tsKnob,ID_KNOB+5);
  FTimeLabel:=CreateControlText(Rect(x-30,y+28,x+30,y+28+12),'TIME');

  y:=88;
  x:=DEF_LEFT;
  CreateControl(x,116,tsButton,ID_BUTTON);
  CreateControlText(Rect(x-20,y,x+20,y+12),'ENABLE');
  x:=DEF_LEFT+1*DEF_WIDTH;
  CreateControl(x,116,tsButton,ID_BUTTON+1);
  CreateControlText(Rect(x-20,y,x+20,y+12),'POLY');
  x:=DEF_LEFT+3*DEF_WIDTH;
  CreateControl(x,116,tsButton,ID_BUTTON+2);
  CreateControlText(Rect(x-30,y,x+30,y+12),'EFFECTS').OnClick:=EffectsClick;
  for i:=0 to 3 do
  begin
    x:=DEF_LEFT+(4+i)*DEF_WIDTH;
    CreateControl(x,116,tsButton,ID_BUTTON+3+i);
    CreateControlText(Rect(x-25,y,x+25,y+12),effText[i]);
  end;

end;

procedure TRMCSunrisePERF.EffectsClick(Sender:TObject);
begin
  genPCC(VirtCC_SHOWEFFECTS,0);
end;

procedure TRMCSunrisePERF.sendEffectsStatus;
begin
  genPCC(VirtCC_EFFECTSETTINGS,FcurEffect+4 * ord(FEffectsEnabled));
end;


procedure TRMCSunrisePERF.setEffect(index:integer;fromButton:boolean);
VAR i:integer;
    const lblText:array[0..3] of string = ('FEEDBACK','FEEDBACK','RATE','RATE');
begin
  FCurEffect:=index;
  for i:=0 to 3 do setValue(ID_BUTTON+i+3,127*ord(index = i));
  FDelayKnob.Visible:=FCurEffect<2;
  FDelayLabel.Visible:=FCurEffect<2;
  FTimeLabel.Caption:=lblText[FCurEffect];
  updateEffectValues;
  if  fromButton then sendEffectsStatus;
end;

function TRMCSunrisePERF.knobToIndex(knob:integer):integer;
// index follows Midi Virt Spec
const tbl:array[0..11] of integer = (1,0,8,3,2,9,-1,4,5,-1,6,7);
begin
  result:=tbl[3*FCurEffect+knob];
end;

procedure TRMCSunrisePERF.updateEffectValues;
VAR i,index: integer;
  function TimeStr(value:integer):string;
  const tstr:array[0..23] of string = ( '1/16','1/8T','1/8', '1/4T' ,'1/8D',
                                 '1/4', '5/16','1/2T' ,'1/4D', '5/12','7/16',
                                 '1/2', '9/16','7/12','5/8', '2/3' ,'11/16',
                                 '1/2D','13/16','5/6' ,'7/8','11/12','15/16','1');
  begin
    if value<5 then result:='Off'
    else
    begin
      value:=round((value-5)*127 / 122);
      index:=round((value+0.1)/5.5);
      result:=tstr[index];
    end
  end;
  function CheckDelayTime(index,value:integer):integer;
  begin
    result:=value;
    if (FCurEffect = EFFECT_DELAY ) and (index=0) then
      result:=round(value*96/127);
  end;
begin
  if FcurEffect = EFFECT_DELAY then
  begin
    FDelayLabel.Caption:=TimeStr(FEffectValue[1]);
    FDelayKnob.MaxValue:=96;
  end
  else
  begin
    FDelayLabel.Caption:='TIME';
    FDelayKnob.MaxValue:=127;
  end;
  for i:=0 to 2 do
  begin
    index:=knobToIndex(i);
    if index<>-1 then
      setValue(ID_KNOB+3+i,CheckDelayTime(i,FEffectValue[index]));
  end;
end;

procedure TRMCSunrisePERF.setEffectsEnabled(enabled,fromButton:boolean);
begin
  FEffectsEnabled:=enabled;
  if not fromButton then setValue(ID_BUTTON+2,127*ord(enabled))
                    else sendEffectsStatus;
end;


procedure TRMCSunrisePERF.setEffectValue(index, value: integer;fromButton:boolean);
// index follows Midi Virt Spec
begin
  FEffectValue[index]:=value;
  UpdateEffectValues;
  if fromButton then genPCC(VirtCC_Effects0+index,value);
end;

procedure TRMCSunrisePERF.UIChanged(Sender: TObject; Index, Value: Integer);
VAR i,ind:integer;
  function CheckDelayTime(index,value:integer):integer;
  begin
    result:=value;
    if (FCurEffect = EFFECT_DELAY ) and (index=0) then
      result:=round(value*127/96);
  end;
begin
  case index of
    ID_KNOB:    genPCC(VirtCC_Glide_Range,value);
    ID_KNOB+1:  genPCC(VirtCC_WHEEL_MOD,value);
    ID_KNOB+2:  genPCC(VirtCC_NOISE_COLOR,value);
    ID_BUTTON:  genPCC(VirtCC_Glide_OnOff,value);
    ID_BUTTON+1:genPCC(VirtCC_POLYMODE,value);
    ID_BUTTON+2:setEffectsEnabled(value and 4 = 4,true);
  end;
  for i:=0 to 2 do if index = ID_KNOB+3+i then
  begin
    ind:=knobToIndex(i);
    if ind<>-1 then setEffectValue(ind,CheckDelayTime(i,value),true);
  end;

  if ( Index>=ID_BUTTON+3) and (index<=ID_BUTTON+6) then
    setEffect(index-ID_BUTTON-3,true);
end;

end.