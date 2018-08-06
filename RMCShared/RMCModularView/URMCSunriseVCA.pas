unit URMCSunriseVCA;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, URMCControls,URMCConstants,URMCBaseControlPanel,
  URMCSunriseControlPanel;

type
  TRMCSunriseVCAFrame = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TRMCSunriseVCA = class (TRMCSunriseControlPanel)
  private
     frame: TRMCSunriseVCAFrame;
     FLfo: integer;
     FCaptionPanel:TPanel;
     procedure Load(aBasePanel:TWinControl); override;
    procedure UIChanged(Sender: TObject; Index, Value: Integer);
    procedure toggleLFO(bit: integer);
    procedure setLFO(value:integer;fromButton: boolean);
  public
    constructor Create(aowner:TComponent);override;
    procedure ChangePCC(pcc, Value: Integer);override;
  end;

var
  RMCSunriseVCAFrame: TRMCSunriseVCAFrame;

implementation

{$R *.dfm}

uses UVirtCC;

const ID_LED = 10;
const ID_KNOB = 20;
const ID_BUTTON = 30;
const ID_SLIDER = 40;

constructor TRMCSunriseVCA.Create(aowner: TComponent);
begin
  DebugName:='TRMCSunriseOSC';
  inherited Create(aowner);
  Parent:=TWinControl(aowner); // changed

  Scalable:=false;
  frame:=TRMCSunriseVCAFrame.Create(self);
  frame.Width:=594;
  frame.Height:=190;
  width:=frame.width;
  height:=frame.height;
  frame.parent:=self;
  frame.Visible:=true;
  frame.align:=alClient;
  Load(frame);
  setDefaultOnChangeHandler;
  OnChanged:=UIChanged;
end;

procedure TRMCSunriseVCA.ChangePCC(pcc, Value: Integer);
begin
  case pcc of
    VirtCC_VCA_MODULATION_DEPTH:  setValue(ID_KNOB+3,value);
    VirtCC_VCA_RANGE:             setValue(ID_KNOB+4,value);
    VirtCC_VCA_MODULATION_SELECT: setLFO(round(3*Value/127),false);
    VirtCC_VCA_ADSR_A:            setValue(ID_SLIDER,value);
    VirtCC_VCA_ADSR_D:            setValue(ID_SLIDER+1,value);
    VirtCC_VCA_ADSR_S:            setValue(ID_SLIDER+2,value);
    VirtCC_VCA_ADSR_R:            setValue(ID_SLIDER+3,value);
    VirtCC_OSC1_VOLUME:           setValue(ID_SLIDER+4,value);
    VirtCC_OSC2_VOLUME:           setValue(ID_SLIDER+5,value);
    VirtCC_OSC3_VOLUME:           setValue(ID_SLIDER+6,value);
    VirtCC_OSC4_VOLUME:           setValue(ID_SLIDER+7,value);
    VirtCC_NOISE_VOLUME:          setValue(ID_SLIDER+8,value);
    VirtCC_RingModLevel:          setValue(ID_SLIDER+9,value);
  end;
end;


procedure TRMCSunriseVCA.Load(aBasePanel: TWinControl);
VAR i:integer;
const DEF_LEFT = 110;
const DEF_WIDTH = 62;
VAR x,y:integer;
const adsr: string = 'ADSR';
const mixer: array[0..5] of string = ('OSC1','OSC2','OSC3','OSC4','NOISE','RING');
begin
  inherited;
  FCaptionPanel:=CreateControlPanel(Rect(15,7,54,22),'VCA');
  x:=35;
  for i:=0 to 5 do
  begin
    CreateControl(x+42*i,96,tsSlider,ID_SLIDER+i+4);
    CreateControlText(Rect(x+42*i-15,168,x+42*i+15,168+10),mixer[i]);
  end;

  x:=DEF_LEFT+3*DEF_WIDTH;
  y:=96;
  CreateControl(x,y,tsKnob,ID_KNOB+3);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'MOD');
  x:=DEF_LEFT+4*DEF_WIDTH;
  CreateControl(x,y,tsKnob,ID_KNOB+4);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'ADSR');

  CreateControl(DEF_LEFT+3*DEF_WIDTH,17,tsButton,ID_BUTTON);
  CreateControl(DEF_LEFT+3*DEF_WIDTH,43,tsButton,ID_BUTTON+1);

  x:=418;
  for i:=0 to 3 do
  begin
    CreateControl(x+46*i,96,tsSlider,ID_SLIDER+i);
    CreateControlText(Rect(x+46*i-10,168,x+46*i+10,168+10),adsr[i+1]);
  end;

end;

procedure TRMCSunriseVCA.toggleLFO(bit:integer);
begin
  setLFO(FLfo xor (1 shl bit),true);
end;

procedure TRMCSunriseVCA.UIChanged(Sender: TObject; Index, Value: Integer);
begin
  case index of
    ID_KNOB+3: genPCC(VirtCC_VCA_MODULATION_DEPTH,value);
    ID_KNOB+4: genPCC(VirtCC_VCA_RANGE,value);
    ID_BUTTON: toggleLFO(0);
    ID_BUTTON+1:toggleLFO(1);
    ID_SLIDER: genPCC(VirtCC_VCA_ADSR_A,value);
    ID_SLIDER+1: genPCC(VirtCC_VCA_ADSR_D,value);
    ID_SLIDER+2: genPCC(VirtCC_VCA_ADSR_S,value);
    ID_SLIDER+3: genPCC(VirtCC_VCA_ADSR_R,value);
    ID_SLIDER+4: genPCC(VirtCC_OSC1_VOLUME,value);
    ID_SLIDER+5: genPCC(VirtCC_OSC2_VOLUME,value);
    ID_SLIDER+6: genPCC(VirtCC_OSC3_VOLUME,value);
    ID_SLIDER+7: genPCC(VirtCC_OSC4_VOLUME,value);
    ID_SLIDER+8: genPCC(VirtCC_NOISE_VOLUME,value);
    ID_SLIDER+9: genPCC(VirtCC_RingModLevel,value);
  end;
end;

procedure TRMCSunriseVCA.setLFO(value:integer;fromButton: boolean);
begin
  Flfo:=value;
  setValue(ID_BUTTON,     127*ord(Flfo and 1 = 1));
  setValue(ID_BUTTON + 1, 127*ord(Flfo and 2 = 2));
  if fromButton then
    genPCC(VirtCC_VCA_MODULATION_SELECT,round(value*127/3));
end;

end.
