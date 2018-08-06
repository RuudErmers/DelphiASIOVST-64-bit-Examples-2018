unit URMCSunriseOSC;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, URMCControls,URMCConstants,URMCBaseControlPanel,
  URMCSunriseControlPanel;

type
  TRMCSunriseOSCFrame = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TRMCSunriseOSC = class (TRMCSunriseControlPanel)
  private
    frame: TRMCSunriseOSCFrame;
    FCurLed,FCurWave:integer;
    FOsc:integer;
    FLfos:array[0..1] of integer;
    FSync:boolean;
    procedure Load(aBasePanel:TWinControl); override;
    procedure setLed(index: integer;fromButton:boolean);
    procedure UIChanged(Sender: TObject; Index, Value: Integer);
    procedure nextLed;
    procedure setWave(index: integer; fromButton: boolean);
    procedure toggleLFO(lfo, bit: integer);
    procedure setLFO(lfo,value:integer;fromButton: boolean);
    procedure setSync(sync:boolean;fromButton: boolean);
    procedure oscDeOffset(var pcc, osc: integer);
    function oscOffset(pcc: integer): integer;
    procedure genPCC(pcc, value: integer);override;
    procedure toggleSync;
  public
    constructor Create(aowner:TComponent;osc:integer);
    procedure ChangePCC(pcc, Value: Integer);override;

  end;

var
  RMCSunriseOSCFrame: TRMCSunriseOSCFrame;

implementation

{$R *.dfm}

uses UVirtCC;

const ID_LED = 10;
const ID_KNOB = 20;
const ID_BUTTON = 30;
const VirtCC_OSC1_DETUNE=VirtCC_OSC2_DETUNE+ VirtCC_OSC1_FOOT - VirtCC_OSC2_FOOT;


procedure TRMCSunriseOSC.oscDeOffset(VAR pcc,osc:integer);
const d = VirtCC_OSC2_FOOT-VirtCC_OSC1_FOOT;
begin
  osc:= (pcc - VirtCC_OSC1_FOOT) DIV d;
  pcc:= pcc - osc * d;
end;

function TRMCSunriseOSC.oscOffset(pcc:integer):integer;
const d = VirtCC_OSC2_FOOT-VirtCC_OSC1_FOOT;
begin
  result:=pcc+ Fosc * d;
end;

procedure TRMCSunriseOSC.ChangePCC(pcc, Value: Integer);
VAR osc:integer;
begin
  if (pcc = VirtCC_ALL_FAT) and (Fosc=0) then
  begin
    setValue(ID_KNOB+3,value);
    exit;
  end;
  if (pcc>=VirtCC_OSC1_SYNC) and (pcc<VirtCC_OSC1_SYNC+4) then
  begin
      if pcc - VirtCC_OSC1_SYNC = Fosc then
        setValue(ID_BUTTON+9,value);
      exit;
  end;
  oscDeOffset(pcc,osc);
  if osc <> FOsc then exit;
  case pcc of
    VirtCC_OSC1_FOOT:             setLed( round(6*(127-Value) / 127),false);
    VirtCC_OSC1_DETUNE:           setValue(ID_KNOB+3,value);
    VirtCC_OSC1_WAVE:             setWave( round(7*Value / 127),false);
    VirtCC_OSC1_SEMI:             setValue(ID_KNOB,value);
    VirtCC_OSC1_PHASEDEPTH:       setValue(ID_KNOB+1,value);
    VirtCC_OSC1_MODULATION_DEPTH: setValue(ID_KNOB+2,value);
//    VirtCC_ALL_FAT:               setValue(ID_KNOB+3,value);
    VirtCC_OSC1_PHASESELECT:      setLFO(0,round(3*Value/127),false);
    VirtCC_OSC1_MODULATION_SELECT:setLFO(1,round(3*Value/127),false);
  end;
end;

constructor TRMCSunriseOSC.Create(aowner: TComponent;osc:integer);
begin
  DebugName:='TRMCSunriseOSC';
  FOsc:=osc;
  inherited Create(aowner);
  Parent:=TWinControl(aowner); // changed
  Scalable:=false;
  frame:=TRMCSunriseOSCFrame.Create(self);
  frame.Width:=335;
  frame.Height:=190;
  width:=frame.width;
  height:=frame.height;
  frame.parent:=self;
  frame.Visible:=true;
  frame.align:=alClient;
  Load(frame);
  setDefaultOnChangeHandler;
  OnChanged:=UIChanged;
  setLed(3,false);
  setWave(0,false);
end;

procedure TRMCSunriseOSC.Load(aBasePanel: TWinControl);
VAR i:integer;
const foottext:array[0..6] of string = ('1"','2"','4"','8"','16"','32"','64"');
const DEF_LEFT = 110;
const DEF_WIDTH = 62;
VAR x,y:integer;
    s:string;
begin
  inherited;
  CreateControlPanel(Rect(15,7,54,22),'OSC'+inttostr(FOsc+1));
  for i:=0 to 6 do
  begin
    CreateControl(20,45+17 * i,tsLed,i+ID_LED);
    CreateControlText(Rect(30,38+17*i,52,48+17*i),foottext[i],taRightJustify);
  end;
  CreateControl(31,172,tsButton,ID_BUTTON+8,1);

  x:=DEF_LEFT; y:=96;
  CreateControl(x,y,tsKnob,ID_KNOB);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'SEMI');
  x:=DEF_LEFT+DEF_WIDTH; y:=96;
  CreateControl(x,y,tsKnob,ID_KNOB+1);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'PWM');
  x:=DEF_LEFT+2*DEF_WIDTH; y:=96;
  CreateControl(x,y,tsKnob,ID_KNOB+2);
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),'MOD');

  x:=DEF_LEFT+3*DEF_WIDTH; y:=96;
  CreateControl(x,y,tsKnob,ID_KNOB+3);
  if Fosc = 0 then s:='FAT' else s:='DETUNE';
  CreateControlText(Rect(x-20,y+28,x+20,y+28+12),s);

  CreateControl(DEF_LEFT+DEF_WIDTH,17,tsButton,ID_BUTTON);
  CreateControl(DEF_LEFT+DEF_WIDTH,43,tsButton,ID_BUTTON+1);
  CreateControl(DEF_LEFT+2*DEF_WIDTH,17,tsButton,ID_BUTTON+2);
  CreateControl(DEF_LEFT+2*DEF_WIDTH,43,tsButton,ID_BUTTON+3);
  CreateControl(DEF_LEFT+3*DEF_WIDTH,17,tsButton,ID_BUTTON+9);
  x:=DEF_LEFT+3*DEF_WIDTH; y:=38;
  if odd(Fosc) then s:='SYNC' else s:='WTF';

  CreateControlText(Rect(x-20,y,x+20,y+12),s);

  CreateControl(DEF_LEFT,172,tsButton,ID_BUTTON+4);
  CreateControl(DEF_LEFT+DEF_WIDTH,172,tsButton,ID_BUTTON+5);
  CreateControl(DEF_LEFT+2*DEF_WIDTH,172,tsButton,ID_BUTTON+6);
  CreateControl(DEF_LEFT+3*DEF_WIDTH,172,tsButton,ID_BUTTON+7);

  CreateControlWave(DEF_LEFT,155,0);
  CreateControlWave(DEF_LEFT+DEF_WIDTH,155,1);
  CreateControlWave(DEF_LEFT+2*DEF_WIDTH,155,2);
  CreateControlWave(DEF_LEFT+3*DEF_WIDTH,155,3);
end;

procedure TRMCSunriseOSC.genPCC(pcc,value:integer);
begin
  if pcc < VirtCC_OSC2_FOOT then pcc:=oscOffset(pcc);
  inherited genPCC(pcc,value);
end;

procedure TRMCSunriseOSC.setLed(index:integer;fromButton:boolean);
VAR i:integer;
begin
  FCurLed:=index;
  for i:=0 to 6 do setValue(ID_LED+i,127*ord(index = i));
  if fromButton then
    genPCC(VirtCC_OSC1_FOOT,127-round(127*FCurLed/6));
end;

procedure TRMCSunriseOSC.setWave(index:integer;fromButton:boolean);
VAR i:integer;
begin
  FCurWave:=index;
  for i:=0 to 3 do setValue(ID_BUTTON+i+4,127*ord(index = i));
  if fromButton then
    genPCC(VirtCC_OSC1_WAVE,round(127*FCurWave/7));
end;


procedure TRMCSunriseOSC.nextLed;
begin
  setLed((FCurLed+6) MOD 7,true);
end;

procedure TRMCSunriseOSC.toggleLFO(lfo,bit:integer);
begin
  setLFO(lfo,FLfos[lfo] xor (1 shl bit), true);
end;

procedure TRMCSunriseOSC.toggleSync;
begin
  setSync(not FSync, true);
end;

procedure TRMCSunriseOSC.UIChanged(Sender: TObject; Index, Value: Integer);
VAR ind:integer;
begin
  case index of
    ID_KNOB  : genPCC(VirtCC_OSC1_SEMI,value);
    ID_KNOB+1: genPCC(VirtCC_OSC1_PHASEDEPTH,value);
    ID_KNOB+2: genPCC(VirtCC_OSC1_MODULATION_DEPTH,value);
    ID_KNOB+3: if FOsc=0 then genPCC(VirtCC_ALL_FAT,value)
                         else genPCC(VirtCC_OSC1_DETUNE,value)
  end;
  if index = ID_BUTTON+8 then nextLed;
  if ( Index>=ID_LED) and (index<=ID_LED+6) then
    setLed(index-ID_LED,true);
  if ( Index>=ID_BUTTON+4) and (index<=ID_BUTTON+7) then
    setWave(index-ID_BUTTON-4,true);
  if ( Index>=ID_BUTTON) and (index<=ID_BUTTON+3) then
  begin
    ind:=index - ID_BUTTON;
    toggleLFO(ind DIV 2, ind MOD 2);
  end;
  if index = ID_BUTTON+9 then toggleSync;
end;


procedure TRMCSunriseOSC.setLFO(lfo,value: integer; fromButton: boolean);
begin
  Flfos[lfo]:=value;
  setValue(ID_BUTTON+ 2 * lfo, 127*ord(value and 1 = 1));
  setValue(ID_BUTTON+ 2 * lfo + 1, 127*ord(value and 2 = 2));
  if fromButton then
  case lfo of
    0: genPCC(VirtCC_OSC1_PHASESELECT,round(127*value/3));
    1: genPCC(VirtCC_OSC1_MODULATION_SELECT,round(127*value/3));
  end;
end;

procedure TRMCSunriseOSC.setSync(sync, fromButton: boolean);
begin
  FSync:=sync;
  if frombutton then
    genPCC(VirtCC_OSC1_SYNC+Fosc,127*(ord(sync)));
end;

end.
