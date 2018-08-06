unit URMCCrumarView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,URMCEmptyPanel,URMCVSTView,
  URMCBaseControlPanel,URMCControls,Vcl.StdCtrls,UCrumarViewFrame;

type TPanelarray = array[0..9] of record index,cc:integer end;
  TRMCCrumarView = class (TRMCVSTView)
  private
    frame:TCrumarViewFrame;
    FValues:array[0..127] of integer;
    FCurEffect:integer;
    FUseEffects:boolean;
    FOsc1Toggled,FOsc2Toggled,FModToggled:boolean;
    procedure Load(aBasePanel:TCustomPanel);
    procedure UIChanged(Sender: TObject; Index, Value: Integer);
    procedure OnClick(Sender: TObject);
    procedure UpdatePanel(panel: TRMCEmptyPanel; caption: string;  panelArray: TPanelarray);
    function ToggleUseEffects: integer;
    procedure UpdateEffectView;
    function SetCurEffect(index: integer): integer;
    function SetUseEffects(value: integer): integer;
    procedure EffectsClick(Sender: TObject);
  protected
  public
    procedure ChangePCC(pcc,value: Integer); override;
   constructor create(aowner: TComponent);override;
   procedure Centre(w,h:integer);

 end;

implementation

uses System.Types,UVirtCC;

const VirtCC_SHOWEFFECTS = 1026;

const PanelArrayOsc1 : TPanelarray = ((index:XRECDS2CC_OSC1WAVE;  cc:XRECDS2CC_OSC1WAVE),
                                      (index:XRECDS2CC_OSC1FOOT;  cc:XRECDS2CC_OSC1FOOT),
                                      (index:XRECDS2CC_OSC1SEMI;  cc:XRECDS2CC_OSC1SEMI),
                                      (index:XRECDS2CC_OSC1LEVEL; cc:XRECDS2CC_OSC1LEVEL),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1));

const PanelArrayOsc2 : TPanelarray = ((index:XRECDS2CC_OSC2WAVE;  cc:XRECDS2CC_OSC2WAVE),
                                      (index:XRECDS2CC_OSC2FOOT;  cc:XRECDS2CC_OSC2FOOT),
                                      (index:XRECDS2CC_OSC2SEMI;  cc:XRECDS2CC_OSC2SEMI),
                                      (index:XRECDS2CC_OSC2LEVEL; cc:XRECDS2CC_OSC2LEVEL),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1));

const PanelArrayOsc3 : TPanelarray = ((index:XRECDS2CC_OSC1WAVE;  cc:XRECDS2CC_OSC3WAVE),
                                      (index:XRECDS2CC_OSC1FOOT;  cc:XRECDS2CC_OSC3FOOT),
                                      (index:XRECDS2CC_OSC1SEMI;  cc:XRECDS2CC_OSC3SEMI),
                                      (index:XRECDS2CC_OSC1LEVEL; cc:XRECDS2CC_OSC3LEVEL),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1));

const PanelArrayOsc4 : TPanelarray = ((index:XRECDS2CC_OSC2WAVE;  cc:XRECDS2CC_OSC4WAVE),
                                      (index:XRECDS2CC_OSC2FOOT;  cc:XRECDS2CC_OSC4FOOT),
                                      (index:XRECDS2CC_OSC2SEMI;  cc:XRECDS2CC_OSC4SEMI),
                                      (index:XRECDS2CC_OSC2LEVEL; cc:XRECDS2CC_OSC4LEVEL),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1),
                                      (index:-1;cc: -1),(index:-1;cc: -1),(index:-1;cc: -1));

const PanelArrayMod1 : TPanelarray = ((index:XRECDS2CC_OSC1MODLEVEL;  cc:XRECDS2CC_OSC1MODLEVEL),
                                      (index:XRECDS2CC_OSC1MODSELECT;  cc:XRECDS2CC_OSC1MODSELECT),
                                      (index:XRECDS2CC_OSC2MODLEVEL;  cc:XRECDS2CC_OSC2MODLEVEL),
                                      (index:XRECDS2CC_OSC2MODSELECT;  cc:XRECDS2CC_OSC2MODSELECT),
                                      (index:XRECDS2CC_VCFMODLEVEL;  cc:XRECDS2CC_VCFMODLEVEL),
                                      (index:XRECDS2CC_VCFMODSELECT;  cc:XRECDS2CC_VCFMODSELECT),
                                      (index:XRECDS2CC_VCAMODLEVEL;  cc:XRECDS2CC_VCAMODLEVEL),
                                      (index:XRECDS2CC_VCAMODSELECT;  cc:XRECDS2CC_VCAMODSELECT),
                                      (index:XRECDS2CC_PWM1MODLEVEL;  cc:XRECDS2CC_PWM1MODLEVEL),
                                      (index:XRECDS2CC_PWM1MODSELECT; cc:XRECDS2CC_PWM1MODSELECT));

const PanelArrayMod2 : TPanelarray = ((index:XRECDS2CC_OSC1MODLEVEL;  cc:XRECDS2CC_OSC3MODLEVEL),
                                      (index:XRECDS2CC_OSC1MODSELECT;  cc:XRECDS2CC_OSC3MODSELECT),
                                      (index:XRECDS2CC_OSC2MODLEVEL;  cc:XRECDS2CC_OSC4MODLEVEL),
                                      (index:XRECDS2CC_OSC2MODSELECT;  cc:XRECDS2CC_OSC4MODSELECT),
                                      (index:XRECDS2CC_VCFMODLEVEL;  cc:XRECDS2CC_PWM2MODLEVEL),
                                      (index:XRECDS2CC_VCFMODSELECT;  cc:XRECDS2CC_PWM2MODSELECT),
                                      (index:XRECDS2CC_VCAMODLEVEL;  cc:XRECDS2CC_PWM3MODLEVEL),
                                      (index:XRECDS2CC_VCAMODSELECT;  cc:XRECDS2CC_PWM3MODSELECT),
                                      (index:XRECDS2CC_PWM1MODLEVEL;  cc:XRECDS2CC_PWM4MODLEVEL),
                                      (index:XRECDS2CC_PWM1MODSELECT; cc:XRECDS2CC_PWM4MODSELECT));

constructor TRMCCrumarView.Create(aowner: TComponent);
begin
  DebugName:='TCrumarView';
  inherited ;
  parent:=TWinControl(aowner);
  Scalable:=false;
  FCurEffect:=0;
  frame:=TCrumarViewFrame.Create(self);
  Width:=frame.Width;
  Height:=frame.Height;
  SetDefaultDimensions(frame.Width,frame.Height);
  frame.parent:=self;
  frame.Visible:=true;
  frame.align:=alClient;
  Load(frame.RMCEmptyPanel1);
  OnPCCChanged:=NIL;
  frame.RMCPanelOSC1.OnClick:=OnClick;
  frame.RMCPanelOSC2.OnClick:=OnClick;
  frame.RMCPanelMod.OnClick:=OnClick;
  frame.LabelEff0.OnClick:=EffectsClick;
  frame.LabelEff1.OnClick:=EffectsClick;
end;

procedure TRMCCrumarView.EffectsClick(Sender:TObject);
begin
  if assigned (OnPCCChanged) then
     OnPCCChanged(self,VirtCC_SHOWEFFECTS,0);
end;

procedure TRMCCrumarView.UpdatePanel(panel:TRMCEmptyPanel;caption:string;panelArray:TPanelarray);
VAR i:integer;
begin
  panel.caption:=caption;
  for i:=0 to 9 do with panelArray[i] do
    if index<>-1 then
      SetValue(index,FValues[cc]);
end;

procedure TRMCCrumarView.OnClick(Sender:TObject);
  procedure SetMixerLabels(s1,s2,s3,s4,s5:string);
  begin
    frame.LabelOSC1.Caption:=s1;
    frame.LabelOSC2.Caption:=s2;
    frame.LabelVCF.Caption:=s3;
    frame.LabelVCA.Caption:=s4;
    frame.LabelPWM.Caption:=s5;
  end;
begin
  if Sender = frame.RMCPanelOSC1 then
  begin
    FOsc1Toggled:=not FOsc1Toggled;
    if not FOsc1Toggled then
      UpdatePanel(frame.RMCPanelOSC1,'OSC 1',PanelArrayOsc1)
    else
      UpdatePanel(frame.RMCPanelOSC1,'OSC 3',PanelArrayOsc3);
  end;
  if Sender = frame.RMCPanelOSC2 then
  begin
    FOsc2Toggled:=not FOsc2Toggled;
    if not FOsc2Toggled then
      UpdatePanel(frame.RMCPanelOSC2,'OSC 2',PanelArrayOsc2)
    else
      UpdatePanel(frame.RMCPanelOSC2,'OSC 4',PanelArrayOsc4);
  end;
  if Sender = frame.RMCPanelMod then
  begin
    FModToggled:=not FModToggled;
    if not FModToggled then
    begin
      UpdatePanel(frame.RMCPanelMod,'LFOs Mixer',PanelArrayMod1);
      SetMixerLabels('OSC 1','OSC 2','VCF','VCA','PWM');
    end
    else
    begin
      UpdatePanel(frame.RMCPanelMod,'LFOs Mixer (2)',PanelArrayMod2);
      SetMixerLabels('OSC 3','OSC 4','PWM 2','PWM 3','PWM 4');
    end;
  end;

end;

procedure TRMCCrumarView.Load(aBasePanel:TCustomPanel);
begin
  inherited Load(aBasePanel);
  setDefaultOnChangeHandler;
  OnChanged:=UIChanged;
end;

procedure TRMCCrumarView.UIChanged(Sender: TObject; Index,Value: Integer);
  procedure GetCC(panelArray:TPanelarray; VAR index:integer);
  VAR i:integer;
  begin
    for i:=0 to 9 do
      if panelArray[i].index = index then index:=panelArray[i].cc;
  end;
begin
  if not FOsc1Toggled then
    GetCC(PanelArrayOsc1,index)
  else
    GetCC(PanelArrayOsc3,index);
  if not FOsc2Toggled then
    GetCC(PanelArrayOsc2,index)
  else
    GetCC(PanelArrayOsc4,index);
  if not FModToggled then
    GetCC(PanelArrayMod1,index)
  else
    GetCC(PanelArrayMod2,index);

(* we should react to XRECDS2CC_DELAYAMOUNT and XRECDS2CC_DELAYAMOUNT+1 *)
   case index of
     XRECDS2CC_DELAYAMOUNT,
     XRECDS2CC_DELAYAMOUNT+1: index:=index + 2 * FcurEffect;

     XRECDS2CC_DELAYAMOUNT+2,
     XRECDS2CC_DELAYAMOUNT+3,
     XRECDS2CC_DELAYAMOUNT+4,
     XRECDS2CC_DELAYAMOUNT+5: begin value:=SetCurEffect(index -XRECDS2CC_DELAYAMOUNT-2);index:=XRECDS2CC_EFFECTSETTINGS; end;
(* we should react to XRECDS2CC_EFFECTSETTINGS *)
     XRECDS2CC_EFFECTSETTINGS: value:=ToggleUseEffects;
   end;
  FValues[Index]:=value;
  if assigned (OnPCCChanged) then OnPCCChanged(self,index,value);
end;

function TRMCCrumarView.ToggleUseEffects:integer;
begin
  FUseEffects:=not FUseEffects;
  UpdateEffectView;
  result:=FCurEffect+4*ord(FUseEffects);
end;

function TRMCCrumarView.SetUseEffects(value:integer):integer;  // value from host, select effect and update XRECDS2CC_EFFECTSETTINGS in UI
begin
  FuseEffects:=value and 4 = 4;
  FCurEffect:=value and 3;
  UpdateEffectView;
end;
function TRMCCrumarView.SetCurEffect(index:integer):integer; // index from UI, generate full value for XRECDS2CC_EFFECTSETTINGS
begin
  FCurEffect:=index;
  UpdateEffectView;
  result:=FCurEffect+4*ord(FUseEffects);
end;

procedure TRMCCrumarView.UpdateEffectView;
VAR i:integer;
begin
  SetValue(XRECDS2CC_EFFECTSETTINGS,127*ord(FuseEffects));
  for i:=0 to 3 do
    SetValue(XRECDS2CC_DELAYAMOUNT+2+i,127*ord(i=FCurEffect));
  SetValue(XRECDS2CC_DELAYAMOUNT,FValues[XRECDS2CC_DELAYAMOUNT+2*FCurEffect]);
  SetValue(XRECDS2CC_DELAYAMOUNT+1,FValues[XRECDS2CC_DELAYAMOUNT+2*FCurEffect+1]);
end;

procedure TRMCCrumarView.Centre(w, h: integer);
begin
  frame.align:=alNone;
  frame.left:=(w-frame.width) DIV 2;
  frame.top:=(h-frame.height) DIV 2;
  Width:=w;
  Height:=h;
end;

procedure TRMCCrumarView.ChangePCC(pcc,Value: Integer);
  function IsParamIn(panelArray:TPanelarray; VAR index:integer):boolean;
  VAR i:integer;
  begin
    for i:=0 to 9 do
    begin
      if panelArray[i].cc = index then
      begin
        index:=panelArray[i].index;
        result:=true;
        exit;
      end;
    end;
    result:=false;
  end;
VAR eff:integer;
begin
  if pcc>=physCC_Effects0 then exit;
  FValues[pcc]:=value;
  if IsParamIn(PanelArrayOsc1,pcc) then if FOsc1Toggled then exit;
  if IsParamIn(PanelArrayOsc3,pcc) then if not FOsc1Toggled then exit;
  if IsParamIn(PanelArrayOsc2,pcc) then if FOsc2Toggled then exit;
  if IsParamIn(PanelArrayOsc4,pcc) then if not FOsc2Toggled then exit;
  if IsParamIn(PanelArrayMod1,pcc) then if FModToggled then exit;
  if IsParamIn(PanelArrayMod2,pcc) then if not FModToggled then exit;
  (* We must react on XRECDS2CC_DELAYAMOUNT .. XRECDS2CC_EFFECTSETTINGS here *)
  if (pcc>=XRECDS2CC_DELAYAMOUNT) and (pcc<=XRECDS2CC_DELAYAMOUNT+8-1) then
  begin
    eff:=(pcc-XRECDS2CC_DELAYAMOUNT) DIV 2;
    SetCurEffect(eff); //  He, update de crumar en geniet.Self Check dan nog even Memory Usage en vervang die dure chorus doet Azurite het echt niet ?
//    if eff<>FCurEffect then exit; // not visible
    exit; // SetCurEffect will update the UI
  end;
(* we should react to XRECDS2CC_EFFECTSETTINGS *)
  if pcc = XRECDS2CC_EFFECTSETTINGS then
  begin
    value:=SetUseEffects(value);
    exit; // SetUseEffects will update the UI
  end;
  SetValue(pcc,Value);
end;

end.
