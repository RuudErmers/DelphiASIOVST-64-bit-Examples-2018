unit XEffects;

interface

uses DAV_VSTEffect,DAV_VSTHost,DAV_Types,Classes,Generics.Collections,ExtCtrls,UFormEffects,ComCtrls,XEffectsBase;
const PARAMSTORECOUNT = 32;

type Teffects = class;
     TeffectMapping = class
                        index,param:integer;
                        constructor create(index,param:integer);
                      end;
     TEffectPlugin = class
                  strict
  private
                 protected
                    fEffects:Teffects;
                    effectList:TList<TeffectMapping>;
                    fTriedLoad:boolean;
                    ftabSheet:TTabSheet;
                    name:string;
                    fexternalEnabled,finternalEnabled:boolean;
                    fParamStore:array[0..PARAMSTORECOUNT-1] of integer;
                    function LoadPlugin(VSTHost: TVSTHost; const VSTDll: string): TCustomVstPlugIn;
                    procedure SetInternalEnabled(enable:boolean);
                    procedure SetExternalEnabled(enable:boolean);
                    procedure CheckLoad(internal,_external:boolean);
                    class function getID:integer; virtual;abstract;
                    property internalEnabled:boolean read fInternalEnabled write SetInternalEnabled;
                  public
                    fPlugin:TCustomVstPlugIn;
                    property Enabled:boolean read fExternalEnabled write SetExternalEnabled;
                    constructor Create(effects:TEffects;dllname:string='');virtual;
                    function ShouldProcess: boolean;
                    procedure SetParameter(index, value: integer);virtual;
                    procedure AddEffect(index,param:integer);
                    procedure SetEffect(index, value: integer); // index 0..15
                  end;
 TEffectPluginClass = class of TEffectPlugin;

TEffects = class(TEffectsBase)
private
    VstHost: TVstHost;
    FormEffects: TFormEffects;
    fPlugins: TList<TEffectPlugin>;
    Timer:TTimer;
    function CreateEffectPlugin(eff:integer):TEffectPlugin;
    procedure InitEffects(effects:TEffectsArray);
    procedure ButtonPluginEditorClick(Sender:TObject);
    procedure ButtonListEditorClick(Sender:TObject);
    procedure OnTimer(Sender:TObject);
public
    procedure SetEffectsEnable(enable: boolean);override;
    procedure ShowEffects;override;
    procedure SetTempo(tempo: single);override;
    constructor Create(effectSettings:TEffectsSettings);override;
    procedure ShowEffect(fx:integer);override;
    procedure Process(const Buf0,Buf1: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);override;
    procedure SetEffect(index, value: integer);override;
    function ProcessCount: integer;override;
    procedure Close;override;
end;

procedure AddEffectPluginClass(cls:TEffectPluginClass);

implementation

{ TEffects }

uses SysUtils,Dialogs,Windows,Math,Controls,UVirtCC;


procedure TEffects.InitEffects;
VAR i:integer;
begin
  for i:=0 to length(effects) -1 do
    fPlugins.add(CreateEffectPlugin(effects[i]));
end;

procedure TEffectPlugin.AddEffect(index, param: integer);
begin
  effectList.Add(TeffectMapping.Create(index,param));
end;

constructor TEffectPlugin.Create(effects:TEffects;dllname: string);
VAR i:integer;
begin
  fEffects:=effects;
  name:=dllname;
  fPlugin:=NIL;
  internalEnabled:=false;
  Enabled:=false;
  for i:=0 to PARAMSTORECOUNT-1 do
    fParamStore[i]:=-1;
  effectList:=TList<TeffectMapping>.Create;
  CheckLoad(true,true);  // sorry, have to do this...
end;

function TEffectPlugin.LoadPlugin(VSTHost: TVSTHost;const VSTDll: string):TCustomVstPlugIn;
begin
  result:=NIL;
  if not FileExists(VSTDll) then Exit;
  result:= VSTHost.VstPlugIns.Add;
  result.LoadFromFile(VSTDll);
  try
    result.Active := True;
  except
    MessageDlg('ERROR: ' + VSTDll + ' is not a valid VST plugin!', mtError, [mbOK], 0);
    VSTHost.VstPlugIns.Delete(VSTHost.VstPlugIns.Count-1);
    result:=NIL;
  end;
end;

procedure TEffects.SetEffect(index, value: integer);
VAR i:integer;
begin
  for i:=0 to fPlugins.Count-1 do
    fPlugins[i].SetEffect(index,value);
end;

procedure TEffectPlugin.SetEffect(index, value: integer);
VAR i:integer;
begin
  for i:=0 to effectList.Count-1 do
    if effectlist[i].index = index then
      SetParameter(effectlist[i].param,value);
end;

procedure TEffectPlugin.CheckLoad(internal,_external:boolean);
VAR i:integer;
begin
  if fTriedLoad then exit;
  if not internal or not _external then exit;
  fTriedLoad:=true;
  fPlugin:=LoadPlugin(fEffects.VstHost,name);
  fInternalEnabled:=fPlugin<>NIL;
  if InternalEnabled then
    for i:=0 to PARAMSTORECOUNT-1 do
    if fParamStore[i]<>-1 then
      SetParameter(i,fParamStore[i]);
end;

procedure TEffectPlugin.SetExternalEnabled(enable: boolean);
begin
  fExternalEnabled:=enable;
  CheckLoad(internalEnabled,enabled);
end;

procedure TEffectPlugin.SetInternalEnabled(enable: boolean);
begin
  CheckLoad(enable,enabled); // handles fInternalEnabled in a correct way: It NEVER becomes true if the plugin can't be loaded
  fInternalEnabled:=enable and (fPlugin<>NIL);
end;

procedure TEffectPlugin.SetParameter(index,value:integer);
begin
  if index<PARAMSTORECOUNT then Fparamstore[index]:=value;
  if fPlugin<>NIL then
    fPlugin.SetParameter(index,value/127);
end;

function TEffectPlugin.ShouldProcess: boolean;
begin
  result:=internalEnabled and Enabled;
end;

procedure TEffects.SetTempo(tempo: single);
begin
  VSTHost.Tempo:=tempo;
end;

procedure TEffects.ButtonPluginEditorClick(Sender:TObject);
VAR fx:integer;
begin
  fx:=FormEffects.PageControl1.ActivePageIndex;
  if fPlugins[fx].fPlugin= NIL then exit;
  with fPlugins[fx],fPlugin do
  begin
    if GUIStyle = gsDefault then exit;
    CloseEdit;
    GUIStyle:=gsDefault;
    ShowEdit(FTabSheet);
  end;
end;

procedure TEffects.ButtonListEditorClick(Sender:TObject);
VAR fx:integer;
begin
  fx:=FormEffects.PageControl1.ActivePageIndex;
  if fPlugins[fx].fPlugin= NIL then exit;
  with fPlugins[fx],fPlugin do
  begin
    if GUIStyle = gsParameterList then exit;
    CloseEdit;
    GUIStyle:=gsParameterList;
    ShowHostGuiList(FTabsheet);
  end;
end;


procedure TEffects.ShowEffect(fx: integer);
begin
  if FormEffects<>NIL then
    FormEffects.PageControl1.ActivePageIndex:=fx;
end;

procedure TEffects.SetEffectsEnable(enable:boolean);
VAR i:integer;
begin
  for i:=0 to fPlugins.Count-1 do
    fPlugins[i].Enabled:=enable;
end;

procedure TEffects.ShowEffects;
    procedure ShowIt(fx:integer;control:TWinControl);
    begin
      if fPlugins[fx].fPlugin<>NIL then
      begin
         fPlugins[fx].fPlugin.CloseEdit;
         fPlugins[fx].fPlugin.GUIStyle:=gsDefault;
         fPlugins[fx].fPlugin.ShowEdit(control);
         TTabSheet(control).Caption:=fPlugins[fx].fPlugin.GetEffectName;
      end;
    end;
VAR i:integer;
    sheet:TTabSheet;
begin
  FormEffects:=TFormEffects.Create(NIL);
  with FormEffects do
  begin
    for i:=0 to FPlugins.Count-1 do with fPlugins[i] do
    begin
      FTabsheet:=TTabSheet.Create(FormEffects);
      FTabsheet.PageControl:=PageControl1;
      ShowIt(i,FTabsheet);
    end;
    if FPlugins.Count>1 then
      PageControl1.ActivePage:=FPlugins[1].ftabSheet;
    if FPlugins.Count>0 then
      PageControl1.ActivePage:=FPlugins[0].ftabSheet;
    ButtonPluginEditor.OnClick:=ButtonPluginEditorClick;
    ButtonListEditor.OnClick:=ButtonListEditorClick;
    ShowModal;
    Free;
    FormEffects:=NIL;
  end;
end;

function TEffects.ProcessCount: integer;
VAR i:integer;
begin
  result:=0;
  for i:=0 to fPlugins.Count-1 do
    if fPlugins[i].ShouldProcess then inc(result);
end;

procedure TEffects.Close;
VAR i:integer;
begin
  inherited;
  Timer.Free;
  VstHost.Free;
  for i:=0 to fPlugins.Count-1 do
   fPlugins[i].free;
  fPlugins.Free;
end;

constructor TEffects.Create(effectSettings:TEffectsSettings);
begin
  inherited ;
  VstHost:=TVstHost.Create(NIL);
  VstHost.Tempo:=120;
  fPlugins:=TList<TEffectPlugin>.Create;
  InitEffects(effectSettings.effects);
  Timer:=TTimer.Create(NIL);
  Timer.Interval:=100;
  Timer.OnTimer:=OnTimer;
  Timer.Enabled:=true;
end;

procedure TEffects.OnTimer(Sender:TObject);
VAR i:integer;
begin
  for i:=0 to fPlugins.Count-1 do
   if fPlugins[i].fPlugin<>NIL then
     fPlugins[i].fPlugin.EditIdle
end;

procedure TEffects.Process(const Buf0, Buf1: TDAVArrayOfSingleFixedArray;  const SampleFrames: Cardinal);
VAR vst:integer;
    pin,pout,pins: PPSingle;

begin
  if ProcessCount = 0 then exit;
  pin:=@Buf0[0];
  pout:=@Buf1[0];
  for vst:=0 to fPlugins.Count-1 do if fPlugins[vst].ShouldProcess then
  begin
    with fPlugins[vst],fPlugin do
    begin
      if effFlagsCanReplacing in EffectOptions
        then Process32Replacing(pin, pout, SampleFrames)
        else Process(pin, pout, SampleFrames);  // should zero Outputs first ?? {: This is not called!
    end;
    // swap buffers
    pins:=pin;
    pin:=pout;
    pout:=pins;
  end
end;


{ TeffectMapping }

constructor TeffectMapping.create(index, param: integer);
begin
  self.index:=index;
  self.param:=param;
end;


VAR EffectPluginClasses: array of TEffectPluginClass;
    EffectPluginClassesCount:integer;

procedure AddEffectPluginClass(cls:TEffectPluginClass);
begin
  inc(EffectPluginClassesCount);
  SetLength(EffectPluginClasses,EffectPluginClassesCount);
  EffectPluginClasses[EffectPluginClassesCount-1]:=cls;
end;

function TEffects.CreateEffectPlugin(eff: integer): TEffectPlugin;
VAR i:integer;
begin
  for i:=0 to EffectPluginClassesCount-1 do if
    EffectPluginClasses[i].getId = eff then
      result:=EffectPluginClasses[i].Create(self);
end;



begin
end.


