unit URMCSyntorchestraView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,URMCEmptyPanel,URMCVSTView,
  URMCControls,Vcl.StdCtrls,URMCSyntorchestraFrame,URMCEffects;
type
  TRMCSyntorchestraView = class (TRMCVSTView)
  private
    frame:TRMCSyntorchestraFrame;
    FPerfPanel:TRMCEffects;
    procedure UIChanged(Sender: TObject; Index, Value: Integer);
    procedure SetMonoSound(value: integer);
    procedure SetPolySound(value: integer);
  protected
  public
    procedure ChangePCC(pcc,Value: Integer);override;
   constructor create(aowner: TComponent);override;
    procedure Centre(w, h: integer);
    procedure Load(aBasePanel:TCustomPanel);

 end;

implementation

uses System.Types,UVirtCC;

constructor TRMCSyntorchestraView.Create(aowner: TComponent);
begin
  DebugName:='TSyntorchestraView';
  inherited ;
  Scalable:=false;
  frame:=TRMCSyntorchestraFrame.Create(self);
  frame.Name:='Fietbel'+inttostr(GetTickCount);
  parent:=TWinControl(aowner);
  frame.parent:=self;
  frame.Visible:=true;
  Width:=frame.Width;
  Height:=frame.Height;

  OnPCCChanged:=NIL;
  frame.OnUIChanged:=UIChanged;
  FPerfPanel:=TRMCEffects.Create(frame.RMCEmptyPanel1);
  FPerfPanel.Parent:=frame.RMCEmptyPanel1;
  FPerfPanel.Left:=800 ;
  FPerfPanel.Top :=40;
  FPerfPanel.OnUIChanged:=UIChanged;
  Load(frame.RMCEmptyPanel1);
end;

procedure TRMCSyntorchestraView.Centre(w, h: integer);
begin
  frame.align:=alNone;
  frame.left:=(w-frame.width) DIV 2;
  frame.top:=(h-frame.height) DIV 2;
  Width:=w;
  Height:=h;
end;

procedure TRMCSyntorchestraView.Load(aBasePanel:TCustomPanel);
begin
  inherited Load(aBasePanel);
  setDefaultOnChangeHandler;
  OnChanged:=UIChanged;
end;

procedure TRMCSyntorchestraView.UIChanged(Sender: TObject; Index,Value: Integer);
begin
  // Poly Instrument Buttons
  if (index>=50) and (index<=53) then
  begin
    value:=16+32*(index-50);
    index:=XRESYNTORCHESTRACC_POLYSOUND;
    SetPolySound(value);
  end
  else
  if (index>=66) and (index<=74) then
  begin
    value:=6+127*(index-66) DIV 9;
    index:=XRESYNTORCHESTRACC_MONOSOUND;
    SetMonoSound(value);
  end;

  if assigned(OnPCCChanged) then
    OnPCCChanged(self,index,value);
end;

procedure TRMCSyntorchestraView.SetPolySound(value:integer);
VAR index:integer;
begin
  for index:=50 to 53 do
    SetValue(index,127*ord(value DIV 32 = index-50));
end;

procedure TRMCSyntorchestraView.SetMonoSound(value:integer);
VAR index:integer;
begin
  for index:=66 to 74 do
    SetValue(index,127*ord(value*9 DIV 128 = index-66));
end;

procedure TRMCSyntorchestraView.ChangePCC(pcc,Value: Integer);
begin
  FPerfPanel.ChangePCC(pcc,value);
  case pcc of
   XRESYNTORCHESTRACC_POLYSOUND: SetPolySound(value);
   XRESYNTORCHESTRACC_MONOSOUND: SetMonoSound(value);
   else SetValue(pcc,value);
  end;
end;

end.