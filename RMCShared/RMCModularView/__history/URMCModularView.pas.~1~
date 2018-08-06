unit URMCModularView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  URMCEmptyPanel,URMCBaseControlPanel,URMCControls,URMCVSTView,URMCSunriseFrame;

type
  TRMCModularView = class (TRMCVSTView)
  private
    frame:TRMCSunriseFrame;
    procedure UIChanged(Sender: TObject; Index, Value: Integer);
  protected
  public
    procedure ChangePCC(pcc,Value: Integer);override;
   constructor create(aowner: TComponent);override;
    procedure Centre(w, h: integer);
 end;

implementation

uses System.Types;

constructor TRMCModularView.Create(aowner: TComponent);
begin
  DebugName:='TModularView';
  inherited ;
  Scalable:=false;
  frame:=TRMCSunriseFrame.Create(self);
  frame.Name:='Fietbel'+inttostr(GetTickCount);
  parent:=TWinControl(aowner);
  frame.parent:=self;
  frame.Visible:=true;
  Width:=frame.Width;
  Height:=frame.Height;

  Load(frame);
  OnPCCChanged:=NIL;
  frame.OnUIChanged:=UIChanged;
end;

procedure TRMCModularView.Centre(w, h: integer);
begin
  frame.align:=alNone;
  frame.left:=(w-frame.width) DIV 2;
  frame.top:=(h-frame.height) DIV 2;
  Width:=w;
  Height:=h;
end;

procedure TRMCModularView.UIChanged(Sender: TObject; Index,Value: Integer);
begin
  if assigned(OnPCCChanged) then OnPCCChanged(self,index,value);
end;

procedure TRMCModularView.ChangePCC(pcc,Value: Integer);
begin
  frame.ChangePCC(pcc,value);
end;

end.
