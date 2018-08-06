unit URMCWavePlayerView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,URMCEmptyPanel,URMCVSTView,
  URMCControls,Vcl.StdCtrls,URMCWavePlayerFrame;
type
  TRMCWavePlayerView = class (TRMCVSTView)
  private
    frame:TRMCWavePlayerFrame;
  protected
  public
    procedure ChangePCC(pcc,Value: Integer);override;
    constructor create(aowner: TComponent);override;
    procedure Centre(w, h: integer);
    procedure SetOnLoadClick(click:TNotifyEvent);
    procedure SetOnPlayClick(click:TNotifyEvent);
    procedure SetOnPauseClick(click:TNotifyEvent);
    procedure SetInfo(filename,status:string);
 end;

implementation

uses System.Types;

constructor TRMCWavePlayerView.Create(aowner: TComponent);
begin
  DebugName:='TWavePlayerView';
  inherited ;
  Scalable:=false;
  frame:=TRMCWavePlayerFrame.Create(self);
  frame.Name:='Fietbel'+inttostr(GetTickCount);
  parent:=TWinControl(aowner);
  frame.parent:=self;
  frame.Visible:=true;
  Width:=frame.Width;
  Height:=frame.Height;
  OnPCCChanged:=NIL;
end;

procedure TRMCWavePlayerView.Centre(w, h: integer);
begin
  frame.align:=alNone;
  frame.left:=(w-frame.width) DIV 2;
  frame.top:=(h-frame.height) DIV 2;
  Width:=w;
  Height:=h;
end;

procedure TRMCWavePlayerView.SetInfo(filename, status: string);
begin
  frame.LabelFilename.Caption:=filename;
  frame.LabelStatus.Caption:=status;
end;

procedure TRMCWavePlayerView.SetOnLoadClick(click: TNotifyEvent);
begin
  frame.setClickHandler(click);
end;

procedure TRMCWavePlayerView.SetOnPauseClick(click: TNotifyEvent);
begin
  frame.ButtonPause.OnClick:=click;
end;

procedure TRMCWavePlayerView.SetOnPlayClick(click: TNotifyEvent);
begin
  frame.ButtonPlay.OnClick:=click;
end;

procedure TRMCWavePlayerView.ChangePCC(pcc,Value: Integer);
begin
//
end;

end.
