unit URMCEmptyPanel;

interface

uses
  System.SysUtils, System.Classes, Messages, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics,Types;

type  TRMCEmptyPanel = class(TCustomPanel)
  private
     FRoundRect:integer;
     FFontSize:integer;
     FCaptions:TStringList;
    procedure SetRoundRect(value:integer);
  protected

     procedure SetCaption(value:string);
     function GetCaption:string;
  public
     procedure Paint; override;
     constructor Create(AOwner: TComponent); override;
     procedure PanelResize(Sender: TObject);
     procedure SetCaptions(captions:TStringList);
     property OnClick;
     property Font;
  published
     property Align;
     property Caption: string read GetCaption write SetCaption;
     property RoundRect: integer read FRoundRect write SetRoundRect;
     property DoubleBuffered;
     property Color;
     property FontSize: integer read FFontSize write FFontsize;
  end;

procedure Register;

implementation

uses URMCControls;

procedure Register;
begin
  RegisterComponents('RMC', [TRMCEmptyPanel]);
end;

{ TRMCChannelBase }

procedure TRMCEmptyPanel.SetCaption(value: string);
begin
  FCaptions.Clear;
  FCaptions.Add(value);
  Invalidate;
end;

procedure TRMCEmptyPanel.SetCaptions(captions: TStringList);
begin
  FCaptions.Assign(captions);
  Invalidate;
end;

procedure TRMCEmptyPanel.SetRoundRect(value: integer);
begin
  FRoundRect:=value;
  invalidate;
end;

function TRMCEmptyPanel.GetCaption: string;
begin
  if FCaptions.Count=0 then result:=''
                       else result:=FCaptions[0];
end;

procedure TRMCEmptyPanel.Paint;
VAR ten,i,l:integer;
    c:TSize;
    r:TRect;
begin
  inherited;

  with Canvas do
  begin
    Pen.Color:=clWhite;
    Pen.Width:=1;
    ten:=FRoundRect;
    if ten>0 then
      RoundRect(ten,ten,Width -ten,Height-ten,ten,ten);
    for i:=0 to FCaptions.Count - 1 do
    if Fcaptions[i]<>'' then
    begin
      Font.Color:=clWhite;
      Font.Size:=FontSize;
      c:=TextExtent(' '+FCaptions[i]+' ');
      l:=15+i*(Width) DIV FCaptions.Count;
      r:=Rect(l,1,l+c.cx,0+c.cy+3);
      FillRect(r);
      TextRect(r,r.Left,(r.Bottom-2)-c.cy,' '+FCaptions[i]+' ');
    end;
  end;
end;

procedure TRMCEmptyPanel.PanelResize(Sender:TObject);
begin
  Invalidate;
end;

constructor TRMCEmptyPanel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  inherited OnResize:=PanelResize;
  ParentDoubleBuffered:=false;
  ParentBackGround:=false;
  DoubleBuffered:=true;

  FontSize:=8;
  Width:=200;
  Height:=100;
  BevelOuter:=bvNone;
  FRoundRect:=10;
  FCaptions:=TStringList.Create;
  Color:=$252525;

end;


end.


