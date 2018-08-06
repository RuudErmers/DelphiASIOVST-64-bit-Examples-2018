unit URMCCheckBox;

interface

uses
  System.SysUtils, System.Classes, Messages, Vcl.Controls, Vcl.Graphics,Types;

type  TRMCCheckBox = class(TCustomControl )
  private
    FChecked:boolean;
    FChar:string;
    FCheckedColor,FUNcheckedColor:TColor;
    procedure SetChar(value:string);
  public
    property CheckedColor:TColor read FCheckedColor write FcheckedColor;
    property UncheckedColor:TColor read FUncheckedColor write FUncheckedColor;
   constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure SetChecked(value:boolean);
    property Checked:boolean read FChecked write SetChecked;
    property Caption:string read FChar write SetChar;
  published
    property OnClick;
   end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RMC', [TRMCCheckBox]);
end;

{ TRMCCheckBox }

constructor TRMCCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  Checked:=false;
  CheckedColor:=clGreen;
  UncheckedColor:=clWhite;
end;

procedure TRMCCheckBox.Paint;
VAR c:TSize;
    x,y:integer;
    r:TRect;
begin
//  inherited;
  with Canvas do
  begin
    Brush.Style:=bsSolid;
    Pen.Style:=psClear;
    if FChecked then
      Brush.Color:=CheckedColor
    else
      Brush.Color:=UncheckedColor;
    Rectangle(0,0,Width+1,Height+1);
    if FChar <> '' then
    begin
      if FChecked then
        Font.Color:=UnCheckedColor
      else
        Font.Color:=checkedColor;
      Font.Size:=6;
      font.Name:='Arial';
      c:=TextExtent(FChar);
      x:=Width DIV 2-c.cx DIV 2;
      y:=Height+1-c.cy;
      TextRect(Rect(0,0,Width,Height),x,y,FChar);
    end;
  end;
end;

procedure TRMCCheckBox.SetChar(value: string);
begin
  FChar:=value;
  Invalidate;
end;

procedure TRMCCheckBox.SetChecked(value: boolean);
begin
  FChecked:=value;
  Invalidate;
end;



end.
