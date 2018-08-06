unit URMCMidiCCGrid;

interface

uses
  Vcl.Grids, System.Classes, System.Types, SysUtils, Vcl.StdCtrls,
  System.UITypes;

type TOnCCClicked = procedure (Sender:TObject;cc:integer;clicked:boolean) of object;
type TonLoadCC = reference to procedure (cc:integer;VAR check:boolean;VAR name:string);
type TColumnCount = 1..128;
     TRMCMidiCCGrid = class(TStringGrid)
  public
    procedure ClearSelection;
    procedure SetCCValue(cc,value:integer);
    constructor Create(aowner:TComponent);override;
    procedure FillStringgrid(onLoadCC:TonLoadCC=NIL);
  private
    FonCCClicked :TOnCCClicked;
    FColumns:TColumnCount;
    FShowCheckBoxes:boolean;
    CheckBoxes:array[0..127] of TCheckBox;
    CCNames:array[0..127] of string;
    CCChecked:array[0..127] of boolean;
    procedure SetColumns(value:TColumnCount);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure SetLastSelected(cc: integer);
    procedure OnCheckBoxClick(Sender: TObject);
    procedure SetCheckBox(cc: integer; check, click: boolean);
    function ItemsPerColumn:integer;
    procedure SetLayout;
    procedure SetShowCheckboxes(value:boolean);
    procedure CheckBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  published
   property onCCClicked :TOnCCClicked read FonCCClicked write FonCCClicked;
   property columns : TColumnCount read FColumns write SetColumns;
   property showCheckboxes: boolean read FShowCheckBoxes write SetShowCheckboxes;
end;

procedure Register;

implementation

{ TRMCMidiCCGrid }

uses Windows, Vcl.Graphics;

procedure Register;
begin
  RegisterComponents('RMC', [TRMCMidiCCGrid]);
end;

procedure TRMCMidiCCGrid.ClearSelection;
VAR c,r:integer;
    s:string;
begin
  for r:=0 to RowCount-1 do
  for c:=0 to ColCount-1 do
  begin
    s:=Cells[c,r];
    if Pos('X',s) = 1 then
      Cells[c,r]:=Copy(s,2);
  end;
end;

constructor TRMCMidiCCGrid.Create(aowner: TComponent);
VAR cc:integer;
begin
  inherited;
  inherited OnDrawCell:=StringGridDrawCell;
  ColCount:=5;
  RowCount:=5;
  FColumns:=6;
  FShowCheckBoxes:=false;
  FixedCols:=0;
  FixedRows:=1;
  for cc:=0 to 127 do
  begin
    CCNames[cc]:='CC='+inttostr(cc);
    CheckBoxes[cc]:=TCheckBox.Create(self);
    with CheckBoxes[cc] do
    begin
    Parent:=self;
      Visible := False;
      Color :=  clWhite;
      Tag:=cc;
      OnClick := OnCheckBoxClick;
      OnMouseDown:=CheckBox1MouseDown;
    end;
  end;
  SetLayout;
end;

procedure TRMCMidiCCGrid.CheckBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetCheckBox((Sender as TCheckBox).Tag,not (Sender as TCheckBox).Checked,true);
end;

procedure TRMCMidiCCGrid.OnCheckBoxClick(Sender:TObject);
begin
  SetCheckBox((Sender as TCheckBox).Tag,(Sender as TCheckBox).Checked,true);
end;

procedure TRMCMidiCCGrid.SetColumns(value: TColumnCount);
begin
 // you should remove all checkboxes from memory....
 FColumns:=value;
 SetLayout;
end;

procedure TRMCMidiCCGrid.SetCCValue(cc, value: integer);
begin
  Cells[2+3*(cc DIV ItemsPerColumn),cc MOD ItemsPerColumn +1]:=inttostr(Value);
  SetLastSelected(cc);
end;

procedure TRMCMidiCCGrid.SetCheckBox(cc:integer;check,click:boolean);
VAR i:integer;
   cb:TCheckBox;
begin
  if assigned(onCCClicked) and click
    then onCCClicked(self,cc,check);
  CCChecked[cc]:=check;
  CheckBoxes[cc].Checked:=check;
end;

procedure TRMCMidiCCGrid.SetLastSelected(cc: integer);
VAR c1,r1:integer;
    s:string;
    c,r:integer;
begin
  c:=2+3*(cc DIV ItemsPerColumn);
  r:=cc MOD ItemsPerColumn +1;
  for r1:=0 to RowCount-1 do
    for c1:=0 to ColCount-1 do
    begin
      s:=Cells[c1,r1];
      if Pos('Y',s) = 1 then
      begin
        if (c1=c) and (r1=r) then exit;
        Cells[c1,r1]:='X'+Copy(s,2);
      end;
    end;
    s:=Cells[c,r];
    if Pos('X',s) = 1 then
        Cells[c,r]:='Y'+Copy(s,2)
    else
        Cells[c,r]:='Y'+s;
end;

procedure TRMCMidiCCGrid.StringGridDrawCell(Sender: TObject; ACol, ARow: Integer;  Rect: TRect; State: TGridDrawState);
VAR s:string;
begin
  if ARow<1 then exit;
  if Acol=0 then exit;
  with TStringGrid(Sender) do
    begin
      s:= Cells[Acol,Arow];
      //paint the background Green
      if Pos('X',s) = 1 then
      begin
        Canvas.Brush.Color := RGB(255,100,255);
        s:=Copy(s,2);
      end
      else if Pos('Y',s) = 1 then
      begin
        Canvas.Brush.Color := RGB(100,255,255);
        s:=Copy(s,2);
      end
      else
        Canvas.Brush.Color := clWhite;
      Canvas.FillRect(Rect);
      Canvas.TextOut(Rect.Left+2,Rect.Top+2,s);
    end;
end;

procedure TRMCMidiCCGrid.FillStringgrid(onLoadCC:TonLoadCC);
VAR i,cc:integer;
    s:string;
    cb:boolean;
begin
  for cc:=0 to 127 do
  begin
    if assigned(onLoadCC) then
    begin
       onLoadCC(cc,cb,s);
       CCNames[cc]:=s;
       CCChecked[cc]:=cb;
    end;
  end;
  SetLayout;
end;

procedure TRMCMidiCCGrid.SetLayout;
VAR i,c,r,cc:integer;
    s:string;
    cb:boolean;
    Rect:TREct;
begin
  RowCount:=ItemsPerColumn + 1;
  ColCount:=3*Columns;
  Height:=(RowCount+2)*DefaultRowHeight;
  Width:= Columns*200+30;
  for i:=0 to ColCount DIV 3 - 1 do
  begin
    ColWidths[0+3*i]:=50;
    ColWidths[1+3*i]:=100;
    ColWidths[2+3*i]:=50;
  end;
  for cc:=0 to 127 do
  begin
    Cells[0+3*(cc DIV ItemsPerColumn),cc MOD ItemsPerColumn +1]:=inttostr(cc);
    Cells[1+3*(cc DIV ItemsPerColumn),cc MOD ItemsPerColumn +1]:=ccNames[cc];
    c:=3*(cc DIV ItemsPerColumn);
    r:=cc MOD ItemsPerColumn +1;
    Rect := CellRect(c,r);
    CheckBoxes[cc].Left  := Rect.Left + (Rect.Right - Rect.Left) DIV 2;
    CheckBoxes[cc].Top   := Rect.Top+2;
    CheckBoxes[cc].Width := 24;
    CheckBoxes[cc].Height := Rect.Bottom - Rect.Top - 4;
    CheckBoxes[cc].Visible := FShowCheckBoxes;
    SetCheckBox(cc,CCChecked[cc],false);
  end;
end;

procedure TRMCMidiCCGrid.SetShowCheckboxes(value: boolean);
VAR i:integer;
begin
  FShowCheckBoxes:=value;
  SetLayout;
end;

function TRMCMidiCCGrid.ItemsPerColumn: integer;
begin
  result:=(128+Columns-1) DIV Columns;
end;

end.
