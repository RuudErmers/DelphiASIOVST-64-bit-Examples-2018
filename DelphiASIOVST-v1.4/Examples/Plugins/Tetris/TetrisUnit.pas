unit TetrisUnit;

{$I DAV_Compiler.inc}

(*
___ ____ ___ ____ _ ____    ___  _   _    ____ ____ _  _ ____
 |  |___  |  |__/ | [__     |__]  \_/     |__| |__/ |\ | |  |
 |  |___  |  |  \ | ___]    |__]   |      |  | |  \ | \| |__|
                                                        arnaud@celermajer.net
*)

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, Classes, Sysutils,
  Graphics;

type
  TSprite = array [0..3, 0..3] of Integer;
  TTetris = class
  private
    FLines            : Integer;
    FCurrentSpritePos : TPoint;
    FCurrentSprite    : TSprite;
    FSprite           : Boolean;
    FCurrentColor     : TColor;
    function CanSpriteBeHere(ASprite: TSprite; APos: TPoint): Boolean;
    function GetCurrentSprite: TSprite;
    procedure SetCurrentSprite(const Value: TSprite);
    procedure SetCurrentSpritePos(const Value: TPoint);
    procedure NewGameBoard;
    procedure FusionCurrentSprite;
  public
    GameBoard: array [0..9, 0..19] of Integer;
    property CurrentSprite: TSprite read GetCurrentSprite write SetCurrentSprite;
    property CurrentSpritePos: TPoint read FCurrentSpritePos write SetCurrentSpritePos;
    procedure StepGame;
    procedure Rotate;
    procedure Left;
    procedure Right;
    procedure DefaultBitmap(bmp: TBitmap);
    property Lines: Integer read FLines write FLines;
  end;

const
  AllSpriteKind: array[0..6] of TSprite =
  (((0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0)),
   ((0, 0, 0, 0), (0, 1, 1, 0), (1, 1, 0, 0), (0, 0, 0, 0)),
   ((0, 0, 0, 0), (1, 1, 0, 0), (0, 1, 1, 0), (0, 0, 0, 0)),
   ((0, 0, 0, 0), (0, 1, 0, 0), (1, 1, 1, 0), (0, 0, 0, 0)),
   ((0, 0, 0, 0), (0, 0, 1, 0), (1, 1, 1, 0), (0, 0, 0, 0)),
   ((0, 0, 0, 0), (1, 1, 1, 0), (0, 0, 1, 0), (0, 0, 0, 0)),
   ((0, 0, 0, 0), (0, 1, 1, 0), (0, 1, 1, 0), (0, 0, 0, 0)));

function TrimInt(Value, Min, Max: Integer): Integer;
    
implementation

{ TTetris }

function TrimInt(Value, Min, Max: Integer): Integer;
begin
 if Value < Min then Result := Min else
  if Value > Max then Result := Max
   else Result := Value;
end;

function TTetris.CanSpriteBeHere(ASprite: TSprite; APos: TPoint): Boolean;
var
  i, ii, jj, j: Integer;
begin
  Result := True;
  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      if ASprite[i, j] = 0 then Continue;
      ii := i + APos.X;
      jj := j + APos.Y;
      if ii < 0 then Result := False;
      if ii >= Length(GameBoard) then Result := False;
      if jj >= Length(GameBoard[0]) then Result := False;
      if Result then
      begin
        if GameBoard[ii, jj] <> 0 then Result := False;
      end;
    end;
end;

procedure TTetris.StepGame;
var
  Pos: TPoint;
begin
  Pos := CurrentSpritePos;
  Pos.Y := CurrentSpritePos.Y + 1;
  if CanSpriteBeHere(CurrentSprite, Pos)
   then CurrentSpritePos := Pos
   else FusionCurrentSprite;
end;

procedure TTetris.DefaultBitmap(bmp: Tbitmap);
var
  i, ii, jj, j: Integer;
  Col1: Cardinal;
  Factor: Integer;
begin
  Factor := 20;
  with bmp do
   begin
    Width := 0;
    Canvas.Brush.Color := clBlack;
    Height := Factor * Length(GameBoard[0]);
    Width := Factor * Length(GameBoard);
   end;

  for i := 0 to Length(GameBoard) - 1 do
    for j := 0 to Length(GameBoard[0]) - 1 do
     begin
      if GameBoard[i, j] = 0 then
       with bmp.Canvas do
        begin
         Pen.Color := clred;
         Brush.Color := GameBoard[i, j];
        end;
      if GameBoard[i, j] <> 0 then
       begin
        Col1 := ColorToRGB(GameBoard[i, j]);
        Col1 := RGB(TrimInt(GetRValue(Col1) - 50, 0, 255),
          TrimInt(GetGValue(Col1) - 50, 0, 255),
          TrimInt(GetBValue(Col1) - 50, 0, 255));
        with bmp.Canvas do
         begin
          Brush.Color := Col1;
          Pen.Color := bmp.Canvas.Brush.Color;
          RoundRect(Factor * i, Factor * j, Factor * i + (Factor - 1),
                    Factor * j + (Factor - 1), 2, 2);
          Brush.Color := GameBoard[i, j];
          Pen.Color := bmp.Canvas.Brush.Color;
          RoundRect(1 + Factor * i, 1 + Factor * j, Factor * i + Factor,
                    Factor * j + Factor, 2, 2);
        end;
       end;
     end;
  for i := 0 to 3 do
    for j := 0 to 3 do
      if CurrentSprite[i, j] <> 0 then
      begin
        ii := i + CurrentSpritePos.x;
        jj := j + CurrentSpritePos.y;

        Col1 := ColorToRGB(FCurrentColor);
        Col1 := RGB(TrimInt(GetRValue(Col1) - 50, 0, 255),
          TrimInt(GetGValue(Col1) - 50, 0, 255), TrimInt(GetBValue(Col1) - 50, 0, 255));
        with bmp.Canvas do
         begin
          Pen.Color := ColorToRGB(FCurrentColor);
          Brush.Color := ColorToRGB(FCurrentColor);
          RoundRect(Factor * ii, Factor * jj, Factor * ii + (Factor - 1), Factor * jj + (Factor - 1), 2, 2);
          Brush.Color := Col1;
          Pen.Color := Col1;
          RoundRect(1 + Factor * ii, 1 + Factor * jj, Factor * ii + Factor, Factor * jj + Factor, 2, 2);
         end;
      end;

end;

procedure TTetris.FusionCurrentSprite;
var
  k, i, ii, jj, j: Integer;
begin
  if FSprite then
   begin
    for i := 0 to 3 do
     for j := 0 to 3 do
      begin
       if CurrentSprite[i, j] = 0 then continue;
       ii := i + CurrentSpritePos.X;
       jj := j + CurrentSpritePos.Y;
       GameBoard[ii, jj] := FCurrentColor;
      end;
    FSprite := False;
   end;
  for j := 0 to Length(GameBoard[0]) - 1 do
   begin
    k := 0;
    for i := 0 to Length(GameBoard) - 1 do
     if GameBoard[i, j] <> 0 then
      Inc(k);
    if k = Length(GameBoard) then
     begin
      inc(Flines);
      for jj := j downto 1 do
        for ii := 0 to Length(GameBoard) - 1 do
          GameBoard[ii, jj] := GameBoard[ii, jj - 1];
      for ii := 0 to Length(GameBoard) - 1 do
        GameBoard[ii, 0] := 0;
     end;
   end;
end;

function TTetris.getCurrentSprite: TSprite;
var
  i: Integer;
begin
  if FSprite then
    Result := FCurrentSprite
  else
  begin
    FSprite := True;
    Randomize;
    i := Random(7);
    FCurrentSprite := AllSpriteKind[i];
    Result := FCurrentSprite;
    case i of
      0: FCurrentColor := cllime;
      1: FCurrentColor := clblue;
      2: FCurrentColor := clOlive;
      3: FCurrentColor := clRed;
      4: FCurrentColor := clAqua;
      5: FCurrentColor := clWhite;
      6: FCurrentColor := clMoneyGreen;
    end;
    CurrentSpritePos := point(3, 0);
    if not CanSpriteBeHere(FCurrentSprite, CurrentSpritePos) then
    begin
      NewGameBoard;
    end;
  end;
end;

procedure TTetris.Left;
var
  Pos: TPoint;
begin
  Pos := CurrentSpritePos;
  Pos.x := CurrentSpritePos.X - 1;
  if CanSpriteBeHere(CurrentSprite, Pos) then
    CurrentSpritePos := Pos
end;

procedure TTetris.NewGameBoard;
var
  i, j: Integer;
begin
  FLines := 0;
  for i := 0 to Length(GameBoard) - 1 do
    for j := 0 to Length(GameBoard[0]) - 1 do
      GameBoard[i, j] := 0;
end;

procedure TTetris.Right;
var
  Pos: TPoint;
begin
  Pos := CurrentSpritePos;
  Pos.x := CurrentSpritePos.X + 1;
  if CanSpriteBeHere(CurrentSprite, Pos) then
    CurrentSpritePos := Pos
end;

procedure TTetris.Rotate;
var
  Sprite: TSprite;
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Sprite[3 - j, i] := CurrentSprite[i, j];
  if CanSpriteBeHere(Sprite, CurrentSpritePos) then
    CurrentSprite := Sprite;
end;

procedure TTetris.SetCurrentSprite(const Value: TSprite);
begin
  FCurrentSprite := Value;
end;

procedure TTetris.SetCurrentSpritePos(const Value: tpoint);
begin
  FCurrentSpritePos := Value;
end;

end.
