unit DAV_GuiADSRGraph;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, LMessages,
  {$ELSE} Windows, Messages, {$ENDIF} Classes, Graphics, Forms, Controls,
  DAV_Common, DAV_GuiCommon, DAV_GuiCustomControl;

type
  TGuiADSRGraphMouseEdit = (meNone, meAttack, meDecay, meSustain, meRelease);
  TGuiADSRGraph = class;
  TGuiADSROnChange = procedure (Sender: TObject; EditType: TGuiADSRGraphMouseEdit) of object;

  TGuiADSRSettings = class(TPersistent)
  private
    FAttack   :  Single;
    FDecay    :  Single;
    FSustain  :  Single;
    FRelease  :  Single;
    FOnChange : TGuiADSROnChange;
    procedure SetAttack(Value: Single);
    procedure SetDecay(Value: Single);
    procedure SetRelease(Value: Single);
    procedure SetSustain(Value: Single);
  protected
    procedure Changed(EditType: TGuiADSRGraphMouseEdit);
    procedure AssignTo(Dest: TPersistent); override;

    property OnChange: TGuiADSROnChange read FOnChange write FOnChange;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Attack: Single read FAttack write SetAttack;
    property Decay: Single read FDecay write SetDecay;
    property Sustain: Single read FSustain write SetSustain;
    property Release: Single read FRelease write SetRelease;
  end;

  TGuiLineStyle = (lsClear, lsSolid, lsDash, lsDot);
  TGuiADSRGraph = class(TGuiCustomControl)
  private
    FADSRSettings    : TGuiADSRSettings;
    FMouseEdit       : TGuiADSRGraphMouseEdit;

    FGridColor       : TColor;
    FGridStyle       : TGuiLineStyle;
    FGridWidth       : Integer;

    FOnAttackChange  : TNotifyEvent;
    FOnSustainChange : TNotifyEvent;
    FOnDecayChange   : TNotifyEvent;
    FOnReleaseChange : TNotifyEvent;
    FLineWidth       : Integer;
    FLineColor       : TColor;

    FGridColor32     : TPixel32;
    FLineColor32     : TPixel32;

    procedure CalcIntValues;
    function GetAttack: Single;
    function GetDecay: Single;
    function GetRelease: Single;
    function GetSustain: Single;
    procedure SetAttack(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetSustain(const Value: Single);

    procedure SetGridColor(const Value: TColor);
    procedure SetGridWidth(const Value: Integer);
    procedure SetGridStyle(const Value: TGuiLineStyle);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineWidth(const Value: Integer);
  protected
    FA, FD, FS, FR : Integer;
    FCursorADR     : TCursor;
    FCursorS       : TCursor;
    FCursorDefault : TCursor;
    procedure GridStyleChanged; virtual;
    procedure GridWidthChanged; virtual;
    procedure GridColorChanged; virtual;
    procedure LineColorChanged; virtual;
    procedure LineWidthChanged; virtual;

    procedure UpdateBuffer; override;
    procedure Resize; override;

//    procedure DragMouseMoveLeft(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure SettingsChanged(Sender: TObject; EditType: TGuiADSRGraphMouseEdit); dynamic;
    function CheckForMouseFunc(x,y: Integer): TGuiADSRGraphMouseEdit; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Attack: Single read GetAttack write SetAttack;
    property Decay: Single read GetDecay write SetDecay;
    property Sustain: Single read GetSustain write SetSustain;
    property Release: Single read GetRelease write SetRelease;
  published    
    {$IFNDEF FPC}
    property Transparent;
    {$ENDIF}

    property Color;
    property ParentColor;

    property ADSRSettings: TGuiADSRSettings read FADSRSettings write FADSRSettings;
    property OnAttackChange : TNotifyEvent read FOnAttackChange write FOnAttackChange;
    property OnDecayChange : TNotifyEvent read FOnDecayChange write FOnDecayChange;
    property OnSustainChange : TNotifyEvent read FOnSustainChange write FOnSustainChange;
    property OnReleaseChange : TNotifyEvent read FOnReleaseChange write FOnReleaseChange;

    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;

    property GridColor: TColor read FGridColor write SetGridColor default clSilver;
    property GridWidth: Integer read FGridWidth write SetGridWidth default 1;
    property GridStyle: TGuiLineStyle read FGridStyle write SetGridStyle default lsSolid;

    property CursorDefault: TCursor read FCursorDefault write FCursorDefault default crDefault;
    property CursorADR: TCursor read FCursorADR write FCursorADR default crSizeWE;
    property CursorS: TCursor read FCursorS write FCursorS default crSizeNS;
  end;

implementation

uses
  SysUtils, Math, DAV_GuiBlend;

constructor TGuiADSRSettings.Create;
begin
  inherited Create;
  FAttack  := 0.5;
  FDecay   := 0.5;
  FSustain := 0.5;
  FRelease := 0.5;
end;

destructor TGuiADSRSettings.Destroy;
begin
  inherited;
end;

procedure TGuiADSRSettings.AssignTo(Dest: TPersistent);
begin
 if Dest is TGuiADSRSettings then
  with TGuiADSRSettings(Dest) do
   begin
    FAttack   := Self.FAttack;
    FDecay    := Self.FDecay;
    FSustain  := Self.FSustain;
    FRelease  := Self.FRelease;
    FOnChange := Self.FOnChange;
   end
 else inherited;
end;

procedure TGuiADSRSettings.Changed(EditType: TGuiADSRGraphMouseEdit);
begin
  if Assigned(FOnChange) then FOnChange(Self, EditType);
end;

procedure TGuiADSRSettings.SetAttack(Value: Single);
begin
 Value := Limit(Value, 0, 1);

 if (FAttack <> Value) then
  begin
   FAttack := Value;
   Changed(meAttack);
  end;
end;

procedure TGuiADSRSettings.SetDecay(Value: Single);
begin
 Value := Limit(Value, 0, 1);

 if (FDecay <> Value) then
  begin
   FDecay := Value;
   Changed(meDecay);
  end;
end;

procedure TGuiADSRSettings.SetSustain(Value: Single);
begin
 Value := Limit(Value, 0, 1);

 if (FSustain <> Value) then
  begin
    FSustain := Value;
    Changed(meSustain);
  end;
end;

procedure TGuiADSRSettings.SetRelease(Value: Single);
begin
 Value := Limit(Value, 0, 1);

 if (FRelease <> Value) then
  begin
   FRelease := Value;
   Changed(meRelease);
  end;
end;


{ TGuiADSRGraph }

constructor TGuiADSRGraph.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FADSRSettings := TGuiADSRSettings.Create;
 FADSRSettings.OnChange := SettingsChanged;

 FGridColor     := clSilver;
 FGridColor32   := ConvertColor(FGridColor);
 FGridWidth     := 1;
 FGridStyle     := lsSolid;
 FLineColor     := clBlack;
 FLineColor32   := ConvertColor(FLineColor);
 FCursorADR     := crSizeWE;
 FCursorS       := crSizeNS;
 FCursorDefault := crDefault;
end;

destructor TGuiADSRGraph.Destroy;
begin
 FreeAndNil(FADSRSettings);
 inherited;
end;

procedure TGuiADSRGraph.SettingsChanged(Sender: TObject; EditType: TGuiADSRGraphMouseEdit);
begin
 CalcIntValues;
 if (EditType = meAttack)  and Assigned(FOnAttackChange)  then FOnAttackChange(self);
 if (EditType = meDecay)   and Assigned(FOnDecayChange)   then FOnDecayChange(self);
 if (EditType = meSustain) and Assigned(FOnSustainChange) then FOnSustainChange(self);
 if (EditType = meRelease) and Assigned(FOnReleaseChange) then FOnReleaseChange(self);
 BufferChanged;
end;

function TGuiADSRGraph.GetAttack: Single;  begin Result := ADSRSettings.Attack; end;
function TGuiADSRGraph.GetDecay: Single;   begin Result := ADSRSettings.Decay; end;
function TGuiADSRGraph.GetRelease: Single; begin Result := ADSRSettings.Release; end;
function TGuiADSRGraph.GetSustain: Single; begin Result := ADSRSettings.Sustain; end;

procedure TGuiADSRGraph.SetAttack(const Value: Single);  begin ADSRSettings.Attack := Value; end;
procedure TGuiADSRGraph.SetDecay(const Value: Single);   begin ADSRSettings.Decay := Value; end;
procedure TGuiADSRGraph.SetRelease(const Value: Single); begin ADSRSettings.Release := Value; end;
procedure TGuiADSRGraph.SetSustain(const Value: Single); begin ADSRSettings.Sustain := Value; end;

procedure TGuiADSRGraph.SetGridColor(const Value: TColor);
begin
 if FGridColor <> Value then
  begin
   FGridColor := Value;
   GridColorChanged;
  end;
end;

procedure TGuiADSRGraph.SetGridWidth(const Value: Integer);
begin
 if FGridWidth <> Value then
  begin
   FGridWidth := Value;
   GridWidthChanged;
  end;
end;

procedure TGuiADSRGraph.SetLineColor(const Value: TColor);
begin
 if FLineColor <> Value then
  begin
   FLineColor := Value;
   LineColorChanged;
  end;
end;

procedure TGuiADSRGraph.SetLineWidth(const Value: Integer);
begin
 if FLineWidth <> Value then
  begin
   FLineWidth := Value;
   LineWidthChanged;
  end;
end;

procedure TGuiADSRGraph.SetGridStyle(const Value: TGuiLineStyle);
begin
 if FGridStyle <> Value then
  begin
   FGridStyle := Value;
   GridStyleChanged;
  end;
end;

procedure TGuiADSRGraph.GridWidthChanged;
begin
 BufferChanged;
end;

procedure TGuiADSRGraph.GridColorChanged;
begin
 FGridColor32 := ConvertColor(FGridColor);
 BufferChanged;
end;

procedure TGuiADSRGraph.LineColorChanged;
begin
 FLineColor32 := ConvertColor(FLineColor);
 BufferChanged;
end;

procedure TGuiADSRGraph.LineWidthChanged;
begin
 BufferChanged;
end;

procedure TGuiADSRGraph.GridStyleChanged;
begin
 BufferChanged;
end;

procedure TGuiADSRGraph.UpdateBuffer;
var
  LineIndex : Integer;
  Offset    : Integer;
begin
 inherited;

 if (Width > 0) and (Height > 0) then
  with FBuffer do
   begin
    // TODO: handle pen style!!!
    // TODO: handle pen width!!!
    case FGridStyle of
     lsSolid :
      begin
       for LineIndex := 0 to FGridWidth - 1 do
        begin
         Offset := LineIndex - FGridWidth div 2;
         if FA + Offset > 0
          then VerticalLine(FA + Offset, 0, Height - 1, FGridColor32);
         VerticalLine(FD + Offset, 0, Height - 1, FGridColor32);
         if FR + Offset < Width
          then VerticalLine(FR + Offset, 0, Height - 1, FGridColor32);
        end;
      end;
    end;

    Line(0, Height - 1, FA, 0, FLineColor32);
    Line(FA, 0, FD, FS, FLineColor32);
    Line(FD, FS, FR, FS, FLineColor32);
    Line(FR, FS, Width - 1, Height - 1, FLineColor32);
   end;
end;

procedure TGuiADSRGraph.CalcIntValues;
var
  IntWidth  : Integer;
  IntHeight : Integer;
begin
 IntWidth := Width - 1;
 IntHeight := Height - 1;

 with FADSRSettings do
  begin
   FA := Round(0.25 * IntWidth * Attack);
   FD := FA + Round(0.25 * IntWidth * Decay);
   FS := Round(IntHeight * (1 - Sustain));
   FR := IntWidth - Round(0.25 * IntWidth * Release);
  end;
end;

function TGuiADSRGraph.CheckForMouseFunc(x,y: Integer): TGuiADSRGraphMouseEdit;
var
  Range : Integer;
begin
 Result := meNone;
 if (x < 0) or (x > Width) or
    (y < 0) or (y > Height) then Exit;

 Range := Max(5, FLinewidth);
 if (x > FA - Range) and (x < FA + Range) then Result := meAttack else
 if (x > FD - Range) and (x < FD + Range) then Result := meDecay else
 if (x > FR - Range) and (x < FR + Range) then Result := meRelease else
 if (y > FS - Range) and (y < FS + Range) and (x >= FD + Range) and (x <= FR - Range)
  then Result := meSustain
  else Result := meNone;
end;

procedure TGuiADSRGraph.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Enabled then
  begin
   if not (ssLeft in Shift) then Exit;
   FMouseEdit := CheckForMouseFunc(x,y);
  end;

 inherited;
end;

procedure TGuiADSRGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Enabled then FMouseEdit := meNone;

 inherited;
end;

procedure TGuiADSRGraph.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  IntWidth  : Integer;
  IntHeight : Integer;
begin
 if Enabled then
  case CheckForMouseFunc(x,y) of
      meNone : Cursor := FCursorDefault;
   meSustain : Cursor := FCursorS;
   else Cursor := FCursorADR;
  end;

 IntWidth := Width - 1;
 IntHeight := Height - 1;
 if not (ssLeft in Shift) then FMouseEdit := meNone else
  case FMouseEdit of
   meAttack   : FADSRSettings.Attack := 4 * x / IntWidth;
   meDecay    : FADSRSettings.Decay := 4 * (x - FA) / IntWidth;
   meSustain  : FADSRSettings.Sustain := 1 - y / IntHeight;
   meRelease  : FADSRSettings.Release := 4 * (IntWidth - x) / IntWidth;
  end;

 inherited;
end;

procedure TGuiADSRGraph.Resize;
begin
 inherited;
 CalcIntValues;
end;

end.
