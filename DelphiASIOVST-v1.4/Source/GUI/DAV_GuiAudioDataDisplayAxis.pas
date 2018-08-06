unit DAV_GuiAudioDataDisplayAxis;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

type
  TCustomGuiAudioDataDisplayAxis = class(TPersistent)
  protected
    FOnChanged: TNotifyEvent;
    procedure AxisChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TCustomGuiAudioDataDisplayXAxis = class(TCustomGuiAudioDataDisplayAxis)
  private
    procedure SetSampleLower(const Value: Int64);
    procedure SetSampleUpper(const Value: Int64);
    procedure SetFractionalLower(const Value: Single);
    procedure SetFractionalUpper(const Value: Single);
    procedure ResetFractionals;
  protected
    FFractionalLower : Single;
    FFractionalUpper : Single;
    FSampleUpper     : Int64;
    FSampleLower     : Int64;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure SetBounds(const Lower, Upper: Int64); overload;
    procedure SetBounds(const Lower: Int64; LowerFractional: Single;
      const Upper: Int64; UpperFractional: Single); overload;
    property SampleLower: Int64 read FSampleLower write SetSampleLower default 0;
    property SampleUpper: Int64 read FSampleUpper write SetSampleUpper default 0;
    property FractionalLower: Single read FFractionalLower write SetFractionalLower;
    property FractionalUpper: Single read FFractionalUpper write SetFractionalUpper;
  end;

  TGuiAudioDataDisplayXAxis = class(TCustomGuiAudioDataDisplayXAxis)
  published
    property SampleLower;
    property SampleUpper;
    property FractionalLower;
    property FractionalUpper;
  end;


implementation

uses
  SysUtils;

{ TCustomGuiAudioDataDisplayAxis }

procedure TCustomGuiAudioDataDisplayAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiAudioDataDisplayAxis then
  with TCustomGuiAudioDataDisplayAxis(Dest) do
   begin
    FOnChanged := Self.FOnChanged;
   end
 else inherited;
end;

procedure TCustomGuiAudioDataDisplayAxis.AxisChanged;
begin
 if assigned(FOnChanged)
  then FOnChanged(Self);
end;

{ TCustomGuiAudioDataDisplayXAxis }

procedure TCustomGuiAudioDataDisplayXAxis.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGuiAudioDataDisplayXAxis then
  with TCustomGuiAudioDataDisplayXAxis(Dest) do
   begin
    FFractionalLower := Self.FFractionalLower;
    FFractionalUpper := Self.FFractionalUpper;
    FSampleLower := Self.FSampleLower;
    FSampleUpper := Self.FSampleUpper;
   end;
 inherited;
end;

constructor TCustomGuiAudioDataDisplayXAxis.Create;
begin
 inherited;
 FSampleLower := 0;
 FSampleUpper := 0;
 ResetFractionals;
end;

procedure TCustomGuiAudioDataDisplayXAxis.SetBounds(const Lower, Upper: Int64);
begin
 if Lower > Upper
  then raise Exception.Create('Error: lower > upper!');

 FSampleLower := Lower;
 FSampleUpper := Upper;
 ResetFractionals;
 AxisChanged;
end;

procedure TCustomGuiAudioDataDisplayXAxis.SetBounds(const Lower: Int64;
  LowerFractional: Single; const Upper: Int64; UpperFractional: Single);
begin
 // ensure lower < upper
 if (Lower > Upper) or ((Lower = Upper) and (LowerFractional > UpperFractional))
  then raise Exception.Create('Error: lower > upper!');

 FSampleLower := Lower;
 FSampleUpper := Upper;
 FFractionalLower := LowerFractional;
 FFractionalUpper := UpperFractional;
 AxisChanged;
end;

procedure TCustomGuiAudioDataDisplayXAxis.ResetFractionals;
begin
 FFractionalLower := -0.5;
 FFractionalUpper := +0.5;
end;

procedure TCustomGuiAudioDataDisplayXAxis.SetFractionalLower(
  const Value: Single);
begin
 if FFractionalLower <> Value then
  begin
   FFractionalLower := Value;
   AxisChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplayXAxis.SetFractionalUpper(
  const Value: Single);
begin
 if FFractionalUpper <> Value then
  begin
   FFractionalUpper := Value;
   AxisChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplayXAxis.SetSampleLower(const Value: Int64);
begin
 if FSampleLower <> Value then
  begin
   FSampleLower := Value;
   AxisChanged;
  end;
end;

procedure TCustomGuiAudioDataDisplayXAxis.SetSampleUpper(const Value: Int64);
begin
 if FSampleUpper <> Value then
  begin
   FSampleUpper := Value;
   AxisChanged;
  end;
end;

end.
