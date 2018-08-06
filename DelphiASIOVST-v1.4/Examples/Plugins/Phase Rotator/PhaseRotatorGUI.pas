unit PhaseRotatorGUI;

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, StdCtrls, Controls, DAV_GuiPng, DAV_GuiLabel, DAV_GuiStitchedPngList,
  DAV_GuiStitchedDial, DAV_GuiImageControl, DAV_GuiStitchedControls,
  DAV_GuiCustomControl, DAV_GuiGraphicControl;

type
  TFmPhaseRotator = class(TForm)
    LbBandwidth: TGuiLabel;
    LbBandwidthValue: TGuiLabel;
    LbFreq: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    DialFrequency: TGuiStitchedDial;
    DialStages: TGuiStitchedDial;
    DialBandwidth: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialBandwidthChange(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure DialFrequencyDblClick(Sender: TObject);
    procedure DialStagesDblClick(Sender: TObject);
    procedure DialBandwidthDblClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateStages;
    procedure UpdateBandwidth;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PhaseRotatorDSP;

procedure TFmPhaseRotator.FormShow(Sender: TObject);
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner)
  do DialStages.Max := ParameterProperties[1].Max;
 UpdateFrequency;
 UpdateBandwidth;
 UpdateStages;
end;

procedure TFmPhaseRotator.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmPhaseRotator.FormClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmPhaseRotator.DialBandwidthDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbBandwidthValue.Left;
   Top := LbBandwidthValue.Top;
   Width := LbBandwidthValue.Width;
   Height := LbBandwidthValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbBandwidthValue.Caption;
   Tag := 2;
   OnKeyPress := EdValueKeyPress;
   Font.Assign(LbBandwidthValue.Font);
   SetFocus;
  end;
end;

procedure TFmPhaseRotator.DialFrequencyChange(Sender: TObject);
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Value
    then Parameter[0] := DialFrequency.Value;
  end;
end;

procedure TFmPhaseRotator.DialFrequencyDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   Font.Assign(LbFrequencyValue.Font);
   SetFocus;
  end;
end;

procedure TFmPhaseRotator.DialStagesChange(Sender: TObject);
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  begin
   if Parameter[1] <> Round(DialStages.Value)
    then Parameter[1] := Round(DialStages.Value);
  end;
end;

procedure TFmPhaseRotator.DialStagesDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbStagesValue.Left;
   Top := LbStagesValue.Top;
   Width := LbStagesValue.Width;
   Height := LbStagesValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbStagesValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   Font.Assign(LbStagesValue.Font);
   SetFocus;
  end;
end;

procedure TFmPhaseRotator.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, AnsiString(FEdValue.Text));
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmPhaseRotator.DialBandwidthChange(Sender: TObject);
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  begin
   if Parameter[2] <> DialBandwidth.Value
    then Parameter[2] := DialBandwidth.Value;
  end;
end;

procedure TFmPhaseRotator.UpdateFrequency;
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  begin
   if DialFrequency.Value <> Parameter[0]
    then DialFrequency.Value := Parameter[0];

   LbFrequencyValue.Caption := string(ParameterDisplay[0] + ' ' + ParameterLabel[0]);
  end;
end;

procedure TFmPhaseRotator.UpdateStages;
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  begin
   if DialStages.Value <> Parameter[1]
    then DialStages.Value := Parameter[1];

   LbStagesValue.Caption := string(ParameterDisplay[1]);
  end;
end;

procedure TFmPhaseRotator.UpdateBandwidth;
begin
 Assert(Owner is TPhaseRotatorModule);
 with TPhaseRotatorModule(Owner) do
  begin
   if DialBandwidth.Value <> Parameter[2]
    then DialBandwidth.Value := Parameter[2];

   LbBandwidthValue.Caption := string(ParameterDisplay[2] + ' ' + ParameterLabel[2]);
  end;
end;

end.
