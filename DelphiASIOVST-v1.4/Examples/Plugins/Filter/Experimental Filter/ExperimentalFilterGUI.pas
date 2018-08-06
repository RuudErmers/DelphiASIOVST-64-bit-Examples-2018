unit ExperimentalFilterGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, StdCtrls, DAV_GuiGroup, DAV_GuiCustomControl, DAV_GuiImageControl,
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedPngList,
  DAV_GuiGraphicControl, DAV_GuiLabel, DAV_GuiSlider;

type
  TFmExperimentalFilter = class(TForm)
    GbExperimentalFilter: TGuiGroupSimple;
    DlFrequency: TGuiStitchedDial;
    SlGain: TGuiSlider;
    SlBandwidth: TGuiSlider;
    SlFrequency: TGuiSlider;
    procedure SlFrequencyChange(Sender: TObject);
    procedure SlGainChange(Sender: TObject);
    procedure SlBandwidthChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateFrequency;
    procedure UpdateGain;
    procedure UpdateBandwidth;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ExperimentalFilterDSP;

procedure TFmExperimentalFilter.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateGain;
 UpdateBandwidth;
end;

procedure TFmExperimentalFilter.SlFrequencyChange(Sender: TObject);
begin
 with TExperimentalFilterModule(Owner) do
  begin
   if Parameter[0] <> SlFrequency.Value
    then Parameter[0] := SlFrequency.Value;
  end;
end;

procedure TFmExperimentalFilter.SlGainChange(Sender: TObject);
begin
 with TExperimentalFilterModule(Owner) do
  begin
   if Parameter[1] <> SlGain.Value
    then Parameter[1] := SlGain.Value;
  end;
end;

procedure TFmExperimentalFilter.SlBandwidthChange(Sender: TObject);
begin
 with TExperimentalFilterModule(Owner) do
  begin
   if Parameter[2] <> SlBandwidth.Value
    then Parameter[2] := SlBandwidth.Value;
  end;
end;

procedure TFmExperimentalFilter.UpdateFrequency;
begin
 with TExperimentalFilterModule(Owner) do
  begin
   if SlFrequency.Value <> Parameter[0]
    then SlFrequency.Value := Parameter[0];
  end;
end;

procedure TFmExperimentalFilter.UpdateGain;
begin
 with TExperimentalFilterModule(Owner) do
  begin
   if SlGain.Value <> Parameter[1]
    then SlGain.Value := Parameter[1];
  end;
end;

procedure TFmExperimentalFilter.UpdateBandwidth;
begin
 with TExperimentalFilterModule(Owner) do
  begin
   if SlBandwidth.Value <> Parameter[2]
    then SlBandwidth.Value := Parameter[2];
  end;
end;

end.
