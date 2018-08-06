unit PhaseAdjustmentGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, DAV_GuiCustomControl, DAV_GuiStitchedControls, DAV_GuiStitchedDial,
  DAV_GuiStitchedPngList;

type
  TFmPhaseAdjustment = class(TForm)
    PngList: TGuiStitchedPNGList;
    PhaseDial: TGuiStitchedDial;
    procedure PhaseDialChange(Sender: TObject);
  public
    procedure UpdatePhaseDial;
  end;

implementation

uses
  PhaseAdjustmentDSP;

{$R *.DFM}

procedure TFmPhaseAdjustment.PhaseDialChange(Sender: TObject);
begin
 with TPhaseAdjustmentModule(Owner) do
  begin
   if Parameter[0] <> PhaseDial.Value
    then Parameter[0] := PhaseDial.Value;
  end;
end;

procedure TFmPhaseAdjustment.UpdatePhaseDial;
begin
 with TPhaseAdjustmentModule(Owner) do
  begin
   if PhaseDial.Value <> Parameter[0]
    then PhaseDial.Value := Parameter[0];
  end;
end;

end.
