unit OscTestModuleU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule, DAVDProcessingComponent, DDspBaseComponent,
  DDSPBaseOsc, DDSPOscNoise, DDSPOscSine, DDSPOscAbsSine, DDSPOscSquare,
  DDSPOscRamp, DDSPOscSaw;

type
  TOscTestModule = class(TVSTModule)
    DspOscSine1: TDspOscSine;
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  OscTestFormU;

procedure TOscTestModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TOscTestForm.Create(Self);
end;

end. 