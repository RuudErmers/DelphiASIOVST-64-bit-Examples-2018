unit NonlinearGUI;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  Winapi.GDIPAPI, FMX.Canvas.GDIP, FMX.Canvas.D2D, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Dialogs;

type
  TFmNonlinearFMX = class(TForm)
    SBGain: TScrollBar;
    LbGain: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure SBGainChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure UpdateGain;
  end;

var
  FmNonlinearFMX: TFmNonlinearFMX;

implementation

{$R *.fmx}

uses
  NonlinearDSP;

{ TFmNonlinearFMX }

procedure TFmNonlinearFMX.FormCreate(Sender: TObject);
begin
 LbGain.Text  := 'OpAmp Gain';
 SbGain.Max   := 1000;
 SbGain.Min   := 100;
 SbGain.Value := 100;
end;

procedure TFmNonlinearFMX.FormActivate(Sender: TObject);
begin
 UpdateGain;
end;

procedure TFmNonlinearFMX.SBGainChange(Sender: TObject);
begin
 with TVSTOpAmp(Owner) do
  begin
   if Parameter[0] <> SBGain.Value * 0.1
    then Parameter[0] := SBGain.Value * 0.1;
  end;
end;

procedure TFmNonlinearFMX.UpdateGain;
begin
 with TVSTOpAmp(Owner) do
  begin
   if Round(10 * Parameter[0]) <> SBGain.Value
    then SBGain.Value := Round(10 * Parameter[0]);
  end;
end;

var
  StartupInput: TGDIPlusStartupInput;
  gdiplusToken: Cardinal;

initialization
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;
  StartupInput.GdiplusVersion := 1;

  GdiplusStartup(gdiplusToken, @StartupInput, nil);

  SetD2DDefault;

finalization
  GdiplusShutdown(gdiplusToken);

end.
