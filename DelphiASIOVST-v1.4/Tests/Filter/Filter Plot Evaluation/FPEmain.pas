unit FPEmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_Common, DAV_Complex, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspFilterBasicsAutomatable, DAV_DspFilterButterworth,
  DAV_DspFilterChebyshev, DAV_DspFilterChebyshevType1,
  DAV_DspFilterChebyshevType2, DAV_GuiEQGraph;

type
  TForm1 = class(TForm)
    GuiEQGraph: TGuiEQGraph;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function GetFilterGain(Sender: TObject; const Frequency: Single): Single;
  private
    FFilter: TCustomFilter;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 FFilter := TButterworthHighCutFilter.Create;
 TButterworthHighCutFilter(FFilter).SetFilterValues(1000, 0);
// TButterworthHighPassFilter(FFilter).Ripple := 1;
 TButterworthHighCutFilter(FFilter).Order := 3;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FFilter);
end;

function TForm1.GetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
var
  Cmplx : TComplex64;
begin
 FFilter.Complex(Frequency, Cmplx.Re, Cmplx.Im);
 Result := 10 * Log10(Sqr(Cmplx.Re) + Sqr(Cmplx.Im));
(*
 Result := FFilter.MagnitudeLog10(Frequency);
*)


// Result := 10 * log10(FFilter.MagnitudeSquared(Frequency));
(*
*)

 // Result := 180 / Pi * FFilter.Phase(Frequency)
end;

end.
