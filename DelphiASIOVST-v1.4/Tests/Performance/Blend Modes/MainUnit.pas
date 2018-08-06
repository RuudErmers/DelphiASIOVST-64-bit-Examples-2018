unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DAV_GuiBlend, DAV_MemoryUtils;

type
  TFmPerformanceTest = class(TForm)
    MeResults: TMemo;
    BtStartTest: TButton;
    procedure BtStartTestClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    function TestBlend: Single;
  end;

var
  FmPerformanceTest: TFmPerformanceTest;

implementation

{$R *.dfm}

uses
  DAV_GuiCommon, DAV_Bindings;

procedure TFmPerformanceTest.BtStartTestClick(Sender: TObject);
begin
 // x86
 BindingBlend.Rebind([]);
 MeResults.Lines.Add('x86 Performance: ' + FloatToStr(1000 * TestBlend));

 // MMX
 BindingBlend.Rebind([pfMMX]);
 MeResults.Lines.Add('MMX Performance: ' + FloatToStr(1000 * TestBlend));

 // SSE2
 BindingBlend.Rebind([pfSSE2]);
 MeResults.Lines.Add('SSE2 Performance: ' + FloatToStr(1000 * TestBlend));
end;

function TFmPerformanceTest.TestBlend: Single;
var
  Start, Stop, Freq : Int64;
  BlendColor32      : PPixel32;
  TestIndex, Index  : Integer;
begin
 GetAlignedMemory(Pointer(BlendColor32), SizeOf(PPixel32));
 BlendColor32^ := pxWhite32;
 BlendColor32^.A := $5A;

 Application.ProcessMessages;

 QueryPerformanceFrequency(Freq);
 QueryPerformanceCounter(Start);
 for Index := 0 to $7FFF do
  begin
   BlendPixel(BlendColor32^, pxBlack32);
   BlendPixel(BlendColor32^, pxRed32);
   BlendPixel(BlendColor32^, pxGreen32);
   BlendPixel(BlendColor32^, pxYellow32);
   BlendPixel(BlendColor32^, pxBlue32);
   BlendPixel(BlendColor32^, pxGray32);
   BlendPixel(BlendColor32^, pxFuchsia32);
   BlendPixel(BlendColor32^, pxWhite32);
  end;
 EMMS;
 QueryPerformanceCounter(Stop);

 Result := (Stop - Start) / Freq;

 for TestIndex := 0 to $FF do
  begin
   QueryPerformanceFrequency(Freq);
   QueryPerformanceCounter(Start);
   for Index := 0 to $7FFF do
    begin
     BlendPixel(BlendColor32^, pxBlack32);
     BlendPixel(BlendColor32^, pxRed32);
     BlendPixel(BlendColor32^, pxGreen32);
     BlendPixel(BlendColor32^, pxYellow32);
     BlendPixel(BlendColor32^, pxBlue32);
     BlendPixel(BlendColor32^, pxGray32);
     BlendPixel(BlendColor32^, pxFuchsia32);
     BlendPixel(BlendColor32^, pxWhite32);
    end;
   EMMS;
   QueryPerformanceCounter(Stop);

   if (Stop - Start) / Freq < Result
    then Result := (Stop - Start) / Freq;
   Application.ProcessMessages;
  end;

 FreeAlignedMemory(Pointer(BlendColor32));
end;

end.

