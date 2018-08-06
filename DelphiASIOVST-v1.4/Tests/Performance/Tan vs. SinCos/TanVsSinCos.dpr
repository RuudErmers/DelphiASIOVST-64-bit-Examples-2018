program TanVsSinCos;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, DAV_Common, DAV_Complex;

procedure GetSinCos(const Frequency: Double; var SinValue, CosValue : Double);
asm
 fld Frequency.Double
 fsincos
 fst CosValue.Double
 fdivp
 fstp SinValue.Double
end;

function Tan(const X: Double): Double;
{  Tan := Sin(X) / Cos(X) }
asm
 fld X
 fptan
 fstp ST(0)
end;

var
  i : Integer;
  A, B, C : Int64;
  Cmplx : TComplexDouble;

begin
 QueryPerformanceCounter(A);
 for i := 0 to 9000000 do
  begin
   GetSinCos(0.1 + random, Cmplx.Re, Cmplx.Im);
  end;
 QueryPerformanceCounter(B);
 QueryPerformanceFrequency(C);
 writeln('SinCos calculation: ' + FloatToStrF((B - A) / C, ffGeneral, 4, 4));

 QueryPerformanceCounter(A);
 for i := 0 to 9000000 do
  begin
   tan(0.1 + random);
  end;
 QueryPerformanceCounter(B);
 QueryPerformanceFrequency(C);
 writeln('Tan calculation: ' + FloatToStrF((B - A) / C, ffGeneral, 4, 4));

 readln;

end.
