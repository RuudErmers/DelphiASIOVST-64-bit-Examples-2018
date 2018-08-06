unit DAV_DspFFT;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The code is based on the FFT routine, (C)1996 S.M.Bernsee                 //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

implementation

uses
  Math, DAV_Types, DAV_Math;

procedure FFT(Buffer: PDAVSingleFixedArray; FFTSize: Integer);
(*
 Sign = -1 is FFT, 1 is iFFT (inverse) fills fftBuffer[0...2*fftFrameSize-1]
 with the Fourier transform of the time domain data in
 fftBuffer[0...2*fftFrameSize-1]. The FFT array takes and returns the cosine
 and sine parts in an interleaved manner, ie. fftBuffer[0] = cosPart[0],
 fftBuffer[1] = sinPart[0], asf. fftFrameSize must be a power of 2.
 It expects a complex input signal (see footnote 2), ie. when working with
 'common' audio signals our input signal has to be passed as
 {in[0],0.,in[1],0.,in[2],0.,...end; asf. In that case, the transform of
 the frequencies of interest is in fftBuffer[0...fftFrameSize].
*)
var
  wr, wi, arg, temp  : Single;
  tr, ti, ur, ui     : Single;
  p1r, p1i, p2r, p2i : PSingle;
  i, bitm, j, le     : Integer;
  le2, k, logN       : Integer;

begin
 logN := round(log2(FFTSize) + 0.5);
                     
 i := 2;
 while i < 2 * FFTSize - 2 do
  begin

   j := 0;
   bitm := 2;
   while bitm < 2 * FFTSize do
    begin
     if (i and bitm) > 0
      then inc(j);
     j := j shl 1;
     bitm := bitm shl 1;
    end;

   if (i < j) then
    begin
     temp           := Buffer^[i];
     Buffer^[i]     := Buffer^[j];
     Buffer^[j]     := temp;
     temp           := Buffer^[i + 1];
     Buffer^[i + 1] := Buffer^[j + 1];
     Buffer^[j + 1] := temp;
    end;

   inc(i, 2);
  end;

 k := 0;
 le := 2;
 while k < logN do
  begin
   le  := le shl 1;
   le2 := le shr 1;
   ur  := 1.0;
   ui  := 0.0;
   arg := PI / (le2 shr 1);
   GetSinCos(arg, wr, wi);

   j := 0;
   while j < le2 do
    begin
     p1r := @Buffer[j          ];
     p1i := @Buffer[j       + 1];
     p2r := @Buffer[j + le2    ];
     p2i := @Buffer[j + le2 + 1];

     i := j;
     while i < 2 * FFTSize do
      begin
       tr   := p2r^ * ur - p2i^ * ui;
       ti   := p2r^ * ui + p2i^ * ur;
       p2r^ := p1r^ - tr;
       p2i^ := p1i^ - ti;
       p1r^ := p1r^ + tr;
       p1i^ := p1i^ + ti;
       Inc(p1r, le);
       Inc(p1i, le);
       Inc(p2r, le);
       Inc(p2i, le);
       i := i + le;
      end;

     tr := ur * wr - ui * wi;
     ui := ur * wi + ui * wr;
     ur := tr;

     inc(j, 2);
    end;
   inc(k);
  end;
end;

end.
