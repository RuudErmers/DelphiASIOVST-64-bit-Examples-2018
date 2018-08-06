unit DAV_SpectralShifting;

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
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_ClassesFft, DAV_Complex,
  DAV_DspFftReal2Complex, DAV_DspSpectralEffects;

type
  TSpectralFrequencyShift32 = class(TCustomSpectralEffect32)
  private
    FBinShift : Integer;
    FShift    : Single;
    procedure SetShift(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;

//    procedure PerformSpectralEffect(SignalIn, SignalOut: PDAVSingleFixedArray); overload; virtual;
    procedure PerformSpectralEffect(Spectum: PDAVComplexSingleFixedArray); overload; override;
    procedure ShiftChanged; virtual;
  public
    constructor Create; override;
    property Shift: Single read FShift write SetShift;
  end;

implementation

{ TSpectralFrequencyShift32 }

constructor TSpectralFrequencyShift32.Create;
begin
 inherited;
 Fft.DataOrder := doPackedComplex;
 FShift := 0;
end;

procedure TSpectralFrequencyShift32.AssignTo(Dest: TPersistent);
begin
 inherited;

end;

procedure TSpectralFrequencyShift32.SetShift(const Value: Single);
begin
 if FShift <> Value then
  begin
   FShift := Value;
   ShiftChanged;
  end;
end;

procedure TSpectralFrequencyShift32.ShiftChanged;
begin
 FBinShift := Round(0.01 * FShift * Fft.BinCount); 
end;

procedure TSpectralFrequencyShift32.PerformSpectralEffect(
  Spectum: PDAVComplexSingleFixedArray);
var
  Bin : Integer;
begin
 inherited;

 Assert(Fft.DataOrder = doPackedComplex);

 if FBinShift > 0 then
  begin
   // build new Nyquist
   Spectum^[0].Im := Sqrt(Sqr(Spectum^[(Fft.BinCount - FBinShift) - 1].Re) +
     Sqr(Spectum^[(Fft.BinCount - FBinShift) - 1].Im));

   Bin := 1;
   Move(Spectum^[Bin], Spectum^[FBinShift + 1], (Fft.BinCount - FBinShift - 2) * SizeOf(TComplex32));

   // shift DC (as complex)
   Spectum^[FBinShift].Re := Spectum^[0].Re;
   Spectum^[FBinShift].Im := 0;

   // clear old DC
   Spectum^[0].Re := 0;

   // fill gap with zero
   FillChar(Spectum^[Bin], (FBinShift - 1) * SizeOf(TComplex32), 0);
  end else
 if FBinShift < 0 then
  begin
   // build new DC
   Spectum^[0].Re := Sqrt(Sqr(Spectum^[-FBinShift].Re) + Sqr(Spectum^[-FBinShift].Im));

   Bin := 1;
   Move(Spectum^[-FBinShift + 1], Spectum^[Bin], (Fft.BinCount + FBinShift - 2) * SizeOf(TComplex32));

   // shift Nyquist as real
   Spectum^[Fft.BinCount + FBinShift].Re := Spectum^[0].Im;
   Spectum^[Fft.BinCount + FBinShift].Im := 0;

   // clear new Nyquist
   Spectum^[0].Im := 0;

   // fill gap with zero
   FillChar(Spectum^[(Fft.BinCount + FBinShift)], (-FBinShift - 1) * SizeOf(TComplex32), 0);
  end;
end;

end.
