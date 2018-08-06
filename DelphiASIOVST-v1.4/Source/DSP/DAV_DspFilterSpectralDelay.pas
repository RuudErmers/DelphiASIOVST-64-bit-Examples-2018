unit DAV_DspFilterSpectralDelay;

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
//  The code is based on the paper SPECTRAL DELAY FILTERS WITH FEEDBACK AND   //
//  TIME-VARYING COEFFICIENTS by Jussi Pekonen, Vesa Välimäki,                //
//  Jonathan S. Abel and Julius O. Smith                                      //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Classes, DAV_DspFilterSimple, DAV_DspFilter;

type
  TSpectralDelayFilter = class(TCustomIIRFilter)
  private
    FFilterCount : Integer;
    FFilters     : array of TFirstOrderAllpassFilter;
    procedure SetFilterCount(const Value: Integer);
  protected
    procedure FilterCountChanged;
    procedure SampleRateChanged; override;
    procedure CalculateCoefficients; override;
    procedure FrequencyChanged; override;
  public
    constructor Create; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override;
  published
    property FilterCount: Integer read FFilterCount write SetFilterCount;
  end;

implementation

uses
  Math, SysUtils;

{ TSpectralDelayFilter }

procedure TSpectralDelayFilter.CalculateCoefficients;
begin
 inherited;
 // do nothing!
end;

constructor TSpectralDelayFilter.Create;
begin
 inherited;
 FFilterCount := 16;
 FilterCountChanged;
end;

function TSpectralDelayFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := Log10(MagnitudeSquared(Frequency));
end;

function TSpectralDelayFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  Filter : Integer;
begin
 Result := 1;
 for Filter := 0 to Length(FFilters) - 1
  do Result := Result * FFilters[Filter].MagnitudeSquared(Frequency);
end;

procedure TSpectralDelayFilter.PopStates;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].PopStates;
end;

function TSpectralDelayFilter.ProcessSample32(Input: Single): Single;
var
  Filter : Integer;
begin
 Result := Input;
 for Filter := 0 to Length(FFilters) - 1
  do Result := FFilters[Filter].ProcessSample64(Result);
end;

function TSpectralDelayFilter.ProcessSample64(Input: Double): Double;
var
  Filter : Integer;
begin
 Result := Input;
 for Filter := 0 to Length(FFilters) - 1
  do Result := FFilters[Filter].ProcessSample64(Result);
end;

procedure TSpectralDelayFilter.PushStates;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].PushStates;
end;

procedure TSpectralDelayFilter.Reset;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].Reset;
end;

procedure TSpectralDelayFilter.ResetStates;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].ResetStates;
end;

procedure TSpectralDelayFilter.ResetStatesInt64;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].ResetStatesInt64;
end;

procedure TSpectralDelayFilter.SampleRateChanged;
var
  Filter : Integer;
begin
 inherited;
 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].SampleRate := SampleRate;
end;

procedure TSpectralDelayFilter.SetFilterCount(const Value: Integer);
begin
 if FFilterCount <> Value then
  begin
   FFilterCount := Value;
   FilterCountChanged;
  end;
end;

procedure TSpectralDelayFilter.FilterCountChanged;
var
  Filter    : Integer;
  OldLength : Integer;
begin
 if Length(FFilters) < FFilterCount then
  begin
   OldLength := Length(FFilters);
   SetLength(FFilters, FFilterCount);
   for Filter := OldLength to FFilterCount - 1 do
    begin
     FFilters[Filter] := TFirstOrderAllpassFilter.Create;
     with FFilters[Filter] do
      begin
       SampleRate := Self.SampleRate;
       Frequency := 0.9;
      end;
    end;
  end
 else
  begin
   for Filter := FFilterCount to Length(FFilters) - 1
    do FreeAndNil(FFilters[Filter]);
   SetLength(FFilters, FFilterCount);
  end;
end;

procedure TSpectralDelayFilter.FrequencyChanged;
var
  Filter : Integer;
begin
 inherited;

//  @CWB: there is no TFirstOrderAllpassFilter.Frequency

// for Filter := 0 to Length(FFilters) - 1
//  do FFilters[Filter].Frequency := Frequency;

 for Filter := 0 to Length(FFilters) - 1
  do FFilters[Filter].FractionalDelay := Frequency;

end;

initialization
  RegisterDspProcessor32(TSpectralDelayFilter);
  RegisterDspProcessor64(TSpectralDelayFilter);

end.
