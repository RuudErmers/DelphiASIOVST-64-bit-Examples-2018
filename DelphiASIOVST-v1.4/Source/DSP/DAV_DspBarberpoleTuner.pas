unit DAV_DspBarberpoleTuner;

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

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspLfo, DAV_DspTuner,
  DAV_DspFilterButterworth;

type
  TCustomBarberpoleFilter = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FLFO     : TLFOSine32;
    FLowpass : TButterworthLowPassFilter;
    function GetFrequency: Single;
    function GetOrder: Integer;
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual;
    function ProcessSample64(Input: Double): Double; virtual;

    property Frequency: Single read GetFrequency write SetFrequency;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TBarberpoleFilter = class(TCustomBarberpoleFilter, IDspProcessor32)
  published
    property Frequency;
    property Order;
    property SampleRate;
  end;

  TCustomBarberpoleTuner = class(TCustomTuner)
  private
    FBarberpoleFilter : TBarberpoleFilter;
    FZCTuner          : TZeroCrossingTuner;
    function GetFrequency: Single;
    function GetFrequencyDifference: Single;
    function GetOrder: Integer;
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure SampleRateChanged; override;
    function GetCurrentFrequency: Single; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessSample32(Input: Single); override;

    property Frequency: Single read GetFrequency write SetFrequency;
    property FrequencyDifference: Single read GetFrequencyDifference;
    property Order: Integer read GetOrder write SetOrder;
  end;

  TBarberpoleTuner = class(TCustomBarberpoleTuner)
  published
    property Frequency;
    property Order;
  end;

implementation

uses
  SysUtils, DAV_Common, DAV_Approximations;

resourcestring
  RCStrOrderMustBeLarger0 = 'Order must be larger than 0!';

{ TCustomBarberpoleFilter }

procedure TCustomBarberpoleFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBarberpoleFilter then
  with TCustomBarberpoleFilter(Dest) do
   begin
    inherited;
    FLFO.Assign(Self.FLFO);
    FLowpass.Assign(Self.FLowpass);
   end
 else inherited;
end;

constructor TCustomBarberpoleFilter.Create;
begin
 inherited;

 FLFO := TLFOSine32.Create;
 with FLFO do
  begin
   Frequency := 440;
   SampleRate := Self.SampleRate;
  end;

 FLowpass := TButterworthLowPassFilter.Create(4);
 with FLowpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := Self.Frequency * FastPower2MinError4(2 * COneTwelfth32) - Self.Frequency;
  end;
end;

destructor TCustomBarberpoleFilter.Destroy;
begin
 FreeAndNil(FLFO);
 FreeAndNil(FLowpass);
 inherited;
end;

function TCustomBarberpoleFilter.GetFrequency: Single;
begin
 Result := FLFO.Frequency;
end;

function TCustomBarberpoleFilter.GetOrder: Integer;
begin
 Result := FLowpass.Order;
end;

procedure TCustomBarberpoleFilter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TCustomBarberpoleFilter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomBarberpoleFilter.ProcessSample32(Input: Single): Single;
begin
 inherited;
 FLFO.CalculateNextSample;
 Result := FLowpass.ProcessSample64(FLFO.Sine * Input);
end;

function TCustomBarberpoleFilter.ProcessSample64(Input: Double): Double;
begin
 inherited;
 FLFO.CalculateNextSample;
 Result := FLowpass.ProcessSample64(FLFO.Sine * Input);
end;

procedure TCustomBarberpoleFilter.SampleRateChanged;
begin
 inherited;
 if assigned(FLFO) then FLFO.SampleRate := SampleRate;
 if assigned(FLowpass) then FLowpass.SampleRate := SampleRate;
end;

procedure TCustomBarberpoleFilter.SetFrequency(const Value: Single);
begin
 if FLFO.Frequency <> Value then
  begin
   FLFO.Frequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomBarberpoleFilter.FrequencyChanged;
begin
 FLowpass.Frequency := FLFO.Frequency * FastPower2MinError4(2 * COneTwelfth32) - FLFO.Frequency;
 Changed;
end;

procedure TCustomBarberpoleFilter.SetOrder(const Value: Integer);
begin
 if Value > 0
  then FLowpass.Order := Value
  else raise Exception.Create(RCStrOrderMustBeLarger0);
end;


{ TCustomBarberpoleTuner }

procedure TCustomBarberpoleTuner.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBarberpoleTuner then
  with TCustomBarberpoleTuner(Dest) do
   begin
    inherited;
    FBarberpoleFilter.Assign(Self.FBarberpoleFilter);
    FZCTuner.Assign(Self.FZCTuner);
   end
 else inherited;
end;

constructor TCustomBarberpoleTuner.Create;
begin
 inherited;
 FBarberpoleFilter := TBarberpoleFilter.Create;
 with FBarberpoleFilter do
  begin
   Frequency := 440;
   SampleRate := Self.SampleRate;
  end;

 FZCTuner := TZeroCrossingTuner.Create;
 with FZCTuner do
  begin
   MinimumFrequency := 0;
   MaximumFrequency := 2 * Self.Frequency * FastPower2MinError4(2 * COneTwelfth32) - Self.Frequency;
   OneCrossingOnly := True;
   SampleRate := Self.SampleRate;
  end;
end;

destructor TCustomBarberpoleTuner.Destroy;
begin
 FreeAndNil(FBarberpoleFilter);
 FreeAndNil(FZCTuner);
 inherited;
end;

procedure TCustomBarberpoleTuner.SampleRateChanged;
begin
 inherited;
 if assigned(FBarberpoleFilter) then FBarberpoleFilter.SampleRate := SampleRate;
 if assigned(FZCTuner) then FZCTuner.SampleRate := SampleRate;
end;

procedure TCustomBarberpoleTuner.SetFrequency(const Value: Single);
begin
 FBarberpoleFilter.Frequency := Value;
 FZCTuner.MaximumFrequency := 2 * Frequency * FastPower2MinError4(2 * COneTwelfth32) - Frequency;
end;

procedure TCustomBarberpoleTuner.SetOrder(const Value: Integer);
begin
 FBarberpoleFilter.Order := Value;
end;

function TCustomBarberpoleTuner.GetCurrentFrequency: Single;
begin
 Result := Frequency + FrequencyDifference;
end;

function TCustomBarberpoleTuner.GetFrequency: Single;
begin
 Result := FBarberpoleFilter.Frequency;
end;

function TCustomBarberpoleTuner.GetFrequencyDifference: Single;
begin
 Result := FZCTuner.CurrentFrequency;
end;

function TCustomBarberpoleTuner.GetOrder: Integer;
begin
 Result := FBarberpoleFilter.Order;
end;

procedure TCustomBarberpoleTuner.ProcessSample32(Input: Single);
begin
 inherited;
 FZCTuner.ProcessSample32(FBarberpoleFilter.ProcessSample32(Input));
end;

initialization
  RegisterDspProcessor32(TBarberpoleFilter);

end.
