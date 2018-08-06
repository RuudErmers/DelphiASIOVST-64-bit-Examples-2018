unit DAV_DspUpDownsampling;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Types, DAV_Classes, DAV_DspFilter,
  DAV_DspFilterButterworth, DAV_DspBesselFilter, DAV_DspFilterChebyshev,
  DAV_DspFilterChebyshevType1, DAV_DspFilterChebyshevType2;

type
  TDAVResampling = class(TDspSampleRatePersistent)
  private
    FFilterClass : TOrderFilterClass;
    procedure SetFactor(const Value: Integer);
    procedure SetOrder(const Value: Integer);
    procedure SetTransitionBandwidth(const Value: Double);
    procedure SetFilterClass(const Value: TOrderFilterClass);
  protected
    FFactor              : Integer;
    FOrder               : Integer;
    FTransitionBandwidth : Double;
    procedure AssignTo(Dest: TPersistent); override;
    procedure FactorChanged; virtual;
    procedure FilterClassChanged; virtual; abstract;
    procedure OrderChanged; virtual;
    procedure SampleRateChanged; override;
    procedure TransitionBandwidthChanged; virtual;
    procedure UpdateFilter; virtual; abstract;
  public
    constructor Create; override;
    procedure ResetStates; virtual; abstract;

    property FilterClass: TOrderFilterClass read FFilterClass write SetFilterClass;
  published
    property Factor: Integer read FFactor write SetFactor;
    property Order: Integer read FOrder write SetOrder default 2;
    property TransitionBandwidth: Double read FTransitionBandwidth write SetTransitionBandwidth;
    property SampleRate;
  end;

  TDAVUpSampling = class(TDAVResampling)
  private
    FFilter : TCustomOrderFilter;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FilterClassChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateFilter; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ResetStates; override;

    procedure Upsample32(Input: Single; Output: PDAVSingleFixedArray);
    procedure Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
  end;

  TDAVDownSampling = class(TDAVResampling)
  private
    FFilter : TCustomOrderFilter;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FilterClassChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateFilter; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ResetStates; override;

    function Downsample32(Input: PDAVSingleFixedArray): Single;
    function Downsample64(Input: PDAVDoubleFixedArray): Double;
  end;

  TDAVUpDownsampling = class(TDAVResampling)
  private
    FFilter : array [0..1] of TCustomOrderFilter;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FilterClassChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateFilter; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ResetStates; override;

    procedure Upsample32(Input: Single; Output: PDAVSingleFixedArray);
    procedure Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
    function Downsample32(Input: PDAVSingleFixedArray): Single;
    function Downsample64(Input: PDAVDoubleFixedArray): Double;
  end;

implementation

uses
  DAV_Common;

{ TDAVResampling }

constructor TDAVResampling.Create;
begin
 inherited;
 FFactor              := 1;
 FTransitionBandwidth := 0.99;
 Order                := 2;
end;

procedure TDAVResampling.AssignTo(Dest: TPersistent);
begin
 if Dest is TDAVResampling then
  with TDAVResampling(Dest) do
   begin
    inherited;
    FFactor              := Self.FFactor;
    FOrder               := Self.FOrder;
    FTransitionBandwidth := Self.FTransitionBandwidth;
   end
 else inherited;
end;

procedure TDAVResampling.SetFactor(const Value: Integer);
begin
 if FFactor <> Value then
  begin
   FFactor := Value;
   FactorChanged;
  end;
end;

procedure TDAVResampling.SetFilterClass(const Value: TOrderFilterClass);
begin
 if FFilterClass <> Value then
  begin
   FFilterClass := Value;
   FilterClassChanged;
  end;
end;

procedure TDAVResampling.FactorChanged;
begin
 UpdateFilter;
 Changed;
end;

procedure TDAVResampling.OrderChanged;
begin
 UpdateFilter;
 Changed;
end;

procedure TDAVResampling.TransitionBandwidthChanged;
begin
 UpdateFilter;
 Changed;
end;

procedure TDAVResampling.SetTransitionBandwidth(const Value: Double);
begin
 if FTransitionBandwidth <> Value then
  begin
   FTransitionBandwidth := Value;
   TransitionBandwidthChanged;
  end;
end;

procedure TDAVResampling.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TDAVResampling.SampleRateChanged;
begin
 UpdateFilter;
 Changed;
end;


{ TDAVUpDownsampling }

procedure TDAVUpDownsampling.ResetStates;
begin
 FFilter[0].ResetStates;
 FFilter[1].ResetStates;
end;

constructor TDAVUpDownsampling.Create;
begin
 FilterClass := TButterworthLowPassFilter;
 inherited;
end;

destructor TDAVUpDownsampling.Destroy;
begin
 FreeAndNil(FFilter[0]);
 FreeAndNil(FFilter[1]);
 inherited;
end;

procedure TDAVUpDownsampling.AssignTo(Dest: TPersistent);
begin
 if Dest is TDAVUpDownsampling then
  with TDAVUpDownsampling(Dest) do
   begin
    inherited;
    FFilter[0].Assign(Self.FFilter[0]);
    FFilter[1].Assign(Self.FFilter[1]);
   end
 else inherited;
end;

procedure TDAVUpDownsampling.UpdateFilter;
var
  Frequency : Double;
begin
 Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
 Assert(Assigned(FFilter[0]));
 Assert(Assigned(FFilter[1]));
 FFilter[0].Frequency := Frequency;
 FFilter[1].Frequency := Frequency;
 FFilter[0].ResetStates;
 FFilter[1].ResetStates;
end;

procedure TDAVUpDownsampling.Upsample32(Input: Single;
  Output: PDAVSingleFixedArray);
var
  i : Integer;
begin
 Output[0] := FFilter[0].ProcessSample64(Factor * Input + cDenorm32);
 for i := 1 to Factor - 1
  do Output[i] := FFilter[0].ProcessSample64(-cDenorm32);
end;

procedure TDAVUpDownsampling.Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
var
  i : Integer;
begin
 Output[0] := FFilter[0].ProcessSample64(Factor * Input + cDenorm64);
 for i := 1 to Factor - 1
  do Output[i] := FFilter[0].ProcessSample64(-cDenorm64);
end;

function TDAVUpDownsampling.Downsample32(Input: PDAVSingleFixedArray): Single;
var
  i : Integer;
begin
 Result := FFilter[1].ProcessSample64(Input[0] + cDenorm32);
 for i := 1 to Factor - 1
  do FFilter[1].ProcessSample64(Input[i]);
end;

function TDAVUpDownsampling.Downsample64(Input: PDAVDoubleFixedArray): Double;
var
  i : Integer;
begin
 result := FFilter[1].ProcessSample64(Input[0] + cDenorm64);
 for i := 1 to Factor - 1
  do FFilter[1].ProcessSample64(Input[i] - cDenorm64);
end;


procedure TDAVUpDownsampling.FilterClassChanged;
var
  i         : Integer;
  oldFilter : TCustomOrderFilter;
begin
 for i := 0 to Length(FFilter) - 1 do
  begin
   oldFilter := FFilter[i];
   FFilter[i] := FFilterClass.Create(FOrder);
   if Assigned(oldFilter)
    then FFilter[i].Assign(oldFilter);
   if FFilter[i] is TCustomChebyshev1Filter then
    with TCustomChebyshev1Filter(FFilter[i]) do
     begin
      Ripple := 0.1;
     end;
   FreeAndNil(oldFilter);
  end;
 Changed;
end;

procedure TDAVUpDownsampling.OrderChanged;
begin
 FFilter[0].Order := FOrder;
 FFilter[1].Order := FOrder;
 FFilter[0].ResetStates;
 FFilter[1].ResetStates;
 inherited;
end;

procedure TDAVUpDownsampling.SampleRateChanged;
begin
 FFilter[0].SampleRate := SampleRate;
 FFilter[1].SampleRate := SampleRate;
 inherited;
end;

{ TDAVUpSampling }

constructor TDAVUpSampling.Create;
begin
 FilterClass := TButterworthLowPassFilter;
 inherited;
end;

destructor TDAVUpSampling.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TDAVUpSampling.AssignTo(Dest: TPersistent);
begin
 if Dest is TDAVUpSampling then
  with TDAVUpSampling(Dest) do
   begin
    inherited;
    FFilter.Assign(Self.FFilter);
   end
 else inherited;
end;

procedure TDAVUpSampling.ResetStates;
begin
 FFilter.ResetStates;
end;

procedure TDAVUpSampling.FilterClassChanged;
var
  OldFilter : TCustomOrderFilter;
begin
 OldFilter := FFilter;
 FFilter := FFilterClass.Create;
 if Assigned(oldFilter)
  then FFilter.Assign(oldFilter);
 if FFilter is TCustomChebyshev1Filter then
  with TCustomChebyshev1Filter(FFilter) do
   begin
    Ripple := 0.1;
   end;
 FreeAndNil(oldFilter);
 Changed;
end;

procedure TDAVUpSampling.OrderChanged;
begin
 FFilter.Order := FOrder;
 FFilter.ResetStates;
 inherited;
end;

procedure TDAVUpSampling.SampleRateChanged;
begin
 inherited;
 UpdateFilter;
end;

procedure TDAVUpSampling.UpdateFilter;
begin
 Assert(Assigned(FFilter));
 FFilter.Frequency := TransitionBandwidth * 0.5 * SampleRate;
 FFilter.SampleRate := FFactor * SampleRate;
 FFilter.ResetStates;
end;

procedure TDAVUpSampling.Upsample32(Input: Single; Output: PDAVSingleFixedArray);
var
  SubSample : Integer;
begin
 Output[0] := FFilter.ProcessSample64(Factor * Input + cDenorm32);
 for SubSample := 1 to Factor - 1
  do Output[SubSample] := FFilter.ProcessSample64(-cDenorm32);
end;

procedure TDAVUpSampling.Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
var
  SubSample : Integer;
begin
 Output[0] := FFilter.ProcessSample64(Factor * Input + cDenorm64);
 for SubSample := 1 to Factor - 1
  do Output[SubSample] := FFilter.ProcessSample64(-cDenorm64);
end;

{ TDAVDownSampling }

procedure TDAVDownSampling.ResetStates;
begin
 FFilter.ResetStates;
end;

constructor TDAVDownSampling.Create;
begin
 FilterClass := TButterworthLowPassFilter;
 inherited;
end;

destructor TDAVDownSampling.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TDAVDownSampling.AssignTo(Dest: TPersistent);
begin
 if Dest is TDAVDownSampling then
  with TDAVDownSampling(Dest) do
   begin
    inherited;
    FFilter.Assign(Self.FFilter);
   end
 else inherited;
end;

procedure TDAVDownSampling.FilterClassChanged;
var
  OldFilter : TCustomOrderFilter;
begin
 OldFilter := FFilter;
 FFilter := FFilterClass.Create;
 if Assigned(oldFilter)
  then FFilter.Assign(oldFilter);
 if FFilter is TCustomChebyshev1Filter then
  with TCustomChebyshev1Filter(FFilter) do
   begin
    Ripple := 0.1;
   end;
 FreeAndNil(OldFilter);
 Changed;
end;

function TDAVDownSampling.Downsample32(Input: PDAVSingleFixedArray): Single;
var
  SubSample : Integer;
begin
 Result := FFilter.ProcessSample64(Input[0] + cDenorm32);
 for SubSample := 1 to Factor - 1
  do FFilter.ProcessSample64(Input[SubSample]);
end;

function TDAVDownSampling.Downsample64(Input: PDAVDoubleFixedArray): Double;
var
  SubSample : Integer;
begin
 Result := FFilter.ProcessSample64(Input[0] + cDenorm64);
 for SubSample := 1 to Factor - 1
  do FFilter.ProcessSample64(Input[SubSample]);
end;

procedure TDAVDownSampling.OrderChanged;
begin
 FFilter.Order := FOrder;
 FFilter.ResetStates;
 inherited;
end;

procedure TDAVDownSampling.SampleRateChanged;
begin
 inherited;
 UpdateFilter;
end;

procedure TDAVDownSampling.UpdateFilter;
begin
 Assert(Assigned(FFilter));
 FFilter.Frequency := 0.5 * TransitionBandwidth * SampleRate;
 FFilter.SampleRate := SampleRate * FFactor;
 FFilter.ResetStates;
end;

end.
