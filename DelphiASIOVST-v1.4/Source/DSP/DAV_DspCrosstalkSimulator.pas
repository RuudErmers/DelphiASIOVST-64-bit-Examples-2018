unit DAV_DspCrosstalkSimulator;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterBasics;

type
  TCustomCrosstalkSimulator = class(TDspSampleRatePersistent)
  public
    procedure ProcessSample(var Left, Right: Single); overload; virtual; abstract;
    procedure ProcessSample(var Left, Right: Double); overload; virtual; abstract;
  end;

  TIIRCrosstalkSimulatorModel = (csmHandcrafted, csmIRCAM, csmHDPHX);
  TCustomIIRCrosstalkSimulator = class(TCustomCrosstalkSimulator)
  private
    FFilter     : Array [0..1, 0..2] of TBiquadIIRFilter;
    FBuffer     : Array [0..1] of PDAVSingleFixedArray;
    FBufPos     : Integer;
    FBufSize    : Integer;
    FMulFactor  : Array [0..1] of Single;

    FDiameter   : Single;
    FModel      : TIIRCrosstalkSimulatorModel;
    FMix        : Single;
    FPolarity   : Boolean;
    procedure SetDiameter(const Value: Single);
    procedure SetModel(const Value: TIIRCrosstalkSimulatorModel);
    procedure SetMix(const Value: Single);
    procedure SetPolarity(const Value: Boolean);
    procedure CalculateBufferSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateMulFactor; virtual;
    procedure DiameterChanged; virtual;
    procedure MixChanged; virtual;
    procedure ModelChanged; virtual;
    procedure PolarityChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessSample(var Left, Right: Single); overload; override;
    procedure ProcessSample(var Left, Right: Double); overload; override;

    property Model: TIIRCrosstalkSimulatorModel read FModel write SetModel;
    property Diameter: Single read FDiameter write SetDiameter;
    property Polarity: Boolean read FPolarity write SetPolarity;
    property Mix: Single read FMix write SetMix;
  end;

  TIIRCrosstalkSimulator = class(TCustomIIRCrosstalkSimulator)
  published
    property Model;
    property Diameter;
    property Polarity;
    property Mix;
  end;

  TCrosstalkSimulator = TIIRCrosstalkSimulator;

implementation

uses
  Math, SysUtils;

{ TCustomIIRCrosstalkSimulator }

constructor TCustomIIRCrosstalkSimulator.Create;
var
  Channel : Integer;
begin
 inherited;
 FDiameter   := 0.11;
 FBufPos     := 0;
 FMix        := 50;
 FPolarity   := True;
 CalculateMulFactor;
 CalculateBufferSize;

 for Channel := 0 to 1 do
  begin
   FFilter[Channel, 0] := TBasicHighShelfFilter.Create;
   FFilter[Channel, 1] := TBasicPeakFilter.Create;
   FFilter[Channel, 2] := TBasicPeakFilter.Create;
  end;

 FModel := csmHandcrafted;
 ModelChanged; 
end;

destructor TCustomIIRCrosstalkSimulator.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FreeAndNil(FFilter[Channel, 0]);
   FreeAndNil(FFilter[Channel, 1]);
   FreeAndNil(FFilter[Channel, 2]);
  end;
end;

procedure TCustomIIRCrosstalkSimulator.SetDiameter(const Value: Single);
begin
 FDiameter := Value;
 DiameterChanged;
end;

procedure TCustomIIRCrosstalkSimulator.SetMix(const Value: Single);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomIIRCrosstalkSimulator.SetModel(const Value: TIIRCrosstalkSimulatorModel);
begin
 if FModel <> Value then
  begin
   FModel := Value;
   ModelChanged;
  end;
end;

procedure TCustomIIRCrosstalkSimulator.SetPolarity(const Value: Boolean);
begin
 if FPolarity <> Value then
  begin
   FPolarity := Value;
   PolarityChanged;
  end;
end;

procedure TCustomIIRCrosstalkSimulator.DiameterChanged;
begin
 CalculateBufferSize;
 Changed;
end;

procedure TCustomIIRCrosstalkSimulator.MixChanged;
begin
 CalculateMulFactor;
 Changed;
end;

procedure TCustomIIRCrosstalkSimulator.PolarityChanged;
begin
 CalculateMulFactor;
 Changed;
end;

procedure TCustomIIRCrosstalkSimulator.CalculateMulFactor;
begin
 FMulFactor[0] := 1 - 0.002 * FMix;
 if FPolarity
  then FMulFactor[1] :=  5 * (1 - FMulFactor[0])
  else FMulFactor[1] := -5 * (1 - FMulFactor[0]);
end;

procedure TCustomIIRCrosstalkSimulator.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomIIRCrosstalkSimulator then
  with TCustomIIRCrosstalkSimulator(Dest) do
   begin
    inherited;
    FDiameter   := Self.FDiameter;
    FModel      := Self.FModel;
    FMix        := Self.FMix;
    FPolarity   := Self.FPolarity;
    FMulFactor  := Self.FMulFactor;
    CalculateBufferSize;

    FFilter[0, 0].Assign(Self.FFilter[0, 0]);
    FFilter[0, 1].Assign(Self.FFilter[0, 1]);
    FFilter[0, 2].Assign(Self.FFilter[0, 2]);
    FFilter[1, 0].Assign(Self.FFilter[1, 0]);
    FFilter[1, 1].Assign(Self.FFilter[1, 1]);
    FFilter[1, 2].Assign(Self.FFilter[1, 2]);

    assert(FBufSize = Self.FBufSize);
    Move(Self.FBuffer[0], FBuffer[0], FBufSize * SizeOf(Single));
    Move(Self.FBuffer[1], FBuffer[1], FBufSize * SizeOf(Single));
   end
  else inherited;
end;

procedure TCustomIIRCrosstalkSimulator.CalculateBufferSize;
var
  Channel : Integer;
begin
 FBufSize := max(1, round(SampleRate * (FDiameter / 344)));

 for Channel := 0 to 1 do
  begin
   ReallocMem(FBuffer[Channel], FBufSize * SizeOf(Single));
   FillChar(FBuffer[Channel]^, FBufSize * SizeOf(Single), 0);
  end;

 if FBufPos = FBufSize
  then FBufPos := 0;
end;

procedure TCustomIIRCrosstalkSimulator.SamplerateChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FFilter[Channel, 0].SampleRate := SampleRate;
   FFilter[Channel, 1].SampleRate := SampleRate;
   FFilter[Channel, 2].SampleRate := SampleRate;
  end;
 CalculateBufferSize; 
end;

procedure TCustomIIRCrosstalkSimulator.ModelChanged;
var
  Channel : Integer;
begin
 case FModel of
  csmHandcrafted :
    for Channel := 0 to 1 do
     begin
      with FFilter[Channel, 0] do
       begin
        Frequency := 700;
        Bandwidth := 3;
        Gain := -18;
       end;
      with FFilter[Channel, 1] do
       begin
        Frequency := 1000;
        Bandwidth := 1;
        Gain := -3;
       end;
      with FFilter[Channel, 2] do
       begin
        Frequency := 8000;
        Bandwidth := 3;
        Gain := 2;
       end;
     end;
  csmIRCAM :
    for Channel := 0 to 1 do
     begin
      with FFilter[Channel, 0] do
       begin
        Frequency := 1400;
        Bandwidth := 2.775;
        Gain := -14;
       end;
      with FFilter[Channel, 1] do
       begin
        Frequency := 1100;
        Bandwidth := 0.5;
        Gain := -4;
       end;
      with FFilter[Channel, 2] do
       begin
        Frequency := 12000;
        Bandwidth := 1;
        Gain := 7;
       end;
     end;
  csmHDPHX :
    for Channel := 0 to 1 do
     begin
      with FFilter[Channel, 0] do
       begin
        Frequency := 700;
        Bandwidth := 2.5;
        Gain := -20;
       end;
      with FFilter[Channel, 1] do
       begin
        Frequency := 1900;
        Bandwidth := 1.6;
        Gain := -3.1;
       end;
      with FFilter[Channel, 2] do
       begin
        Frequency := 5150;
        Bandwidth := 2.1;
        Gain := 9.1;
       end;
     end;
 end;
 Changed;
end;

procedure TCustomIIRCrosstalkSimulator.ProcessSample(var Left, Right: Single);
var
  ip : Array [0..1] of Single;
begin
 ip[0] := Left;
 ip[1] := Right;
 inc(FBufPos);
 if FBufPos >= FBufSize then FBufPos := 0;
 Left  := FMulFactor[0] * Left  + FBuffer[0, FBufPos];
 Right := FMulFactor[0] * Right + FBuffer[1, FBufPos];
 FBuffer[0, FBufPos] := FMulFactor[1] * FFilter[0, 2].ProcessSample64(FFilter[0, 1].ProcessSample64(FFilter[0, 0].ProcessSample64(ip[1])));
 FBuffer[1, FBufPos] := FMulFactor[1] * FFilter[1, 2].ProcessSample64(FFilter[1, 1].ProcessSample64(FFilter[1, 0].ProcessSample64(ip[0])));
end;

procedure TCustomIIRCrosstalkSimulator.ProcessSample(var Left, Right: Double);
var
  ip : Array [0..1] of Double;
begin
 ip[0] := Left;
 ip[1] := Right;
 inc(FBufPos);
 if FBufPos >= FBufSize then FBufPos := 0;
 Left  := FMulFactor[0] * Left  + FBuffer[0, FBufPos];
 Right := FMulFactor[0] * Right + FBuffer[1, FBufPos];
 FBuffer[0, FBufPos] := FMulFactor[1] * FFilter[0, 2].ProcessSample64(FFilter[0, 1].ProcessSample64(FFilter[0, 0].ProcessSample64(ip[1])));
 FBuffer[1, FBufPos] := FMulFactor[1] * FFilter[1, 2].ProcessSample64(FFilter[1, 1].ProcessSample64(FFilter[1, 0].ProcessSample64(ip[0])));
end;

end.
