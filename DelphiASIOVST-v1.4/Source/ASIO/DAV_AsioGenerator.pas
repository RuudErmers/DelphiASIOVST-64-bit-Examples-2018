unit DAV_AsioGenerator;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types;

type
  TNoiseColor = (ncWhite, ncPink, ncBrown);
  TWaveform = (wfSine, wfSaw, wfSquare, wfTriangle, wfPulse);
  TASIOGenerator = class(TComponent)
  private
    FSampleRate  : Double;
    FBlockSize   : Cardinal;
    procedure SetSampleRate(const Value: Double);
    procedure SetBlockSize(const Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessBuffer32(Buffer: TDAVArrayOfSingleFixedArray; IsOutput: Boolean = True); virtual;
    procedure ProcessBuffer64(Buffer: TDAVArrayOfDoubleFixedArray; IsOutput: Boolean = True); virtual;
  published
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property BlockSize: Cardinal read FBlockSize write SetBlockSize;
  end;

  TASIOGeneratorNoise = class(TASIOGenerator)
  private
    FNoiZe      : Array[0..7] of Single;
    FNoiseColor : TNoiseColor;
  public
    procedure ProcessBuffer32(Buffer: TDAVArrayOfSingleFixedArray; isOutput: Boolean = True); override;
    procedure ProcessBuffer64(Buffer: TDAVArrayOfDoubleFixedArray; IsOutput: Boolean = True); override;
  published
    property NoiseColor: TNoiseColor read FNoiseColor write FNoiseColor;
  end;

  TASIOGeneratorTone = class(TASIOGenerator)
  private
    FFrequency  : Single;
    FToneFlavor : TWaveform;
    FPhase      : Double;
    procedure SetFrequency(value: Single);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ProcessBuffer32(Buffer: TDAVArrayOfSingleFixedArray; isOutput: Boolean = True); override;
    procedure ProcessBuffer64(Buffer: TDAVArrayOfDoubleFixedArray; IsOutput: Boolean = True); override;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property Waveform: TWaveform read FToneFlavor write FToneFlavor;
  end;

implementation

uses
  Math, DAV_ASIOHost;

{ TASIOGenerator }

constructor TASIOGenerator.Create(AOwner: TComponent);
begin
 FBlockSize := 2048;
 FSampleRate := 44100;
 inherited;
end;

destructor TASIOGenerator.Destroy;
begin
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TASIOGenerator.ProcessBuffer32(Buffer: TDAVArrayOfSingleFixedArray; IsOutput: Boolean = True);
begin
 FillChar(Buffer[0], BlockSize * SizeOf(Single), 0);
end;

procedure TASIOGenerator.ProcessBuffer64(Buffer: TDAVArrayOfDoubleFixedArray; IsOutput: Boolean = True);
begin
 FillChar(Buffer[0], BlockSize * SizeOf(Double), 0);
end;

procedure TASIOGenerator.SetSampleRate(const Value: Double);
begin
 FSampleRate := Value;
end;

procedure TASIOGenerator.SetBlockSize(const Value: Cardinal);
begin
 FBlockSize := Value;
end;

{ TASIOGeneratorNoise }

procedure TASIOGeneratorNoise.ProcessBuffer32(Buffer: TDAVArrayOfSingleFixedArray;
  IsOutput: Boolean = True);
var
  Channel, Sample : integer;
begin
 if NoiseColor = ncWhite then
  for Channel := 0 to Length(Buffer) - 1 do
   for Sample := 0 to Blocksize - 1
    do Buffer[Channel, Sample] := (2 * Random - 1)
 else if NoiseColor = ncPink then
  for Channel := 0 to Length(Buffer) - 1 do
   for Sample := 0 to Blocksize - 1 do
    begin
     FNoiZe[0]:=(2 * Random - 1);
     FNoiZe[1]:= 0.99886 * FNoiZe[1] + FNoiZe[0] * 0.0555179;
     FNoiZe[2]:= 0.99332 * FNoiZe[2] + FNoiZe[0] * 0.0750759;
     FNoiZe[3]:= 0.96900 * FNoiZe[3] + FNoiZe[0] * 0.1538520;
     FNoiZe[4]:= 0.86650 * FNoiZe[4] + FNoiZe[0] * 0.3104856;
     FNoiZe[5]:= 0.55000 * FNoiZe[5] + FNoiZe[0] * 0.5329522;
     FNoiZe[6]:= -0.7616 * FNoiZe[6] - FNoiZe[0] * 0.0168980;
     Buffer[Channel,Sample] := 0.1 * (FNoiZe[0] + FNoiZe[1] + FNoiZe[2] + FNoiZe[3] + FNoiZe[4] + FNoiZe[5] + FNoiZe[6] + FNoiZe[7] * 0.5362);   FNoiZe[7]:= FNoiZe[0] * 0.115926;
    end
 else if NoiseColor = ncBrown then
  for Channel := 0 to Length(Buffer) - 1 do
   for Sample := 0 to Blocksize - 1 do
    begin
     Buffer[Channel, Sample] := FNoiZe[Channel] + 0.0002 * (2 * Random - 1);
     FNoiZe[Channel] := 0.999 * Buffer[Channel, Sample];
     Buffer[Channel, Sample] := 150 * Buffer[Channel, Sample];
   end;
end;

procedure TASIOGeneratorNoise.ProcessBuffer64(Buffer: TDAVArrayOfDoubleFixedArray;
  IsOutput: Boolean);
var
  Channel, Sample : integer;
begin
 if NoiseColor = ncWhite then
  for Channel := 0 to Length(Buffer) - 1 do
   for Sample := 0 to Blocksize - 1
    do Buffer[Channel, Sample] := (2 * Random - 1)
 else if NoiseColor = ncPink then
  for Channel := 0 to Length(Buffer) - 1 do
   for Sample := 0 to Blocksize - 1 do
    begin
     FNoiZe[0]:=(2 * Random - 1);
     FNoiZe[1]:= 0.99886 * FNoiZe[1] + FNoiZe[0] * 0.0555179;
     FNoiZe[2]:= 0.99332 * FNoiZe[2] + FNoiZe[0] * 0.0750759;
     FNoiZe[3]:= 0.96900 * FNoiZe[3] + FNoiZe[0] * 0.1538520;
     FNoiZe[4]:= 0.86650 * FNoiZe[4] + FNoiZe[0] * 0.3104856;
     FNoiZe[5]:= 0.55000 * FNoiZe[5] + FNoiZe[0] * 0.5329522;
     FNoiZe[6]:= -0.7616 * FNoiZe[6] - FNoiZe[0] * 0.0168980;
     Buffer[Channel,Sample] := 0.1 * (FNoiZe[0] + FNoiZe[1] + FNoiZe[2] + FNoiZe[3] + FNoiZe[4] + FNoiZe[5] + FNoiZe[6] + FNoiZe[7] * 0.5362);   FNoiZe[7]:= FNoiZe[0] * 0.115926;
    end
 else if NoiseColor = ncBrown then
  for Channel := 0 to Length(Buffer) - 1 do
   for Sample := 0 to Blocksize - 1 do
    begin
     Buffer[Channel, Sample] := FNoiZe[Channel] + 0.0002 * (2 * Random - 1);
     FNoiZe[Channel] := 0.999 * Buffer[Channel, Sample];
     Buffer[Channel, Sample] := 150 * Buffer[Channel, Sample];
   end;
end;

{ TASIOGeneratorTone }

procedure TASIOGeneratorTone.SetFrequency(value:Single);
begin
 FFrequency := value;
end;

constructor TASIOGeneratorTone.Create(AOwner: TComponent);
begin
 inherited;
 FFrequency := 1000;
end;

procedure TASIOGeneratorTone.ProcessBuffer32(Buffer: TDAVArrayOfSingleFixedArray;
  IsOutput: Boolean = True);
var
  Channel, Sample : Integer;
  CurrentPhase    : Double;
begin
 CurrentPhase := Frequency / SampleRate;
 Channel := 0;
 for Sample := 0 to Blocksize - 1 do
 begin
  case Waveform of
  wfSine: Buffer[Channel, Sample] := sin(FPhase * 2 * PI);
  wfSaw: if FPhase <= 0.5 then Buffer[Channel, Sample] := 2 * FPhase
         else Buffer[Channel, Sample] := 2 * FPhase - 2;
  wfSquare: if FPhase <= 0.5 then Buffer[Channel, Sample] := 1 else
   Buffer[Channel, Sample] := -1;
  wfTriangle: if (FPhase >= 0.25) and (FPhase < 0.75) then
   Buffer[Channel, Sample] := -4 * (FPhase + 0.25) + 3
   else if (FPhase < 0.25) then Buffer[Channel, Sample] := 4 * (FPhase + 0.25) - 1
   else Buffer[Channel, Sample] := 4 * (FPhase + 0.25) - 5;
  wfPulse: if Sample = 0 then Buffer[Channel, Sample] := 0 else
   Buffer[Channel, Sample] := 1;
  else
  end;
  FPhase := FPhase + CurrentPhase;
  if FPhase >= 1 then FPhase := FPhase - 1;
 end;

 for Channel := 1 to Length(Buffer) - 1
  do Move(Buffer[0, 0], Buffer[Channel, 0], Blocksize * SizeOf(Double));
end;

procedure TASIOGeneratorTone.ProcessBuffer64(Buffer: TDAVArrayOfDoubleFixedArray;
  IsOutput: Boolean);
var
  Channel, Sample : Integer;
  CurrentPhase    : Double;
begin
 CurrentPhase := Frequency / SampleRate;
 Channel := 0;
 for Sample := 0 to Blocksize - 1 do
 begin
  case Waveform of
  wfSine: Buffer[Channel, Sample] := sin(FPhase * 2 * PI);
  wfSaw: if FPhase <= 0.5 then Buffer[Channel, Sample] := 2 * FPhase
         else Buffer[Channel, Sample] := 2 * FPhase - 2;
  wfSquare: if FPhase <= 0.5 then Buffer[Channel, Sample] := 1 else
    Buffer[Channel, Sample] := -1;
  wfTriangle: if (FPhase >= 0.25) and (FPhase < 0.75) then
    Buffer[Channel, Sample] := -4 * (FPhase + 0.25) + 3
    else if (FPhase < 0.25) then Buffer[Channel, Sample] := 4 * (FPhase + 0.25) - 1
    else Buffer[Channel, Sample] := 4 * (FPhase + 0.25) - 5;
  wfPulse: if Sample = 0 then Buffer[Channel, Sample] := 0 else
    Buffer[Channel, Sample] := 1;
  else
  end;

  FPhase := FPhase + CurrentPhase;
  if FPhase >= 1 then FPhase := FPhase - 1;
 end;

 for Channel := 1 to Length(Buffer) - 1
  do Move(Buffer[0, 0], Buffer[Channel, 0], Blocksize * SizeOf(Double));
end;

initialization

end.
