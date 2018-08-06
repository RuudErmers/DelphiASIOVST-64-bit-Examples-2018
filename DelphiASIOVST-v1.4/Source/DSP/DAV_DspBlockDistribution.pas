unit DAV_DspBlockDistribution;

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
  DAV_Classes, DAV_Types, DAV_DspBuildingBlocks, DAV_DspFilterButterworth;

type
  TCustomBlockDistribution32 = class(TCustomBuildingBlocks, IDspProcessor32)
  private
    procedure CalculateSampleAdvance;
  protected
    FBuffer32       : PDAVSingleFixedArray;
    FBlock32        : PDAVSingleFixedArray;
    FAddBuffer32    : PDAVSingleFixedArray;
    FSamplesInBlock : Integer;
    FSampleAdvance  : Integer;
    procedure AllocateBuffer; override;
    procedure ClearBuffer; virtual;
    procedure OverlapSizeChanged; override;
    procedure BlockSizeChanged; override;
    procedure CopyBlock; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    function ProcessSample32(Input: Single): Single;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleFrames: Integer); overload;
    procedure ProcessBlock32(const Input, Output: PDAVSingleFixedArray; SampleFrames: Integer); overload;
  end;

  TCustomBlockDisorder32 = class(TCustomBlockDistribution32, IDspProcessor32)
  private
    FFilterOrder: Integer;
    procedure SetFilterOrder(const Value: Integer);
  protected
    FFactor      : Single;
    FPositions   : PIntegerArray;
    FFilter      : array [0..1] of TButterworthLowPassFilter;
    FFilterIndex : Integer;
    procedure FilterOrderChanged;

    procedure AllocateBuffer; override;
    procedure BlockSizeChanged; override;
    procedure UpdateFilter; virtual;
    procedure CopyBlock; override;
    procedure BlockComplete; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property FilterOrder: Integer read FFilterOrder write SetFilterOrder;
  end;

  TBlockDistribution32 = class(TCustomBlockDistribution32)
  published
    property BlockSize;
  end;

  TBlockDisorder32 = class(TCustomBlockDisorder32)
  published
    property BlockSize;
    property FilterOrder;
  end;



implementation

uses
  SysUtils, DAV_BlockProcessing;

{ TCustomBlockDistribution32 }

constructor TCustomBlockDistribution32.Create;
begin
 FBuffer32 := nil;
 FBlock32 := nil;

 inherited;

 FOverlapSize := FBlockSize shr 1;
 CalculateSampleAdvance;
 AllocateBuffer;
 Reset;
end;

destructor TCustomBlockDistribution32.Destroy;
begin
 Dispose(FAddBuffer32);
 Dispose(FBlock32);
 Dispose(FBuffer32);
 inherited;
end;

procedure TCustomBlockDistribution32.AllocateBuffer;
begin
 inherited;
 ReallocMem(FBuffer32, FBlockSize * SizeOf(Single));
 ReallocMem(FBlock32, FBlockSize * SizeOf(Single));
 ReallocMem(FAddBuffer32, FBlockSize * SizeOf(Single));
end;

procedure TCustomBlockDistribution32.BlockSizeChanged;
begin
 inherited;
 OverlapSize := BlockSize div 2;
 CalculateSampleAdvance;
end;

procedure TCustomBlockDistribution32.CalculateSampleAdvance;
begin
 FSampleAdvance := FBlockSize - FOverlapSize;
end;

procedure TCustomBlockDistribution32.ClearBuffer;
begin
 FillChar(FBuffer32^, FBlockSize * SizeOf(Single), 0);
 FillChar(FBlock32^, FBlockSize * SizeOf(Single), 0);
 FillChar(FAddBuffer32^, FBlockSize * SizeOf(Single), 0);
end;

procedure TCustomBlockDistribution32.OverlapSizeChanged;
begin
 inherited;
 CalculateSampleAdvance;
end;

procedure TCustomBlockDistribution32.Reset;
begin
 inherited;
 FBlockPosition := FOverlapSize;
 ClearBuffer;
end;

procedure TCustomBlockDistribution32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleFrames: Integer);
begin
 ProcessBlock32(Data, Data, SampleFrames);
end;

procedure TCustomBlockDistribution32.ProcessBlock32(const Input,
  Output: PDAVSingleFixedArray; SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockSize then
   begin
    Move(Input^[CurrentPosition], FBuffer32^[FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));
    Move(FBlock32^[FBlockPosition], Output^[CurrentPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    Move(Input^[CurrentPosition], FBuffer32^[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Single));
    Move(FBlock32^[FBlockPosition], Output^[CurrentPosition], (FBlockSize - FBlockPosition) * SizeOf(Single));

    CopyBlock;

    Move(FBuffer32[(FBlockSize - FOverlapSize)], FBuffer32[0], FOverlapSize * SizeOf(Single));

    CurrentPosition := CurrentPosition + (FBlockSize - FBlockPosition);
    FBlockPosition := FOverlapSize;
   end;
  until CurrentPosition >= SampleFrames;
end;

function TCustomBlockDistribution32.ProcessSample32(Input: Single): Single;
begin
 Result := FBlock32[FBlockPosition];
 FBuffer32[FBlockPosition] := Input;
 Inc(FBlockPosition);

 if FBlockPosition >= FBlockSize then
  begin
   CopyBlock;

   Move(FBuffer32[(FBlockSize - FOverlapSize)], FBuffer32[0], FOverlapSize * SizeOf(Single));
   FBlockPosition := FOverlapSize;
  end;
end;

{ TCustomBlockDisorder32 }

constructor TCustomBlockDisorder32.Create;
begin
 FPositions := nil;

 inherited;

 FFactor := 1;
 FFilterIndex := 0;

 FFilter[0] := TButterworthLowPassFilter.Create;
 with FFilter[0] do
  begin
   Order := 1;
  end;
 FFilter[1] := TButterworthLowPassFilter.Create;
 with FFilter[1] do
  begin
   Order := 1;
  end;
 UpdateFilter;
end;

destructor TCustomBlockDisorder32.Destroy;
begin
 FreeAndNil(FFilter[0]);
 FreeAndNil(FFilter[1]);
 Dispose(FPositions);
 inherited;
end;

procedure TCustomBlockDisorder32.AllocateBuffer;
var
  Sample : Integer;
begin
 inherited;
 ReallocMem(FPositions, FBlockSize * SizeOf(Single));

 for Sample := 0 to FBlockSize - 1
  do FPositions[Sample] := Sample;
end;

procedure TCustomBlockDisorder32.BlockComplete;
begin
  inherited;
  // do nothing yet
end;

procedure TCustomBlockDisorder32.BlockSizeChanged;
begin
 inherited;
 UpdateFilter;
end;

procedure TCustomBlockDisorder32.CopyBlock;
var
  Sample : Integer;
  Scale  : Single;
begin
 Move(FBuffer32^[0], FBlock32^[0], FBlockSize * SizeOf(Single));

 QuickSortWithPosition(FBlock32, 0, FBlockSize - 1, FPositions);

 for Sample := 0 to FBlockSize - 1
  do FBlock32^[Sample] := FFilter[FFilterIndex].ProcessSample64(FFactor * FBlock32^[Sample]);

 FFactor := -FFactor;
 FFilterIndex := 1 - FFilterIndex;

 ReorderPositions(FBlock32, 0, FBlockSize - 1, FPositions);

 Scale := 1 / (FBlockSize div 2);
 for Sample := 0 to (FBlockSize div 2) - 1 do
  begin
   FAddBuffer32^[Sample] := FFactor * ((Sample * Scale) * FBlock32^[Sample] +
     (1 - (Sample * Scale)) * FAddBuffer32^[(FBlockSize div 2) + Sample]);
   FAddBuffer32^[(FBlockSize div 2) + Sample] := -FBlock32^[(FBlockSize div 2) + Sample];
  end;

 Move(FAddBuffer32^[0], FBlock32^[FBlockSize div 2], (FBlockSize div 2) * SizeOf(Single));
end;

procedure TCustomBlockDisorder32.SetFilterOrder(const Value: Integer);
begin
 if FFilterOrder <> Value then
  begin
   FFilterOrder := Value;
   FilterOrderChanged;
  end;
end;

procedure TCustomBlockDisorder32.FilterOrderChanged;
begin
 FFilter[0].Order := FFilterOrder;
 FFilter[1].Order := FFilterOrder;
end;

procedure TCustomBlockDisorder32.UpdateFilter;
begin
 FFilter[0].Frequency := 0.5 * FFilter[0].SampleRate / FBlockSize;
 FFilter[1].Frequency := FFilter[0].Frequency;
end;

end.
