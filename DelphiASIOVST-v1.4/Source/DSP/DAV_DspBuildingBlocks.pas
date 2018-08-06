unit DAV_DspBuildingBlocks;

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
  DAV_Classes, DAV_Types;

type
  TCustomBuildingBlocks = class(TDspPersistent)
  private
    procedure SetBlockSize(const Value: Integer);
    procedure SetOverlapSize(Value: Integer);
  protected
    FBlockSize     : Integer;
    FOverlapSize   : Integer;
    FBlockPosition : Integer;
    procedure AllocateBuffer; virtual; abstract;
    procedure BlockSizeChanged; virtual;
    procedure OverlapSizeChanged; virtual;
    procedure BlockComplete; virtual; abstract;
  public
    constructor Create; virtual;

    procedure Reset; virtual;

    property BlockSize: Integer read FBlockSize write SetBlockSize;
    property OverlapSize: Integer read FOverlapSize write SetOverlapSize;
  end;

  TProcessBlock32 = procedure(Sender: TObject; const Input: PDAVSingleFixedArray) of object;
  TProcessBlock64 = procedure(Sender: TObject; const Input: PDAVDoubleFixedArray) of object;

  TCustomBuildingBlocks32 = class(TCustomBuildingBlocks)
  protected
    FBuffer32  : PDAVSingleFixedArray;
    FOnProcess : TProcessBlock32;
    procedure AllocateBuffer; override;
    procedure ClearBuffer; virtual;
    procedure BlockComplete; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    property OnProcess: TProcessBlock32 read FOnProcess write FOnProcess;
  end;

  TCustomBuildingBlocks64 = class(TCustomBuildingBlocks)
  protected
    FBuffer64  : PDAVDoubleFixedArray;
    FOnProcess : TProcessBlock64;
    procedure AllocateBuffer; override;
    procedure ClearBuffer; virtual;
    procedure BlockComplete; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    property OnProcess: TProcessBlock64 read FOnProcess write FOnProcess;
  end;

  TBuildingBlocks32 = class(TCustomBuildingBlocks32, IDspSink32)
  public
    procedure ProcessSample32(Input: Single); overload;
    procedure ProcessBlock32(const Input: PDAVSingleFixedArray; SampleFrames: Integer); overload;
  published
    property BlockSize;
    property OverlapSize;

    property OnProcess;
  end;

  TBuildingBlocksCircular32 = class(TCustomBuildingBlocks32, IDspSink32)
  private
    procedure CalculateSampleAdvance;
  protected
    FBlock32        : PDAVSingleFixedArray;
    FSamplesInBlock : Integer;
    FSampleAdvance  : Integer;
    procedure AllocateBuffer; override;
    procedure OverlapSizeChanged; override;
    procedure BlockSizeChanged; override;
    procedure BlockComplete; override;
    procedure ClearBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessSample32(Input: Single); overload;
    procedure ProcessBlock32(const Input: PDAVSingleFixedArray; SampleFrames: Integer); overload;
  published
    property BlockSize;
    property OverlapSize;

    property OnProcess;
  end;

  TBuildingBlocks64 = class(TCustomBuildingBlocks64, IDspSink64)
  public
    procedure ProcessSample64(Input: Double); overload;
    procedure ProcessBlock64(const Input: PDAVDoubleFixedArray; SampleFrames: Integer); overload;
  published
    property BlockSize;
    property OverlapSize;

    property OnProcess;
  end;

  TBuildingBlocksCircular64 = class(TCustomBuildingBlocks64, IDspSink64)
  private
    procedure CalculateSampleAdvance;
  protected
    FBlock64        : PDAVDoubleFixedArray;
    FSamplesInBlock : Integer;
    FSampleAdvance  : Integer;
    procedure AllocateBuffer; override;
    procedure ClearBuffer; override;
    procedure OverlapSizeChanged; override;
    procedure BlockComplete; override;
    procedure BlockSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessSample64(Input: Double); overload;
    procedure ProcessBlock64(const Input: PDAVDoubleFixedArray; SampleFrames: Integer); overload;
  published
    property BlockSize;
    property OverlapSize;

    property OnProcess;
  end;

implementation

{ TCustomBuildingBlocks }

constructor TCustomBuildingBlocks.Create;
begin
 inherited;
 FBlockSize := 1 shl 8;
 FOverlapSize := FBlockSize shr 1;
end;

procedure TCustomBuildingBlocks.SetBlockSize(const Value: Integer);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TCustomBuildingBlocks.SetOverlapSize(Value: Integer);
begin
 if Value >= FBlockSize
  then Value := FBlockSize - 1;
 
 if FOverlapSize <> Value then
  begin
   FOverlapSize := Value;
   OverlapSizeChanged;
  end;
end;

procedure TCustomBuildingBlocks.BlockSizeChanged;
begin
 if FOverlapSize >= FBlockSize
  then FOverlapSize := FBlockSize div 2;

 if FBlockPosition >= FBlockSize
  then FBlockPosition := 0;
 
 AllocateBuffer;
 Changed;
end;

procedure TCustomBuildingBlocks.OverlapSizeChanged;
begin
 Changed;
end;

procedure TCustomBuildingBlocks.Reset;
begin
 FBlockPosition := 0;
end;


{ TCustomBuildingBlocks32 }

constructor TCustomBuildingBlocks32.Create;
begin
 FBuffer32 := nil;
 inherited;
 AllocateBuffer;
 Reset;
end;

destructor TCustomBuildingBlocks32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TCustomBuildingBlocks32.Reset;
begin
 inherited;
 FBlockPosition := FOverlapSize;
 ClearBuffer;
end;

procedure TCustomBuildingBlocks32.AllocateBuffer;
begin
 ReallocMem(FBuffer32, FBlockSize * SizeOf(Single));
end;

procedure TCustomBuildingBlocks32.BlockComplete;
begin
 if Assigned(FOnProcess)
  then FOnProcess(Self, FBuffer32);
end;

procedure TCustomBuildingBlocks32.ClearBuffer;
begin
 FillChar(FBuffer32^, FBlockSize * SizeOf(Single), 0);
end;


{ TCustomBuildingBlocks64 }

constructor TCustomBuildingBlocks64.Create;
begin
 FBuffer64 := nil;
 inherited;
 AllocateBuffer;
 Reset;
end;

destructor TCustomBuildingBlocks64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TCustomBuildingBlocks64.Reset;
begin
 inherited;
 FBlockPosition := FOverlapSize;
 ClearBuffer;
end;

procedure TCustomBuildingBlocks64.AllocateBuffer;
begin
 ReallocMem(FBuffer64, FBlockSize * SizeOf(Double));
end;

procedure TCustomBuildingBlocks64.BlockComplete;
begin
 if Assigned(FOnProcess)
  then FOnProcess(Self, FBuffer64);
end;

procedure TCustomBuildingBlocks64.ClearBuffer;
begin
 FillChar(FBuffer64^, FBlockSize * SizeOf(Double), 0);
end;


{ TBuildingBlocks32 }

procedure TBuildingBlocks32.ProcessBlock32(const Input: PDAVSingleFixedArray; SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockSize then
   begin
    Move(Input^[CurrentPosition], FBuffer32^[FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Single));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    Move(Input^[CurrentPosition], FBuffer32^[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Single));

    BlockComplete;

    Move(FBuffer32^[(FBlockSize - FOverlapSize)], FBuffer32^[0], FOverlapSize * SizeOf(Single));

    CurrentPosition := CurrentPosition + (FBlockSize - FBlockPosition);
    FBlockPosition := FOverlapSize;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TBuildingBlocks32.ProcessSample32(Input: Single);
begin
 FBuffer32[FBlockPosition] := Input;
 Inc(FBlockPosition);

 if FBlockPosition >= FBlockSize then
  begin
   BlockComplete;

   Move(FBuffer32[(FBlockSize - FOverlapSize)], FBuffer32[0], FOverlapSize * SizeOf(Single));
   FBlockPosition := FOverlapSize;
  end;
end;


{ TBuildingBlocksCircular32 }

constructor TBuildingBlocksCircular32.Create;
begin
 FBlock32 := nil;
 inherited;
 CalculateSampleAdvance;
end;

destructor TBuildingBlocksCircular32.Destroy;
begin
 Dispose(FBlock32);
 inherited;
end;

procedure TBuildingBlocksCircular32.OverlapSizeChanged;
begin
 inherited;
 CalculateSampleAdvance;
end;

procedure TBuildingBlocksCircular32.BlockSizeChanged;
begin
 inherited;
 CalculateSampleAdvance;
end;

procedure TBuildingBlocksCircular32.CalculateSampleAdvance;
begin
 FSampleAdvance := FBlockSize - FOverlapSize;
end;

procedure TBuildingBlocksCircular32.AllocateBuffer;
begin
 inherited;
 ReallocMem(FBlock32, FBlockSize * SizeOf(Single));
end;

procedure TBuildingBlocksCircular32.ClearBuffer;
begin
 inherited;
 FillChar(FBlock32^, FBlockSize * SizeOf(Single), 0);
end;

procedure TBuildingBlocksCircular32.BlockComplete;
begin
 Move(FBuffer32^[FBlockPosition], FBlock32^[0], (FBlockSize - FBlockPosition) * SizeOf(Single));
 Move(FBuffer32^[0], FBlock32^[(FBlockSize - FBlockPosition)], FBlockPosition * SizeOf(Single));
 inherited;
 FSamplesInBlock := 0;
end;

procedure TBuildingBlocksCircular32.ProcessBlock32(
  const Input: PDAVSingleFixedArray; SampleFrames: Integer);
var
  SamplesWritten : Integer;
begin
 SamplesWritten := 0;
 repeat
  if FSamplesInBlock + SampleFrames < FSampleAdvance then
   begin
    // all samples can be written!
    Inc(FSamplesInBlock, SampleFrames);

    // check whether a wrap around occurs
    if FBlockPosition + SampleFrames < FBlockSize then
     begin
      Move(Input[SamplesWritten], FBuffer32[FBlockPosition], SampleFrames * SizeOf(Single));

      // advance block position
      FBlockPosition := FBlockPosition + SampleFrames;
     end
    else
     begin
      Move(Input[SamplesWritten], FBuffer32[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Single));

      // advance
      Dec(SampleFrames, FBlockSize - FBlockPosition);
      Inc(SamplesWritten, FBlockSize - FBlockPosition);
      FBlockPosition := SampleFrames;

      // move missing samples
      Move(Input[SamplesWritten], FBuffer32[0], SampleFrames * SizeOf(Single));
     end;

    // all samples written -> exit immediatly!
    Exit;
   end
  else
   begin
    if FBlockPosition + FSampleAdvance - FSamplesInBlock < FBlockSize then
     begin
      Move(Input[SamplesWritten], FBuffer32[FBlockPosition], (FSampleAdvance - FSamplesInBlock) * SizeOf(Single));

      // advance
      Dec(SampleFrames, FSampleAdvance - FSamplesInBlock);
      Inc(SamplesWritten, FSampleAdvance - FSamplesInBlock);
      Inc(FBlockPosition, FSampleAdvance - FSamplesInBlock);

      BlockComplete;
     end
    else
     begin
      Move(Input[SamplesWritten], FBuffer32[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Single));

      // advance
      Dec(SampleFrames, FBlockSize - FBlockPosition);
      Inc(SamplesWritten, FBlockSize - FBlockPosition);
      Inc(FSamplesInBlock, FBlockSize - FBlockPosition);

      // move missing samples
      Move(Input[SamplesWritten], FBuffer32[0], (FSampleAdvance - FSamplesInBlock) * SizeOf(Single));
      FBlockPosition := (FSampleAdvance - FSamplesInBlock);

      BlockComplete;
     end;
   end;
  until SampleFrames = 0;
end;

procedure TBuildingBlocksCircular32.ProcessSample32(Input: Single);
begin
 FBuffer32[FBlockPosition] := Input;
 Inc(FBlockPosition);
 if FBlockPosition >= FBlockSize
  then FBlockPosition := 0;

 Inc(FSamplesInBlock);

 if FSamplesInBlock = FSampleAdvance
  then BlockComplete;
end;


{ TBuildingBlocks64 }

procedure TBuildingBlocks64.ProcessBlock64(const Input: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;
 repeat
  if FBlockPosition + (SampleFrames - CurrentPosition) < FBlockSize then
   begin
    Move(Input^[CurrentPosition], FBuffer64^[FBlockPosition], (SampleFrames - CurrentPosition) * SizeOf(Double));

    FBlockPosition := FBlockPosition + (SampleFrames - CurrentPosition);
    CurrentPosition := SampleFrames;
   end
  else
   begin
    Move(Input^[CurrentPosition], FBuffer64^[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Double));

    if Assigned(FOnProcess)
     then FOnProcess(Self, FBuffer64);

    Move(FBuffer64^[(FBlockSize - FOverlapSize)], FBuffer64^[0], FOverlapSize * SizeOf(Double));

    CurrentPosition := CurrentPosition + (FBlockSize - FBlockPosition);
    FBlockPosition := FOverlapSize;
   end;
  until CurrentPosition >= SampleFrames;
end;

procedure TBuildingBlocks64.ProcessSample64(Input: Double);
begin
 FBuffer64[FBlockPosition] := Input;
 Inc(FBlockPosition);

 if FBlockPosition >= FBlockSize then
  begin
   if Assigned(FOnProcess)
    then FOnProcess(Self, FBuffer64);

   Move(FBuffer64[(FBlockSize - FOverlapSize)], FBuffer64[0], FOverlapSize * SizeOf(Double));
   FBlockPosition := FOverlapSize;
  end;
end;


{ TBuildingBlocksCircular64 }

constructor TBuildingBlocksCircular64.Create;
begin
 FBlock64 := nil;
 inherited;
 CalculateSampleAdvance;
 ClearBuffer;
end;

destructor TBuildingBlocksCircular64.Destroy;
begin
 Dispose(FBlock64);
 inherited;
end;

procedure TBuildingBlocksCircular64.OverlapSizeChanged;
begin
 inherited;
 CalculateSampleAdvance;
end;

procedure TBuildingBlocksCircular64.BlockSizeChanged;
begin
 inherited;
 CalculateSampleAdvance;
end;

procedure TBuildingBlocksCircular64.CalculateSampleAdvance;
begin
 FSampleAdvance := FBlockSize - FOverlapSize;
end;

procedure TBuildingBlocksCircular64.AllocateBuffer;
begin
 inherited;
 ReallocMem(FBlock64, FBlockSize * SizeOf(Double));
end;

procedure TBuildingBlocksCircular64.ClearBuffer;
begin
 inherited;
 FillChar(FBlock64^, FBlockSize * SizeOf(Double), 0);
end;

procedure TBuildingBlocksCircular64.BlockComplete;
begin
 Move(FBuffer64^[FBlockPosition], FBlock64^[0], (FBlockSize - FBlockPosition) * SizeOf(Double));
 Move(FBuffer64^[0], FBlock64^[(FBlockSize - FBlockPosition)], FBlockPosition * SizeOf(Double));

 inherited;

 FSamplesInBlock := 0;
end;

procedure TBuildingBlocksCircular64.ProcessBlock64(
  const Input: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  SamplesWritten : Integer;
begin
 SamplesWritten := 0;
 repeat
  if FSamplesInBlock + SampleFrames < FSampleAdvance then
   begin
    // all samples can be written!
    Inc(FSamplesInBlock, SampleFrames);

    // check whether a wrap around occurs
    if FBlockPosition + SampleFrames < FBlockSize then
     begin
      Move(Input[SamplesWritten], FBuffer64[FBlockPosition], SampleFrames * SizeOf(Double));

      // advance block position
      FBlockPosition := FBlockPosition + SampleFrames;
     end
    else
     begin
      Move(Input[SamplesWritten], FBuffer64[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Double));

      // advance
      Dec(SampleFrames, FBlockSize - FBlockPosition);
      Inc(SamplesWritten, FBlockSize - FBlockPosition);
      FBlockPosition := SampleFrames;

      // move missing samples
      Move(Input[SamplesWritten], FBuffer64[0], SampleFrames * SizeOf(Double));
     end;

    // all samples written -> exit immediatly!
    Exit;
   end
  else
   begin
    if FBlockPosition + FSampleAdvance - FSamplesInBlock < FBlockSize then
     begin
      Move(Input[SamplesWritten], FBuffer64[FBlockPosition], (FSampleAdvance - FSamplesInBlock) * SizeOf(Double));

      // advance
      Dec(SampleFrames, FSampleAdvance - FSamplesInBlock);
      Inc(SamplesWritten, FSampleAdvance - FSamplesInBlock);
      Inc(FBlockPosition, FSampleAdvance - FSamplesInBlock);

      BlockComplete;
     end
    else
     begin
      Move(Input[SamplesWritten], FBuffer64[FBlockPosition], (FBlockSize - FBlockPosition) * SizeOf(Double));

      // advance
      Dec(SampleFrames, FBlockSize - FBlockPosition);
      Inc(SamplesWritten, FBlockSize - FBlockPosition);
      Inc(FSamplesInBlock, FBlockSize - FBlockPosition);

      // move missing samples
      Move(Input[SamplesWritten], FBuffer64[0], (FSampleAdvance - FSamplesInBlock) * SizeOf(Double));
      FBlockPosition := (FSampleAdvance - FSamplesInBlock);

      BlockComplete;
     end;
   end;
  until SampleFrames = 0;
end;

procedure TBuildingBlocksCircular64.ProcessSample64(Input: Double);
begin
 FBuffer64[FBlockPosition] := Input;
 Inc(FBlockPosition);
 if FBlockPosition >= FBlockSize
  then FBlockPosition := 0;

 Inc(FSamplesInBlock);

 if FSamplesInBlock = FSampleAdvance
  then BlockComplete;
end;

end.
