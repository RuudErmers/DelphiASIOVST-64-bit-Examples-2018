unit TestGuiBlend;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  TestFramework, Windows, Controls, Types, Classes, SysUtils, Messages,
  Graphics, DAV_GuiCommon, DAV_GuiBlend, DAV_ProcessorInfo, DAV_Bindings;

type
  TCustomTestBlendModes = class(TTestCase)
  strict private
    FForeground : PPixel32Array;
    FBackground : PPixel32Array;
    FColorDiff  : Byte;
  protected
    function CompareColors(Expected, Actual: TPixel32): Boolean;
    procedure TestBlend; virtual;
    procedure TestBlendInplace; virtual;
    procedure TestBlendPixelLine; virtual;
    procedure TestBlendLine; virtual;
    procedure TestCombine; virtual;
    procedure TestCombineInplace; virtual;
    procedure TestCombineLine; virtual;
    procedure TestMerge; virtual;
    procedure TestMergeInplace; virtual;
    procedure TestMergeLine; virtual;
    procedure PerformanceTest; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestBlendModesNative = class(TCustomTestBlendModes)
  published
    procedure TestBlend; override;
    procedure TestBlendInplace; override;
    procedure TestBlendLine; override;
    procedure TestBlendPixelLine; override;
    procedure TestCombine; override;
    procedure TestCombineInplace; override;
    procedure TestCombineLine; override;
    procedure TestMerge; override;
    procedure TestMergeInplace; override;
    procedure TestMergeLine; override;
    procedure PerformanceTest; override;
  end;

  TTestBlendModesMMX = class(TCustomTestBlendModes)
  published
    procedure TestBlend; override;
    procedure TestBlendInplace; override;
    procedure TestBlendLine; override;
    procedure TestBlendPixelLine; override;
    procedure TestCombine; override;
    procedure TestCombineInplace; override;
    procedure TestCombineLine; override;
    procedure TestMerge; override;
    procedure TestMergeInplace; override;
    procedure TestMergeLine; override;
    procedure PerformanceTest; override;
  end;

  TTestBlendModesSSE2 = class(TCustomTestBlendModes)
  published
    procedure TestBlend; override;
    procedure TestBlendInplace; override;
    procedure TestBlendPixelLine; override;
    procedure TestBlendLine; override;
    procedure TestCombine; override;
    procedure TestCombineInplace; override;
    procedure TestCombineLine; override;
    procedure TestMerge; override;
    procedure TestMergeInplace; override;
    procedure TestMergeLine; override;
    procedure PerformanceTest; override;
  end;

implementation

uses
  DAV_GuiBlendReference;

{ TTestBlendModes }

procedure TCustomTestBlendModes.SetUp;
begin
 inherited;
 GetMem(FForeground, 256 * SizeOf(TPixel32));
 GetMem(FBackground, 256 * SizeOf(TPixel32));
 FColorDiff := 1;
end;

procedure TCustomTestBlendModes.TearDown;
begin
 inherited;
 Dispose(FForeground);
 Dispose(FBackground);
end;

function TCustomTestBlendModes.CompareColors(Expected, Actual: TPixel32): Boolean;
begin
 Result := False;
 if Abs(Expected.A - Actual.A) > FColorDiff then Exit;
 if Abs(Expected.R - Actual.R) > FColorDiff then Exit;
 if Abs(Expected.G - Actual.G) > FColorDiff then Exit;
 if Abs(Expected.B - Actual.B) > FColorDiff then Exit;
 Result := True;
end;


procedure TCustomTestBlendModes.TestBlend;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  RefIndex, Index : Integer;
begin
 // static test
 BlendColor32.A := $1A;
 BlendColor32.B := $2B;
 BlendColor32.G := $3C;
 BlendColor32.R := $4D;

 ExpectedColor32 := BlendPixelReference(BlendColor32, BlendColor32);
 CombinedColor32 := BlendPixel(BlendColor32, BlendColor32);
 EMMS;
 CombinedColor32.A := $FF;
 ExpectedColor32.A := $FF;
 CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
   'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
   ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));

 for RefIndex := 0 to High(Byte) do
  begin
   BlendColor32.B := RefIndex;
   BlendColor32.G := RefIndex shr 1;
   BlendColor32.R := RefIndex shr 2;
   for Index := 0 to High(Byte) do
    begin
     BlendColor32.A := Index;
     ExpectedColor32 := BlendPixelReference(BlendColor32, pxBlack32);
     CombinedColor32 := BlendPixel(BlendColor32, pxBlack32);
     EMMS;
     CombinedColor32.A := $FF;
     ExpectedColor32.A := $FF;
     CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
       'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
       ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
    end;
  end;
end;

procedure TCustomTestBlendModes.TestBlendInplace;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  RefIndex, Index : Integer;
begin
 for RefIndex := 0 to High(Byte) do
  begin
   BlendColor32.B := RefIndex;
   BlendColor32.G := RefIndex shr 1;
   BlendColor32.R := RefIndex shr 2;
   for Index := 0 to High(Byte) do
    begin
     BlendColor32.A := Index;
     ExpectedColor32 := pxBlack32;
     BlendPixelInplaceReference(BlendColor32, ExpectedColor32);
     CombinedColor32 := pxBlack32;
     BlendPixelInplace(BlendColor32, CombinedColor32);
     EMMS;
     CombinedColor32.A := $FF;
     CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
       'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
       ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
    end;
  end;
end;

procedure TCustomTestBlendModes.TestBlendLine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := pxBlack32;
    FForeground^[Index] := pxWhite32;
    FForeground^[Index].A := Index;
  end;

  BlendLine(PPixel32(FForeground), PPixel32(FBackground), 256);
  EMMS;

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    FBackground^[Index].A := 0;
    CheckTrue(CompareColors(ExpectedColor32, FBackground^[Index]),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestBlendPixelLine;
var
  BlendColor32 : TPixel32;
  Index        : Integer;
begin
  BlendColor32 := pxSemiWhite32;
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index].R := Index;
    FBackground^[Index].G := Index;
    FBackground^[Index].B := Index;
    FBackground^[Index].A := $FF;
    FForeground^[Index] := FBackground^[Index];
  end;

  BlendPixelLineReference(BlendColor32, PPixel32(FForeground), 256);
  BlendPixelLine(BlendColor32, PPixel32(FBackground), 256);
  EMMS;

  for Index := 0 to High(Byte) do
  begin
    FForeground^[Index].A := $FF;
    FBackground^[Index].A := $FF;
    CheckTrue(CompareColors(FForeground^[Index], FBackground^[Index]),
      'Color should be: ' + IntToHex(FForeground^[Index].ARGB, 8) +
      ', but was: ' + IntToHex(FBackground^[Index].ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestCombine;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  RefIndex, Index : Integer;
begin
 for RefIndex := 0 to High(Byte) do
  begin
   BlendColor32.A := $FF;
   BlendColor32.B := RefIndex;
   BlendColor32.G := RefIndex shr 1;
   BlendColor32.R := RefIndex shr 2;
   for Index := 0 to High(Byte) do
    begin
     ExpectedColor32 := CombinePixelReference(BlendColor32, pxBlack32, Index);
     CombinedColor32 := CombinePixel(BlendColor32, pxBlack32, Index);
     EMMS;
     CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
       'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
       ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
    end;
  end;
end;

procedure TCustomTestBlendModes.TestCombineInplace;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  RefIndex, Index : Integer;
begin
 for RefIndex := 0 to High(Byte) do
  begin
   BlendColor32.A := $FF;
   BlendColor32.B := RefIndex;
   BlendColor32.G := RefIndex shr 1;
   BlendColor32.R := RefIndex shr 2;
   for Index := 0 to High(Byte) do
    begin
     ExpectedColor32 := pxBlack32;
     CombinePixelInplaceReference(BlendColor32, ExpectedColor32, Index);
     CombinedColor32 := pxBlack32;
     CombinePixelInplace(BlendColor32, CombinedColor32, Index);
     EMMS;
     CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
       'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
       ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
    end;
  end;
end;

procedure TCustomTestBlendModes.TestCombineLine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := pxBlack32;
    FForeground^[Index] := pxWhite32;
    FForeground^[Index].A := Index;
  end;

  CombineLine(PPixel32(FForeground), PPixel32(FBackground), 256, $FF);

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 24) or $FFFFFF;
    EMMS;
    CheckTrue(CompareColors(ExpectedColor32, FBackground^[Index]),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestMerge;
var
  MergeColor32    : TPixel32;
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
 MergeColor32 := pxBlack32;
 for RefIndex := 0 to High(Byte) do
  begin
   BlendColor32.B := RefIndex;
   BlendColor32.G := RefIndex shr 1;
   BlendColor32.R := RefIndex shr 2;
   for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
    begin
     BlendColor32.A := AlphaIndex shl 4;
     for Index := 0 to High(Byte) do
      begin
       MergeColor32.A := Index;
       ExpectedColor32 := MergePixelReference(BlendColor32, MergeColor32);
       CombinedColor32 := MergePixel(BlendColor32, MergeColor32);
       EMMS;
       CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
         'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
         ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
         '(RefIndex: ' + IntToStr(RefIndex) + ', ' +
         ' AlphaIndex: ' + IntToStr(AlphaIndex) + #10#13 +
         'Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
         ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8));
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestMergeInplace;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    BlendColor32 := pxWhite32;
    BlendColor32.A := Index;
    ExpectedColor32.ARGB := $FF000000 + (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := pxBlack32;
    MergePixelInplace(BlendColor32, CombinedColor32);
    EMMS;
    CheckTrue(CompareColors(ExpectedColor32, CombinedColor32),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestMergeLine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := pxBlack32;
    FForeground^[Index] := pxWhite32;
    FForeground^[Index].A := Index;
  end;

  MergeLine(PPixel32(FForeground), PPixel32(FBackground), 256);

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    EMMS;
    FBackground^[Index].A := 0;
    CheckTrue(CompareColors(ExpectedColor32, FBackground^[Index]),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.PerformanceTest;
var
  Start, Stop, Freq : Int64;
  BlendColor32      : TPixel32;
  Index             : Integer;
begin
 BlendColor32 := pxWhite32;
 BlendColor32.A := $5A;

 QueryPerformanceFrequency(Freq);
 QueryPerformanceCounter(Start);

 for Index := 0 to $7FFFFFF
  do BlendPixel(BlendColor32, pxBlack32);
 EMMS;

 for Index := 0 to $7FFFFFF do
  begin
   BlendPixel(BlendColor32, pxBlack32);
   EMMS;
  end;

 QueryPerformanceCounter(Stop);

 Fail('Performance: ' + FloatToStr(1000 * (Stop - Start) / Freq));
end;


{ TTestBlendModesNative }

procedure TTestBlendModesNative.TestBlend;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestBlendInplace;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestBlendLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestBlendPixelLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestCombine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestCombineInplace;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestCombineLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestMerge;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestMergeInplace;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestMergeLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.PerformanceTest;
begin
  BindingBlend.Rebind([]);
  inherited;
end;


{ TTestBlendModesMMX }

procedure TTestBlendModesMMX.TestBlend;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendInplace;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendPixelLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestCombine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineInplace;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestMerge;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeInplace;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.PerformanceTest;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;


{ TTestBlendModesSSE2 }

procedure TTestBlendModesSSE2.TestBlend;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendInplace;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendPixelLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineInplace;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMerge;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeInplace;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.PerformanceTest;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;


initialization
  RegisterTest(TTestBlendModesNative.Suite);
  if ProcessorInfo.HasMMX
   then RegisterTest(TTestBlendModesMMX.Suite);
  if ssSSE2 in ProcessorInfo.SupportsSSE
   then RegisterTest(TTestBlendModesSSE2.Suite);

end.
