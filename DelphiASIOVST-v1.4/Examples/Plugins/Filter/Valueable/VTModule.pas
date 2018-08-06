unit VTModule;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_VSTModule;

type
  TTimeDomainConvole = procedure(InOutBuffer, IRBuffer: PSingle;
    SampleCount: Integer; Current: Single);

  TDriveMode = (dmRoasty1 = 1, dmRoasty2 = 2, dmSteamin1 = 3, dmSteamin2 = 4);

  TVTVSTModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamDriveDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHiGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamChannelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowBypassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHiBypassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FDriveMode       : TDriveMode;
    FBufferPos       : Cardinal;
    FCircularBuffer  : array [0..1] of PDAVSingleFixedArray;
    FHistoryBuffer   : array [0..1] of PDAVSingleFixedArray;
    FBassKernel      : PDAVSingleFixedArray;
    FTrebleKernel    : PDAVSingleFixedArray;
    FFilterKernel    : PDAVSingleFixedArray;
    FImpulseResponse : array [1..4, 0..1, 0..47] of PDAVSingleFixedArray;
    FKernelSize      : Cardinal;
    FOutGain         : Single;
    FConvolveIR      : TTimeDomainConvole;
    FModeBypass      : array [0..1] of Boolean;
    procedure SetCPUDependant;
    procedure SetKernelSize(const Value: Cardinal);
    procedure KernelSizeChanged;
    procedure BuildBassFilterKernel;
    procedure BuildTrebleFilterKernel;
    procedure BuildCompleteFilterKernel;
  public
    property KernelSize: Cardinal read FKernelSize write SetKernelSize;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, DAV_Common, DAV_DspDFT, DAV_VSTCustomModule,
  DAV_VSTModuleWithPrograms, VTGUI;

const
  CKernelSizes: array [1..4, 0..1] of Integer =
    ((148, 55), (144, 49), (147, 55), (146, 50));

  CKernelResourceNames: array [1..4, 0..1] of AnsiString =
    (('Roasty1Bass', 'Roasty1Treble'), ('Roasty2Bass', 'Roasty2Treble'),
     ('Steamin1Bass', 'Steamin1Treble'), ('Steamin2Bass', 'Steamin2Treble'));

procedure ConvolveIR_X87(InOutBuffer, IRBuffer: PSingle;
  SampleFrames: Integer; Current: Single);
asm
    FLD     Current.Single
@SmallLoop:
    FLD     [EDX].Single
    FMUL    ST(0), ST(1)
    FLD     [EAX].Single
    FADDP

    FSTP    [EAX].Single
    ADD     EAX, 4
    ADD     EDX, 4
    LOOP    @SmallLoop

@EndSmallLoop:
    FSTP ST(0)
end;

{$IFNDEF CPU64}
procedure ConvolveIR_X87large(InOutBuffer, IRBuffer: PSingle;
  SampleCount: Integer; Current: Single);
asm
    FLD     Current.Single

    PUSH    ECX
    SHR     ECX, 2
    JZ      @SkipLargeAddLoop

@LargeLoop:
    FLD     [EDX].Single
    FMUL    ST(0), ST(1)
    FLD     [EAX].Single
    FADDP
    FSTP    [EAX].Single
    FLD     [EDX + 4].Single
    FMUL    ST(0), ST(1)
    FLD     [EAX + 4].Single
    FADDP
    FSTP    [EAX + 4].Single
    FLD     [EDX + 8].Single
    FMUL    ST(0), ST(1)
    FLD     [EAX + 8].Single
    FADDP
    FSTP    [EAX + 8].Single
    FLD     [EDX + 12].Single
    FMUL    ST(0), ST(1)
    FLD     [EAX + 12].Single
    FADDP
    FSTP    [EAX + 12].Single

    ADD     EAX, 16
    ADD     EDX, 16
    LOOP    @LargeLoop

@SkipLargeAddLoop:
    POP     ECX
    AND     ECX, $3
    JZ @EndSmallLoop

    @SmallLoop:
    FLD     [EDX].Single
    FMUL    ST(0), ST(1)
    FLD     [EAX].Single
    FADDP
    FSTP    [EAX].Single

    ADD     EAX, 4
    ADD     EDX, 4
    LOOP    @SmallLoop

@EndSmallLoop:
    FSTP    ST(0)
end;
{$ENDIF}

procedure ConvolveIR_X87SSE(InOutBuffer, IRBuffer: PSingle;
  SampleCount: Integer; Current: Single);
asm
{$IFDEF CPU64}
    PUSH    R8
    SHR     R8, 3
    JZ      @SkipLargeAddLoop

    MOVSS   XMM7, Current
    SHUFPS  XMM7, XMM7, 0h
@LargeLoop:
    MOVUPS  XMM0, [EDX]
    MULPS   XMM0, XMM7
    MOVUPS  XMM1, [EAX]
    ADDPS   XMM0, XMM1
    MOVUPS  [EAX], XMM0

    MOVUPS  XMM2, [EDX+16]
    MULPS   XMM2, XMM7
    MOVUPS  XMM3, [EAX+16]
    ADDPS   XMM2, XMM3
    MOVUPS  [EAX+16], XMM2

    ADD     EAX, 32
    ADD     EDX, 32
    LOOP    @LargeLoop

@SkipLargeAddLoop:
    POP     R8
    AND     R8, $7
    JZ      @EndSmallLoop

    FLD     Current.Single
@SmallLoop:
    FLD     [EDX].Single
    FMUL    ST(0),ST(1)
    FLD     [EAX].Single
    FADDP
    FSTP    [EAX].Single

    ADD     EAX, 4
    ADD     EDX, 4
    LOOP    @SmallLoop

@EndSmallLoop:
{$ELSE}
    PUSH    ECX
    shr     ECX,3
    JZ      @SkipLargeAddLoop

    MOVSS   xmm7, Current
    SHUFPS  xmm7, xmm7, 0h
    @LargeLoop:
    MOVUPS  xmm0,[EDX]
    MULPS   xmm0,xmm7
    MOVUPS  xmm1,[EAX]
    ADDPS   xmm0,xmm1
    MOVUPS  [EAX],xmm0

    MOVUPS  xmm2,[EDX+16]
    MULPS   xmm2,xmm7
    MOVUPS  xmm3,[EAX+16]
    ADDPS   xmm2,xmm3
    MOVUPS  [EAX+16],xmm2

    ADD     EAX, 32
    ADD     EDX, 32
    LOOP    @LargeLoop

    @SkipLargeAddLoop:
    POP     ECX
    AND     ECX,$00000007
    JZ      @EndSmallLoop

    FLD     Current.Single
    @SmallLoop:
    FLD     [EDX].Single
    FMUL    ST(0),ST(1)
    FLD     [EAX].Single
    FADDP
    FSTP    [EAX].Single

    ADD     EAX, 4
    ADD     EDX, 4
    LOOP    @SmallLoop

    @EndSmallLoop:
    FSTP   ST(0)
{$ENDIF}
end;


{ TVTVSTModule }

procedure TVTVSTModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TVTVSTModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TVTVSTModule.VSTModuleOpen(Sender: TObject);
var
  i, m, n, sz : Integer;
begin
 FDriveMode := dmRoasty1;
 FBassKernel := nil;
 FTrebleKernel := nil;

 FOutGain := 1.25;
 FBufferPos := 0;
 FHistoryBuffer[0] := nil;
 FHistoryBuffer[1] := nil;
 FCircularBuffer[0] := nil;
 FCircularBuffer[1] := nil;

 for m := 1 to 4 do
  for n := 0 to 1 do
   with TResourceStream.Create(hInstance, CKernelResourceNames[m, n], RT_RCDATA) do
    try
     for i := 0 to Length(FImpulseResponse[m, n]) - 1 do
      begin
       GetMem(FImpulseResponse[m, n, i], CKernelSizes[m, n] * SizeOf(Single));
       sz := Read(FImpulseResponse[m, n, i]^, CKernelSizes[m, n] * SizeOf(Single));
       Assert(sz = CKernelSizes[m, n] * SizeOf(Single));
      end;
    finally
     Free;
    end;
 SetCPUDependant;

 BuildBassFilterKernel;
 BuildTrebleFilterKernel;
 BuildCompleteFilterKernel;

 EditorFormClass := TFmVT;
end;

procedure TVTVSTModule.VSTModuleClose(Sender: TObject);
begin
  Dispose(FHistoryBuffer[0]);
  Dispose(FHistoryBuffer[1]);
  Dispose(FCircularBuffer[0]);
  Dispose(FCircularBuffer[1]);
  Dispose(FBassKernel);
  Dispose(FTrebleKernel);
end;

procedure TVTVSTModule.SetCPUDependant;
begin
// if not (isFPU in CPU.Instructions) then raise Exception.Create('FPU not found');
  FConvolveIR := ConvolveIR_X87large;
(*
 if (isSSE in CPU.Instructions)
  then FConvolveIR := ConvolveIR_X87SSE;
*)
end;


procedure TVTVSTModule.KernelSizeChanged;
begin
 ReallocMem(FFilterKernel, FKernelSize * SizeOf(Single));
 ReallocMem(FHistoryBuffer[0], FKernelSize * SizeOf(Single));
 ReallocMem(FHistoryBuffer[1], FKernelSize * SizeOf(Single));
 ReallocMem(FCircularBuffer[0], 2 * FKernelSize * SizeOf(Single));
 ReallocMem(FCircularBuffer[1], 2 * FKernelSize * SizeOf(Single));

 FillChar(FFilterKernel^[0], FKernelSize * SizeOf(Single), 0);
 FillChar(FHistoryBuffer[0]^[0], FKernelSize * SizeOf(Single), 0);
 FillChar(FHistoryBuffer[1]^[0], FKernelSize * SizeOf(Single), 0);
 FillChar(FCircularBuffer[0]^[0], 2 * FKernelSize * SizeOf(Single), 0);
 FillChar(FCircularBuffer[1]^[0], 2 * FKernelSize * SizeOf(Single), 0);

 if FBufferPos >= FKernelSize
  then FBufferPos := 0;
end;

procedure TVTVSTModule.SetKernelSize(const Value: Cardinal);
begin
 if FKernelSize <> Value then
  begin
   FKernelSize := Value;
   KernelSizeChanged;
  end;
end;

procedure TVTVSTModule.ParamHiBypassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FModeBypass[1] := (Round(Value) > 0);
 BuildCompleteFilterKernel;

 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleBypass;
end;

procedure TVTVSTModule.ParamLowBypassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FModeBypass[0] := (Round(Value) > 0);
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassBypass;
end;

procedure TVTVSTModule.ParamDriveDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  1 : PreDefined := 'Roasty 1';
  2 : PreDefined := 'Roasty 2';
  3 : PreDefined := 'Steamin'' 1';
  4 : PreDefined := 'Steamin'' 2';
 end;
end;

procedure TVTVSTModule.ParamChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  1 : PreDefined := 'Mono';
  2 : PreDefined := 'Stereo';
 end;
end;

procedure TVTVSTModule.BuildBassFilterKernel;
var
  SngleIndex : Single;
  LowerIndex : Integer;
  UpperIndex : Integer;
  i          : Integer;
  Lwr, Upr   : PDAVSingleFixedArray;
begin
 // Calculate Filter Index
 SngleIndex := (Parameter[0] + 12) * 2;
 LowerIndex := Round(SngleIndex - 0.5);
 UpperIndex := LowerIndex + 1;
 if LowerIndex <  0 then LowerIndex :=  0;
 if UpperIndex <  0 then UpperIndex :=  0;
 if LowerIndex > 47 then LowerIndex := 47;
 if UpperIndex > 47 then UpperIndex := 47;
 SngleIndex := SngleIndex - LowerIndex;

 // Setup Filter Kernel Pointer
 Lwr := FImpulseResponse[Integer(FDriveMode), 0, LowerIndex];
 Upr := FImpulseResponse[Integer(FDriveMode), 0, UpperIndex];

 // Build Filter Kernel
 ReallocMem(FBassKernel, CKernelSizes[Integer(FDriveMode), 0] * SizeOf(Single));
 for i := 0 to CKernelSizes[Integer(FDriveMode), 0] - 1
  do FBassKernel[i] := (1 - SngleIndex) * Lwr^[i] + SngleIndex  * Upr^[i];
end;

procedure TVTVSTModule.BuildTrebleFilterKernel;
var
  SngleIndex : Single;
  LowerIndex : Integer;
  UpperIndex : Integer;
  i          : Integer;
  Lwr, Upr   : PDAVSingleFixedArray;
begin
 // Calculate Filter Index
 SngleIndex := (Parameter[1] + 12) * 2;
 LowerIndex := Round(SngleIndex - 0.5);
 UpperIndex := LowerIndex + 1;
 if LowerIndex <  0 then LowerIndex :=  0;
 if UpperIndex <  0 then UpperIndex :=  0;
 if LowerIndex > 47 then LowerIndex := 47;
 if UpperIndex > 47 then UpperIndex := 47;
 SngleIndex := SngleIndex - LowerIndex;

 // Setup Filter Kernel Pointer
 Lwr := FImpulseResponse[Integer(FDriveMode), 1, LowerIndex];
 Upr := FImpulseResponse[Integer(FDriveMode), 1, UpperIndex];

 // Build Filter Kernel
 ReallocMem(FTrebleKernel, CKernelSizes[Integer(FDriveMode), 1] * SizeOf(Single));
 for i := 0 to CKernelSizes[Integer(FDriveMode), 1] - 1
  do FTrebleKernel[i] := (1 - SngleIndex) * Lwr^[i] + SngleIndex  * Upr^[i];
end;

procedure TVTVSTModule.BuildCompleteFilterKernel;

  procedure SimpleConvolveIR(const OutBuffer: PSingle;
                             const IR1: PSingle;
                             const IR1Size: Integer;
                             const IR2: PSingle;
                             const IR2Size: Integer);
  asm
   PUSH ebx
   PUSH edi
   mov edi, IR2Size
   imul edi, 4
   sub edi, 4
   @OuterLoop:
     mov ebx, IR2
     PUSH ECX
     mov ECX, IR2Size
     FLD [EDX].Single            // load IR1 sample
     @InnerLoop:
       FLD   [ebx].Single        // IR2, IR1
       FMUL  ST(0), ST(1)        // IR1 * IR2, IR1
       FLD   [EAX].Single        // Out, IR1 * IR2, IR1
       FADDP                     // Out + IR1 * IR2, IR1
       FSTP [EAX].Single         // NewOut := Out + IR1 * IR2, IR1

       ADD   EAX, 4              // inc(Out)
       ADD   ebx, 4              // inc(IR2)
       LOOP  @InnerLoop
     POP ECX
     FSTP  ST(0)
     ADD   EDX, 4                // inc(IR1)
     sub   EAX, edi
   LOOP @OuterLoop

   POP edi
   POP ebx
  end;

var
  TempIR : PDAVSingleFixedArray;
begin
 FCriticalSection.Enter;
 try
  if FModeBypass[0] and FModeBypass[1] then
   begin
    KernelSize := 1;
    FFilterKernel^[0] := 0.8;
   end else
  if FModeBypass[0] then
   begin
    KernelSize := CKernelSizes[Integer(FDriveMode), 1];
    Move(FTrebleKernel[0], FFilterKernel^[0], KernelSize * SizeOf(Single));
   end else
  if FModeBypass[1] then
   begin
    KernelSize := CKernelSizes[Integer(FDriveMode), 0];
    Move(FBassKernel[0], FFilterKernel^[0], KernelSize * SizeOf(Single));
   end
  else
   begin
    KernelSize := CKernelSizes[Integer(FDriveMode), 0] + CKernelSizes[Integer(FDriveMode), 1];
    GetMem(TempIR, KernelSize * SizeOf(Single));
    FillChar(TempIR^[0], KernelSize * SizeOf(Single), 0);
    SimpleConvolveIR(@TempIR^[0],
                     @FBassKernel[0], CKernelSizes[Integer(FDriveMode), 0] - 1,
                     @FTrebleKernel[0], CKernelSizes[Integer(FDriveMode), 1] - 1);
    Move(TempIR^[0], FFilterKernel^[0], KernelSize * SizeOf(Single));
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVTVSTModule.ParamLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BuildBassFilterKernel;
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassGain;
end;

procedure TVTVSTModule.ParamHiGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BuildTrebleFilterKernel;
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleGain;
end;

procedure TVTVSTModule.ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  NewDriveMode : TDriveMode;
begin
 NewDriveMode := TDriveMode(Round(Value));

 if FDriveMode <> NewDriveMode then
  begin
   FDriveMode := NewDriveMode;
   BuildBassFilterKernel;
   BuildTrebleFilterKernel;
   BuildCompleteFilterKernel;
   if EditorForm is TFmVT then
    with TFmVT(EditorForm) do UpdateSelector;
  end;
end;

procedure TVTVSTModule.ParamChannelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else OnProcess := nil;
 end;
 OnProcess32Replacing := OnProcess;

 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateChannel;
end;

procedure TVTVSTModule.ParamOutGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutGain := 1.25 * dB_to_Amp(Value);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateGain;
end;

procedure TVTVSTModule.VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex: Cardinal;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FHistoryBuffer[0, FBufferPos] := Inputs[0, SampleIndex];
    Outputs[0, SampleIndex] := FOutGain * (FCircularBuffer[0, FBufferPos] + FHistoryBuffer[0, FBufferPos] * FFilterKernel[0]);
    FCircularBuffer[0, FBufferPos] := 0;
    FConvolveIR(@FCircularBuffer[0, FBufferPos], @FFilterKernel[0], FKernelSize, FHistoryBuffer[0, FBufferPos]);
    Inc(FBufferPos);
    if FBufferPos >= FKernelSize then
     begin
      FBufferPos := 0;
      Move(FCircularBuffer[0, FKernelSize], FCircularBuffer[0, 0], FKernelSize * SizeOf(Single));
      FillChar(FCircularBuffer[0, FKernelSize], FKernelSize * SizeOf(Single), 0);
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVTVSTModule.VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex: Cardinal;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    FHistoryBuffer[0, FBufferPos] := Inputs[0, SampleIndex];
    Outputs[0, SampleIndex] := FOutGain * (FCircularBuffer[0, FBufferPos] + FHistoryBuffer[0, FBufferPos] * FFilterKernel[0]);
    FCircularBuffer[0, FBufferPos] := 0;

    FHistoryBuffer[1, FBufferPos] := Inputs[1, SampleIndex];
    Outputs[1, SampleIndex] := FOutGain * (FCircularBuffer[1, FBufferPos] + FHistoryBuffer[1, FBufferPos] * FFilterKernel[0]);
    FCircularBuffer[1, FBufferPos] := 0;

    FConvolveIR(@FCircularBuffer[0, FBufferPos], @FFilterKernel[0], FKernelSize, FHistoryBuffer[0, FBufferPos]);
    FConvolveIR(@FCircularBuffer[1, FBufferPos], @FFilterKernel[0], FKernelSize, FHistoryBuffer[1, FBufferPos]);
    Inc(FBufferPos);

    if FBufferPos >= FKernelSize then
     begin
      FBufferPos := 0;
      Move(FCircularBuffer[0, FKernelSize], FCircularBuffer[0, 0], FKernelSize * SizeOf(Single));
      Move(FCircularBuffer[1, FKernelSize], FCircularBuffer[1, 0], FKernelSize * SizeOf(Single));
      FillChar(FCircularBuffer[0, FKernelSize], FKernelSize * SizeOf(Single), 0);
      FillChar(FCircularBuffer[1, FKernelSize], FKernelSize * SizeOf(Single), 0);
     end;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVTVSTModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate - 44100) > 4000
  then ShowMessage('Samplerates other than 44.1 kHz have not been implemented yet');
end;

end.
