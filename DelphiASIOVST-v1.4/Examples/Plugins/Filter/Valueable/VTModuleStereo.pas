unit VTModuleStereo;

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
    Samples: Integer; Current: Single);

  TDriveMode = (dmRoasty1 = 1, dmRoasty2 = 2, dmSteamin1 = 3, dmSteamin2 = 4);

  TVTVSTModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMidSide(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamChannelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDriveDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamHiBypassLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHiBypassRightChange(Sender: TObject; const Index: Integer;var Value: Single);
    procedure ParamHiGainLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHiGainRightChange(Sender: TObject; const Index: Integer;var Value: Single);
    procedure ParamLowBypassLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowBypassRightChange(Sender: TObject; const Index: Integer;var Value: Single);
    procedure ParamLowGainLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowGainRightChange(Sender: TObject; const Index: Integer;var Value: Single);
    procedure ParamOutGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBypassDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FDriveMode       : TDriveMode;
    FBufferPos       : array [0..1] of Cardinal;
    FCircularBuffer  : array [0..1] of PDAVSingleFixedArray;
    FHistoryBuffer   : array [0..1] of PDAVSingleFixedArray;
    FBassKernel      : array [0..1] of PDAVSingleFixedArray;
    FTrebleKernel    : array [0..1] of PDAVSingleFixedArray;
    FFilterKernel    : array [0..1] of PDAVSingleFixedArray;
    FImpulseResponse : array [1..4, 0..1, 0..47] of PDAVSingleFixedArray;
    FKernelSize      : array [0..1] of Cardinal;
    FOutGain         : Single;
    FConvolveIR      : TTimeDomainConvole;
    FModeBypass      : array [0..1, 0..1] of Boolean;
    function GetKernelSize(Index: Integer): Cardinal;
    procedure SetKernelSize(Index: Integer; const Value: Cardinal);
    procedure SetCPUDependant;
    procedure SetKernelSizeLeft(const Value: Cardinal);
    procedure SetKernelSizeRight(const Value: Cardinal);
  protected
    procedure BuildBassFilterKernel(const index: Integer);
    procedure BuildTrebleFilterKernel(const index: Integer);
    procedure BuildCompleteFilterKernel(const index: Integer);

    procedure BuildEntireFilters; virtual;
    procedure KernelSizeLeftChanged; virtual;
    procedure KernelSizeRightChanged; virtual;
  public
    property KernelSizeLeft: Cardinal read FKernelSize[0] write SetKernelSizeLeft;
    property KernelSizeRight: Cardinal read FKernelSize[1] write SetKernelSizeRight;
    property KernelSize[Index: Integer]: Cardinal read GetKernelSize write SetKernelSize;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, DAV_Common, DAV_DspDFT, DAV_VSTCustomModule,
  DAV_VSTModuleWithPrograms, VTGUIStereo;

const
  CKernelSizes: array [1..4, 0..1] of Integer =
    ((148, 55), (144, 49), (147, 55), (146, 50));

  CKernelResourceNames: array [1..4, 0..1] of AnsiString =
    (('Roasty1Bass', 'Roasty1Treble'), ('Roasty2Bass', 'Roasty2Treble'),
     ('Steamin1Bass', 'Steamin1Treble'), ('Steamin2Bass', 'Steamin2Treble'));

procedure ConvolveIR_X87(InOutBuffer, IRBuffer: PSingle;
  SampleFrames: Integer; Current: Single);
asm
 fld   Current.Single
 @SmallLoop:
 fld   [edx].Single
 fmul  st(0),st(1)
 fld   [eax].Single
 faddp

 fstp [eax].Single
 add   eax, 4
 add   edx, 4
 loop  @SmallLoop

 @EndSmallLoop:
 ffree st(0)
end;

procedure ConvolveIR_X87large(InOutBuffer, IRBuffer: PSingle;
  samples: Integer; Current: Single);
asm
    fld   Current.Single

    push ecx
    shr ecx,2
    jz @SkipLargeAddLoop
    @LargeLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp
    fstp [eax].Single
    fld   [edx+4].Single
    fmul  st(0),st(1)
    fld   [eax+4].Single
    faddp
    fstp [eax+4].Single
    fld   [edx+8].Single
    fmul  st(0),st(1)
    fld   [eax+8].Single
    faddp
    fstp [eax+8].Single
    fld   [edx+12].Single
    fmul  st(0),st(1)
    fld   [eax+12].Single
    faddp
    fstp [eax+12].Single

    add   eax, 16
    add   edx, 16
    loop  @LargeLoop

    @SkipLargeAddLoop:
    pop ecx
    and ecx,$00000003
    jz @EndSmallLoop

    @SmallLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp
    fstp [eax].Single

    add   eax, 4
    add   edx, 4
    loop  @SmallLoop

    @EndSmallLoop:
    ffree st(0)
end;

procedure ConvolveIR_X87SSE(InOutBuffer, IRBuffer: PSingle;
  samples: Integer; Current: Single);
asm
    push ecx
    shr ecx,3
    jz @SkipLargeAddLoop

    movss xmm7, Current
    shufps xmm7, xmm7, 0h
    @LargeLoop:
    movups xmm0,[edx]
    mulps xmm0,xmm7
    movups xmm1,[eax]
    addps xmm0,xmm1
    movups [eax],xmm0

    movups xmm2,[edx+16]
    mulps xmm2,xmm7
    movups xmm3,[eax+16]
    addps xmm2,xmm3
    movups [eax+16],xmm2

    add   eax, 32
    add   edx, 32
    loop  @LargeLoop

    @SkipLargeAddLoop:
    pop ecx
    and ecx,$00000007
    jz @EndSmallLoop

    fld   Current.Single
    @SmallLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp
    fstp [eax].Single

    add   eax, 4
    add   edx, 4
    loop  @SmallLoop

    @EndSmallLoop:
    ffree st(0)
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
 EditorFormClass := TFmVT;

 FDriveMode := dmRoasty1;
 FOutGain := 1.25;
 FBufferPos[0] := 0;
 FBufferPos[1] := 0;
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
 BuildEntireFilters;

 // Initial Parameters
 Parameter[ 0] := 0;
 Parameter[ 1] := 0;
 Parameter[ 2] := 0;
 Parameter[ 3] := 0;
 Parameter[ 4] := 0;
 Parameter[ 5] := 0;
 Parameter[ 6] := 0;
 Parameter[ 7] := 0;
 Parameter[ 8] := 1;
 Parameter[ 9] := 1;
 Parameter[10] := 0;
end;

procedure TVTVSTModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FHistoryBuffer[0]);
 Dispose(FHistoryBuffer[1]);
 Dispose(FCircularBuffer[0]);
 Dispose(FCircularBuffer[1]);
 Dispose(FBassKernel[0]);
 Dispose(FBassKernel[1]);
 Dispose(FTrebleKernel[0]);
 Dispose(FTrebleKernel[1]);
 Dispose(FFilterKernel[0]);
 Dispose(FFilterKernel[1]);
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


procedure TVTVSTModule.KernelSizeLeftChanged;
begin
 ReallocMem(FFilterKernel[0], FKernelSize[0] * SizeOf(Single));
 ReallocMem(FHistoryBuffer[0], FKernelSize[0] * SizeOf(Single));
 ReallocMem(FCircularBuffer[0], 2 * FKernelSize[0] * SizeOf(Single));

 FillChar(FFilterKernel[0]^[0], FKernelSize[0] * SizeOf(Single), 0);
 FillChar(FHistoryBuffer[0]^[0], FKernelSize[0] * SizeOf(Single), 0);
 FillChar(FCircularBuffer[0]^[0], 2 * FKernelSize[0] * SizeOf(Single), 0);

 if FBufferPos[0] >= FKernelSize[0]
  then FBufferPos[0] := 0;
end;

procedure TVTVSTModule.KernelSizeRightChanged;
begin
 ReallocMem(FFilterKernel[1], FKernelSize[1] * SizeOf(Single));
 ReallocMem(FHistoryBuffer[1], FKernelSize[1] * SizeOf(Single));
 ReallocMem(FCircularBuffer[1], 2 * FKernelSize[1] * SizeOf(Single));

 FillChar(FFilterKernel[1]^[0], FKernelSize[1] * SizeOf(Single), 0);
 FillChar(FHistoryBuffer[1]^[0], FKernelSize[1] * SizeOf(Single), 0);
 FillChar(FCircularBuffer[1]^[0], 2 * FKernelSize[1] * SizeOf(Single), 0);

 if FBufferPos[1] >= FKernelSize[1]
  then FBufferPos[1] := 0;
end;

procedure TVTVSTModule.SetKernelSize(Index: Integer; const Value: Cardinal);
begin
 case Index of
  0 : SetKernelSizeLeft(Value);
  1 : SetKernelSizeRight(Value);
 end;
end;

procedure TVTVSTModule.SetKernelSizeLeft(const Value: Cardinal);
begin
 if FKernelSize[0] <> Value then
  begin
   FKernelSize[0] := Value;
   KernelSizeLeftChanged;
  end;
end;

procedure TVTVSTModule.SetKernelSizeRight(const Value: Cardinal);
begin
 if FKernelSize[1] <> Value then
  begin
   FKernelSize[1] := Value;
   KernelSizeRightChanged;
  end;
end;

procedure TVTVSTModule.ParamHiBypassLeftChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FModeBypass[0, 1] := (Round(Value) > 0);
 BuildCompleteFilterKernel(0);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleBypassLeft;
end;

procedure TVTVSTModule.ParamHiBypassRightChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FModeBypass[1, 1] := (Round(Value) > 0);
 BuildCompleteFilterKernel(1);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleBypassRight;
end;

procedure TVTVSTModule.ParamLowBypassLeftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FModeBypass[0, 0] := (Round(Value) > 0);
 BuildCompleteFilterKernel(0);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassBypassLeft;
end;

procedure TVTVSTModule.ParamLowBypassRightChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FModeBypass[1, 0] := (Round(Value) > 0);
 BuildCompleteFilterKernel(1);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassBypassRight;
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
    1 : PreDefined := 'Mid/Side';
    2 : PreDefined := 'Left/Right';
   end;
end;

procedure TVTVSTModule.BuildBassFilterKernel(const index: Integer);
var
  SngleIndex : Single;
  LowerIndex : Integer;
  UpperIndex : Integer;
  i          : Integer;
  Lwr, Upr   : PDAVSingleFixedArray;
begin
 // Calculate Filter Index
 SngleIndex := (Parameter[2 + 4 * index] + 12) * 2;
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
 ReallocMem(FBassKernel[index], CKernelSizes[Integer(FDriveMode), 0] * SizeOf(Single));
 for i := 0 to CKernelSizes[Integer(FDriveMode), 0] - 1 do
  begin
   FBassKernel[index, i] := (1 - SngleIndex) * Lwr^[i] +
                                 SngleIndex  * Upr^[i];
  end;
end;

procedure TVTVSTModule.BuildTrebleFilterKernel(const index: Integer);
var
  SngleIndex : Single;
  LowerIndex : Integer;
  UpperIndex : Integer;
  i          : Integer;
  Lwr, Upr   : PDAVSingleFixedArray;
begin
 // Calculate Filter Index
 SngleIndex := (Parameter[4 * index] + 12) * 2;
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
 ReallocMem(FTrebleKernel[index], CKernelSizes[Integer(FDriveMode), 1] * SizeOf(Single));
 for i := 0 to CKernelSizes[Integer(FDriveMode), 1] - 1 do
  begin
   FTrebleKernel[index, i] := (1 - SngleIndex) * Lwr^[i] +
                                   SngleIndex  * Upr^[i];
  end;
end;

function TVTVSTModule.GetKernelSize(Index: Integer): Cardinal;
begin
 case Index of
  0..1 : result := FKernelSize[Index];
  else result := 0;
 end;
end;

procedure TVTVSTModule.BuildCompleteFilterKernel(const index: Integer);

  procedure SimpleConvolveIR(const OutBuffer: PSingle;
                             const IR1: PSingle;
                             const IR1Size: Integer;
                             const IR2: PSingle;
                             const IR2Size: Integer);
  asm
   push ebx
   push edi
   mov edi, IR2Size
   imul edi, 4
   sub edi, 4
   @OuterLoop:
     mov ebx, IR2
     push ecx
     mov ecx, IR2Size
     fld [edx].Single            // load IR1 sample
     @InnerLoop:
       fld   [ebx].Single        // IR2, IR1
       fmul  st(0), st(1)        // IR1 * IR2, IR1
       fld   [eax].Single        // Out, IR1 * IR2, IR1
       faddp                     // Out + IR1 * IR2, IR1
       fstp [eax].Single         // NewOut := Out + IR1 * IR2, IR1

       add   eax, 4              // inc(Out)
       add   ebx, 4              // inc(IR2)
       loop  @InnerLoop
     pop ecx
     fstp  st(0)
     add   edx, 4                // inc(IR1)
     sub   eax, edi
   loop @OuterLoop

   pop edi
   pop ebx
  end;

var
  TempIR : PDAVSingleFixedArray;
begin
 FCriticalSection.Enter;
 try
  if FModeBypass[index, 0] and FModeBypass[index, 1] then
   begin
    KernelSize[index] := 1;
    FFilterKernel[index]^[0] := 0.8;
   end else
  if FModeBypass[index, 0] then
   begin
    FKernelSize[index] := CKernelSizes[Integer(FDriveMode), 1];
    Move(FTrebleKernel[index]^[0], FFilterKernel[index]^[0], KernelSize[index] * SizeOf(Single));
   end else
  if FModeBypass[index, 1] then
   begin
    KernelSize[index] := CKernelSizes[Integer(FDriveMode), 0];
    Move(FBassKernel[index]^[0], FFilterKernel[index]^[0], KernelSize[index] * SizeOf(Single));
   end
  else
   begin
    KernelSize[index] := CKernelSizes[Integer(FDriveMode), 0] + CKernelSizes[Integer(FDriveMode), 1];
    GetMem(TempIR, KernelSize[index] * SizeOf(Single));
    FillChar(TempIR^[0], KernelSize[index] * SizeOf(Single), 0);
    SimpleConvolveIR(@TempIR^[0],
                     @FBassKernel[index]^[0], CKernelSizes[Integer(FDriveMode), 0] - 1,
                     @FTrebleKernel[index]^[0], CKernelSizes[Integer(FDriveMode), 1] - 1);
    Move(TempIR^[0], FFilterKernel[index]^[0], KernelSize[index] * SizeOf(Single));
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVTVSTModule.ParamLowGainLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BuildBassFilterKernel(0);
 BuildCompleteFilterKernel(0);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassGainLeft;
end;

procedure TVTVSTModule.ParamLowGainRightChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 BuildBassFilterKernel(1);
 BuildCompleteFilterKernel(1);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassGainRight;
end;

procedure TVTVSTModule.ParamHiGainLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BuildTrebleFilterKernel(0);
 BuildCompleteFilterKernel(0);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleGainLeft;
end;

procedure TVTVSTModule.ParamHiGainRightChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 BuildTrebleFilterKernel(1);
 BuildCompleteFilterKernel(1);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleGainRight;
end;

procedure TVTVSTModule.BuildEntireFilters;
begin
 BuildBassFilterKernel(0);
 BuildBassFilterKernel(1);
 BuildTrebleFilterKernel(0);
 BuildTrebleFilterKernel(1);
 BuildCompleteFilterKernel(0);
 BuildCompleteFilterKernel(1);
end;

procedure TVTVSTModule.ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  NewDriveMode : TDriveMode;
begin
 NewDriveMode := TDriveMode(Round(Value));

 if FDriveMode <> NewDriveMode then
  begin
   FDriveMode := NewDriveMode;
   BuildEntireFilters;
   if EditorForm is TFmVT then
    with TFmVT(EditorForm) do UpdateSelector;
  end;
end;

procedure TVTVSTModule.ParamChannelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
   1 : OnProcess := VSTModuleProcessStereo;
   2 : OnProcess := VSTModuleProcessMidSide;
  else OnProcess := nil;
 end;
 OnProcess32Replacing := OnProcess;

 // update GUI
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateChannel;
end;

procedure TVTVSTModule.ParamOutGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutGain := 1.25 * dB_to_Amp(Value);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateGain;
end;

procedure TVTVSTModule.VSTModuleProcessMidSide(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i    : Cardinal;
  M, S : Single;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHistoryBuffer[0, FBufferPos[0]] := Inputs[0, i] + Inputs[1, i];
    M := FOutGain * (FCircularBuffer[0, FBufferPos[0]] + FHistoryBuffer[0, FBufferPos[0]] * FFilterKernel[0]^[0]);
    FCircularBuffer[0, FBufferPos[0]] := 0;

    FHistoryBuffer[1, FBufferPos[1]] := Inputs[0, i] - Inputs[1, i];
    S := FOutGain * (FCircularBuffer[1, FBufferPos[1]] + FHistoryBuffer[1, FBufferPos[1]] * FFilterKernel[1]^[0]);
    FCircularBuffer[1, FBufferPos[1]] := 0;

    Outputs[0, i] := 0.5 * (M + S);
    Outputs[1, i] := 0.5 * (M - S);

    FConvolveIR(@FCircularBuffer[0, FBufferPos[0]], @FFilterKernel[0]^[0], KernelSize[0], FHistoryBuffer[0, FBufferPos[0]]);
    FConvolveIR(@FCircularBuffer[1, FBufferPos[1]], @FFilterKernel[1]^[0], KernelSize[1], FHistoryBuffer[1, FBufferPos[1]]);
    Inc(FBufferPos[0]);
    Inc(FBufferPos[1]);

    if FBufferPos[0] >= FKernelSize[0] then
     begin
      FBufferPos[0] := 0;
      Move(FCircularBuffer[0, FKernelSize[0]], FCircularBuffer[0, 0], FKernelSize[0] * SizeOf(Single));
      FillChar(FCircularBuffer[0, FKernelSize[0]], FKernelSize[0] * SizeOf(Single), 0);
     end;
    if FBufferPos[1] >= FKernelSize[1] then
     begin
      FBufferPos[1] := 0;
      Move(FCircularBuffer[1, FKernelSize[1]], FCircularBuffer[1, 0], FKernelSize[1] * SizeOf(Single));
      FillChar(FCircularBuffer[1, FKernelSize[1]], FKernelSize[1] * SizeOf(Single), 0);
     end;
   end;
 finally
  FCriticalSection.Leave
 end;
end;

procedure TVTVSTModule.VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i: Cardinal;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHistoryBuffer[0, FBufferPos[0]] := Inputs[0, i];
    Outputs[0, i] := FOutGain * (FCircularBuffer[0, FBufferPos[0]] + FHistoryBuffer[0, FBufferPos[0]] * FFilterKernel[0]^[0]);
    FCircularBuffer[0, FBufferPos[0]] := 0;

    FHistoryBuffer[1, FBufferPos[1]] := Inputs[1, i];
    Outputs[1, i] := FOutGain * (FCircularBuffer[1, FBufferPos[1]] + FHistoryBuffer[1, FBufferPos[1]] * FFilterKernel[1]^[0]);
    FCircularBuffer[1, FBufferPos[1]] := 0;

    FConvolveIR(@FCircularBuffer[0, FBufferPos[0]], @FFilterKernel[0]^[0], KernelSize[0], FHistoryBuffer[0, FBufferPos[0]]);
    FConvolveIR(@FCircularBuffer[1, FBufferPos[1]], @FFilterKernel[1]^[0], KernelSize[1], FHistoryBuffer[1, FBufferPos[1]]);
    Inc(FBufferPos[0]);
    Inc(FBufferPos[1]);

    if FBufferPos[0] >= FKernelSize[0] then
     begin
      FBufferPos[0] := 0;
      Move(FCircularBuffer[0, FKernelSize[0]], FCircularBuffer[0, 0], FKernelSize[0] * SizeOf(Single));
      FillChar(FCircularBuffer[0, FKernelSize[0]], FKernelSize[0] * SizeOf(Single), 0);
     end;
    if FBufferPos[1] >= FKernelSize[1] then
     begin
      FBufferPos[1] := 0;
      Move(FCircularBuffer[1, FKernelSize[1]], FCircularBuffer[1, 0], FKernelSize[1] * SizeOf(Single));
      FillChar(FCircularBuffer[1, FKernelSize[1]], FKernelSize[1] * SizeOf(Single), 0);
     end;
   end;
 finally
  FCriticalSection.Leave
 end;
end;

procedure TVTVSTModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate - 44100) > 4000
  then ShowMessage('Samplerates other than 44.1 kHz have not been implemented yet');
end;

procedure TVTVSTModule.ParameterBypassDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Boolean(Round(Parameter[Index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

end.
