unit RePsychoDM;

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
  Forms, DAV_Types, DAV_VSTModule;

type
  TRePsychoQuality = (rqLow, rqHigh);
  TRePsychoDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleProcessLowQuality(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessHighQuality(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterQualityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFineChanged(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTuneDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFineDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterHoldDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterQualityChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FWet       : Single;
    FDry       : Single;
    FEnvelope  : Single;
    FHold      : Single;
    FSize      : Integer;
    FBuffer    : array [0..1] of PDAVSingleFixedArray;
    FState     : array [0..1] of Single;
    FDelayTime : Integer;
    FGain      : Single;
    FThreshold : Single;
    FTune      : Single;
    FFineTune  : Single;
    FSemiTones : Integer;
    FSampleCnt : Integer;
    FQuality   : TRePsychoQuality;
    procedure QualityChanged;
  protected
    procedure AllocateBuffer; virtual;
    procedure ClearBuffer; virtual;
    procedure TuneChanged; virtual;
    procedure BufferSizeChanged; virtual;
    procedure CalculateBufferSize; virtual;
    procedure HoldChanged; virtual;
    procedure CalculateDelayTime; virtual; 
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_VSTCustomModule;

procedure TRePsychoDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSize      := Round(0.5 * SampleRate);
 FDelayTime := Round(0.01 * SampleRate + 0.5 * FSize * 0.45);
 FSampleCnt := FSize + 1;

 AllocateBuffer;
 ClearBuffer;

 // Initial Parameters
 Parameter[0] := 0;    // Tune
 Parameter[1] := 0;    // Fine Tune
 Parameter[2] := 0;    // Envelope
 Parameter[3] := 0.6;  // Threshold
 Parameter[4] := 0.45; // Minimum Chunk Length
 Parameter[5] := 100;  // Mix
 Parameter[6] := 0.4;  // Quality
end;

procedure TRePsychoDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuffer[0]) then Dispose(FBuffer[0]);
 if Assigned(FBuffer[1]) then Dispose(FBuffer[1]);
end;

procedure TRePsychoDataModule.CalculateBufferSize;
begin
 if FSize <> Round(0.5 * SampleRate) then
  begin
   FSize := Round(0.5 * SampleRate);
   BufferSizeChanged;
  end;
end;

procedure TRePsychoDataModule.BufferSizeChanged;
begin
 FSampleCnt  := FSize + 1;
 CalculateDelayTime;
 AllocateBuffer;
end;

procedure TRePsychoDataModule.AllocateBuffer;
begin
 ReallocMem(FBuffer[0], FSize * SizeOf(Single));
 ReallocMem(FBuffer[1], FSize * SizeOf(Single));
end;

procedure TRePsychoDataModule.ClearBuffer;
begin
 FillChar(FBuffer[0]^, FSize * SizeOf(Single), 0);
 FillChar(FBuffer[1]^, FSize * SizeOf(Single), 0);
end;

procedure TRePsychoDataModule.ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := (0.5 * sqrt(0.01 * Value));
 FDry := sqrt(1 - 0.01 * Value);
end;

procedure TRePsychoDataModule.ParameterDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0
  then FEnvelope := 1 + 0.003 * Power(Value * 0.02, 5)
  else FEnvelope := 1 + 0.025 * Power(Value * 0.02, 5);

// if Value > 0.5
//  then FEnvelope := (1.0 + 0.01 * (Value / 50))
//  else FEnvelope := (1.0 + 0.01 * (Value / 50));
end;

procedure TRePsychoDataModule.ParameterHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FHold <> Value then
  begin
   FHold := Value;
   HoldChanged;
  end;
end;

procedure TRePsychoDataModule.HoldChanged;
begin
 CalculateDelayTime;
end;

procedure TRePsychoDataModule.CalculateDelayTime;
begin
 FDelayTime := Round(0.01 * SampleRate + 0.5 * FSize * FHold);
end;

procedure TRePsychoDataModule.ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FThreshold := dB_to_Amp(Parameter[3]);
end;

procedure TRePsychoDataModule.ParameterTuneChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FSemiTones <> Round(Value) then
  begin
   FSemiTones := Round(Value);
   TuneChanged;
  end;
end;

procedure TRePsychoDataModule.ParameterTuneDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(FSemiTones);
end;

procedure TRePsychoDataModule.ParameterQualityChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FQuality <> TRePsychoQuality(Value > 0.5) then
  begin
   FQuality := TRePsychoQuality(Value > 0.5);
   QualityChanged;
  end;
end;

procedure TRePsychoDataModule.QualityChanged;
begin
 case FQuality of
  rqLow  : OnProcess := VSTModuleProcessLowQuality;
  rqHigh : OnProcess := VSTModuleProcessHighQuality;
  else raise Exception.Create('not yet defined');
 end;
 
 OnProcessReplacing := OnProcess;
end;

procedure TRePsychoDataModule.ParameterFineDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(FFineTune));
end;

procedure TRePsychoDataModule.ParameterHoldDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(1000.0 * FDelayTime / SampleRate));
end;

procedure TRePsychoDataModule.ParameterFineChanged(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FFineTune <> Value then
  begin
   FFineTune := Value;
   TuneChanged;
  end;
end;

procedure TRePsychoDataModule.ParameterQualityDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'HIGH'
  else PreDefined := 'LOW';
end;

procedure TRePsychoDataModule.TuneChanged;
begin
 FTune := (FSemiTones + 0.01 * FFineTune) / 24;
 FTune := Power(10, 0.60206 * FTune);
end;

procedure TRePsychoDataModule.VSTModuleProcessLowQuality(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Left   : Single;
  Right  : Single;
  Mono   : Single;
  Output : Single;
  State  : Single;
const
  COne80th : Single = 0.0125;
begin
 Output := 0;
 State := FState[0];
     
 for Sample := 0 to SampleFrames - 1 do
  begin
   Left  := Inputs[0, Sample];
   Right := Inputs[1, Sample];
   Mono  := Left + Right;

   // Process from here...

   // Trigger
   if ((Mono > FThreshold) and (FSampleCnt > FDelayTime)) then
    begin
     FGain := 1;
     FSampleCnt := 0;
    end;

   // Play buffer
   if FSampleCnt < FSize then
    begin
     // Check fade in
     if FSampleCnt < 80 then
      begin
       if FSampleCnt = 0 then State := Output;

       FBuffer[0, FSampleCnt] := Mono;
       Output := FBuffer[0, Round(FSampleCnt * FTune)];

       // Fade
       Output := (State  * (1 - (COne80th * FSampleCnt)) +
                 (Output * COne80th * FSampleCnt));
      end
     else
      begin
       //update to/from FBuffer[0]
       FBuffer[0, FSampleCnt] := Mono;
       Output := FBuffer[0, Round(FSampleCnt * FTune)];
      end;

     Inc(FSampleCnt);
     FGain := FGain * FEnvelope;
    end
   else FGain := 0; //mute

   // Output
   Outputs[0, Sample] := (Inputs[0, Sample] * FDry) + (Output * FGain * FWet);
   Outputs[1, Sample] := (Inputs[1, Sample] * FDry) + (Output * FGain * FWet);
  end;

 FState[0]  := State;
end;

procedure TRePsychoDataModule.VSTModuleProcessHighQuality(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample      : Integer;
  Left, Right : Single;
  Output      : array [0..1] of Single;
  it1, it2    : Single;
  of1, of2    : Integer;
const
  COne80th : Single = 0.0125;
begin
 Output[0] := 0;
 Output[1] := 0;

 FWet := 2 * FWet;
 for Sample := 0 to SampleFrames - 1 do
  begin
   Left := Inputs[0, Sample];
   Right := Inputs[1, Sample];

   //process from here...
   if ((Left + Right > FThreshold) and (FSampleCnt > FDelayTime)) then //trigger
    begin
     FGain := 1;
     FSampleCnt := 0;
    end;

    // Play Out
    if (FSampleCnt < FSize) then
     begin
      // Fade In
      if (FSampleCnt < 80) then
       begin
        if (FSampleCnt = 0) then
         begin
          FState[0] := Output[0];
          FState[1] := Output[1];
         end;

        FBuffer[0, FSampleCnt] := Left;
        FBuffer[1, FSampleCnt] := Right;

        Output[0] := FBuffer[0, Round(FSampleCnt * FTune)];
        Output[1] := FBuffer[1, Round(FSampleCnt * FTune)];

        Output[0] := (FState[0] * (1 - (COne80th * FSampleCnt)) + (Output[0] * COne80th * FSampleCnt));
        Output[1] := (FState[1] * (1 - (COne80th * FSampleCnt)) + (Output[1] * COne80th * FSampleCnt));
       end
      else
       begin
        // update to/from FBuffer[0]
        FBuffer[0, FSampleCnt] := Left;
        FBuffer[1, FSampleCnt] := Right;

        // Interpolation
        it1 := (FSampleCnt * FTune);
        of1 := Round(it1);
        of2 := of1 + 1;
        it1 := it1 - of1;
        it2 := (1 - it1);

        Output[0] := (it2 * FBuffer[0, of1]) + (it1 * FBuffer[0, of2]);
        Output[1] := (it2 * FBuffer[1, of1]) + (it1 * FBuffer[1, of2]);
       end;

      Inc(FSampleCnt);
      FGain := FGain * FEnvelope;
     end
    else FGain := 0; // Mute

    // Output
    Outputs[0, Sample] := (Left * FDry) + (Output[0] * FGain * FWet);
    Outputs[1, Sample] := (Right * FDry) + (Output[1] * FGain * FWet);
  end;
end;

procedure TRePsychoDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateBufferSize;
 ClearBuffer;
end;

procedure TRePsychoDataModule.VSTModuleSuspend(Sender: TObject);
begin
 ClearBuffer;
end;

end.
