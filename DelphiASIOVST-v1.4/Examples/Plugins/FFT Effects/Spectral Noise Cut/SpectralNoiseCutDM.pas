unit SpectralNoiseCutDM;

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

uses
  Windows, Messages, Classes, Forms, SyncObjs, DAV_Types, DAV_Complex,
  DAV_DspSpectralNoiseReduction, DAV_VSTModule;

type
  TSpectralNoiseCutModule = class(TVSTModule)
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection  : TCriticalSection;
    FSpectralNoiseCut : array of TSpectralNoiseCut32;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, Math, DAV_VSTModuleWithPrograms;

procedure TSpectralNoiseCutModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpectralNoiseCutModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSpectralNoiseCutModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);

 SetLength(FSpectralNoiseCut, numInputs);
 for Channel := 0 to Length(FSpectralNoiseCut) - 1 do
  begin
   FSpectralNoiseCut[Channel] := TSpectralNoiseCut32.Create;
   with FSpectralNoiseCut[Channel] do
    begin
     FftOrder := Round(Log2(BlockModeSize));
     Assert(FftSize = BlockModeSize);
    end;
  end;
  
 Parameter[0] := -30;
end;

procedure TSpectralNoiseCutModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseCut) - 1
  do FreeAndNil(FSpectralNoiseCut[Channel]);
end;

procedure TSpectralNoiseCutModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseCut) - 1
  do FSpectralNoiseCut[Channel].Threshold := Value;
end;

procedure TSpectralNoiseCutModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralNoiseCut) - 1 do
  begin
   FSpectralNoiseCut[Channel].ProcessBlock(@Inputs[Channel, 0],
     @Outputs[Channel, 0], SampleFrames);
  end;
end;

end.
