unit ChunkDemoGUI;

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
  Forms, Controls, DAV_Types, DAV_VSTModule, DAV_GuiLabel, DAV_GuiPixelMap,
  DAV_GuiStitchedControls, DAV_GuiStitchedPngList, DAV_GuiStitchedDial;

type
  TFmChunkDemo = class(TForm)
    DialAlpha: TGuiStitchedDial;
    DialBeta: TGuiStitchedDial;
    DialDelta: TGuiStitchedDial;
    DialGamma: TGuiStitchedDial;
    GSPL: TGuiStitchedPNGList;
    LbAlpha: TGuiLabel;
    LbBeta: TGuiLabel;
    LbDelta: TGuiLabel;
    LbGamma: TGuiLabel;
    procedure FormShow(Sender: TObject);
    procedure DialAlphaChange(Sender: TObject);
    procedure DialBetaChange(Sender: TObject);
    procedure DialGammaChange(Sender: TObject);
    procedure DialDeltaChange(Sender: TObject);
  public
    procedure UpdateAlpha;
    procedure UpdateBeta;
    procedure UpdateGamma;
    procedure UpdateDelta;
  end;

implementation

uses
  ChunkDemoDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TFmChunkDemo }

procedure TFmChunkDemo.FormShow(Sender: TObject);
begin
 UpdateAlpha;
 UpdateBeta;
 UpdateGamma;
 UpdateDelta;
end;

procedure TFmChunkDemo.DialAlphaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[0] := DialAlpha.Value;
  end;
end;

procedure TFmChunkDemo.DialBetaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[1] := DialBeta.Value;
  end;
end;

procedure TFmChunkDemo.DialGammaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[2] := DialGamma.Value;
  end;
end;

procedure TFmChunkDemo.DialDeltaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[3] := DialDelta.Value;
  end;
end;

procedure TFmChunkDemo.UpdateAlpha;
var
  Alpha : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Alpha := Parameter[0];
   if DialAlpha.Value <> Alpha
    then DialAlpha.Value := Alpha;
  end;
end;

procedure TFmChunkDemo.UpdateBeta;
var
  Beta : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Beta := Parameter[1];
   if DialBeta.Value <> Beta
    then DialBeta.Value := Beta;
  end;
end;

procedure TFmChunkDemo.UpdateDelta;
var
  Delta : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Delta := Parameter[2];
   if DialGamma.Value <> Delta
    then DialGamma.Value := Delta;
  end;
end;

procedure TFmChunkDemo.UpdateGamma;
var
  Gamma : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Gamma := Parameter[3];
   if DialDelta.Value <> Gamma
    then DialDelta.Value := Gamma;
  end;
end;

end.
