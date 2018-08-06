unit SampleDelayGui;

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

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_GuiBackgrounds;

type
  TFmSampleDelay = class(TForm)
    BrushedMetal: TGuiBackground;
    LbSamplesLeft: TLabel;
    SbSamplesLeft: TScrollBar;
    LbSamplesLeftValue: TLabel;
    LbSamplesRight: TLabel;
    LbSamplesRightValue: TLabel;
    SbSamplesRight: TScrollBar;
    CbLink: TCheckBox;
    CbMilliseconds: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SbSamplesLeftChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SbSamplesRightChange(Sender: TObject);
    procedure CbLinkClick(Sender: TObject);
  public
    procedure UpdateSamplesLeft;
    procedure UpdateSamplesRight;
  end;

implementation

uses
  SampleDelayDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSampleDelay.CbLinkClick(Sender: TObject);
begin
 UpdateSamplesLeft;
 UpdateSamplesRight;
end;

procedure TFmSampleDelay.FormCreate(Sender: TObject);
begin
// BrushedMetal.Active := True;
end;

procedure TFmSampleDelay.FormShow(Sender: TObject);
begin
 UpdateSamplesLeft;
 UpdateSamplesRight;
end;

procedure TFmSampleDelay.SbSamplesLeftChange(Sender: TObject);
begin
 with TSampleDelayDataModule(Owner) do
  begin
   if Round(Parameter[0]) <> SbSamplesLeft.Position
    then Parameter[0] := SbSamplesLeft.Position;
   if CbLink.Checked and (Round(Parameter[1]) <> SbSamplesLeft.Position)
    then Parameter[1] := SbSamplesLeft.Position;
  end;
end;

procedure TFmSampleDelay.SbSamplesRightChange(Sender: TObject);
begin
 with TSampleDelayDataModule(Owner) do
  begin
   if Round(Parameter[1]) <> SbSamplesRight.Position
    then Parameter[1] := SbSamplesRight.Position;
   if CbLink.Checked and (Round(Parameter[0]) <> SbSamplesRight.Position)
    then Parameter[0] := SbSamplesRight.Position;
  end;
end;

procedure TFmSampleDelay.UpdateSamplesLeft;
begin
 with TSampleDelayDataModule(Owner) do
  begin
   if SbSamplesLeft.Position <> Round(Parameter[0])
    then SbSamplesLeft.Position := Round(Parameter[0]);

   if CbMilliseconds.Checked
    then LbSamplesLeftValue.Caption := IntToStr(Round(1E3 * Parameter[1] / SampleRate)) + ' ms'
    else LbSamplesLeftValue.Caption := IntToStr(Round(Parameter[0])) + ' samples';
  end;
end;

procedure TFmSampleDelay.UpdateSamplesRight;
begin
 with TSampleDelayDataModule(Owner) do
  begin
   if SbSamplesRight.Position <> Round(Parameter[1])
    then SbSamplesRight.Position := Round(Parameter[1]);

   if CbMilliseconds.Checked
    then LbSamplesRightValue.Caption := IntToStr(Round(1E3 * Parameter[1] / SampleRate)) + ' ms'
    else LbSamplesRightValue.Caption := IntToStr(Round(Parameter[1])) + ' samples';
  end;
end;

end.
