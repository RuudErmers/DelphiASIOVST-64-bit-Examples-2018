unit EditorFrm;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Forms, Controls, StdCtrls, DAV_VSTModule;

type
  TEditorForm = class(TForm)
    LbAttack: TLabel;
    LbAttackValue: TLabel;
    LbRelease: TLabel;
    LbReleaseValue: TLabel;
    LbThreshold: TLabel;
    LbThresholdValue: TLabel;
    SBAttack: TScrollBar;
    SBRelease: TScrollBar;
    SBThreshold: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure SBThresholdChange(Sender: TObject);
    procedure SBAttackChange(Sender: TObject);
    procedure SBReleaseChange(Sender: TObject);
  public
    procedure UpdateThreshold;
    procedure UpdateAttack;
    procedure UpdateRelease;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  SimpleLimiterDM;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateThreshold;
 UpdateAttack;
 UpdateRelease;
end;

procedure TEditorForm.SBThresholdChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   if Parameter[0] <> SBThreshold.Position
    then Parameter[0] := SBThreshold.Position;
  end;
end;

procedure TEditorForm.SBAttackChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   if Parameter[1] <> SBAttack.Position
    then Parameter[1] := SBAttack.Position;
  end;
end;

procedure TEditorForm.SBReleaseChange(Sender: TObject);
begin
 with TSimpleLimiterDataModule(Owner) do
  begin
   if Parameter[2] <> SBRelease.Position
    then Parameter[2] := SBRelease.Position;
  end;
end;

procedure TEditorForm.UpdateThreshold;
begin
 with Owner as TSimpleLimiterDataModule do
  begin
   if Round(Parameter[0]) <> SBThreshold.Position
    then SBThreshold.Position := Round(Parameter[0]);
   LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
begin
 with Owner as TSimpleLimiterDataModule do
  begin
   if Round(Parameter[1]) <> SBAttack.Position
    then SBAttack.Position := Round(Parameter[1]);
   LbAttackValue.Caption := IntToStr(SBAttack.Position) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
begin
 with Owner as TSimpleLimiterDataModule do
  begin
   if Round(Parameter[2]) <> SBRelease.Position
    then SBRelease.Position := Round(Parameter[2]);
   LbReleaseValue.Caption := IntToStr(SBRelease.Position) + ' ms';
  end;
end;

end.
