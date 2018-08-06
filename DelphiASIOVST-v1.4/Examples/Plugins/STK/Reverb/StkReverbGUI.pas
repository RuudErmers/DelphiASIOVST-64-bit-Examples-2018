unit StkReverbGUI;

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
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule;

type
  TFmStkReverb = class(TForm)
    LbT60: TLabel;
    SBT60: TScrollBar;
    LbT60Value: TLabel;
    procedure SBT60Change(Sender: TObject);
  public
    procedure UpdateT60;  
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkReverbDM;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmStkReverb.SBT60Change(Sender: TObject);
begin
 with TStkReverbModule(Owner) do
  begin
   Parameter[0] := SBT60.Position;
  end;
end;

procedure TFmStkReverb.UpdateT60;
begin
 with TStkReverbModule(Owner) do
  begin
   if SBT60.Position <> Round(Parameter[0])
    then SBT60.Position := Round(Parameter[0]);
   LbT60Value.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';  
  end;
end;

end.
