unit DAV_AsioRegister;

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
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde, based on a code snipped by Frederic Vanmol            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_AsioHostRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes,
  {$IFDEF DELPHI7_UP} DesignIntf, {$ENDIF}
  DAV_ASIOHost, DAV_ASIOHostAudioData, DAV_ASIOGenerator;

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TASIOHost, TASIOHostBasic,
   TASIOHostAudioData, TASIOGeneratorNoise, TASIOGeneratorTone]);
 {$IFDEF COMPILER5}
 RegisterComponentEditor(TASIOHost, TASIOControlPanel);
 {$ENDIF}

 {$IFDEF DELPHI7_UP}
 RegisterPropertiesInCategory('Buffer Information', TCustomASIOHostBasic,
    ['BufferGranularity', 'BufferMaximum', 'BufferMinimum',
     'BufferPreferredSize', 'BufferSize']);
 RegisterPropertiesInCategory('Input', TCustomASIOHostBasic,
    ['InputChannelCount', 'InputChannelInfo', 'InputLatency', 'InputMeter']);
 {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_AsioHostRegister.lrs}
{$ENDIF}

end.
