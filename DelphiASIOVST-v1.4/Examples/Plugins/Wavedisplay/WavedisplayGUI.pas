unit WavedisplayGUI;

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
  Forms, Controls, ExtCtrls, StdCtrls, DAV_Types, DAV_VSTModule, 
  DAV_GuiBaseControl, DAV_GuiLevelMeter, DAV_GuiStaticWaveform, 
  DAV_GuiDynamicWaveform;

type
  TWavedisplayGUI = class(TForm)
    Display: TGuiDynamicWaveform;
    LbDrawMode: TLabel;
    LbWaveSize: TLabel;
    LbProcessingMode: TLabel;
    ddWaveSize: TComboBox;
    ddDrawMode: TComboBox;
    ddProcessing: TComboBox;
    LevelMeter: TGuiLevelMeter;
    procedure ddProcessingChange(Sender: TObject);
    procedure ddWaveSizeChange(Sender: TObject);
    procedure ddDrawModeChange(Sender: TObject);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TWavedisplayGUI.ddProcessingChange(Sender: TObject);
begin
 with Display do
  case ddProcessing.ItemIndex of
   0: WaveProcessMode := wpmScroll;
   1: WaveProcessMode := wpmReplace;
   2: WaveProcessMode := wpmStretch;
  end;
end;

procedure TWavedisplayGUI.ddWaveSizeChange(Sender: TObject);
begin
 with Display do
  case ddWaveSize.ItemIndex of
   0: InternalBufferSize := 256;
   1: InternalBufferSize := 512;
   2: InternalBufferSize := 1024;
   3: InternalBufferSize := 2048;
   4: InternalBufferSize := 4096;
   5: InternalBufferSize := 8192;
  end;
end;

procedure TWavedisplayGUI.ddDrawModeChange(Sender: TObject);
begin
 with Display do
  case ddDrawMode.ItemIndex of
   0: WaveDrawMode := wdmSolid;
   1: WaveDrawMode := wdmOutline;
   2: WaveDrawMode := wdmPoints;
   3: WaveDrawMode := wdmSimple;
  end;
end;

end.
