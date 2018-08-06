unit MaxxBassCloneGUI;

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

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiLevelMeter,
  DAV_GuiSlider, DAV_GuiButton, DAV_GuiGraphicControl, DAV_GuiEQGraph,
  DAV_GuiImageControl, DAV_GuiPngList, DAV_GuiCustomControl, DAV_GuiFader;

type
  TFmHarmonicBassClone = class(TForm)
    OutputMeterLeft: TGuiColorLevelMeter;
    OutputMeterRight: TGuiColorLevelMeter;
    EqGraph: TGuiEQGraph;
    LbOutput: TGuiLabel;
    LbAudio: TGuiButton;
    LbMaxxBass: TGuiButton;
    LbOriginalBass: TGuiButton;
    LbClipIndicator: TGuiButton;
    GuiLabel2: TGuiLabel;
    SBFrequency: TGuiSlider;
    GuiButton1: TGuiButton;
    GuiButton2: TGuiButton;
    GuiButton3: TGuiButton;
    GuiButton4: TGuiButton;
    GuiButton5: TGuiButton;
    GuiButton6: TGuiButton;
    GuiButton7: TGuiButton;
    GuiButton8: TGuiButton;
    GuiButton9: TGuiButton;
    GuiFader1: TGuiFader;
    GuiPNGList: TGuiPNGList;
    GuiFader2: TGuiFader;
    GuiFader3: TGuiFader;
    GuiLabel1: TGuiLabel;
    GuiLabel3: TGuiLabel;
    GuiLabel4: TGuiLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmHarmonicBassClone: TFmHarmonicBassClone;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

end.
