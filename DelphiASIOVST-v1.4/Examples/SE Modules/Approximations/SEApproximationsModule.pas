unit SEApproximationsModule;

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
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule, DAV_Approximations;

type
  TSEApproximationsModuleClass = class of TSEModuleBase;
  TSEApproximationsModule = class(TSEModuleBase)
  protected
    procedure Open; override;
  public
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
    class procedure GetModuleProperties(Properties: PSEModuleProperties); override;
  end;

  TSEFastSineApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastSineApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastCosineApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastCosineApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastTangensApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastTangensApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastCotangensApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastCotangensApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastArcTanApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastArcTanApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastArcCotanApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastArcCotanApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastLog2ApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastLog2ApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastPower2ApproximationsSingleModule = class(TSEApproximationsModule)
  protected
    FFloatIn  : Single;
    FFloatOut : Single;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;

  TSEFastPower2ApproximationsDoubleModule = class(TSEApproximationsModule)
  protected
    FDoubleIn  : Double;
    FDoubleOut : Double;
    FTerms    : Integer;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
  end;



implementation

uses
  Math, SysUtils, DAV_Common;

{ TSEApproximationsModule }

class procedure TSEApproximationsModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

procedure TSEApproximationsModule.Open;
begin
 inherited Open;

 OnProcess := SubProcessStatic;

 // 'transmit' new output status to next module 'downstream'
 if Assigned(Pin[0])
  then Pin[0].TransmitStatusChange(SampleClock, stStatic);
 if Assigned(Pin[1])
  then Pin[1].TransmitStatusChange(SampleClock, stStatic);
end;

// The most important part, processing the audio
procedure TSEApproximationsModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;



{ TSEFastSineApproximationsSingleModule }

class procedure TSEFastSineApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Sine Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Sine Approximations Single';
  end;
end;

function TSEFastSineApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 3, 7';
      end;
  else Result := False; // host will ask for plugs 0, 1, 2, 3 etc. return false to signal when done
 end;
end;

procedure TSEFastSineApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FFloatOut := FastSin3Term(FFloatIn);
   4 : FFloatOut := FastSin4Term(FFloatIn);
   5 : FFloatOut := FastSin5Term(FFloatIn);
   6 : FFloatOut := FastSin6Term(FFloatIn);
   7 : FFloatOut := FastSin7Term(FFloatIn);
  end;
end;


{ TSEFastSineApproximationsDoubleModule }

class procedure TSEFastSineApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Sine Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Sine Approximations Double';
  end;
end;

function TSEFastSineApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 3, 7';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastSineApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FDoubleOut := FastSin3Term(FDoubleIn);
   4 : FDoubleOut := FastSin4Term(FDoubleIn);
   5 : FDoubleOut := FastSin5Term(FDoubleIn);
   6 : FDoubleOut := FastSin6Term(FDoubleIn);
   7 : FDoubleOut := FastSin7Term(FDoubleIn);
  end;
end;


{ TSEFastCosineApproximationsSingleModule }

class procedure TSEFastCosineApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Cosine Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Cosine Approximations Single';
  end;
end;

function TSEFastCosineApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 3, 7';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastCosineApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FFloatOut := FastCos3Term(FFloatIn);
   4 : FFloatOut := FastCos4Term(FFloatIn);
   5 : FFloatOut := FastCos5Term(FFloatIn);
   6 : FFloatOut := FastCos6Term(FFloatIn);
   7 : FFloatOut := FastCos7Term(FFloatIn);
  end;
end;


{ TSEFastCosineApproximationsDoubleModule }

class procedure TSEFastCosineApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Cosine Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Cosine Approximations Double';
  end;
end;

function TSEFastCosineApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 3, 7';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastCosineApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FDoubleOut := FastCos3Term(FDoubleIn);
   4 : FDoubleOut := FastCos4Term(FDoubleIn);
   5 : FDoubleOut := FastCos5Term(FDoubleIn);
   6 : FDoubleOut := FastCos6Term(FDoubleIn);
   7 : FDoubleOut := FastCos7Term(FDoubleIn);
  end;
end;


{ TSEFastTangensApproximationsSingleModule }

class procedure TSEFastTangensApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Tangens Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Tangens Approximations Single';
  end;
end;

function TSEFastTangensApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '2, 3, 4, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastTangensApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FFloatOut := FastTan2Term(FFloatIn);
   3 : FFloatOut := FastTan3Term(FFloatIn);
   4 : FFloatOut := FastTan4Term(FFloatIn);
   6 : FFloatOut := FastTan6Term(FFloatIn);
  end;
end;


{ TSEFastTangensApproximationsDoubleModule }

class procedure TSEFastTangensApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Tangens Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Tangens Approximations Double';
  end;
end;

function TSEFastTangensApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '2, 3, 4, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastTangensApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FDoubleOut := FastTan2Term(FDoubleIn);
   3 : FDoubleOut := FastTan3Term(FDoubleIn);
   4 : FDoubleOut := FastTan4Term(FDoubleIn);
   6 : FDoubleOut := FastTan6Term(FDoubleIn);
  end;
end;


{ TSEFastCotangensApproximationsSingleModule }

class procedure TSEFastCotangensApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Cotangens Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Cotangens Approximations Single';
  end;
end;

function TSEFastCotangensApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '2, 3, 4, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastCotangensApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FFloatOut := FastCotan2Term(FFloatIn);
   3 : FFloatOut := FastCotan3Term(FFloatIn);
   4 : FFloatOut := FastCotan4Term(FFloatIn);
   6 : FFloatOut := FastCotan6Term(FFloatIn);
  end;
end;


{ TSEFastCotangensApproximationsDoubleModule }

class procedure TSEFastCotangensApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Cotangens Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Cotangens Approximations Double';
  end;
end;

function TSEFastCotangensApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '2, 3, 4, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastCotangensApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FDoubleOut := FastCotan2Term(FDoubleIn);
   3 : FDoubleOut := FastCotan3Term(FDoubleIn);
   4 : FDoubleOut := FastCotan4Term(FDoubleIn);
   6 : FDoubleOut := FastCotan6Term(FDoubleIn);
  end;
end;


{ TSEFastArcTanApproximationsSingleModule }

class procedure TSEFastArcTanApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast ArcTan Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast ArcTan Approximations Single';
  end;
end;

function TSEFastArcTanApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '3, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastArcTanApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FFloatOut := FastArcTan3Term(FFloatIn);
   6 : FFloatOut := FastArcTan6Term(FFloatIn);
  end;
end;


{ TSEFastArcTanApproximationsDoubleModule }

class procedure TSEFastArcTanApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast ArcTan Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast ArcTan Approximations Double';
  end;
end;

function TSEFastArcTanApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '3, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastArcTanApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FDoubleOut := FastArcTan3Term(FDoubleIn);
   6 : FDoubleOut := FastArcTan6Term(FDoubleIn);
  end;
end;


{ TSEFastArcCotanApproximationsSingleModule }

class procedure TSEFastArcCotanApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast ArcCotan Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast ArcCotan Approximations Single';
  end;
end;

function TSEFastArcCotanApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '3, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastArcCotanApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FFloatOut := FastArcCotan3Term(FFloatIn);
   6 : FFloatOut := FastArcCotan6Term(FFloatIn);
  end;
end;


{ TSEFastArcCotanApproximationsDoubleModule }

class procedure TSEFastArcCotanApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast ArcCotan Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast ArcCotan Approximations Double';
  end;
end;

function TSEFastArcCotanApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := '3, 6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastArcCotanApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   3 : FDoubleOut := FastArcCotan3Term(FDoubleIn);
   6 : FDoubleOut := FastArcCotan6Term(FDoubleIn);
  end;
end;


{ TSEFastLog2ApproximationsSingleModule }

class procedure TSEFastLog2ApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Log2 Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Log2 Approximations Single';
  end;
end;

function TSEFastLog2ApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 2, 5';
      end;
  else Result := False; // host will ask for plugs 0, 1, 2, 3 etc. return false to signal when done
 end;
end;

procedure TSEFastLog2ApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FFloatOut := FastLog2MinError2(FFloatIn);
   3 : FFloatOut := FastLog2MinError3(FFloatIn);
   4 : FFloatOut := FastLog2MinError4(FFloatIn);
   5 : FFloatOut := FastLog2MinError5(FFloatIn);
  end;
end;


{ TSEFastLog2ApproximationsDoubleModule }

class procedure TSEFastLog2ApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Log2 Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Log2 Approximations Double';
  end;
end;

function TSEFastLog2ApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 2, 5';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastLog2ApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FDoubleOut := FastLog2MinError2(FDoubleIn);
   3 : FDoubleOut := FastLog2MinError3(FDoubleIn);
   4 : FDoubleOut := FastLog2MinError4(FDoubleIn);
   5 : FDoubleOut := FastLog2MinError5(FDoubleIn);
  end;
end;


{ TSEFastPower2ApproximationsSingleModule }

class procedure TSEFastPower2ApproximationsSingleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Power2 Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Power2 Approximations Single';
  end;
end;

function TSEFastPower2ApproximationsSingleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FFloatIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FFloatOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 2, 5';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastPower2ApproximationsSingleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FFloatOut := FastPower2MinError2(FFloatIn);
   3 : FFloatOut := FastPower2MinError3(FFloatIn);
   4 : FFloatOut := FastPower2MinError4(FFloatIn);
   5 : FFloatOut := FastPower2MinError5(FFloatIn);
  end;
end;


{ TSEFastPower2ApproximationsDoubleModule }

class procedure TSEFastPower2ApproximationsDoubleModule.GetModuleProperties(
  Properties: PSEModuleProperties);
begin
 inherited;
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fast Power2 Approximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Fast Power2 Approximations Double';
  end;
end;

function TSEFastPower2ApproximationsDoubleModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'In';
       VariableAddress := @FDoubleIn;
       Direction       := drIn;
       Datatype        := dtSingle;
       DefaultValue    := '0';
      end;
  1: with Properties^ do
      begin
       Name            := 'Out';
       VariableAddress := @FDoubleOut;
       Direction       := drOut;
       Datatype        := dtSingle;
       DefaultValue    := '1';
      end;
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '3';
       DatatypeExtra   := 'range 2, 5';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSEFastPower2ApproximationsDoubleModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 0 then
  case FTerms of
   2 : FDoubleOut := FastPower2MinError2(FDoubleIn);
   3 : FDoubleOut := FastPower2MinError3(FDoubleIn);
   4 : FDoubleOut := FastPower2MinError4(FDoubleIn);
   5 : FDoubleOut := FastPower2MinError5(FDoubleIn);
  end;
end;

end.
