unit SETanhAproximationsModule;

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
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSETanhAproximationsModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FTerms        : Integer;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcessOpt3asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt4asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt5asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt6asm(const BufferOffset, SampleFrames: Integer);
    procedure SubProcessOpt7asm(const BufferOffset, SampleFrames: Integer);
  end;

  TSETanhAproxModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    FTerms        : Integer;
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer); override;

    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess2a(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess2b(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess2c(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess2d(const BufferOffset, SampleFrames: Integer);
  end;

  TSETanhModule = class(TSEModuleBase)
  protected
    FInput1Buffer : PDAVSingleFixedArray; // pointer to circular buffer of samples
    FOutputBuffer : PDAVSingleFixedArray;
    procedure Open; override;
  public
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  Math, SysUtils, DAV_Approximations;

constructor TSETanhAproximationsModule.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FTerms := 3;
end;

procedure TSETanhAproximationsModule.Open;
begin
 // choose which function is used to process audio
 OnProcess := SubProcessOpt3asm;

 inherited Open;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

procedure TSETanhAproximationsModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;
 if CurrentPin.PinID = 2 then
  begin
   case FTerms of
    3 : OnProcess := SubProcessOpt3asm;
    4 : OnProcess := SubProcessOpt4asm;
    5 : OnProcess := SubProcessOpt5asm;
    6 : OnProcess := SubProcessOpt6asm;
    7 : OnProcess := SubProcessOpt7asm;
   end;
  end;
end;

// The most important part, processing the audio
procedure TSETanhAproximationsModule.SubProcessOpt3asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 {$IFDEF PUREPASCAL}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt3Term(Input[Sample]);
 {$ELSE}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt3TermFPU(Input[Sample]);
 {$ENDIF}
end;

procedure TSETanhAproximationsModule.SubProcessOpt4asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 {$IFDEF PUREPASCAL}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt4Term(Input[Sample]);
 {$ELSE}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt4TermFPU(Input[Sample]);
 {$ENDIF}
end;

procedure TSETanhAproximationsModule.SubProcessOpt5asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 {$IFDEF PUREPASCAL}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt5Term(Input[Sample]);
 {$ELSE}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt5TermFPU(Input[Sample]);
 {$ENDIF}
end;

procedure TSETanhAproximationsModule.SubProcessOpt6asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 {$IFDEF PUREPASCAL}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt6Term(Input[Sample]);
 {$ELSE}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt6TermFPU(Input[Sample]);
 {$ENDIF}
end;

procedure TSETanhAproximationsModule.SubProcessOpt7asm(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 {$IFDEF PUREPASCAL}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt7Term(Input[Sample]);
 {$ELSE}
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanhOpt7TermFPU(Input[Sample]);
 {$ENDIF}
end;

// describe your module
class procedure TSETanhAproximationsModule.GetModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Tanh Aproximations';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Tanh Aproximations';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSETanhAproximationsModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInput1Buffer;
       Direction       := drIn;
       Datatype        := dtFSAMPLE;
       DefaultValue    := '0';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;

  // parameter
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drParameter;
       Datatype        := dtEnum;
       DatatypeExtra   := 'range 3,7';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

{ TSETanhAproxModule }

constructor TSETanhAproxModule.Create(AudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited;
 FTerms := 3;
end;

procedure TSETanhAproxModule.Open;
begin
 // choose which function is used to process audio
 OnProcess := SubProcess2d;

 inherited Open;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

class procedure TSETanhAproxModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Tanh Aprox.';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Tanh Aprox.';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

function TSETanhAproxModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInput1Buffer;
       Direction       := drIn;
       Datatype        := dtFSAMPLE;
       DefaultValue    := '0';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;

  // parameter
  2: with Properties^ do
      begin
       Name            := 'Terms';
       VariableAddress := @FTerms;
       Direction       := drParameter;
       Datatype        := dtEnum;
       DatatypeExtra   := 'range 3,6';
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

procedure TSETanhAproxModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 if CurrentPin.PinID = 2 then
  begin
   case FTerms of
    3 : OnProcess := SubProcess2d;
    4 : OnProcess := SubProcess2c;
    5 : OnProcess := SubProcess2b;
    6 : OnProcess := SubProcess2a;
   end;
  end;
 inherited;
end;

procedure TSETanhAproxModule.SubProcess2a(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanh2Like1Term(Input[Sample]);
end;

procedure TSETanhAproxModule.SubProcess2b(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanh2Like2Term(Input[Sample]);
end;

procedure TSETanhAproxModule.SubProcess2c(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanh2Like3Term(Input[Sample]);
end;

procedure TSETanhAproxModule.SubProcess2d(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := FastTanh2Like4Term(Input[Sample]);
end;

{ TSETanhModule }

procedure TSETanhModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // let 'downstream' modules know audio data is coming
 Pin[1].TransmitStatusChange(SampleClock, stRun);
end;

class procedure TSETanhModule.GetModuleProperties(Properties: PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Tanh';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Tanh';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   SDKVersion := CSeSdkVersion;
  end;
end;

function TSETanhModule.GetPinProperties(const Index: Integer;
  Properties: PSEPinProperties): Boolean;
begin  
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  0: with Properties^ do
      begin
       Name            := 'Input';
       VariableAddress := @FInput1Buffer;
       Direction       := drIn;
       Datatype        := dtFSAMPLE;
       DefaultValue    := '0';
      end;

  // typical output plug
  1: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

procedure TSETanhModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Input  : PDAVSingleFixedArray;
  Output : PDAVSingleFixedArray;
  Sample : Integer;
begin
 Input  := PDAVSingleFixedArray(@FInput1Buffer[BufferOffset]);
 Output := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1
  do Output^[Sample] := Tanh(Input[Sample]);
end;

end.
