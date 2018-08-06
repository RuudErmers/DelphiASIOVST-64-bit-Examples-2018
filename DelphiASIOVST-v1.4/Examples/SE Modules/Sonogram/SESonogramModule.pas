unit SESonogramModule;

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

uses
  SyncObjs, DAV_Types, DAV_SECommon, DAV_SEModule, DAV_Sonogram,
  DAV_DspWindowFunctions; // DAV_DspWindowFunctionsAdvanced;

type
  TSESonogramModule = class(TSEModuleBase)
  private
    FDirection           : Integer;
    FSonogram            : TBitmapSonogram32;
    FInputBuffer         : PDAVSingleFixedArray;
    FStaticCount         : Integer;
    FFFTOrder            : Integer;
    FUpperFrequency      : Single;
    FLowerFrequency      : Single;
    FMinimumLevel        : Single;
    FMaximumLevel        : Single;
    FLogarithmic         : Boolean;
    FOverlapFactor       : Integer;
    FWindowFunctionIndex : Integer;
    procedure SubProcessStatic(const BufferOffset, SampleFrames: Integer);
  protected
    procedure Open; override;
    procedure ChooseProcess; virtual;

    procedure PlugStateChange(const CurrentPin: TSEPin); override;
    procedure Close; override;
  public
    function GetPinProperties(const Index: Integer; Properties : PSEPinProperties): Boolean; override;
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;

    procedure SubProcessSleep(const BufferOffset, SampleFrames: Integer);
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

var
  CriticalSection : TCriticalSection;

implementation

uses
  SysUtils;

procedure TSESonogramModule.Open;
begin
 inherited Open;

 FSonogram := TBitmapSonogram32.Create;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TSESonogramModule.Close;
begin
 if Assigned(FSonogram)
  then FreeAndNil(FSonogram);
 inherited;
end;

// The most important part, processing the audio
procedure TSESonogramModule.SubProcessStatic(const BufferOffset, SampleFrames: Integer);
begin
 SubProcess(BufferOffset, SampleFrames);
 FStaticCount := FStaticCount - SampleFrames;
 if FStaticCount <= 0
  then CallHost(SEAudioMasterSleepMode);
end;

procedure TSESonogramModule.SubProcess(const BufferOffset,
  SampleFrames: Integer);
begin
 CriticalSection.Enter;
 try
  FSonogram.ProcessBlock32(PDAVSingleFixedArray(@FInputBuffer[BufferOffset]), SampleFrames);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TSESonogramModule.SubProcessSleep(const BufferOffset, SampleFrames: Integer);
begin
 CallHost(SEAudioMasterSleepMode);
end;

procedure TSESonogramModule.ChooseProcess;
begin
 if Pin[1].Status = stRun
  then OnProcess := SubProcess
  else
   begin
    FStaticCount := BlockSize;
    OnProcess := SubProcessStatic;
   end;
end;

// describe your module
class procedure TSESonogramModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Sonogram';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Sonogram';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';

   Flags      := [];
   GuiFlags   := [gfControlView, gfStructureView];
   SdkVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSESonogramModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
   0 : with Properties^ do
        begin
         Name            := 'Internal Sonogram Reference (DSP)';
         VariableAddress := @FSonogram;
         Direction       := drOut;
         Datatype        := dtInteger;
         Flags           := [iofPatchStore, iofHidePin];
        end;
   1 : with Properties^ do
        begin
         Name            := 'Internal Sonogram Reference (GUI)';
         Direction       := drIn;
         Datatype        := dtInteger;
         Flags           := [iofUICommunication, iofPatchStore, iofHidePin];
        end;
   2 : with Properties^ do
        begin
         Name            := 'Input';
         VariableAddress := @FInputBuffer;
         Direction       := drIn;
         Datatype        := dtFSample;
         DefaultValue    := '0';
        end;
   3 : with Properties^ do
        begin
         Name            := 'FFTOrder';
         VariableAddress := @FFFTOrder;
         Direction       := drIn;
         Datatype        := dtEnum;
         DatatypeExtra   := 'range 6,13';
         DefaultValue    := '10';
        end;
   4 : with Properties^ do
        begin
         Name            := 'Overlap Factor';
         VariableAddress := @FOverlapFactor;
         Direction       := drIn;
         Datatype        := dtEnum;
         DatatypeExtra   := 'range 1,512';
         DefaultValue    := '8';
        end;
   5 : with Properties^ do
        begin
         Name            := 'Window Function';
         VariableAddress := @FWindowFunctionIndex;
         Direction       := drIn;
         Datatype        := dtEnum;
         DatatypeExtra   := 'Rectangle, Triangle, Hanning, Hamming, Blackman, Lanczos, Welch';
         DefaultValue    := 'Blackman';
        end;
   6 : with Properties^ do
        begin
         Name            := 'Logarithmic';
         VariableAddress := @FLogarithmic;
         Direction       := drIn;
         Datatype        := dtBoolean;
         DefaultValue    := 'True';
        end;
   7 : with Properties^ do
        begin
         Name            := 'Lower Frequency';
         VariableAddress := @FLowerFrequency;
         Direction       := drIn;
         Datatype        := dtSingle;
         DefaultValue    := '20';
        end;
   8 : with Properties^ do
        begin
         Name            := 'Upper Frequency';
         VariableAddress := @FUpperFrequency;
         Direction       := drIn;
         Datatype        := dtSingle;
         DefaultValue    := '20000';
        end;
   9 : with Properties^ do
        begin
         Name            := 'Minimum Level';
         VariableAddress := @FMinimumLevel;
         Direction       := drIn;
         Datatype        := dtSingle;
         DefaultValue    := '-90';
        end;
  10 : with Properties^ do
        begin
         Name            := 'Maximum Level';
         VariableAddress := @FMaximumLevel;
         Direction       := drIn;
         Datatype        := dtSingle;
         DefaultValue    := '6';
        end;

  // typical input plug (inputs are listed first)
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

// An input plug has changed value
procedure TSESonogramModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 inherited;

 case CurrentPin.PinID of
   1 : begin
        Pin[0].TransmitStatusChange(SampleClock, stStatic);
        ChooseProcess;
       end;
   2 : begin
        CriticalSection.Enter;
        try
         FSonogram.FFTOrder := FFFTOrder;
        finally
         CriticalSection.Leave;
        end;
       end;
   3 : begin
        CriticalSection.Enter;
        try
         FSonogram.OverlapFactor := FOverlapFactor;
        finally
         CriticalSection.Leave;
        end;
       end;
   4 : begin
        CriticalSection.Enter;
        try
         FSonogram.WindowClass := GWindowFunctions[FWindowFunctionIndex];
        finally
         CriticalSection.Leave;
        end;
       end;
   5 : begin
        CriticalSection.Enter;
        try
         FSonogram.Logarithmic := FLogarithmic;
        finally
         CriticalSection.Leave;
        end;
       end;
   6 : begin
        CriticalSection.Enter;
        try
         FSonogram.LowerFrequency := FLowerFrequency;
        finally
         CriticalSection.Leave;
        end;
       end;
   7 : begin
        CriticalSection.Enter;
        try
         FSonogram.UpperFrequency := FUpperFrequency;
        finally
         CriticalSection.Leave;
        end;
       end;
   8 : begin
        CriticalSection.Enter;
        try
         FSonogram.MinimumLevel := FMinimumLevel;
        finally
         CriticalSection.Leave;
        end;
       end;
   9 : begin
        CriticalSection.Enter;
        try
         FSonogram.MaximumLevel := FMaximumLevel;
        finally
         CriticalSection.Leave;
        end;
       end;
 end;
end;

initialization
  CriticalSection := TCriticalSection.Create;

finalization
  FreeAndNil(CriticalSection);  

end.
