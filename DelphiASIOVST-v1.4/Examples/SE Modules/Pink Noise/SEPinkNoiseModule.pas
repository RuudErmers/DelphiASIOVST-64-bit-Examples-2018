unit SEPinkNoiseModule;

interface

uses
  DAV_Types, DAV_Complex, DAV_SECommon, DAV_SEModule;

type
  // define some constants to make referencing in/outs clearer
  TSEPinkNoisePins = (pinOutput);

  TSEPinkNoiseModule = class(TSEModuleBase)
  private
    FOutputBuffer : PDAVSingleFixedArray;
    FContribution : Array [0..4] of Double;
  protected
    procedure Open; override;
  public
    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  SysUtils;

procedure TSEPinkNoiseModule.Open;
begin
 inherited Open;

 FillChar(FContribution[0], Length(FContribution) * SizeOf(Double), 0);

 // choose which function is used to process audio
 OnProcess := SubProcess;

 // 'transmit' new output status to next module 'downstream'
 Pin[Integer(pinOutput)].TransmitStatusChange(SampleClock, stRun);
end;

// The most important part, processing the audio
procedure TSEPinkNoiseModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  ur1    : Double;
  Sample : Integer;
const
  pA   : Array [0..4] of Double = (0.23980, 0.18727, 0.1638, 0.194685, 0.214463);
  pSUM : Array [0..4] of Double = (0.00198, 0.01478, 0.06378, 0.23378, 0.91578);
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   ur1 := random;
   if (ur1 <= pSUM[0]) then FContribution[0] := (2 * random - 1) * pA[0] else
   if (ur1 <= pSUM[1]) then FContribution[1] := (2 * random - 1) * pA[1] else
   if (ur1 <= pSUM[2]) then FContribution[2] := (2 * random - 1) * pA[2] else
   if (ur1 <= pSUM[3]) then FContribution[3] := (2 * random - 1) * pA[3] else
   if (ur1 <= pSUM[4]) then FContribution[4] := (2 * random - 1) * pA[4];
   FOutputBuffer[BufferOffset + Sample] := (FContribution[0] +
     FContribution[1] + FContribution[2] + FContribution[3] + FContribution[4]);
  end;
end;

// describe your module
class procedure TSEPinkNoiseModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Pink Noise Generator';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'DAV Pink Noise Generator';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEPinkNoiseModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case TSEPinkNoisePins(index) of
  // typical output plug
  pinOutput: with Properties^ do
              begin
               Name            := 'Output';
               VariableAddress := @FOutputBuffer;
               Direction       := drOut;
               Datatype        := dtFSample;
//               Flags           := [iofAutoDuplicate, iofRename, iofCustomisable];
              end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;
end;

end.
