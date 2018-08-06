unit SEFibonacciModule;

interface

uses
  DAV_Types, DAV_SECommon, DAV_SEModule;

type
  TSEFibonacciModule = class(TSEModuleBase)
  private
    FOutputBuffer : PDAVSingleFixedArray;
    FLastState    : Single;
    FOrder        : Integer;
    FDownsample   : Integer;
    FFiboLength   : Integer;
    FFiboIndex    : Integer;
    FFiboStates   : Array [0..1] of Integer;    
    FSqr5, FA, fB : Double;
    procedure OrderChanged;
  protected
    procedure Open; override;
    procedure PlugStateChange(const CurrentPin: TSEPin); override;
  public
    constructor Create(SEAudioMaster: TSE2audioMasterCallback; Reserved: Pointer); override;
    destructor Destroy; override;

    class procedure GetModuleProperties(Properties : PSEModuleProperties); override;
    function GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean; override;
    procedure SubProcess(const BufferOffset, SampleFrames: Integer);
  end;

implementation

uses
  Math;

constructor TSEFibonacciModule.Create(SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer);
begin
 inherited Create(SEAudioMaster, Reserved);
 fSqr5          := Sqrt(5);
 fA             := (1 + fSqr5) * 0.5;
 fB             := (1 - fSqr5) * 0.5;
 FOrder         := 8;
 FDownsample    := 1;
 OrderChanged;
 FFiboIndex     := 0;
 FFiboStates[0] := 1;
 FFiboStates[1] := 1;
end;

destructor TSEFibonacciModule.Destroy;
begin
 // This is where you free any memory/resources your module has created
 inherited;
end;

procedure TSEFibonacciModule.Open;
begin
 inherited Open;

 // choose which function is used to process audio
 OnProcess := SubProcess;
end;

procedure TSEFibonacciModule.OrderChanged;
begin
 FFiboLength := FDownsample * Round((Power(FA, FOrder) - Power(FB, FOrder)) / FSqr5);
end;

procedure TSEFibonacciModule.PlugStateChange(const CurrentPin: TSEPin);
begin
 case CurrentPin.PinID of
  1 : begin
       OrderChanged;
       if FFiboIndex > FFiboLength then
        begin
         FFiboIndex     := 0;
         FFiboStates[0] := 1;
         FFiboStates[1] := 1;
        end;
      end;
 end;
 inherited;
end;

// The most important part, processing the audio
procedure TSEFibonacciModule.SubProcess(const BufferOffset, SampleFrames: Integer);
var
  Out1   : PDAVSingleFixedArray;
  Sample : Integer;
  spd    : Double;
begin
 Out1 := PDAVSingleFixedArray(@FOutputBuffer[BufferOffset]);
 for Sample := 0 to SampleFrames - 1 do
  begin
   if (FFiboIndex mod FDownsample) = 0 then
    begin
     spd := FFiboStates[1] / FFiboStates[0];
     if spd * spd > spd + 1 then
      begin
       FLastState := -1;
       Inc(FFiboStates[0]);
      end
     else
      begin
       FLastState := 1;
       Inc(FFiboStates[1]);
      end;
    end;

   Out1^[Sample] := FLastState;
   inc(FFiboIndex);
   if FFiboIndex >= FFiboLength then
    begin
     FFiboIndex     := 0;
     FFiboStates[0] := 1;
     FFiboStates[1] := 1;
    end;
  end;
end;

// describe your module
class procedure TSEFibonacciModule.getModuleProperties(Properties : PSEModuleProperties);
begin
 with Properties^ do
  begin
   // describe the plugin, this is the name the end-user will see.
   Name := 'Fibonacci Example';

   // return a unique string 32 characters max
   // if posible include manufacturer and plugin identity
   // this is used internally by SE to identify the plug.
   // No two plugs may have the same id.
   ID := 'Synthedit Fibonacci Example';

   // Info, may include Author, Web page whatever
   About := 'by Christian-W. Budde';
   SDKVersion := CSeSdkVersion;
  end;
end;

// describe the pins (plugs)
function TSEFibonacciModule.GetPinProperties(const Index: Integer; Properties: PSEPinProperties): Boolean;
begin
 Result := True;
 case Index of
  // typical input plug (inputs are listed first)
  // typical output plug
  0: with Properties^ do
      begin
       Name            := 'Output';
       VariableAddress := @FOutputBuffer;
       Direction       := drOut;
       Datatype        := dtFSample;
      end;
  1: with Properties^ do
      begin
       Name            := 'Order';
       VariableAddress := @FOrder;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '8';
       DatatypeExtra   := 'range 2, 64'
      end;
  2: with Properties^ do
      begin
       Name            := 'Downsample';
       VariableAddress := @FDownsample;
       Direction       := drIn;
       Datatype        := dtEnum;
       DefaultValue    := '1';
       DatatypeExtra   := 'range 1, 1024'
      end;
  else Result := False; // host will ask for plugs 0,1,2,3 etc. return false to signal when done
 end;;
end;

end.
