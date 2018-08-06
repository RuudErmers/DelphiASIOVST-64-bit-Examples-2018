unit AsioDriverMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, ComObj, DAV_Types, DAV_ASIO, DAV_ASIOExtendedDriver, DAV_ASIODriver;

const
  DTest_guid: TGUID = '{A8DD45FD-34CC-4996-9695-CDD2AE483B47}';
  DTest_classname = 'DAVTestDriver';
  DTest_name = 'DAV Test Driver';

type
  IDriverTest = interface(IDavASIODriverInterface)
    ['{A8DD45FD-34CC-4996-9695-CDD2AE483B47}']
  end;

  TDriverTest = class(TDavASIOExtendedDriver)
  protected
    procedure InitializeDriverParams; override;
    procedure ProcessBuffers; override;
  end;

  TTestTCWrapper = class(TDavASIOTCWrapper, IDriverTest)
  protected
    function GetDriverClass: TTDavASIODriver; override;
  end;


implementation

uses
  ComServ,AsioDriverMainCPanel, DAV_Common;

function TTestTCWrapper.GetDriverClass: TTDavASIODriver;
begin
  result := TDriverTest;
end;

{ TDriverTest }

procedure TDriverTest.InitializeDriverParams;
begin
  SetDriverName(DTest_name);
  SetDriverVersion(1);
  AddClock('Default Clock',0);
  AddClock('Second Clock',1);
  AddChannel('c1i',0,ASIOSTFloat32LSB,true);
  AddChannel('c2i',1,ASIOSTFloat32LSB,true);
  AddChannel('c1o',0,ASIOSTFloat32LSB,false);
  AddChannel('c2o',1,ASIOSTFloat32LSB,false);
  SetSampleRateMode(edsrm_List);
  AddSampleRate(11025);
  AddSampleRate(22050);
  AddSampleRate(44100);
  AddSampleRate(48000);
  AddSampleRate(96000);
  SetBufferSizes(64,1024,512,-1);
  SetControlPanelClass(TDriverTestCP);
end;

procedure TDriverTest.ProcessBuffers;
var i, frame: LongInt;
begin
  for i := 0 to fInChannelList.Count-1 do
    with TDavASIOExtDrvrChannelListItem(fInChannelList.Items[i]) do
      if IsActive then
        for frame:=0 to fBufferSize.Current-1 do
        begin
          TDAVSingleDynArray(DoubleBuffer[fCurrentBuffer])[frame] := sin(frame/fBufferSize.Current*2*Pi);
        end;

  // this is a dummy, override it
  ASIOBufferSwitch(fCurrentBuffer, ASIOTrue);
  Sleep(round(1000 * fBufferSize.Current / fSampleRate));
  fCurrentBuffer := 1-fCurrentBuffer;
end;

initialization
  TDavAsioDriverFactory.Create(ComServer, TTestTCWrapper, DTest_guid,
    DTest_classname, DTest_name, ciSingleInstance, tmApartment);

end.
