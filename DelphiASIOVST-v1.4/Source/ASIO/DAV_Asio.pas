unit DAV_Asio;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Steinberg Audio Stream I/O API                                            //
//  (c) 1997 - 1999, Steinberg Soft- und Hardware GmbH                        //
//                                                                            //
//  Asio Interface Specification v 2.0                                        //
//                                                                            //
//  basic concept is an i/o synchronous double-buffer scheme:                 //
//                                                                            //
//  on bufferSwitch(index == 0), host will read/write:                        //
//                                                                            //
//    after AsioStart(), the                                                  //
//  read  first input buffer A (index 0)                                      //
//  |   will be invalid (empty)                                               //
//  *   ------------------------                                              //
//  |------------------------|-----------------------|                        //
//  |                        |                       |                        //
//  |  Input Buffer A (0)    |   Input Buffer B (1)  |                        //
//  |                        |                       |                        //
//  |------------------------|-----------------------|                        //
//  |                        |                       |                        //
//  |  Output Buffer A (0)   |   Output Buffer B (1) |                        //
//  |                        |                       |                        //
//  |------------------------|-----------------------|                        //
//  *                        -------------------------                        //
//  |                        before calling AsioStart(),                      //
//  write                      host will have filled output                   //
//                             buffer B (index 1) already                     //
//                                                                            //
//  *please* take special care of proper statement of input and output        //
//  latencies (see AsioGetLatencies()), these control sequencer sync          //
//  accuracy                                                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, {$IFDEF FPC} LCLIntf, LCLType; {$ELSE} Windows; {$ENDIF}

const
  CAsioFalse = 0;
  CAsioTrue  = 1;

//-------------------------
// Type definitions
//-------------------------

type
  // use the function AsioSamplesToInt64 to convert to an Int64
  TAsioInt64 = record
    case Integer of
    0: (Hi : DWORD;
        Lo : DWORD);
    1: (Native: Int64);
  end;

  TAsioSamples = TAsioInt64;

  // Timestamp data type is 64 bit integer, Time format is Nanoseconds.
  TAsioTimeStamp = TAsioInt64;

  // Samplerates are expressed in IEEE 754 64 bit double float,
  // native format as host computer
  TAsioSampleRate = Double;

  // Boolean values are expressed as long
  TAsioBool = LongInt;

  // Sample Types are expressed as long
  TAsioSampleType = LongInt;

function AsioSamplesToInt64(const Samples: TAsioInt64): Int64;
function Int64ToAsioSamples(const Value: Int64): TAsioInt64;

const
  CAsioSTInt16MSB   = 0;
  CAsioSTInt24MSB   = 1;       // used for 20 bits as well
  CAsioSTInt32MSB   = 2;
  CAsioSTFloat32MSB = 3;       // IEEE 754 32 bit float
  CAsioSTFloat64MSB = 4;       // IEEE 754 64 bit double float

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can be more easily used with these
  CAsioSTInt32MSB16 = 8;       // 32 bit data with 18 bit alignment
  CAsioSTInt32MSB18 = 9;       // 32 bit data with 18 bit alignment
  CAsioSTInt32MSB20 = 10;      // 32 bit data with 20 bit alignment
  CAsioSTInt32MSB24 = 11;      // 32 bit data with 24 bit alignment

  CAsioSTInt16LSB   = 16;
  CAsioSTInt24LSB   = 17;      // used for 20 bits as well
  CAsioSTInt32LSB   = 18;
  CAsioSTFloat32LSB = 19;      // IEEE 754 32 bit float, as found on Intel x86 architecture
  CAsioSTFloat64LSB = 20;      // IEEE 754 64 bit double float, as found on Intel x86 architecture

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can more easily used with these
  CAsioSTInt32LSB16 = 24;      // 32 bit data with 16 bit alignment
  CAsioSTInt32LSB18 = 25;      // 32 bit data with 18 bit alignment
  CAsioSTInt32LSB20 = 26;      // 32 bit data with 20 bit alignment
  CAsioSTInt32LSB24 = 27;      // 32 bit data with 24 bit alignment

  // Asio DSD format.
  CAsioSTDSDInt8LSB1 = 32;     // DSD 1 bit data, 8 Samples per byte. First sample in Least significant bit.
  CAsioSTDSDInt8MSB1 = 33;     // DSD 1 bit data, 8 Samples per byte. First sample in Most significant bit.
  CAsioSTDSDInt8NER8 = 40;     // DSD 8 bit data, 1 sample per byte. No Endianness required.


///////////////////////////////////////////////////////////////////////////////
// DSD operation and buffer layout
// Definition by Steinberg/Sony Oxford.
//
// We have tried to treat DSD as PCM and so keep a consistant structure across
// the Asio interface.
//
// DSD's sample rate is normally referenced as a multiple of 44.1Khz, so
// the standard sample rate is refered to as 64Fs (or 2.8224Mhz). We looked
// at making a special case for DSD and adding a field to the AsioFuture that
// would allow the user to select the Over Sampleing Rate (OSR) as a seperate
// entity but decided in the end just to treat it as a simple Value of
// 2.8224Mhz and use the standard interface to set it.
//
// The second problem was the "word" size, in PCM the word size is always a
// greater than or equal to 8 bits (a byte). This makes life easy as we can
// then pack the Samples into the "natural" size for the machine.
// In DSD the "word" size is 1 bit. This is not a major problem and can easily
// be dealt with if we ensure that we always deal with a multiple of 8 Samples.
//
// DSD brings with it another twist to the Endianness religion. How are the
// Samples packed into the byte. It would be nice to just say the most significant
// bit is always the first sample, however there would then be a performance hit
// on little endian machines. Looking at how some of the processing goes...
// Little endian machines like the first sample to be in the Least Significant
//   Bit, this is because when you write it to memory the data is in the
//   correct format to be shifted in and out of the words.
// Big endian machine prefer the first sample to be in the Most Significant
//   Bit, again for the same reason.
//
// And just when things were looking really muddy there is a proposed extension
// to DSD that uses 8 bit word sizes. It does not care what endianness you use.
//
// Switching the driver between DSD and PCM mode
// AsioFuture allows for extending the Asio API quite transparently.
// See kAsioSetIoFormat, kAsioGetIoFormat, kAsioCanDoIoFormat
///////////////////////////////////////////////////////////////////////////////


/////////////////
// Error codes //
/////////////////

type
  TAsioError = LongInt;

const
  ASE_OK               = 0;                   // This Value will be returned whenever the call succeeded
  ASE_SUCCESS          = $3F4847A0;           // unique success return Value for AsioFuture calls
  ASE_NotPresent       = -1000;               // hardware input or output is not present or available
  ASE_HWMalfunction    = ASE_NotPresent + 1;  // hardware is malfunctioning (can be returned by any Asio function)
  ASE_InvalidParameter = ASE_NotPresent + 2;  // input parameter invalid
  ASE_InvalidMode      = ASE_NotPresent + 3;  // hardware is in a bad mode or used in a bad mode
  ASE_SPNotAdvancing   = ASE_NotPresent + 4;  // hardware is not running when sample position is inquired
  ASE_NoClock          = ASE_NotPresent + 5;  // sample clock or rate cannot be determined or is not present
  ASE_NoMemory         = ASE_NotPresent + 6;  // not enough memory for completing the request


///////////////////////
// Time Info support //
///////////////////////

type
  TAsioTimeCode = packed record
    Speed           : Double;        // speed relation (fraction of nominal speed)
                                     // optional; set to 0. or 1. if not supported
    TimecodeSamples : TAsioSamples;  // time in Samples
    Flags           : Longword;      // some information flags (see below)
    Future          : array[0..63] of AnsiChar;
  end;

type
  AsioTimeCodeFlags = LongInt;

const
  kTcValid      = 1;
  kTcRunning    = 1 shl 1;
  kTcReverse    = 1 shl 2;
  kTcOnspeed    = 1 shl 3;
  kTcStill      = 1 shl 4;
  kTcSpeedValid = 1 shl 8;

type
  TAsioTimeInfo = packed record
    Speed          : Double;           // absolute speed (1. = nominal)
    SystemTime     : TAsioTimeStamp;   // system time related to samplePosition, in nanoseconds
                                       // on mac, must be derived from Microseconds() (not UpTime()!)
                                       // on windows, must be derived from timeGetTime()
    SamplePosition : TAsioSamples;
    SampleRate     : TAsioSampleRate;  // current rate
    Flags          : Longword;         // (see below)
    Reserved       : array[0..11] of AnsiChar;
  end;
  TAsioTimeInfoFlags = LongInt;

const
  kSystemTimeValid     = 1;            // must always be valid
  kSamplePositionValid = 1 shl 1;      // must always be valid
  kSampleRateValid     = 1 shl 2;
  kSpeedValid          = 1 shl 3;
  kSampleRateChanged   = 1 shl 4;
  kClockSourceChanged  = 1 shl 5;

type
  PAsioTime = ^TAsioTime;
  TAsioTime = packed record            // both input/output
    Reserved : array[0..3] of LongInt; // must be 0
    TimeInfo : TAsioTimeInfo;          // required
    TimeCode : TAsioTimeCode;          // optional, evaluated if (timeCode.flags and kTcValid)
  end;

////////////////////////////////////////////////////////////////////////////////
// using time info:
// it is recommended to use the new method with time info even if the Asio
// device does not support timecode; continuous calls to AsioGetSamplePosition
// and AsioGetSampleRate are avoided, and there is a more defined relationship
// between callback time and the time info.
//
// see the example below.
// to initiate time info mode, after you have received the callbacks pointer in
// AsioCreateBuffers, you will call the AsioMessage callback with
// kAsioSupportsTimeInfo as the argument. if this returns 1, host has accepted
// time info mode.
// Now host expects the new callback bufferSwitchTimeInfo to be used instead
// of the old bufferSwitch method. the AsioTime structure is assumed to be valid
// and accessible until the callback returns.
//
// using time code:
// if the device supports reading time code, it will call host's AsioMessage
// callback with kAsioSupportsTimeCode as the Selector. it may then fill the
// according fields and set the kTcValid flag.
// host will call the future method with the kAsioEnableTimeCodeRead Selector
// when it wants to enable or disable tc reading by the device. you should also
// support the kAsioCanTimeInfo and kAsioCanTimeCode selectors in AsioFuture
// (see example).
//
//
// note:
// the AsioTimeInfo/AsioTimeCode pair is supposed to work in both directions.
// as a matter of convention, the relationship between the sample
// position counter and the time code at buffer switch time is
// (ignoring offset between tc and sample pos when tc is running):
//
// on input:  sample 0 -> input  buffer sample 0 -> time code 0
// on output:  sample 0 -> output buffer sample 0 -> time code 0
//
// this means that for 'real' calculations, one has to take into account
// the according latencies.
//
// example:
//
// AsioTime AsioTime;
//
// in createBuffers()
// {
//   memset(&AsioTime, 0, SizeOf(AsioTime));
// //   AsioTimeInfo* ti = &AsioTime.timeInfo;
//   ti->sampleRate = theSampleRate;
//   AsioTimeCode* tc = &AsioTime.timeCode;
//   tc->speed = 1.;
//   timeInfoMode = false;
//   canTimeCode = false;
//   if(callbacks->AsioMessage(kAsioSupportsTimeInfo, 0, 0, 0) == 1)
//   {
//     timeInfoMode = true;
// #if kCanTimeCode
//     if(callbacks->AsioMessage(kAsioSupportsTimeCode, 0, 0, 0) == 1)
//       canTimeCode = true;
// #endif
//   }
// }
//
// void switchBuffers(long DoubleBufferIndex, bool processNow)
// {
//   if(timeInfoMode)
//   {
//     AsioTimeInfo* ti = &AsioTime.timeInfo;
//     ti->flags =  kSystemTimeValid | kSamplePositionValid | kSampleRateValid;
//     ti->systemTime = theNanoSeconds;
//     ti->samplePosition = theSamplePosition;
//     if(ti->sampleRate != theSampleRate)
//       ti->flags |= kSampleRateChanged;
//     ti->sampleRate = theSampleRate;
//
// #if kCanTimeCode
//     if(canTimeCode && timeCodeEnabled)
//     {
//       AsioTimeCode* tc = &AsioTime.timeCode;
//       tc->timeCodeSamples = tcSamples;            // tc in Samples
//       tc->flags = kTcValid | kTcRunning | kTcOnspeed;      // if so...
//     }
//     AsioTime* bb = callbacks->bufferSwitchTimeInfo(&AsioTime, DoubleBufferIndex, processNow ? AsioTrue : AsioFalse);
// #else
//     callbacks->bufferSwitchTimeInfo(&AsioTime, DoubleBufferIndex, processNow ? AsioTrue : AsioFalse);
// #endif
//   }
//   else
//     callbacks->bufferSwitch(DoubleBufferIndex, AsioFalse);
// }
//
// AsioError AsioFuture(long Selector, void *Params)
// {
//   switch(Selector)
//   {
//     case kAsioEnableTimeCodeRead:
//       timeCodeEnabled = true;
//       return ASE_SUCCESS;
//     case kAsioDisableTimeCodeRead:
//       timeCodeEnabled = false;
//       return ASE_SUCCESS;
//     case kAsioCanTimeInfo:
//       return ASE_SUCCESS;
//     #if kCanTimeCode
//     case kAsioCanTimeCode:
//       return ASE_SUCCESS;
//     #endif
//   }
//   return ASE_NotPresent;
// };
//
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////
// application's audio stream handler callbacks //
//////////////////////////////////////////////////

type
  TAsioBufferSwitchProc = procedure(DoubleBufferIndex: LongInt; DirectProcess: TAsioBool); cdecl;
  TAsioSampleRateDidChangeProc = procedure(SampleRate: TAsioSampleRate); cdecl;
  TAsioMessageFunc = function(Selector, Value: LongInt; message: Pointer; opt: PDouble): LongInt; cdecl;
  TAsioBufferSwitchTimeInfoFunc = function(var Params: TAsioTime; DoubleBufferIndex: LongInt; DirectProcess: TAsioBool): PAsioTime; cdecl;

  PAsioCallbacks = ^TAsioCallbacks;
  TAsioCallbacks = packed record
    bufferSwitch : TAsioBufferSwitchProc;
    // bufferSwitch indicates that both input and output are to be processed.
    // the current buffer half index (0 for A, 1 for B) determines
    // - the output buffer that the host should start to fill. the other buffer
    //   will be passed to output hardware regardless of whether it got filled
    //   in time or not.
    // - the input buffer that is now filled with incoming data. Note that
    //   because of the synchronicity of i/o, the input always has at
    //   least one buffer latency in relation to the output.
    // DirectProcess suggests to the host whether it should immedeately
    // start processing (DirectProcess == AsioTrue), or whether its process
    // should be deferred because the call comes from a very low level
    // (for instance, a high level priority interrupt), and direct processing
    // would cause timing instabilities for the rest of the system. If in doubt,
    // DirectProcess should be set to AsioFalse.
    // Note: bufferSwitch may be called at interrupt time for highest efficiency.

    sampleRateDidChange : TAsioSampleRateDidChangeProc;
    // gets called when the AudioStreamIO detects a sample rate change
    // If sample rate is unknown, 0 is passed (for instance, clock loss
    // when externally synchronized).

    AsioMessage : TAsioMessageFunc;
    // generic callback for various purposes, see selectors below.
    // note this is only present if the Asio version is 2 or higher

    bufferSwitchTimeInfo : TAsioBufferSwitchTimeInfoFunc;
    // New callback with time info. Makes AsioGetSamplePosition() and various
    // calls to AsioGetSampleRate obsolete.
    // Allows for timecode sync etc. to be preferred; will be used if
    // the driver calls AsioMessage with Selector kAsioSupportsTimeInfo.
  end;


const                                // AsioMessage selectors
  kAsioSelectorSupported    = 1;     // Selector in <Value>, returns 1L if supported,
                                     //   0 otherwise
  kAsioEngineVersion        = 2;     // returns engine (host) Asio implementation version,
                                     //   2 or higher
  kAsioResetRequest         = 3;     // request driver reset. if accepted, this
                                     //   will close the driver (Asio_Exit() ) and
                                     //   re-open it again (Asio_Init() etc). some
                                     //   drivers need to reconfigure for instance
                                     //   when the sample rate changes, or some basic
                                     //   changes have been made in Asio_ControlPanel().
                                     //   returns 1L; note the request is merely passed
                                     //   to the application, there is no way to determine
                                     //   if it gets accepted at this time (but it usually
                                     //   will be).
  kAsioBufferSizeChange     = 4;     // not yet supported, will currently always return 0L.
                                     //   for now, use kAsioResetRequest instead.
                                     //   once implemented, the new buffer size is expected
                                     //   in <Value>, and on success returns 1L
  kAsioResyncRequest        = 5;     // the driver went out of sync, such that
                                     //   the timestamp is no longer valid. this
                                     //   is a request to re-start the engine and
                                     //   slave devices (sequencer). returns 1 for ok,
                                     //   0 if not supported.
  kAsioLatenciesChanged     = 6;     // the drivers latencies have changed. The engine
                                     //   will refetch the latencies.
  kAsioSupportsTimeInfo     = 7;     // if host returns true here, it will expect the
                                     //   callback bufferSwitchTimeInfo to be called instead
                                     //   of bufferSwitch
  kAsioSupportsTimeCode     = 8;     // supports time code reading/writing
  kAsioSupportsInputMonitor = 9;     // supports input monitoring
  kAsioNumMessageSelectors  = 10;

////////////////////////////////////////////////////////////////////////////////

type
  TAsioDriverInfo = packed record
    AsioVersion   : LongInt;        // currently, 2
    DriverVersion : LongInt;        // driver specific
    Name          : array[0..31] of AnsiChar;
    ErrorMessage  : array[0..123] of AnsiChar;
    SysRef        : HWND;           // on input: system reference
                                    // (Windows: application main window handle, Mac & SGI: 0)
  end;

  PAsioClockSource = ^TAsioClockSource;
  TAsioClockSource = packed record
    Index             : LongInt;    // as used for AsioSetClockSource()
    AssociatedChannel : LongInt;    // for instance, S/PDIF or AES/EBU
    AssociatedGroup   : LongInt;    // see channel groups (AsioGetChannelInfo())
    IsCurrentSource   : TAsioBool;  // AsioTrue if this is the current clock source
    Name              : array[0..31] of AnsiChar;   // for user selection
  end;
  TAsioClockSources = array [0..0] of TAsioClockSource;
  PAsioClockSources = ^TAsioClockSources;

  TAsioChannelInfo = packed record
    Channel      : LongInt;                  // on input, channel index
    IsInput      : TAsioBool;                // on input
    IsActive     : TAsioBool;                // on exit
    ChannelGroup : LongInt;                  // dto
    SampleType   : TAsioSampleType;          // dto
    Name         : array[0..31] of AnsiChar; // dto
  end;

  PAsioBufferInfo = ^TAsioBufferInfo;
  TAsioBufferInfo = packed record
    IsInput    : TAsioBool;               // on input:  AsioTrue: input, else output
    ChannelNum : LongInt;                 // on input:  channel index
    Buffers    : array [0..1] of Pointer; // on output: double buffer addresses
  end;
  TAsioBufferInfos = array [0..0] of TAsioBufferInfo;
  PAsioBufferInfos = ^TAsioBufferInfos;

const
  kAsioEnableTimeCodeRead  =  1;    // no arguments
  kAsioDisableTimeCodeRead =  2;    // no arguments
  kAsioSetInputMonitor     =  3;    // AsioInputMonitor* in Params
  kAsioTransport           =  4;    // AsioTransportParameters* in Params
  kAsioSetInputGain        =  5;    // AsioChannelControls* in Params, apply gain
  kAsioGetInputMeter       =  6;    // AsioChannelControls* in Params, fill meter
  kAsioSetOutputGain       =  7;    // AsioChannelControls* in Params, apply gain
  kAsioGetOutputMeter      =  8;    // AsioChannelControls* in Params, fill meter
  kAsioCanInputMonitor     =  9;    // no arguments for kAsioCanXXX selectors
  kAsioCanTimeInfo         = 10;
  kAsioCanTimeCode         = 11;
  kAsioCanTransport        = 12;
  kAsioCanInputGain        = 13;
  kAsioCanInputMeter       = 14;
  kAsioCanOutputGain       = 15;
  kAsioCanOutputMeter      = 16;

  // DSD support
  // The following extensions are required to allow switching
  // and control of the DSD subsystem.
  kAsioSetIoFormat         = $23111961; // AsioIoFormat * in Params.
  kAsioGetIoFormat         = $23111983; // AsioIoFormat * in Params.
  kAsioCanDoIoFormat       = $23112004; // AsioIoFormat * in Params.

type
  TAsioInputMonitor = packed record
    Input     : LongInt;   // this input was set to monitor (or off), -1: all
    Output    : LongInt;   // suggested output for monitoring the input (if so)
    Gain      : LongInt;   // suggested gain, ranging 0 - 0x7fffffffL (-inf to +12 dB)
    State     : TAsioBool; // AsioTrue => on, AsioFalse => off
    Pan       : LongInt;   // suggested pan, 0 => all left, 0x7fffffff => right
  end;

  TAsioChannelControls = packed record
    Channel   : LongInt;        // on input, channel index
    IsInput   : TAsioBool;      // on input
    Gain      : LongInt;        // on input,  ranges 0 thru 0x7fffffff
    Meter     : LongInt;        // on return, ranges 0 thru 0x7fffffff
    Future    : array[0..31] of AnsiChar;
  end;

  PAsioTransportParameters = ^TAsioTransportParameters;
  TAsioTransportParameters = packed record
    Command        : LongInt;                   // see enum below
    SamplePosition : TAsioSamples;
    Track          : LongInt;
    TrackSwitches  : array[0..15] of LongInt;   // 512 tracks on/off
    Future         : array[0..63] of AnsiChar;
  end;

// DSD support
//  Some notes on how to use AsioIoFormatType.
//
//  The caller will fill the format with the request types.
//  If the board can do the request then it will leave the
//  values unchanged. If the board does not support the
//  request then it will change that entry to Invalid (-1)
//
//  So to request DSD then
//
//  AsioIoFormat NeedThis={kAsioDSDFormat};
//
//  if(ASE_SUCCESS != AsioFuture(kAsioSetIoFormat,&NeedThis) ){
//    // If the board did not accept one of the parameters then the
//    // whole call will fail and the failing parameter will
//    // have had its Value changes to -1.
//  }
//
// Note: Switching between the formats need to be done before the "prepared"
// state (see Asio 2 documentation) is entered.

  TAsioIoFormatType = (kAsioFormatInvalid, kAsioPCMFormat, kAsioDSDFormat);

  TAsioIoFormat_s = packed record
    FormatType : TAsioIoFormatType;
    Future     : array [0..512 - SizeOf(TAsioIoFormatType)] of Char;
  end;

const
  kTransStart       = 1;
  kTransStop        = 2;
  kTransLocate      = 3;    // to samplePosition
  kTransPunchIn     = 4;
  kTransPunchOut    = 5;
  kTransArmOn       = 6;    // track
  kTransArmOff      = 7;    // track
  kTransMonitorOn   = 8;    // track
  kTransMonitorOff  = 9;    // track
  kTransArm         = 10;   // trackSwitches
  kTransMonitor     = 11;   // trackSwitches

implementation

function AsioSamplesToInt64(const Samples: TAsioSamples): Int64;
begin
  Result := (Samples.Hi * $100000000) + Samples.Lo;
end;

function Int64ToAsioSamples(const Value: Int64): TAsioSamples;
begin
  Result.Hi := (Value and $FFFFFFFF00000000) shr 32;
  Result.Lo := (Value and $00000000FFFFFFFF);
end;

end.
