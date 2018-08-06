unit DAV_AudioFileWAVPack;

interface

uses
  Classes, Contnrs, SysUtils, DAVDCommon, DAudioFile, DChunkClasses,
  DWaveFileTypes;

const
  cChunkHeaderFormat   = '4L';
  cWaveHeaderFormat    = 'SSLLSSSSLS';
  cWavpackHeaderFormat = '4LS2LLLLL';
type
(*
typedef struct {
    FormatTag, NumChannels : shortInt;
    uint32_t SampleRate, BytesPerSecond;
    ushort BlockAlign, BitsPerSample;
    ushort cbSize, ValidBitsPerSample;
    int32_t ChannelMask;
    ushort SubFormat;
    char GUID [14];
} WaveHeader;

*)

// This is the ONLY structure that occurs in WavPack files (as of version
// 4.0), and is the preamble to every block in both the .wv and .wvc
// files (in little-endian format). Normally, this structure has no use
// to an application using the library to read or write WavPack files,
// but if an application needs to manually parse WavPack files then this
// would be used (with appropriate endian correction).

  TWavpackHeader = packed record
    ckID           : TChunkName;
    ckSize         : Cardinal;
    version        : ShortInt;
    track_no       : ShortInt;
    index_no       : ShortInt;
    total_samples  : Cardinal;
    block_index    : Cardinal;
    block_samples  : Cardinal;
    flags          : Cardinal;
    crc            : Cardinal;
  end;

// or-values for WavpackHeader.flags
const
  cBytesStored    =  $3;    // 1-4 bytes/sample
  cMonoFlag       =  $4;    // not stereo
  cHybridFlag     =  $8;    // hybrid mode
  cJointStereo    = $10;    // joint stereo
  cCrossDecorr    = $20;    // no-delay cross decorrelation
  cHybridShape    = $40;    // noise shape (hybrid mode only)
  cFloatData      = $80;    // ieee 32-bit floating point data

  cInt32Data      = $100   // special extended int handling
  cHybridBitrate  = $200   // bitrate noise (hybrid mode only)
  cHybridBalance  = $400   // balance noise (hybrid stereo mode only)

  cInitialBlock   = $800   // initial block of multichannel segment
  cFinalBlock     = $1000  // final block of multichannel segment

  cShiftLSB       = 13
(*
  cSHIFT_MASK     = ($1fL << SHIFT_LSB)

  cMAG_LSB        = 18
  cMAG_MASK       = ($1fL << MAG_LSB)

  cSRATE_LSB      = 23
  cSRATE_MASK     = ($fL << SRATE_LSB)
*)

  cFalseStereo    = $40000000;   // block is stereo, but data is mono

  cIgnoredFlags   = $18000000;   // reserved, but ignore if encountered
  cNewShaping     = $20000000;   // use IIR filter for negative shaping
  cUnknownFlags   = $80000000;   // also reserved, but refuse decode if encountered

//  cMONO_DATA (MONO_FLAG | FALSE_STEREO)

  cMinStreamVers  = $402;      // lowest stream version we'll decode
  cMaxStreamVers  = $410;      // highest stream version we'll decode or encode
  cCurStreamVers  = $407;      // stream version we are writing now

// These are the mask bit definitions for the metadata chunk id byte (see format.txt)

  cIdUnique          = $3F;
  cIdOptionalData    = $20;
  cIdOddSize         = $40;
  cIdLarge           = $80;

  cIdDummy           = $0;
  cIdEncoderInfo     = $1;
  cIdDecorrTerms     = $2;
  cIdDecorrWeights   = $3;
  cIdDecorrSamples   = $4;
  cIdEntropyVars     = $5;
  cIdHybridProfile   = $6;
  cIdShapingWeights  = $7;
  cIdFloatInfo       = $8;
  cIdInt32Info       = $9;
  cIdWV_Bitstream    = $A;
  cIdWVC_Bitstream   = $B;
  cIdWVX_Bitstream   = $C;
  cIdChannelInfo     = $D;

(*
  cIdRIFF_HEADER          (IdOPTIONAL_DATA | $1)
  cIdRIFF_TRAILER         (IdOPTIONAL_DATA | $2)
  cIdREPLAY_GAIN          (IdOPTIONAL_DATA | $3)    // never used (APEv2)
  cIdCUESHEET             (IdOPTIONAL_DATA | $4)    // never used (APEv2)
  cIdCONFIG_BLOCK         (IdOPTIONAL_DATA | $5)
  cIdMD5_CHECKSUM         (IdOPTIONAL_DATA | $6)
  cIdSAMPLE_RATE          (IdOPTIONAL_DATA | $7)
*)

///////////////////////// WavPack Configuration ///////////////////////////////

// This external structure is used during encode to provide configuration to
// the encoding engine and during decoding to provide fle information back to
// the higher level functions. Not all fields are used in both modes.

type
  TWavpackConfig = packed record
    Bitrate          : Single;
    ShapingWeight    : Single;
    bits_per_sample  : Integer;
    bytes_per_sample : Integer;
    qmode, flags     : Integer;
    xmode            : Integer;
    num_channels     : Integer;
    float_norm_exp   : Integer;
    block_samples    : Integer;
    extra_flags      : Integer;
    sample_rate      : Integer;
    channel_mask     : Integer;
(*
    uchar md5_checksum [16], md5_read;
    int num_tag_strings;
    char **tag_strings;
*)
  end;


const
  cConfigHybridFlag      = $8;        // hybrid mode
  cConfigJointStereo     = $10;       // joint stereo
  cConfigHybridShape     = $40;       // noise shape (hybrid mode only)
  cConfigFastFlag        = $200;      // fast mode
  cConfigHighFlag        = $800;      // high quality mode
  cConfigVeryHighFlag    = $1000;     // very high
  cConfigBitrateKBPS     = $2000;     // bitrate is kbps, not bits / sample
  cConfigShapeOverride   = $8000;     // shaping mode specified
  cConfigJointOverride   = $10000;    // joint-stereo mode specified
  cConfigDynamicShaping  = $20000;    // dynamic noise shaping
  cConfigCreateEXE       = $40000;    // create executable
  cConfigCreateWVC       = $80000;    // create correction file
  cConfigOptimizeWVC     = $100000;   // maximize bybrid compression
  cConfigCalcNoise       = $800000;   // calc noise in hybrid mode
  cConfigExtraMode       = $2000000;  // extra processing mode
  cConfigSkipWVX         = $4000000;  // no wvx stream w/ floats & big ints
  cConfigMD5Checksum     = $8000000;  // store MD5 signature
  cConfigMergeBlocks     = $10000000; // merge blocks of equal redundancy (for lossyWAV)
  cConfigOptimizeMono    = $80000000; // optimize for mono streams posing as stereo

////////////// Callbacks used for reading & writing WavPack streams //////////

(*
typedef struct {
    int32_t (*read_bytes)(void *id, void *data, int32_t bcount);
    uint32_t (*get_pos)(void *id);
    int (*set_pos_abs)(void *id, uint32_t pos);
    int (*set_pos_rel)(void *id, int32_t delta, int mode);
    int (*push_back_byte)(void *id, int c);
    uint32_t (*get_length)(void *id);
    int (*can_seek)(void *id);

    // this callback is for writing edited tags only
    int32_t (*write_bytes)(void *id, void *data, int32_t bcount);
} WavpackStreamReader;

typedef int (*WavpackBlockOutput)(void *id, void *data, int32_t bcount);
*)

//////////////////////////// function prototypes /////////////////////////////

// Note: See wputils.c sourcecode for descriptions for using these functions.

type
  PWavpackContext = Pointer;

function WavpackOpenFileInputEx(Reader: PWavpackStreamReader; wv_id, wvc_id: Pointer; error: PChar; Flags, NormOffset: Integer): PWavpackContext;
function WavpackOpenFileInput(const infilename: PChar; error: PChar; Flags, NormOffset: Integer): PWavpackContext;

const
  cOpenWVC        = $1;     // open/read "correction" file
  cOpenTags       = $2;     // read ID3v1 / APEv2 tags (seekable file)
  cOpenWrapper    = $4;     // make audio wrapper available (i.e. RIFF)
  cOpen2chMax     = $8;     // open multichannel as stereo (no downmix)
  cOpenNormalize  = $10;    // normalize floating point data to +/- 1.0
  cOpenStreaming  = $20;    // "streaming" mode blindly unpacks blocks
                            // w/o regard to header file position info
  cOpenEditTags   = $40;    // allow editing of tags

function WavpackGetMode(WPC: PWavpackContext): Integer;

const
  cModeWVC        = $1;
  cModeLossless   = $2;
  cModeHybrid     = $4;
  cModeFloat      = $8;
  cModeValidTag   = $10;
  cModeHigh       = $20;
  cModeFast       = $40;
  cModeExtra      = $80;    // extra mode used, see ModeXMODE for possible level
  cModeAPETag     = $100;
  cModeSFX        = $200;
  cModeVeryHigh   = $400;
  cModeMD5        = $800;
  cModeXMode      = $7000;  // mask for extra level (1-6, 0=unknown)
  cModeDNS        = $8000;

function WavpackGetErrorMessage(WPC: PWavpackContext): PChar;
function WavpackGetVersion(WPC: PWavpackContext): Integer;
function WavpackUnpackSamples(WPC: PWavpackContext; int32_t *buffer, uint32_t samples): Cardinal;
function WavpackGetNumSamples(WPC: PWavpackContext): Cardinal;
function WavpackGetSampleIndex(WPC: PWavpackContext): Cardinal;
function WavpackGetNumErrors(WPC: PWavpackContext): Integer;
function WavpackLossyBlocks(WPC: PWavpackContext): Integer;
function WavpackSeekSample(WPC: PWavpackContext; uint32_t sample): Integer;
function WavpackCloseFile(WPC: PWavpackContext): PWavpackContext;
function WavpackGetSampleRate(WPC: PWavpackContext): Cardinal;
function WavpackGetBitsPerSample(WPC: PWavpackContext): Integer;
function WavpackGetBytesPerSample(WPC: PWavpackContext): Integer;
function WavpackGetNumChannels(WPC: PWavpackContext): Integer;
function WavpackGetChannelMask(WPC: PWavpackContext): Integer;
function WavpackGetReducedChannels(WPC: PWavpackContext): Integer;
function WavpackGetFloatNormExp(WPC: PWavpackContext): Integer;
function WavpackGetMD5Sum(WPC: PWavpackContext; uchar data [16]): Integer;
function WavpackGetWrapperBytes(WPC: PWavpackContext): Cardinal;
// uchar *WavpackGetWrapperData(WPC: PWavpackContext);
procedure WavpackFreeWrapper(WPC: PWavpackContext);
procedure WavpackSeekTrailingWrapper(WPC: PWavpackContext);
function WavpackGetProgress(WPC: PWavpackContext): Double;
function WavpackGetFileSize(WPC: PWavpackContext): Cardinal;
function WavpackGetRatio(WPC: PWavpackContext): Double;
function WavpackGetAverageBitrate(WPC: PWavpackContext; CountWVC : Integer): Double;
function WavpackGetInstantBitrate(WPC: PWavpackContext): Double;
function WavpackGetNumTagItems(WPC: PWavpackContext): Integer;
(*
function WavpackGetTagItem(WPC: PWavpackContext; const char *item, char *value, int size): Integer;
function WavpackGetTagItemIndexed(WPC: PWavpackContext; int index, char *item, int size): Integer;
function WavpackAppendTagItem(WPC: PWavpackContext; const char *item, const char *value, int vsize): Integer;
function WavpackDeleteTagItem(WPC: PWavpackContext; const char *item): Integer;
function WavpackWriteTag(WPC: PWavpackContext): Integer;

WavpackContext *WavpackOpenFileOutput (WavpackBlockOutput blockout, void *wv_id, void *wvc_id): Integer;
function WavpackSetConfiguration(WPC: PWavpackContext; WavpackConfig *config, uint32_t total_samples): Integer;
function WavpackAddWrapper(WPC: PWavpackContext; void *data, uint32_t bcount): Integer;
function WavpackStoreMD5Sum(WPC: PWavpackContext; uchar data [16]): Integer;
*)
function WavpackPackInit(WPC: PWavpackContext): Integer;
function WavpackPackSamples(WPC: PWavpackContext; SampleBuffer: PInteger; SampleCount: Cardinal): Integer;
function WavpackFlushSamples(WPC: PWavpackContext) : Integer;
procedure WavpackUpdateNumSamples(WPC: PWavpackContext; FirstBlock: Pointer);
function WavpackGetWrapperLocation(FirstBlock: Pointer; Size: PCardinal) : Pointer;
function WavpackGetEncodedNoise(WPC: PWavpackContext; Peak: PDouble): Double;

procedure WavpackFloatNormalize(Values: PInteger; NumValues: Integer; DeltaExp: Integer);

procedure WavpackLittleEndianToNative(Data: Pointer; Format: PChar);
procedure WavpackNativeToLittleEndian(Data: Pointer; Format: PChar);

function WavpackGetLibraryVersion: Cardinal;
function WavpackGetLibraryVersionString : PChar;

implementation

end;
