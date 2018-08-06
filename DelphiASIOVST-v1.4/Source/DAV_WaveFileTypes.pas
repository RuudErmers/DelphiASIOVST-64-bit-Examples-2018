{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_WaveFileTypes;

interface

{$I DAV_Compiler.inc}

type
{$IFDEF Delphi5}
  TWavEncoding = (etUnknown, etPCM, etMSADPCM, etPCMFLOAT, etCompaqVSELP,
    etIBMCVSD, etALAW, etMuLAW, etMicrosoftDTS, etDRM, etWMA9Speech,
    etWMRTVoice, etDVIADPCM);
{$ELSE}
  TWavEncoding = (
    etUnknown              =  $0,
    etPcm                  =  $1,
    etMsAdPcm              =  $2,
    etPcmFloat             =  $3,
    etCompaqVSELP          =  $4,
    etIbmCVSD              =  $5,
    etALaw                 =  $6,
    etMuLaw                =  $7,
    etMicrosoftDTS         =  $8,
    etDRM                  =  $9,
    etWMA9Speech           =  $A,
    etWMRTVoice            =  $B,
    etOKIAdPcm             = $10,
    etDVIAdPcm             = $11,
    etMediaSpaceAdPcm      = $12,
    etSierraAdPcm          = $13,
    etG723AdPcm            = $14,
    etDIGISTD              = $15,
    etDIGIFIX              = $16,
    etDiaLogicAdPcm        = $17,
    etMVAdPcm              = $18,
    etHPCU                 = $19,
    etHPDynamicVoice       = $1A,
    etYamahaAdPcm          = $20,
    etSONARC               = $21,
    etTrueSpeech           = $22,
    etECHOSC1              = $23,
    etAF36                 = $24,
    etAPTX                 = $25,
    etAF10                 = $26,
    etProsody1612          = $27,
    etMergingTechLRC       = $28,
    etDolbyAC2             = $30,
    etGSM610               = $31,
    etMSNAudio             = $32,
    etAntexAdPcmE          = $33,
    etResVQLPC1            = $34,
    etResVQLPC2            = $35,
    etDigiAdPcm            = $36,
    etResCR10              = $37,
    etVBXAdPcm             = $38,
    etIMAAdPcm             = $39,
    etECHOSC3              = $3A,
    etRockwellAdPcm        = $3B,
    etDIGITALK             = $3C,
    etXebecMultimedia      = $3D,
    etG721AdPcm            = $40,
    etAntexG728CELP        = $41,
    etMicrosoftMSG723      = $42,
    etIBMAVCAdPcm          = $43,
    etITU_TG726            = $45,
    etMPEG                 = $50,
    etRT23orPAC            = $51,
    etInSoftRT24           = $52,
    etInSoftPAC            = $53,
    etMP3                  = $55,
    etCirrus               = $59,
    etCirrusLogic          = $60,
    etESSTechPCM           = $61,
    etVoxwareInc           = $62,
    CanopusATRAC           = $63,
    etAPICOMG726AdPcm      = $64,
    etAPICOMG722AdPcm      = $65,
    etMicrosoftDSAT        = $66,
    etMSDSATDISPLAY        = $67,
    etXboxAdPcm            = $69,
    etVoxwareAC8           = $70,
    etVoxwareAC10          = $71,
    etVoxwareAC16          = $72,
    etVoxwareAC20          = $73,
    etVoxwareMetaVoice     = $74,
    etVoxwareMetaSound     = $75,
    etVoxwareRT29HW        = $76,
    etVoxwareVR12          = $77,
    etVoxwareVR18          = $78,
    etVoxwareTQ40          = $79,
    etVoxwareSC3A          = $7A,
    etVoxwareSC3B          = $7B,
    etSoundsoft            = $80,
    etVoxwareTQ60          = $81,
    etMicrosoftMSRT24      = $82,
    etATandTG729A          = $83,
    etMP_MVI_MV12          = $84,
    etDF_G726              = $85,
    etDF_GSM610            = $86,
    etItrdSystemsAudio     = $88,
    etOnlive               = $89,
    etM_FTSX20             = $8A,
    etITSASG721AdPcm       = $8B,
    etConvediaG729         = $8C,
    etNSpC_Inc             = $8D,
    etSiemensSBC24         = $91,
    etSF_DolbyAC3APDIF     = $92,
    etMediaSonicG723       = $93,
    etProsody8kbps         = $94,
    etZyXELAdPcm           = $97,
    etPhilipsLPCBB         = $98,
    etStuderProPacked      = $99,
    etMaldenPhonyTalk      = $A0,
    etRacalRecorderGSM     = $A1,
    etRecorderG720a        = $A2,
    etRacalG723_1          = $A3,
    etRacalTetraACELP      = $A4,
    etNECAAC               = $B0,
    etExtended             = $FE,
    etAAC                  = $FF,
    etRhetorexAdPcm        = $100,
    etIBMuLaw              = $101,
    etIBMaLaw              = $102,
    etIBMAdPcm             = $103,
    etVivoG723             = $111,
    etVivoSiren            = $112,
    etCELP                 = $120,
    etGRUNDIG              = $121,
    etDigitalG723          = $123,
    etSanyoLD_AdPcm        = $125,
    etSiproLabACEPLNET     = $130,
    etSL_ACELP4800         = $131,
    etSL_ACELP8V3          = $132,
    etSL_G729              = $133,
    etSL_G729A             = $134,
    etSL_Kelvin            = $135,
    etVoiceAgeAMR          = $136,
    etG726AdPcm            = $140,
    etQC_PureVoice         = $150,
    etQC_HalfRate          = $151,
    etRZS_TUBGSM           = $155,
    etMicrosoftAudio       = $160,
    etWMA_DivX_AC3         = $161,
    etWMA_ProV9            = $162,
    etWMA_LosslessV9       = $163,
    etWMAProOverSPDIF      = $164,
    etUNISYS_AdPcm         = $170,
    etUNISYS_ULAW          = $171,
    etUNISYS_ALAW          = $172,
    etUNISYS_16K           = $173,
    etSYC008_SyCom         = $174,
    etSYC701_G726L         = $175,
    etSYC701_CELP54        = $176,
    etSYC701_CELP68        = $177,
    etKA_AdPcm             = $178,
    etIISMPEG2AAC          = $180,
    etDTS_DS               = $190,
    etCreativeAdPcm        = $200,
    etFastSpeech8          = $202,
    etFastSpeech10         = $203,
    etUHERAdPcm            = $210,
    etUleadDVACM_A         = $215,
    etUleadDVACM_B         = $216,
    etQuarterdeckCorp      = $220,
    etILinkVC              = $230,
    etAurealRawSport       = $240,
    etESSTAC3              = $241,
    etIP_HSX               = $250,
    etIP_RPELP             = $251,
    etConsistentCS2        = $260,
    etSonySCX              = $270,
    etSonySCY              = $271,
    etSonyATRAC3           = $272,
    etSonySPC              = $273,
    etTELUM_TelumInc       = $280,
    etTELUMIA_TelumInc     = $281,
    etNVS_AdPcm            = $285,
    etFMTownsSND           = $300,
    etFujitsu1             = $301,
    etFujitsu2             = $302,
    etFujitsu3             = $303,
    etFujitsu4             = $304,
    etFujitsu5             = $305,
    etFujitsu6             = $306,
    etFujitsu7             = $307,
    etFujitsu8             = $308,
    etMSIncDev             = $350,
    etMSIncCELP833         = $351,
    etBrooktreeDigital     = $400,
    etIntel_IMC            = $401,
    etLigosIndeoAudio      = $402,
    etQDesignMusic         = $450,
    etOn2VP7               = $500,
    etOn2VP6               = $501,
    etATandT_VMPCM         = $680,
    etATandT_TCP           = $681,
    etYMPEGAlpha           = $700,
    etClearJumpLiteWav     = $8AE,
    etOLIGSM               = $1000,
    etOLIAdPcm             = $1001,
    etOLICELP              = $1002,
    etOLISBC               = $1003,
    etOLIOPR               = $1004,
    etLH                   = $1100,
    etLH_CELPcodec         = $1101,
    etLH_SBCcodecA         = $1102,
    etLH_SBCcodecB         = $1103,
    etLH_SBCcodec          = $1104,
    etNorrisCommInc        = $1400,
    etISIAudio             = $1401,
    etATnT_Soundspace      = $1500,
    etVoxWareRT24          = $181C,
    etLucentAX24000P       = $181E,
    etSF_LOSSLESS          = $1971,
    etITI_AdPcm            = $1979,
    etLucentSX8300P        = $1C07,
    etLucentSX5363S        = $1C0C, // G.723 complient
    etCUseeMeDigiTalk      = $1F03,
    etNCTSoftALF2CDACM     = $1FC4,
    etFASTMultimDVM        = $2000,
    etDolbyDTS             = $2001,
    etRealAudio14_4        = $2002,
    etRealAudio28_8        = $2003,
    etRealAudioG28Cook     = $2004,
    etRealAudioMusic       = $2005,
    etRealAudio10RAAC      = $2006,
    etRealAudio10RACP      = $2007,
    etmakeAVIS             = $3313,
    etDivioMPEG4AAC        = $4143,
    etNokiaAdaptiveMR      = $4201,
    etDivioG726            = $4243,
    etLEADSpeech           = $434C,
    etLEADVorbis           = $564C,
    etWavPackAudio         = $5756,
    etOggVorbisMode1       = $674F,
    etOggVorbisMode2       = $6750,
    etOggVorbisMode3       = $6751,
    etOggVorbisMode1p      = $676F,
    etOggVorbisMode2p      = $6770,
    etOggVorbisMode3p      = $6771,
    et3COM_NBX             = $7000,
    etFAAD_AAC             = $706D,
    etGSM_AMR_CBR          = $7A21,
    etGSM_AMR_VBR          = $7A22,
    etComInfosysG723       = $A100,
    etComInfosysAVQSBC     = $A101,
    etComInfosysOLDSBC     = $A102,
    etSymbolTec_G729A      = $A103,
    etVoiceAgeAMRWB        = $A104,
    etIngTech_G726         = $A105,
    etISOMPEG4_AAC         = $A106,
    etEncoreSoft_G726      = $A107,
    etSpeexACMCodec        = $A109,
    etSF_ACM_Codec         = $DFAC,
    etFLAC                 = $F1AC,
    etExtensible           = $FFFE,
    etExperimental         = $FFFF);
{$ENDIF}

  TWavFormatRecord = packed record
    FormatTag       : Word;     // format type
    Channels        : Word;     // number of channels (i.e. mono, stereo, etc.)
    SampleRate      : Cardinal; // sample rate
    BytesPerSecond  : Cardinal; // = SampleRate * BlockAlign
    BlockAlign      : Word;     // block size of data
    BitsPerSample   : Word;     // = 3, 4, 8, 16 or 32 Bits/sample
  end;

  PWavFormatChunkExtensible = ^TWavFormatChunkExtensible;
  TWavFormatChunkExtensible = packed record
    SamplesPerBlock : Word;      // number of samples per channel per Block
    ChMask          : Integer;
    GUID            : TGUID;     // was array [0..71] of Byte;
  end;

  TWavAdPcmCoefficientSet = packed record
    Coefficient : array [0..1] of SmallInt;
  end;

  TWavAdPcmInfoEx =  packed record
    SamplesPerBlock : Word;
    NumCoeff        : Word;
    CoefSets        : array [0..35] of TWavAdPcmCoefficientSet; // is that enough?
  end;

  TAdPcm_State = packed record
    PrevSamp_l : SmallInt;
    Index_l    : Byte;
    PrevSamp_r : SmallInt;
    Index_r    : Byte;
  end;

  TAdPcm_Ms = packed record
    Predictor : array[0..1] of Byte;
    Delta     : array[0..1] of SmallInt;
    Samp1     : array[0..1] of SmallInt;
    Samp2     : array[0..1] of SmallInt;
  end;

implementation

end.
