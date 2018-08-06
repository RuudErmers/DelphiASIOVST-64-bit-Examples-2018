{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DAV_Common_Lazarus; 

interface

uses
  DAV_Common, DAV_CommonRegister, DAV_Complex, DAV_ComplexData, DAV_AudioData, 
  DAV_Approximations, DAV_MpegAudioLayer3, DAV_AudioFile, DAV_AudioFileAIFF, 
  DAV_AudioFileAU, DAV_AudioFileDataCache, DAV_AudioFileWAV, 
  DAV_ChannelDataCoder, DAV_ChunkAiffBasic, DAV_ChunkClasses, 
  DAV_ChunkWaveBasic, DAV_ChunkWaveCustom, DAV_MpegAudio, DAV_Math, 
  DAV_MiniFloat, DAV_FixedPoint, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_CommonRegister', @DAV_CommonRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_Common_Lazarus', @Register); 
end.
