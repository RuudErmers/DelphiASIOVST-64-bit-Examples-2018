unit DAV_ChunkWaveCustom;

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
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} {$IFDEF MSWindows} Windows, {$ENDIF} {$ENDIF}
  Classes, SysUtils, DAV_Types, DAV_ChunkClasses, DAV_ChunkWaveBasic,
  DAV_WaveFileTypes;

type
  TWavSDA8Chunk = class(TDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TWavSDAChunk = class(TWavBinaryChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// ITA Header Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TLeRi      = (lrLeft, lrRight);
  TLRSingle  = array [TLeRi] of Single;
  TLRInteger = array [TLeRi] of Integer;

  TdBBlock   = packed record
    OnTopdB  : Double; // dB on top
    Dyn      : Double; // Top [db] - bottom [dB]
    Ref0dB   : Double; // Ref val (eff) for 0 dB
  end;

  TItaHeader = packed record
    SampleCount   : Integer;   // ! Anzahl Samples pro Kanal!!
    SampleRate    : Double;    // ! Abtastrate in Hz
    AdOffset      : Word;      // Zeitsignale : Integer-Offset der AW's
    KanAnzahl     : Byte;      // 1 : Mono, bis zu 31 sind erlaubt
    KanalNr       : Byte;      // aktueller Kanal, 0 bis KanAnzahl - 1
    alleBearb     : Boolean;   // 1 : Editieren wirkt auf alle Kanäle
    Preemph       : Boolean;   // 1 : 50/15 µs Preemph
    LiCursAct     : Boolean;   // 1 : Linker Cursor aktiv
    CursCross     : Boolean;   // 1 : Horizontale Cursorlinie erscheint
    CursLock      : Boolean;   // 1 : Cursor bewegen sich gemeinsam
    DrawMode      : Byte;      // 0 : Punkte,1 : Striche, 2 : Balken
    ODrawMode     : Byte;      // wie 21, aber nur für Ortskurve
    Weighting     : AnsiChar;  // für Spektren : Bewertung als Char 'A'
    ZeroOnly      : Byte;      // derzeit unbenutzt
    NormMax0dB    : Boolean;   // 1 : Spektren und Leistung : 0dB = |Max|
    Yzoom         : Boolean;   // 1 : Y-Bereich auf Maximum normieren
    Xlog          : Boolean;   // 1 : Spektren : Frequenz logarithmisch
    Ylog          : Byte;      // YLog-Bits 1,2,4,8
    Version       : Byte;      // früher 0, jetzt 95
    FFTnorm       : Byte;      // 0 : Leistung, 1 Energie, 2 Passband
    VoltageRange  : Double;    // ! max. Spannung im Zeitsignal

    // Block for internal usage - must be zero!
    PasstInsMem   : Boolean;
    auch0         : Integer;
    inDat         : TLRInteger;
    imMem         : TLRInteger;
    OfsDiffProKan : Integer;
    SegDiffProKan : Word;
    Re            : Pointer;
    fw1           : Word;
    Im            : Pointer;
    fw2           : Word;
    Bq            : Pointer;
    fw3           : Word;
    FltBW         : Single;                    // 0: nix, 1.0: Oktave, 3.0: Terzen
    FltSta        : ShortInt;                  // Startindex bezogen auf 1 kHz
    FltEnd        : ShortInt;                  // Endindex   bezogen auf 1 kHz
    Modus         : Byte;

    Hun           : Byte;                      // Hundertstel
    Sec           : Byte;
    Min           : Byte;
    Hour          : Byte;                      // Entstehungszeit des Signals
    Day           : Byte;
    Month         : Byte;
    Year          : Word;                      // Entstehungsdatum des Signals
    Start         : Integer;                   // (Nr-1) des ersten Wertes in Datei
    xAxUnit       : array [0..2] of AnsiChar;  // Einheit an der X-Achse
    yAxUnit       : array [0..2] of AnsiChar;  // Einheit an der Y-Achse
    Rand          : TLRInteger;                //  dargestellter X-Bereich
    Cursor        : TLRInteger;                // Position der Cursorlinien
    MainDelay     : Double;                    //  [s]
    dBBlock       : TdBblock;                  // bestehend aus folgenden 3 Werten...
    LcursOld      : Word;                      // alt, für Signale bis Länge 65535
    RcursOld      : Word;                      // alt, für Signale bis Länge 65535
    ADDAident     : array [0..19] of AnsiChar; // Quantisierung als ASCII-String
    Comment       : array [0..70] of AnsiChar; // Beliebiges Blabla
  end;

  TWavItaHeaderChunk = class(TWavFixedDefinedChunk)
  protected
    FItaHeader : TItaHeader;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName: TChunkName; override;
  published
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// AFsp Chunk /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TWavAFspChunk = class(TWavChunkText)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property Text: AnsiString read FText write FText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Link Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  // -> see: http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s4_tcm6-10484.pdf

  TBWFLinkChunk = class(TWavChunkText)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property XMLData: AnsiString read FText write FText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// AXML Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  // -> see: http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s5_tcm6-10485.pdf

  TBwfAXMLChunk = class(TWavChunkText)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property XMLData: AnsiString read FText write FText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Display Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TWavDisplayChunk = class(TWavDefinedChunk)
  private
    FData   : AnsiString;
  protected
    FTypeID : Cardinal;
    procedure AssignTo(Dest: TPersistent); override;
  published
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property TypeID: Cardinal read FTypeID write FTypeID;
    property Data: AnsiString read FData write FData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Peak Chunk /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TPeakRecord = record
    Version   : Cardinal; // version of the PEAK chunk
    TimeStamp : Cardinal; // secs since 1/1/1970
  end;

  TWavPeakChunk = class(TWavDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    Peak : TPeakRecord;
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

implementation

{ TWavSDA8Chunk }

constructor TWavSDA8Chunk.Create;
begin
 inherited;
 ChunkFlags := ChunkFlags + [cfPadSize, cfReversedByteOrder];
end;

class function TWavSDA8Chunk.GetClassChunkName: TChunkName;
begin
 Result := 'SDA8';
end;

procedure TWavSDA8Chunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 // not yet defined
end;

procedure TWavSDA8Chunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream
  do Position := Position + FChunkSize;
end;

procedure TWavSDA8Chunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := 0;
 inherited;

 // check and eventually add zero pad
 CheckAddZeroPad(Stream);
end;


{ TWavSDAChunk }

class function TWavSDAChunk.GetClassChunkName: TChunkName;
begin
 Result := 'SDA ';
end;


{ TWavItaHeaderChunk }

constructor TWavItaHeaderChunk.Create;
begin
 inherited;
 StartAddress := @FItaHeader;
end;

procedure TWavItaHeaderChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
end;

class function TWavItaHeaderChunk.GetClassChunkName: TChunkName;
begin
 Result := 'itah';
end;

class function TWavItaHeaderChunk.GetClassChunkSize: Integer;
begin
  Result := 256;
end;


{ TWavAFspChunk }

class function TWavAFspChunk.GetClassChunkName: TChunkName;
begin
 Result := 'afsp';
end;


{ TBWFLinkChunk }

class function TBWFLinkChunk.GetClassChunkName: TChunkName;
begin
 Result := 'link';
end;


{ TBWFAXMLChunk }

class function TBwfAXMLChunk.GetClassChunkName: TChunkName;
begin
 Result := 'axml';
end;


{ TWavDisplayChunk }

constructor TWavDisplayChunk.Create;
begin
 inherited;
 ChunkFlags := ChunkFlags + [cfPadSize];
end;

procedure TWavDisplayChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TWavDisplayChunk then
  begin
   TWavDisplayChunk(Dest).FTypeID := FTypeID;
   TWavDisplayChunk(Dest).FData := FData;
  end;
end;

class function TWavDisplayChunk.GetClassChunkName: TChunkName;
begin
 Result := 'DISP';
end;

procedure TWavDisplayChunk.LoadFromStream(Stream: TStream);
var
  ChunkEnd : Integer;
begin
 inherited;
 // calculate end of stream position
 ChunkEnd := Stream.Position + FChunkSize;
// assert(ChunkEnd <= Stream.Size);

 // read type ID
 Stream.Read(FTypeID, SizeOf(Cardinal));

 // set length of data and read data
 SetLength(FData, FChunkSize - SizeOf(Cardinal));
 Stream.Read(FData[1], Length(FData));

 assert(Stream.Position <= ChunkEnd);

 // goto end of this chunk
 Stream.Position := ChunkEnd;

 // eventually skip padded zeroes
 if cfPadSize in ChunkFlags
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TWavDisplayChunk.SaveToStream(Stream: TStream);
begin
 // calculate chunk size
 FChunkSize := SizeOf(Cardinal) + Length(FData);

 // write basic chunk information
 inherited;

 // write custom chunk information
 with Stream do
  begin
   Write(FTypeID, SizeOf(Cardinal));
   Write(FData[1], FChunkSize - SizeOf(Cardinal));
  end;

 // check and eventually add zero pad
 CheckAddZeroPad(Stream);
end;

{ TWavPeakChunk }

constructor TWavPeakChunk.Create;
begin
 inherited;
 ChunkFlags := ChunkFlags + [cfPadSize];
end;

procedure TWavPeakChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TWavPeakChunk
  then TWavPeakChunk(Dest).Peak := Peak; 
end;

class function TWavPeakChunk.GetClassChunkName: TChunkName;
begin
 Result := 'PEAK';
end;

procedure TWavPeakChunk.LoadFromStream(Stream: TStream);
var
  ChunkEnd : Integer;
begin
 inherited;
 ChunkEnd := Stream.Position + FChunkSize;
 Stream.Read(Peak, SizeOf(TPeakRecord));
 Stream.Position := ChunkEnd;
end;

procedure TWavPeakChunk.SaveToStream(Stream: TStream);
begin
 // calculate chunk size
 FChunkSize := SizeOf(TPeakRecord);

 // write basic chunk information
 inherited;

 // write custom chunk information
 Stream.Write(Peak, FChunkSize);

 // check and eventually add zero pad
 CheckAddZeroPad(Stream);
end;

initialization
  RegisterWaveChunks([TWavItaHeaderChunk, TWavSDA8Chunk, TWavSDAChunk,
    TBWFLinkChunk, TBWFAXMLChunk, TWavDisplayChunk, TWavAFspChunk,
    TWavPeakChunk])

end.
