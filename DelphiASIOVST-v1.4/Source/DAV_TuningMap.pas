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

unit DAV_TuningMap;

//  This is code is the Object Pascal/Delphi version, written by
//  Tobias Fleischer alias Tobybear (tobybear@web.de)
//  Original C++ version by Mark Henning
//  (C)opyright in 2003 by Mark Henning, Germany
//  Contact email: info@anamark.de or mh@homolog.de

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils;

type
  TTuningMap = class
  private
    function GetNoteFreq(Index: Integer): Double; // Absolute tune in Hz
    function GetRelativeTune(Index: Integer): Double; // Relative tune in cents
    procedure SetRelativeTune(Index: Integer; Value: Double);
    procedure SetBaseFreq(const Value: Double); // BaseFreq in Hz
  protected
    FTunes    : array [0..127] of Double;  // Unit: Cents
    FBaseFreq : Double;                    // Unit: Hz
  public
    constructor Create; virtual;
    procedure Reset;

    procedure SaveToFile(const FileName: TFileName; const SaveBaseFrequency: Boolean = False);
    procedure LoadFromFile(const FileName: TFileName);

    property BaseFrequency: Double read FBaseFreq write SetBaseFreq; // Unit: Hz
    property NoteFrequency[Index: Integer]: Double read GetNoteFreq;
    property RelativeTune[Index: Integer]: Double read GetRelativeTune write SetRelativeTune; // Relative tune in cents
  end;

implementation

uses
  Math, Classes;

type
  eSection = (SEC_None, SEC_Unknown, SEC_Tuning, SEC_ExactTuning);

function StripBlanks(s: string): String;
var
  j: Integer;
begin
  if s = '' then exit;
  j := 1;
  while ((s[j] <> #0) and ((s[j] = ' ') or (s[j] = #9))) do Inc(j);
  s := copy(s, j, length(s) - j + 1);
  j := length(s);
  while ((j > 1) and ((s[j] = ' ') or (s[j] = #9))) do Dec(j);
  Result := copy(s, 1, j);
end;

{ TTuningMap }

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

constructor TTuningMap.Create;
begin
 inherited;
 DecimalSeparator := '.';
 Reset;
end;

//////////////////////////////////////////////////////////////////////
// Public Functions
//////////////////////////////////////////////////////////////////////

procedure TTuningMap.Reset;
var
  i: Integer;
begin
 // This function _must_ never produce an error, so we don't need
 // to return a bool value...
 FBaseFreq := 8.1757989156437073336; // Means A = 440Hz
 for i := 0 to 127 do FTunes[i] := 100 * i;
end;

procedure TTuningMap.SaveToFile(const FileName: TFileName; const SaveBaseFrequency: Boolean = False);
var
  i   : Integer;
  ofs : TFileStream;
  s   : String;
const
  endl = #13#10;
begin
  ofs := TFileStream.Create(FileName, fmCreate);
  s := ';' + endl;
  ofs.Write(s[1], length(s));
  s := '; AnaMark / VAZ 1.5 Plus tuning map file' + endl;
  ofs.Write(s[1], length(s));
  s := ';' + endl;
  ofs.Write(s[1], length(s));
  s := ';' + endl;
  ofs.Write(s[1], length(s));
  s := '; 1. VAZ-section with quantized tunings' + endl;
  ofs.Write(s[1], length(s));
  s := ';' + endl;
  ofs.Write(s[1], length(s));
  s := '[Tuning]' + endl;
  ofs.Write(s[1], length(s));

  for i := 0 to 127 do
   begin
    s := 'note ' + IntToStr(i) + ' = ' + IntToStr(round(FTunes[i])) + endl;
    ofs.Write(s[1], length(s));
   end;

  s := ';' + endl;
  ofs.Write(s[1], length(s));
  s := '; 2. AnaMark-specific section with exact tunings' + endl;
  ofs.Write(s[1], length(s));
  s := ';' + endl;
  ofs.Write(s[1], length(s));
  s := '[Exact Tuning]' + endl;
  ofs.Write(s[1], length(s));

  if (SaveBaseFrequency) then
   begin
    s := 'basefreq = ' + FloatToStr(FBaseFreq) + endl;
    ofs.Write(s[1], length(s));
   end;

  for i := 0 to 127 do
   begin
    s := 'note ' + IntToStr(i) + ' = ' + FloatToStr(round(FTunes[i])) + endl;
    ofs.Write(s[1], length(s));
   end;

  FreeAndNil(ofs);
end;

procedure TTuningMap.LoadFromFile(const FileName: TFileName);
var
  secCurr: eSection;
  lTunes: array[0..127] of Longint; // For temporary use, unit: Cents
  bTuningFound, bExactTuningFound: Boolean;
  lNoteIndex, nCurrPos, i, j, lET_LastNoteFound, lLineCount: Longint;
  ifs: TFileStream;
  pstr: array[0..255] of Char;
  ch: Char;
  p: Double;
  tunes: array[0..127] of Double;
  szParam, szValue, szLine, szCurr: String;

  procedure ImportScala(FileName: TFileName);
  var
    numnotes: Integer;
  var
    f: textfile;
    s1, s2, s: String;
    d: Double;
    j, k, lc: Longint;
  begin
    assignfile(f, FileName);
    system.reset(f);
    for lc := 0 to 127 do
      tunes[lc] := 0;
    lc := 0;
    numnotes := 0;
    repeat
      readln(f, s);
      if (s = '') then
        s := '!';
      s := stripblanks(s);
      if (s[1] = '!') or (s[1] = ';') then
      else
        Inc(lc);

      if lc = 1 then
      else
      if (lc = 2) and (numnotes = 0) then
        numnotes := StrToInt(s)
      else if ((lc > 2) and (length(s) > 0) and (s[1] <> '!') and (s[1] <> ';')) then
       begin
        j := pos('/', s);
        k := pos('.', s);

        if (j > 0) and ((k = 0) or (j < k)) then
         begin
          s1 := copy(s, 1, j - 1);
          s2 := copy(s, j + 1, length(s) - j);
          s1 := stripblanks(s1);
          s2 := stripblanks(s2);
          k := pos(' ', s2);
          if k > 0 then
            s2 := copy(s2, 1, k - 1);
          d := StrToFloat(s2);
          if d = 0 then
            d := 1;
          tunes[lc - 3] := 1200 * log2(StrToFloat(s1) / d);
         end else
        if (k >= 0) and ((j = 0) or (k < j)) then
         begin
          s := stripblanks(s);
          k := pos(' ', s);
          if k > 0 then
            s := copy(s, 1, k - 1);
          s := stripblanks(s);
          tunes[lc - 3] := StrToFloat(s);
         end;
       end;
    until EOF(f);

    lc := -1;
    k := (lc);
    while k > numnotes - 1 do
      k := k - numnotes;
    while k < 0 do
      k := k + numnotes;
    d := 1200 * (((lc + 1) div numnotes) - 1) + tunes[k];

    FTunes[lc + 1] := d;

    for lc := 0 to 126 do
     begin
      k := lc;
      while k > numnotes - 1 do
        k := k - numnotes;
      while k < 0 do
        k := k + numnotes;

      d := 1200 * (lc div numnotes) + tunes[k];
      FTunes[lc + 1] := d;
     end;

    closefile(f);
  end;

begin
 secCurr := SEC_None;
 bTuningFound := False;
 bExactTuningFound := False;
 lET_LastNoteFound := -1;
 lLineCount := 0;

 // Initialize data
 // Important, because notes not listed in the tuning file
 // should always have standard tuning.
 Reset;
 for i := 0 to 127 do lTunes[i] := round(FTunes[i]);

  if uppercase(extractfileext(FileName)) = '.SCL'
   then ImportScala(FileName)
   else
    begin
     // Now open the file
     ifs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
     if not assigned(ifs)
      then raise Exception.CreateFmt('Error opening the file: %s', [FileName]);

     while (ifs.position < ifs.size) do
      begin
       // Increase Line counter to make it easier detecting errors, if
       // a more detailed output is wanted.
       Inc(lLineCount);

       // Read line, until '\n', '\r' or '\0' is reached
       // Thus it is able to read WIN/DOS-style as well as UNIX-style files
       // By the way: Skip empty lines or multiple line-end-characters
       // Is not case sensitive, so all chars are converted to lower ones
       nCurrPos := 0;
       szLine := '';
       repeat
        while ((ifs.position < ifs.size) and (nCurrPos < 510)) do
         begin
          ch := ' ';
          ifs.Read(ch, 1);
          if ((ch = #0) or (ch = #10) or (ch = #13)) then
            break;
          szLine := szLine + ch;
          Inc(nCurrPos);
         end
       until ((ifs.position >= ifs.size) or (nCurrPos <> 0));
       if (nCurrPos >= 510) then
        begin
         FreeAndNil(ifs);
         raise Exception.CreateFmt('Line too long (line %d)', [lLineCount]);
        end;
       if szLine = '' then continue;

       szLine := lowercase(szLine);
       szLine[nCurrPos + 1] := #0;

       szCurr := szLine;

       // Skip empty lines
       if (szCurr = #0) then continue;

       // Skip leading and trailing spaces/tabs
       szCurr := StripBlanks(szCurr);

       // Skip comment lines
       if (szCurr[1] = ';') then Continue;

       // Check for new section
       if (szCurr[1] = '[') then
        begin
         if (szCurr[length(szCurr)] <> ']') then
          begin
           FreeAndNil(ifs);
           raise Exception.CreateFmt('Syntax error: Section-tag must be the only string in the line! (line %d)', [lLineCount]);
          end;
         // Known section found?
         secCurr := SEC_Unknown;
         if (szCurr = '[tuning]') then
          begin
           secCurr := SEC_Tuning;
           bTuningFound := True;
          end;
         if (szCurr = '[exact tuning]') then
          begin
           secCurr := SEC_ExactTuning;
           bExactTuningFound := True;
          end;

         // Now process next line
         Continue;
        end;

       // Skip all lines which are in none or in an unknown section
       if ((secCurr = SEC_None) or (secCurr = SEC_Unknown)) then Continue;

       // Separate parameter name and value
       j := pos('=', szCurr);
       if j < 1 then
        begin
         FreeAndNil(ifs);
         raise Exception.CreateFmt('Syntax error: "=" missing! (line %s)', [lLineCount]);
        end;
       szParam := copy(szCurr, 1, j - 1);
       szValue := copy(szCurr, j + 1, length(szCurr) - j);

       // Skip leading and trailing spaces/tabs
       szParam := StripBlanks(szParam);
       szValue := StripBlanks(szValue);

       // Now process the different sections:
       case secCurr of
        SEC_Tuning :
          if (copy(szParam, 1, 4) = 'note') then
           begin
            // Get MIDI-Note number
            lNoteIndex := StrToInt(copy(szParam, 5, length(szParam) - 4));
            // Check for correct range [0;127] and ignore it, if it's out of range.
            if ((lNoteIndex >= 0) and (lNoteIndex <= 127))
             then lTunes[lNoteIndex] := StrToInt(szValue);
           end;// Check for note-tag

        SEC_ExactTuning :
         begin
          // Check for note-tag
          if (copy(szParam, 1, 4) = 'note') then
           begin
            // note-tag found
            // Get MIDI-Note number
            lNoteIndex := StrToInt(copy(szParam, 5, length(szParam) - 4));

            // Check for correct range [0;127] and ignore it, if it's out of range.
            if ((lNoteIndex >= 0) and (lNoteIndex <= 127)) then
              FTunes[lNoteIndex] := StrToFloat(szValue);

            if (lET_LastNoteFound < lNoteIndex) then
              lET_LastNoteFound := lNoteIndex;
           end;
          // Check for basefreq parameter
          if (copy(szParam, 1, 8) = 'basefreq')
           then FBaseFreq := StrToFloat(szValue); // basefreq found
         end;
       end;
     end;

    if ((not bTuningFound) and (not bExactTuningFound)) then
     begin
      FreeAndNil(ifs);
      raise Exception.Create('No tuning data found!');
     end;

    if (not bExactTuningFound) then
     for i := 0 to 127
      do FTunes[i] := lTunes[i] // There are no exact tuning values, so map the quantized
     // values to the exact ones:
    else
    if ((lET_LastNoteFound >= 0) and (lET_LastNoteFound < 127)) then
     begin
      // Now loop the given data (auto expand):
      j := lET_LastNoteFound; // Highest MIDI note number
      P := FTunes[j];     // Period length
      for i := j to 127 do
        FTunes[i] := FTunes[i - j] + P;
     end;// [Exact Tuning] section found, so ignore the values found
    // in the [Tuning] section and do the "auto expand":

    FreeAndNil(ifs);
   end;
end;

function TTuningMap.GetNoteFreq(Index: Integer): Double;
begin
  Result := FBaseFreq * Power(2, GetRelativeTune(Index) / 1200);
end;

function TTuningMap.GetRelativeTune(Index: Integer): Double;
begin
 // First make sure, that the note index is in the valid range
 // If not, return a "standard value"
  if ((Index >= 0) and (Index <= 127))
   then Result := FTunes[Index]
   else Result := 100 * Index;
end;

procedure TTuningMap.SetBaseFreq(const Value: Double);
var
  pstr: array[0..255] of Char;
begin
 // First make sure, that the base frequency is in the valid range
  if (Value > 0)
   then FBaseFreq := Value
   else raise Exception.CreateFmt('Base frequency out of range: %s', [FloatToStr(Value)]);
end;

procedure TTuningMap.SetRelativeTune(Index: Integer; Value: Double);
begin
 // First make sure, that the note index is in the valid range
 // If not, return false;
 if ((Index >= 0) and (Index <= 127))
  then FTunes[Index] := Value
  else raise Exception.CreateFmt('Note index out of range: %d', [Index]);
end;

end.
