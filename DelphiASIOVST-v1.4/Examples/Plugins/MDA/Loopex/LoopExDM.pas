unit LoopExDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule;

type
  TLoopExDataModule = class(TVSTModule)
  private
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

end.

(*
class IdleList  //wzDSP objects to receive idle timer (same timer is used for all instances)
{
public:
  IdleList(mdaLooplex *effect, IdleList *next);

  mdaLooplex    *effect;
  IdleList *next;
  bool      remove;

} static idleList(0, 0);  //idleList->next points to start of idle list


#if _WIN32
  #include <windows.h>
  #pragma comment(lib, "user32.lib")
  static UINT timer = 0;
  VOID CALLBACK TimerCallback(HWND hwnd, UINT uMsg, UINT idEvent, DWORD dwTime)
#else //OSX
  #include <Carbon/Carbon.h>
  EventLoopTimerRef timer = 0;
  pascal void TimerCallback(EventLoopTimerRef timerRef, void *userData)
#endif
{
  IdleList *prev = &idleList;
  IdleList *item = prev->next;
  while(item)
  {
    if(item->remove)
    {
      prev->next = item->next;  //remove item from list and close gap
      delete item;
      item = prev->next;

      if(prev == &idleList && prev->next == 0) //stop timer after last item removed
      {
        #if _WIN32
          KillTimer(NULL, timer);
        #else //OSX
          RemoveEventLoopTimer(timer);
        #endif
        timer = 0;
      }
    }
    else
    {
      item->effect->idle();  //call idle() for each item in list
      prev = item;
      item = item->next;
    }
  }
}


IdleList::IdleList(mdaLooplex *effect, IdleList *next) : effect(effect), next(next), remove(false)
{ 
  if(effect && !timer) //start timer
  {
    #if WIN32
      timer = SetTimer(NULL, 0, IDLE_MSEC, TimerCallback);
    #else //OSX
      double ms = kEventDurationMillisecond * (double)IDLE_MSEC;
      InstallEventLoopTimer(GetCurrentEventLoop(), ms, ms, NewEventLoopTimerUPP(TimerCallback), 0, &timer);
    #endif
  }
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


mdaLooplexProgram::mdaLooplexProgram()
{
  param[0]  = 0.00f; //max delay
  param[1]  = 0.00f; //reset (malloc)
  param[2]  = 0.00f; //record (sus ped)
  param[3]  = 1.00f; //input mix
  param[4]  = 0.50f; //input pan
  param[5]  = 1.00f; //feedback (mod whl)
  param[6]  = 1.00f; //out mix
  //param[7]  = 0.00f;

  strcpy (name, "mda Looplex");
}


mdaLooplex::mdaLooplex(audioMasterCallback audioMaster) : AudioEffectX(audioMaster, NPROGS, NPARAMS)
{

  Fs = 44100.0f; //won't we know the sample rate by the time we need it?

  //initialise...
  bypass = bypassed = busy = status = 0; //0=not recorded 1=first pass 2=looping
  bufmax = 882000;
  buffer = new short[bufmax + 10];
  memset(buffer, 0, (bufmax + 10) * sizeof(short));
  bufpos = 0;
  buflen = 0;
  recreq = 0;
  oldParam0 = oldParam1 = oldParam2 = 0.0f;
  modwhl = 1.0f;

  programs = new mdaLooplexProgram[NPROGS];
  if(programs) setProgram(0);

  if(audioMaster)
  {
    setNumInputs(NOUTS);        
    setNumOutputs(NOUTS);
    canProcessReplacing();
    //needIdle(); idle is broken in VST2.4
    setUniqueID('MDA~');  ///
  }

  update();
  suspend();
  
  idleList.next = new IdleList(this, idleList.next); //add to idle list, start timer if not running...
}


void mdaLooplex::update()  //parameter change
{
  if(fabs(param[1] - oldParam1) > 0.1f) 
  {
    oldParam1 = param[1]; 
    if(fabs(oldParam0 - param[0]) > 0.01f)
    {
      oldParam0 = param[0];
      bypass = 1;  //re-assign memory
    }
  }

  if(param[2] > 0.5f && oldParam2 < 0.5f)
  { 
    if(recreq == 0) recreq = 1;
    oldParam2 = param[2];
  }
  if(param[2] < 0.5f && oldParam2 > 0.5f)
  { 
    if(recreq == 1) recreq = 0;
    oldParam2 = param[2];  
  }

  in_mix = 2.0f * param[3] * param[3];
  
  in_pan = param[4];
  
  feedback = param[5];
  
  out_mix = 0.000030517578f * param[6] * param[6];
}


void mdaLooplex::setSampleRate(float sampleRate)
{
  AudioEffectX::setSampleRate(sampleRate);
  Fs = sampleRate;
}


void mdaLooplex::resume()
{    
  //should reset position here...
  bufpos = 0;

  //needIdle();  //idle broken in VST2.4
  DECLARE_VST_DEPRECATED (wantEvents) ();
}


void mdaLooplex::idle()
{
  if(bypassed)
  {
    if(busy) return; //only do once per bypass
    busy = 1; 

    bufmax = 2 * (long)Fs * (long)(10.5f + 190.0f * param[0]);
    if(buffer) delete [] buffer;
    buffer = new short[bufmax + 10];
    if(buffer) memset(buffer, 0, (bufmax + 10) * sizeof(short)); else bufmax = 0;

    bypass = busy = status = bufpos = buflen = 0;
  }
}


mdaLooplex::~mdaLooplex ()  //destroy any buffers...
{
  for(IdleList *item=idleList.next; item; item=item->next)  //remove from idle list, stop timer if last item
  {
    if(item->effect == this) 
    { 
      item->remove = true;
      #if _WIN32 //and stop timer in case our last instance is about to unload
        TimerCallback(0, 0, 0, 0);
      #else
        TimerCallback(0, 0);
      #endif
      break; 
    }
  }

  if(programs) delete [] programs;
  
  if(buffer)
  {
    FILE *fp; //dump loop to file

    if(buflen && (fp = fopen("looplex.wav", "wb")))
    {
      char wh[44];
      memcpy(wh, "RIFF____WAVEfmt \20\0\0\0\1\0\2\0________\4\0\20\0data____", 44);
  
      long l = 36 + buflen * 2;
      wh[4] = (char)(l & 0xFF);  l >>= 8;
      wh[5] = (char)(l & 0xFF);  l >>= 8;
      wh[6] = (char)(l & 0xFF);  l >>= 8;
      wh[7] = (char)(l & 0xFF);

      l = (long)(Fs + 0.5f);
      wh[24] = (char)(l & 0xFF);  l >>= 8;
      wh[25] = (char)(l & 0xFF);  l >>= 8;
      wh[26] = (char)(l & 0xFF);  l >>= 8;
      wh[27] = (char)(l & 0xFF);

      l = 4 * (long)(Fs + 0.5f);
      wh[28] = (char)(l & 0xFF);  l >>= 8;
      wh[29] = (char)(l & 0xFF);  l >>= 8;
      wh[30] = (char)(l & 0xFF);  l >>= 8;
      wh[31] = (char)(l & 0xFF);

      l = buflen * 2;
      wh[40] = (char)(l & 0xFF);  l >>= 8;
      wh[41] = (char)(l & 0xFF);  l >>= 8;
      wh[42] = (char)(l & 0xFF);  l >>= 8;
      wh[43] = (char)(l & 0xFF);
      
      fwrite(wh, 1, 44, fp);

      #if __BIG_ENDIAN__
        char *c = (char * )buffer;
        char t;

        for(l=0; l<buflen; l++) //swap endian-ness
        {
          t = *c;
          *c = *(c + 1);
          c++;
          *c = t;
          c++;
        }
      #endif
      
      fwrite(buffer, sizeof(short), buflen, fp);
      fclose(fp);
    }

    delete [] buffer;
  }
}


void mdaLooplex::setProgram(VstInt32 program)
{
  long i;

  mdaLooplexProgram *p = &programs[program];
  curProgram = program;
  setProgramName(p->name);
  for(i=0; i<NPARAMS; i++) param[i] = p->param[i];
  update();
}


void mdaLooplex::setParameter(VstInt32 index, float value)
{
  mdaLooplexProgram *p = &programs[curProgram];
  param[index] = p->param[index] = value;
  update();
}

bool mdaLooplex::getProgramNameIndexed(VstInt32 category, VstInt32 index, char* text)
{
  if(index<NPROGS)
  {
    strcpy(text, programs[index].name);
    return true;
  }
  return false;
}

bool mdaLooplex::copyProgram(VstInt32 destination)
{
  if(destination<NPROGS)
  {
    programs[destination] = programs[curProgram];
    return true;
  }
  return false;
}

VstInt32 mdaLooplex::canDo(char* text)
{
  if(strcmp(text, "receiveVstEvents") == 0) return 1;
  if(strcmp(text, "receiveVstMidiEvent") == 0) return 1;
  return -1;
}

void mdaLooplex::getParameterDisplay(VstInt32 index, char *text)
{
  char string[16];
  
  switch(index)
  {
    case  0: sprintf(string, "%4d s", (int)(10.5f + 190.0f * param[index])); break; //10 to 200 sec

    case  1: sprintf(string, "%5.1f MB", (float)bufmax / 524288.0f); break;
  
    case  2: if(recreq) strcpy(string, "RECORD"); else strcpy(string, "MONITOR"); break;

    case  3:
    case  6: if(param[index] < 0.01f) strcpy(string, "OFF");
             else sprintf(string, "%.1f dB", 20.0f * log10(param[index] * param[index])); break;

    case  5: if(param[index] < 0.01f) strcpy(string, "OFF");
             else sprintf(string, "%.1f dB", 20.0f * log10(param[index])); break;

    case  4: if(param[index] < 0.505f) 
             {
               if(param[index] > 0.495f) strcpy(string, "C"); else 
                 sprintf(string, "L%.0f", 100.0f - 200.0f * param[index]);
             }
             else sprintf(string, "R%.0f", 200.0f * param[index] - 100.0f); break;

    default: strcpy(string, " ");
  }
  string[8] = 0;
  strcpy(text, (char * )string);
}

void mdaLooplex::process(float **inputs, float **outputs, VstInt32 sampleFrames)
{
  notes[0] = EVENTS_DONE;  //mark events buffer as done
}


void mdaLooplex::processReplacing(float **inputs, float **outputs, VstInt32 sampleFrames)
{
  float* in1 = inputs[0];
  float* in2 = inputs[1];
  float* out1 = outputs[0];
  float* out2 = outputs[1];
  long event=0, frame=0, frames;
  float l, r, dl, dr, d0 = 0.0f, d1;
  float imix = in_mix, ipan = in_pan, omix = out_mix, fb = feedback * modwhl;
  long x;
  
  if((bypassed = bypass)) return;

  while(frame<sampleFrames)
  {
    frames = notes[event++];
    if(frames>sampleFrames) frames = sampleFrames;
    frames -= frame;
    frame += frames;

    while(--frames>=0)
    {
      l = *in1++;
      r = *in2++;

      //input mix
      r *= imix * ipan;
      l *= imix - ipan * imix;

      if(recreq == 1 && status == 0) status = 1; //first pass
      if(recreq == 0 && status == 1) status = 2; //later pass

      if(status)
      {
        //dither
        d1 = d0;
    #if WIN32
          d0 = 0.000061f * (float)(rand() - 16384);
        #else
          d0 = 0.000061f * (float)((rand() & 32767) - 16384);
    #endif
    
        //left delay
        dl = fb * (float)buffer[bufpos];
        if(recreq)
        {
          x = (long)(32768.0f * l + dl + d0 - d1 + 100000.5f) - 100000;
          if(x > 32767) x = 32767; else if(x < -32768) x = -32768;
          buffer[bufpos] = (short)x;
        }
        bufpos++;

        //right delay
        dr = fb * (float)buffer[bufpos];
        if(recreq)        
        {
          x = (long)(32768.0f * r + dr - d0 + d1 + 100000.5f) - 100000;
          if(x > 32767) x = 32767; else if(x < -32768) x = -32768;
          buffer[bufpos] = (short)x;
        }
        bufpos++;
  
        //looping
        if(bufpos >= bufmax)
        { 
          buflen = bufmax;
          bufpos -= buflen;
          status = 2; 
        }
        else
        {
          if(status == 1) buflen = bufpos; else if(bufpos >= buflen) bufpos -= buflen;
        }

        //output
        l += omix * dl;
        r += omix * dr;
      }

      *out1++ = l;
      *out2++ = r;
    }

    if(frame<sampleFrames)
    {
      long note = notes[event++];
      //long vel  = notes[event++];

      if(note == 2)
      {
        bufpos = 0; //resync
      }
      else if(note == 1)
      {
        if(recreq) recreq = 0; else recreq = 1; //toggle recording
      }
    }
  }
  notes[0] = EVENTS_DONE;  //mark events buffer as done
}


VstInt32 mdaLooplex::processEvents(VstEvents* ev)
{
  long npos=0;
  
  for (long i=0; i<ev->numEvents; i++)
  {
    if((ev->events[i])->type != kVstMidiType) continue;
    VstMidiEvent* event = (VstMidiEvent* )ev->events[i];
    char* midiData = event->midiData;
    
    switch(midiData[0] & 0xf0) //status byte (all channels)
    {
      case 0x80: //note off
        //notes[npos++] = event->deltaFrames; //delta
        //notes[npos++] = midiData[1] & 0x7F; //note
        //notes[npos++] = 0;                  //vel
        break;

      case 0x90: //note on
        //notes[npos++] = event->deltaFrames; //delta
        //notes[npos++] = midiData[1] & 0x7F; //note
        //notes[npos++] = midiData[2] & 0x7F; //vel
        break;

      case 0xB0: //controller
        switch(midiData[1])
        {
          case 0x01:  //mod wheel
            modwhl = 1.0f - 0.007874f * (float)midiData[2];
            break;

          case 0x40:  //sustain
            if(midiData[2] > 63)
            {
              notes[npos++] = event->deltaFrames; //delta
              notes[npos++] = 1; //note
              notes[npos++] = 127; //vel
            }
            break;

          case 0x42:  //soft/sost
          case 0x43:
            if(midiData[2] > 63)
            {
              notes[npos++] = event->deltaFrames; //delta
              notes[npos++] = 2; //note
              notes[npos++] = 127; //vel
            }
            break;

          default:  //all notes off
            if(midiData[1]>0x7A) 
            {

            }
            break;
        }
        break;
      
      default: break;
    }

    if(npos>EVENTBUFFER) npos -= 3; //discard events if buffer full!!
    event++;
  }
  notes[npos] = EVENTS_DONE;
  return 1;
}
*)
