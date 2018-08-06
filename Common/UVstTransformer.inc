{$ifdef VSTTransformerVirtCC}
// Don't Edit this code. It was generated with PrjMidiMapping
// Make sure you update the Arduino code !!
const ControllerNames:array[0..127] of string = ('Unassigned','Modulation Wheel','Breath Controller','Mute','','Glide Range','','Volume','Poly Range','Poly Mode','OSC1 Course','OSC1 Semi','OSC1 Phase','OSC1 Wave','OSC1 Volume','OSC1 Reserved 2','OSC1 Phase Depth','OSC1 Phase Select','OSC1 Modulation Depth','OSC1 Modulation Select','OSC2 Course','OSC2 Semi','OSC2 Phase','OSC2 Wave','OSC2 Volume','OSC2 Detune','OSC2 Phase Depth','OSC2 Phase Select','OSC2 Modulation Depth','OSC2 Modulation Select','OSC3 Course','OSC3 Semi','OSC3 Phase','OSC3 Wave','OSC3 Volume','OSC3 Detune','OSC3 Phase Depth','OSC3 Phase Select','OSC3 Modulation Depth','OSC3 Modulation Select','OSC4 Course','OSC4 Semi','OSC4 Phase','OSC4 Wave','OSC4 Volume','OSC4 Detune','OSC4 Phase Depth','OSC4 Phase Select','OSC4 Modulation Depth','OSC4 Modulation Select','LFO1 Wave','LFO1 Delay','LFO1 Delay Enable','LFO1 Rate ','LFO1 Reserved 2','LFO2 Wave','LFO2 Delay','LFO2 Delay Enable','LFO2 Rate ','LFO2 Reserved 2','VCA A','VCA D','VCA S','VCA R','Hold Pedal','Glide On Off','VCA Range','VCF Modulation Depth','VCF Modulation Select','VCA Modulation Depth','VCA Modulation Select','VCF Resonance','VCF Range','VCF Filter Type','VCF Cutoff','VCF_ADSR_A','VCF_ADSR_D','VCF_ADSR_S','VCF_ADSR_R','VCF Key Range','Delay Amount','Delay Time','Reverb Amount','Reverb Time','Chorus Depth','Chorus Rate','Phaser Depth','Phaser Rate','Delay Feedback','Reverb Feedback','','','','','','','EffectsSettings','','MOD_ADSR_A','MOD_ADSR_D','MOD_ADSR_S','MOD_ADSR_R','OSC1_SYNC','OSC2_SYNC','OSC3_SYNC','OSC4_SYNC','','Fat','','','','Noise Color','Noise Volume','Ring Modulation','LFOVCOKnobControl','LFOVCFKnobControl','VCF12_24db','','','','Unavailable (Used as Crumar Virtcc_Transpose)','Unavailable (Used as Crumar Virtcc_Velocity)','Unavailable (Used as Crumar VirtCC_Browse)','Unavailable (Used As Crumar VirtCC_ARPSpeed)','Unavailable (Used As Crumar VirtCC_ARMMode)','Unavailable (Used as Crumar VirtCC_ARPON)','Unavailable (Used in RC as DigFlags)','Unavailable (Used in RC as KeyInfinite');

const DIG_NON_SELECTED = 1024;
const CrumarCC_IS_INVALID = -10;
const VirtCC_WHEEL_MOD = 1;
const VirtCC_PITCH_MOD = 2;
const VirtCC_Mute = 3;
const VirtCC_GLIDE_RANGE = 5;
const VirtCC_MAIN_VOLUME = 7;
const VirtCC_POLYRANGE = 8;
const VirtCC_POLYMODE = 9;
const VirtCC_OSC1_FOOT = 10;
const VirtCC_OSC1_SEMI = 11;
const VirtCC_OSC1_Phase = 12;
const VirtCC_OSC1_WAVE = 13;
const VirtCC_OSC1_VOLUME = 14;
const VirtCC_OSC1_PHASEDEPTH = 16;
const VirtCC_OSC1_PHASESELECT = 17;
const VirtCC_OSC1_MODULATION_DEPTH = 18;
const VirtCC_OSC1_MODULATION_SELECT = 19;
const VirtCC_OSC2_FOOT = 20;
const VirtCC_OSC2_SEMI = 21;
const VirtCC_OSC2_Phase = 22;
const VirtCC_OSC2_WAVE = 23;
const VirtCC_OSC2_VOLUME = 24;
const VirtCC_OSC2_Detune = 25;
const VirtCC_OSC2_PHASEDEPTH = 26;
const VirtCC_OSC2_PHASESELECT = 27;
const VirtCC_OSC2_MODULATION_DEPTH = 28;
const VirtCC_OSC2_MODULATION_SELECT = 29;
const VirtCC_OSC3_FOOT = 30;
const VirtCC_OSC3_SEMI = 31;
const VirtCC_OSC3_Phase = 32;
const VirtCC_OSC3_WAVE = 33;
const VirtCC_OSC3_VOLUME = 34;
const VirtCC_OSC3_Detune = 35;
const VirtCC_OSC3_PHASEDEPTH = 36;
const VirtCC_OSC3_PHASESELECT = 37;
const VirtCC_OSC3_MODULATION_DEPTH = 38;
const VirtCC_OSC3_MODULATION_SELECT = 39;
const VirtCC_OSC4_FOOT = 40;
const VirtCC_OSC4_SEMI = 41;
const VirtCC_OSC4_Phase = 42;
const VirtCC_OSC4_WAVE = 43;
const VirtCC_OSC4_VOLUME = 44;
const VirtCC_OSC4_Detune = 45;
const VirtCC_OSC4_PHASEDEPTH = 46;
const VirtCC_OSC4_PHASESELECT = 47;
const VirtCC_OSC4_MODULATION_DEPTH = 48;
const VirtCC_OSC4_MODULATION_SELECT = 49;
const VirtCC_LFO1_WAVE = 50;
const VirtCC_LFO1_DELAY = 51;
const VirtCC_LFO1_DELAYENABLED = 52;
const VirtCC_LFO1_RATE = 53;
const VirtCC_LFO2_WAVE = 55;
const VirtCC_LFO2_DELAY = 56;
const VirtCC_LFO2_DELAYENABLED = 57;
const VirtCC_LFO2_RATE = 58;
const VirtCC_VCA_ADSR_A = 60;
const VirtCC_VCA_ADSR_D = 61;
const VirtCC_VCA_ADSR_S = 62;
const VirtCC_VCA_ADSR_R = 63;
const VirtCC_HoldPedal = 64;
const VirtCC_Glide_OnOff = 65;
const VirtCC_VCA_RANGE = 66;
const VirtCC_VCF_MODULATION_DEPTH = 67;
const VirtCC_VCF_MODULATION_SELECT = 68;
const VirtCC_VCA_MODULATION_DEPTH = 69;
const VirtCC_VCA_MODULATION_SELECT = 70;
const VirtCC_VCF_RESONANCE = 71;
const VirtCC_VCF_RANGE = 72;
const VirtCC_Filter_Type = 73;
const VirtCC_VCF_CUTOFF = 74;
const VirtCC_VCF_ADSR_A = 75;
const VirtCC_VCF_ADSR_D = 76;
const VirtCC_VCF_ADSR_S = 77;
const VirtCC_VCF_ADSR_R = 78;
const VirtCC_VCF_KEYRANGE = 79;
const VirtCC_Effects0 = 80;
const VirtCC_DELAY_PARAM1 = 81;
const VirtCC_REVERB_DEPTH = 82;
const VirtCC_REVERB_PARAM1 = 83;
const VirtCC_CHORUS_DEPTH = 84;
const VirtCC_CHORUS_RATE = 85;
const VirtCC_PHASER_DEPTH = 86;
const VirtCC_PHASER_RATE = 87;
const VirtCC_DELAY_PARAM2 = 88;
const VirtCC_REVERB_PARAM2 = 89;
const VirtCC_EffectsMax = 95;
const VirtCC_EFFECTSETTINGS = 96;
const VirtCC_MOD_ADSR_A = 98;
const VirtCC_MOD_ADSR_D = 99;
const VirtCC_MOD_ADSR_S = 100;
const VirtCC_MOD_ADSR_R = 101;
const VirtCC_OSC1_SYNC = 102;
const VirtCC_OSC2_SYNC = 103;
const VirtCC_OSC3_SYNC = 104;
const VirtCC_OSC4_SYNC = 105;
const VirtCC_ALL_FAT = 107;
const VirtCC_Noise_Color = 111;
const VirtCC_Noise_Volume = 112;
const VirtCC_RingModLevel = 113;
const VirtCC_LFOVCOKnobControl = 114;
const VirtCC_LFOVCFKnobControl = 115;
const VirtCC_VCF12_24db = 116;
const VirtCC_DigFlags = 126;
const VirtCC_KeyInfinite = 127;
type VirtCCDefItem = record
                       text,desc,shape:string;
                     end;
const VirtCCDef: array[0..127] of VirtCCDefItem = (
(text: '';desc: 'Unassigned';shape: ''),
(text: '';desc: 'Modulation Wheel';shape: ''),
(text: '';desc: 'Breath Controller';shape: ''),
(text: '';desc: 'Mute';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'Glide Range';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'Volume';shape: ''),
(text: '';desc: 'Poly Range';shape: ''),
(text: '';desc: 'Poly Mode';shape: ''),
(text: '-3,-2,-1,0,1,2,3';desc: 'O1 Pitch';shape: 'F'),
(text: '-7,+7';desc: 'OSC1 Semi';shape: 'S'),
(text: '';desc: 'OSC1 Phase';shape: ''),
(text: 'Saw,Square,Sine,Triangle,HPulse,Qpulse,TriSaw,Noise';desc: 'O1 Wave';shape: 'W'),
(text: '';desc: 'OSC1 Volume';shape: ''),
(text: '';desc: 'OSC1 Reserved 2';shape: ''),
(text: '';desc: 'OSC1 Phase Depth';shape: ''),
(text: '';desc: 'OSC1 Phase Select';shape: 'T'),
(text: '';desc: 'OSC1 Modulation Depth';shape: ''),
(text: 'none,lfo1,lfo2,lfo1+lfo2';desc: 'OSC1 Modulation Select';shape: 'T'),
(text: '-3,-2,-1,0,1,2,3';desc: 'O2 Pitch';shape: 'F'),
(text: '';desc: 'OSC2 Semi';shape: 'S'),
(text: '';desc: 'OSC2 Phase';shape: ''),
(text: 'Saw,Square,Sine,Triangle,HPulse,Qpulse,TriSaw,Noise';desc: 'O2 Wave';shape: 'W'),
(text: '';desc: 'OSC2 Volume';shape: ''),
(text: '';desc: 'OSC2 Detune';shape: ''),
(text: '';desc: 'OSC2 Phase Depth';shape: ''),
(text: '';desc: 'OSC2 Phase Select';shape: 'T'),
(text: '';desc: 'OSC2 Modulation Depth';shape: ''),
(text: '';desc: 'OSC2 Modulation Select';shape: 'T'),
(text: '-3,-2,-1,0,1,2,3';desc: 'O3 Pitch';shape: 'F'),
(text: '';desc: 'OSC3 Semi';shape: 'S'),
(text: '';desc: 'OSC3 Phase';shape: ''),
(text: 'Saw,Square,Sine,Triangle,HPulse,Qpulse,TriSaw,Noise';desc: 'O3 Wave';shape: 'W'),
(text: '';desc: 'OSC3 Volume';shape: ''),
(text: '';desc: 'OSC3 Detune';shape: ''),
(text: '';desc: 'OSC3 Phase Depth';shape: ''),
(text: '';desc: 'OSC3 Phase Select';shape: 'T'),
(text: '';desc: 'OSC3 Modulation Depth';shape: ''),
(text: '';desc: 'OSC3 Modulation Select';shape: 'T'),
(text: '-3,-2,-1,0,1,2,3';desc: 'O4 Pitch';shape: 'F'),
(text: '';desc: 'OSC4 Semi';shape: 'S'),
(text: '';desc: 'OSC4 Phase';shape: ''),
(text: '';desc: 'O4 Noise';shape: 'N'),
(text: '';desc: 'OSC4 Volume';shape: ''),
(text: '';desc: 'OSC4 Detune';shape: ''),
(text: '';desc: 'OSC4 Phase Depth';shape: ''),
(text: '';desc: 'OSC4 Phase Select';shape: 'T'),
(text: '';desc: 'OSC4 Modulation Depth';shape: ''),
(text: '';desc: 'OSC4 Modulation Select';shape: 'T'),
(text: 'Saw,Squ,Tri,Noise';desc: 'L1 Wave';shape: 'L'),
(text: '';desc: 'LFO1 Delay';shape: ''),
(text: '';desc: 'LFO1 Delay Enable';shape: ''),
(text: '';desc: 'LFO1 Rate ';shape: ''),
(text: '';desc: 'LFO1 Reserved 2';shape: ''),
(text: 'Saw,Squ,Tri,Noise';desc: 'L2 Wave';shape: 'L'),
(text: '';desc: 'LFO2 Delay';shape: ''),
(text: '';desc: 'LFO2 Delay Enable';shape: ''),
(text: '';desc: 'LFO2 Rate ';shape: ''),
(text: '';desc: 'LFO2 Reserved 2';shape: ''),
(text: '';desc: 'VCA A';shape: ''),
(text: '';desc: 'VCA D';shape: ''),
(text: '';desc: 'VCA S';shape: ''),
(text: '';desc: 'VCA R';shape: ''),
(text: '';desc: 'Hold Pedal';shape: ''),
(text: '';desc: 'Glide On Off';shape: ''),
(text: '';desc: 'VCA Range';shape: ''),
(text: '';desc: 'VCF Modulation Depth';shape: ''),
(text: '';desc: 'VCF Modulation Select';shape: 'T'),
(text: '';desc: 'VCA Modulation Depth';shape: ''),
(text: '';desc: 'VCA Modulation Select';shape: 'T'),
(text: '';desc: 'VCF Resonance';shape: ''),
(text: '';desc: 'VCF Range';shape: ''),
(text: '';desc: 'VCF Filter Type';shape: ''),
(text: '';desc: 'VCF Cutoff';shape: ''),
(text: '';desc: 'VCF_ADSR_A';shape: ''),
(text: '';desc: 'VCF_ADSR_D';shape: ''),
(text: '';desc: 'VCF_ADSR_S';shape: ''),
(text: '';desc: 'VCF_ADSR_R';shape: ''),
(text: '';desc: 'VCF Key Range';shape: ''),
(text: '';desc: 'Delay Amount';shape: ''),
(text: '';desc: 'Delay Time';shape: ''),
(text: '';desc: 'Reverb Amount';shape: ''),
(text: '';desc: 'Reverb Time';shape: ''),
(text: '';desc: 'Chorus Depth';shape: ''),
(text: '';desc: 'Chorus Rate';shape: ''),
(text: '';desc: 'Phaser Depth';shape: ''),
(text: '';desc: 'Phaser Rate';shape: ''),
(text: '';desc: 'Delay Feedback';shape: ''),
(text: '';desc: 'Reverb Feedback';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'EffectsSettings';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'MOD_ADSR_A';shape: ''),
(text: '';desc: 'MOD_ADSR_D';shape: ''),
(text: '';desc: 'MOD_ADSR_S';shape: ''),
(text: '';desc: 'MOD_ADSR_R';shape: ''),
(text: '';desc: 'OSC1_SYNC';shape: ''),
(text: '';desc: 'OSC2_SYNC';shape: ''),
(text: '';desc: 'OSC3_SYNC';shape: ''),
(text: '';desc: 'OSC4_SYNC';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'Fat';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'Noise Color';shape: ''),
(text: '';desc: 'Noise Volume';shape: ''),
(text: '';desc: 'Ring Modulation';shape: ''),
(text: '';desc: 'LFOVCOKnobControl';shape: ''),
(text: '';desc: 'LFOVCFKnobControl';shape: ''),
(text: '';desc: 'VCF12_24db';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: '';shape: ''),
(text: '';desc: 'Unavailable (Used as Crumar Virtcc_Transpose)';shape: ''),
(text: '';desc: 'Unavailable (Used as Crumar Virtcc_Velocity)';shape: ''),
(text: '';desc: 'Unavailable (Used as Crumar VirtCC_Browse)';shape: ''),
(text: '';desc: 'Unavailable (Used As Crumar VirtCC_ARPSpeed)';shape: ''),
(text: '';desc: 'Unavailable (Used As Crumar VirtCC_ARMMode)';shape: ''),
(text: '';desc: 'Unavailable (Used as Crumar VirtCC_ARPON)';shape: ''),
(text: '';desc: 'Unavailable (Used in RC as DigFlags)';shape: ''),
(text: '';desc: 'Unavailable (Used in RC as KeyInfinite';shape: ''));
{$endif}
const DUNECC_BANKSELECTLOW = 0;
const DUNECC_MODULATIONWHEEL = 1;
const DUNECC_PORTAMENTO = 5;
const DUNEPARM_PORTAMENTO = 78;
const DUNECC_6 = 6;
const DUNECC_MIX_MIX = 8;
const DUNEPARM_MIX_MIX = 12;
const DUNECC_LFO1_RATE = 14;
const DUNEPARM_LFO1_RATE = 34;
const DUNECC_LFO1_WAVE = 15;
const DUNEPARM_LFO1_WAVE = 33;
const DUNECC_LFO1SYNC = 16;
const DUNECC_LFO1SKEW = 17;
const DUNECC_LFO1_DELAY = 18;
const DUNEPARM_LFO1_DELAY = 36;
const DUNECC_LFO2_RATE = 19;
const DUNEPARM_LFO2_RATE = 43;
const DUNECC_LFO2_WAVE = 20;
const DUNEPARM_LFO2_WAVE = 42;
const DUNECC_LFO2SYNC = 21;
const DUNECC_LFO2SKEW = 22;
const DUNECC_LFO2_DELAY = 23;
const DUNEPARM_LFO2_DELAY = 45;
const DUNECC_OSC1_FINE = 24;
const DUNEPARM_OSC1_FINE = 2;
const DUNECC_MODATTACK = 26;
const DUNEPARM_MODATTACK = 57;
const DUNECC_MODDECAY = 27;
const DUNEPARM_MODDECAY = 58;
const DUNECC_MODSUSTAIN = 28;
const DUNEPARM_MODSUSTAIN = 59;
const DUNECC_MODRELEASE = 29;
const DUNEPARM_MODRELEASE = 60;
const DUNECC_VCO1_WAVE = 30;
const DUNEPARM_VCO1_WAVE = 0;
const DUNECC_VCO2_WAVE = 31;
const DUNEPARM_VCO2_WAVE = 5;
const DUNECC_BANKSELECTHI = 32;
const DUNECC_OSC2_FINE = 34;
const DUNEPARM_OSC2_FINE = 7;
const DUNECC_OSCSYNC = 35;
const DUNECC_VCA_ADSR_D = 36;
const DUNEPARM_VCA_ADSR_D = 19;
const DUNECC_VCA_ADSR_S = 37;
const DUNEPARM_VCA_ADSR_S = 20;
const DUNECC_VCF_ADSR_A = 38;
const DUNEPARM_VCF_ADSR_A = 22;
const DUNECC_VCF_ADSR_D = 39;
const DUNEPARM_VCF_ADSR_D = 23;
const DUNECC_VCF_ADSR_S = 40;
const DUNEPARM_VCF_ADSR_S = 24;
const DUNECC_VCF_ADSR_R = 41;
const DUNEPARM_VCF_ADSR_R = 25;
const DUNECC_VCF_RESONANCE = 42;
const DUNEPARM_VCF_RESONANCE = 29;
const DUNECC_VCF_RANGE = 43;
const DUNEPARM_VCF_RANGE = 30;
const DUNECC_VCF_KEYRANGE = 46;
const DUNEPARM_VCF_KEYRANGE = 26;
const DUNECC_ARPON = 47;
const DUNECC_MONOON = 48;
const DUNEPARM_MONOON = 127;
const DUNECC_ARPRATE = 50;
const DUNECC_VOICES = 51;
const DUNECC_ARPNOTELENGTH = 52;
const DUNECC_DETUNE = 53;
const DUNECC_PANSPREAD = 54;
const DUNECC_MM13 = 55;
const DUNEPARM_MM13 = 172;
const DUNECC_MM14 = 56;
const DUNEPARM_MM14 = 173;
const DUNECC_MM15 = 57;
const DUNEPARM_MM15 = 174;
const DUNECC_MM16 = 58;
const DUNEPARM_MM16 = 175;
const DUNECC_MM17 = 59;
const DUNEPARM_MM17 = 176;
const DUNECC_MM18 = 60;
const DUNEPARM_MM18 = 177;
const DUNECC_MM19 = 61;
const DUNEPARM_MM19 = 178;
const DUNECC_MM20 = 62;
const DUNEPARM_MM20 = 179;
const DUNECC_MM21 = 63;
const DUNEPARM_MM21 = 180;
const DUNECC_SUSTAINPEDAL = 64;
const DUNECC_MM22 = 65;
const DUNEPARM_MM22 = 181;
const DUNECC_MM23 = 66;
const DUNEPARM_MM23 = 182;
const DUNECC_MM24 = 67;
const DUNEPARM_MM24 = 183;
const DUNECC_FMAMOUNT1 = 70;
const DUNECC_RINGMODULATION = 71;
const DUNECC_VCA_ADSR_R = 72;
const DUNEPARM_VCA_ADSR_R = 21;
const DUNECC_VCA_ADSR_A = 73;
const DUNEPARM_VCA_ADSR_A = 18;
const DUNECC_VCF_CUTOFF = 74;
const DUNEPARM_VCF_CUTOFF = 28;
const DUNECC_OSC1_COARSE = 77;
const DUNEPARM_OSC1_COARSE = 1;
const DUNECC_OSC2_COARSE = 78;
const DUNEPARM_OSC2_COARSE = 6;
const DUNECC_PULSEWIDTH = 79;
const DUNECC_LFO3RATE = 80;
const DUNECC_LFO3WAVEFORM = 81;
const DUNECC_LFO3SYNC = 82;
const DUNECC_LFO3SKEW = 83;
const DUNECC_LFO3FADEIN = 84;
const DUNECC_CHORUSENABLE = 85;
const DUNEPARM_CHORUSENABLE = 116;
const DUNECC_DELAY_ENABLE = 86;
const DUNEPARM_DELAY_ENABLE = 117;
const DUNECC_REVERB_ENABLE = 87;
const DUNEPARM_REVERB_ENABLE = 118;
const DUNECC_EQ1GAIN = 89;
const DUNECC_EQ2GAIN = 90;
const DUNECC_REVERB_AMOUNT = 91;
const DUNEPARM_REVERB_AMOUNT = 94;
const DUNECC_DELAY_AMOUNT = 92;
const DUNEPARM_DELAY_AMOUNT = 88;
const DUNECC_CHORUS_AMOUNT = 93;
const DUNEPARM_CHORUS_AMOUNT = 84;
const DUNECC_DISTORTIONAMOUN = 94;
const DUNECC_OSC1LEVEL = 96;
const DUNECC_OSC2LEVEL = 97;
const DUNECC_DELAY_RATEL = 98;
const DUNEPARM_DELAY_RATEL = 89;
const DUNECC_DELAY_RATER = 99;
const DUNEPARM_DELAY_RATER = 90;
const DUNECC_DELAY_FEEDBACK = 100;
const DUNEPARM_DELAY_FEEDBACK = 87;
const DUNECC_CHORUSRATE = 101;
const DUNEPARM_CHORUSRATE = 80;
const DUNECC_OSC1_FAT = 102;
const DUNEPARM_OSC1_FAT = 3;
const DUNECC_OSC2_FAT = 103;
const DUNEPARM_OSC2_FAT = 8;
const DUNECC_REVERB_PREDELAY = 104;
const DUNEPARM_REVERB_PREDELAY = 95;
const DUNECC_REVERB_DAMP = 105;
const DUNEPARM_REVERB_DAMP = 92;
const DUNECC_FMAMOUNT2 = 106;
const DUNECC_MIX_OSC3 = 107;
const DUNEPARM_MIX_OSC3 = 11;
const DUNECC_VCO3_WAVE = 108;
const DUNEPARM_VCO3_WAVE = 10;
const DUNECC_MIX_NOISE = 109;
const DUNEPARM_MIX_NOISE = 14;
const DUNECC_NOISE_WAVE = 110;
const DUNEPARM_NOISE_WAVE = 13;
const DUNECC_MIX_VOLUME = 111;
const DUNEPARM_MIX_VOLUME = 74;
const DUNECC_MMAMOUNT1 = 112;
const DUNECC_MMAMOUNT2 = 113;
const DUNECC_MMAMOUNT3 = 114;
const DUNECC_MMAMOUNT4 = 115;
const DUNECC_MMAMOUNT5 = 116;
const DUNECC_MMAMOUNT6 = 117;
const DUNECC_MMAMOUNT7 = 118;
const DUNECC_MMAMOUNT8 = 119;
const VirtualToPhysDUNE: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,48,77,-1,102,30,96,-1,10,11,12,13,78,-1,103,31,97,-1,-1,-1,-1,-1,-1,-1,-1,108,107,-1,-1,-1,-1,-1,-1,-1,-1,-1,109,-1,-1,-1,-1,-1,15,18,-1,14,-1,20,23,-1,19,-1,73,36,37,72,64,-1,111,-1,-1,-1,-1,42,43,-1,74,38,39,40,41,46,92,98,91,104,93,101,-1,-1,100,105,-1,-1,-1,-1,-1,-1,-1,-1,26,27,28,29,-1,-1,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualDUNE: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,-1,16,17,18,19,53,50,-1,-1,51,58,55,-1,-1,56,-1,-1,98,99,100,101,13,23,-1,-1,-1,-1,61,62,75,76,77,78,71,72,-1,-1,79,-1,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,-1,-1,-1,-1,-1,-1,-1,63,60,74,-1,-1,10,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,82,80,84,-1,-1,14,24,81,-1,88,85,12,22,83,89,-1,34,33,44,111,66,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountDUNE = 184;
const ParamToPhysDUNE: array[0..ParamCountDUNE-1] of integer = (
30,77,24,102,-1,31,78,34,103,-1,108,107,8,110,109,-1,-1,-1,73,36,37,72,38,39,40,41,46,-1,74,42,43,-1,-1,15,14,-1,18,-1,-1,-1,-1,-1,20,19,-1,23,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,26,27,28,29,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,111,-1,-1,-1,5,-1,101,-1,-1,-1,93,-1,-1,100,92,98,99,-1,105,-1,91,104,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,85,86,87,-1,-1,-1,-1,-1,-1,-1,-1,48,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,55,56,57,58,59,60,61,62,63,65,66,67);
const PhysToParamDUNE: array[0..127] of integer = (-1,-1,-1,-1,-1,78,-1,-1,12,-1,-1,-1,-1,-1,34,33,-1,-1,36,43,42,-1,-1,45,2,-1,57,58,59,60,0,5,-1,-1,7,-1,19,20,22,23,24,25,29,30,-1,-1,26,-1,127,-1,-1,-1,-1,-1,-1,172,173,174,175,176,177,178,179,180,-1,181,182,183,-1,-1,-1,-1,21,18,28,-1,-1,1,6,-1,-1,-1,-1,-1,-1,116,117,118,-1,-1,-1,94,88,84,-1,-1,-1,-1,89,90,87,80,3,8,95,92,-1,11,10,14,13,74,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerDUNE}
const DUNENames:array[0..127] of string = (
   'Bank Select Low','Modulation Wheel','','','','Portamento','6','','MIX_Mix','','','','','','LFO1_RATE','LFO1_WAVE',
   'LFO 1 Sync','LFO 1 Skew','LFO1_DELAY','LFO2_RATE','LFO2_WAVE','LFO 2 Sync','LFO 2 Skew','LFO2_DELAY','Osc1_Fine','','Mod Attack','Mod Decay','Mod Sustain','Mod Release','VCO1_WAVE','VCO2_WAVE',
   'Bank Select HI','','Osc2_Fine','Osc Sync','VCA_ADSR_D','VCA_ADSR_S','VCF_ADSR_A','VCF_ADSR_D','VCF_ADSR_S','VCF_ADSR_R','VCF_RESONANCE','VCF_RANGE','','','VCF_KEYRANGE','Arp On',
   'MonoOn','','Arp Rate','Voices','Arp Note Length','Detune','Pan Spread','MM 13','MM 14','MM 15','MM 16','MM 17','MM 18','MM 19','MM 20','MM 21',
   'Sustain Pedal','MM 22','MM 23','MM 24','','','FM Amount 1','Ring Modulation','VCA_ADSR_R','VCA_ADSR_A','VCF_CUTOFF','','','OSC1_Coarse','OSC2_Coarse','Pulse Width',
   'LFO 3 Rate','LFO 3 Waveform','LFO 3 Sync','LFO 3 Skew','LFO 3 Fade In','Chorus Enable','Delay_Enable','Reverb_Enable','','EQ 1 Gain','EQ 2 Gain','Reverb_Amount','Delay_Amount','Chorus_Amount','Distortion Amoun','',
   'Osc 1 Level','Osc 2 Level','Delay_RateL','Delay_RateR','Delay_Feedback','Chorus Rate','OSC1_FAT','OSC2_FAT','Reverb_PreDelay','Reverb_Damp','FM Amount 2','MIX_Osc3','VCO3_WAVE','MIX_Noise','NOISE_WAVE','MIX_Volume',
   'MM Amount 1','MM Amount 2','MM Amount 3','MM Amount 4','MM Amount 5','MM Amount 6','MM Amount 7','MM Amount 8','','','','','','','',''
);

function TVSTTransformerDUNE.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysDUNE)) and (vcc<=high(VirtualToPhysDUNE))
    then
    begin
      result:=VirtualToPhysDUNE[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerDUNE.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualDUNE)) and (pcc<=high(PhysToVirtualDUNE))
    then
  result:=PhysToVirtualDUNE[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerDUNE.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamDUNE)) and (pcc<=high(PhysToParamDUNE))
    then
  result:=PhysToParamDUNE[pcc]
    else
  result:=-1;
end;

function TVSTTransformerDUNE.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysDUNE)) and (paramnr<=high(ParamToPhysDUNE))
    then
  result:=ParamToPhysDUNE[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerDUNE.ControllerName(pcc:integer):string;
begin
  result:=DUNENames[pcc]
end;

function TVSTTransformerDUNE.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [8,14,48,55,56,57,58,59,60,61,62,63,65,66,67,85,86,87,93,98,99,100,101,104,105] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const SYLENTHCC_MODWHEEL = 1;
const SYLENTHCC_POLYPHONY = 4;
const SYLENTHPARM_POLYPHONY = 139;
const SYLENTHCC_PORTAMENTO = 5;
const SYLENTHPARM_PORTAMENTO = 141;
const SYLENTHCC_MIXA = 8;
const SYLENTHPARM_MIXA = 69;
const SYLENTHCC_MIXB = 9;
const SYLENTHPARM_MIXB = 70;
const SYLENTHCC_OSC1_VOLUME = 10;
const SYLENTHPARM_OSC1_VOLUME = 91;
const SYLENTHCC_OSCA1PHASE = 11;
const SYLENTHPARM_OSCA1PHASE = 87;
const SYLENTHCC_OSC1DETUNE = 12;
const SYLENTHPARM_OSC1DETUNE = 81;
const SYLENTHCC_OSCA1STEREO = 13;
const SYLENTHPARM_OSCA1STEREO = 89;
const SYLENTHCC_OSCA1PAN = 14;
const SYLENTHPARM_OSCA1PAN = 86;
const SYLENTHCC_OSC2_VOLUME = 15;
const SYLENTHPARM_OSC2_VOLUME = 103;
const SYLENTHCC_OSCA2PHASE = 16;
const SYLENTHPARM_OSCA2PHASE = 99;
const SYLENTHCC_OSC2DETUNE = 17;
const SYLENTHPARM_OSC2DETUNE = 93;
const SYLENTHCC_OSCA2STEREO = 18;
const SYLENTHPARM_OSCA2STEREO = 101;
const SYLENTHCC_OSCA2PAN = 19;
const SYLENTHPARM_OSCA2PAN = 98;
const SYLENTHCC_OSC3_VOLUME = 20;
const SYLENTHPARM_OSC3_VOLUME = 115;
const SYLENTHCC_LFO2AMOUNT2 = 21;
const SYLENTHPARM_LFO2AMOUNT2 = 204;
const SYLENTHCC_OSC3DETUNE = 22;
const SYLENTHPARM_OSC3DETUNE = 105;
const SYLENTHCC_OSCB1STEREO = 23;
const SYLENTHPARM_OSCB1STEREO = 113;
const SYLENTHCC_OSCB1PAN = 24;
const SYLENTHPARM_OSCB1PAN = 110;
const SYLENTHCC_OSC4_VOLUME = 25;
const SYLENTHPARM_OSC4_VOLUME = 127;
const SYLENTHCC_LFO1SELECT1 = 26;
const SYLENTHPARM_LFO1SELECT1 = 229;
const SYLENTHCC_OSC4DETUNE = 27;
const SYLENTHPARM_OSC4DETUNE = 117;
const SYLENTHCC_OSCB2STEREO = 28;
const SYLENTHPARM_OSCB2STEREO = 125;
const SYLENTHCC_OSCB2PAN = 29;
const SYLENTHPARM_OSCB2PAN = 122;
const SYLENTHCC_LFO1_WAVE = 30;
const SYLENTHPARM_LFO1_WAVE = 62;
const SYLENTHCC_LFO1AMOUNT1 = 31;
const SYLENTHPARM_LFO1AMOUNT1 = 201;
const SYLENTHCC_LFO1AMOUNT2 = 33;
const SYLENTHPARM_LFO1AMOUNT2 = 202;
const SYLENTHCC_LFO1SELECT2 = 34;
const SYLENTHPARM_LFO1SELECT2 = 230;
const SYLENTHCC_LFO2_WAVE = 35;
const SYLENTHPARM_LFO2_WAVE = 67;
const SYLENTHCC_LFO2AMOUNT1 = 36;
const SYLENTHPARM_LFO2AMOUNT1 = 203;
const SYLENTHCC_LFO2SELECT1 = 37;
const SYLENTHPARM_LFO2SELECT1 = 231;
const SYLENTHCC_FILTERTYPEA = 38;
const SYLENTHPARM_FILTERTYPEA = 46;
const SYLENTHCC_LFO2SELECT2 = 39;
const SYLENTHPARM_LFO2SELECT2 = 232;
const SYLENTHCC_MODENV1A = 40;
const SYLENTHPARM_MODENV1A = 71;
const SYLENTHCC_MODENV1D = 41;
const SYLENTHPARM_MODENV1D = 72;
const SYLENTHCC_MODENV1S = 42;
const SYLENTHPARM_MODENV1S = 74;
const SYLENTHCC_MODENV1R = 43;
const SYLENTHPARM_MODENV1R = 73;
const SYLENTHCC_MODENV1AMOUNT1 = 44;
const SYLENTHPARM_MODENV1AMOUNT1 = 197;
const SYLENTHCC_MODENV1SELECT1 = 45;
const SYLENTHPARM_MODENV1SELECT1 = 225;
const SYLENTHCC_MODENV1AMOUNT2 = 46;
const SYLENTHPARM_MODENV1AMOUNT2 = 198;
const SYLENTHCC_MODENV1SELECT2 = 47;
const SYLENTHPARM_MODENV1SELECT2 = 226;
const SYLENTHCC_OSC1OCTAVE = 48;
const SYLENTHPARM_OSC1OCTAVE = 85;
const SYLENTHCC_OSC1WAVE = 49;
const SYLENTHPARM_OSC1WAVE = 92;
const SYLENTHCC_MODENV2A = 50;
const SYLENTHPARM_MODENV2A = 75;
const SYLENTHCC_MODENV2D = 51;
const SYLENTHPARM_MODENV2D = 76;
const SYLENTHCC_MODENV2S = 52;
const SYLENTHPARM_MODENV2S = 78;
const SYLENTHCC_MODENV2R = 53;
const SYLENTHPARM_MODENV2R = 77;
const SYLENTHCC_MODENV2AMOUNT1 = 54;
const SYLENTHPARM_MODENV2AMOUNT1 = 199;
const SYLENTHCC_MODENV2SELECT1 = 55;
const SYLENTHPARM_MODENV2SELECT1 = 227;
const SYLENTHCC_MODENV2AMOUNT2 = 56;
const SYLENTHPARM_MODENV2AMOUNT2 = 200;
const SYLENTHCC_MODENV2SELECT2 = 57;
const SYLENTHPARM_MODENV2SELECT2 = 228;
const SYLENTHCC_OSC2OCTAVE = 58;
const SYLENTHPARM_OSC2OCTAVE = 97;
const SYLENTHCC_OSC2WAVE = 59;
const SYLENTHPARM_OSC2WAVE = 104;
const SYLENTHCC_OSC3OCTAVE = 60;
const SYLENTHPARM_OSC3OCTAVE = 109;
const SYLENTHCC_OSC3WAVE = 61;
const SYLENTHPARM_OSC3WAVE = 116;
const SYLENTHCC_OSC4OCTAVE = 62;
const SYLENTHPARM_OSC4OCTAVE = 121;
const SYLENTHCC_OSC4_WAVE = 63;
const SYLENTHPARM_OSC4_WAVE = 128;
const SYLENTHCC_HOLDPEDAL = 64;
const SYLENTHCC_CHORUSRATE = 65;
const SYLENTHPARM_CHORUSRATE = 19;
const SYLENTHCC_CHORUSDRYWET = 66;
const SYLENTHPARM_CHORUSDRYWET = 16;
const SYLENTHCC_FILTERADRIVE = 67;
const SYLENTHCC_FILTERBDRIVE = 68;
const SYLENTHCC_VCF_A_RESONANCE = 69;
const SYLENTHCC_VCF_B_RESONANCE = 70;
const SYLENTHCC_VCF_RESONANCE = 71;
const SYLENTHPARM_VCF_RESONANCE = 56;
const SYLENTHCC_VCF_A_CUTOFF = 72;
const SYLENTHCC_VCF_B_CUTOFF = 73;
const SYLENTHPARM_VCF_B_CUTOFF = 48;
const SYLENTHCC_VCF_CUTOFF = 74;
const SYLENTHPARM_VCF_CUTOFF = 54;
const SYLENTHCC_VCF_KEYRANGE = 75;
const SYLENTHPARM_VCF_KEYRANGE = 55;
const SYLENTHCC_VCA_ADSR_A_A = 76;
const SYLENTHPARM_VCA_ADSR_A_A = 0;
const SYLENTHCC_VCA_ADSR_A_D = 77;
const SYLENTHPARM_VCA_ADSR_A_D = 1;
const SYLENTHCC_VCA_ADSR_A_S = 78;
const SYLENTHPARM_VCA_ADSR_A_S = 3;
const SYLENTHCC_VCA_ADSR_A_R = 79;
const SYLENTHPARM_VCA_ADSR_A_R = 2;
const SYLENTHCC_VCA_ADSR_B_A = 80;
const SYLENTHCC_VCA_ADSR_B_D = 81;
const SYLENTHCC_VCA_ADSR_B_S = 82;
const SYLENTHCC_VCA_ADSR_B_R = 83;
const SYLENTHCC_LFO1RATE = 84;
const SYLENTHPARM_LFO1RATE = 61;
const SYLENTHCC_LFO1_GAIN = 85;
const SYLENTHPARM_LFO1_GAIN = 59;
const SYLENTHCC_LFO1OFFSET = 86;
const SYLENTHCC_LFO2RATE = 87;
const SYLENTHPARM_LFO2RATE = 66;
const SYLENTHCC_LFO2_GAIN = 88;
const SYLENTHPARM_LFO2_GAIN = 64;
const SYLENTHCC_LFO2OFFSET = 89;
const SYLENTHCC_OSC1_NOTE = 90;
const SYLENTHPARM_OSC1_NOTE = 84;
const SYLENTHCC_OSC2_NOTE = 91;
const SYLENTHPARM_OSC2_NOTE = 96;
const SYLENTHCC_OSC3_NOTE = 92;
const SYLENTHPARM_OSC3_NOTE = 108;
const SYLENTHCC_OSC4_NOTE = 93;
const SYLENTHPARM_OSC4_NOTE = 120;
const SYLENTHCC_DELAYFEEDBACK = 94;
const SYLENTHPARM_DELAYFEEDBACK = 26;
const SYLENTHCC_REVERBSIZE = 95;
const SYLENTHPARM_REVERBSIZE = 145;
const SYLENTHCC_REVERBDAMP = 96;
const SYLENTHPARM_REVERBDAMP = 142;
const SYLENTHCC_CHORUSOFFON = 97;
const SYLENTHPARM_CHORUSOFFON = 218;
const SYLENTHCC_SELECTPARTA = 102;
const SYLENTHPARM_SELECTPARTA = 237;
const SYLENTHCC_KEYTRACK = 103;
const SYLENTHCC_MONOLEGATO = 104;
const SYLENTHCC_DELAYLEFT = 105;
const SYLENTHPARM_DELAYLEFT = 32;
const SYLENTHCC_DELAYRIGHT = 106;
const SYLENTHPARM_DELAYRIGHT = 33;
const SYLENTHCC_DELAYDRYWET = 107;
const SYLENTHPARM_DELAYDRYWET = 25;
const SYLENTHCC_DELAYOFFON = 108;
const SYLENTHPARM_DELAYOFFON = 220;
const SYLENTHCC_REVERBDRYWET = 109;
const SYLENTHPARM_REVERBDRYWET = 143;
const SYLENTHCC_REVERBOFFON = 110;
const SYLENTHPARM_REVERBOFFON = 224;
const SYLENTHCC_PHASERRATE = 111;
const SYLENTHPARM_PHASERRATE = 133;
const SYLENTHCC_PHASERDEPTH = 112;
const SYLENTHPARM_PHASERDEPTH = 132;
const SYLENTHCC_PHASERDRYWET = 113;
const SYLENTHPARM_PHASERDRYWET = 130;
const SYLENTHCC_PHASEROFFON = 114;
const SYLENTHPARM_PHASEROFFON = 223;
const SYLENTHCC_PORTAMODE = 115;
const SYLENTHPARM_PORTAMODE = 140;
const SYLENTHCC_MAINVOLUME = 116;
const SYLENTHPARM_MAINVOLUME = 68;
const SYLENTHCC_FILTERADB = 117;
const SYLENTHPARM_FILTERADB = 47;
const SYLENTHCC_VOLUME = 118;
const SYLENTHCC_CHORUSDEPTH = 119;
const SYLENTHPARM_CHORUSDEPTH = 15;
const SYLENTHCC_ARPON = 120;
const SYLENTHPARM_ARPON = 217;
const SYLENTHCC_ARPSPEED = 121;
const SYLENTHPARM_ARPSPEED = 11;
const SYLENTHCC_ARPGATE = 122;
const SYLENTHPARM_ARPGATE = 8;
const SYLENTHCC_ARPMODE = 123;
const SYLENTHPARM_ARPMODE = 9;
const VirtualToPhysSYLENTH: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,4,48,90,-1,49,10,-1,31,26,33,34,58,91,-1,59,15,-1,36,37,21,39,60,92,-1,61,20,-1,44,45,46,47,62,93,-1,63,25,-1,54,55,56,57,30,-1,85,84,-1,35,-1,88,87,-1,76,77,78,79,-1,115,116,-1,-1,-1,-1,71,-1,38,74,40,41,42,43,75,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,50,51,52,53,-1,-1,-1,-1,-1,12,-1,-1,-1,73,-1,-1,-1,-1,117,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualSYLENTH: array[0..127] of integer = (-1,1,-1,-1,9,5,-1,-1,-1,-1,14,-1,107,-1,-1,24,-1,-1,-1,-1,34,28,-1,-1,-1,44,17,-1,-1,-1,50,16,-1,18,19,55,26,27,73,29,75,76,77,78,36,37,38,39,10,13,98,99,100,101,46,47,48,49,20,23,30,33,40,43,-1,-1,-1,-1,-1,-1,-1,71,-1,111,74,79,60,61,62,63,-1,-1,-1,-1,53,52,-1,58,57,-1,11,21,31,41,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,65,66,116,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountSYLENTH = 238;
const ParamToPhysSYLENTH: array[0..ParamCountSYLENTH-1] of integer = (
76,77,79,78,-1,-1,-1,-1,122,123,-1,121,-1,-1,-1,119,66,-1,-1,65,-1,-1,-1,-1,-1,107,94,-1,-1,-1,-1,-1,105,106,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,38,117,73,-1,-1,-1,-1,-1,74,75,71,-1,-1,85,-1,84,30,-1,88,-1,87,35,116,8,9,40,41,43,42,50,51,53,52,-1,-1,12,-1,-1,90,48,14,11,-1,13,-1,10,49,17,-1,-1,91,58,19,16,-1,18,-1,15,59,22,-1,-1,92,60,24,-1,-1,23,-1,20,61,27,-1,-1,93,62,29,-1,-1,28,-1,25,63,-1,113,-1,112,111,-1,-1,-1,-1,-1,4,115,5,96,109,-1,95,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,44
,46,54,56,31,33,36,21,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,120,97,-1,108,-1,-1,114,110,45,47,55,57,26,34,37,39,-1,-1,-1,-1,102);
const PhysToParamSYLENTH: array[0..127] of integer = (-1,-1,-1,-1,139,141,-1,-1,69,70,91,87,81,89,86,103,99,93,101,98,115,204,105,113,110,127,229,117,125,122,62,201,-1,202,230,67,203,231,46,232,71,72,74,73,197,225,198,226,85,92,75,76,78,77,199,227,200,228,97,104,109,116,121,128,-1,19,16,-1,-1,-1,-1,56,-1,48,54,55,0,1,3,2,-1,-1,-1,-1,61,59,-1,66,64,-1,84,96,108,120,26,145,142,218,-1,-1,-1,-1,237,-1,-1,32,33,25,220,143,224,133,132,130,223,140,68,47,-1,15,217,11,8,9,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerSYLENTH}
const SYLENTHNames:array[0..127] of string = (
   '','Modwheel','','','Polyphony','Portamento','','','Mix A','Mix B','OSC1_VOLUME','Osc A1 Phase','OSC1 Detune','Osc A1 Stereo','Osc A1 Pan','OSC2_VOLUME',
   'Osc A2 Phase','Osc2 Detune','Osc A2 Stereo','Osc A2 Pan','OSC3_VOLUME','LFO2 Amount 2','Osc 3 Detune','Osc B1 Stereo','Osc B1 Pan','OSC4_VOLUME','LFO1 Select 1','Osc4 Detune','Osc B2 Stereo','Osc B2 Pan','LFO1_WAVE','LFO1 Amount 1',
   '','LFO1 Amount 2','LFO1 Select 2','LFO2_WAVE','LFO2 Amount 1','LFO2 Select 1','FilterTypeA','LFO2 Select 2','MOD ENV1 A','MOD ENV1 D','MOD ENV1 S','MOD ENV1 R','MODENV1 AMOUNT1','MOD ENV1 Select 1','MOD ENV1 Amount 2','MOD ENV1 Select 2',
   'OSC1 Octave','OSC1 Wave','MOD ENV2 A','MOD ENV2 D','MOD ENV2 S','MOD ENV2 R','MODENV2AMOUNT1','MOD ENV2 Select 1','MODENV2AMOUNT2','MOD ENV2 Select 2','OSC2 Octave','OSC2 Wave','OSC3 Octave','OSC3 Wave','OSC4 Octave','OSC4_WAVE',
   'Hold Pedal','Chorus Rate','Chorus Dry/Wet','Filter A Drive','Filter B Drive','VCF_A_RESONANCE','VCF_B_RESONANCE','VCF_RESONANCE','VCF_A_CUTOFF','VCF_B_CUTOFF','VCF_CUTOFF','VCF_KEYRANGE','VCA_ADSR_A_A','VCA_ADSR_A_D','VCA_ADSR_A_S','VCA_ADSR_A_R',
   'VCA_ADSR_B_A','VCA_ADSR_B_D','VCA_ADSR_B_S','VCA_ADSR_B_R','LFO 1 Rate','LFO1_GAIN','LFO 1 Offset','LFO 2 Rate','LFO2_GAIN','LFO 2 Offset','OSC1_NOTE','OSC2_NOTE','OSC3_NOTE','OSC4_NOTE','Delay Feedback','Reverb Size',
   'Reverb Damp','Chorus Off/On','','','','','Select Part A','Keytrack','Mono legato','Delay Left','Delay Right','Delay Dry/Wet','Delay Off/On','Reverb Dry/Wet','Reverb Off/On','Phaser Rate',
   'Phaser Depth','Phaser Dry/Wet','Phaser Off/On','Portamode','Main Volume','FilterADB','Volume','Chorus Depth','Arp On','Arp Speed','Arp Gate','Arp Mode','','','',''
);

function TVSTTransformerSYLENTH.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysSYLENTH)) and (vcc<=high(VirtualToPhysSYLENTH))
    then
    begin
      result:=VirtualToPhysSYLENTH[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerSYLENTH.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualSYLENTH)) and (pcc<=high(PhysToVirtualSYLENTH))
    then
  result:=PhysToVirtualSYLENTH[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerSYLENTH.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamSYLENTH)) and (pcc<=high(PhysToParamSYLENTH))
    then
  result:=PhysToParamSYLENTH[pcc]
    else
  result:=-1;
end;

function TVSTTransformerSYLENTH.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysSYLENTH)) and (paramnr<=high(ParamToPhysSYLENTH))
    then
  result:=ParamToPhysSYLENTH[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerSYLENTH.ControllerName(pcc:integer):string;
begin
  result:=SYLENTHNames[pcc]
end;

function TVSTTransformerSYLENTH.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [1,4,5,8,9,10,12,15,17,20,21,25,26,30,31,33,34,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,65,66,94,95,96,97,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const MINIMONSTACC_BANKSELECT0_32 = 0;
const MINIMONSTACC_MODWHEEL = 1;
const MINIMONSTAPARM_MODWHEEL = 4;
const MINIMONSTACC_GLIDEAMOUNT = 5;
const MINIMONSTAPARM_GLIDEAMOUNT = 69;
const MINIMONSTACC_VOLUME = 7;
const MINIMONSTAPARM_VOLUME = 339;
const MINIMONSTACC_PAN = 10;
const MINIMONSTAPARM_PAN = 354;
const MINIMONSTACC_DELAYTIMEL = 12;
const MINIMONSTAPARM_DELAYTIMEL = 384;
const MINIMONSTACC_DELAYTIMER = 13;
const MINIMONSTAPARM_DELAYTIMER = 399;
const MINIMONSTACC_DELAYFEEDBACK = 14;
const MINIMONSTAPARM_DELAYFEEDBACK = 414;
const MINIMONSTACC_VCAENVSUSTAIN = 15;
const MINIMONSTAPARM_VCAENVSUSTAIN = 324;
const MINIMONSTACC_OSC1WAVEFORM = 16;
const MINIMONSTAPARM_OSC1WAVEFORM = 49;
const MINIMONSTACC_OSC2WAVEFORM = 17;
const MINIMONSTAPARM_OSC2WAVEFORM = 51;
const MINIMONSTACC_OSC3WAVEFORM = 18;
const MINIMONSTAPARM_OSC3WAVEFORM = 53;
const MINIMONSTACC_VCFENVATKTIME = 20;
const MINIMONSTAPARM_VCFENVATKTIME = 249;
const MINIMONSTACC_VCFENVDCYTIME = 21;
const MINIMONSTAPARM_VCFENVDCYTIME = 264;
const MINIMONSTACC_VCFENVSUSTAIN = 22;
const MINIMONSTAPARM_VCFENVSUSTAIN = 279;
const MINIMONSTACC_MODMIX = 23;
const MINIMONSTAPARM_MODMIX = 114;
const MINIMONSTACC_POLYPHONYMODE = 24;
const MINIMONSTAPARM_POLYPHONYMODE = 2;
const MINIMONSTACC_UNISON = 25;
const MINIMONSTAPARM_UNISON = 3;
const MINIMONSTACC_NOISESWITCH = 26;
const MINIMONSTAPARM_NOISESWITCH = 61;
const MINIMONSTACC_OSC2TRANSPOSE = 27;
const MINIMONSTAPARM_OSC2TRANSPOSE = 129;
const MINIMONSTACC_OSC3TRANSPOSE = 28;
const MINIMONSTAPARM_OSC3TRANSPOSE = 144;
const MINIMONSTACC_GLIDEMODE = 65;
const MINIMONSTAPARM_GLIDEMODE = 68;
const MINIMONSTACC_NOISECOLOR = 67;
const MINIMONSTAPARM_NOISECOLOR = 57;
const MINIMONSTACC_DECAYMODE = 69;
const MINIMONSTAPARM_DECAYMODE = 64;
const MINIMONSTACC_VCFCONTOUR = 70;
const MINIMONSTAPARM_VCFCONTOUR = 234;
const MINIMONSTACC_VCFEMPHASIS = 71;
const MINIMONSTAPARM_VCFEMPHASIS = 34;
const MINIMONSTACC_VCAENVDCYTIME = 72;
const MINIMONSTAPARM_VCAENVDCYTIME = 309;
const MINIMONSTACC_VCAENVATKTIME = 73;
const MINIMONSTAPARM_VCAENVATKTIME = 294;
const MINIMONSTACC_VCFCUTOFF = 74;
const MINIMONSTAPARM_VCFCUTOFF = 19;
const MINIMONSTACC_OSC1MIX = 75;
const MINIMONSTAPARM_OSC1MIX = 159;
const MINIMONSTACC_OSC2MIX = 76;
const MINIMONSTAPARM_OSC2MIX = 174;
const MINIMONSTACC_OSC3MIX = 77;
const MINIMONSTAPARM_OSC3MIX = 189;
const MINIMONSTACC_NOISEMIX = 78;
const MINIMONSTAPARM_NOISEMIX = 204;
const MINIMONSTACC_EXTINPUTMIX = 79;
const MINIMONSTAPARM_EXTINPUTMIX = 219;
const MINIMONSTACC_OSC1SWITCH = 80;
const MINIMONSTAPARM_OSC1SWITCH = 58;
const MINIMONSTACC_OSC2SWITCH = 81;
const MINIMONSTAPARM_OSC2SWITCH = 59;
const MINIMONSTACC_OSC3SWITCH = 82;
const MINIMONSTAPARM_OSC3SWITCH = 60;
const MINIMONSTACC_EXTINPUTSWITCH = 83;
const MINIMONSTAPARM_EXTINPUTSWITCH = 62;
const MINIMONSTACC_FEEDBACK = 84;
const MINIMONSTAPARM_FEEDBACK = 63;
const MINIMONSTACC_OSCMODULATION = 85;
const MINIMONSTAPARM_OSCMODULATION = 55;
const MINIMONSTACC_VCFMODULATION = 86;
const MINIMONSTAPARM_VCFMODULATION = 65;
const MINIMONSTACC_VCFKBDCTRL1 = 87;
const MINIMONSTAPARM_VCFKBDCTRL1 = 66;
const MINIMONSTACC_VCFKBDCTRL2 = 88;
const MINIMONSTAPARM_VCFKBDCTRL2 = 67;
const MINIMONSTACC_OSC3CONTROL = 89;
const MINIMONSTAPARM_OSC3CONTROL = 56;
const MINIMONSTACC_TUNE = 90;
const MINIMONSTAPARM_TUNE = 84;
const MINIMONSTACC_DELAYAMOUNT = 91;
const MINIMONSTAPARM_DELAYAMOUNT = 369;
const MINIMONSTACC_OSC1RANGE = 92;
const MINIMONSTAPARM_OSC1RANGE = 50;
const MINIMONSTACC_OSC2RANGE = 93;
const MINIMONSTAPARM_OSC2RANGE = 52;
const MINIMONSTACC_DETUNE = 94;
const MINIMONSTAPARM_DETUNE = 99;
const MINIMONSTACC_OSC3RANGE = 95;
const MINIMONSTAPARM_OSC3RANGE = 54;
const MINIMONSTACC_LFO1RATE = 96;
const MINIMONSTACC_LFO1WAVE = 97;
const VirtualToPhysMINIMONSTA: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,24,92,-1,-1,16,75,-1,-1,-1,-1,-1,93,27,-1,17,76,-1,-1,-1,-1,-1,95,28,-1,18,77,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,97,-1,-1,96,-1,-1,-1,-1,-1,-1,73,72,15,69,-1,65,7,-1,-1,-1,-1,71,70,-1,74,20,21,22,-1,87,91,12,-1,-1,-1,-1,-1,-1,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,67,78,-1,85,86,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualMINIMONSTA: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,66,-1,-1,-1,-1,81,-1,88,62,13,23,33,-1,75,76,77,-1,9,-1,-1,21,31,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,65,-1,111,-1,63,72,71,61,60,74,14,24,34,112,-1,-1,-1,-1,-1,-1,114,115,79,-1,-1,-1,80,10,20,-1,30,53,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountMINIMONSTA = 415;
const ParamToPhysMINIMONSTA: array[0..ParamCountMINIMONSTA-1] of integer = (
-1,-1,24,25,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,74,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,71,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,16,92,17,93,18,95,85,89,67,80,81,82,26,83,84,69,86,87,88,65,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,90,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,94,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,23,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,75,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,76,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,77,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
,-1,-1,78,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,79,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,70,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,21,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,22,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,73,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,72,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,91,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,13,-1,-1,-1
,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,14);
const PhysToParamMINIMONSTA: array[0..127] of integer = (-1,4,-1,-1,-1,69,-1,339,-1,-1,354,-1,384,399,414,324,49,51,53,-1,249,264,279,114,2,3,61,129,144,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,68,-1,57,-1,64,234,34,309,294,19,159,174,189,204,219,58,59,60,62,63,55,65,66,67,56,84,369,50,52,99,54,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerMINIMONSTA}
const MINIMONSTANames:array[0..127] of string = (
   'Bank Select 0_32','// Mod Wheel','','','','// Glide Amount','','// Volume','','','// Pan','','// Delay Time L','// Delay Time R','// Delay Feedback','// VCA Env Sustain',
   '// Osc 1 Waveform','// Osc 2 Waveform','// Osc 3 Waveform','','// VCF Env Atk Time','// VCF Env Dcy Time','// VCF Env Sustain','// Mod Mix','// Polyphony Mode','// Unison','// Noise Switch','// Osc 2 Transpose','// Osc 3 Transpose','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','// Glide Mode','','// Noise Color','','// Decay Mode','// VCF Contour','// VCF Emphasis','// VCA Env Dcy Time','// VCA Env Atk Time','// VCF Cutoff','// Osc 1 Mix','// Osc 2 Mix','// Osc 3 Mix','// Noise Mix','// Ext Input Mix',
   '// Osc 1 Switch','// Osc 2 Switch','// Osc 3 Switch','// Ext Input Switch','// Feedback','// Osc Modulation','// VCF Modulation','// VCF Kbd Ctrl 1','// VCF Kbd Ctrl 2','// Osc 3 Control','// Tune','// Delay Amount','// Osc 1 Range','// Osc 2 Range','// Detune','// Osc 3 Range',
   'LFO1 Rate','LFO1 Wave','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerMINIMONSTA.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysMINIMONSTA)) and (vcc<=high(VirtualToPhysMINIMONSTA))
    then
    begin
      result:=VirtualToPhysMINIMONSTA[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerMINIMONSTA.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualMINIMONSTA)) and (pcc<=high(PhysToVirtualMINIMONSTA))
    then
  result:=PhysToVirtualMINIMONSTA[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerMINIMONSTA.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamMINIMONSTA)) and (pcc<=high(PhysToParamMINIMONSTA))
    then
  result:=PhysToParamMINIMONSTA[pcc]
    else
  result:=-1;
end;

function TVSTTransformerMINIMONSTA.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysMINIMONSTA)) and (paramnr<=high(ParamToPhysMINIMONSTA))
    then
  result:=ParamToPhysMINIMONSTA[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerMINIMONSTA.ControllerName(pcc:integer):string;
begin
  result:=MINIMONSTANames[pcc]
end;
{$endif}


const SYNAPSECC_BANKSELECTLOW = 0;
const SYNAPSECC_MODULATIONWHEEL = 1;
const SYNAPSEPARM_MODULATIONWHEEL = 1;
const SYNAPSECC_PORTAMENTO = 5;
const SYNAPSEPARM_PORTAMENTO = 76;
const SYNAPSECC_LFO1_RATE = 14;
const SYNAPSEPARM_LFO1_RATE = 82;
const SYNAPSECC_LFO1_WAVE = 15;
const SYNAPSEPARM_LFO1_WAVE = 80;
const SYNAPSECC_LFO1SYNC = 16;
const SYNAPSECC_LFO1_DELAY = 18;
const SYNAPSEPARM_LFO1_DELAY = 83;
const SYNAPSECC_LFO2_RATE = 19;
const SYNAPSEPARM_LFO2_RATE = 88;
const SYNAPSECC_LFO2_WAVE = 20;
const SYNAPSEPARM_LFO2_WAVE = 86;
const SYNAPSECC_LFO2SYNC = 21;
const SYNAPSECC_LFO2_DELAY = 23;
const SYNAPSEPARM_LFO2_DELAY = 89;
const SYNAPSECC_OSC1_FINE = 24;
const SYNAPSEPARM_OSC1_FINE = 8;
const SYNAPSECC_OSC1_VAWAVE = 25;
const SYNAPSEPARM_OSC1_VAWAVE = 10;
const SYNAPSECC_OSC2_VAWAVE = 26;
const SYNAPSEPARM_OSC2_VAWAVE = 30;
const SYNAPSECC_OSC3_WAVE = 27;
const SYNAPSEPARM_OSC3_WAVE = 44;
const SYNAPSECC_OSC1_WTWAVE = 28;
const SYNAPSEPARM_OSC1_WTWAVE = 12;
const SYNAPSECC_OSC2_WTWAVE = 29;
const SYNAPSEPARM_OSC2_WTWAVE = 32;
const SYNAPSECC_OSC1_SPREAD = 30;
const SYNAPSEPARM_OSC1_SPREAD = 5;
const SYNAPSECC_OSC2_SPREAD = 31;
const SYNAPSEPARM_OSC2_SPREAD = 25;
const SYNAPSECC_BANKSELECTHI = 32;
const SYNAPSECC_OSC2_FINE = 34;
const SYNAPSEPARM_OSC2_FINE = 28;
const SYNAPSECC_VCA_ADSR_D = 36;
const SYNAPSEPARM_VCA_ADSR_D = 72;
const SYNAPSECC_VCA_ADSR_S = 37;
const SYNAPSEPARM_VCA_ADSR_S = 73;
const SYNAPSECC_VCF_ADSR_A = 38;
const SYNAPSEPARM_VCF_ADSR_A = 67;
const SYNAPSECC_VCF_ADSR_D = 39;
const SYNAPSEPARM_VCF_ADSR_D = 68;
const SYNAPSECC_VCF_ADSR_S = 40;
const SYNAPSEPARM_VCF_ADSR_S = 69;
const SYNAPSECC_VCF_ADSR_R = 41;
const SYNAPSEPARM_VCF_ADSR_R = 70;
const SYNAPSECC_VCF_RESONANCE = 42;
const SYNAPSEPARM_VCF_RESONANCE = 61;
const SYNAPSECC_VCF_RANGE = 43;
const SYNAPSEPARM_VCF_RANGE = 64;
const SYNAPSECC_VCF_KEYRANGE = 46;
const SYNAPSEPARM_VCF_KEYRANGE = 63;
const SYNAPSECC_ARPON = 47;
const SYNAPSEPARM_ARPON = 77;
const SYNAPSECC_ARPRATE = 50;
const SYNAPSECC_ARPNOTELENGTH = 52;
const SYNAPSECC_DETUNE = 53;
const SYNAPSECC_PANSPREAD = 54;
const SYNAPSECC_MM13 = 55;
const SYNAPSEPARM_MM13 = 288;
const SYNAPSECC_MM14 = 56;
const SYNAPSEPARM_MM14 = 289;
const SYNAPSECC_MM15 = 57;
const SYNAPSEPARM_MM15 = 290;
const SYNAPSECC_MM16 = 58;
const SYNAPSEPARM_MM16 = 291;
const SYNAPSECC_MM17 = 59;
const SYNAPSEPARM_MM17 = 292;
const SYNAPSECC_MM18 = 60;
const SYNAPSEPARM_MM18 = 293;
const SYNAPSECC_MM19 = 61;
const SYNAPSEPARM_MM19 = 294;
const SYNAPSECC_MM20 = 62;
const SYNAPSEPARM_MM20 = 295;
const SYNAPSECC_MM21 = 63;
const SYNAPSEPARM_MM21 = 296;
const SYNAPSECC_SUSTAINPEDAL = 64;
const SYNAPSECC_MM22 = 65;
const SYNAPSEPARM_MM22 = 297;
const SYNAPSECC_MM23 = 66;
const SYNAPSEPARM_MM23 = 298;
const SYNAPSECC_MM24 = 67;
const SYNAPSEPARM_MM24 = 299;
const SYNAPSECC_OSC1PW = 70;
const SYNAPSEPARM_OSC1PW = 9;
const SYNAPSECC_OSC2PW = 71;
const SYNAPSEPARM_OSC2PW = 29;
const SYNAPSECC_VCA_ADSR_R = 72;
const SYNAPSEPARM_VCA_ADSR_R = 74;
const SYNAPSECC_VCA_ADSR_A = 73;
const SYNAPSEPARM_VCA_ADSR_A = 71;
const SYNAPSECC_VCF_CUTOFF = 74;
const SYNAPSEPARM_VCF_CUTOFF = 60;
const SYNAPSECC_OSC1LEVEL = 75;
const SYNAPSEPARM_OSC1LEVEL = 50;
const SYNAPSECC_OSC2LEVEL = 76;
const SYNAPSEPARM_OSC2LEVEL = 52;
const SYNAPSECC_OSC1_COARSE = 77;
const SYNAPSEPARM_OSC1_COARSE = 7;
const SYNAPSECC_OSC2_COARSE = 78;
const SYNAPSEPARM_OSC2_COARSE = 27;
const SYNAPSECC_LFO3RATE = 80;
const SYNAPSECC_LFO3WAVEFORM = 81;
const SYNAPSECC_LFO3SYNC = 82;
const SYNAPSECC_LFO3FADEIN = 84;
const SYNAPSECC_CHORUSENABLE = 85;
const SYNAPSEPARM_CHORUSENABLE = 116;
const SYNAPSECC_DELAY_ENABLE = 86;
const SYNAPSEPARM_DELAY_ENABLE = 114;
const SYNAPSECC_REVERB_ENABLE = 87;
const SYNAPSEPARM_REVERB_ENABLE = 115;
const SYNAPSECC_EQ1GAIN = 89;
const SYNAPSECC_EQ2GAIN = 90;
const SYNAPSECC_REVERB_AMOUNT = 91;
const SYNAPSEPARM_REVERB_AMOUNT = 171;
const SYNAPSECC_DELAY_AMOUNT = 92;
const SYNAPSEPARM_DELAY_AMOUNT = 153;
const SYNAPSECC_CHORUS_AMOUNT = 93;
const SYNAPSEPARM_CHORUS_AMOUNT = 177;
const SYNAPSECC_DISTORTIONAMOUN = 94;
const SYNAPSECC_PHASERAMOUNT = 95;
const SYNAPSECC_REVERBFEEDBACK = 97;
const SYNAPSEPARM_REVERBFEEDBACK = 165;
const SYNAPSECC_DELAY_RATEL = 98;
const SYNAPSEPARM_DELAY_RATEL = 145;
const SYNAPSECC_DELAY_RATER = 99;
const SYNAPSEPARM_DELAY_RATER = 146;
const SYNAPSECC_DELAY_FEEDBACK = 100;
const SYNAPSEPARM_DELAY_FEEDBACK = 150;
const SYNAPSECC_CHORUSRATE = 101;
const SYNAPSEPARM_CHORUSRATE = 173;
const SYNAPSECC_OSC1_DETUNE = 102;
const SYNAPSEPARM_OSC1_DETUNE = 4;
const SYNAPSECC_OSC2_DETUNE = 103;
const SYNAPSEPARM_OSC2_DETUNE = 24;
const SYNAPSECC_REVERB_TIME = 104;
const SYNAPSEPARM_REVERB_TIME = 166;
const SYNAPSECC_OSC3COARSE = 105;
const SYNAPSEPARM_OSC3COARSE = 45;
const SYNAPSECC_OSC3LEVEL = 106;
const SYNAPSEPARM_OSC3LEVEL = 54;
const SYNAPSECC_NOISELP = 107;
const SYNAPSECC_NOISEHP = 108;
const SYNAPSEPARM_NOISEHP = 48;
const SYNAPSECC_NOISEWIDTH = 109;
const SYNAPSECC_NOISELEVEL = 110;
const SYNAPSEPARM_NOISELEVEL = 58;
const SYNAPSECC_VOLUME = 111;
const SYNAPSEPARM_VOLUME = 0;
const SYNAPSECC_MMAMOUNT1 = 112;
const SYNAPSEPARM_MMAMOUNT1 = 276;
const SYNAPSECC_MMAMOUNT2 = 113;
const SYNAPSEPARM_MMAMOUNT2 = 277;
const SYNAPSECC_MMAMOUNT3 = 114;
const SYNAPSEPARM_MMAMOUNT3 = 278;
const SYNAPSECC_MMAMOUNT4 = 115;
const SYNAPSEPARM_MMAMOUNT4 = 279;
const SYNAPSECC_MMAMOUNT5 = 116;
const SYNAPSEPARM_MMAMOUNT5 = 280;
const SYNAPSECC_MMAMOUNT6 = 117;
const SYNAPSEPARM_MMAMOUNT6 = 281;
const SYNAPSECC_MMAMOUNT7 = 118;
const SYNAPSEPARM_MMAMOUNT7 = 282;
const SYNAPSECC_MMAMOUNT8 = 119;
const SYNAPSEPARM_MMAMOUNT8 = 283;
const VirtualToPhysSYNAPSE: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,-1,77,-1,102,25,75,-1,-1,-1,-1,-1,78,-1,103,26,76,-1,-1,-1,-1,-1,105,-1,-1,27,106,-1,-1,-1,-1,-1,-1,-1,-1,108,110,-1,-1,-1,-1,-1,15,18,-1,14,-1,20,23,-1,19,-1,73,36,37,72,64,-1,111,-1,-1,-1,-1,42,43,-1,74,38,39,40,41,46,92,98,91,104,93,101,-1,-1,100,97,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualSYNAPSE: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,-1,-1,-1,-1,-1,53,50,-1,-1,51,58,55,-1,-1,56,-1,13,23,33,-1,-1,-1,-1,-1,-1,-1,-1,61,62,75,76,77,78,71,72,-1,-1,79,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,64,-1,-1,-1,-1,-1,-1,-1,63,60,74,14,24,10,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,82,80,84,-1,-1,-1,89,81,-1,88,85,12,22,83,30,34,-1,43,-1,44,66,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountSYNAPSE = 300;
const ParamToPhysSYNAPSE: array[0..ParamCountSYNAPSE-1] of integer = (
111,1,-1,-1,102,30,-1,77,24,70,25,-1,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,103,31,-1,78,34,71,26,-1,29,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,105,-1,-1,108,-1,75,-1,76,-1,106,-1,-1,-1,110,-1,74,42,-1,46,43,-1,-1,38,39,40,41,73,36,37,72,-1,5,47,-1,-1,15,-1,14,18,-1,-1,20,-1,19,23,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,86,87,85,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,98,99,-1,-1,-1,100,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,97,104,-1,-1,-1,-1,91,-1,101,-1,-1,-1,93,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,112,113,114,115,116,117,118,119,-1,-1,-1,-1,55,56,57,58,59,60,61,62,63,65,66,67);
const PhysToParamSYNAPSE: array[0..127] of integer = (-1,1,-1,-1,-1,76,-1,-1,-1,-1,-1,-1,-1,-1,82,80,-1,-1,83,88,86,-1,-1,89,8,10,30,44,12,32,5,25,-1,-1,28,-1,72,73,67,68,69,70,61,64,-1,-1,63,77,-1,-1,-1,-1,-1,-1,-1,288,289,290,291,292,293,294,295,296,-1,297,298,299,-1,-1,9,29,74,71,60,50,52,7,27,-1,-1,-1,-1,-1,-1,116,114,115,-1,-1,-1,171,153,177,-1,-1,-1,165,145,146,150,173,4,24,166,45,54,-1,48,-1,58,0,276,277,278,279,280,281,282,283,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerSYNAPSE}
const SYNAPSENames:array[0..127] of string = (
   'Bank Select Low','Modulation Wheel','','','','Portamento','','','','','','','','','LFO1_RATE','LFO1_WAVE',
   'LFO 1 Sync','','LFO1_DELAY','LFO2_RATE','LFO2_WAVE','LFO 2 Sync','','LFO2_DELAY','Osc1_Fine','Osc1_VaWave','Osc2_VaWave','Osc3_Wave','Osc1_WTWave','Osc2_WTWave','Osc1_Spread','Osc2_Spread',
   'Bank Select HI','','Osc2_Fine','','VCA_ADSR_D','VCA_ADSR_S','VCF_ADSR_A','VCF_ADSR_D','VCF_ADSR_S','VCF_ADSR_R','VCF_RESONANCE','VCF_RANGE','','','VCF_KEYRANGE','Arp On',
   '','','Arp Rate','','Arp Note Length','Detune','Pan Spread','MM 13','MM 14','MM 15','MM 16','MM 17','MM 18','MM 19','MM 20','MM 21',
   'Sustain Pedal','MM 22','MM 23','MM 24','','','Osc1 PW','Osc2 PW','VCA_ADSR_R','VCA_ADSR_A','VCF_CUTOFF','Osc1 Level','Osc2 Level','OSC1_Coarse','OSC2_Coarse','',
   'LFO 3 Rate','LFO 3 Waveform','LFO 3 Sync','','LFO 3 Fade In','Chorus Enable','Delay_Enable','Reverb_Enable','','EQ 1 Gain','EQ 2 Gain','Reverb_Amount','Delay_Amount','Chorus_Amount','Distortion Amoun','Phaser Amount',
   '','Reverb Feedback','Delay_RateL','Delay_RateR','Delay_Feedback','Chorus Rate','OSC1_Detune','OSC2_Detune','Reverb_Time','Osc3 Coarse','Osc3 Level','Noise LP','Noise HP','Noise Width','Noise Level','Volume',
   'MM Amount 1','MM Amount 2','MM Amount 3','MM Amount 4','MM Amount 5','MM Amount 6','MM Amount 7','MM Amount 8','','','','','','','',''
);

function TVSTTransformerSYNAPSE.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysSYNAPSE)) and (vcc<=high(VirtualToPhysSYNAPSE))
    then
    begin
      result:=VirtualToPhysSYNAPSE[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerSYNAPSE.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualSYNAPSE)) and (pcc<=high(PhysToVirtualSYNAPSE))
    then
  result:=PhysToVirtualSYNAPSE[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerSYNAPSE.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamSYNAPSE)) and (pcc<=high(PhysToParamSYNAPSE))
    then
  result:=PhysToParamSYNAPSE[pcc]
    else
  result:=-1;
end;

function TVSTTransformerSYNAPSE.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysSYNAPSE)) and (paramnr<=high(ParamToPhysSYNAPSE))
    then
  result:=ParamToPhysSYNAPSE[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerSYNAPSE.ControllerName(pcc:integer):string;
begin
  result:=SYNAPSENames[pcc]
end;

function TVSTTransformerSYNAPSE.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [15,20,25,26,27,28,29,55,56,57,58,59,60,61,62,63,65,66,67,85,86,87,97,98,99,100,101,104] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const CRUMARCC_KYBD_ROTARY3 = 1;
const CRUMARCC_KYBD_ROTARY0 = 5;
const CRUMARCC_MAIN_VOLUME = 7;
const CRUMARCC_OSC1_COURSE = 10;
const CRUMARCC_OSC1_FINE = 11;
const CRUMARCC_OSC1_WAVE = 12;
const CRUMARCC_OSC1_VOLUME = 13;
const CRUMARCC_OSC1_MOD_DEPTH = 14;
const CRUMARCC_OSC2_COURSE = 16;
const CRUMARCC_OSC2_FINE = 17;
const CRUMARCC_OSC2_WAVE = 18;
const CRUMARCC_OSC2_VOLUME = 19;
const CRUMARCC_OSC2_MOD_DEPTH = 20;
const CRUMARCC_OSC3_COURSE = 22;
const CRUMARCC_OSC3_FINE = 23;
const CRUMARCC_OSC3_WAVE = 24;
const CRUMARCC_OSC3_VOLUME = 25;
const CRUMARCC_LFO1_RATE = 30;
const CRUMARCC_LFO2_RATE = 31;
const CRUMARCC_LFO1_DELAY = 32;
const CRUMARCC_LFO2_DELAY = 33;
const CRUMARCC_LFO1_WAVE = 34;
const CRUMARCC_LFO2_WAVE = 35;
const CRUMARCC_VCF_ADSR_A = 36;
const CRUMARCC_VCF_ADSR_D = 37;
const CRUMARCC_VCF_ADSR_S = 38;
const CRUMARCC_VCF_ADSR_R = 39;
const CRUMARCC_VCA_ADSR_A = 40;
const CRUMARCC_VCA_ADSR_D = 41;
const CRUMARCC_VCA_ADSR_S = 42;
const CRUMARCC_VCA_ADSR_R = 43;
const CRUMARCC_MOTORCONTROL = 44;
const CRUMARCC_VCF_RANGE = 46;
const CRUMARCC_VCF_MOD_DEPTH = 47;
const CRUMARCC_VCF_KEYRANGE = 48;
const CRUMARCC_VCA_RANGE = 49;
const CRUMARCC_VCA_MOD_DEPTH = 50;
const CRUMARCC_DIG_NOISECOLOR = 51;
const CRUMARCC_PULSE_MOD_DEPTH = 52;
const CRUMARCC_NOISE_VOLUME = 53;
const CRUMARCC_NOISE_COLOR = 54;
const CRUMARCC_DIG_POLY = 55;
const CRUMARCC_DELAYDEPTH = 56;
const CRUMARCC_DELAYTIME = 57;
const CRUMARCC_REVERBDEPTH = 58;
const CRUMARCC_REVERBTIME = 59;
const CRUMARCC_CHORUSDEPTH = 60;
const CRUMARCC_CHORUSTIME = 61;
const CRUMARCC_PHASERDEPTH = 62;
const CRUMARCC_PHASERTIME = 63;
const CRUMARCC_KYBD_DIG0 = 65;
const CRUMARCC_KYBD_ROTARY1 = 67;
const CRUMARCC_KYBD_ROTARY2 = 68;
const CRUMARCC_KEYPAD = 70;
const CRUMARCC_VCF_RESONANCE = 71;
const CRUMARCC_POLY_RANGE = 72;
const CRUMARCC_DIG_VOLUME = 73;
const CRUMARCC_VCF_CUTOFF = 74;
const CRUMARCC_KYBD_DIG1 = 76;
const CRUMARCC_KYBD_DIG2 = 77;
const CRUMARCC_KYBD_DIG3 = 78;
const CRUMARCC_POLY_MOD_DEPTH = 80;
const CRUMARCC_SELECTEFFECT = 81;
const CRUMARCC_INTERNALINSTRUMENTNAME = 82;
const CRUMARCC_INTERNALPROGRAMNAMEANDNUMBER = 83;
const CRUMARCC_INTERNALCONNECTED = 84;
const CRUMARCC_INTERNALOSC3OSC4 = 85;
const CRUMARCC_DIG_LFO1 = 86;
const CRUMARCC_DIG_LFO2 = 87;
const CRUMARCC_OSC1_MOD_DIG = 88;
const CRUMARCC_OSC2_MOD_DIG = 89;
const CRUMARCC_VCF_MOD_DIG = 90;
const CRUMARCC_VCA_MOD_DIG = 91;
const CRUMARCC_PULSE_MOD_DIG = 92;
const CRUMARCC_POLY_MOD_DIG = 93;
const CRUMARCC_VCF_KEYRANGE_DIG = 94;
const VirtualToPhysCRUMAR: array[0..127] of integer = (-1,-1,-1,73,-1,-1,-1,7,72,55,10,11,-1,12,13,-1,52,92,14,88,16,17,-1,18,19,-1,-1,-1,20,89,22,23,-1,24,25,-1,-1,-1,-1,-1,26,27,-1,28,29,-1,-1,-1,-1,-1,34,32,-1,30,-1,35,33,-1,31,-1,40,41,42,43,-1,-1,49,47,90,50,91,71,46,-1,74,36,37,38,39,-1,56,57,58,59,60,61,62,63,-1,-1,-1,-1,-1,-1,-1,-1,81,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,54,53,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualCRUMAR: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,7,-1,-1,10,11,13,14,18,-1,20,21,23,24,28,-1,30,31,33,34,40,41,43,44,53,58,51,56,50,55,75,76,77,78,60,61,62,63,-1,-1,72,67,-1,66,69,-1,16,112,111,9,80,81,82,83,84,85,86,87,-1,-1,-1,-1,-1,-1,-1,71,8,3,74,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,19,29,68,70,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToParamCRUMAR: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerCRUMAR}
const CRUMARNames:array[0..127] of string = (
   '','KYBD_ROTARY3','','','','KYBD_ROTARY0','','MAIN_VOLUME ','','','OSC1_COURSE','OSC1_FINE','OSC1_WAVE','OSC1_VOLUME','OSC1_MOD_DEPTH','',
   'OSC2_COURSE','OSC2_FINE','OSC2_WAVE','OSC2_VOLUME','OSC2_MOD_DEPTH','','OSC3_COURSE','OSC3_FINE','OSC3_WAVE','OSC3_VOLUME','','','','','LFO1_RATE ','LFO2_RATE ',
   'LFO1_DELAY ','LFO2_DELAY ','LFO1_WAVE','LFO2_WAVE','VCF_ADSR_A ','VCF_ADSR_D ','VCF_ADSR_S ','VCF_ADSR_R ','VCA_ADSR_A ','VCA_ADSR_D ','VCA_ADSR_S ','VCA_ADSR_R ','MotorControl','','VCF_RANGE','VCF_MOD_DEPTH',
   'VCF_KEYRANGE','VCA_RANGE  ','VCA_MOD_DEPTH','DIG_NOISECOLOR','PULSE_MOD_DEPTH','NOISE_VOLUME ','NOISE_COLOR','DIG_POLY','Delay Depth','Delay Time','Reverb Depth','Reverb Time','Chorus Depth','Chorus Time','Phaser Depth','Phaser Time',
   '','KYBD_DIG0','','KYBD_ROTARY1','KYBD_ROTARY2','','KeyPad','VCF_RESONANCE ','POLY_RANGE','DIG_VOLUME','VCF_CUTOFF ','','KYBD_DIG1','KYBD_DIG2','KYBD_DIG3','',
   'POLY_MOD_DEPTH','SelectEffect','InternalInstrumentName','InternalProgramNameAndNumber','Internal Connected','InternalOsc3Osc4','DIG_LFO1','DIG_LFO2','OSC1_MOD_DIG','OSC2_MOD_DIG','VCF_MOD_DIG','VCA_MOD_DIG','PULSE_MOD_DIG','POLY_MOD_DIG','VCF_KEYRANGE_DIG','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerCRUMAR.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysCRUMAR)) and (vcc<=high(VirtualToPhysCRUMAR))
    then
    begin
      result:=VirtualToPhysCRUMAR[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerCRUMAR.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualCRUMAR)) and (pcc<=high(PhysToVirtualCRUMAR))
    then
  result:=PhysToVirtualCRUMAR[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerCRUMAR.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamCRUMAR)) and (pcc<=high(PhysToParamCRUMAR))
    then
  result:=PhysToParamCRUMAR[pcc]
    else
  result:=-1;
end;

function TVSTTransformerCRUMAR.ParamToPhys(paramnr: integer): integer;
begin
  result:=-1;
end;

function TVSTTransformerCRUMAR.ControllerName(pcc:integer):string;
begin
  result:=CRUMARNames[pcc]
end;
{$endif}


const TAPEOTRONICCC_BANKSELECT032 = 0;
const TAPEOTRONICCC_ATTACK = 10;
const TAPEOTRONICPARM_ATTACK = 0;
const TAPEOTRONICCC_RELEASE = 11;
const TAPEOTRONICPARM_RELEASE = 2;
const TAPEOTRONICCC_TONE = 12;
const TAPEOTRONICPARM_TONE = 4;
const TAPEOTRONICCC_SOUND = 13;
const TAPEOTRONICPARM_SOUND = 3;
const TAPEOTRONICCC_VOLUME = 14;
const TAPEOTRONICPARM_VOLUME = 6;
const TAPEOTRONICCC_FREE0 = 15;
const TAPEOTRONICPARM_FREE0 = 99;
const TAPEOTRONICCC_FREE1 = 16;
const TAPEOTRONICPARM_FREE1 = 100;
const TAPEOTRONICCC_FREE2 = 17;
const TAPEOTRONICPARM_FREE2 = 101;
const VirtualToPhysTAPEOTRONIC: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualTAPEOTRONIC: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountTAPEOTRONIC = 102;
const ParamToPhysTAPEOTRONIC: array[0..ParamCountTAPEOTRONIC-1] of integer = (
10,-1,11,13,12,-1,14,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,15,16,17);
const PhysToParamTAPEOTRONIC: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,2,4,3,6,99,100,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerTAPEOTRONIC}
const TAPEOTRONICNames:array[0..127] of string = (
   'Bank Select 0, 32','','','','','','','','','','Attack','Release','Tone','Sound','Volume','Free 0',
   'Free 1','Free 2','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerTAPEOTRONIC.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysTAPEOTRONIC)) and (vcc<=high(VirtualToPhysTAPEOTRONIC))
    then
    begin
      result:=VirtualToPhysTAPEOTRONIC[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerTAPEOTRONIC.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualTAPEOTRONIC)) and (pcc<=high(PhysToVirtualTAPEOTRONIC))
    then
  result:=PhysToVirtualTAPEOTRONIC[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerTAPEOTRONIC.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamTAPEOTRONIC)) and (pcc<=high(PhysToParamTAPEOTRONIC))
    then
  result:=PhysToParamTAPEOTRONIC[pcc]
    else
  result:=-1;
end;

function TVSTTransformerTAPEOTRONIC.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysTAPEOTRONIC)) and (paramnr<=high(ParamToPhysTAPEOTRONIC))
    then
  result:=ParamToPhysTAPEOTRONIC[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerTAPEOTRONIC.ControllerName(pcc:integer):string;
begin
  result:=TAPEOTRONICNames[pcc]
end;

function TVSTTransformerTAPEOTRONIC.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [10,11,12,13,14,15,16,17] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const ALCHEMYCC_BANKSELECT0_32 = 0;
const ALCHEMYCC_CONTROLLER1 = 10;
const ALCHEMYPARM_CONTROLLER1 = 0;
const ALCHEMYCC_CONTROLLER2 = 11;
const ALCHEMYPARM_CONTROLLER2 = 1;
const ALCHEMYCC_CONTROLLER3 = 12;
const ALCHEMYPARM_CONTROLLER3 = 2;
const ALCHEMYCC_CONTROLLER4 = 13;
const ALCHEMYPARM_CONTROLLER4 = 3;
const ALCHEMYCC_CONTROLLER5 = 14;
const ALCHEMYPARM_CONTROLLER5 = 4;
const ALCHEMYCC_CONTROLLER6 = 15;
const ALCHEMYPARM_CONTROLLER6 = 5;
const ALCHEMYCC_CONTROLLER7 = 16;
const ALCHEMYPARM_CONTROLLER7 = 6;
const ALCHEMYCC_CONTROLLER8 = 17;
const ALCHEMYPARM_CONTROLLER8 = 7;
const VirtualToPhysALCHEMY: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualALCHEMY: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountALCHEMY = 8;
const ParamToPhysALCHEMY: array[0..ParamCountALCHEMY-1] of integer = (
10,11,12,13,14,15,16,17);
const PhysToParamALCHEMY: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,2,3,4,5,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerALCHEMY}
const ALCHEMYNames:array[0..127] of string = (
   'Bank Select 0_32','','','','','','','','','','Controller 1','Controller 2','Controller 3','Controller 4','Controller 5','Controller 6',
   'Controller 7','Controller 8','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);
function TVSTTransformerALCHEMY.ReactToProgramChange:boolean;
begin
  result:=false
end;


function TVSTTransformerALCHEMY.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysALCHEMY)) and (vcc<=high(VirtualToPhysALCHEMY))
    then
    begin
      result:=VirtualToPhysALCHEMY[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerALCHEMY.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualALCHEMY)) and (pcc<=high(PhysToVirtualALCHEMY))
    then
  result:=PhysToVirtualALCHEMY[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerALCHEMY.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamALCHEMY)) and (pcc<=high(PhysToParamALCHEMY))
    then
  result:=PhysToParamALCHEMY[pcc]
    else
  result:=-1;
end;

function TVSTTransformerALCHEMY.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysALCHEMY)) and (paramnr<=high(ParamToPhysALCHEMY))
    then
  result:=ParamToPhysALCHEMY[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerALCHEMY.ControllerName(pcc:integer):string;
begin
  result:=ALCHEMYNames[pcc]
end;

function TVSTTransformerALCHEMY.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [10,11,12,13,14,15,16,17] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const DRUMSCC_BANKSELECT0_32 = 0;
const DRUMSCC_KICK = 10;
const DRUMSPARM_KICK = 0;
const DRUMSCC_HHOPEN = 11;
const DRUMSPARM_HHOPEN = 4;
const DRUMSCC_SNARE = 12;
const DRUMSPARM_SNARE = 1;
const DRUMSCC_HHCLOSE = 13;
const DRUMSPARM_HHCLOSE = 3;
const DRUMSCC_TOLO = 14;
const DRUMSPARM_TOLO = 7;
const DRUMSCC_TOMMID = 15;
const DRUMSPARM_TOMMID = 6;
const DRUMSCC_TOMHI = 16;
const DRUMSPARM_TOMHI = 5;
const DRUMSCC_RIDE = 17;
const DRUMSPARM_RIDE = 8;
const VirtualToPhysDRUMS: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualDRUMS: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountDRUMS = 9;
const ParamToPhysDRUMS: array[0..ParamCountDRUMS-1] of integer = (
10,12,-1,13,11,16,15,14,17);
const PhysToParamDRUMS: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,4,1,3,7,6,5,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerDRUMS}
const DRUMSNames:array[0..127] of string = (
   'Bank Select 0_32','','','','','','','','','','Kick','HH Open','Snare','HH Close','To LO','Tom Mid',
   'Tom Hi','Ride','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);
function TVSTTransformerDRUMS.ReactToProgramChange:boolean;
begin
  result:=false
end;


function TVSTTransformerDRUMS.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysDRUMS)) and (vcc<=high(VirtualToPhysDRUMS))
    then
    begin
      result:=VirtualToPhysDRUMS[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerDRUMS.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualDRUMS)) and (pcc<=high(PhysToVirtualDRUMS))
    then
  result:=PhysToVirtualDRUMS[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerDRUMS.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamDRUMS)) and (pcc<=high(PhysToParamDRUMS))
    then
  result:=PhysToParamDRUMS[pcc]
    else
  result:=-1;
end;

function TVSTTransformerDRUMS.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysDRUMS)) and (paramnr<=high(ParamToPhysDRUMS))
    then
  result:=ParamToPhysDRUMS[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerDRUMS.ControllerName(pcc:integer):string;
begin
  result:=DRUMSNames[pcc]
end;

function TVSTTransformerDRUMS.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [10,11,12,13,14,15,16,17] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const PROSOLOCC_BANKSELECT032 = 0;
const PROSOLOCC_GLIDE = 5;
const PROSOLOPARM_GLIDE = 4;
const PROSOLOCC_GLIDEONOFF = 65;
const PROSOLOPARM_GLIDEONOFF = 13;
const PROSOLOCC_BRILLIANCE = 74;
const PROSOLOPARM_BRILLIANCE = 3;
const PROSOLOCC_VOICE = 13;
const PROSOLOPARM_VOICE = 17;
const VirtualToPhysPROSOLO: array[0..127] of integer = (-1,-1,-1,-1,-1,5,-1,-1,-1,-1,-1,-1,-1,13,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,65,-1,-1,-1,-1,-1,-1,-1,-1,74,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualPROSOLO: array[0..127] of integer = (-1,-1,-1,-1,-1,5,-1,-1,-1,-1,-1,-1,-1,13,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,65,-1,-1,-1,-1,-1,-1,-1,-1,74,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountPROSOLO = 18;
const ParamToPhysPROSOLO: array[0..ParamCountPROSOLO-1] of integer = (
-1,-1,-1,74,5,-1,-1,-1,-1,-1,-1,-1,-1,65,-1,-1,-1,13);
const PhysToParamPROSOLO: array[0..127] of integer = (-1,-1,-1,-1,-1,4,-1,-1,-1,-1,-1,-1,-1,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,13,-1,-1,-1,-1,-1,-1,-1,-1,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerPROSOLO}
const PROSOLONames:array[0..127] of string = (
   'Bank Select 0, 32','','','','','Glide','','','','','','','','Voice','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','Glide On Off','','','','','','','','','Brilliance','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerPROSOLO.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysPROSOLO)) and (vcc<=high(VirtualToPhysPROSOLO))
    then
    begin
      result:=VirtualToPhysPROSOLO[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerPROSOLO.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualPROSOLO)) and (pcc<=high(PhysToVirtualPROSOLO))
    then
  result:=PhysToVirtualPROSOLO[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerPROSOLO.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamPROSOLO)) and (pcc<=high(PhysToParamPROSOLO))
    then
  result:=PhysToParamPROSOLO[pcc]
    else
  result:=-1;
end;

function TVSTTransformerPROSOLO.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysPROSOLO)) and (paramnr<=high(ParamToPhysPROSOLO))
    then
  result:=ParamToPhysPROSOLO[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerPROSOLO.ControllerName(pcc:integer):string;
begin
  result:=PROSOLONames[pcc]
end;

function TVSTTransformerPROSOLO.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [5,65,74,13] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerOMNISPHERE}
function TVSTTransformerOMNISPHERE.ReactToProgramChange:boolean;
begin
  result:=false
end;

{$endif}


const RETROLOGUECC_MONO = 9;
const RETROLOGUEPARM_MONO = 2;
const RETROLOGUECC_GLIDEENABLE = 65;
const RETROLOGUEPARM_GLIDEENABLE = 3;
const RETROLOGUECC_GLIDETIME = 5;
const RETROLOGUEPARM_GLIDETIME = 5;
const RETROLOGUECC_OSC1OCTAVE = 10;
const RETROLOGUEPARM_OSC1OCTAVE = 15;
const RETROLOGUECC_OSC1COARSE = 11;
const RETROLOGUEPARM_OSC1COARSE = 16;
const RETROLOGUECC_OSC1PHASE = 12;
const RETROLOGUEPARM_OSC1PHASE = 20;
const RETROLOGUECC_OSC1WAVEFORM = 13;
const RETROLOGUEPARM_OSC1WAVEFORM = 21;
const RETROLOGUECC_OSC1LEVEL = 14;
const RETROLOGUEPARM_OSC1LEVEL = 25;
const RETROLOGUECC_OSC1ENABLE = 15;
const RETROLOGUEPARM_OSC1ENABLE = 26;
const RETROLOGUECC_OSC2OCTAVE = 20;
const RETROLOGUEPARM_OSC2OCTAVE = 27;
const RETROLOGUECC_OSC2COARSE = 21;
const RETROLOGUEPARM_OSC2COARSE = 28;
const RETROLOGUECC_OSC2PHASE = 22;
const RETROLOGUEPARM_OSC2PHASE = 32;
const RETROLOGUECC_OSC2WAVEFORM = 23;
const RETROLOGUEPARM_OSC2WAVEFORM = 33;
const RETROLOGUECC_OSC2LEVEL = 24;
const RETROLOGUEPARM_OSC2LEVEL = 37;
const RETROLOGUECC_OSC2ENABLE = 25;
const RETROLOGUEPARM_OSC2ENABLE = 38;
const RETROLOGUECC_OSC3OCTAVE = 30;
const RETROLOGUEPARM_OSC3OCTAVE = 39;
const RETROLOGUECC_OSC3COARSE = 31;
const RETROLOGUEPARM_OSC3COARSE = 40;
const RETROLOGUECC_OSC3PHASE = 32;
const RETROLOGUEPARM_OSC3PHASE = 44;
const RETROLOGUECC_OSC3WAVEFORM = 33;
const RETROLOGUEPARM_OSC3WAVEFORM = 45;
const RETROLOGUECC_OSC3LEVEL = 34;
const RETROLOGUEPARM_OSC3LEVEL = 49;
const RETROLOGUECC_OSC3ENABLE = 35;
const RETROLOGUEPARM_OSC3ENABLE = 50;
const RETROLOGUECC_SUBOSCENABLE = 44;
const RETROLOGUEPARM_SUBOSCENABLE = 54;
const RETROLOGUECC_NOISETYPE = 111;
const RETROLOGUEPARM_NOISETYPE = 55;
const RETROLOGUECC_NOISELEVEL = 112;
const RETROLOGUEPARM_NOISELEVEL = 56;
const RETROLOGUECC_NOISEENABLE = 113;
const RETROLOGUEPARM_NOISEENABLE = 57;
const RETROLOGUECC_RINGENABLE = 45;
const RETROLOGUEPARM_RINGENABLE = 59;
const RETROLOGUECC_FILTERCUTOFF = 74;
const RETROLOGUEPARM_FILTERCUTOFF = 64;
const RETROLOGUECC_FILTERRESONANCE = 71;
const RETROLOGUEPARM_FILTERRESONANCE = 65;
const RETROLOGUECC_FILTERENVAMOUNT = 72;
const RETROLOGUEPARM_FILTERENVAMOUNT = 67;
const RETROLOGUECC_FILTERKEYFOLLOW = 79;
const RETROLOGUEPARM_FILTERKEYFOLLOW = 68;
const RETROLOGUECC_LEVEL = 66;
const RETROLOGUEPARM_LEVEL = 70;
const RETROLOGUECC_DCAATTACK = 60;
const RETROLOGUEPARM_DCAATTACK = 72;
const RETROLOGUECC_DCADECAY = 61;
const RETROLOGUEPARM_DCADECAY = 73;
const RETROLOGUECC_DCASUSTAIN = 62;
const RETROLOGUEPARM_DCASUSTAIN = 74;
const RETROLOGUECC_DCARELEASE = 63;
const RETROLOGUEPARM_DCARELEASE = 75;
const RETROLOGUECC_DCFATTACK = 75;
const RETROLOGUEPARM_DCFATTACK = 76;
const RETROLOGUECC_DCFDECAY = 76;
const RETROLOGUEPARM_DCFDECAY = 77;
const RETROLOGUECC_DCFSUSTAIN = 77;
const RETROLOGUEPARM_DCFSUSTAIN = 78;
const RETROLOGUECC_DCFRELEASE = 78;
const RETROLOGUEPARM_DCFRELEASE = 79;
const RETROLOGUECC_LFON1WAVEFORM = 50;
const RETROLOGUEPARM_LFON1WAVEFORM = 82;
const RETROLOGUECC_LFO1WAVEFORM = 50;
const RETROLOGUEPARM_LFO1WAVEFORM = 98;
const RETROLOGUECC_LFO1FREQUENCY = 53;
const RETROLOGUEPARM_LFO1FREQUENCY = 100;
const RETROLOGUECC_LFO1FADEIN = 51;
const RETROLOGUEPARM_LFO1FADEIN = 106;
const RETROLOGUECC_LFO2WAVEFORM = 55;
const RETROLOGUEPARM_LFO2WAVEFORM = 107;
const RETROLOGUECC_LFO2FREQUENCY = 58;
const RETROLOGUEPARM_LFO2FREQUENCY = 109;
const RETROLOGUECC_LFO2FADEIN = 56;
const RETROLOGUEPARM_LFO2FADEIN = 115;
const VirtualToPhysRETROLOGUE: array[0..127] of integer = (-1,-1,-1,-1,-1,5,-1,-1,-1,-1,-1,-1,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,22,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,51,-1,53,-1,-1,56,-1,58,-1,60,61,62,63,-1,65,66,-1,-1,-1,-1,71,72,-1,74,75,76,77,78,79,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualRETROLOGUE: array[0..127] of integer = (-1,-1,-1,-1,-1,5,-1,-1,-1,-1,-1,-1,12,-1,-1,-1,-1,-1,-1,-1,-1,-1,22,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,51,-1,53,-1,-1,56,-1,58,-1,60,61,62,63,-1,65,66,-1,-1,-1,-1,71,72,-1,74,75,76,77,78,79,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountRETROLOGUE = 116;
const ParamToPhysRETROLOGUE: array[0..ParamCountRETROLOGUE-1] of integer = (
-1,-1,9,65,-1,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,-1,-1,-1,12,13,-1,-1,-1,14,15,20,21,-1,-1,-1,22,23,-1,-1,-1,24,25,30,31,-1,-1,-1,32,33,-1,-1,-1,34,35,-1,-1,-1,44,111,112,113,-1,45,-1,-1,-1,-1,74,71,-1,72,79,-1,66,-1,60,61,62,63,75,76,77,78,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,50,-1,53,-1,-1,-1,-1,-1,51,55,-1,58,-1,-1,-1,-1,-1,56);
const PhysToParamRETROLOGUE: array[0..127] of integer = (-1,-1,-1,-1,-1,5,-1,-1,-1,2,15,16,20,21,25,26,-1,-1,-1,-1,27,28,32,33,37,38,-1,-1,-1,-1,39,40,44,45,49,50,-1,-1,-1,-1,-1,-1,-1,-1,54,59,-1,-1,-1,-1,98,106,-1,100,-1,107,115,-1,109,-1,72,73,74,75,-1,3,70,-1,-1,-1,-1,65,67,-1,64,76,77,78,79,68,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerRETROLOGUE}
const RETROLOGUENames:array[0..127] of string = (
   '','','','','','Glide Time','','','','Mono','Osc 1 Octave','Osc 1 Coarse','Osc 1 Phase','Osc 1 Waveform','Osc 1 Level','Osc 1 Enable',
   '','','','','Osc 2 Octave','Osc 2 Coarse','Osc 2 Phase','Osc 2 Waveform','Osc 2 Level','Osc 2 Enable','','','','','Osc 3 Octave','Osc 3 Coarse',
   'Osc 3 Phase','Osc 3 Waveform','Osc 3 Level','Osc 3 Enable','','','','','','','','','Sub Osc Enable','Ring Enable','','',
   '','','LFO 1 Waveform','LFO 1 Fade In','','LFO 1 Frequency','','LFO 2 Waveform','LFO 2 Fade In','','LFO 2 Frequency','','DCA Attack','DCA Decay','DCA Sustain','DCA Release',
   '','Glide Enable','Level','','','','','Filter Resonance','Filter Env Amount','','Filter Cutoff','DCF Attack','DCF Decay','DCF Sustain','DCF Release','Filter Key Follow',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','Noise Type',
   'Noise Level','Noise Enable','','','','','','','','','','','','','',''
);

function TVSTTransformerRETROLOGUE.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysRETROLOGUE)) and (vcc<=high(VirtualToPhysRETROLOGUE))
    then
    begin
      result:=VirtualToPhysRETROLOGUE[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerRETROLOGUE.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualRETROLOGUE)) and (pcc<=high(PhysToVirtualRETROLOGUE))
    then
  result:=PhysToVirtualRETROLOGUE[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerRETROLOGUE.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamRETROLOGUE)) and (pcc<=high(PhysToParamRETROLOGUE))
    then
  result:=PhysToParamRETROLOGUE[pcc]
    else
  result:=-1;
end;

function TVSTTransformerRETROLOGUE.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysRETROLOGUE)) and (paramnr<=high(ParamToPhysRETROLOGUE))
    then
  result:=ParamToPhysRETROLOGUE[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerRETROLOGUE.ControllerName(pcc:integer):string;
begin
  result:=RETROLOGUENames[pcc]
end;

function TVSTTransformerRETROLOGUE.ForceCCOverHost(var m:TMidiEvent): boolean;
VAR pcc:integer;
begin
  result:=false;
  pcc:=m.data1;
  if pcc in [9,65,5,10,11,12,13,14,15,20,21,22,23,24,25,30,31,32,33,34,35,44,111,112,113,45,74,71,72,79,66,60,61,62,63,75,76,77,78,50,50,53,51,55,58,56] then
  begin
    pcc:=PhysToParam(pcc);
    result:=pcc>=0;
    if result then m.data1:=pcc;
  end
end;
{$endif}


const XRECDS2CC_BANKSELECTLOW = 0;
const XRECDS2CC_MODULATIONWHEEL = 1;
const XRECDS2CC_OSC1WAVE = 13;
const XRECDS2PARM_OSC1WAVE = 0;
const XRECDS2CC_OSC1FOOT = 10;
const XRECDS2PARM_OSC1FOOT = 1;
const XRECDS2CC_OSC1SEMI = 11;
const XRECDS2PARM_OSC1SEMI = 2;
const XRECDS2CC_OSC1LEVEL = 14;
const XRECDS2PARM_OSC1LEVEL = 3;
const XRECDS2CC_OSC2WAVE = 23;
const XRECDS2PARM_OSC2WAVE = 4;
const XRECDS2CC_OSC2FOOT = 20;
const XRECDS2PARM_OSC2FOOT = 5;
const XRECDS2CC_OSC2SEMI = 21;
const XRECDS2PARM_OSC2SEMI = 6;
const XRECDS2CC_OSC2LEVEL = 24;
const XRECDS2PARM_OSC2LEVEL = 7;
const XRECDS2CC_NOISECOLOR = 111;
const XRECDS2PARM_NOISECOLOR = 8;
const XRECDS2CC_NOISELEVEL = 112;
const XRECDS2PARM_NOISELEVEL = 9;
const XRECDS2CC_LFO1WAVE = 50;
const XRECDS2PARM_LFO1WAVE = 10;
const XRECDS2CC_LFO1RATE = 53;
const XRECDS2PARM_LFO1RATE = 11;
const XRECDS2CC_LFO1DELAYENABLE = 52;
const XRECDS2PARM_LFO1DELAYENABLE = 12;
const XRECDS2CC_LFO2WAVE = 55;
const XRECDS2PARM_LFO2WAVE = 13;
const XRECDS2CC_LFO2RATE = 58;
const XRECDS2PARM_LFO2RATE = 14;
const XRECDS2CC_LFO2DELAYENABLE = 57;
const XRECDS2PARM_LFO2DELAYENABLE = 15;
const XRECDS2CC_LFO1DELAY = 51;
const XRECDS2PARM_LFO1DELAY = 16;
const XRECDS2CC_VCAATTACK = 60;
const XRECDS2PARM_VCAATTACK = 17;
const XRECDS2CC_VCADECAY = 61;
const XRECDS2PARM_VCADECAY = 18;
const XRECDS2CC_VCASUSTAIN = 62;
const XRECDS2PARM_VCASUSTAIN = 19;
const XRECDS2CC_VCARELEASE = 63;
const XRECDS2PARM_VCARELEASE = 20;
const XRECDS2CC_VCALEVEL = 66;
const XRECDS2PARM_VCALEVEL = 21;
const XRECDS2CC_VCFATTACK = 75;
const XRECDS2PARM_VCFATTACK = 22;
const XRECDS2CC_VCFDECAY = 76;
const XRECDS2PARM_VCFDECAY = 23;
const XRECDS2CC_VCFSUSTAIN = 77;
const XRECDS2PARM_VCFSUSTAIN = 24;
const XRECDS2CC_VCFRELEASE = 78;
const XRECDS2PARM_VCFRELEASE = 25;
const XRECDS2CC_VCFLEVEL = 72;
const XRECDS2PARM_VCFLEVEL = 26;
const XRECDS2CC_CUTOFF = 74;
const XRECDS2PARM_CUTOFF = 27;
const XRECDS2CC_RESONANCE = 71;
const XRECDS2PARM_RESONANCE = 28;
const XRECDS2CC_VCFKEYFOLLOW = 79;
const XRECDS2PARM_VCFKEYFOLLOW = 29;
const XRECDS2CC_OUTPUTLEVEL = 15;
const XRECDS2PARM_OUTPUTLEVEL = 30;
const XRECDS2CC_OSC1MODSELECT = 19;
const XRECDS2PARM_OSC1MODSELECT = 31;
const XRECDS2CC_OSC1MODLEVEL = 18;
const XRECDS2PARM_OSC1MODLEVEL = 32;
const XRECDS2CC_OSC2MODSELECT = 29;
const XRECDS2PARM_OSC2MODSELECT = 33;
const XRECDS2CC_OSC2MODLEVEL = 28;
const XRECDS2PARM_OSC2MODLEVEL = 34;
const XRECDS2CC_VCFMODSELECT = 68;
const XRECDS2PARM_VCFMODSELECT = 35;
const XRECDS2CC_VCFMODLEVEL = 67;
const XRECDS2PARM_VCFMODLEVEL = 36;
const XRECDS2CC_VCAMODSELECT = 70;
const XRECDS2PARM_VCAMODSELECT = 37;
const XRECDS2CC_VCAMODLEVEL = 69;
const XRECDS2PARM_VCAMODLEVEL = 38;
const XRECDS2CC_PWM1MODSELECT = 17;
const XRECDS2PARM_PWM1MODSELECT = 39;
const XRECDS2CC_PWM1MODLEVEL = 16;
const XRECDS2PARM_PWM1MODLEVEL = 40;
const XRECDS2CC_OSCFAT = 107;
const XRECDS2PARM_OSCFAT = 41;
const XRECDS2CC_GLIDE = 5;
const XRECDS2PARM_GLIDE = 42;
const XRECDS2CC_GLIDEENABLE = 65;
const XRECDS2PARM_GLIDEENABLE = 43;
const XRECDS2CC_POLY = 9;
const XRECDS2PARM_POLY = 44;
const XRECDS2CC_SUSTAIN = 64;
const XRECDS2CC_OSC3WAVE = 33;
const XRECDS2PARM_OSC3WAVE = 45;
const XRECDS2CC_OSC3FOOT = 30;
const XRECDS2PARM_OSC3FOOT = 46;
const XRECDS2CC_OSC3SEMI = 31;
const XRECDS2PARM_OSC3SEMI = 47;
const XRECDS2CC_OSC3LEVEL = 34;
const XRECDS2PARM_OSC3LEVEL = 48;
const XRECDS2CC_OSC4WAVE = 43;
const XRECDS2PARM_OSC4WAVE = 49;
const XRECDS2CC_OSC4FOOT = 40;
const XRECDS2PARM_OSC4FOOT = 50;
const XRECDS2CC_OSC4SEMI = 41;
const XRECDS2PARM_OSC4SEMI = 51;
const XRECDS2CC_OSC4LEVEL = 44;
const XRECDS2PARM_OSC4LEVEL = 52;
const XRECDS2CC_PWM2MODSELECT = 27;
const XRECDS2PARM_PWM2MODSELECT = 53;
const XRECDS2CC_PWM2MODLEVEL = 26;
const XRECDS2PARM_PWM2MODLEVEL = 54;
const XRECDS2CC_PWM3MODSELECT = 37;
const XRECDS2PARM_PWM3MODSELECT = 55;
const XRECDS2CC_PWM3MODLEVEL = 36;
const XRECDS2PARM_PWM3MODLEVEL = 56;
const XRECDS2CC_PWM4MODSELECT = 47;
const XRECDS2PARM_PWM4MODSELECT = 57;
const XRECDS2CC_PWM4MODLEVEL = 46;
const XRECDS2PARM_PWM4MODLEVEL = 58;
const XRECDS2CC_OSC3MODSELECT = 39;
const XRECDS2PARM_OSC3MODSELECT = 59;
const XRECDS2CC_OSC3MODLEVEL = 38;
const XRECDS2PARM_OSC3MODLEVEL = 60;
const XRECDS2CC_OSC4MODSELECT = 49;
const XRECDS2PARM_OSC4MODSELECT = 61;
const XRECDS2CC_OSC4MODLEVEL = 48;
const XRECDS2PARM_OSC4MODLEVEL = 62;
const XRECDS2CC_LFO2DELAY = 56;
const XRECDS2PARM_LFO2DELAY = 63;
const XRECDS2CC_DELAYAMOUNT = 80;
const XRECDS2PARM_DELAYAMOUNT = 64;
const XRECDS2CC_DELAYTIME = 81;
const XRECDS2PARM_DELAYTIME = 65;
const XRECDS2CC_REVERBAMOUNT = 82;
const XRECDS2PARM_REVERBAMOUNT = 66;
const XRECDS2CC_REVERBTIME = 83;
const XRECDS2PARM_REVERBTIME = 67;
const XRECDS2CC_CHORUSDEPTH = 84;
const XRECDS2PARM_CHORUSDEPTH = 68;
const XRECDS2CC_CHORUSRATE = 85;
const XRECDS2PARM_CHORUSRATE = 69;
const XRECDS2CC_PHASERDEPTH = 86;
const XRECDS2PARM_PHASERDEPTH = 70;
const XRECDS2CC_PHASERRATE = 87;
const XRECDS2PARM_PHASERRATE = 71;
const XRECDS2CC_DELAYFEEDBACK = 88;
const XRECDS2PARM_DELAYFEEDBACK = 72;
const XRECDS2CC_REVERBFEEDBACK = 89;
const XRECDS2PARM_REVERBFEEDBACK = 73;
const XRECDS2CC_EFFECTSETTINGS = 96;
const XRECDS2PARM_EFFECTSETTINGS = 74;
const XRECDS2CC_RINGMODULATION = 113;
const XRECDS2PARM_RINGMODULATION = 75;
const XRECDS2CC_OSC2DETUNE = 25;
const XRECDS2PARM_OSC2DETUNE = 76;
const XRECDS2CC_OSC3DETUNE = 35;
const XRECDS2PARM_OSC3DETUNE = 77;
const XRECDS2CC_OSC4DETUNE = 45;
const XRECDS2PARM_OSC4DETUNE = 78;
const XRECDS2CC_OSC1_SYNC = 102;
const XRECDS2PARM_OSC1_SYNC = 79;
const XRECDS2CC_OSC2_SYNC = 103;
const XRECDS2PARM_OSC2_SYNC = 80;
const XRECDS2CC_OSC3_SYNC = 104;
const XRECDS2PARM_OSC3_SYNC = 81;
const XRECDS2CC_OSC4_SYNC = 105;
const XRECDS2PARM_OSC4_SYNC = 82;
const VirtualToPhysXRECDS2: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,9,10,11,-1,13,14,15,16,17,18,19,20,21,-1,23,24,25,26,27,28,29,30,31,-1,33,34,35,36,37,38,39,40,41,-1,43,44,45,46,47,48,49,50,51,52,53,-1,55,56,57,58,-1,60,61,62,63,64,65,66,67,68,69,70,71,72,-1,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,102,103,104,105,-1,107,-1,-1,-1,111,112,113,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualXRECDS2: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,9,10,11,-1,13,14,15,16,17,18,19,20,21,-1,23,24,25,26,27,28,29,30,31,-1,33,34,35,36,37,38,39,40,41,-1,43,44,45,46,47,48,49,50,51,52,53,-1,55,56,57,58,-1,60,61,62,63,64,65,66,67,68,69,70,71,72,-1,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,102,103,104,105,-1,107,-1,-1,-1,111,112,113,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountXRECDS2 = 83;
const ParamToPhysXRECDS2: array[0..ParamCountXRECDS2-1] of integer = (
13,10,11,14,23,20,21,24,111,112,50,53,52,55,58,57,51,60,61,62,63,66,75,76,77,78,72,74,71,79,15,19,18,29,28,68,67,70,69,17,16,107,5,65,9,33,30,31,34,43,40,41,44,27,26,37,36,47,46,39,38,49,48,56,80,81,82,83,84,85,86,87,88,89,96,113,25,35,45,102,103,104,105);
const PhysToParamXRECDS2: array[0..127] of integer = (-1,-1,-1,-1,-1,42,-1,-1,-1,44,1,2,-1,0,3,30,40,39,32,31,5,6,-1,4,7,76,54,53,34,33,46,47,-1,45,48,77,56,55,60,59,50,51,-1,49,52,78,58,57,62,61,10,16,12,11,-1,13,63,15,14,-1,17,18,19,20,-1,43,21,36,35,38,37,28,26,-1,27,22,23,24,25,29,64,65,66,67,68,69,70,71,72,73,-1,-1,-1,-1,-1,-1,74,-1,-1,-1,-1,-1,79,80,81,82,-1,41,-1,-1,-1,8,9,75,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerXRECDS2}
const XRECDS2Names:array[0..127] of string = (
   'Bank Select Low','Modulation Wheel','','','','GLIDE','','','','POLY','OSC1FOOT','OSC1SEMI','','OSC1WAVE','OSC1LEVEL','OUTPUTLEVEL',
   'PWM1MODLEVEL','PWM1MODSELECT','OSC1MODLEVEL','OSC1MODSELECT','OSC2FOOT','OSC2SEMI','','OSC2WAVE','OSC2LEVEL','OSC2DETUNE','PWM2MODLEVEL','PWM2MODSELECT','OSC2MODLEVEL','OSC2MODSELECT','OSC3FOOT','OSC3SEMI',
   '','OSC3WAVE','OSC3LEVEL','OSC3DETUNE','PWM3MODLEVEL','PWM3MODSELECT','OSC3MODLEVEL','OSC3MODSELECT','OSC4FOOT','OSC4SEMI','','OSC4WAVE','OSC4LEVEL','OSC4DETUNE','PWM4MODLEVEL','PWM4MODSELECT',
   'OSC4MODLEVEL','OSC4MODSELECT','LFO1WAVE','LFO1DELAY','LFO1DELAYENABLE','LFO1RATE','','LFO2WAVE','LFO2DELAY','LFO2DELAYENABLE','LFO2RATE','','VCAATTACK','VCADECAY','VCASUSTAIN','VCARELEASE',
   'Sustain','GLIDEENABLE','VCALEVEL','VCFMODLEVEL','VCFMODSELECT','VCAMODLEVEL','VCAMODSELECT','RESONANCE','VCFLEVEL','','CUTOFF','VCFATTACK','VCFDECAY','VCFSUSTAIN','VCFRELEASE','VCFKEYFOLLOW',
   'DelayAmount','DelayTime','ReverbAmount','ReverbTime','ChorusDepth','ChorusRate','PhaserDepth','PhaserRate','DelayFeedback','ReverbFeedback','','','','','','',
   'EffectSettings','','','','','','OSC1_SYNC','OSC2_SYNC','OSC3_SYNC','OSC4_SYNC','','OSCFAT','','','','NOISECOLOR',
   'NOISELEVEL','RingModulation','','','','','','','','','','','','','',''
);

function TVSTTransformerXRECDS2.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysXRECDS2)) and (vcc<=high(VirtualToPhysXRECDS2))
    then
    begin
      result:=VirtualToPhysXRECDS2[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerXRECDS2.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualXRECDS2)) and (pcc<=high(PhysToVirtualXRECDS2))
    then
  result:=PhysToVirtualXRECDS2[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerXRECDS2.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamXRECDS2)) and (pcc<=high(PhysToParamXRECDS2))
    then
  result:=PhysToParamXRECDS2[pcc]
    else
  result:=-1;
end;

function TVSTTransformerXRECDS2.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysXRECDS2)) and (paramnr<=high(ParamToPhysXRECDS2))
    then
  result:=ParamToPhysXRECDS2[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerXRECDS2.ControllerName(pcc:integer):string;
begin
  result:=XRECDS2Names[pcc]
end;
{$endif}


const XREWAVEPLAYERCC_BANKSELECT032 = 0;
const XREWAVEPLAYERCC_POSITION0 = 10;
const XREWAVEPLAYERPARM_POSITION0 = 0;
const XREWAVEPLAYERCC_POSITION1 = 11;
const XREWAVEPLAYERPARM_POSITION1 = 1;
const XREWAVEPLAYERCC_POSITION2 = 12;
const XREWAVEPLAYERPARM_POSITION2 = 2;
const XREWAVEPLAYERCC_LENGTH0 = 13;
const XREWAVEPLAYERPARM_LENGTH0 = 3;
const XREWAVEPLAYERCC_LENGTH1 = 14;
const XREWAVEPLAYERPARM_LENGTH1 = 4;
const XREWAVEPLAYERCC_LENGTH2 = 15;
const XREWAVEPLAYERPARM_LENGTH2 = 5;
const XREWAVEPLAYERCC_DELAYAMOUNT = 80;
const XREWAVEPLAYERPARM_DELAYAMOUNT = 6;
const XREWAVEPLAYERCC_DELAYTIME = 81;
const XREWAVEPLAYERPARM_DELAYTIME = 7;
const XREWAVEPLAYERCC_REVERBAMOUNT = 82;
const XREWAVEPLAYERPARM_REVERBAMOUNT = 8;
const XREWAVEPLAYERCC_REVERBTIME = 83;
const XREWAVEPLAYERPARM_REVERBTIME = 9;
const XREWAVEPLAYERCC_CHORUSDEPTH = 84;
const XREWAVEPLAYERPARM_CHORUSDEPTH = 10;
const XREWAVEPLAYERCC_CHORUSRATE = 85;
const XREWAVEPLAYERPARM_CHORUSRATE = 11;
const XREWAVEPLAYERCC_PHASERDEPTH = 86;
const XREWAVEPLAYERPARM_PHASERDEPTH = 12;
const XREWAVEPLAYERCC_PHASERRATE = 87;
const XREWAVEPLAYERPARM_PHASERRATE = 13;
const XREWAVEPLAYERCC_DELAYFEEDBACK = 88;
const XREWAVEPLAYERPARM_DELAYFEEDBACK = 14;
const XREWAVEPLAYERCC_REVERBFEEDBACK = 89;
const XREWAVEPLAYERPARM_REVERBFEEDBACK = 15;
const XREWAVEPLAYERCC_EFFECTSETTINGS = 96;
const XREWAVEPLAYERPARM_EFFECTSETTINGS = 16;
const VirtualToPhysXREWAVEPLAYER: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualXREWAVEPLAYER: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountXREWAVEPLAYER = 17;
const ParamToPhysXREWAVEPLAYER: array[0..ParamCountXREWAVEPLAYER-1] of integer = (
10,11,12,13,14,15,80,81,82,83,84,85,86,87,88,89,96);
const PhysToParamXREWAVEPLAYER: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,2,3,4,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,6,7,8,9,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,16,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerXREWAVEPLAYER}
const XREWAVEPLAYERNames:array[0..127] of string = (
   'Bank Select 0, 32','','','','','','','','','','Position0','Position1','Position2','Length0','Length1','Length2',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   'DelayAmount','DelayTime','ReverbAmount','ReverbTime','ChorusDepth','ChorusRate','PhaserDepth','PhaserRate','DelayFeedback','ReverbFeedback','','','','','','',
   'EffectSettings','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);
function TVSTTransformerXREWAVEPLAYER.ReactToProgramChange:boolean;
begin
  result:=false
end;


function TVSTTransformerXREWAVEPLAYER.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysXREWAVEPLAYER)) and (vcc<=high(VirtualToPhysXREWAVEPLAYER))
    then
    begin
      result:=VirtualToPhysXREWAVEPLAYER[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerXREWAVEPLAYER.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualXREWAVEPLAYER)) and (pcc<=high(PhysToVirtualXREWAVEPLAYER))
    then
  result:=PhysToVirtualXREWAVEPLAYER[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerXREWAVEPLAYER.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamXREWAVEPLAYER)) and (pcc<=high(PhysToParamXREWAVEPLAYER))
    then
  result:=PhysToParamXREWAVEPLAYER[pcc]
    else
  result:=-1;
end;

function TVSTTransformerXREWAVEPLAYER.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysXREWAVEPLAYER)) and (paramnr<=high(ParamToPhysXREWAVEPLAYER))
    then
  result:=ParamToPhysXREWAVEPLAYER[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerXREWAVEPLAYER.ControllerName(pcc:integer):string;
begin
  result:=XREWAVEPLAYERNames[pcc]
end;
{$endif}


const XREMODULARCC_BANKSELECTLOW = 0;
const XREMODULARCC_MODULATIONWHEEL = 1;
const XREMODULARCC_OSC1WAVE = 13;
const XREMODULARPARM_OSC1WAVE = 0;
const XREMODULARCC_OSC1FOOT = 10;
const XREMODULARPARM_OSC1FOOT = 1;
const XREMODULARCC_OSC1SEMI = 11;
const XREMODULARPARM_OSC1SEMI = 2;
const XREMODULARCC_OSC1LEVEL = 14;
const XREMODULARPARM_OSC1LEVEL = 3;
const XREMODULARCC_OSC2WAVE = 23;
const XREMODULARPARM_OSC2WAVE = 4;
const XREMODULARCC_OSC2FOOT = 20;
const XREMODULARPARM_OSC2FOOT = 5;
const XREMODULARCC_OSC2SEMI = 21;
const XREMODULARPARM_OSC2SEMI = 6;
const XREMODULARCC_OSC2LEVEL = 24;
const XREMODULARPARM_OSC2LEVEL = 7;
const XREMODULARCC_NOISECOLOR = 111;
const XREMODULARPARM_NOISECOLOR = 8;
const XREMODULARCC_NOISELEVEL = 112;
const XREMODULARPARM_NOISELEVEL = 9;
const XREMODULARCC_LFO1WAVE = 50;
const XREMODULARPARM_LFO1WAVE = 10;
const XREMODULARCC_LFO1RATE = 53;
const XREMODULARPARM_LFO1RATE = 11;
const XREMODULARCC_LFO1DELAYENABLE = 52;
const XREMODULARPARM_LFO1DELAYENABLE = 12;
const XREMODULARCC_LFO2WAVE = 55;
const XREMODULARPARM_LFO2WAVE = 13;
const XREMODULARCC_LFO2RATE = 58;
const XREMODULARPARM_LFO2RATE = 14;
const XREMODULARCC_LFO2DELAYENABLE = 57;
const XREMODULARPARM_LFO2DELAYENABLE = 15;
const XREMODULARCC_LFO1DELAY = 51;
const XREMODULARPARM_LFO1DELAY = 16;
const XREMODULARCC_VCAATTACK = 60;
const XREMODULARPARM_VCAATTACK = 17;
const XREMODULARCC_VCADECAY = 61;
const XREMODULARPARM_VCADECAY = 18;
const XREMODULARCC_VCASUSTAIN = 62;
const XREMODULARPARM_VCASUSTAIN = 19;
const XREMODULARCC_VCARELEASE = 63;
const XREMODULARPARM_VCARELEASE = 20;
const XREMODULARCC_VCALEVEL = 66;
const XREMODULARPARM_VCALEVEL = 21;
const XREMODULARCC_VCFATTACK = 75;
const XREMODULARPARM_VCFATTACK = 22;
const XREMODULARCC_VCFDECAY = 76;
const XREMODULARPARM_VCFDECAY = 23;
const XREMODULARCC_VCFSUSTAIN = 77;
const XREMODULARPARM_VCFSUSTAIN = 24;
const XREMODULARCC_VCFRELEASE = 78;
const XREMODULARPARM_VCFRELEASE = 25;
const XREMODULARCC_VCFLEVEL = 72;
const XREMODULARPARM_VCFLEVEL = 26;
const XREMODULARCC_CUTOFF = 74;
const XREMODULARPARM_CUTOFF = 27;
const XREMODULARCC_RESONANCE = 71;
const XREMODULARPARM_RESONANCE = 28;
const XREMODULARCC_VCFKEYFOLLOW = 79;
const XREMODULARPARM_VCFKEYFOLLOW = 29;
const XREMODULARCC_OUTPUTLEVEL = 15;
const XREMODULARPARM_OUTPUTLEVEL = 30;
const XREMODULARCC_OSC1MODSELECT = 19;
const XREMODULARPARM_OSC1MODSELECT = 31;
const XREMODULARCC_OSC1MODLEVEL = 18;
const XREMODULARPARM_OSC1MODLEVEL = 32;
const XREMODULARCC_OSC2MODSELECT = 29;
const XREMODULARPARM_OSC2MODSELECT = 33;
const XREMODULARCC_OSC2MODLEVEL = 28;
const XREMODULARPARM_OSC2MODLEVEL = 34;
const XREMODULARCC_VCFMODSELECT = 68;
const XREMODULARPARM_VCFMODSELECT = 35;
const XREMODULARCC_VCFMODLEVEL = 67;
const XREMODULARPARM_VCFMODLEVEL = 36;
const XREMODULARCC_VCAMODSELECT = 70;
const XREMODULARPARM_VCAMODSELECT = 37;
const XREMODULARCC_VCAMODLEVEL = 69;
const XREMODULARPARM_VCAMODLEVEL = 38;
const XREMODULARCC_PWM1MODSELECT = 17;
const XREMODULARPARM_PWM1MODSELECT = 39;
const XREMODULARCC_PWM1MODLEVEL = 16;
const XREMODULARPARM_PWM1MODLEVEL = 40;
const XREMODULARCC_OSCFAT = 107;
const XREMODULARPARM_OSCFAT = 41;
const XREMODULARCC_GLIDE = 5;
const XREMODULARPARM_GLIDE = 42;
const XREMODULARCC_GLIDEENABLE = 65;
const XREMODULARPARM_GLIDEENABLE = 43;
const XREMODULARCC_POLY = 9;
const XREMODULARPARM_POLY = 44;
const XREMODULARCC_SUSTAIN = 64;
const XREMODULARCC_OSC3WAVE = 33;
const XREMODULARPARM_OSC3WAVE = 45;
const XREMODULARCC_OSC3FOOT = 30;
const XREMODULARPARM_OSC3FOOT = 46;
const XREMODULARCC_OSC3SEMI = 31;
const XREMODULARPARM_OSC3SEMI = 47;
const XREMODULARCC_OSC3LEVEL = 34;
const XREMODULARPARM_OSC3LEVEL = 48;
const XREMODULARCC_OSC4WAVE = 43;
const XREMODULARPARM_OSC4WAVE = 49;
const XREMODULARCC_OSC4FOOT = 40;
const XREMODULARPARM_OSC4FOOT = 50;
const XREMODULARCC_OSC4SEMI = 41;
const XREMODULARPARM_OSC4SEMI = 51;
const XREMODULARCC_OSC4LEVEL = 44;
const XREMODULARPARM_OSC4LEVEL = 52;
const XREMODULARCC_PWM2MODSELECT = 27;
const XREMODULARPARM_PWM2MODSELECT = 53;
const XREMODULARCC_PWM2MODLEVEL = 26;
const XREMODULARPARM_PWM2MODLEVEL = 54;
const XREMODULARCC_PWM3MODSELECT = 37;
const XREMODULARPARM_PWM3MODSELECT = 55;
const XREMODULARCC_PWM3MODLEVEL = 36;
const XREMODULARPARM_PWM3MODLEVEL = 56;
const XREMODULARCC_PWM4MODSELECT = 47;
const XREMODULARPARM_PWM4MODSELECT = 57;
const XREMODULARCC_PWM4MODLEVEL = 46;
const XREMODULARPARM_PWM4MODLEVEL = 58;
const XREMODULARCC_OSC3MODSELECT = 39;
const XREMODULARPARM_OSC3MODSELECT = 59;
const XREMODULARCC_OSC3MODLEVEL = 38;
const XREMODULARPARM_OSC3MODLEVEL = 60;
const XREMODULARCC_OSC4MODSELECT = 49;
const XREMODULARPARM_OSC4MODSELECT = 61;
const XREMODULARCC_OSC4MODLEVEL = 48;
const XREMODULARPARM_OSC4MODLEVEL = 62;
const XREMODULARCC_LFO2DELAY = 56;
const XREMODULARPARM_LFO2DELAY = 63;
const XREMODULARCC_DELAYAMOUNT = 80;
const XREMODULARPARM_DELAYAMOUNT = 64;
const XREMODULARCC_DELAYTIME = 81;
const XREMODULARPARM_DELAYTIME = 65;
const XREMODULARCC_REVERBAMOUNT = 82;
const XREMODULARPARM_REVERBAMOUNT = 66;
const XREMODULARCC_REVERBTIME = 83;
const XREMODULARPARM_REVERBTIME = 67;
const XREMODULARCC_CHORUSDEPTH = 84;
const XREMODULARPARM_CHORUSDEPTH = 68;
const XREMODULARCC_CHORUSRATE = 85;
const XREMODULARPARM_CHORUSRATE = 69;
const XREMODULARCC_PHASERDEPTH = 86;
const XREMODULARPARM_PHASERDEPTH = 70;
const XREMODULARCC_PHASERRATE = 87;
const XREMODULARPARM_PHASERRATE = 71;
const XREMODULARCC_DELAYFEEDBACK = 88;
const XREMODULARPARM_DELAYFEEDBACK = 72;
const XREMODULARCC_REVERBFEEDBACK = 89;
const XREMODULARPARM_REVERBFEEDBACK = 73;
const XREMODULARCC_EFFECTSETTINGS = 96;
const XREMODULARPARM_EFFECTSETTINGS = 74;
const XREMODULARCC_RINGMODULATION = 113;
const XREMODULARPARM_RINGMODULATION = 75;
const XREMODULARCC_OSC2DETUNE = 25;
const XREMODULARPARM_OSC2DETUNE = 76;
const XREMODULARCC_OSC3DETUNE = 35;
const XREMODULARPARM_OSC3DETUNE = 77;
const XREMODULARCC_OSC4DETUNE = 45;
const XREMODULARPARM_OSC4DETUNE = 78;
const XREMODULARCC_OSC1_SYNC = 102;
const XREMODULARPARM_OSC1_SYNC = 79;
const XREMODULARCC_OSC2_SYNC = 103;
const XREMODULARPARM_OSC2_SYNC = 80;
const XREMODULARCC_OSC3_SYNC = 104;
const XREMODULARPARM_OSC3_SYNC = 81;
const XREMODULARCC_OSC4_SYNC = 105;
const XREMODULARPARM_OSC4_SYNC = 82;
const VirtualToPhysXREMODULAR: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,9,10,11,-1,13,14,15,16,17,18,19,20,21,-1,23,24,25,26,27,28,29,30,31,-1,33,34,35,36,37,38,39,40,41,-1,43,44,45,46,47,48,49,50,51,52,53,-1,55,56,57,58,-1,60,61,62,63,64,65,66,67,68,69,70,71,72,-1,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,102,103,104,105,-1,107,-1,-1,-1,111,112,113,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualXREMODULAR: array[0..127] of integer = (-1,1,-1,-1,-1,5,-1,-1,-1,9,10,11,-1,13,14,15,16,17,18,19,20,21,-1,23,24,25,26,27,28,29,30,31,-1,33,34,35,36,37,38,39,40,41,-1,43,44,45,46,47,48,49,50,51,52,53,-1,55,56,57,58,-1,60,61,62,63,64,65,66,67,68,69,70,71,72,-1,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,102,103,104,105,-1,107,-1,-1,-1,111,112,113,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountXREMODULAR = 83;
const ParamToPhysXREMODULAR: array[0..ParamCountXREMODULAR-1] of integer = (
13,10,11,14,23,20,21,24,111,112,50,53,52,55,58,57,51,60,61,62,63,66,75,76,77,78,72,74,71,79,15,19,18,29,28,68,67,70,69,17,16,107,5,65,9,33,30,31,34,43,40,41,44,27,26,37,36,47,46,39,38,49,48,56,80,81,82,83,84,85,86,87,88,89,96,113,25,35,45,102,103,104,105);
const PhysToParamXREMODULAR: array[0..127] of integer = (-1,-1,-1,-1,-1,42,-1,-1,-1,44,1,2,-1,0,3,30,40,39,32,31,5,6,-1,4,7,76,54,53,34,33,46,47,-1,45,48,77,56,55,60,59,50,51,-1,49,52,78,58,57,62,61,10,16,12,11,-1,13,63,15,14,-1,17,18,19,20,-1,43,21,36,35,38,37,28,26,-1,27,22,23,24,25,29,64,65,66,67,68,69,70,71,72,73,-1,-1,-1,-1,-1,-1,74,-1,-1,-1,-1,-1,79,80,81,82,-1,41,-1,-1,-1,8,9,75,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerXREMODULAR}
const XREMODULARNames:array[0..127] of string = (
   'Bank Select Low','Modulation Wheel','','','','GLIDE','','','','POLY','OSC1FOOT','OSC1SEMI','','OSC1WAVE','OSC1LEVEL','OUTPUTLEVEL',
   'PWM1MODLEVEL','PWM1MODSELECT','OSC1MODLEVEL','OSC1MODSELECT','OSC2FOOT','OSC2SEMI','','OSC2WAVE','OSC2LEVEL','OSC2DETUNE','PWM2MODLEVEL','PWM2MODSELECT','OSC2MODLEVEL','OSC2MODSELECT','OSC3FOOT','OSC3SEMI',
   '','OSC3WAVE','OSC3LEVEL','OSC3DETUNE','PWM3MODLEVEL','PWM3MODSELECT','OSC3MODLEVEL','OSC3MODSELECT','OSC4FOOT','OSC4SEMI','','OSC4WAVE','OSC4LEVEL','OSC4DETUNE','PWM4MODLEVEL','PWM4MODSELECT',
   'OSC4MODLEVEL','OSC4MODSELECT','LFO1WAVE','LFO1DELAY','LFO1DELAYENABLE','LFO1RATE','','LFO2WAVE','LFO2DELAY','LFO2DELAYENABLE','LFO2RATE','','VCAATTACK','VCADECAY','VCASUSTAIN','VCARELEASE',
   'Sustain','GLIDEENABLE','VCALEVEL','VCFMODLEVEL','VCFMODSELECT','VCAMODLEVEL','VCAMODSELECT','RESONANCE','VCFLEVEL','','CUTOFF','VCFATTACK','VCFDECAY','VCFSUSTAIN','VCFRELEASE','VCFKEYFOLLOW',
   'DelayAmount','DelayTime','ReverbAmount','ReverbTime','ChorusDepth','ChorusRate','PhaserDepth','PhaserRate','DelayFeedback','ReverbFeedback','','','','','','',
   'EffectSettings','','','','','','OSC1_SYNC','OSC2_SYNC','OSC3_SYNC','OSC4_SYNC','','OSCFAT','','','','NOISECOLOR',
   'NOISELEVEL','RingModulation','','','','','','','','','','','','','',''
);

function TVSTTransformerXREMODULAR.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysXREMODULAR)) and (vcc<=high(VirtualToPhysXREMODULAR))
    then
    begin
      result:=VirtualToPhysXREMODULAR[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerXREMODULAR.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualXREMODULAR)) and (pcc<=high(PhysToVirtualXREMODULAR))
    then
  result:=PhysToVirtualXREMODULAR[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerXREMODULAR.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamXREMODULAR)) and (pcc<=high(PhysToParamXREMODULAR))
    then
  result:=PhysToParamXREMODULAR[pcc]
    else
  result:=-1;
end;

function TVSTTransformerXREMODULAR.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysXREMODULAR)) and (paramnr<=high(ParamToPhysXREMODULAR))
    then
  result:=ParamToPhysXREMODULAR[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerXREMODULAR.ControllerName(pcc:integer):string;
begin
  result:=XREMODULARNames[pcc]
end;
{$endif}


const SYSTEM8CC_MOD = 1;
const SYSTEM8CC_CUTOFF = 3;
const SYSTEM8CC_PORTAMENTO = 5;
const SYSTEM8CC_RESO = 9;
const SYSTEM8CC_PEDALCONTROL = 11;
const SYSTEM8CC_EFFECTSDEPTH = 12;
const SYSTEM8CC_DELAYCHORUSTIME = 13;
const SYSTEM8CC_LFOVARIATION = 14;
const SYSTEM8CC_LFOTRIGENV = 15;
const SYSTEM8CC_MIXEROSC1 = 16;
const SYSTEM8CC_MIXEROSC2 = 17;
const SYSTEM8CC_MIXEROSC3SUBOSC = 18;
const SYSTEM8CC_MIXERNOISE = 19;
const SYSTEM8CC_OSC1COARSETUNE = 20;
const SYSTEM8CC_OSC1FINETUNE = 21;
const SYSTEM8CC_PITCHENV = 22;
const SYSTEM8CC_PITCHATTACK = 23;
const SYSTEM8CC_PITCHDECAY = 24;
const SYSTEM8CC_OSC3SUBOSCCOLOR = 25;
const SYSTEM8CC_LFOPITCH = 26;
const SYSTEM8CC_LFOFADETIME = 27;
const SYSTEM8CC_LFOFILTER = 28;
const SYSTEM8CC_LFORATE = 29;
const SYSTEM8CC_LFOAMP = 30;
const SYSTEM8CC_LFOWAVE = 35;
const SYSTEM8CC_BENDRANGE = 41;
const SYSTEM8CC_OSC1WAVE = 46;
const SYSTEM8CC_OSC1OCTAVE = 47;
const SYSTEM8CC_OSC3SUBOSCTUNE = 48;
const SYSTEM8CC_BENDGAIN = 49;
const SYSTEM8CC_OSC1COLOR = 50;
const SYSTEM8CC_FILTERVARIATION = 51;
const SYSTEM8CC_OSC1CROSSMOD = 52;
const SYSTEM8CC_FILTERVELOCITYSENS = 53;
const SYSTEM8CC_AMPVELOCITYSENS = 54;
const SYSTEM8CC_OSC2COLOR = 55;
const SYSTEM8CC_OSC2FINETUNE = 56;
const SYSTEM8CC_OSC1MOD = 60;
const SYSTEM8CC_OSC2WAVE = 61;
const SYSTEM8CC_OSC2OCTAVE = 62;
const SYSTEM8CC_OSC2MOD = 63;
const SYSTEM8CC_PEDALHOLD = 64;
const SYSTEM8CC_ARPSWITCH = 65;
const SYSTEM8CC_ARPTYPE = 66;
const SYSTEM8CC_ARPSTEP = 67;
const SYSTEM8CC_BENDSENSPITCH = 68;
const SYSTEM8CC_AMPTONE = 69;
const SYSTEM8CC_EFFECTSTYPE = 70;
const SYSTEM8CC_EFFECTSTONE = 71;
const SYSTEM8CC_DELAYCHORUSTYPE = 72;
const SYSTEM8CC_REVERBTYPE = 73;
const SYSTEM8CC_REVERBTIME = 74;
const SYSTEM8CC_BENDSENSFILTER = 75;
const SYSTEM8CC_SEL_SYSTEM8 = 76;
const SYSTEM8CC_SEL_JUPITER8 = 77;
const SYSTEM8CC_SEL_SH2 = 78;
const SYSTEM8CC_HPFCUTOFF = 79;
const SYSTEM8CC_MODSENSPITCH = 80;
const SYSTEM8CC_FILTERENV = 81;
const SYSTEM8CC_FILTERKEY = 82;
const SYSTEM8CC_FILTERATTACK = 83;
const SYSTEM8CC_FILTERDECAY = 84;
const SYSTEM8CC_FILTERSUSTAIN = 85;
const SYSTEM8CC_FILTERRELEASE = 86;
const SYSTEM8CC_OSC2COARSETUNE = 87;
const SYSTEM8CC_AMPATTACK = 89;
const SYSTEM8CC_AMPDECAY = 90;
const SYSTEM8CC_REVERBLEVEL = 91;
const SYSTEM8CC_AMP_CHANGE_NOTIFY = 92;
const SYSTEM8CC_DELAYCHORUSLEVEL = 94;
const SYSTEM8CC_AMPSUSTAIN = 96;
const SYSTEM8CC_AMPRELEASE = 97;
const SYSTEM8CC_OSC1VARIATION = 103;
const SYSTEM8CC_OSC2VARIATION = 104;
const SYSTEM8CC_OSC3SUBOSCWAVE = 107;
const SYSTEM8CC_FILTERTYPE = 108;
const SYSTEM8CC_MODSENSFILTER = 109;
const SYSTEM8CC_AMPLEVEL = 110;
const SYSTEM8CC_OSC2RING = 111;
const SYSTEM8CC_OSC2SYNC = 112;
const SYSTEM8CC_MIXERNOISETYPE = 114;
const SYSTEM8CC_LEGATO = 116;
const SYSTEM8CC_LFOKEYTRIG = 117;
const SYSTEM8CC_TEMPOSYNC = 118;
const SYSTEM8CC_MONO = 119;
const VirtualToPhysSYSTEM8: array[0..127] of integer = (-1,1,-1,3,-1,5,-1,-1,-1,9,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,-1,-1,-1,-1,35,-1,-1,-1,-1,-1,41,-1,-1,-1,-1,46,47,48,49,50,51,52,53,54,55,56,-1,-1,-1,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,-1,89,90,91,92,-1,94,-1,96,97,-1,-1,-1,-1,-1,103,104,-1,-1,107,108,109,110,111,112,-1,114,-1,116,117,118,119,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualSYSTEM8: array[0..127] of integer = (-1,1,-1,3,-1,5,-1,-1,-1,9,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,-1,-1,-1,-1,35,-1,-1,-1,-1,-1,41,-1,-1,-1,-1,46,47,48,49,50,51,52,53,54,55,56,-1,-1,-1,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,-1,89,90,91,92,-1,94,-1,96,97,-1,-1,-1,-1,-1,103,104,-1,-1,107,108,109,110,111,112,-1,114,-1,116,117,118,119,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToParamSYSTEM8: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerSYSTEM8}
const SYSTEM8Names:array[0..127] of string = (
   '','MOD','','CUTOFF','','PORTAMENTO','','','','RESO','','PEDAL CONTROL','EFFECTS DEPTH','DELAY/CHORUS TIME','LFO VARIATION','LFO TRIG ENV',
   'MIXER OSC1','MIXER OSC2','MIXER OSC3/SUB OSC','MIXER NOISE','OSC1 COARSE TUNE','OSC1 FINE TUNE','PITCH ENV','PITCH ATTACK','PITCH DECAY','OSC3/SUB OSC COLOR','LFO PITCH','LFO FADE TIME','LFO FILTER','LFO RATE','LFO AMP','',
   '','','','LFO WAVE','','','','','','BEND RANGE','','','','','OSC1 WAVE','OSC1 OCTAVE',
   'OSC3/SUB OSC TUNE','BEND GAIN','OSC1 COLOR','FILTER VARIATION','OSC1 CROSS MOD','FILTER VELOCITY SENS','AMP VELOCITY SENS','OSC2 COLOR','OSC2 FINE TUNE','','','','OSC1 MOD','OSC2 WAVE','OSC2 OCTAVE','OSC2 MOD',
   'PEDAL HOLD','ARPSWITCH','ARPTYPE','ARPSTEP','BEND SENS PITCH','AMP TONE','EFFECTS TYPE','EFFECTS TONE','DELAY/CHORUS TYPE','REVERB TYPE','REVERB TIME','BEND SENS FILTER','SEL_SYSTEM8','SEL_JUPITER8','SEL_SH2','HPF CUTOFF',
   'MOD SENS PITCH','FILTER ENV','FILTER KEY','FILTER ATTACK','FILTER DECAY','FILTER SUSTAIN','FILTER RELEASE','OSC2 COARSE TUNE','','AMP ATTACK','AMP DECAY','REVERB LEVEL','AMP_CHANGE_NOTIFY','','DELAY/CHORUS LEVEL','',
   'AMP SUSTAIN','AMP RELEASE','','','','','','OSC1 VARIATION','OSC2 VARIATION','','','OSC3/SUB OSC WAVE','FILTER TYPE','MOD SENS FILTER','AMP LEVEL','OSC2 RING',
   'OSC2 SYNC','','MIXER NOISE TYPE','','LEGATO','LFO KEY TRIG','TEMPO SYNC','MONO','','','','','','','',''
);

function TVSTTransformerSYSTEM8.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysSYSTEM8)) and (vcc<=high(VirtualToPhysSYSTEM8))
    then
    begin
      result:=VirtualToPhysSYSTEM8[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerSYSTEM8.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualSYSTEM8)) and (pcc<=high(PhysToVirtualSYSTEM8))
    then
  result:=PhysToVirtualSYSTEM8[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerSYSTEM8.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamSYSTEM8)) and (pcc<=high(PhysToParamSYSTEM8))
    then
  result:=PhysToParamSYSTEM8[pcc]
    else
  result:=-1;
end;

function TVSTTransformerSYSTEM8.ParamToPhys(paramnr: integer): integer;
begin
  result:=-1;
end;

function TVSTTransformerSYSTEM8.ControllerName(pcc:integer):string;
begin
  result:=SYSTEM8Names[pcc]
end;
{$endif}


const ODYSSEYCC_NOISECOLOR = 0;
const ODYSSEYPARM_NOISECOLOR = 0;
const VirtualToPhysODYSSEY: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualODYSSEY: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountODYSSEY = 1;
const ParamToPhysODYSSEY: array[0..ParamCountODYSSEY-1] of integer = (
0);
const PhysToParamODYSSEY: array[0..127] of integer = (0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerODYSSEY}
const ODYSSEYNames:array[0..127] of string = (
   'Noise Color','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerODYSSEY.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysODYSSEY)) and (vcc<=high(VirtualToPhysODYSSEY))
    then
    begin
      result:=VirtualToPhysODYSSEY[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerODYSSEY.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualODYSSEY)) and (pcc<=high(PhysToVirtualODYSSEY))
    then
  result:=PhysToVirtualODYSSEY[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerODYSSEY.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamODYSSEY)) and (pcc<=high(PhysToParamODYSSEY))
    then
  result:=PhysToParamODYSSEY[pcc]
    else
  result:=-1;
end;

function TVSTTransformerODYSSEY.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysODYSSEY)) and (paramnr<=high(ParamToPhysODYSSEY))
    then
  result:=ParamToPhysODYSSEY[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerODYSSEY.ControllerName(pcc:integer):string;
begin
  result:=ODYSSEYNames[pcc]
end;
{$endif}


const XREROMPLERCC_BANKSELECT032 = 0;
const XREROMPLERCC_VIBRATO = 11;
const XREROMPLERPARM_VIBRATO = 0;
const XREROMPLERCC_MONOGLIDE = 12;
const XREROMPLERPARM_MONOGLIDE = 1;
const XREROMPLERCC_MONOVOLUME = 13;
const XREROMPLERPARM_MONOVOLUME = 2;
const XREROMPLERCC_POLYVOLUME = 14;
const XREROMPLERPARM_POLYVOLUME = 3;
const XREROMPLERCC_MONOVCFATTACK = 15;
const XREROMPLERPARM_MONOVCFATTACK = 4;
const XREROMPLERCC_MONOBRILLIANCE = 16;
const XREROMPLERPARM_MONOBRILLIANCE = 5;
const XREROMPLERCC_POLYBRILLIANCE = 17;
const XREROMPLERPARM_POLYBRILLIANCE = 6;
const XREROMPLERCC_VCAA = 18;
const XREROMPLERPARM_VCAA = 7;
const XREROMPLERCC_VCAR = 19;
const XREROMPLERPARM_VCAR = 8;
const XREROMPLERCC_POLYVIBDIG = 20;
const XREROMPLERPARM_POLYVIBDIG = 9;
const XREROMPLERCC_MONOVIBDIG = 21;
const XREROMPLERPARM_MONOVIBDIG = 10;
const XREROMPLERCC_VIBDELAYDIG = 22;
const XREROMPLERPARM_VIBDELAYDIG = 11;
const XREROMPLERCC_ATTACK = 23;
const XREROMPLERPARM_ATTACK = 12;
const XREROMPLERCC_RELEASE = 24;
const XREROMPLERPARM_RELEASE = 13;
const XREROMPLERCC_TONE = 25;
const XREROMPLERPARM_TONE = 14;
const XREROMPLERCC_SOUND = 26;
const XREROMPLERPARM_SOUND = 15;
const XREROMPLERCC_VOLUME = 27;
const XREROMPLERPARM_VOLUME = 16;
const XREROMPLERCC_TUNE = 28;
const XREROMPLERPARM_TUNE = 17;
const XREROMPLERCC_DELAYAMOUNT = 80;
const XREROMPLERPARM_DELAYAMOUNT = 18;
const XREROMPLERCC_DELAYTIME = 81;
const XREROMPLERPARM_DELAYTIME = 19;
const XREROMPLERCC_REVERBAMOUNT = 82;
const XREROMPLERPARM_REVERBAMOUNT = 20;
const XREROMPLERCC_REVERBTIME = 83;
const XREROMPLERPARM_REVERBTIME = 21;
const XREROMPLERCC_CHORUSDEPTH = 84;
const XREROMPLERPARM_CHORUSDEPTH = 22;
const XREROMPLERCC_CHORUSRATE = 85;
const XREROMPLERPARM_CHORUSRATE = 23;
const XREROMPLERCC_PHASERDEPTH = 86;
const XREROMPLERPARM_PHASERDEPTH = 24;
const XREROMPLERCC_PHASERRATE = 87;
const XREROMPLERPARM_PHASERRATE = 25;
const XREROMPLERCC_DELAYFEEDBACK = 88;
const XREROMPLERPARM_DELAYFEEDBACK = 26;
const XREROMPLERCC_REVERBFEEDBACK = 89;
const XREROMPLERPARM_REVERBFEEDBACK = 27;
const XREROMPLERCC_EFFECTSETTINGS = 96;
const XREROMPLERPARM_EFFECTSETTINGS = 28;
const VirtualToPhysXREROMPLER: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualXREROMPLER: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountXREROMPLER = 29;
const ParamToPhysXREROMPLER: array[0..ParamCountXREROMPLER-1] of integer = (
11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,80,81,82,83,84,85,86,87,88,89,96);
const PhysToParamXREROMPLER: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,18,19,20,21,22,23,24,25,26,27,-1,-1,-1,-1,-1,-1,28,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerXREROMPLER}
const XREROMPLERNames:array[0..127] of string = (
   'Bank Select 0, 32','','','','','','','','','','','Vibrato','Mono Glide','Mono Volume','Poly Volume','Mono VCF Attack',
   'Mono Brilliance','Poly Brilliance','VCA A','VCA R','Poly Vib Dig','Mono Vib Dig','Vib Delay Dig','Attack','Release','Tone','Sound','Volume','Tune','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   'DelayAmount','DelayTime','ReverbAmount','ReverbTime','ChorusDepth','ChorusRate','PhaserDepth','PhaserRate','DelayFeedback','ReverbFeedback','','','','','','',
   'EffectSettings','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerXREROMPLER.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysXREROMPLER)) and (vcc<=high(VirtualToPhysXREROMPLER))
    then
    begin
      result:=VirtualToPhysXREROMPLER[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerXREROMPLER.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualXREROMPLER)) and (pcc<=high(PhysToVirtualXREROMPLER))
    then
  result:=PhysToVirtualXREROMPLER[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerXREROMPLER.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamXREROMPLER)) and (pcc<=high(PhysToParamXREROMPLER))
    then
  result:=PhysToParamXREROMPLER[pcc]
    else
  result:=-1;
end;

function TVSTTransformerXREROMPLER.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysXREROMPLER)) and (paramnr<=high(ParamToPhysXREROMPLER))
    then
  result:=ParamToPhysXREROMPLER[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerXREROMPLER.ControllerName(pcc:integer):string;
begin
  result:=XREROMPLERNames[pcc]
end;
{$endif}


const XRESYNTORCHESTRACC_BANKSELECT032 = 0;
const XRESYNTORCHESTRACC_VIBRATO = 11;
const XRESYNTORCHESTRAPARM_VIBRATO = 0;
const XRESYNTORCHESTRACC_MONOGLIDE = 12;
const XRESYNTORCHESTRAPARM_MONOGLIDE = 1;
const XRESYNTORCHESTRACC_MONOVOLUME = 13;
const XRESYNTORCHESTRAPARM_MONOVOLUME = 2;
const XRESYNTORCHESTRACC_POLYVOLUME = 14;
const XRESYNTORCHESTRAPARM_POLYVOLUME = 3;
const XRESYNTORCHESTRACC_RESONANCE = 15;
const XRESYNTORCHESTRAPARM_RESONANCE = 4;
const XRESYNTORCHESTRACC_MONOBRILLIANCE = 16;
const XRESYNTORCHESTRAPARM_MONOBRILLIANCE = 5;
const XRESYNTORCHESTRACC_POLYBRILLIANCE = 17;
const XRESYNTORCHESTRAPARM_POLYBRILLIANCE = 6;
const XRESYNTORCHESTRACC_SOFFIATO = 18;
const XRESYNTORCHESTRAPARM_SOFFIATO = 7;
const XRESYNTORCHESTRACC_DECAY = 19;
const XRESYNTORCHESTRAPARM_DECAY = 8;
const XRESYNTORCHESTRACC_POLYVIBDIG = 20;
const XRESYNTORCHESTRAPARM_POLYVIBDIG = 9;
const XRESYNTORCHESTRACC_MONOVIBDIG = 21;
const XRESYNTORCHESTRAPARM_MONOVIBDIG = 10;
const XRESYNTORCHESTRACC_VIBDELAYDIG = 22;
const XRESYNTORCHESTRAPARM_VIBDELAYDIG = 11;
const XRESYNTORCHESTRACC_POLYSOUND = 23;
const XRESYNTORCHESTRAPARM_POLYSOUND = 12;
const XRESYNTORCHESTRACC_MONOSOUND = 24;
const XRESYNTORCHESTRAPARM_MONOSOUND = 13;
const XRESYNTORCHESTRACC_LFO1SPEED = 25;
const XRESYNTORCHESTRAPARM_LFO1SPEED = 14;
const XRESYNTORCHESTRACC_LFO2SPEED = 26;
const XRESYNTORCHESTRAPARM_LFO2SPEED = 15;
const XRESYNTORCHESTRACC_WHAWHADIG = 27;
const XRESYNTORCHESTRAPARM_WHAWHADIG = 16;
const XRESYNTORCHESTRACC_DECAYDIG = 28;
const XRESYNTORCHESTRAPARM_DECAYDIG = 17;
const XRESYNTORCHESTRACC_PITCHSEMI = 29;
const XRESYNTORCHESTRAPARM_PITCHSEMI = 18;
const XRESYNTORCHESTRACC_DETUNE = 30;
const XRESYNTORCHESTRAPARM_DETUNE = 19;
const XRESYNTORCHESTRACC_DELAYAMOUNT = 80;
const XRESYNTORCHESTRAPARM_DELAYAMOUNT = 20;
const XRESYNTORCHESTRACC_DELAYTIME = 81;
const XRESYNTORCHESTRAPARM_DELAYTIME = 21;
const XRESYNTORCHESTRACC_REVERBAMOUNT = 82;
const XRESYNTORCHESTRAPARM_REVERBAMOUNT = 22;
const XRESYNTORCHESTRACC_REVERBTIME = 83;
const XRESYNTORCHESTRAPARM_REVERBTIME = 23;
const XRESYNTORCHESTRACC_CHORUSDEPTH = 84;
const XRESYNTORCHESTRAPARM_CHORUSDEPTH = 24;
const XRESYNTORCHESTRACC_CHORUSRATE = 85;
const XRESYNTORCHESTRAPARM_CHORUSRATE = 25;
const XRESYNTORCHESTRACC_PHASERDEPTH = 86;
const XRESYNTORCHESTRAPARM_PHASERDEPTH = 26;
const XRESYNTORCHESTRACC_PHASERRATE = 87;
const XRESYNTORCHESTRAPARM_PHASERRATE = 27;
const XRESYNTORCHESTRACC_DELAYFEEDBACK = 88;
const XRESYNTORCHESTRAPARM_DELAYFEEDBACK = 28;
const XRESYNTORCHESTRACC_REVERBFEEDBACK = 89;
const XRESYNTORCHESTRAPARM_REVERBFEEDBACK = 29;
const XRESYNTORCHESTRACC_EFFECTSETTINGS = 96;
const XRESYNTORCHESTRAPARM_EFFECTSETTINGS = 30;
const VirtualToPhysXRESYNTORCHESTRA: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const PhysToVirtualXRESYNTORCHESTRA: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,80,81,82,83,84,85,86,87,88,89,-1,-1,-1,-1,-1,-1,96,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
const ParamCountXRESYNTORCHESTRA = 31;
const ParamToPhysXRESYNTORCHESTRA: array[0..ParamCountXRESYNTORCHESTRA-1] of integer = (
11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,80,81,82,83,84,85,86,87,88,89,96);
const PhysToParamXRESYNTORCHESTRA: array[0..127] of integer = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,20,21,22,23,24,25,26,27,28,29,-1,-1,-1,-1,-1,-1,30,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
// Don't Edit this code. It was generated with PrjMidiMapping
{$ifdef VSTTransformerXRESYNTORCHESTRA}
const XRESYNTORCHESTRANames:array[0..127] of string = (
   'Bank Select 0, 32','','','','','','','','','','','Vibrato','Mono Glide','Mono Volume','Poly Volume','Resonance',
   'Mono Brilliance','Poly Brilliance','Soffiato','Decay','Poly Vib Dig','Mono Vib Dig','Vib Delay Dig','PolySound','MonoSound','LFO1Speed','LFO2Speed','WhaWhaDig','DecayDig','PitchSemi','Detune','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','','',
   'DelayAmount','DelayTime','ReverbAmount','ReverbTime','ChorusDepth','ChorusRate','PhaserDepth','PhaserRate','DelayFeedback','ReverbFeedback','','','','','','',
   'EffectSettings','','','','','','','','','','','','','','','',
   '','','','','','','','','','','','','','','',''
);

function TVSTTransformerXRESYNTORCHESTRA.VirtualToPhys(vcc: integer): integer;
begin
  result:=-1;
    if (vcc>=low(VirtualToPhysXRESYNTORCHESTRA)) and (vcc<=high(VirtualToPhysXRESYNTORCHESTRA))
    then
    begin
      result:=VirtualToPhysXRESYNTORCHESTRA[vcc];
      if (result = -1) then
      // don't map to inherited!
        if (vcc>=VirtCC_Effects0) and (vcc<=VirtCC_EffectsMax) then
           result:=physCC_Effects0+vcc-VirtCC_Effects0
    end;
end;

function TVSTTransformerXRESYNTORCHESTRA.PhysToVirtual(pcc: integer): integer;
begin
  result:=-1;
  if (pcc>=low(PhysToVirtualXRESYNTORCHESTRA)) and (pcc<=high(PhysToVirtualXRESYNTORCHESTRA))
    then
  result:=PhysToVirtualXRESYNTORCHESTRA[pcc]
    else
    // don't map to inherited!
      if (pcc>=physCC_Effects0) and (pcc<=PhysCC_EffectsMax) then
        result:=VirtCC_Effects0+pcc-physCC_Effects0;
end;

function TVSTTransformerXRESYNTORCHESTRA.PhysToParam(pcc: integer): integer;
begin
  if (pcc>=low(PhysToParamXRESYNTORCHESTRA)) and (pcc<=high(PhysToParamXRESYNTORCHESTRA))
    then
  result:=PhysToParamXRESYNTORCHESTRA[pcc]
    else
  result:=-1;
end;

function TVSTTransformerXRESYNTORCHESTRA.ParamToPhys(paramnr: integer): integer;
begin
  if (paramnr>=low(ParamToPhysXRESYNTORCHESTRA)) and (paramnr<=high(ParamToPhysXRESYNTORCHESTRA))
    then
  result:=ParamToPhysXRESYNTORCHESTRA[paramnr]
    else
  result:=-1;
end;

function TVSTTransformerXRESYNTORCHESTRA.ControllerName(pcc:integer):string;
begin
  result:=XRESYNTORCHESTRANames[pcc]
end;
{$endif}


